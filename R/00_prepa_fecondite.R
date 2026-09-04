# ============================================================================
#  SRCV — Préparation du fichier d'analyse "logement × fécondité"
# ----------------------------------------------------------------------------
#  Sourcé par 02a (modèles par vague) ET 02b (prix au m² et évolution agrégée),
#  qui partageaient auparavant les mêmes 100 lignes de préparation.
#
#  POINTS MÉTHODO :
#
#  1) LECTURE — décimale '.' partout, séparateur ';' (2006-2023) ou ','
#     (2024-2025). Géré par lire_srcv() + cache .rds.
#
#  2) BAD CONTROL — NENFANTS et la taille du ménage COMPTENT le nouveau-né :
#     une naissance implique mécaniquement nb_enfants >= 1 et taille +1
#     (vérifié : sur 307 naissances 2006, 306 ont >= 1 enfant). On reconstruit
#     donc des contrôles "AVANT NAISSANCE" (nb_enf_avant, taille_avant).
#
#  3) TAUX D'EFFORT en POINTS de % (0-100) -> OR lisible "par +1 point".
#     ⚠️ CONVENTION UNIFIÉE (voir plus bas) : les valeurs extrêmes sont
#     CENSURÉES à 100, elles ne sont plus supprimées.
#
#  4) NAISSANCE — EVENEMEN_C (2006-2018) ; pour les fichiers FPR (2022-2025),
#     le module "événements" n'existe pas : elle est RECONSTRUITE depuis le
#     fichier individus (enfant né en N ou N-1). Définition différente ->
#     comparaison FPR vs vagues anciennes à nuancer.
#
#  5) PANEL — DB030 chaîne le même ménage à l'intérieur d'un groupe de panel
#     (2014-2018 ; 2022-2025). D'où id_cluster, utilisé pour les erreurs-types
#     des modèles poolés.
# ============================================================================

source("R/00_utils.R")     # lire_srcv(), num(), recode_*(), taux_effort()...
source("R/00_config.R")    # VAGUES : chemins + mapping des variables

# ── CHAMP DE L'ANALYSE ───────────────────────────────────────────────────────
# TRUE  : population "à risque" = ménages EN COUPLE dont la femme (PR ou
#         conjointe) a AGE_MIN-AGE_MAX ans. C'est le champ pertinent pour
#         modéliser une naissance : sur l'ensemble des ménages, les résultats
#         sont dominés par des effets de composition (retraités, personnes
#         seules) — vérifié : l'effet du taux d'effort disparaît une fois le
#         champ restreint.
# FALSE : tous les ménages (analyse historique, tous âges et tous statuts).
#
# Bornes de l'âge de procréer : 15-49 ans, convention OMS/ONU.
# (Les 15-17 ans en couple sont quasi inexistants : passer de 18 à 15 ne change
#  l'échantillon que de ~5 observations sur 27 000.)
RESTREINDRE_FECONDITE <- TRUE
AGE_MIN <- 15
AGE_MAX <- 49

# ── Préparation des données d'un millésime ──────────────────────────────────
preparer_donnees <- function(annee, cfg, restreindre = RESTREINDRE_FECONDITE) {
  # Naissance récente : EVENEMEN_C, ou reconstruction depuis le fichier
  # individus pour les vagues FPR. Logique commune -> 00_utils.R.
  raw <- ajouter_naissance(lire_srcv(cfg$men), cfg)

  verifier_colonnes(raw, cfg$vars, paste0("(millésime ", annee, ")"))

  d <- raw %>%
    select(all_of(cfg$vars)) %>%
    # ZEAT recodée ICI, avant tout filtre : la réparation par REGION exige que
    # les lignes de `raw` et celles en cours de traitement coïncident encore.
    # (00_utils.R : seule géographie commune aux 8 vagues.)
    mutate(zeat = recode_zeat(zeat,
                              if ("REGION" %in% names(raw)) raw$REGION else NULL),
           # Unités de consommation : nécessaire au niveau de vie, mesure de
           # revenu comparable entre ménages de tailles différentes.
           uc = num(raw$HX050),
           # Nombre d'actifs occupés : utilisé par l'ACM et les prestations.
           nactocc = num(raw$NACTOCCUP),
           # tuu est lu en texte ("00".."10" pour TAU99) ou en numérique
           # (0..8 pour tuu10) selon la vague -> uniformisé, sinon bind_rows()
           # refuse d'empiler les millésimes.
           tuu = trimws(as.character(tuu))) %>%
    # id en caractère : DB030 est lu tantôt en texte (zéros initiaux conservés),
    # tantôt en numérique selon la vague -> uniformisé pour permettre bind_rows().
    mutate(id = trimws(as.character(id)),
           across(c(cout_log, revenu, taille, nb_enf, poids, pieces, y_birth, datent,
                    age_pr, age_cj, surface, rembours), num)) %>%
    # ── COÛT = charges (HH070) + MENSUALITÉ D'EMPRUNT (REMP/REMPR) ─────────
    # HH070 exclut le remboursement du capital : pour un accédant il ne couvre
    # qu'une fraction du décaissement réel (2025 : 438 € contre 850 € de
    # mensualité médiane). Seule mesure comparable locataires / accédants.
    # REMP/REMPR = NA signifie "pas d'emprunt en cours" -> 0.
    mutate(
      rembours              = coalesce(rembours, 0),
      cout_log_hors_emprunt = cout_log,
      cout_log              = cout_log + rembours
    ) %>%
    filter(!is.na(revenu), revenu > 0, !is.na(cout_log), !is.na(poids), poids > 0) %>%
    # ── TAUX D'EFFORT : convention UNIQUE du projet (00_utils.R) ────────────
    # Coût mensuel annualisé / revenu disponible annuel, CENSURÉ dans [0 ; 100]
    # points. Ce script supprimait auparavant les ménages au-delà de 100 % :
    # ce groupe (revenus très faibles, charge de logement élevée) n'est pas
    # aléatoire, l'écarter tronquait le champ par le bas de la distribution.
    # Il est désormais conservé, avec un taux d'effort plafonné — même règle
    # que 09 et 12.
    mutate(
      taux_effort_pct = taux_effort(cout_log, revenu),   # points de % dans [0 ; 100]
      taux_effort     = taux_effort_pct / 100            # ratio, pour compatibilité
    ) %>%
    # ── Intensité du logement : décomposition prix × quantité ────────────────
    # taux_effort = (cout_m2 × surface × 12) / revenu : en séparant le PRIX au
    # m² de la QUANTITÉ de surface consommée, on distingue "logement cher au m²"
    # (pression du marché) de "grand logement" (choix de quantité).
    # Surfaces implausibles (< 9 m² ou > 400 m²) mises à NA plutôt que la ligne
    # supprimée (quelques valeurs jusqu'à 9 100 m² dans les fichiers).
    mutate(
      surface        = if_else(surface >= 9 & surface <= 400, surface, NA_real_),
      cout_m2        = cout_log / surface,            # € par m² et par mois
      taux_effort_m2 = taux_effort_pct / surface,     # points de taux d'effort par m²
      surface_piece  = surface / pmax(pieces, 1)      # m² par pièce
    ) %>%
    # ── Recodages ────────────────────────────────────────────────────────────
    mutate(
      naissance    = factor(y_birth, levels = c(0, 1), labels = c("Non", "Oui")),

      # BAD CONTROL : on retire le nouveau-né pour obtenir des contrôles de
      # parité / taille ANTÉRIEURS à la naissance.
      naiss01      = as.integer(y_birth == 1),
      nb_enf_avant = pmax(nb_enf - naiss01, 0),
      taille_avant = taille - naiss01,
      nb_enfants   = cut(nb_enf_avant, breaks = c(-Inf, 0, 1, 2, Inf),
                         labels = c("0", "1", "2", "3+")),

      statut_occ   = recode_occ(occ, cfg$occ_type),          # binaire (historique)
      statut3      = recode_statut3(recode_occ(occ, cfg$occ_type), rembours),
      csp          = factor(csp),
      couple       = recode_couple(couple),
      urbain       = recode_urbain(urbain),   # DB100 : Dense / Intermediaire / Peu_dense
      # Maison / Appartement : nomenclature vintage-dependante, cf. 00_utils.R
      maison_appart = recode_typlog(typlog, cfg$typlog_type),
      # Tranche d'unite urbaine en 5 classes ; NA avant 2014 (cf. 00_utils.R)
      densite      = recode_densite(tuu, cfg$tuu_type),
      niveau_vie   = if_else(!is.na(uc) & uc > 0, revenu / uc, revenu),
      surf_pers    = surface / pmax(taille_avant, 1),

      # Déménagement l'année d'enquête (DATENT/EMMENAG = année d'emménagement).
      # Manquant -> NA. (Fenêtre élargie : as.integer(annee) - datent <= 1)
      demenage     = factor(if_else(datent == as.integer(annee), "Oui", "Non"),
                            levels = c("Non", "Oui")),

      # Âge de la FEMME du ménage (SEXE : 1 = homme, 2 = femme) : c'est l'âge
      # pertinent pour la fécondité. NA si aucune femme PR/conjointe.
      age_femme    = case_when(trimws(as.character(sexe_pr)) == "2" ~ age_pr,
                               trimws(as.character(sexe_cj)) == "2" ~ age_cj,
                               TRUE ~ NA_real_),
      age_c        = (age_femme - 30) / 10,   # âge centré à 30 ans, en décennies

      # Identifiant de cluster : DB030 n'identifie le même ménage qu'à
      # l'intérieur d'un groupe de panel (cf. 00_config.R).
      id_cluster   = paste0(cfg$panel_grp, "_", trimws(as.character(id)))
    ) %>%
    filter(!is.na(naissance), !is.na(statut_occ)) %>%
    mutate(annee = annee, .before = 1)

  # Restriction au champ "à risque" (cf. drapeau ci-dessus)
  if (restreindre)
    d <- d %>% filter(couple == "En_couple", !is.na(age_femme),
                      age_femme >= AGE_MIN, age_femme <= AGE_MAX)
  d
}

# ── Variables conservées pour les analyses en aval ──────────────────────────
VARS_INTERET <- c("annee", "id", "id_cluster", "naissance", "taux_effort", "taux_effort_pct",
                  "statut_occ", "statut3", "couple", "demenage", "nb_enfants",
                  "nb_enf_avant", "taille_avant", "pieces", "csp",
                  "revenu", "cout_log", "cout_log_hors_emprunt", "rembours", "poids",
                  "age_femme", "age_c", "age_pr", "urbain", "zeat",
                  "uc", "niveau_vie", "surf_pers", "taille", "nb_enf", "densite",
                  "nactocc", "diplome",
                  "surface", "cout_m2", "taux_effort_m2", "surface_piece", "maison_appart")

# Prépare les 8 millésimes et renvoie la liste (nommée par millésime).
charger_millesimes <- function(restreindre = RESTREINDRE_FECONDITE)
  imap(VAGUES, ~ preparer_donnees(.y, .x, restreindre = restreindre))

# ── Modèle de DÉCOMPOSITION prix au m² × surface ────────────────────────────
#  Estimé dans 02b, réutilisé pour les scénarios dans 16. Défini ici pour que
#  les deux scripts ne puissent pas diverger.
#  Identité sous-jacente : taux_effort = (cout_m2 × surface × 12) / revenu.
#  En séparant prix unitaire et quantité, revenu contrôlé, on distingue
#  "logement cher au m²" de "grand logement".
F_DECOMPOSITION <- naissance ~ cout_m2 + surface_10 + revenu_10k +
  statut3 + taille_avant + nb_enf_avant + demenage +
  age_c + I(age_c^2) + urbain + zeat + factor(annee)

preparer_df_m2 <- function(df_tot) {
  df_tot %>%
    filter(!is.na(cout_m2), !is.na(surface), is.finite(cout_m2)) %>%
    mutate(revenu_10k = revenu / 10000,
           surface_10 = surface / 10)      # effet "par +10 m²"
}

# ── CACHE MÉMOÏSÉ des préparations lourdes ──────────────────────────────────
#  charger_millesimes() coûte ~7 s et age_moyen_menage() sur 8 vagues ~5 s.
#  Réexécutés dans chaque script, ils dominent le temps total du projet. Ces
#  wrappers mettent le résultat en cache sur disque (Data/_cache/), invalidé
#  dès que 00_config.R, 00_utils.R ou 00_prepa_fecondite.R change.
#  Mettre CACHE_PREPA à FALSE pour forcer le recalcul.
CACHE_PREPA <- TRUE

.cle_socle <- function() {
  f <- c("R/00_config.R", "R/00_utils.R", "R/00_prepa_fecondite.R")
  paste0(as.integer(max(file.mtime(f[file.exists(f)]))))
}

memo_rds <- function(nom, expr) {
  if (!isTRUE(CACHE_PREPA)) return(force(expr))
  dir_cache <- file.path("Data", "_cache")
  if (!dir.exists(dir_cache)) dir.create(dir_cache, recursive = TRUE)
  f <- file.path(dir_cache, paste0("prepa_", nom, "__", .cle_socle(), ".rds"))
  if (file.exists(f)) return(readRDS(f))
  val <- force(expr)
  saveRDS(val, f)
  val
}

# Versions mises en cache, à utiliser dans les scripts compacts.
millesimes_prepares <- function(restreindre = RESTREINDRE_FECONDITE)
  memo_rds(paste0("millesimes_", if (restreindre) "fecond" else "tous"),
           charger_millesimes(restreindre = restreindre))

# Âge moyen du ou des parents, toutes vagues, avec identifiant normalisé prêt
# pour la jointure sur (annee, id).
ages_parents <- function(ans = names(VAGUES))
  memo_rds(paste0("ages_parents_", paste(ans, collapse = "_")),
           imap_dfr(VAGUES[ans],
                    ~ age_moyen_menage(.x, .y, liens = LIENS_PARENTS) %>%
                        mutate(annee = .y)))

# ── Activation du cache sur les fonctions lourdes ───────────────────────────
#  Remplace preparer_donnees(), charger_millesimes() et age_moyen_menage() par
#  des versions mémoïsées sur disque. Les scripts appelants n'ont RIEN à
#  changer : ils bénéficient du cache automatiquement. À appeler une fois en
#  tête des fichiers compacts, qui enchaînent plusieurs analyses partageant les
#  mêmes préparations.
#  Le cache est invalidé dès qu'un fichier 00_* change (cf. .cle_socle()).
activer_cache_prepa <- function(env = globalenv()) {
  if (isTRUE(env$.CACHE_PREPA_ACTIF)) return(invisible(FALSE))
  pd  <- preparer_donnees
  amm <- age_moyen_menage
  env$preparer_donnees <- function(annee, cfg, restreindre = RESTREINDRE_FECONDITE)
    memo_rds(paste0("pd_", annee, "_", isTRUE(restreindre)), pd(annee, cfg, restreindre))
  env$charger_millesimes <- function(restreindre = RESTREINDRE_FECONDITE)
    imap(VAGUES, ~ env$preparer_donnees(.y, .x, restreindre = restreindre))
  env$age_moyen_menage <- function(cfg, annee, liens = LIENS_PARENTS)
    memo_rds(paste0("amm_", annee, "_", if (is.null(liens)) "tous" else "parents"),
             amm(cfg, annee, liens))
  env$.CACHE_PREPA_ACTIF <- TRUE
  message("Cache des préparations activé (Data/_cache/).")
  invisible(TRUE)
}
