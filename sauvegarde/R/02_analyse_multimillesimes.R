# ============================================================================
#  SRCV — Taux d'effort logement & naissance récente : analyse MULTI-MILLÉSIMES
#  Millésimes 2006, 2010, 2014, 2018, 2022, 2023, 2024, 2025.
# ----------------------------------------------------------------------------
#  Chemins, mapping des variables et utilitaires sont centralisés dans
#  R/00_config.R et R/00_utils.R (ajouter une vague = 1 bloc dans 00_config.R).
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
#  3) TAUX D'EFFORT en POINTS de % (0–100) -> OR lisible "par +1 point".
#
#  4) CROSS-CHECK CAUSALITÉ INVERSE — interaction taux d'effort × déménagement.
#
#  5) NAISSANCE — EVENEMEN_C (2006-2018) ; pour les fichiers FPR (2022-2025),
#     le module "événements" n'existe pas : elle est RECONSTRUITE depuis le
#     fichier individus (enfant né en N ou N-1). Définition différente ->
#     comparaison FPR vs vagues anciennes à nuancer.
#
#  6) PANEL — DB030 chaîne le même ménage à l'intérieur d'un groupe de panel
#     (2014-2018 ; 2022-2025). Les modèles PAR VAGUE utilisent ids = ~1 (un
#     ménage n'y apparaît qu'une fois) ; le modèle POOLÉ en fin de script
#     regroupe les erreurs-types par ménage (ids = ~id_cluster), sinon elles
#     seraient sous-estimées (~68 % de ménages communs entre vagues FPR).
# ============================================================================

source("R/00_utils.R")     # lire_srcv(), num(), recode_occ(), recode_couple()...
source("R/00_config.R")    # VAGUES : chemins + mapping des variables

library(survey)      # régression pondérée (plan de sondage)
library(broom)       # résultats de modèles propres

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
  raw <- lire_srcv(cfg$men)

  # Fichiers FPR : reconstruction de la naissance depuis le fichier individus
  # (y_birth = 1 si un enfant né en cfg$naiss_ind$annees est présent).
  if (!is.null(cfg$naiss_ind)) {
    ni <- cfg$naiss_ind
    naiss_men <- lire_srcv(cfg$ind) %>%
      transmute(.cle = trimws(as.character(.data[[ni$cle_ind]])),
                ne   = as.integer(num(.data[[ni$annais]]) %in% ni$annees)) %>%
      group_by(.cle) %>%
      summarise(y_birth = as.integer(any(ne == 1L, na.rm = TRUE)), .groups = "drop")
    raw <- raw %>%
      mutate(.cle = trimws(as.character(.data[[ni$cle_men]]))) %>%
      left_join(naiss_men, by = ".cle") %>%
      mutate(y_birth = replace_na(y_birth, 0L)) %>%
      select(-.cle)
  }

  verifier_colonnes(raw, cfg$vars, paste0("(millésime ", annee, ")"))

  d <- raw %>%
    select(all_of(cfg$vars)) %>%
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
    # ── Taux d'effort : HH070 mensuel -> annualisé / revenu annuel, borné [0,1] ─
    filter(!is.na(revenu), revenu > 0, !is.na(cout_log), !is.na(poids), poids > 0) %>%
    mutate(
      taux_effort     = (cout_log * 12) / revenu,
      taux_effort_pct = taux_effort * 100          # en POINTS de % -> OR "par +1 point"
    ) %>%
    filter(taux_effort >= 0, taux_effort <= 1) %>%
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

  # Restriction au champ "à risque" (cf. drapeau en tête de script)
  if (restreindre)
    d <- d %>% filter(couple == "En_couple", !is.na(age_femme),
                      age_femme >= AGE_MIN, age_femme <= AGE_MAX)
  d
}

# ── Modèles ──────────────────────────────────────────────────────────────────
# Contrôles : versions PRÉ-NAISSANCE (nb_enfants / taille_avant).
# NB : `pieces * demenage` (et non `pieces:demenage`) pour conserver l'effet
# principal de `pieces` — sans lui, le modèle impose une paramétrisation en
# pentes séparées, difficile à lire et incohérente avec le cross-check.
# Sur le champ restreint, `couple` est constant (= En_couple) : il sortirait en
# erreur (facteur à un seul niveau). On le remplace par les contrôles d'âge de
# la femme (déterminant majeur de la fécondité) + le degré d'urbanisation.
if (RESTREINDRE_FECONDITE) {
  formule_principale <- naissance ~ taux_effort_pct + statut3 + nb_enfants +
    taille_avant + csp + pieces * demenage + age_c + I(age_c^2) + urbain
  formule_crosscheck <- naissance ~ taux_effort_pct * demenage + statut3 +
    nb_enfants + taille_avant + csp + pieces + age_c + I(age_c^2) + urbain
} else {
  formule_principale <- naissance ~ taux_effort_pct + statut3 + nb_enfants +
    taille_avant + csp + pieces * demenage + couple + urbain
  formule_crosscheck <- naissance ~ taux_effort_pct * demenage + statut3 +
    nb_enfants + taille_avant + csp + pieces + couple + urbain
}

# Modèle PAR VAGUE : ids = ~1 (chaque ménage n'apparaît qu'une fois par vague).
ajuster_svy <- function(d, formule) {
  design <- svydesign(ids = ~1, weights = ~poids, data = d)
  svyglm(formule, design = design, family = quasibinomial())
}

resumer <- function(modele, annee) {
  broom::tidy(modele, conf.int = TRUE, exponentiate = TRUE) %>%
    mutate(annee = annee, n_modele = stats::nobs(modele), .before = 1)
}

# ── Préparation : chaque fichier lu UNE seule fois (puis mis en cache) ───────
donnees <- imap(VAGUES, ~ preparer_donnees(.y, .x))

# Diagnostics par millésime
iwalk(donnees, function(d, annee) {
  message("\n──────────── Millésime ", annee, " ────────────")
  message("n analysable : ", nrow(d),
          " | déménageurs : ", round(100 * mean(d$demenage == "Oui", na.rm = TRUE), 1), " %",
          " | naissances : ", sum(d$naissance == "Oui"))
  d %>%
    mutate(q_effort = ntile(taux_effort, 4)) %>%
    group_by(q_effort) %>%
    summarise(n = n(), taux_naissance = mean(naissance == "Oui"), .groups = "drop") %>%
    print()
})

# ── 1) Modèle principal : OR par millésime ──────────────────────────────────
resultats_principal <- imap_dfr(donnees, ~ resumer(ajuster_svy(.x, formule_principale), .y))

cat("\n=== MODÈLE PRINCIPAL — Odds ratio du TAUX D'EFFORT (par +1 point) ===\n")
resultats_principal %>%
  filter(term == "taux_effort_pct") %>%
  select(annee, n_modele, odds_ratio = estimate, conf.low, conf.high, p.value) %>%
  print(n = Inf)

cat("\n=== Vérif : OR de nb_enfants (parité PRÉ-naissance -> doivent être plausibles) ===\n")
resultats_principal %>%
  filter(str_starts(term, "nb_enfants")) %>%
  select(annee, term, odds_ratio = estimate, p.value) %>%
  print(n = Inf)

# ── 2) Cross-check : interaction taux d'effort × déménagement ────────────────
resultats_crosscheck <- imap_dfr(donnees, ~ resumer(ajuster_svy(.x, formule_crosscheck), .y))

termes_cles <- c("taux_effort_pct", "demenageOui", "taux_effort_pct:demenageOui")
crosscheck_cle <- resultats_crosscheck %>%
  filter(term %in% termes_cles) %>%
  select(annee, term, odds_ratio = estimate, conf.low, conf.high, p.value)

cat("\n=== CROSS-CHECK — termes clés (interaction taux_effort × déménagement) ===\n")
print(crosscheck_cle, n = Inf)

# ── Export ───────────────────────────────────────────────────────────────────
if (!dir.exists("Output")) dir.create("Output")
write_csv(resultats_principal, "Output/resultats_principal.csv")
write_csv(resultats_crosscheck, "Output/resultats_crosscheck.csv")
write_csv(crosscheck_cle,       "Output/crosscheck_demenage_cle.csv")

# ── Data.frames par millésime (variables d'intérêt) pour analyses ad hoc ─────
vars_interet <- c("annee", "id", "id_cluster", "naissance", "taux_effort", "taux_effort_pct",
                  "statut_occ", "statut3", "couple", "demenage", "nb_enfants",
                  "nb_enf_avant", "taille_avant", "pieces", "csp",
                  "revenu", "cout_log", "cout_log_hors_emprunt", "rembours", "poids", "age_femme", "age_c", "age_pr", "urbain",
                  "surface", "cout_m2", "taux_effort_m2", "surface_piece")
dfs <- imap(donnees, ~ .x %>% select(any_of(vars_interet)))
invisible(list2env(setNames(dfs, paste0("df", names(dfs))), envir = environment()))  # df2006 … df2025

# ── Lecture rapide ──────────────────────────────────────────────────────────
# taux_effort_pct : OR > 1 -> taux d'effort élevé associé à PLUS de naissances.
# Cross-check : si l'effet ne tenait QUE chez les déménageurs -> causalité
#   inverse. S'il tient chez les NON-déménageurs (coût figé avant la
#   naissance) -> association robuste.
# ATTENTION : association, pas causalité (données transversales par vague ;
#   pour un design avant/après, voir 06_panel.R et 07_modele_*).


###Calculs maisons ###############################################################

# df_tot est construit à partir de la LISTE dfs : toute nouvelle vague ajoutée
# dans 00_config.R y entre automatiquement (plus de risque d'en oublier une).
df_tot <- bind_rows(dfs)
cat("\ndf_tot :", nrow(df_tot), "obs |", n_distinct(df_tot$annee), "millésimes :",
    paste(sort(unique(df_tot$annee)), collapse = ", "), "\n")

library(survey)

# Erreurs-types regroupées par MÉNAGE : les vagues 2014-2018 et 2022-2025 sont
# des panels (mêmes ménages revus) -> ids = ~1 sous-estimerait les écarts-types.
design <- svydesign(ids = ~id_cluster, weights = ~poids, data = df_tot)

tester_svy <- svyglm(naissance ~ taux_effort_pct*statut3 +
                       taille_avant + nb_enf_avant + pieces*demenage +
                       age_c + I(age_c^2) + urbain + factor(annee),
                     design = design,
                     family = quasibinomial(link = "probit"))
summary(tester_svy)

library(marginaleffects)
avg_slopes(tester_svy, wts = tester_svy$prior.weights)

avg_slopes(tester_svy, variables = "taux_effort_pct",
           by = "statut3", wts = tester_svy$prior.weights)


avg_slopes(tester_svy, variables = "taux_effort_pct",
           by = "statut3", wts = tester_svy$prior.weights)

# df_fec == df_tot lorsque RESTREINDRE_FECONDITE = TRUE (le champ est déjà
# appliqué en amont, dans preparer_donnees) : alias conservé pour la suite.
df_fec <- df_tot


## ── Variante : DÉCOMPOSITION du taux d'effort en PRIX au m² × SURFACE ───────
# Identité : taux_effort = (cout_m2 * surface * 12) / revenu. En introduisant
# cout_m2 et surface séparément (revenu contrôlé), on sépare :
#   - cout_m2  = prix unitaire du logement (pression du marché local)
#   - surface  = quantité de logement consommée (choix / contrainte de place)
# Le taux d'effort agrégé confondait les deux.
df_m2 <- df_fec %>%
  filter(!is.na(cout_m2), !is.na(surface), is.finite(cout_m2)) %>%
  mutate(revenu_10k  = revenu / 10000,
         surface_10  = surface / 10)      # effet "par +10 m²"

cat("\n=== Variante : coût au m² + surface (couples, femme",
    AGE_MIN, "-", AGE_MAX, "ans) ===\n")
cat("n =", nrow(df_m2), "| naissances :", sum(df_m2$naissance == "Oui"), "\n")
cat("cout_m2 (€/m²/mois) : médiane ", round(median(df_m2$cout_m2), 2),
    " | surface médiane ", round(median(df_m2$surface)), " m²\n")

design_m2 <- svydesign(ids = ~id_cluster, weights = ~poids, data = df_m2)
modele_m2 <- svyglm(naissance ~ cout_m2 + surface_10 + revenu_10k +
                      statut3 + taille_avant + nb_enf_avant + demenage +
                      age_c + I(age_c^2) + urbain + factor(annee),
                    design = design_m2,
                    family = quasibinomial(link = "probit"))
summary(modele_m2)
avg_slopes(modele_m2, variables = c("cout_m2", "surface_10", "revenu_10k"),
           wts = modele_m2$prior.weights)


#### Graphs ####################################################################

library(ggplot2)

df_tot %>%
  group_by(annee) %>%
  mutate(w = poids / sum(poids)) %>%
  ungroup() %>%
  ggplot(aes(x = cout_m2, colour = factor(annee), linetype = statut3, weight = w)) +
  geom_density(linewidth = 0.8) +
  labs(x = "Prix au m2", y = "Densité", colour = "Année", linetype = "Statut") +
  theme_minimal()


#### Évolution du PRIX AU M² : test 2025 vs 2024 (et autres années) ############
#  Question : le coût au m² est-il plus élevé en 2025 qu'en 2024, une fois
#  l'évolution des REVENUS prise en compte ?
#
#  ⚠️ cout_m2 est en EUROS COURANTS : une comparaison brute entre années
#     confond hausse réelle et inflation. D'où les trois approches ci-dessous.
#  ⚠️ Champ : si RESTREINDRE_FECONDITE = TRUE, le test porte sur les couples
#     18-49 ans. Passer le drapeau à FALSE pour raisonner sur tous les ménages.
#  ⚠️ Le contrôle par le revenu corrige l'évolution du pouvoir d'achat des
#     ménages, PAS l'inflation des autres biens. Pour de l'euro constant,
#     déflater cout_m2 par l'IPC (INSEE) avant estimation.

#  NB REVENU : `revenu` = HY020 = REVENU DISPONIBLE TOTAL du ménage. Il inclut
#  les revenus du travail ET du capital ET toutes les prestations sociales
#  (retraites, chômage, prestations familiales, aides au logement, minima),
#  nets d'impôts et de cotisations. Ce n'est pas le seul salaire.
#  NB PONDÉRATION : tous les calculs ci-dessous sont pondérés (svydesign avec
#  weights = ~poids, soit DB090 / pond_18 selon la vague).

#  NB STATUT : HH070 ne mesure pas la même chose selon le statut d'occupation.
#  - LOCATAIRES : loyer + charges -> proche d'un prix de marché du logement.
#  - PROPRIÉTAIRES : intérêts d'emprunt + taxes + charges + entretien, HORS
#    remboursement du capital ; quasi nul pour un propriétaire non accédant.
#  Les NIVEAUX ne sont donc pas comparables entre les deux groupes ; seules
#  les ÉVOLUTIONS au sein de chaque groupe le sont. La série "locataires" est
#  la plus proche d'un indicateur de prix du logement.

ANNEE_REF <- "2025"     # année de référence des comparaisons

# ── EUROS CONSTANTS : déflation par l'IPC INSEE (base 2025, hors tabac) ─────
#  Coûts relevés à la date d'enquête -> déflatés par l'indice de l'année N.
#  Revenu HY020 (année N-1) -> déflaté par l'indice de N-1.
#  Inflation cumulée vs 2025 : +33,5 % depuis 2006, +7,7 % depuis 2022.
IPC <- charger_ipc()
df_tot <- df_tot %>%
  mutate(annee_num     = as.integer(annee),
         cout_m2_reel  = deflater(cout_m2,  annee_num,     IPC, base = 2025),
         cout_log_reel = deflater(cout_log, annee_num,     IPC, base = 2025),
         revenu_reel   = deflater(revenu,   annee_num - 1, IPC, base = 2025))

d_prix <- df_tot %>%
  filter(!is.na(cout_m2), is.finite(cout_m2), cout_m2 > 0,
         !is.na(revenu), revenu > 0, !is.na(surface), !is.na(poids), poids > 0) %>%
  mutate(annee_f     = relevel(factor(annee), ref = ANNEE_REF),
         log_cout_m2      = log(cout_m2),
         log_cout_m2_reel = log(cout_m2_reel),   # euros constants 2025
         log_revenu  = log(revenu))

# ── Analyses (A) et (B) sur un sous-ensemble ────────────────────────────────
# statut_occ n'est plus dans la formule quand on travaille dans un groupe
# (il y serait constant).
analyser_prix <- function(d, libelle, avec_statut = FALSE) {
  cat("\n##########", libelle, "— n =", nrow(d), "##########\n")
  des <- svydesign(ids = ~id_cluster, weights = ~poids, data = d)

  cat("\n-- Prix au m² moyen pondéré (€/m²/mois) --\n")
  print(svyby(~cout_m2, ~annee, des, svymean, na.rm = TRUE))

  f_a <- if (avec_statut)
    log_cout_m2 ~ annee_f + log_revenu + statut3 + urbain + surface + taille_avant
  else
    log_cout_m2 ~ annee_f + log_revenu + urbain + surface + taille_avant
  mA <- svyglm(f_a, design = des)
  cat("\n-- (A) log(prix au m²) ~ année (réf.", ANNEE_REF, ") + revenu + contrôles --\n")
  print(tidy(mA, conf.int = TRUE) %>%
          filter(str_starts(term, "annee_f") | term == "log_revenu") %>%
          mutate(ecart_pct = round(100 * (exp(estimate) - 1), 1)) %>%
          select(term, ecart_pct, conf.low, conf.high, p.value))

  # (A') Évolution RÉELLE : même modèle sur le coût déflaté par l'IPC ->
  # les coefficients d'année mesurent l'écart de prix HORS inflation générale.
  mA2 <- svyglm(update(f_a, log_cout_m2_reel ~ .), design = des)
  cat("\n-- (A') log(prix au m² RÉEL, € constants 2025) ~ année --\n")
  print(tidy(mA2, conf.int = TRUE) %>%
          filter(str_starts(term, "annee_f")) %>%
          mutate(ecart_pct = round(100 * (exp(estimate) - 1), 1)) %>%
          select(term, ecart_pct, p.value))

  f_b <- if (avec_statut)
    log(taux_effort_m2) ~ annee_f + statut3 + urbain + surface + taille_avant
  else
    log(taux_effort_m2) ~ annee_f + urbain + surface + taille_avant
  mB <- svyglm(f_b, design = des)
  cat("\n-- (B) log(effort par m², = prix rapporté au revenu) ~ année --\n")
  print(tidy(mB, conf.int = TRUE) %>%
          filter(str_starts(term, "annee_f")) %>%
          mutate(ecart_pct = round(100 * (exp(estimate) - 1), 1)) %>%
          select(term, ecart_pct, p.value))
  invisible(NULL)
}

# ── (C) Comparaison INTRA-MÉNAGE (mêmes ménages revus) ─────────────────────
# Chaînage possible uniquement à l'intérieur d'un groupe de panel -> 2022-2025.
# On exige le MÊME statut d'occupation aux deux dates, pour ne pas mélanger
# l'évolution des prix avec les changements de statut (accession, etc.).
comparer_paire <- function(a1, a2, statut = NULL) {
  p <- df_tot %>%
    filter(annee %in% c(a1, a2), !is.na(cout_m2), is.finite(cout_m2),
           cout_m2 > 0, !is.na(revenu), revenu > 0) %>%
    select(id_cluster, annee, cout_m2, revenu, poids, statut3) %>%
    pivot_wider(names_from = annee,
                values_from = c(cout_m2, revenu, poids, statut3)) %>%
    rename(c1 = paste0("cout_m2_", a1),   c2 = paste0("cout_m2_", a2),
           r1 = paste0("revenu_", a1),    r2 = paste0("revenu_", a2),
           s1 = paste0("statut3_", a1), s2 = paste0("statut3_", a2),
           w2 = paste0("poids_", a2)) %>%
    filter(!is.na(c1), !is.na(c2), !is.na(w2), !is.na(s1), !is.na(s2), s1 == s2)
  if (!is.null(statut)) p <- p %>% filter(s1 == statut)
  if (nrow(p) < 30) { cat("  (effectif insuffisant :", nrow(p), ")\n"); return(invisible(NULL)) }

  # Évolutions en EUROS CONSTANTS : on retire l'inflation entre a1 et a2.
  # d_relatif est un rapport prix/revenu -> l'inflation s'y annule d'elle-même.
  infl <- log(IPC$ipc[IPC$annee == as.integer(a2)] / IPC$ipc[IPC$annee == as.integer(a1)])
  p <- p %>% mutate(d_log_prix   = log(c2) - log(c1) - infl,
                    d_log_revenu = log(r2) - log(r1) - infl,
                    d_relatif    = (log(c2) - log(c1)) - (log(r2) - log(r1)))
  cat("\n--- ", a1, " -> ", a2, " | ", ifelse(is.null(statut), "ensemble", statut),
      " (n = ", nrow(p), ") ---\n", sep = "")
  des <- svydesign(ids = ~1, weights = ~w2, data = p)
  for (v in c("d_log_prix", "d_log_revenu", "d_relatif")) {
    m <- svyglm(as.formula(paste(v, "~ 1")), design = des)
    est <- coef(m)[1]; ic <- confint(m)[1, ]
    cat(sprintf("  %-13s : %+6.2f %% [%+6.2f ; %+6.2f]  p = %.3g\n",
                v, 100*(exp(est)-1), 100*(exp(ic[1])-1), 100*(exp(ic[2])-1),
                coef(summary(m))[1, 4]))
  }
  invisible(NULL)
}

# ── Exécution : ensemble, puis locataires et propriétaires séparément ──────
analyser_prix(d_prix, "ENSEMBLE", avec_statut = TRUE)
analyser_prix(filter(d_prix, statut3 == "Locataire"),                 "LOCATAIRES")
analyser_prix(filter(d_prix, statut3 == "Proprietaire_accedant"),     "PROPRIÉTAIRES ACCÉDANTS")
analyser_prix(filter(d_prix, statut3 == "Proprietaire_non_accedant"), "PROPRIÉTAIRES NON ACCÉDANTS")

cat("\n\n=== (C) Évolution INTRA-MÉNAGE vers", ANNEE_REF, "(statut constant) ===\n")
for (st in list(NULL, "Locataire", "Proprietaire_accedant", "Proprietaire_non_accedant"))
  for (a in c("2022", "2023", "2024")) comparer_paire(a, ANNEE_REF, st)
#### ÉVOLUTION AGRÉGÉE : moyennes pondérées par vague ##########################
#  Lecture MACRO, complémentaire des modèles (A)/(A')/(B) :
#   - pas de suivi de ménages (contrairement au test intra-ménage (C)),
#   - pas de contrôle de caractéristiques : on prend la MOYENNE PONDÉRÉE de
#     chaque vague, donc une population représentative à chaque date.
#  L'évolution mêle donc effet prix ET déformation de la population (âge,
#  statut d'occupation, taille des logements...) — c'est voulu ici.
#
#  Produite sur DEUX champs :
#   1. TOUS LES MÉNAGES  -> lecture macro d'ensemble.
#   2. COUPLES FÉCONDS (femme AGE_MIN-AGE_MAX ans) -> conditions de logement de
#      la population effectivement susceptible d'avoir un enfant.
#
#  ⚠️ Les vagues 2014-2018 et 2022-2025 partagent une partie de leurs ménages
#     (panel) : les écarts entre vagues proches ne sont pas indépendants.

prep_agrege <- function(d) {
  d %>%
    mutate(annee_num    = as.integer(annee),
           cout_m2_reel = deflater(cout_m2,  annee_num,     IPC, base = 2025),
           revenu_reel  = deflater(revenu,   annee_num - 1, IPC, base = 2025),
           cout_an      = cout_log * 12,                                # coût annuel
           cout_an_reel = deflater(cout_an,  annee_num,     IPC, base = 2025)) %>%
    filter(!is.na(cout_m2), is.finite(cout_m2), cout_m2 > 0,
           !is.na(revenu), revenu > 0, !is.na(poids), poids > 0)
}

analyse_agregee <- function(d, libelle) {
  cat("\n\n##########", libelle, "— n =", nrow(d), "##########\n")
  des <- svydesign(ids = ~1, weights = ~poids, data = d)

  agg <- svyby(~cout_m2 + cout_m2_reel + revenu + revenu_reel + cout_an,
               ~annee, des, svymean, na.rm = TRUE) %>%
    as_tibble() %>%
    transmute(annee,
              cout_m2_nom  = round(cout_m2, 2),  cout_m2_reel = round(cout_m2_reel, 2),
              revenu_nom   = round(revenu),      revenu_reel  = round(revenu_reel),
              # taux d'effort AGRÉGÉ = rapport des moyennes (coût annuel / revenu)
              effort_agrege_pct = round(100 * cout_an / revenu, 1))
  cat("\n-- Moyennes pondérées par vague --\n"); print(as.data.frame(agg))

  base <- agg %>% filter(annee == "2006")
  idx <- agg %>%
    transmute(annee,
              idx_cout_m2_nom  = round(100 * cout_m2_nom  / base$cout_m2_nom),
              idx_revenu_nom   = round(100 * revenu_nom   / base$revenu_nom),
              idx_cout_m2_reel = round(100 * cout_m2_reel / base$cout_m2_reel),
              idx_revenu_reel  = round(100 * revenu_reel  / base$revenu_reel),
              effort_agrege_pct)
  cat("\n-- Indices base 100 en 2006 --\n"); print(as.data.frame(idx))

  cat("\n-- Prix au m² moyen (€ constants 2025) par statut --\n")
  print(svyby(~cout_m2_reel, ~annee + statut3, des, svymean, na.rm = TRUE) %>%
          as_tibble() %>% transmute(annee, statut3, v = round(cout_m2_reel, 2)) %>%
          pivot_wider(names_from = statut3, values_from = v) %>% as.data.frame())

  cat("\n-- Taux d'effort agrégé (coût annuel moyen / revenu moyen, %) par statut --\n")
  print(svyby(~cout_an + revenu, ~annee + statut3, des, svymean, na.rm = TRUE) %>%
          as_tibble() %>% transmute(annee, statut3, v = round(100 * cout_an / revenu, 1)) %>%
          pivot_wider(names_from = statut3, values_from = v) %>% as.data.frame())
  agg
}

# Champ 1 : tous les ménages (indépendant de RESTREINDRE_FECONDITE)
df_tous <- bind_rows(imap(VAGUES, ~ preparer_donnees(.y, .x, restreindre = FALSE))) %>%
  prep_agrege()
agg_tous <- analyse_agregee(df_tous, "TOUS LES MÉNAGES")

# Champ 2 : couples féconds (femme AGE_MIN-AGE_MAX ans)
df_fecond <- bind_rows(imap(VAGUES, ~ preparer_donnees(.y, .x, restreindre = TRUE))) %>%
  prep_agrege()
agg_fecond <- analyse_agregee(
  df_fecond, paste0("COUPLES FÉCONDS (femme ", AGE_MIN, "-", AGE_MAX, " ans)"))

write_csv(bind_rows(agg_tous   %>% mutate(champ = "Tous ménages"),
                    agg_fecond %>% mutate(champ = "Couples féconds")),
          "Output/evolution_agregee.csv")
cat("\nExporté : Output/evolution_agregee.csv\n")
