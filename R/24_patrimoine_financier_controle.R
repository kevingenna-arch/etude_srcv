# ============================================================================
#  SRCV — 24. LE PATRIMOINE FINANCIER COMME CONTRÔLE
# ----------------------------------------------------------------------------
#  Question : les effets de la surface et du statut d'occupation sur la
#  naissance (script 23) survivent-ils quand on contrôle le COUSSIN FINANCIER
#  du ménage, en plus du niveau de vie (flux de revenu) ? Un ménage accédant
#  peut être plus enclin à avoir un enfant non pas grâce à son logement, mais
#  parce qu'il a aussi plus d'épargne de précaution — un canal que le revenu
#  courant ne capte pas.
#
#  SOURCE — module MRF* du fichier ménages (déclaratif, en tranches), présent
#  sur les 8 vagues : MRFEXO (livrets exonérés), MRFLIV (livrets imposables),
#  MRFASV (assurance-vie), MRFAUT (autres placements), MRFLOG (épargne
#  logement), MRFVAL (valeurs mobilières). MRFPEA n'existe qu'à partir de 2022
#  -> exclu du score pour rester comparable sur les 8 vagues. Les variantes
#  "B" (MRFVALB...) ne sont qu'un RAFFINEMENT de la tranche la plus haute du
#  produit correspondant (vérifié : MRFVALB n'est renseigné QUE quand MRFVAL
#  est à son code maximal) : elles ne comptent pas comme un produit distinct.
#
#  Le flag compagnon _DRAP (nom en minuscule en 2018 seulement) vaut
#  0 = ne détient pas ce produit (filtre, pas une non-réponse),
#  1 = détient, montant renseigné en tranche, -1/-2 = ne sait pas / refus.
#  Les tranches NE SONT PAS les mêmes d'une vague à l'autre (vérifié 2006 vs
#  2025 sur MRFLIV) : on ne peut donc pas sommer les codes bruts pour obtenir
#  un montant comparable. Le score utilisé ici normalise chaque produit par
#  son code maximal DANS LA VAGUE avant de sommer, ce qui reste un score
#  ordinal WAVE-COMPARABLE, pas un montant — suffisant pour un contrôle
#  (les effets fixes d'année absorbent le reste).
#
#  À LANCER DEPUIS LA RACINE DU PROJET :
#     source("R/24_patrimoine_financier_controle.R")
#
#  Sorties : patrimoine_couverture.csv, controle_patrimoine.csv
# ============================================================================

source("R/00_prepa_fecondite.R")
library(survey); library(broom); library(marginaleffects)
activer_cache_prepa()

IPC <- charger_ipc()
if (!dir.exists("Output")) dir.create("Output")

# ── Extraction du patrimoine financier, une vague à la fois ────────────────
PRODUITS <- c("MRFEXO", "MRFLIV", "MRFASV", "MRFAUT", "MRFLOG", "MRFVAL")

col_ci <- function(d, motif) {
  # Colonnes 2018 en "_drap" minuscule -> recherche insensible a la casse.
  hit <- names(d)[toupper(names(d)) == toupper(motif)]
  if (length(hit) == 0) return(NULL)
  hit[1]
}

extraire_patrimoine <- function(annee) {
  d <- lire_srcv(chemin_men(annee))
  id <- trimws(as.character(d[[col_ci(d, "DB030")]]))

  par_produit <- map(PRODUITS, function(p) {
    c_val  <- col_ci(d, p)
    c_drap <- col_ci(d, paste0(p, "_DRAP"))
    val  <- num(d[[c_val]])
    drap <- num(d[[c_drap]])
    detient <- drap == 1
    refus   <- drap %in% c(-1, -2)
    max_code <- suppressWarnings(max(val[detient], na.rm = TRUE))
    score <- if_else(detient, val / max_code, if_else(refus, NA_real_, 0))
    tibble(detient = as.integer(detient), refus = as.integer(refus), score)
  })
  names(par_produit) <- PRODUITS

  tibble(
    annee = as.character(annee), id = id,
    nb_produits    = reduce(map(par_produit, "detient"), `+`),
    nb_refus       = reduce(map(par_produit, "refus"), `+`),
    patrimoine_score = reduce(map(par_produit, "score"), `+`, .init = 0) /
      length(PRODUITS)  # ramené à [0 ; 1], moyenne des scores normalisés
  ) %>%
    mutate(aucun_produit = as.integer(nb_produits == 0 & nb_refus == 0))
}

patrimoine <- map_dfr(names(VAGUES), extraire_patrimoine)

cat("Couverture du module patrimoine, par vague :\n")
couverture <- patrimoine %>% group_by(annee) %>%
  summarise(n = n(),
            nb_produits_moy = round(mean(nb_produits), 2),
            part_aucun_produit_pct = round(100 * mean(aucun_produit), 1),
            part_au_moins_1_refus_pct = round(100 * mean(nb_refus > 0), 1),
            .groups = "drop")
print(as.data.frame(couverture), row.names = FALSE)
write_csv(couverture, "Output/patrimoine_couverture.csv")

# ============================================================================
#  §1 — JOINTURE SUR LE CHAMP DE L'ÉTUDE (couples, femme 15-49 ans)
# ============================================================================
cat("\n\n########## §1 — JOINTURE SUR LE CHAMP DE L'ETUDE ##########\n")

SEUILS <- c(-Inf, 50, 70, 90, 120, Inf)
LIB_SEUILS <- c("Moins de 50 m2", "50 a 69 m2", "70 a 89 m2 (ref.)",
                "90 a 119 m2", "120 m2 et plus")

DONNEES_FEC <- millesimes_prepares(restreindre = TRUE)
df <- bind_rows(imap(DONNEES_FEC, ~ .x %>% select(any_of(VARS_INTERET)))) %>%
  mutate(annee_num = as.integer(annee),
         naiss01 = as.integer(naissance == "Oui"),
         cout_m2_reel = deflater(cout_m2, annee_num, IPC, base = 2025),
         niveau_vie_reel = deflater(niveau_vie, annee_num - 1, IPC, base = 2025),
         surface_10 = surface / 10, nv_10k = niveau_vie_reel / 10000,
         rang = factor(if_else(nb_enf_avant == 0, "1er enfant", "Rang 2+"),
                       levels = c("1er enfant", "Rang 2+")),
         across(where(is.factor), ~ factor(as.character(.))),
         statut3 = factor(statut3, levels = c("Locataire", "Proprietaire_accedant",
                                              "Proprietaire_non_accedant")),
         rang = factor(rang, levels = c("1er enfant", "Rang 2+"))) %>%
  filter(!is.na(surface), !is.na(cout_m2_reel), is.finite(cout_m2_reel),
         !is.na(statut3), !is.na(age_femme), !is.na(niveau_vie_reel),
         !is.na(poids), poids > 0) %>%
  mutate(surf_tr = relevel(factor(cut(surface, SEUILS, labels = LIB_SEUILS,
                                      right = FALSE), levels = LIB_SEUILS),
                           ref = "70 a 89 m2 (ref.)")) %>%
  left_join(patrimoine, by = c("annee", "id"))

taux_jointure <- 100 * mean(!is.na(df$patrimoine_score))
cat("Taux de jointure sur le champ de l'etude :", round(taux_jointure, 1), "%\n")
cat("(", sum(is.na(df$patrimoine_score)), "menages non apparies sur",
    nrow(df), ")\n")

# Sanity check : le score de patrimoine croit-il avec le niveau de vie et le
# statut, comme attendu si la variable mesure bien quelque chose ?
cat("\nScore de patrimoine moyen par statut et niveau de vie (sanity check) :\n")
print(as.data.frame(df %>% filter(!is.na(patrimoine_score)) %>%
  group_by(statut3) %>%
  summarise(n = n(), score_moy = round(weighted.mean(patrimoine_score, poids), 3),
            nb_produits_moy = round(weighted.mean(nb_produits, poids), 2),
            aucun_produit_pct = round(100 * weighted.mean(aucun_produit, poids), 1),
            nv_k = round(weighted.mean(niveau_vie_reel, poids) / 1000, 1),
            .groups = "drop")), row.names = FALSE)

cat("\nCorrelation (non ponderee) score de patrimoine / niveau de vie reel :",
    round(cor(df$patrimoine_score, df$niveau_vie_reel, use = "complete.obs"), 3), "\n")

df_p <- df %>% filter(!is.na(patrimoine_score)) %>% droplevels()
cat("\nChamp retenu pour les modeles avec controle patrimoine :", nrow(df_p),
    "menages,", sum(df_p$naiss01), "naissances (", round(100 * nrow(df_p) / nrow(df), 1),
    "% du champ complet)\n")

des <- function(d) svydesign(ids = ~id_cluster, weights = ~poids, data = droplevels(d))
ame <- function(m, vars, ...) as_tibble(avg_slopes(m, variables = vars,
                                                   wts = m$prior.weights, ...))
en_points <- function(x) x %>% mutate(across(any_of(c("estimate", "std.error",
                                                      "conf.low", "conf.high")),
                                             ~ 100 * .x))
elaguer <- function(f, d) {
  vars <- setdiff(all.vars(f), all.vars(f[[2]]))
  mono <- vars[vapply(vars, function(v) v %in% names(d) &&
                        is.factor(d[[v]]) && nlevels(droplevels(d[[v]])) < 2, logical(1))]
  if (length(mono)) f <- update(f, paste(". ~ . -", paste(mono, collapse = " - ")))
  f
}

# ============================================================================
#  §2 — LE PATRIMOINE, EN CONTRÔLE DU MODÈLE SURFACE (linéaire ET en tranches)
# ----------------------------------------------------------------------------
#  Même champ (df_p) dans les deux colonnes "sans" / "avec" patrimoine, pour
#  que la comparaison ne soit pas polluée par le changement d'échantillon.
# ============================================================================
cat("\n\n########## §2 — SURFACE, AVEC ET SANS CONTROLE DE PATRIMOINE ##########\n")

F_LIN_SANS <- naiss01 ~ surface_10 + cout_m2_reel + nv_10k + statut3 +
  age_c + I(age_c^2) + demenage + csp + urbain + zeat + factor(annee) +
  nb_enfants + taille_avant
F_LIN_AVEC <- update(F_LIN_SANS, . ~ . + patrimoine_score)

F_TR_SANS <- update(F_LIN_SANS, . ~ . - surface_10 + surf_tr)
F_TR_AVEC <- update(F_TR_SANS, . ~ . + patrimoine_score)

estimer <- function(d, f, lib, var) {
  d <- droplevels(d)
  m <- svyglm(elaguer(f, d), design = des(d), family = quasibinomial(link = "probit"))
  ame(m, var) %>% en_points() %>%
    transmute(specification = lib, n = nrow(d), naissances = sum(d$naiss01),
              variable = coalesce(term, contrast), ame_pts = estimate,
              se = std.error, conf.low, conf.high, p.value)
}

res_lin <- bind_rows(
  estimer(df_p, F_LIN_SANS, "Lineaire, sans patrimoine", "surface_10"),
  estimer(df_p, F_LIN_AVEC, "Lineaire, avec patrimoine", "surface_10"),
  estimer(filter(df_p, rang == "1er enfant"), F_LIN_SANS,
          "Lineaire, 1er enfant, sans patrimoine", "surface_10"),
  estimer(filter(df_p, rang == "1er enfant"), F_LIN_AVEC,
          "Lineaire, 1er enfant, avec patrimoine", "surface_10"))
cat("\nEffet de la surface, +10 m2 (specification lineaire) :\n")
print(as.data.frame(res_lin %>% mutate(across(where(is.numeric), ~ round(.x, 4)))),
      row.names = FALSE)

res_tr <- bind_rows(
  estimer(filter(df_p, rang == "1er enfant"), F_TR_SANS,
          "1er enfant, sans patrimoine", "surf_tr"),
  estimer(filter(df_p, rang == "1er enfant"), F_TR_AVEC,
          "1er enfant, avec patrimoine", "surf_tr"))
cat("\nEcart de probabilite par tranche de surface, 1er enfant :\n")
print(as.data.frame(res_tr %>% mutate(across(where(is.numeric), ~ round(.x, 3)))),
      row.names = FALSE)

# Effet propre du patrimoine lui-meme
m_patr <- svyglm(elaguer(F_TR_AVEC, filter(df_p, rang == "1er enfant")),
                 design = des(filter(df_p, rang == "1er enfant")),
                 family = quasibinomial(link = "probit"))
cat("\nEffet propre du score de patrimoine (1er enfant, +1 ecart-type de score) :\n")
print(ame(m_patr, "patrimoine_score") %>% en_points() %>%
        mutate(across(where(is.numeric), ~ round(.x, 3))) %>% as.data.frame(),
      row.names = FALSE)

write_csv(bind_rows(res_lin, res_tr), "Output/controle_patrimoine_surface.csv")

# ============================================================================
#  §3 — LE PATRIMOINE DANS L'ESCALIER DE CONTRÔLES DU STATUT D'OCCUPATION
# ----------------------------------------------------------------------------
#  Reprend l'escalier du script 23 (§3) et ajoute une marche M6 : le
#  patrimoine financier, après le niveau de vie. Si l'ecart accedant vs
#  locataire tenait a un coussin d'epargne plutot qu'au logement lui-meme, il
#  devrait s'effondrer ici comme il ne l'a pas fait avec le seul revenu.
# ============================================================================
cat("\n\n########## §3 — ESCALIER DE CONTROLES, MARCHE PATRIMOINE ##########\n")

ESCALIER_P <- list(
  "M3 : + niveau de vie"                 = naiss01 ~ statut3 + age_c + I(age_c^2) +
    nb_enfants + taille_avant + nv_10k + factor(annee),
  "M3b : + patrimoine (sans niveau de vie)" = naiss01 ~ statut3 + age_c + I(age_c^2) +
    nb_enfants + taille_avant + patrimoine_score + factor(annee),
  "M6 : + niveau de vie ET patrimoine"   = naiss01 ~ statut3 + age_c + I(age_c^2) +
    nb_enfants + taille_avant + nv_10k + patrimoine_score + factor(annee),
  "M7 : + tout (CSP, geo, logement, patrimoine)" = naiss01 ~ statut3 + age_c + I(age_c^2) +
    nb_enfants + taille_avant + nv_10k + csp + urbain + zeat +
    surface_10 + cout_m2_reel + patrimoine_score + factor(annee))

escalier_sur <- function(d, lib) {
  d <- droplevels(d)
  imap_dfr(ESCALIER_P, function(f, nom) {
    m <- svyglm(elaguer(f, d), design = des(d), family = quasibinomial(link = "probit"))
    ame(m, "statut3") %>% en_points() %>%
      transmute(echantillon = lib, modele = nom, contraste = contrast,
                ame_pts = estimate, conf.low, conf.high, p.value)
  })
}
res_escalier_p <- bind_rows(
  escalier_sur(df_p, "Ensemble"),
  escalier_sur(filter(df_p, rang == "1er enfant"), "1er enfant"),
  escalier_sur(filter(df_p, rang == "Rang 2+"), "Rang 2 et plus"))
cat("\nEcart de probabilite de naissance vs LOCATAIRE, avec le patrimoine (points) :\n")
print(as.data.frame(res_escalier_p %>% mutate(across(where(is.numeric), ~ round(.x, 3)))),
      row.names = FALSE)
write_csv(res_escalier_p, "Output/controle_patrimoine_statut.csv")

cat("\n\n=== 24 termine. ===\n")
