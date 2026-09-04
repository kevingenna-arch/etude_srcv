# ============================================================================
#  SRCV 2022 — Arbitrage intra-couple du congé selon les revenus relatifs
# ----------------------------------------------------------------------------
#  Question : dans un couple où la FEMME gagne plus que l'homme, l'homme
#  prend-il davantage son congé ?
#
#  Niveau INDIVIDUS (fichier tab_ind_fpr_2022). On apparie les 2 conjoints
#  (personne de référence LIENPREF="00" + conjoint LIENPREF="01"), on ne garde
#  que les couples hétéro avec un jeune enfant (né 2020-2022).
#
#  DEUX outcomes (côté HOMME) :
#    - congé maternité/paternité : NBM_MAT > 0  (mois de congé mat/pat en N-1)
#    - congé parental (proxy)     : NBM_FOY > 0  (mois "au foyer / garde d'enfants")
#
#  Prédicteur : la femme gagne plus (revenu individuel PY010N + PY050N).
#
#  ⚠️ CAVEATS :
#   - Revenu COURANT contaminé : un conjoint en congé gagne mécaniquement moins
#     -> "la femme gagne plus" est en partie une CONSÉQUENCE du congé (endogène).
#   - NBM_FOY mélange congé parental et "au foyer" au sens large.
#   - Petits effectifs (surtout congé mat/pat côté homme) -> exploratoire.
# ============================================================================

source("R/00_utils.R")     # lire_srcv(), num(), verifier_colonnes()...
source("R/00_config.R")    # VAGUES : chemins des fichiers

library(survey)
library(broom)

# ── Millésime (fichiers FPR individus : 2022, 2023, 2024, 2025) ─────────────
# NB : analyse sur UNE vague à la fois — les vagues FPR sont un PANEL (~68 % de
# ménages communs d'une année à la suivante), les empiler compterait plusieurs
# fois les mêmes couples.
ANNEE <- "2025"    # <- changer ici pour une autre vague
stopifnot("Millésime sans variables NBM_* (fichiers FPR ≥ 2022 requis)" =
            isTRUE(VAGUES[[ANNEE]]$has_nbm))
an  <- as.integer(ANNEE)
ind <- lire_srcv(chemin_ind(ANNEE))
verifier_colonnes(ind, c("RB030","RB040","RB050","ANAIS","LIENPREF","SEXE",
                         "PY010N","PY050N","NBM_MAT","NBM_FOY"),
                  paste0("(individus ", ANNEE, ")"))

# ── Ménages avec un enfant né récemment (année N à N-2) ─────────────────────
hh_young <- ind %>%
  mutate(hid = trimws(as.character(RB040)), anais = num(ANAIS)) %>%
  group_by(hid) %>%
  summarise(jeune_enfant = any(anais %in% (an - 2):an, na.rm = TRUE), .groups = "drop") %>%
  filter(jeune_enfant)

# ── Les 2 conjoints (PR + conjoint) de ces ménages ──────────────────────────
partenaires <- ind %>%
  mutate(hid = trimws(as.character(RB040))) %>%
  semi_join(hh_young, by = "hid") %>%
  filter(LIENPREF %in% c("00", "01")) %>%          # personne de référence + conjoint
  transmute(
    hid,
    sexe   = num(SEXE),                             # 1 = homme, 2 = femme (INSEE)
    revenu = coalesce(num(PY010N), 0) + coalesce(num(PY050N), 0),  # salaire + indép.
    mois_matpat = num(NBM_MAT),                     # mois congé maternité/paternité
    mois_foyer  = num(NBM_FOY),                     # mois au foyer / garde d'enfants
    poids  = num(RB050)
  )

# ── Un couple = 1 ligne (homme vs femme) ; couples hétéro uniquement ────────
couples <- partenaires %>%
  group_by(hid) %>%
  filter(n() == 2, n_distinct(sexe) == 2) %>%       # 2 partenaires, sexes différents
  summarise(
    revenu_h   = revenu[sexe == 1], revenu_f   = revenu[sexe == 2],
    matpat_h   = mois_matpat[sexe == 1], matpat_f = mois_matpat[sexe == 2],
    foyer_h    = mois_foyer[sexe == 1],  foyer_f  = mois_foyer[sexe == 2],
    poids      = poids[sexe == 1],
    .groups = "drop"
  ) %>%
  mutate(
    femme_gagne_plus  = factor(if_else(revenu_f > revenu_h, "Oui", "Non"),
                               levels = c("Non", "Oui")),
    # Outcomes côté HOMME
    homme_matpat      = as.integer(coalesce(matpat_h, 0) > 0),
    homme_parental    = as.integer(coalesce(foyer_h, 0) > 0)
  )

cat("Nombre de couples hétéro avec jeune enfant :", nrow(couples), "\n")
cat("dont la femme gagne plus :", sum(couples$femme_gagne_plus == "Oui"),
    sprintf(" (%.1f %%)\n", 100 * mean(couples$femme_gagne_plus == "Oui")))

# ── Descriptif : part d'hommes prenant un congé selon qui gagne le plus ─────
cat("\n=== Part d'HOMMES prenant un congé, selon 'la femme gagne plus' ===\n")
couples %>%
  group_by(femme_gagne_plus) %>%
  summarise(
    n                  = n(),
    conge_matpat_homme = mean(homme_matpat == 1),
    conge_parental_homme = mean(homme_parental == 1),
    .groups = "drop"
  ) %>%
  print()

# ── Modèles logistiques pondérés ────────────────────────────────────────────
design <- svydesign(ids = ~1, weights = ~poids, data = couples)

cat("\n=== Congé MATERNITÉ/PATERNITÉ (homme) ~ femme gagne plus ===\n")
m_matpat <- svyglm(homme_matpat ~ femme_gagne_plus, design = design,
                   family = quasibinomial())
print(tidy(m_matpat, conf.int = TRUE, exponentiate = TRUE))

cat("\n=== Congé PARENTAL / au foyer (homme) ~ femme gagne plus ===\n")
m_parental <- svyglm(homme_parental ~ femme_gagne_plus, design = design,
                     family = quasibinomial())
print(tidy(m_parental, conf.int = TRUE, exponentiate = TRUE))

# ── Export ───────────────────────────────────────────────────────────────────
if (!dir.exists("Output")) dir.create("Output")
write_csv(couples, paste0("Output/couples_conge_", ANNEE, ".csv"))
