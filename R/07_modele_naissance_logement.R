# ============================================================================
#  SRCV — Modèle : la TAILLE et le PRIX du logement (en t) affectent-ils la
#  probabilité d'une NAISSANCE (en t+1) ?
# ----------------------------------------------------------------------------
#  Données : Output/panel_naissance_logement.csv (produit par 06_panel.R).
#  Logement mesuré en t (AVANT la naissance) -> ordre temporel qui réduit la
#  causalité inverse (mais PAS l'anticipation : cf. déménagements "année avant").
#
#  Échantillon à risque : femmes de 15-49 ans. Erreurs-types regroupées par
#  ménage (un ménage = plusieurs transitions) via svydesign(ids=~id).
#
#  ⚠️ PONDÉRATION : les transitions t->t+1 sont empilées (3 par ménage au plus)
#  et portent le poids transversal de t. La somme des poids vaut donc ~3x la
#  population : les ODDS RATIOS et proportions restent valides, mais tout
#  TOTAL pondéré (effectifs, masses) serait faux. Ne pas publier de totaux.
#
#  Prédicteurs : surface (+10 m²), coût du logement (+100 €, EUROS CONSTANTS
#  2025, charges + mensualité d.emprunt),
#  et — pour les propriétaires — valeur estimée (par +100 000 €).
#  Contrôles : parité PRÉ-naissance, âge de la femme (+ âge²), statut d'occ.,
#  revenu, diplôme PR, CSP, nb d'actifs occupés, région, année.
# ============================================================================

library(tidyverse)
library(survey)
library(broom)

d <- read_csv("Output/panel_naissance_logement.csv", show_col_types = FALSE) %>%
  # échantillon à risque + variables complètes pour le modèle
  filter(age_femme >= 15, age_femme <= 49,
         !is.na(surface), !is.na(cout_log), !is.na(statut3),
         !is.na(revenu), revenu > 0, !is.na(age_femme), !is.na(poids), poids > 0) %>%
  mutate(
    parite    = factor(parite, levels = c("0","1","2","3+")),
    # Statut en 3 postes : locataire / propriétaire ACCÉDANT / NON accédant.
    # (le coût du logement inclut désormais la mensualité d'emprunt : sans la
    #  distinction, accédants et non-accédants seraient confondus alors que
    #  leurs charges diffèrent d'un facteur ~5)
    statut3   = factor(statut3, levels = c("Locataire", "Proprietaire_accedant",
                                           "Proprietaire_non_accedant")),
    couple_f  = factor(couple == 1, labels = c("Non","Oui")),
    csp = factor(csp), dipl_pr = factor(dipl_pr), zeat = factor(zeat), annee_t = factor(annee_t),
    age_c = (age_femme - 30) / 10          # âge centré (30 ans), en décennies
  )

cat("Observations à risque (femmes 15-49) :", nrow(d),
    "| naissances :", sum(d$naissance), sprintf(" (%.2f %%)\n", 100*mean(d$naissance)))

# Plan de sondage : pondéré + regroupement par ménage (transitions répétées)
design <- svydesign(ids = ~id, weights = ~poids, data = d)

# ── Modèle principal (tous statuts d'occupation) ────────────────────────────
f_principal <- naissance ~ surface_10 + cout_100 + parite + age_c + I(age_c^2) +
  statut3 + couple_f + revenu_10k + dipl_pr + csp + nactocc + zeat + annee_t
m <- svyglm(f_principal, design = design, family = quasibinomial())

res <- tidy(m, conf.int = TRUE, exponentiate = TRUE)
cat("\n=== MODÈLE PRINCIPAL — variables d'intérêt (odds ratios) ===\n")
res %>% filter(term %in% c("surface_10","cout_100")) %>%
  mutate(term = recode(term, surface_10 = "Surface (+10 m²)", cout_100 = "Coût logement (+100 €/mois)")) %>%
  select(term, odds_ratio = estimate, conf.low, conf.high, p.value) %>% print()

cat("\n=== Contrôles clés ===\n")
res %>% filter(term %in% c("statut3Proprietaire_accedant","couple_fOui","revenu_10k","age_c") |
               str_starts(term, "parite")) %>%
  select(term, odds_ratio = estimate, p.value) %>% print()

# ── Variante propriétaires : ajout de la valeur estimée du logement ─────────
d_pro <- d %>% filter(statut3 != "Locataire", !is.na(vfepp_reel), vfepp_reel > 0)
cat("\n=== VARIANTE PROPRIÉTAIRES (n =", nrow(d_pro), ", naissances =", sum(d_pro$naissance), ") ===\n")
design_pro <- svydesign(ids = ~id, weights = ~poids, data = d_pro)
m_pro <- svyglm(naissance ~ surface_10 + cout_100 + vfepp_100k + parite + age_c + I(age_c^2) +
                  statut3 + couple_f + revenu_10k + dipl_pr + csp + nactocc + zeat + annee_t,
                design = design_pro, family = quasibinomial())
tidy(m_pro, conf.int = TRUE, exponentiate = TRUE) %>%
  filter(term %in% c("surface_10","cout_100","vfepp_100k")) %>%
  mutate(term = recode(term, surface_10="Surface (+10 m²)", cout_100="Coût (+100 €)",
                       vfepp_100k="Valeur logement (+100 000 €)")) %>%
  select(term, odds_ratio = estimate, conf.low, conf.high, p.value) %>% print()

# ── Export ───────────────────────────────────────────────────────────────────
if (!dir.exists("Output")) dir.create("Output")
write_csv(res, "Output/modele_naissance_logement.csv")
cat("\nExporté : Output/modele_naissance_logement.csv\n")
cat("\n⚠️ Association à ordre temporel, pas causalité pure : l'ANTICIPATION",
    "(déménager en t en prévision d'un enfant en t+1) n'est pas éliminée.\n")
