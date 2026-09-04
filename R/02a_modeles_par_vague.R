# ============================================================================
#  SRCV — Taux d'effort logement & naissance récente : MODÈLES PAR MILLÉSIME
#  Millésimes 2006, 2010, 2014, 2018, 2022, 2023, 2024, 2025.
# ----------------------------------------------------------------------------
#  Première moitié de l'ancien 02_analyse_multimillesimes.R (sauvegardé dans
#  sauvegarde/02_analyse_multimillesimes_avant_scission.R).
#  La préparation des données est dans R/00_prepa_fecondite.R, partagée avec 02b.
#
#  CE QUE FAIT CE SCRIPT :
#   1) modèle principal par vague : naissance ~ taux d'effort + contrôles
#   2) cross-check de causalité inverse : taux d'effort × déménagement
#
#  PANEL — les modèles PAR VAGUE utilisent ids = ~1 : un ménage n'apparaît
#  qu'une fois dans une vague donnée. Les modèles POOLÉS (02b) regroupent au
#  contraire les erreurs-types par ménage (~68 % de ménages communs entre
#  vagues FPR), sinon elles seraient sous-estimées.
#
#  Sorties : Output/resultats_principal.csv
#            Output/resultats_crosscheck.csv
#            Output/crosscheck_demenage_cle.csv
# ============================================================================

source("R/00_prepa_fecondite.R")   # preparer_donnees(), champ, VARS_INTERET

library(survey)      # régression pondérée (plan de sondage)
library(broom)       # résultats de modèles propres

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
    taille_avant + csp + pieces * demenage + age_c + I(age_c^2) + urbain + zeat
  formule_crosscheck <- naissance ~ taux_effort_pct * demenage + statut3 +
    nb_enfants + taille_avant + csp + pieces + age_c + I(age_c^2) + urbain + zeat
} else {
  formule_principale <- naissance ~ taux_effort_pct + statut3 + nb_enfants +
    taille_avant + csp + pieces * demenage + couple + urbain + zeat
  formule_crosscheck <- naissance ~ taux_effort_pct * demenage + statut3 +
    nb_enfants + taille_avant + csp + pieces + couple + urbain + zeat
}
# ZEAT en effet fixe : seule géographie commune aux 8 vagues. En 2010 et 2014,
# 21-23 % des ménages (pondérés) forment la modalité "Non renseigne" — elle est
# donc estimée comme une zone à part entière dans ces deux vagues.

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
donnees <- charger_millesimes()

# Diagnostics par millésime
iwalk(donnees, function(d, annee) {
  message("\n──────────── Millésime ", annee, " ────────────")
  message("n analysable : ", nrow(d),
          " | déménageurs : ", round(100 * mean(d$demenage == "Oui", na.rm = TRUE), 1), " %",
          " | naissances : ", sum(d$naissance == "Oui"),
          " | taux d'effort censuré à 100 % : ", sum(d$taux_effort_pct >= 100))
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

cat("\n=== CROSS-CHECK — termes clés (interaction taux d'effort × déménagement) ===\n")
print(crosscheck_cle, n = Inf)

# ── Export ───────────────────────────────────────────────────────────────────
if (!dir.exists("Output")) dir.create("Output")
write_csv(resultats_principal, "Output/resultats_principal.csv")
write_csv(resultats_crosscheck, "Output/resultats_crosscheck.csv")
write_csv(crosscheck_cle,       "Output/crosscheck_demenage_cle.csv")
cat("\nExporté : Output/resultats_principal.csv, resultats_crosscheck.csv,",
    "crosscheck_demenage_cle.csv\n")

# ── Lecture rapide ──────────────────────────────────────────────────────────
# taux_effort_pct : OR > 1 -> taux d'effort élevé associé à PLUS de naissances.
# Cross-check : si l'effet ne tenait QUE chez les déménageurs -> causalité
#   inverse. S'il tient chez les NON-déménageurs (coût figé avant la
#   naissance) -> association robuste.
# ATTENTION : association, pas causalité (données transversales par vague).
#   Pour un design avant/après : 06_panel.R, 07_modele_* et 15_test_*.
