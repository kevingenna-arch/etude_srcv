# ============================================================================
#  SRCV — Logement par QUINTILE DE NIVEAU DE VIE, 2006 vs 2025
# ----------------------------------------------------------------------------
#  Pour trois classes d'âge de la personne de référence (20-29, 30-39 et
#  15-49 ans — cette dernière ENGLOBE les deux autres : les classes se
#  chevauchent, ce n'est pas une partition), deux statuts (locataires /
#  propriétaires accédants) et cinq quintiles :
#    - surface moyenne (m²)
#    - coût au m² en EUROS CONSTANTS 2025 (déflateur IPC)
#    - taux d'effort (coût annuel / revenu disponible), invariant au déflateur
#  et l'évolution 2006 -> 2025 de chacun.
#
#  QUINTILES : découpage NATIONAL du niveau de vie (revenu disponible / UC) de
#  chaque vague, calculé sur TOUS les ménages, pondéré. Q1 = 20 % les plus
#  modestes. Un ménage jeune est donc positionné dans la distribution générale,
#  pas seulement parmi ses pairs — c'est la lecture usuelle d'un « quintile de
#  revenus », mais elle concentre mécaniquement les 20-29 ans dans le bas.
#
#  ⚠️ « Accédant » = propriétaire remboursant un emprunt sur sa résidence
#  principale (REMP/REMPR > 0). SRCV n'identifie PAS la primo-accession : un
#  second achat est compté de la même façon. Aux âges retenus l'accédant est
#  très majoritairement primo-accédant, mais ce n'est qu'une approximation.
#
#  Coût = charges (HH070) + mensualité d'emprunt (REMP/REMPR).
#  Sortie : Output/logement_par_quintile.csv
# ============================================================================

source("R/00_utils.R")
source("R/00_config.R")
library(survey)

ANNEES <- c("2006", "2025")
IPC    <- charger_ipc()

# Chargement standard (00_utils.R) : coût = charges + mensualité, statut en
# 3 postes, surfaces bornées, niveau de vie, taux d'effort. wquantile() y est
# également défini.
brut <- imap_dfr(VAGUES[ANNEES], ~ charger_menages(.y, .x))

# ── Quintiles nationaux de niveau de vie, par vague ─────────────────────────
seuils <- brut %>%
  filter(!is.na(niveau_vie), niveau_vie > 0) %>%
  group_by(annee) %>%
  summarise(q = list(wquantile(niveau_vie, poids, c(.2, .4, .6, .8))), .groups = "drop")

cat("=== Seuils de niveau de vie (euros courants / an / UC) ===\n")
print(as.data.frame(seuils %>%
  mutate(Q20 = map_dbl(q, 1), Q40 = map_dbl(q, 2),
         Q60 = map_dbl(q, 3), Q80 = map_dbl(q, 4)) %>%
  transmute(annee, across(Q20:Q80, round))), row.names = FALSE)

quintile <- function(nv, an) {
  br <- seuils$q[[match(an, seuils$annee)]]
  cut(nv, breaks = c(-Inf, br, Inf), labels = paste0("Q", 1:5))
}

# ── Champ d'analyse ─────────────────────────────────────────────────────────
#  ⚠️ Les classes se CHEVAUCHENT : 15-49 ans englobe les deux autres. Un ménage
#  apparaît donc dans plusieurs lignes du résultat — ce n'est pas une partition,
#  chaque classe est une analyse séparée. Ajouter une classe = un élément ici.
CLASSES <- list("20-29 ans" = c(20, 29),
                "30-39 ans" = c(30, 39),
                "15-49 ans" = c(15, 49))

d <- imap_dfr(CLASSES, function(bornes, cl)
        brut %>% filter(!is.na(age_pr), age_pr >= bornes[1], age_pr <= bornes[2]) %>%
                 mutate(classe = cl)) %>%
  mutate(classe = factor(classe, levels = names(CLASSES))) %>%
  filter(!is.na(statut3), !is.na(cout), cout >= 0,
         statut3 %in% c("Locataire", "Proprietaire_accedant")) %>%
  group_by(annee) %>%
  mutate(quintile = quintile(niveau_vie, annee[1])) %>%
  ungroup() %>%
  filter(!is.na(quintile)) %>%
  # `effort` (taux d'effort censuré dans [0 ; 100]) vient de charger_menages().
  mutate(statut3   = droplevels(statut3),
         cout_defl = deflater(cout, as.integer(annee), IPC, base = 2025),
         cout_m2   = cout_defl / surface)

cat("\n=== Effectifs non pondérés par cellule ===\n")
print(as.data.frame(d %>% count(classe, statut3, quintile, annee) %>%
                      pivot_wider(names_from = annee, values_from = n, values_fill = 0)),
      row.names = FALSE)

# ── Moyennes pondérées ──────────────────────────────────────────────────────
#  DEUX VENTILATIONS PARALLÈLES, jamais croisées : quintile de niveau de vie et
#  ZEAT. Le croisement donnerait 3 classes × 2 statuts × 5 quintiles × 9 zones
#  × 2 années = 540 cellules, alors que le seul croisement par quintile place
#  déjà les 20-29 ans accédants sous le seuil de 30 observations.
#
#  ⚠️ La ZEAT est exploitable ICI parce que ce script ne compare que 2006 et
#  2025 : 2006 est réparable par REGION et 2025 est complète. Les vagues 2010
#  et 2014, où 21-23 % des ménages n'ont pas de ZEAT, ne sont pas utilisées.
VENTILATIONS <- c(quintile = "Output/logement_par_quintile.csv",
                  zeat     = "Output/logement_par_zeat.csv")

des <- svydesign(ids = ~1, weights = ~poids, data = d)
if (!dir.exists("Output")) dir.create("Output")

for (v in names(VENTILATIONS)) {
  cles <- c("annee", "classe", "statut3", v)
  res <- svyby(~surface + cout_m2 + effort,
               reformulate(cles), des, svymean, na.rm = TRUE) %>%
    as_tibble() %>%
    left_join(count(d, across(all_of(cles)), name = "n"), by = cles)

  evo <- res %>%
    select(all_of(c(cles, "surface", "cout_m2", "effort", "n"))) %>%
    pivot_wider(names_from = annee, values_from = c(surface, cout_m2, effort, n)) %>%
    mutate(d_surface_pct = 100 * (surface_2025 / surface_2006 - 1),
           d_cout_m2_pct = 100 * (cout_m2_2025 / cout_m2_2006 - 1),
           d_effort_pts  = effort_2025 - effort_2006) %>%
    arrange(classe, statut3, .data[[v]])

  write_csv(evo, VENTILATIONS[[v]])

  cat("\n=== Évolutions 2006 -> 2025, ventilation par ", toupper(v), " ===\n", sep = "")
  print(as.data.frame(evo %>%
    transmute(classe, statut3, ventilation = .data[[v]],
              surf06 = round(surface_2006, 1), surf25 = round(surface_2025, 1),
              surf_pct = round(d_surface_pct, 1),
              m2_06 = round(cout_m2_2006, 1), m2_25 = round(cout_m2_2025, 1),
              m2_pct = round(d_cout_m2_pct, 1),
              eff06 = round(effort_2006, 1), eff25 = round(effort_2025, 1),
              eff_pts = round(d_effort_pts, 1),
              n06 = n_2006, n25 = n_2025)), row.names = FALSE)
  cat("Exporté :", VENTILATIONS[[v]], "\n")
}

cat("\nExporté : Output/logement_par_quintile.csv\n")
