# ============================================================================
#  SRCV — 54. Crédit, marché et capacité d'achat : la fluidité résidentielle
#             à revenu donné, 2014-2025
# ----------------------------------------------------------------------------
#  Données Banque de France (Webstat, jeu MIR1, dans « Data/banque de france ») :
#   - taux des crédits nouveaux à l'habitat des particuliers (A22, mensuel ;
#     la série hors renégociations n'existe pas pour le taux) ;
#   - montants de crédits nouveaux à l'habitat hors renégociations (A22HR,
#     flux mensuel CVS, milliards d'euros).
#  Avec les prix DVF+ (script 26) et les revenus SRCV :
#   §1 crédit : taux moyen annuel, montants annuels en euros de 2025 ;
#   §2 capacité d'achat en m² à revenu donné : revenus fixes (P25, P50, P75
#      du revenu disponible des couples du champ en 2014, euros de 2025,
#      ramenés en euros courants de chaque année), un tiers du revenu en
#      mensualités sur 25 ans au taux moyen de l'année, 7,5 % de frais
#      d'acquisition, prix médian au m² DVF+ (appartements, maisons ; région
#      parisienne et reste de la métropole) ;
#   §3 décomposition de l'évolution de la capacité d'achat : revenu, taux,
#      prix (écarts de logarithmes, 2014 -> chaque année) ;
#   §4 effort pour acheter 60 m² au revenu médian ;
#   §5 tableau de bord de la fluidité : crédit, transactions, capacité
#      d'achat, mobilité du recensement (script 53) et de SRCV (script 51).
#  Sortie : Output/credit_marche_fluidite.csv
# ============================================================================

source("R/00_prepa_fecondite.R")
activer_cache_prepa()
IPC <- charger_ipc()
DIR_BDF <- file.path("Data", "banque de france")
PART_REVENU <- 1 / 3; DUREE_MOIS <- 300; FRAIS <- 0.075; SURFACE_REF <- 60

lire_webstat <- function(motif) {
  f <- list.files(DIR_BDF, pattern = motif, full.names = TRUE)
  # deux exports par série : on garde le format long (première ligne « series_key;... »)
  f <- f[vapply(f, function(x) grepl("series_key", readLines(x, n = 1, encoding = "UTF-8")), logical(1))]
  stopifnot(length(f) == 1)
  read_delim(f, delim = ";", locale = locale(decimal_mark = ",", encoding = "UTF-8"), show_col_types = FALSE) %>%
    transmute(annee = as.integer(substr(time_period, 1, 4)), mois = as.integer(substr(time_period, 6, 7)),
              valeur = as.numeric(obs_value))
}

# ── §1 Crédit ──────────────────────────────────────────────────────────────
taux <- lire_webstat("A22\\.A\\.R\\.A") %>% group_by(annee) %>%
  summarise(taux_moyen_pct = mean(valeur), mois = n(), .groups = "drop")
volumes <- lire_webstat("A22HR\\.A\\.5\\.A") %>% group_by(annee) %>%
  summarise(credits_nouveaux_mds = sum(valeur), mois = n(), .groups = "drop") %>%
  filter(mois == 12) %>%
  mutate(credits_nouveaux_mds_2025 = deflater(credits_nouveaux_mds, annee, IPC, base = 2025))
credit <- taux %>% select(-mois) %>% left_join(volumes %>% select(-mois), by = "annee") %>%
  filter(annee >= 2010, annee <= 2025)
cat("§1 Crédit à l'habitat :\n")
print(as.data.frame(credit %>% mutate(across(where(is.double), ~ round(.x, 2)))), row.names = FALSE)

# ── §2 Capacité d'achat en m² à revenu donné ───────────────────────────────
d14 <- millesimes_prepares(restreindre = TRUE)[["2014"]] %>%
  mutate(revenu_reel = deflater(revenu, 2013, IPC, base = 2025)) %>% filter(!is.na(revenu_reel), poids > 0)
REVENUS <- setNames(wquantile(d14$revenu_reel, d14$poids, c(.25, .5, .75)), c("P25", "P50", "P75"))
cat("\nRevenus disponibles de référence des couples (2014, € de 2025 par an) :", paste(names(REVENUS), round(REVENUS), collapse = " ; "), "\n")

dvf <- read_csv("Output/dvf_zeat_annee.csv", show_col_types = FALSE) %>%
  mutate(zone = if_else(zeat == 1, "Region parisienne", "Province"))
prix <- bind_rows(dvf %>% mutate(zone = "France"), dvf) %>% group_by(annee, zone, type_bien) %>%
  summarise(prix_m2 = weighted.mean(prix_m2_median, n), .groups = "drop")
annuite <- function(r_pct, n = DUREE_MOIS) { i <- r_pct / 1200; (1 - (1 + i)^(-n)) / i }
capacite <- prix %>% inner_join(taux %>% select(annee, taux_moyen_pct), by = "annee") %>%
  tidyr::crossing(niveau = names(REVENUS)) %>%
  mutate(revenu_courant = REVENUS[niveau] / deflater(1, annee, IPC, base = 2025),
         capital = PART_REVENU * revenu_courant / 12 * annuite(taux_moyen_pct),
         m2_achetables = capital / (1 + FRAIS) / prix_m2) %>%
  group_by(zone, type_bien, niveau) %>%
  mutate(indice_2014 = 100 * m2_achetables / m2_achetables[annee == 2014]) %>% ungroup()
cat("\n§2 m² achetables à revenu donné (France, revenu médian de 2014 en euros constants) :\n")
print(as.data.frame(capacite %>% filter(zone == "France", niveau == "P50") %>%
  select(annee, type_bien, taux_moyen_pct, prix_m2, m2_achetables, indice_2014) %>%
  mutate(across(where(is.double), ~ round(.x, 1)))), row.names = FALSE)

# ── §3 Décomposition : revenu, taux, prix ──────────────────────────────────
decompo <- capacite %>% group_by(zone, type_bien, niveau) %>% arrange(annee) %>%
  mutate(revenu_pct = 100 * (log(revenu_courant) - log(revenu_courant[annee == 2014])),
         taux_pct = 100 * (log(annuite(taux_moyen_pct)) - log(annuite(taux_moyen_pct[annee == 2014]))),
         prix_pct = -100 * (log(prix_m2) - log(prix_m2[annee == 2014])),
         total_pct = revenu_pct + taux_pct + prix_pct) %>% ungroup() %>%
  filter(niveau == "P50") %>% select(annee, zone, type_bien, revenu_pct, taux_pct, prix_pct, total_pct)
cat("\n§3 Évolution de la capacité d'achat depuis 2014 (écarts de log × 100), revenu médian :\n")
print(as.data.frame(decompo %>% filter(zone == "France") %>% mutate(across(where(is.double), ~ round(.x, 1)))),
      row.names = FALSE)

# ── §4 Effort pour acheter 60 m² au revenu médian ──────────────────────────
effort <- capacite %>% filter(niveau == "P50") %>%
  mutate(mensualite = SURFACE_REF * prix_m2 * (1 + FRAIS) / annuite(taux_moyen_pct),
         effort_60m2_pct = 100 * mensualite / (revenu_courant / 12)) %>%
  select(annee, zone, type_bien, effort_60m2_pct)
cat("\n§4 Taux d'effort pour acheter 60 m² au prix médian, revenu médian (%) :\n")
print(as.data.frame(effort %>% mutate(effort_60m2_pct = round(effort_60m2_pct, 1)) %>%
  pivot_wider(names_from = annee, values_from = effort_60m2_pct)), row.names = FALSE)

# ── §5 Tableau de bord de la fluidité ──────────────────────────────────────
transactions <- dvf %>% group_by(annee) %>% summarise(transactions = sum(n), .groups = "drop")
tableau <- credit %>% filter(annee >= 2014) %>%
  left_join(transactions, by = "annee") %>%
  left_join(capacite %>% filter(zone == "France", niveau == "P50", type_bien == "Appartement") %>%
              select(annee, m2_appartement_revenu_median = m2_achetables), by = "annee") %>%
  mutate(indice_credit = 100 * credits_nouveaux_mds_2025 / credits_nouveaux_mds_2025[annee == 2014],
         indice_transactions = 100 * transactions / transactions[annee == 2014],
         indice_capacite = 100 * m2_appartement_revenu_median / m2_appartement_revenu_median[annee == 2014])
f53 <- "Output/recensement_trois_vagues.csv"; f51 <- "Output/fluidite_residentielle.csv"
if (file.exists(f53)) {
  rp <- read_csv(f53, show_col_types = FALSE) %>% filter(bloc == "B1 Mobilite", groupe == "Ensemble") %>%
    transmute(annee = as.integer(annee), mobilite_recensement_pct = mobiles_an_pct)
  tableau <- tableau %>% left_join(rp, by = "annee")
}
if (file.exists(f51)) {
  srcv <- read_csv(f51, show_col_types = FALSE) %>% filter(bloc == "Mobilite (installes < 2 ans)", groupe == "Ensemble") %>%
    transmute(annee = as.integer(annee), mobilite_srcv_pct = mobiles_pct)
  tableau <- tableau %>% left_join(srcv, by = "annee")
}
cat("\n§5 Tableau de bord de la fluidité (indices 2014 = 100) :\n")
print(as.data.frame(tableau %>% mutate(across(where(is.double), ~ round(.x, 1)))), row.names = FALSE)

ecrire(bind_rows(credit %>% mutate(bloc = "Credit", .before = 1),
                 capacite %>% mutate(bloc = "Capacite d'achat", .before = 1),
                 decompo %>% mutate(bloc = "Decomposition revenu-taux-prix", .before = 1),
                 effort %>% mutate(bloc = "Effort pour 60 m2", .before = 1),
                 tableau %>% mutate(bloc = "Tableau de bord", .before = 1)),
       "Output/credit_marche_fluidite.csv")
