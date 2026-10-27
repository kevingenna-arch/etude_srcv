# ============================================================================
#  SRCV — 48. Recensement 2014, 2019 et 2023 : extraction des couples féconds
# ----------------------------------------------------------------------------
#  Fichiers détail Insee « individus localisés au canton-ou-ville » (INDCVI) :
#  RP 2014 (collectes 2012 à 2016), RP 2019 (2017 à 2021) et RP 2023 (2021 à 2025), environ
#  18 à 20 millions de lignes chacun. On en tire, pour chaque ménage ordinaire dont
#  la famille principale est un couple où la femme a de 15 à 49 ans :
#   - la naissance : un enfant de la famille âgé de moins d'un an (AGEREV = 0),
#     né dans les douze mois précédant la collecte ;
#   - le logement : pièces (NBPI), surface en tranches (SURF), statut (STOCD),
#     type (TYPL), ancienneté d'emménagement (ANEMR), logement de la femme au
#     1er janvier de l'année précédente (IRAN) ;
#   - la composition du ménage AVANT la naissance (un nouveau-né retiré), pour
#     la norme de peuplement (R/00_norme_peuplement.R) ;
#   - la population de la commune, calculée sur le recensement lui-même : les
#     communes découpées en IRIS sont identifiées ; les autres (moins de 5 000
#     habitants environ) forment une classe à part.
#  Couples de même sexe écartés (la femme du couple n'est pas définie).
#  Sorties (dans Data/, non versionné) :
#    Data/recensement/_extrait/couples_RP2014.rds, couples_RP2019.rds, couples_RP2023.rds
# ============================================================================

source("R/00_utils.R")
source("R/00_config.R")
source("R/00_norme_peuplement.R")
suppressPackageStartupMessages(library(data.table))

FICHIERS <- c("2014" = "FD_INDCVI_2014.txt", "2019" = "FD_INDCVI_2019.csv", "2023" = "FD_INDCVI_2023.csv")
CS       <- c("2014" = "CS1", "2019" = "CS1", "2023" = "GS")   # catégorie sociale : 8 postes en 2014-2019, 6 en 2023
DIPLV    <- c("2014" = "DIPL_15", "2019" = "DIPL", "2023" = "DIPL")
COLS <- c("CANTVILLE", "NUMMI", "AGEREV", "SEXE", "LPRF", "NUMF", "TYPFC", "CATL", "NBPI",
          "SURF", "STOCD", "TYPL", "ANEMR", "IRAN", "IPONDI", "TACT", "DEPT",
          "METRODOM", "IRIS", "STAT_CONJ", "HLML", "INPER")
DIR_EXTRAIT <- file.path(DIR_RP, "_extrait")
dir.create(DIR_EXTRAIT, showWarnings = FALSE)

# Diplôme en 5 postes : codes 01-19 en 2019, 101-111 en 2023 (pas de recouvrement) ;
# non disponible en 2014 (DIPL_15 n'a que 4 postes).
diplome5 <- function(x) {
  x <- suppressWarnings(as.integer(x))
  factor(case_when(x %in% c(1:3, 11, 12, 101:103) ~ "Infra CAP", x %in% c(13, 104) ~ "CAP-BEP",
                   x %in% c(14, 15, 105, 106) ~ "Bac", x %in% c(16, 108) ~ "Bac+2",
                   x %in% c(17:19, 109:111) ~ "Bac+3 et plus"),
         levels = c("Bac", "Infra CAP", "CAP-BEP", "Bac+2", "Bac+3 et plus"))
}
# Diplôme en 4 postes, commun aux trois millésimes (DIPL_15 de 2014 : A à D)
diplome4 <- function(x) {
  x <- trimws(as.character(x)); n <- suppressWarnings(as.integer(x))
  factor(case_when(x == "A" | n %in% c(1:3, 11, 12, 101:103) ~ "Infra CAP",
                   x == "B" | n %in% c(13, 104) ~ "CAP-BEP",
                   x == "C" | n %in% c(14, 15, 105, 106) ~ "Bac",
                   x == "D" | n %in% c(16:19, 108:111) ~ "Superieur"),
         levels = c("Bac", "Infra CAP", "CAP-BEP", "Superieur"))
}

extraire <- function(an) {
  t0 <- Sys.time()
  x <- fread(file.path(DIR_RP, FICHIERS[[an]]), sep = ";", select = c(COLS, CS[[an]], DIPLV[[an]]),
             colClasses = "character", showProgress = FALSE)
  setnames(x, c(CS[[an]], DIPLV[[an]]), c("cs", "DIPL"))
  x[, IPONDI := as.numeric(IPONDI)]
  cat(an, ":", nrow(x), "individus lus en", round(difftime(Sys.time(), t0, units = "secs")), "s\n")

  # Population des communes découpées en IRIS (Paris, Lyon, Marseille : villes entières)
  x[, commune := fifelse(substr(IRIS, 1, 1) == "Z", NA_character_, substr(IRIS, 1, 5))]
  x[startsWith(commune, "751"), commune := "75056"]
  x[startsWith(commune, "6938"), commune := "69123"]
  x[startsWith(commune, "132"), commune := "13055"]
  pop <- x[!is.na(commune), .(pop_commune = sum(IPONDI)), by = commune]

  # Ménages ordinaires
  x <- x[CATL == "1"]
  x[, `:=`(hh = paste(CANTVILLE, NUMMI), age = as.integer(AGEREV))]
  couple1 <- x[NUMF == "1" & LPRF %in% c("1", "2") & TYPFC == "2"]
  sexes <- couple1[, .(n_femmes = sum(SEXE == "2"), n_hommes = sum(SEXE == "1")), by = hh]
  hh_ok <- sexes[n_femmes == 1 & n_hommes == 1, hh]
  femmes <- couple1[SEXE == "2" & hh %in% hh_ok & age >= 15 & age <= 49]
  conj <- couple1[SEXE == "1" & hh %in% femmes$hh, .(hh, age_conj = age, tact_conj = TACT)]
  m <- x[hh %in% femmes$hh]

  # Naissance et enfants de la famille principale
  enf <- m[NUMF == "1" & LPRF == "3",
           .(naiss01 = as.integer(any(age == 0)), n_enf = .N, n_age1 = sum(age == 1)), by = hh]

  # Composition hors couple, un nouveau-né retiré
  autres <- m[!(NUMF == "1" & LPRF %in% c("1", "2"))]
  autres[, nn := NUMF == "1" & LPRF == "3" & age == 0]
  autres[, k := cumsum(nn), by = hh]
  autres <- autres[!(nn & k == 1)]
  compo <- autres[, .(jg = sum(age < 7 & SEXE == "1"), jf = sum(age < 7 & SEXE == "2"),
                      ag = sum(age >= 7 & age < 19 & SEXE == "1"),
                      af = sum(age >= 7 & age < 19 & SEXE == "2"),
                      adultes = sum(age >= 19)), by = hh]

  d <- femmes[, .(hh, annee = an, poids = IPONDI, age_femme = age, diplome = diplome5(DIPL),
                  diplome4 = diplome4(DIPL),
                  tact = TACT, cs, stat_conj = STAT_CONJ, iran = IRAN, pieces = as.integer(NBPI),
                  surf = SURF, stocd = STOCD, typl = TYPL, anemr = ANEMR, hlml = HLML,
                  dept = DEPT, metrodom = METRODOM, commune, inper = as.integer(INPER))]
  d <- merge(d, conj, by = "hh", all.x = TRUE)
  d <- merge(d, enf, by = "hh", all.x = TRUE)
  d <- merge(d, compo, by = "hh", all.x = TRUE)
  d <- merge(d, pop, by = "commune", all.x = TRUE)
  d[, `:=`(naiss01 = coalesce(naiss01, 0L), n_enf = coalesce(n_enf, 0L),
           n_age1 = coalesce(n_age1, 0L))]
  d[, nb_enf_avant := n_enf - naiss01]
  d <- as_tibble(d) %>% ajouter_place()

  cat(an, ":", nrow(d), "couples,", sum(d$naiss01), "naissances ;",
      round(difftime(Sys.time(), t0, units = "secs")), "s au total\n")
  saveRDS(d, file.path(DIR_EXTRAIT, paste0("couples_RP", an, ".rds")))
  rm(x, m, autres); gc(verbose = FALSE)
  invisible(d)
}

for (an in names(FICHIERS)) extraire(an)
