# ============================================================================
#  67. Vie en couple et tension du logement, au canton-ou-ville (2014, 2019, 2023)
# ----------------------------------------------------------------------------
#  Le script 66 compare 96 départements. Ici, les ~1 990 cantons-ou-villes de
#  métropole (codes stables de 2014 à 2023), pour comparer des territoires
#  voisins, avec deux mesures de tension :
#   - interne : part des couples sans enfant de 25 à 34 ans qui n'ont pas la
#     place d'un enfant, au recensement de 2014 (comme au script 55) ;
#   - externe : part de la population du canton en zone A bis, A ou B1 du
#     zonage A/B/C du logement (liste des communes de 2022, ministère chargé
#     du logement). Communes rattachées au canton par la table d'appartenance
#     géographique de l'Insee (2022, CANOV). Codes de Paris et de Lyon
#     harmonisés entre millésimes. Poids : population du recensement
#     de 2014 pour les communes découpées en IRIS ; le reste de la population
#     du canton est réparti à parts égales entre ses autres communes.
#  Modèles (personnes de 20 à 34 ans) : en couple, chez ses parents, seul ~
#  tension x millésime | âge x millésime + canton, puis + département x
#  millésime (seule compte alors la tension relative aux voisins du même
#  département), puis + activité et diplôme. Erreurs-types groupées par canton.
#
#  ⚠️ Comparaison de territoires, pas une expérience : les jeunes et les
#     couples se déplacent selon les prix. Le zonage A/B/C est fixé d'après les
#     loyers et les prix, il n'est pas exogène ; celui de 2022 décrit la
#     tension au milieu de la période.
#  Entrées : Data/recensement/FD_INDCVI_*, extraits du script 48,
#            Data/zonage/zonage-abc-communes-2022.csv (data.gouv.fr),
#            Data/zonage/table-appartenance-geo-communes-2022.xlsx (Insee)
#  Sorties : Data/recensement/_extrait/cohabitation_canton.rds (cache),
#            Output/vie_en_couple_canton.csv
# ============================================================================

source("R/00_utils.R")
source("R/00_config.R")
source("R/00_norme_peuplement.R")
suppressPackageStartupMessages({library(data.table); library(fixest); library(readxl)})
setFixest_nthreads(1); setFixest_notes(FALSE)   # un seul fil : voir R/57_erfs_preparation.R

ANS      <- c("2014", "2019", "2023")
FICHIERS <- c("2014" = "FD_INDCVI_2014.txt", "2019" = "FD_INDCVI_2019.csv", "2023" = "FD_INDCVI_2023.csv")
DIPLV    <- c("2014" = "DIPL_15", "2019" = "DIPL", "2023" = "DIPL")
F_CELL   <- file.path(DIR_RP, "_extrait", "cohabitation_canton.rds")
ville    <- function(code) fcase(startsWith(code, "751"), "75056", startsWith(code, "6938"), "69123",
                                 startsWith(code, "132"), "13055", default = code)
diplome4 <- function(x) {
  x <- trimws(as.character(x)); n <- suppressWarnings(as.integer(x))
  fcase(x == "A" | n %in% c(1:3, 11, 12, 101:103), "Infra CAP", x == "B" | n %in% c(13, 104), "CAP-BEP",
        x == "C" | n %in% c(14, 15, 105, 106), "Bac", x == "D" | n %in% c(16:19, 108:111), "Superieur",
        default = "Non renseigne")
}

# ── §0 Extraction : personnes de 20 à 34 ans par canton-ou-ville ───────────
if (!file.exists(F_CELL)) {
  ext <- lapply(ANS, function(an) {
    t0 <- Sys.time()
    x <- fread(file.path(DIR_RP, FICHIERS[[an]]), sep = ";", colClasses = "character", showProgress = FALSE,
               select = c("CANTVILLE", "AGEREV", "SEXE", "IPONDI", "DEPT", "IRIS", "METRODOM", "MOCO", "TACT", "EMPL", DIPLV[[an]]))
    setnames(x, DIPLV[[an]], "DIPL")
    x <- x[METRODOM == "M"]
    x[, `:=`(age = as.integer(AGEREV), w = as.numeric(IPONDI),
             commune = fifelse(substr(IRIS, 1, 1) == "Z", NA_character_, ville(substr(IRIS, 1, 5))))]
    pop <- list(canton = x[, .(pop = sum(w)), by = .(cantville = CANTVILLE)][, annee := an],
                commune = x[!is.na(commune), .(pop = sum(w)), by = .(cantville = CANTVILLE, commune)][, annee := an])
    x <- x[age >= 20 & age <= 34]
    x[, `:=`(cohab = fcase(MOCO %in% c("21", "22"), "En couple", MOCO %in% c("11", "12"), "Chez ses parents",
                           MOCO == "32", "Seul", default = "Autre"),
             act = fcase(TACT == "11" & EMPL %in% c("16", "101"), "Emploi stable",
                         TACT == "11" & EMPL %in% c("11", "12", "13", "14", "15", "102"), "Emploi a duree limitee",
                         TACT == "11", "Non salarie", TACT == "12", "Chomage", TACT == "22", "Etudes",
                         default = "Autre inactivite"),
             dipl = diplome4(DIPL))]
    cel <- x[, .(n = sum(w), obs = .N), by = .(sexe = SEXE, age, cohab, act, dipl, cantville = CANTVILLE, dept = DEPT)][, annee := an]
    cat(an, ":", nrow(x), "personnes de 20 à 34 ans,", nrow(cel), "cellules ;", round(difftime(Sys.time(), t0, units = "secs")), "s\n")
    rm(x); gc(verbose = FALSE)
    list(cel = cel, pop_canton = pop$canton, pop_commune = pop$commune)
  })
  saveRDS(list(cel = rbindlist(lapply(ext, `[[`, "cel")), pop_canton = rbindlist(lapply(ext, `[[`, "pop_canton")),
               pop_commune = rbindlist(lapply(ext, `[[`, "pop_commune"))), F_CELL)
}
E <- readRDS(F_CELL)
#  Paris et la métropole de Lyon changent de code en 2023 (75ZZ -> 7599, 69ZZ -> 6999, codes de la table
#  de l'Insee) ; les autres changements touchent de petits cantons (moins de 20 000 habitants).
harmoniser <- function(x) dplyr::recode(x, "75ZZ" = "7599", "69ZZ" = "6999")
E <- lapply(E, function(t) { t[, cantville := harmoniser(cantville)]; t })
ce <- as_tibble(E$cel) %>% mutate(sexe = if_else(sexe == "2", "Femmes", "Hommes"))
cat("Cantons-ou-villes par millésime :", paste(tapply(ce$cantville, ce$annee, n_distinct), collapse = " / "), "\n")

# ── §1 Tension par canton ──────────────────────────────────────────────────
#  a. Interne, recensement de 2014
t_int <- readRDS(file.path(DIR_RP, "_extrait", "couples_RP2014.rds")) %>%
  filter(metrodom == "M", !is.na(pieces), nb_enf_avant == 0, age_femme >= 25, age_femme <= 34) %>%
  mutate(cantville = harmoniser(sub(" .*", "", hh))) %>% group_by(cantville) %>%
  summarise(tension_int = 100 * sum(poids[place != PLACE[1]]) / sum(poids), couples_obs = n(), .groups = "drop")
cat("\nTension interne (couples sans enfant de 25-34 ans sans place, 2014) : couples observés par canton\n")
print(quantile(t_int$couples_obs, c(.1, .25, .5, .75, .9)))
#  b. Zonage A/B/C, pondéré par la population de 2014
cog <- read_excel(file.path("Data", "zonage", "table-appartenance-geo-communes-2022.xlsx"), sheet = "COM", skip = 5) %>%
  transmute(commune = CODGEO, cantville = CANOV, dep = DEP)
abc <- read.csv(file.path("Data", "zonage", "zonage-abc-communes-2022.csv"), colClasses = "character", encoding = "UTF-8") %>%
  transmute(commune = id, zone = zonage)
pc14 <- as_tibble(E$pop_canton) %>% filter(annee == "2014") %>% select(cantville, pop_canton = pop)
pm14 <- as_tibble(E$pop_commune) %>% filter(annee == "2014") %>% select(commune, pop_iris = pop)
z <- cog %>% left_join(abc, by = "commune") %>% left_join(pm14, by = "commune") %>%
  filter(cantville %in% pc14$cantville) %>% group_by(cantville) %>%
  mutate(n_autres = sum(is.na(pop_iris)), connu = sum(pop_iris, na.rm = TRUE)) %>% ungroup() %>%
  left_join(pc14, by = "cantville") %>%
  mutate(w = coalesce(pop_iris, pmax(pop_canton - connu, 0) / pmax(n_autres, 1)))
cat("Communes sans zone A/B/C :", sum(is.na(z$zone)), "| cantons du recensement rattachés :", n_distinct(z$cantville),
    "sur", nrow(pc14), "\n")
t_abc <- z %>% filter(!is.na(zone)) %>% group_by(cantville) %>%
  summarise(tendu_abc = 100 * sum(w[zone %in% c("Abis", "A", "B1")]) / sum(w),
            zone_maj = names(which.max(tapply(w, zone, sum))), .groups = "drop") %>%
  mutate(zone_cl = factor(case_when(zone_maj %in% c("Abis", "A") ~ "A et A bis", TRUE ~ zone_maj),
                          levels = c("C", "B2", "B1", "A et A bis")))
tension <- full_join(t_int, t_abc, by = "cantville")
cat("\nPart de la population en zone A bis, A ou B1, par canton (%) :\n"); print(round(quantile(tension$tendu_abc, c(.1, .25, .5, .75, .9), na.rm = TRUE), 1))
cat("Cantons par zone majoritaire :\n"); print(table(tension$zone_cl, useNA = "ifany"))
cat("Corrélation tension interne / zonage :", round(cor(tension$tension_int, tension$tendu_abc, use = "complete.obs"), 2), "\n")

# ── §2 Descriptif : part en couple à 25-29 ans selon la zone ───────────────
d <- ce %>% inner_join(tension, by = "cantville") %>%
  mutate(couple = cohab == "En couple", parents = cohab == "Chez ses parents", seul = cohab == "Seul",
         t_int_10 = tension_int / 10, abc_10 = tendu_abc / 10)
part <- function(x, w) 100 * sum(w[x]) / sum(w)
par_zone <- d %>% filter(age %in% 25:29, !is.na(zone_cl)) %>% group_by(sexe, zone_cl, annee) %>%
  summarise(en_couple = part(couple, n), chez_ses_parents = part(parents, n), seul = part(seul, n),
            personnes = sum(n), .groups = "drop")
cat("\n25-29 ans selon la zone A/B/C majoritaire du canton (%) :\n")
print(as.data.frame(par_zone %>% select(-personnes) %>% mutate(across(where(is.double), ~ round(.x, 1))) %>%
                      pivot_wider(names_from = annee, values_from = c(en_couple, chez_ses_parents, seul))), row.names = FALSE)

# ── §3 Modèles ─────────────────────────────────────────────────────────────
modele <- function(x, y, tv, fe, ctrl = "") {
  m <- feols(as.formula(paste0(y, " ~ i(annee, ", tv, ", ref = '2014') ", ctrl, " | ", fe)),
             data = x, weights = ~n, cluster = ~cantville)
  ct <- as.data.frame(coeftable(m)); ct$terme <- rownames(ct)
  ct %>% filter(grepl(tv, terme), grepl("annee::", terme)) %>%
    transmute(annee = sub("annee::([0-9]+):.*", "\\1", terme), effet_pts = 100 * Estimate, se = 100 * `Std. Error`,
              p = `Pr(>|t|)`, n_cantons = n_distinct(x$cantville))
}
TENSIONS <- c("Interne (sans place, 2014)" = "t_int_10", "Zonage A/B/C (part en A bis, A, B1)" = "abc_10")
SPECS <- list("Age x millesime + canton" = list(fe = "age^annee + cantville", ctrl = ""),
              "+ departement x millesime" = list(fe = "age^annee + cantville + dept^annee", ctrl = ""),
              "+ activite et diplome" = list(fe = "age^annee + cantville + dept^annee", ctrl = "+ act + dipl"))
ISSUES <- c("En couple" = "couple", "Chez ses parents" = "parents", "Seul" = "seul")
GROUPES <- list("20-34 ans" = 20:34, "25-29 ans" = 25:29)
modeles <- map_dfr(c("Femmes", "Hommes"), function(sx) imap_dfr(GROUPES, function(ages, g) imap_dfr(TENSIONS, function(tv, tl)
  imap_dfr(SPECS, function(sp, sl) imap_dfr(ISSUES, function(y, yl) {
    x <- filter(d, sexe == sx, age %in% ages, !is.na(.data[[tv]]))
    modele(x, y, tv, sp$fe, sp$ctrl) %>% mutate(sexe = sx, groupe = g, tension = tl, specification = sl, issue = yl, .before = 1)
  })))))
cat("\nEffet de 10 points de tension sur l'évolution depuis 2014 (points de pourcentage) :\n")
print(as.data.frame(modeles %>% filter(annee == "2023") %>% select(-annee) %>% mutate(across(where(is.double), ~ round(.x, 3)))),
      row.names = FALSE)

#  Ordre de grandeur : recul de la part en couple à 25-29 ans, et ce que la tension en explique
#  (effet x tension moyenne, pour un canton qui n'aurait aucune tension)
ordre <- d %>% filter(age %in% 25:29) %>% group_by(sexe, annee) %>% summarise(en_couple = part(couple, n), .groups = "drop") %>%
  pivot_wider(names_from = annee, values_from = en_couple, names_prefix = "couple_") %>%
  mutate(recul_pts = couple_2023 - couple_2014) %>%
  left_join(d %>% filter(age %in% 25:29) %>% group_by(sexe) %>%
              summarise(tension_int_moy = sum(n * tension_int, na.rm = TRUE) / sum(n[!is.na(tension_int)]),
                        abc_moy = sum(n * tendu_abc, na.rm = TRUE) / sum(n[!is.na(tendu_abc)]), .groups = "drop"), by = "sexe")
cat("\nRecul de la part en couple à 25-29 ans et tension moyenne :\n"); print(as.data.frame(ordre %>% mutate(across(where(is.double), ~ round(.x, 2)))))

# ── Sortie ─────────────────────────────────────────────────────────────────
ecrire(bind_rows(par_zone %>% mutate(bloc = "25-29 ans selon la zone A/B/C", zone_cl = as.character(zone_cl), .before = 1),
                 modeles %>% mutate(bloc = "Modeles", .before = 1),
                 ordre %>% mutate(bloc = "Recul et tension moyenne, 25-29 ans", .before = 1)),
       "Output/vie_en_couple_canton.csv")
