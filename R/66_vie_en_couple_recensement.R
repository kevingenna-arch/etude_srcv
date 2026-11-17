# ============================================================================
#  66. Vie en couple et baisse des naissances : recensement 2014, 2019, 2023
# ----------------------------------------------------------------------------
#  Dans la décomposition de la baisse de l'ICF (scripts 34, 39, 41), le recul
#  de la vie en couple est le premier facteur (un tiers), mesuré dans SRCV sur
#  quelques milliers de femmes. Ce script le reprend sur les fichiers détail
#  du recensement (toutes les personnes de 15 à 49 ans, France métropolitaine),
#  et cherche ce qui le remplace et ce qui l'accompagne.
#
#  0. Extraction : cellules année x sexe x âge x mode de cohabitation (MOCO) x
#     activité x diplôme x département x taille de commune, avec les
#     naissances des femmes (un enfant de moins d'un an dans leur famille).
#  1. Mode de cohabitation par âge : en couple, chez ses parents, seul, parent
#     seul, hors famille à plusieurs (colocation), hors ménage.
#  2. Décomposition de la baisse des naissances par femme (somme des taux par
#     âge, un ICF du recensement) : part des femmes en couple à âge donné,
#     taux des couples, taux hors couple. Contre-épreuve des 34 % de SRCV.
#  3. Ce qui accompagne le recul : études, diplôme, emploi. Part du recul qui
#     disparaît à composition de 2014 (activité x diplôme, à âge donné).
#  4. Logement : le recul est-il plus fort là où se loger est difficile
#     (tension de 2014 par département, script 55 ; taille de commune) ?
#
#  ⚠️ Naissance = nouveau-né vivant avec sa mère au recensement ; couple =
#     couple cohabitant (MOCO), pas la vie en couple déclarée. L'emploi et le
#     couple se décident ensemble : §3 décrit des associations. Le type de
#     contrat n'a plus que deux postes en 2023, et l'apprentissage a beaucoup
#     progressé depuis 2018. Les familles quittent les zones tendues : §4 est
#     indicatif.
#  Entrées : Data/recensement/FD_INDCVI_*.{txt,csv}, extraits du script 48
#  Sorties : Data/recensement/_extrait/cohabitation_cellules.rds (cache),
#            Output/vie_en_couple_recensement.csv
# ============================================================================

source("R/00_utils.R")
source("R/00_config.R")
source("R/00_norme_peuplement.R")
suppressPackageStartupMessages({library(data.table); library(fixest)})
setFixest_nthreads(1); setFixest_notes(FALSE)   # un seul fil : voir R/57_erfs_preparation.R

ANS      <- c("2014", "2019", "2023")
FICHIERS <- c("2014" = "FD_INDCVI_2014.txt", "2019" = "FD_INDCVI_2019.csv", "2023" = "FD_INDCVI_2023.csv")
DIPLV    <- c("2014" = "DIPL_15", "2019" = "DIPL", "2023" = "DIPL")
COHAB    <- c("En couple", "Chez ses parents", "Seul", "Parent seul", "Hors famille, a plusieurs", "Hors menage")
F_CELL   <- file.path(DIR_RP, "_extrait", "cohabitation_cellules.rds")
ville    <- function(code) fcase(startsWith(code, "751"), "75056", startsWith(code, "6938"), "69123",
                                 startsWith(code, "132"), "13055", default = code)
# Diplôme en 4 postes, commun aux trois millésimes (comme au script 48)
diplome4 <- function(x) {
  x <- trimws(as.character(x)); n <- suppressWarnings(as.integer(x))
  fcase(x == "A" | n %in% c(1:3, 11, 12, 101:103), "Infra CAP", x == "B" | n %in% c(13, 104), "CAP-BEP",
        x == "C" | n %in% c(14, 15, 105, 106), "Bac", x == "D" | n %in% c(16:19, 108:111), "Superieur",
        default = "Non renseigne")
}

# ── §0 Extraction ──────────────────────────────────────────────────────────
if (!file.exists(F_CELL)) {
  cellules <- rbindlist(lapply(ANS, function(an) {
    t0 <- Sys.time()
    x <- fread(file.path(DIR_RP, FICHIERS[[an]]), sep = ";", colClasses = "character", showProgress = FALSE,
               select = c("CANTVILLE", "NUMMI", "NUMF", "LPRF", "AGEREV", "SEXE", "IPONDI", "DEPT", "IRIS",
                          "METRODOM", "MOCO", "TACT", "EMPL", DIPLV[[an]]))
    setnames(x, DIPLV[[an]], "DIPL")
    x <- x[METRODOM == "M"]
    x[, `:=`(age = as.integer(AGEREV), w = as.numeric(IPONDI),
             commune = fifelse(substr(IRIS, 1, 1) == "Z", NA_character_, ville(substr(IRIS, 1, 5))),
             fam = paste(CANTVILLE, NUMMI, NUMF))]
    pop <- x[!is.na(commune), .(pop = sum(w)), by = commune]
    bebes <- unique(x[LPRF == "3" & age == 0 & MOCO %in% c("11", "12"), .(fam)])[, bebe := 1L]
    x <- x[age >= 15 & age <= 49]
    cat(an, ": type de contrat des personnes en emploi (EMPL) :\n"); print(x[TACT == "11", .N, keyby = EMPL])
    x[bebes, bebe := i.bebe, on = "fam"]
    x[pop, pop := i.pop, on = "commune"]
    x[, `:=`(
      naiss = as.integer(SEXE == "2" & LPRF %in% c("1", "2") & !is.na(bebe)),
      cohab = fcase(MOCO %in% c("21", "22"), COHAB[1], MOCO %in% c("11", "12"), COHAB[2], MOCO == "32", COHAB[3],
                    MOCO == "23", COHAB[4], MOCO == "31", COHAB[5], default = COHAB[6]),
      act = fcase(TACT == "11" & EMPL %in% c("16", "101"), "Emploi stable",
                  TACT == "11" & EMPL %in% c("11", "12", "13", "14", "15", "102"), "Emploi a duree limitee",
                  TACT == "11", "Non salarie", TACT == "12", "Chomage", TACT == "22", "Etudes",
                  default = "Autre inactivite"),
      dipl = diplome4(DIPL),
      com_cl = fcase(commune == "75056", "Paris", DEPT %in% c("92", "93", "94"), "Petite couronne",
                     is.na(pop), "Commune non decoupee en IRIS", pop < 20000, "Moins de 20 000 hab.",
                     pop < 100000, "20 000 a 99 999 hab.", default = "100 000 hab. ou plus"))]
    out <- x[, .(n = sum(w), naiss = sum(w * naiss), obs = .N),
             by = .(sexe = SEXE, age, cohab, act, dipl, dept = DEPT, com_cl)][, annee := an]
    cat(an, ":", nrow(x), "personnes de 15 à 49 ans,", nrow(out), "cellules ;",
        round(difftime(Sys.time(), t0, units = "secs")), "s\n")
    rm(x); gc(verbose = FALSE)
    out
  }))
  saveRDS(cellules, F_CELL)
}
ce <- as_tibble(readRDS(F_CELL)) %>%
  mutate(sexe = if_else(sexe == "2", "Femmes", "Hommes"), couple = cohab == "En couple",
         age5 = cut(age, c(14, 19, 24, 29, 34, 39, 44, 49),
                    labels = c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49")))
cat("Cellules :", nrow(ce), "| personnes de 15 à 49 ans (millions, pondéré) :\n")
print(as.data.frame(ce %>% group_by(annee, sexe) %>% summarise(millions = sum(n) / 1e6, obs = sum(obs), .groups = "drop")))
part <- function(x, w) 100 * sum(w[x]) / sum(w)

# ── §1 Mode de cohabitation par âge ────────────────────────────────────────
cohabitation <- ce %>% group_by(sexe, age5, annee, cohab) %>% summarise(n = sum(n), .groups = "drop_last") %>%
  mutate(part_pct = 100 * n / sum(n)) %>% ungroup() %>% select(-n)
cat("\nMode de cohabitation des femmes, par âge (%) :\n")
print(as.data.frame(cohabitation %>% filter(sexe == "Femmes", age5 %in% c("20-24", "25-29", "30-34", "35-39")) %>%
                      mutate(part_pct = round(part_pct, 1)) %>% pivot_wider(names_from = annee, values_from = part_pct) %>%
                      mutate(variation = `2023` - `2014`)), row.names = FALSE)
cat("\nMode de cohabitation des hommes, 25-29 et 30-34 ans (%) :\n")
print(as.data.frame(cohabitation %>% filter(sexe == "Hommes", age5 %in% c("25-29", "30-34")) %>%
                      mutate(part_pct = round(part_pct, 1)) %>% pivot_wider(names_from = annee, values_from = part_pct) %>%
                      mutate(variation = `2023` - `2014`)), row.names = FALSE)
en_couple_1549 <- ce %>% group_by(sexe, annee) %>% summarise(en_couple_pct = part(couple, n), .groups = "drop")
cat("\nPart des 15-49 ans en couple (%) :\n"); print(as.data.frame(en_couple_1549 %>% pivot_wider(names_from = annee, values_from = en_couple_pct)))

# ── §2 Décomposition de la baisse des naissances par femme ─────────────────
#  Par âge a : naissances par femme = p_a x taux des couples + (1 - p_a) x taux hors couple.
#  Somme sur les âges = un ICF du recensement. Entre deux dates : effet de la part en couple
#  (aux taux de départ), des taux des couples et des taux hors couple (à la composition d'arrivée).
fa <- ce %>% filter(sexe == "Femmes") %>% group_by(annee, age, couple) %>%
  summarise(n = sum(n), naiss = sum(naiss), .groups = "drop") %>%
  group_by(annee, age) %>% mutate(p = n / sum(n)) %>% ungroup() %>% mutate(r = naiss / n)
large <- fa %>% mutate(k = if_else(couple, "c", "h")) %>% select(annee, age, k, p, r) %>%
  pivot_wider(names_from = k, values_from = c(p, r))
icf_rp <- large %>% group_by(annee) %>%
  summarise(icf = sum(p_c * r_c + p_h * r_h), dont_couples = sum(p_c * r_c), dont_hors_couple = sum(p_h * r_h),
            part_naissances_en_couple_pct = 100 * dont_couples / icf, .groups = "drop")
cat("\nNaissances par femme au recensement (somme des taux par âge) :\n"); print(as.data.frame(icf_rp))
decomposer <- function(a0, a1) {
  j <- inner_join(filter(large, annee == a0), filter(large, annee == a1), by = "age", suffix = c("0", "1")) %>%
    mutate(composition = (p_c1 - p_c0) * (r_c0 - r_h0), taux_couples = p_c1 * (r_c1 - r_c0),
           taux_hors_couple = p_h1 * (r_h1 - r_h0),
           age5 = cut(age, c(14, 24, 29, 34, 49), labels = c("15-24", "25-29", "30-34", "35-49")))
  tot <- sum(j$composition + j$taux_couples + j$taux_hors_couple)
  bind_rows(
    tibble(groupe = "Ensemble", composition = sum(j$composition), taux_couples = sum(j$taux_couples),
           taux_hors_couple = sum(j$taux_hors_couple)),
    j %>% group_by(groupe = as.character(age5)) %>%
      summarise(across(c(composition, taux_couples, taux_hors_couple), sum), .groups = "drop")) %>%
    mutate(periode = paste(a0, a1, sep = "-"), variation_icf = tot,
           across(c(composition, taux_couples, taux_hors_couple), ~ 100 * .x / tot, .names = "part_{.col}_pct"),
           .before = 1)
}
decomposition <- bind_rows(decomposer("2014", "2023"), decomposer("2014", "2019"), decomposer("2019", "2023"))
cat("\nDécomposition de la variation (parts en % de la baisse) :\n")
print(as.data.frame(decomposition %>% mutate(across(where(is.double), ~ round(.x, 3)))), row.names = FALSE)

# ── §2 bis L'âge des couples et leur parité ────────────────────────────────
#  Le diaporama attribuait 4 % de la baisse au vieillissement des couples (SRCV, âge transposé à
#  parité donnée, script 23). Sur le recensement, recomposer le taux BRUT des couples par classe
#  d'âge en donne 21 %. Les deux ne mesurent pas la même chose :
#   (a) taux brut des femmes et des couples : il dépend de la pyramide des âges, que l'ICF neutralise ;
#   (b) ICF : à âge donné, la structure par âge n'a plus de rôle ; reste, dans le taux des couples à
#       âge donné, la répartition par nombre d'enfants déjà nés (parité) et les taux par parité.
#  Couples de la famille principale (extraits du script 48), femme de 15 à 49 ans.
brut <- ce %>% filter(sexe == "Femmes") %>% group_by(annee) %>%
  summarise(taux_brut_femmes_pct = 100 * sum(naiss) / sum(n), age_moyen_femmes = sum(n * age) / sum(n),
            taux_brut_couples_pct = 100 * sum(naiss[couple]) / sum(n[couple]),
            age_moyen_couples = sum((n * age)[couple]) / sum(n[couple]), .groups = "drop") %>%
  left_join(icf_rp %>% select(annee, icf), by = "annee")
cat("\nTaux brut et somme des taux par âge (le premier dépend de la pyramide, pas le second) :\n")
print(as.data.frame(brut %>% mutate(across(where(is.double), ~ round(.x, 3)))))

cp <- map_dfr(ANS, ~ readRDS(file.path(DIR_RP, "_extrait", paste0("couples_RP", .x, ".rds")))) %>%
  filter(metrodom == "M") %>%
  transmute(annee, poids, naiss01, age = age_femme, parite = pmin(nb_enf_avant, 3L),
            age_cl = cut(age_femme, c(14, 24, 29, 34, 39, 44, 49)))
#  (a) Recomposition du taux brut des couples (taux de 2023 aux parts de 2014) selon la variable de cellule
recomposer_brut <- function(var) {
  t <- cp %>% filter(annee %in% c("2014", "2023")) %>% group_by(annee, cle = interaction(across(all_of(var)))) %>%
    summarise(w = sum(poids), r = sum(poids * naiss01) / sum(poids), .groups = "drop") %>%
    group_by(annee) %>% mutate(s = w / sum(w)) %>% ungroup() %>% select(-w) %>%
    pivot_wider(names_from = annee, values_from = c(s, r), values_fill = 0)
  obs0 <- sum(t$s_2014 * t$r_2014); obs1 <- sum(t$s_2023 * t$r_2023); cf <- sum(t$s_2014 * t$r_2023)
  tibble(cellules = paste(var, collapse = " x "), taux_2014_pct = 100 * obs0, taux_2023_pct = 100 * obs1,
         taux_2023_parts_2014_pct = 100 * cf, part_de_la_baisse_pct = 100 * (cf - obs1) / (obs0 - obs1))
}
recomp_age <- map_dfr(list("age_cl", "age", "parite", c("age", "parite")), recomposer_brut)
#  Âge à parité donnée, la répartition par parité restant celle de 2023 : le calcul du script 23 sur SRCV
cel <- cp %>% filter(annee %in% c("2014", "2023")) %>% group_by(annee, parite, age) %>%
  summarise(w = sum(poids), r = sum(poids * naiss01) / sum(poids), .groups = "drop") %>%
  group_by(annee, parite) %>% mutate(sa = w / sum(w)) %>% ungroup()
Sk1 <- cp %>% filter(annee == "2023") %>% count(parite, wt = poids, name = "Sk") %>% mutate(Sk = Sk / sum(Sk))
jp <- full_join(cel %>% filter(annee == "2014") %>% select(parite, age, sa0 = sa, r0 = r),
                cel %>% filter(annee == "2023") %>% select(parite, age, sa1 = sa, r1 = r), by = c("parite", "age")) %>%
  mutate(across(c(sa0, sa1, r0, r1), ~ coalesce(.x, 0))) %>% left_join(Sk1, by = "parite")
recomp_age <- bind_rows(recomp_age, tibble(
  cellules = "age dans parite (parite de 2023)", taux_2014_pct = recomp_age$taux_2014_pct[1],
  taux_2023_pct = 100 * sum(jp$Sk * jp$sa1 * jp$r1), taux_2023_parts_2014_pct = 100 * sum(jp$Sk * jp$sa0 * jp$r1)) %>%
  mutate(part_de_la_baisse_pct = 100 * (taux_2023_parts_2014_pct - taux_2023_pct) / (taux_2014_pct - taux_2023_pct)))
cat("\n(a) Taux brut des couples : part de la baisse due à la recomposition des cellules (%) :\n")
print(as.data.frame(recomp_age %>% mutate(across(where(is.double), ~ round(.x, 2)))), row.names = FALSE)

#  (b) Dans l'ICF : le terme « taux des couples à âge donné » du §2, scindé entre la répartition par
#  parité à chaque âge et les taux par parité ; poids : part en couple de 2023 (§2).
ap <- cp %>% group_by(annee, age, parite) %>% summarise(w = sum(poids), r = sum(poids * naiss01) / sum(poids), .groups = "drop") %>%
  group_by(annee, age) %>% mutate(s = w / sum(w)) %>% ungroup()
scinder <- function(a0, a1) {
  j <- full_join(filter(ap, annee == a0) %>% select(age, parite, s0 = s, r0 = r),
                 filter(ap, annee == a1) %>% select(age, parite, s1 = s, r1 = r), by = c("age", "parite")) %>%
    mutate(across(c(s0, s1, r0, r1), ~ coalesce(.x, 0))) %>%
    left_join(filter(large, annee == a1) %>% select(age, p_c1 = p_c), by = "age") %>%
    mutate(parite_comp = p_c1 * (s1 - s0) * r0, taux_parite = p_c1 * s1 * (r1 - r0))
  tot <- decomposition %>% filter(periode == paste(a0, a1, sep = "-"), groupe == "Ensemble") %>% pull(variation_icf)
  bind_rows(
    tibble(rang = "Ensemble", parite_comp = sum(j$parite_comp), taux_parite = sum(j$taux_parite)),
    j %>% group_by(rang = c("1er enfant", "2e enfant", "3e enfant", "4e ou plus")[parite + 1]) %>%
      summarise(parite_comp = sum(parite_comp), taux_parite = sum(taux_parite), .groups = "drop")) %>%
    mutate(periode = paste(a0, a1, sep = "-"), variation_icf = tot,
           part_parite_pct = 100 * parite_comp / tot, part_taux_par_parite_pct = 100 * taux_parite / tot, .before = 1)
}
parite <- bind_rows(scinder("2014", "2023"), scinder("2014", "2019"), scinder("2019", "2023"))
cat("\n(b) ICF, taux des couples à âge donné : répartition par parité et taux par parité (% de la baisse) :\n")
print(as.data.frame(parite %>% mutate(across(where(is.double), ~ round(.x, 3)))), row.names = FALSE)

# ── §3 Ce qui accompagne le recul : études, diplôme, emploi ────────────────
jeunes <- ce %>% filter(age >= 20, age <= 34)
activite <- jeunes %>% group_by(sexe, age5, annee, act) %>% summarise(couple_pct = part(couple, n), n = sum(n), .groups = "drop_last") %>%
  mutate(part_pct = 100 * n / sum(n)) %>% ungroup() %>% select(-n)
cat("\nFemmes de 25-29 ans : répartition par activité et part en couple (%) :\n")
print(as.data.frame(activite %>% filter(sexe == "Femmes", age5 == "25-29") %>%
                      mutate(across(where(is.double), ~ round(.x, 1))) %>%
                      pivot_wider(names_from = annee, values_from = c(part_pct, couple_pct))), row.names = FALSE)
#  À structure par âge de 2023 : part en couple observée en 2014 et 2023, et en 2023 avec la
#  composition de 2014 (activité, diplôme, ou les deux) à chaque âge.
recomposer <- function(x, vars) {
  A <- x %>% filter(annee == "2023") %>% group_by(age) %>% summarise(a = sum(n), .groups = "drop") %>% mutate(a = a / sum(a))
  k <- x %>% group_by(annee, age, across(all_of(vars))) %>%
    summarise(c = sum(n[couple]) / sum(n), n = sum(n), .groups = "drop") %>%
    group_by(annee, age) %>% mutate(s = n / sum(n)) %>% ungroup()
  w <- k %>% select(-n) %>% pivot_wider(names_from = annee, values_from = c(s, c), values_fill = 0)
  w %>% group_by(age) %>%
    summarise(obs14 = sum(s_2014 * c_2014), obs23 = sum(s_2023 * c_2023), cf = sum(s_2014 * c_2023), .groups = "drop") %>%
    inner_join(A, by = "age") %>%
    summarise(couple_2014 = 100 * sum(a * obs14), couple_2023 = 100 * sum(a * obs23),
              couple_2023_composition_2014 = 100 * sum(a * cf)) %>%
    mutate(recul_pts = couple_2023 - couple_2014,
           part_du_recul_pct = 100 * (couple_2023_composition_2014 - couple_2023) / (couple_2014 - couple_2023))
}
COMPO <- list("Activite" = "act", "Diplome" = "dipl", "Activite et diplome" = c("act", "dipl"))
GROUPES <- list("20-34 ans" = 20:34, "20-24 ans" = 20:24, "25-29 ans" = 25:29, "30-34 ans" = 30:34)
recomposition <- map_dfr(c("Femmes", "Hommes"), function(sx) imap_dfr(GROUPES, function(ages, g) imap_dfr(COMPO, function(v, lib)
  recomposer(filter(jeunes, sexe == sx, age %in% ages), v) %>% mutate(sexe = sx, groupe = g, variables = lib, .before = 1))))
cat("\nPart du recul de la vie en couple qui disparaît à composition de 2014 :\n")
print(as.data.frame(recomposition %>% mutate(across(where(is.double), ~ round(.x, 1)))), row.names = FALSE)

# ── §4 Logement : tension locale et taille de commune ──────────────────────
#  Tension de 2014 par département (script 55) : part des couples sans enfant de 25 à 34 ans
#  qui n'ont pas la place d'un enfant.
tension <- readRDS(file.path(DIR_RP, "_extrait", "couples_RP2014.rds")) %>%
  filter(metrodom == "M", !is.na(pieces), nb_enf_avant == 0, age_femme >= 25, age_femme <= 34) %>%
  group_by(dept) %>% summarise(tension = 100 * sum(poids[place != PLACE[1]]) / sum(poids), .groups = "drop")
cat("\nTension de 2014 par département (%) :\n"); print(round(quantile(tension$tension, c(0, .1, .25, .5, .75, .9, 1)), 1))
brk <- quantile(tension$tension, c(0, 1 / 3, 2 / 3, 1))
jl <- jeunes %>% inner_join(tension, by = "dept") %>%
  mutate(tension_10 = tension / 10, chez_parents = cohab == "Chez ses parents", seul = cohab == "Seul",
         t3 = cut(tension, brk, include.lowest = TRUE,
                  labels = c("Peu tendu", "Intermediaire", "Tendu")))
par_tension <- jl %>% filter(age %in% 25:29) %>% group_by(sexe, t3, annee) %>%
  summarise(en_couple = part(couple, n), chez_ses_parents = part(chez_parents, n), seul = part(seul, n), .groups = "drop")
cat("\n25-29 ans, selon la tension du département (tiers de départements) (%) :\n")
print(as.data.frame(par_tension %>% mutate(across(where(is.double), ~ round(.x, 1))) %>%
                      pivot_wider(names_from = annee, values_from = c(en_couple, chez_ses_parents, seul))), row.names = FALSE)
par_commune <- jeunes %>% filter(age %in% 25:29, sexe == "Femmes") %>% group_by(com_cl, annee) %>%
  summarise(en_couple = part(couple, n), chez_ses_parents = part(cohab == "Chez ses parents", n), seul = part(cohab == "Seul", n),
            .groups = "drop")
cat("\nFemmes de 25-29 ans, selon la taille de commune (%) :\n")
print(as.data.frame(par_commune %>% mutate(across(where(is.double), ~ round(.x, 1))) %>%
                      pivot_wider(names_from = annee, values_from = c(en_couple, chez_ses_parents, seul))), row.names = FALSE)
#  Modèle : issue ~ tension x millésime | âge x millésime + département, puis avec activité et diplôme
modele <- function(x, y, ctrl) {
  m <- feols(as.formula(paste0(y, " ~ i(annee, tension_10, ref = '2014') ", ctrl, " | age^annee + dept")),
             data = x, weights = ~n, cluster = ~dept)
  ct <- as.data.frame(coeftable(m)); ct$terme <- rownames(ct)
  ct %>% filter(grepl("tension_10", terme)) %>%
    transmute(annee = sub("annee::([0-9]+):.*", "\\1", terme), effet_pts = 100 * Estimate, se = 100 * `Std. Error`, p = `Pr(>|t|)`)
}
ISSUES <- c("En couple" = "couple", "Chez ses parents" = "chez_parents", "Seul" = "seul")
tension_modeles <- map_dfr(c("Femmes", "Hommes"), function(sx) imap_dfr(GROUPES[c("20-34 ans", "25-29 ans")], function(ages, g)
  imap_dfr(ISSUES, function(y, lib) bind_rows(
    modele(filter(jl, sexe == sx, age %in% ages), y, "") %>% mutate(controles = "Age"),
    modele(filter(jl, sexe == sx, age %in% ages), y, "+ act + dipl") %>% mutate(controles = "Age, activite, diplome")) %>%
      mutate(sexe = sx, groupe = g, issue = lib, .before = 1))))
cat("\nEffet de 10 points de tension en 2014 sur l'évolution depuis 2014 (points) :\n")
print(as.data.frame(tension_modeles %>% mutate(across(where(is.double), ~ round(.x, 3)))), row.names = FALSE)

# ── Sortie ─────────────────────────────────────────────────────────────────
ecrire(bind_rows(cohabitation %>% mutate(bloc = "Cohabitation par age", age5 = as.character(age5), .before = 1),
                 en_couple_1549 %>% mutate(bloc = "Part des 15-49 ans en couple", .before = 1),
                 icf_rp %>% mutate(bloc = "Naissances par femme au recensement", .before = 1),
                 decomposition %>% mutate(bloc = "Decomposition", .before = 1),
                 brut %>% mutate(bloc = "Taux brut et ICF", .before = 1),
                 recomp_age %>% mutate(bloc = "Recomposition du taux brut des couples", .before = 1),
                 parite %>% mutate(bloc = "ICF, taux des couples par parite", .before = 1),
                 activite %>% mutate(bloc = "Activite et vie en couple", age5 = as.character(age5), .before = 1),
                 recomposition %>% mutate(bloc = "Recomposition", .before = 1),
                 par_tension %>% mutate(bloc = "Selon la tension du departement, 25-29 ans", t3 = as.character(t3), .before = 1),
                 par_commune %>% mutate(bloc = "Selon la taille de commune, femmes de 25-29 ans", .before = 1),
                 tension_modeles %>% mutate(bloc = "Tension et evolution", .before = 1)),
       "Output/vie_en_couple_recensement.csv")
