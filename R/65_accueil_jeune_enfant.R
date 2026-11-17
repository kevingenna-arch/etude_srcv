# ============================================================================
#  65. Offre d'accueil du jeune enfant et naissances : recensement 2014, 2019, 2023
# ----------------------------------------------------------------------------
#  Les scripts 57 à 64 ne mesurent que le barème monétaire. Ici, la politique
#  familiale en nature : les places d'accueil des enfants de moins de 3 ans
#  (crèches, assistantes maternelles, garde à domicile, école à 2 ans), que la
#  Cnaf publie par département et par commune. Elles sont reliées aux couples
#  du recensement (script 48 : département pour tous, commune pour les
#  communes découpées en IRIS), à trois dates.
#
#  1. Offre locale. Mesure : places pour 100 femmes de 20 à 39 ans, et non le
#     taux de couverture (places pour 100 enfants de moins de 3 ans), qui monte
#     mécaniquement quand les naissances baissent. Femmes et enfants par
#     territoire : fichiers détail du recensement.
#       RP 2019 : places 2017-2020 (série historique de la Cnaf) ;
#       RP 2023 : places 2021-2023 (série actuelle raccordée sur 2022) ;
#       RP 2014 : seul le taux global existe (ancienne série, 2013-2015) ; les
#       places en sont déduites (taux x enfants du recensement), calées par
#       département sur le rapport observé en 2017-2020 entre les deux séries.
#       ⚠️ Cette reconstruction n'est PAS fiable d'un département à l'autre :
#       ses places suivent les enfants presque un pour un (corrélation de
#       0,74), contre 0,23 dans les séries annuelles publiées (§1 bis). Elle
#       importe la baisse des naissances dans la mesure de l'offre. Le
#       millésime 2014 ne sert donc qu'au niveau national ; son modèle est
#       gardé pour mémoire (« non retenu »).
#  2. Modèles, 2019 et 2023 : naissance ~ offre + contrôles du script 53 |
#     âge x millésime + territoire. L'effet n'est identifié que par l'évolution
#     de l'offre dans un même territoire. Départements, puis communes de plus
#     de 10 000 habitants avec des effets département x millésime ; ensemble
#     et par mode d'accueil.
#  3. Sens de la causalité : les places suivent-elles les enfants (séries
#     annuelles de la Cnaf) ? La baisse passée des naissances prédit-elle
#     celle de l'offre ?
#  4. Ce que l'évolution de l'offre peut expliquer de la baisse 2019-2023.
#
#  ⚠️ Association à territoire donné, pas une expérience : on ouvre des places
#     là où l'on attend des enfants, et les places d'assistantes maternelles
#     suivent la demande. Places théoriques au 31 décembre, sans les places
#     vacantes. Deux millésimes seulement, qui partagent une année de collecte.
#  Entrées : Data/Couverture accueil enfance/ (Cnaf, data.caf.fr et ancienne
#            série reprise sur web.archive.org), Data/recensement/
#  Sorties : Data/recensement/_extrait/denominateurs_accueil.rds (cache),
#            Output/accueil_jeune_enfant.csv
# ============================================================================

source("R/00_utils.R")
source("R/00_config.R")
source("R/00_norme_peuplement.R")
suppressPackageStartupMessages({library(data.table); library(fixest)})
setFixest_nthreads(1); setFixest_notes(FALSE)   # un seul fil : voir R/57_erfs_preparation.R

DIR_CNAF <- file.path("Data", "Couverture accueil enfance")
ANS      <- c("2014", "2019", "2023")
FICHIERS <- c("2014" = "FD_INDCVI_2014.txt", "2019" = "FD_INDCVI_2019.csv", "2023" = "FD_INDCVI_2023.csv")
MODES    <- c("eaje", "prescol", "am", "gad", "total")
pondm    <- function(x, w) sum(x * w) / sum(w)
ville    <- function(code) fcase(startsWith(code, "751"), "75056", startsWith(code, "6938"), "69123",
                                 startsWith(code, "132"), "13055", default = code)

# ── §0 Femmes et enfants par territoire (fichiers détail du recensement) ───
F_DENOM <- file.path(DIR_RP, "_extrait", "denominateurs_accueil.rds")
if (!file.exists(F_DENOM)) {
  denom <- lapply(ANS, function(an) {
    t0 <- Sys.time()
    x <- fread(file.path(DIR_RP, FICHIERS[[an]]), sep = ";", colClasses = "character", showProgress = FALSE,
               select = c("AGEREV", "SEXE", "IPONDI", "DEPT", "IRIS", "METRODOM"))
    x <- x[METRODOM == "M"]
    x[, `:=`(age = as.integer(AGEREV), w = as.numeric(IPONDI))]
    x[, `:=`(f2039 = w * (SEXE == "2" & age >= 20 & age <= 39), enf = w * (age < 3),
             commune = fifelse(substr(IRIS, 1, 1) == "Z", NA_character_, ville(substr(IRIS, 1, 5))))]
    som <- function(by) x[, .(femmes_20_39 = sum(f2039), enfants_m3 = sum(enf), population = sum(w)), by = by]
    cat(an, ": dénominateurs calculés en", round(difftime(Sys.time(), t0, units = "secs")), "s\n")
    list(dep = som("DEPT")[, annee := an], com = som("commune")[!is.na(commune)][, annee := an])
  })
  saveRDS(list(dep = rbindlist(lapply(denom, `[[`, "dep")), com = rbindlist(lapply(denom, `[[`, "com"))), F_DENOM)
}
denom <- readRDS(F_DENOM)
den_dep <- as_tibble(denom$dep) %>% rename(dept = DEPT)
den_com <- as_tibble(denom$com)

# ── §1 Offre d'accueil ─────────────────────────────────────────────────────
#  Places par mode (séries de data.caf.fr). Colonnes repérées par leur rang :
#  année, code, nom, [région], EAJE PSU, hors PSU, EAJE, préscolarisation,
#  assistante maternelle, garde à domicile, accueil individuel, ensemble.
places <- function(f) {
  x <- fread(file.path(DIR_CNAF, f), sep = ";", colClasses = "character", encoding = "UTF-8")
  k <- ncol(x)
  tibble(annee = as.integer(x[[1]]), code = x[[2]],
         eaje = as.numeric(x[[k - 5]]), prescol = as.numeric(x[[k - 4]]), am = as.numeric(x[[k - 3]]),
         gad = as.numeric(x[[k - 2]]), total = as.numeric(x[[k]])) %>%
    mutate(across(all_of(MODES), ~ coalesce(.x, 0)))
}
#  Raccord de la série actuelle (2022-2023) sur la série historique (2017-2022), par territoire et par mode
raccorder <- function(hist, actuel) {
  r <- inner_join(filter(hist, annee == 2022), filter(actuel, annee == 2022), by = "code", suffix = c("_h", "_a")) %>%
    transmute(code, across(ends_with("_h"), ~ if_else(get(sub("_h$", "_a", cur_column())) > 0,
                                                     .x / get(sub("_h$", "_a", cur_column())), 1),
                           .names = "r_{sub('_h$', '', .col)}"))
  suite <- filter(actuel, annee > 2022) %>% inner_join(r, by = "code") %>%
    mutate(across(all_of(MODES), ~ .x * get(paste0("r_", cur_column())))) %>% select(-starts_with("r_"))
  bind_rows(hist, suite)
}
#  Moyenne des années de collecte de chaque millésime : RP 2019 = 2017-2020, RP 2023 = 2021-2023
par_millesime <- function(x) x %>%
  mutate(rp = case_when(annee %in% 2017:2020 ~ "2019", annee %in% 2021:2023 ~ "2023")) %>% filter(!is.na(rp)) %>%
  group_by(code, annee = rp) %>% summarise(across(all_of(MODES), mean), .groups = "drop")

pl_dep <- raccorder(places("nbpla_pe_dep_hist.csv"), places("nbpla_pe_dep.csv"))
pl_com <- bind_rows(places("nbpla_pe_com_hist.csv") %>% mutate(serie = "h"), places("nbpla_pe_com.csv") %>% mutate(serie = "a")) %>%
  mutate(code = ville(code)) %>% group_by(serie, annee, code) %>% summarise(across(all_of(MODES), sum), .groups = "drop")
pl_com <- raccorder(filter(pl_com, serie == "h") %>% select(-serie), filter(pl_com, serie == "a") %>% select(-serie))

france <- pl_dep %>% filter(!grepl("^97|^X", code)) %>% group_by(annee) %>%
  summarise(across(all_of(MODES), sum), .groups = "drop") %>% mutate(annee = as.character(annee))
cat("\nPlaces d'accueil des moins de 3 ans, France métropolitaine (série raccordée) :\n")
print(as.data.frame(france %>% mutate(across(all_of(MODES), round))), row.names = FALSE)

#  Ancienne série : taux de couverture global (places pour 100 enfants de moins de 3 ans)
lire_ancien <- function(an) {
  x <- fread(file.path(DIR_CNAF, "taux_global_2013_2021_archive", paste0("txcouvglo", an, ".csv")), sep = ";",
             colClasses = "character", encoding = "Latin-1", fill = TRUE)
  n <- toupper(names(x)); num <- function(v) suppressWarnings(as.numeric(sub(",", ".", v, fixed = TRUE)))
  col <- function(motif) x[[which(grepl(motif, n))[1]]]
  tibble(annee = an, commune = col("^NUM_?COM$"), dept = col("^NUM_?DEP$"),
         tx_com = num(col("^T.*COM")), tx_dep = num(col("^T.*DEP"))) %>%
    filter(grepl("^[0-9][0-9AB]?[0-9]*$", commune)) %>%                       # en-tête répété, lignes « XX »
    mutate(commune = ville(str_pad(commune, 5, pad = "0")), dept = str_pad(dept, 2, pad = "0"))
}
ancien <- map_dfr(2013:2021, lire_ancien)
anc_dep <- ancien %>% filter(!is.na(tx_dep)) %>% group_by(annee, dept) %>% summarise(tx = first(tx_dep), .groups = "drop")
cat("Ancienne série : codes de département absents du recensement :",
    paste(setdiff(unique(anc_dep$dept), c(unique(den_dep$dept), sprintf("97%d", 1:8))), collapse = " "), "\n")

#  Départements, trois millésimes : places de 2014 = taux x enfants, calées sur 2017-2020
enf <- den_dep %>% select(dept, annee, enfants_m3)
tx_per <- anc_dep %>% mutate(per = case_when(annee %in% 2013:2015 ~ "2014", annee %in% 2017:2020 ~ "2019")) %>%
  filter(!is.na(per)) %>% group_by(dept, annee = per) %>% summarise(tx = mean(tx), .groups = "drop") %>%
  inner_join(enf, by = c("dept", "annee")) %>% mutate(places_taux = tx * enfants_m3 / 100)
pm_dep <- par_millesime(pl_dep) %>% rename(dept = code)
calage <- tx_per %>% filter(annee == "2019") %>% inner_join(filter(pm_dep, annee == "2019"), by = "dept") %>%
  transmute(dept, calage = total / places_taux)
cat("\nRaccord des deux séries en 2017-2020 (places publiées / places déduites du taux), par département :\n")
print(round(quantile(calage$calage, c(0, .1, .25, .5, .75, .9, 1)), 3))
p14 <- tx_per %>% filter(annee == "2014") %>% inner_join(calage, by = "dept") %>%
  transmute(dept, annee, total = places_taux * calage)
offre_dep <- bind_rows(p14, pm_dep) %>% inner_join(den_dep, by = c("dept", "annee")) %>%
  mutate(across(any_of(MODES), ~ 100 * .x / femmes_20_39, .names = "s_{.col}"),
         s_autres = s_prescol + s_gad, taux_couv = 100 * total / enfants_m3)
nat <- offre_dep %>% group_by(annee) %>%
  summarise(departements = n(), places = sum(total), femmes_20_39 = sum(femmes_20_39), enfants_m3 = sum(enfants_m3),
            creches = sum(eaje), assistantes_maternelles = sum(am), .groups = "drop") %>%
  mutate(places_pour_100_femmes = 100 * places / femmes_20_39, taux_couverture = 100 * places / enfants_m3,
         creches_pour_100_femmes = 100 * creches / femmes_20_39, am_pour_100_femmes = 100 * assistantes_maternelles / femmes_20_39)
cat("\nOffre d'accueil par millésime du recensement, France métropolitaine :\n")
print(as.data.frame(nat %>% mutate(across(where(is.double), ~ round(.x, 1)))), row.names = FALSE)
disp <- offre_dep %>% select(dept, annee, s_total) %>% pivot_wider(names_from = annee, values_from = s_total, names_prefix = "s") %>%
  mutate(d1419 = s2019 - s2014, d1923 = s2023 - s2019)
cat("\nPlaces pour 100 femmes de 20-39 ans par département : niveau 2019 et variations (quantiles) :\n")
print(round(sapply(disp[c("s2019", "d1419", "d1923")], quantile, c(.05, .25, .5, .75, .95), na.rm = TRUE), 2))

# ── §1 bis La reconstruction de 2014 est-elle fiable ? ─────────────────────
#  Séries annuelles de la Cnaf, 2017-2021 : enfants de moins de 3 ans selon la Cnaf = places publiées /
#  taux de l'ancienne série. On compare, par département, la façon dont les places suivent les enfants
#  dans ces séries et dans la reconstruction 2014 -> 2019.
ann <- pl_dep %>% filter(annee %in% c(2017, 2021), !grepl("^97|^X", code)) %>% rename(dept = code) %>%
  inner_join(anc_dep, by = c("dept", "annee")) %>% mutate(k = 100 * total / tx) %>%
  select(dept, annee, total, eaje, am, k) %>% pivot_wider(names_from = annee, values_from = c(total, eaje, am, k)) %>%
  transmute(dept, d_places = log(total_2021 / total_2017), d_creches = log(eaje_2021 / eaje_2017),
            d_am = log(am_2021 / am_2017), d_enfants = log(k_2021 / k_2017)) %>% filter(if_all(-dept, is.finite))
rec <- tx_per %>% select(dept, annee, tx, enfants_m3) %>% pivot_wider(names_from = annee, values_from = c(tx, enfants_m3)) %>%
  transmute(dept, d_enfants = log(enfants_m3_2019 / enfants_m3_2014), d_places = log(tx_2019 / tx_2014) + d_enfants)
pente <- function(y, x, lib) {
  r <- summary(lm(y ~ x))$coefficients
  tibble(test = lib, coef = r[2, 1], se = r[2, 2], p = r[2, 4], correlation = cor(x, y), n = length(x))
}
suivi <- bind_rows(
  pente(ann$d_places, ann$d_enfants, "Series publiees 2017-2021 : places sur enfants"),
  pente(ann$d_creches, ann$d_enfants, "Series publiees 2017-2021 : creches sur enfants"),
  pente(ann$d_am, ann$d_enfants, "Series publiees 2017-2021 : assistantes maternelles sur enfants"),
  pente(rec$d_places, rec$d_enfants, "Reconstruction 2014-2019 : places sur enfants (non fiable)"))
cat("\nLes places suivent-elles les enfants ? Variations en log par département :\n")
print(as.data.frame(suivi %>% mutate(across(where(is.double), ~ round(.x, 3)))), row.names = FALSE)

#  Communes de plus de 10 000 habitants, 2019 et 2023, par mode
offre_com <- par_millesime(pl_com) %>% rename(commune = code) %>% inner_join(den_com, by = c("commune", "annee")) %>%
  group_by(commune) %>% filter(n() == 2) %>% ungroup() %>%
  mutate(across(all_of(MODES), ~ 100 * .x / femmes_20_39, .names = "c_{.col}"), c_autres = c_prescol + c_gad)
cat("\nCommunes reliées (présentes en 2019 et 2023) :", n_distinct(offre_com$commune), "\n")

# ── §2 Couples du recensement ──────────────────────────────────────────────
SURF_LIB <- c("1" = "Moins de 30 m2", "2" = "30 a 39 m2", "3" = "40 a 59 m2", "4" = "60 a 79 m2",
              "5" = "80 a 99 m2", "6" = "100 a 119 m2", "7" = "120 m2 ou plus")
COM_NIV <- c("Commune non decoupee en IRIS", "Moins de 20 000 hab.", "20 000 a 99 999 hab.",
             "100 000 hab. ou plus", "Petite couronne", "Paris")
d <- map_dfr(ANS, ~ readRDS(file.path(DIR_RP, "_extrait", paste0("couples_RP", .x, ".rds")))) %>%
  filter(metrodom == "M", !is.na(pieces), surf %in% names(SURF_LIB), !is.na(diplome4)) %>%
  mutate(rang = if_else(nb_enf_avant == 0, "1er enfant", "Rang 2 et plus"),
         statut = factor(case_when(stocd == "10" ~ "Proprietaire", stocd == "22" ~ "Locataire HLM",
                                   stocd %in% c("21", "23") ~ "Locataire prive", TRUE ~ "Loge gratuitement")),
         cs6 = if_else(cs %in% as.character(1:6), cs, "Sans"), demenage = iran != "1",
         com_cl = factor(case_when(commune == "75056" ~ "Paris", dept %in% c("92", "93", "94") ~ "Petite couronne",
                                   is.na(pop_commune) ~ "Commune non decoupee en IRIS",
                                   pop_commune < 20000 ~ "Moins de 20 000 hab.", pop_commune < 100000 ~ "20 000 a 99 999 hab.",
                                   TRUE ~ "100 000 hab. ou plus"), levels = COM_NIV),
         nb_enf_cl = factor(pmin(nb_enf_avant, 3)), taille_avant = pmin(inper - naiss01, 10),
         tact_conj = coalesce(tact_conj, "Inconnu"), en_emploi = tact == "11") %>%
  select(annee, poids, naiss01, rang, age_femme, dept, commune, place, diplome4, tact, tact_conj, cs6, statut,
         demenage, com_cl, nb_enf_cl, taille_avant, en_emploi) %>%
  inner_join(select(offre_dep, dept, annee, starts_with("s_"), taux_couv), by = c("dept", "annee"))
cat("\nCouples du recensement reliés à l'offre départementale :\n"); print(count(d, annee, rang))
taux <- bind_rows(d %>% group_by(annee, rang) %>% summarise(taux_pct = 100 * pondm(naiss01, poids), .groups = "drop"),
                  d %>% group_by(annee, rang = "Ensemble") %>% summarise(taux_pct = 100 * pondm(naiss01, poids), .groups = "drop"))
print(as.data.frame(taux %>% pivot_wider(names_from = annee, values_from = taux_pct)))

# ── §3 Modèles ─────────────────────────────────────────────────────────────
CTRL <- "place + diplome4 + tact + tact_conj + cs6 + statut + demenage + com_cl + nb_enf_cl + taille_avant"
RANGS <- list("Ensemble" = function(x) x, "1er enfant" = function(x) filter(x, rang == "1er enfant"),
              "Rang 2 et plus" = function(x) filter(x, rang != "1er enfant"))
estimer <- function(x, vars, fe, grappe, extra = "") {
  m <- feols(as.formula(paste("naiss01 ~", paste(vars, collapse = " + "), extra, "+", CTRL, "|", fe)), data = x,
             weights = ~poids, cluster = grappe)
  ct <- as.data.frame(coeftable(m)); ct$terme <- rownames(ct)
  ct %>% filter(grepl(paste0("^(", paste(vars, collapse = "|"), ")"), terme)) %>%
    transmute(terme, effet_pts = 100 * Estimate, se = 100 * `Std. Error`, p = `Pr(>|t|)`, n = nobs(m),
              naissances = sum(x$naiss01), taux_pct = 100 * pondm(x$naiss01, x$poids))
}
#  A. Départements : places pour 100 femmes, puis taux de couverture (biais mécanique). Référence : 2019 et
#     2023 ; les trois millésimes, avec le niveau reconstruit de 2014, sont gardés pour mémoire (non retenus).
MESURES_DEP <- list("Places pour 100 femmes de 20-39 ans" = "s_total", "Taux de couverture (pour 100 enfants)" = "taux_couv")
PERIODES <- list("2019, 2023" = c("2019", "2023"), "2014, 2019, 2023 (non retenu)" = ANS)
dep_modeles <- imap_dfr(PERIODES, function(ans, per) imap_dfr(MESURES_DEP, function(v, mes) imap_dfr(RANGS, function(f, ech)
  estimer(f(filter(d, annee %in% ans)), v, "age_femme^annee + dept", ~dept) %>%
    mutate(niveau = "Departement", periode = per, mesure = mes, echantillon = ech, .before = 1))))
#  B. Départements, 2019 et 2023, par mode d'accueil ; et selon l'emploi de la femme
d2 <- filter(d, annee != "2014")
dep_modes <- imap_dfr(RANGS, function(f, ech)
  estimer(f(d2), c("s_eaje", "s_am", "s_autres"), "age_femme^annee + dept", ~dept) %>%
    mutate(niveau = "Departement", periode = "2019, 2023", mesure = "Par mode d'accueil", echantillon = ech, .before = 1))
dep_emploi <- imap_dfr(RANGS, function(f, ech)
  estimer(f(d2), c("s_total", "s_total:en_emploi"), "age_femme^annee + dept", ~dept) %>%
    mutate(niveau = "Departement", periode = "2019, 2023", mesure = "Selon l'emploi de la femme", echantillon = ech, .before = 1))
#  C. Communes de plus de 10 000 habitants, 2019 et 2023 : effets commune et département x millésime
dc <- d2 %>% select(-starts_with("s_"), -taux_couv) %>%
  inner_join(select(offre_com, commune, annee, starts_with("c_")), by = c("commune", "annee"))
cat("\nCouples des communes de plus de 10 000 habitants reliées :", nrow(dc), "|", n_distinct(dc$commune), "communes\n")
com_modeles <- bind_rows(
  imap_dfr(RANGS, function(f, ech) estimer(f(dc), "c_total", "age_femme^annee + commune + dept^annee", ~commune) %>%
             mutate(mesure = "Places pour 100 femmes de 20-39 ans", echantillon = ech, .before = 1)),
  imap_dfr(RANGS, function(f, ech) estimer(f(dc), c("c_eaje", "c_am", "c_autres"), "age_femme^annee + commune + dept^annee", ~commune) %>%
             mutate(mesure = "Par mode d'accueil", echantillon = ech, .before = 1))) %>%
  mutate(niveau = "Commune", periode = "2019, 2023", .before = 1)
modeles <- bind_rows(dep_modeles, dep_modes, dep_emploi, com_modeles)
cat("\nEffet d'une place de plus pour 100 femmes de 20-39 ans sur la probabilité de naissance (points) :\n")
print(as.data.frame(modeles %>% mutate(across(where(is.double), ~ round(.x, 4)))), row.names = FALSE)

# ── §4 Sens de la causalité, par département ───────────────────────────────
#  Taux de naissance à caractéristiques données (résidu du modèle sans l'offre), puis variations.
m0 <- feols(as.formula(paste("naiss01 ~", CTRL, "| age_femme^annee")), data = d, weights = ~poids)
agr <- d %>% mutate(res = 100 * resid(m0)) %>% group_by(dept, annee) %>%
  summarise(b = pondm(res, poids), w = sum(poids), s = first(s_total), .groups = "drop") %>%
  pivot_wider(names_from = annee, values_from = c(b, w, s)) %>%
  mutate(db1 = b_2019 - b_2014, db2 = b_2023 - b_2019, ds1 = s_2019 - s_2014, ds2 = s_2023 - s_2019)
dd <- function(f, lib) {
  m <- feols(f, data = agr, weights = ~w_2019, vcov = "hetero"); ct <- coeftable(m)
  tibble(test = lib, coef = ct[2, 1], se = ct[2, 2], p = ct[2, 4], n = nobs(m))
}
causalite <- bind_rows(
  suivi,
  dd(db2 ~ ds2, "Naissances 2019-2023 sur offre 2019-2023 (meme periode)"),
  dd(ds2 ~ db1, "Offre 2019-2023 sur naissances 2014-2019 (les naissances precedent)"))
cat("\nSens de la causalité, par département (points de taux de naissance ; places pour 100 femmes) :\n")
print(as.data.frame(causalite %>% mutate(across(where(is.double), ~ round(.x, 4)))), row.names = FALSE)

# ── §5 Ce que l'évolution de l'offre peut expliquer de la baisse 2019-2023 ──
#  Effet d'une place x variation nationale de l'offre, rapporté à la baisse du taux de naissance des couples.
ds_nat <- diff(nat$places_pour_100_femmes[nat$annee %in% c("2019", "2023")])
s_com <- offre_com %>% group_by(annee) %>% summarise(s = 100 * sum(total) / sum(femmes_20_39), .groups = "drop")
ds_com <- s_com$s[s_com$annee == "2023"] - s_com$s[s_com$annee == "2019"]
part_offre <- modeles %>%
  filter(periode == "2019, 2023", terme %in% c("s_total", "c_total"), mesure == "Places pour 100 femmes de 20-39 ans") %>%
  left_join(taux %>% pivot_wider(names_from = annee, values_from = taux_pct, names_prefix = "taux_") %>%
              rename(echantillon = rang), by = "echantillon") %>%
  mutate(variation_offre = if_else(niveau == "Commune", ds_com, ds_nat), baisse_pts = taux_2023 - taux_2019,
         part_pct = 100 * effet_pts * variation_offre / baisse_pts,
         part_bas = 100 * (effet_pts - 1.96 * se) * variation_offre / baisse_pts,
         part_haut = 100 * (effet_pts + 1.96 * se) * variation_offre / baisse_pts) %>%
  select(niveau, echantillon, effet_pts, se, variation_offre, taux_2019, taux_2023, baisse_pts, part_pct, part_bas, part_haut)
cat("\nPart de la baisse 2019-2023 du taux de naissance des couples associée à l'évolution de l'offre (%) :\n")
print(as.data.frame(part_offre %>% mutate(across(where(is.double), ~ round(.x, 3)))), row.names = FALSE)

# ── Sortie ─────────────────────────────────────────────────────────────────
ecrire(bind_rows(france %>% mutate(bloc = "Places par annee, France metropolitaine", .before = 1),
                 nat %>% mutate(bloc = "Offre par millesime", .before = 1),
                 taux %>% mutate(bloc = "Taux de naissance des couples", .before = 1),
                 modeles %>% mutate(bloc = "Modeles", .before = 1),
                 causalite %>% mutate(bloc = "Sens de la causalite", .before = 1),
                 part_offre %>% mutate(bloc = "Part de la baisse 2019-2023", .before = 1)),
       "Output/accueil_jeune_enfant.csv")
