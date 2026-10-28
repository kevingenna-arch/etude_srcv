# ============================================================================
#  SRCV — 55. Naissances repoussées ou perdues ? Le manque de place selon
#             l'âge (recensements 2014, 2019, 2023)
# ----------------------------------------------------------------------------
#  Question : la difficulté à trouver un logement assez grand fait-elle
#  renoncer au premier enfant, ou le repousse-t-elle à plus tard ?
#  Extraits du script 48 : couples de la famille principale, femme 15-49 ans,
#  France métropolitaine. « Sans place » réunit, selon la norme de peuplement,
#  « pas de place pour un enfant de plus » et « il manque déjà une pièce ».
#   §1 Taux de première naissance par âge et place (descriptif).
#   §2 Effet de l'absence de place sur la première naissance, par classe d'âge
#      de la femme, mêmes contrôles que le script 53 ; par millésime puis sur
#      les trois réunis ; tous les couples sans enfant, puis les installés
#      depuis 2 ans ou plus (logement antérieur au projet d'enfant). Un report
#      donne un écart fort aux jeunes âges qui se réduit, voire s'inverse,
#      après 35 ans ; un renoncement, un écart qui persiste.
#   §3 Le bilan en fin de vie féconde, par département : là où les jeunes
#      couples manquaient le plus souvent de place en 2014 (couples sans
#      enfant, femme de 25 à 34 ans), les femmes de 40-44 ans de 2023, qui
#      avaient 31 à 35 ans en 2014, sont-elles plus souvent sans enfant au
#      foyer, et le premier enfant arrive-t-il plus tard ? Plus de naissances
#      tardives sans plus de femmes sans enfant : report ; plus de femmes sans
#      enfant : naissances perdues. Comparaison entre départements : les
#      familles quittent les zones tendues, d'où des estimations indicatives.
#   §4 Ordre de grandeur du canal de la mobilité : couples sans enfant sans
#      place × recul de leur mobilité à caractéristiques données (script 53,
#      B2) × surcroît de premières naissances après un déménagement dans un
#      logement avec place, rapporté au recul du taux de première naissance
#      2014-2023. Borne haute : une partie des déménagements vise l'enfant.
#   §5 Un premier enfant plus tardif, moins de suivants ? (a) Probabilité
#      d'un deuxième enfant chez les couples qui en ont un, selon l'âge de la
#      femme ; (b) par département, tension 2014 et, en 2023, part des mères
#      de 40-44 ans qui n'ont qu'un enfant au foyer, ou trois et plus, et
#      deuxièmes naissances par âge. Les aînés partis du foyer font passer
#      pour enfant unique des familles de deux ; ils sont plus nombreux là où
#      les enfants sont venus tôt, donc dans les départements peu tendus :
#      le biais joue contre l'hypothèse.
#  Modèles : probabilité linéaire pondérée (fixest) ; erreurs-types par
#  canton-ou-ville (§2) ou par département (§3, exposition départementale).
#  Sortie : Output/report_naissances_age.csv
# ============================================================================

source("R/00_utils.R")
source("R/00_config.R")
source("R/00_norme_peuplement.R")
suppressPackageStartupMessages(library(fixest))
setFixest_notes(FALSE)

ANS <- c("2014", "2019", "2023")
AGES <- c("15-24", "25-29", "30-34", "35-39", "40-49")
COM_NIV <- c("Commune non decoupee en IRIS", "Moins de 20 000 hab.", "20 000 a 99 999 hab.",
             "100 000 hab. ou plus", "Petite couronne", "Paris")
IDF <- c("75", "77", "78", "91", "92", "93", "94", "95")

d <- map_dfr(ANS, ~ readRDS(file.path(DIR_RP, "_extrait", paste0("couples_RP", .x, ".rds")))) %>%
  filter(metrodom == "M", !is.na(pieces), !is.na(diplome4)) %>%
  mutate(cantville = sub(" .*", "", hh),
         sans_place = as.integer(place != PLACE[1]),
         age5 = factor(cut(age_femme, c(14, 24, 29, 34, 39, 49), labels = AGES), levels = AGES),
         statut = factor(case_when(stocd == "10" ~ "Proprietaire", stocd == "22" ~ "Locataire HLM",
                                   stocd %in% c("21", "23") ~ "Locataire prive", TRUE ~ "Loge gratuitement"),
                         levels = c("Locataire prive", "Locataire HLM", "Proprietaire", "Loge gratuitement")),
         cs6 = if_else(cs %in% as.character(1:6), cs, "Sans"),
         demenage = iran != "1",
         com_cl = factor(case_when(commune == "75056" ~ "Paris",
                                   dept %in% c("92", "93", "94") ~ "Petite couronne",
                                   is.na(pop_commune) ~ "Commune non decoupee en IRIS",
                                   pop_commune < 20000 ~ "Moins de 20 000 hab.",
                                   pop_commune < 100000 ~ "20 000 a 99 999 hab.",
                                   TRUE ~ "100 000 hab. ou plus"), levels = COM_NIV),
         tact_conj = coalesce(tact_conj, "Inconnu"))
sans_enf <- filter(d, nb_enf_avant == 0)
pond <- function(x, w) 100 * sum(x * w) / sum(w)
coefs <- function(m, motif) {
  ct <- as.data.frame(coeftable(m))
  tibble(terme = rownames(ct), effet_pts = 100 * ct[, 1], se = 100 * ct[, 2], p = ct[, 4]) %>%
    filter(grepl(motif, terme))
}

# ── §1 Taux de première naissance par âge et place ─────────────────────────
taux_age <- sans_enf %>% group_by(annee, age5, sans_place) %>%
  summarise(n = n(), taux_pct = pond(naiss01, poids), .groups = "drop")
cat("§1 Taux de première naissance (%), couples sans enfant :\n")
print(as.data.frame(taux_age %>%
  transmute(age5, place = if_else(sans_place == 1, "sans place", "avec place"), annee, t = round(taux_pct, 1)) %>%
  pivot_wider(names_from = annee, values_from = t, names_prefix = "RP") %>%
  mutate(evol_2014_2023_pct = round(100 * (RP2023 / RP2014 - 1)))), row.names = FALSE)

# ── §2 Effet de l'absence de place, par âge ────────────────────────────────
CTRL <- "diplome4 + tact + tact_conj + cs6 + statut + demenage + com_cl"
CHAMPS <- list("Tous les couples sans enfant" = function(x) x,
               "Installes depuis 2 ans ou plus" = function(x) filter(x, anemr != "00"))
par_age <- function(x, fe) {
  m <- feols(as.formula(paste("naiss01 ~ i(age5, sans_place) +", CTRL, "|", fe)), data = x,
             weights = ~poids, cluster = ~cantville)
  ref <- x %>% filter(sans_place == 0) %>% group_by(age5) %>%
    summarise(taux_avec_place_pct = pond(naiss01, poids), .groups = "drop")
  coefs(m, "^age5::") %>%
    mutate(age5 = sub("^age5::(.*):sans_place$", "\\1", terme)) %>%
    left_join(ref %>% mutate(age5 = as.character(age5)), by = "age5") %>%
    mutate(ecart_relatif_pct = 100 * effet_pts / taux_avec_place_pct, n = nobs(m))
}
effets_age <- bind_rows(
  map_dfr(ANS, function(an) imap_dfr(CHAMPS, ~ par_age(.x(filter(sans_enf, annee == an)), "age_femme + dept") %>%
    mutate(annee = an, champ = .y, .before = 1))),
  imap_dfr(CHAMPS, ~ par_age(.x(sans_enf), "age_femme^annee + dept^annee") %>%
    mutate(annee = "2014-2023", champ = .y, .before = 1)))
cat("\n§2 Absence de place et première naissance, par âge : points (en % du taux avec place) :\n")
print(as.data.frame(effets_age %>%
  transmute(champ, age5, annee, lib = sprintf("%.1f (%.0f %%)%s", effet_pts, ecart_relatif_pct,
                                              if_else(p < 0.05, "", " ns"))) %>%
  pivot_wider(names_from = annee, values_from = lib)), row.names = FALSE)

# ── §3 Bilan en fin de vie féconde, par département ────────────────────────
tension <- d %>% filter(annee == "2014", nb_enf_avant == 0, age_femme >= 25, age_femme <= 34) %>%
  group_by(dept) %>% summarise(tension_2014 = pond(sans_place, poids), .groups = "drop")
cat("\nTension 2014 (part des couples sans enfant de 25-34 ans sans place), par département :\n")
print(summary(tension$tension_2014))

d23 <- d %>% filter(annee == "2023") %>% inner_join(tension, by = "dept") %>%
  mutate(tension_10 = tension_2014 / 10, sans_enfant = as.integer(n_enf == 0))
CTRL3 <- "diplome4 + tact + tact_conj + cs6"
ISSUES <- list(
  "Sans enfant au foyer, femme de 40-44 ans" =
    list(f = "sans_enfant", x = function(x) filter(x, age_femme >= 40, age_femme <= 44)),
  "Enfants au foyer, femme de 40-44 ans" =
    list(f = "n_enf", x = function(x) filter(x, age_femme >= 40, age_femme <= 44)),
  "Premiere naissance, femme de 25-34 ans sans enfant" =
    list(f = "naiss01", x = function(x) filter(x, nb_enf_avant == 0, age_femme >= 25, age_femme <= 34)),
  "Premiere naissance, femme de 35-44 ans sans enfant" =
    list(f = "naiss01", x = function(x) filter(x, nb_enf_avant == 0, age_femme >= 35, age_femme <= 44)),
  "Age de la mere a la premiere naissance" =
    list(f = "age_femme", x = function(x) filter(x, nb_enf_avant == 0, naiss01 == 1)))
SPECS3 <- list("Controles individuels" = CTRL3,
               "Et taille de commune" = paste(CTRL3, "+ com_cl"),
               "Hors Ile-de-France" = CTRL3)
fin_vie <- imap_dfr(ISSUES, function(iss, lib) imap_dfr(SPECS3, function(ctrl, sp) {
  x <- iss$x(d23)
  if (sp == "Hors Ile-de-France") x <- filter(x, !dept %in% IDF)
  fe <- if (iss$f == "age_femme") "" else "| age_femme"
  m <- feols(as.formula(paste(iss$f, "~ tension_10 +", ctrl, fe)), data = x,
             weights = ~poids, cluster = ~dept)
  ct <- coeftable(m)["tension_10", ]
  tibble(issue = lib, specification = sp, effet_10pts = unname(ct[1]), se = unname(ct[2]), p = unname(ct[4]),
         moyenne = weighted.mean(x[[iss$f]], x$poids), n = nobs(m))
}))
cat("\n§3 Effet de 10 points de tension 2014 sur la situation en 2023 :\n")
print(as.data.frame(fin_vie %>% mutate(across(where(is.double), ~ signif(.x, 3)))), row.names = FALSE)

# ── §4 Ordre de grandeur du canal de la mobilité ───────────────────────────
f53 <- "Output/recensement_trois_vagues.csv"
stopifnot(file.exists(f53))
recul_mob <- read_csv(f53, show_col_types = FALSE) %>%
  filter(bloc == "B2 Ecart avec 2014", groupe == "Sans enfant, pas de place", terme == "annee::2023") %>%
  pull(effet_pts)
flux <- sans_enf %>% filter(annee %in% c("2014", "2023")) %>%
  mutate(recent = anemr == "00") %>% group_by(annee) %>%
  summarise(part_sans_place = mean(sans_place * poids) / mean(poids),
            taux_recents_avec_place = pond(naiss01[recent & sans_place == 0], poids[recent & sans_place == 0]),
            taux_installes_sans_place = pond(naiss01[!recent & sans_place == 1], poids[!recent & sans_place == 1]),
            taux_ensemble = pond(naiss01, poids), .groups = "drop")
canal <- tibble(
  part_sans_place_2023_pct = 100 * flux$part_sans_place[flux$annee == "2023"],
  recul_mobilite_pts = recul_mob,
  gain_naissance_pts = flux$taux_recents_avec_place[flux$annee == "2014"] - flux$taux_installes_sans_place[flux$annee == "2014"],
  recul_taux_1re_naissance_pts = flux$taux_ensemble[flux$annee == "2014"] - flux$taux_ensemble[flux$annee == "2023"]) %>%
  mutate(effet_canal_pts = part_sans_place_2023_pct / 100 * (-recul_mobilite_pts) / 100 * gain_naissance_pts,
         part_du_recul_pct = 100 * effet_canal_pts / recul_taux_1re_naissance_pts)
cat("\n§4 Canal de la mobilité, borne haute :\n")
print(as.data.frame(canal %>% mutate(across(everything(), ~ round(.x, 2)))), row.names = FALSE)

# ── §5 Premier enfant plus tardif, moins de suivants ? ─────────────────────
second_age <- d %>% filter(nb_enf_avant == 1) %>% group_by(annee, age5) %>%
  summarise(n = n(), taux_2e_pct = pond(naiss01, poids), .groups = "drop")
cat("\n§5a Deuxième naissance chez les couples qui ont un enfant (%), selon l'âge de la femme :\n")
print(as.data.frame(second_age %>% transmute(age5, annee, t = round(taux_2e_pct, 1)) %>%
  pivot_wider(names_from = annee, values_from = t, names_prefix = "RP")), row.names = FALSE)
meres <- d %>% filter(age_femme >= 40, age_femme <= 44, n_enf >= 1) %>% group_by(annee) %>%
  summarise(un_enfant_pct = pond(n_enf == 1, poids), trois_et_plus_pct = pond(n_enf >= 3, poids), .groups = "drop")
cat("\nMères de 40-44 ans en couple : part avec un seul enfant au foyer, avec trois et plus (%) :\n")
print(as.data.frame(meres %>% mutate(across(where(is.double), ~ round(.x, 1)))), row.names = FALSE)

d23 <- d23 %>% mutate(un_enfant = as.integer(n_enf == 1), trois_plus = as.integer(n_enf >= 3))
ISSUES5 <- list(
  "Un seul enfant au foyer, meres de 40-44 ans" =
    list(f = "un_enfant", x = function(x) filter(x, age_femme >= 40, age_femme <= 44, n_enf >= 1)),
  "Trois enfants ou plus au foyer, meres de 40-44 ans" =
    list(f = "trois_plus", x = function(x) filter(x, age_femme >= 40, age_femme <= 44, n_enf >= 1)),
  "Deuxieme naissance, un enfant, femme de 25-34 ans" =
    list(f = "naiss01", x = function(x) filter(x, nb_enf_avant == 1, age_femme >= 25, age_femme <= 34)),
  "Deuxieme naissance, un enfant, femme de 35-44 ans" =
    list(f = "naiss01", x = function(x) filter(x, nb_enf_avant == 1, age_femme >= 35, age_femme <= 44)))
taille_fam <- imap_dfr(ISSUES5, function(iss, lib) imap_dfr(SPECS3, function(ctrl, sp) {
  x <- iss$x(d23)
  if (sp == "Hors Ile-de-France") x <- filter(x, !dept %in% IDF)
  m <- feols(as.formula(paste(iss$f, "~ tension_10 +", ctrl, "| age_femme")), data = x,
             weights = ~poids, cluster = ~dept)
  ct <- coeftable(m)["tension_10", ]
  tibble(issue = lib, specification = sp, effet_10pts = unname(ct[1]), se = unname(ct[2]), p = unname(ct[4]),
         moyenne = weighted.mean(x[[iss$f]], x$poids), n = nobs(m))
}))
cat("\n§5b Effet de 10 points de tension 2014 sur la taille des familles en 2023 :\n")
print(as.data.frame(taille_fam %>% mutate(across(where(is.double), ~ signif(.x, 3)))), row.names = FALSE)

ecrire(bind_rows(taux_age %>% mutate(bloc = "Taux par age et place", age5 = as.character(age5), .before = 1),
                 effets_age %>% mutate(bloc = "Effet de l'absence de place par age", .before = 1),
                 tension %>% mutate(bloc = "Tension 2014 par departement", .before = 1),
                 fin_vie %>% mutate(bloc = "Fin de vie feconde en 2023", .before = 1),
                 canal %>% mutate(bloc = "Canal de la mobilite", .before = 1),
                 second_age %>% mutate(bloc = "Deuxieme naissance par age", age5 = as.character(age5), .before = 1),
                 meres %>% mutate(bloc = "Meres de 40-44 ans, taille de famille", .before = 1),
                 taille_fam %>% mutate(bloc = "Taille des familles en 2023", .before = 1)),
       "Output/report_naissances_age.csv")
