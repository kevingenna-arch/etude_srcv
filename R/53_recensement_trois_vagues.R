# ============================================================================
#  SRCV — 53. Recensement sur trois millésimes (2014, 2019, 2023) : robustesse
#             des résultats et mobilité résidentielle
# ----------------------------------------------------------------------------
#  Extraits du script 48 (couples de la famille principale, femme 15-49 ans,
#  France métropolitaine). RP 2014 = enquêtes 2012-2016 ; RP 2019 = 2017-2021 ;
#  RP 2023 = 2021-2025 (2014 et 2019 ne se recouvrent pas ; 2019 et 2023
#  partagent l'enquête de 2021).
#  Partie A — les tests de l'annexe C de l'article (script 49), étendus à
#  2014, avec un diplôme en quatre postes commun aux trois millésimes (le
#  script 49, à cinq postes, reste celui de l'article) :
#   A1 place pour un enfant de plus et surface : répartition, taux ;
#   A2 modèles en coupe (place, place et surface, surface) ;
#   A3 anticipation (installés depuis 2 ou 5 ans, arrivés depuis moins de 2 ans) ;
#   A4 taille de la commune, sans puis avec la place ;
#   A5 évolution 2014 -> 2019 -> 2023 : recomposition des situations.
#  Partie B — mobilité résidentielle (la femme habitait-elle un autre logement
#  au 1er janvier de l'année précédente ?) :
#   B1 taux par millésime, statut d'occupation (locatif privé, HLM,
#      propriétaires), âge, diplôme, place disponible ;
#   B2 écart avec 2014 à caractéristiques données, par sous-population ;
#   B3 où vont les couples mobiles : place pour un enfant, propriété ;
#   B4 rotation du parc locatif : part des locataires arrivés dans l'année.
#  Modèles : probabilité linéaire pondérée (fixest), effets fixes d'âge
#  détaillé et de département, erreurs-types par canton-ou-ville.
#  Sortie : Output/recensement_trois_vagues.csv
# ============================================================================

source("R/00_utils.R")
source("R/00_config.R")
source("R/00_norme_peuplement.R")
suppressPackageStartupMessages(library(fixest))
setFixest_notes(FALSE)

ANS <- c("2014", "2019", "2023")
SURF_LIB <- c("1" = "Moins de 30 m2", "2" = "30 a 39 m2", "3" = "40 a 59 m2", "4" = "60 a 79 m2",
              "5" = "80 a 99 m2", "6" = "100 a 119 m2", "7" = "120 m2 ou plus")
COM_NIV <- c("Commune non decoupee en IRIS", "Moins de 20 000 hab.", "20 000 a 99 999 hab.",
             "100 000 hab. ou plus", "Petite couronne", "Paris")

d <- map_dfr(ANS, ~ readRDS(file.path(DIR_RP, "_extrait", paste0("couples_RP", .x, ".rds")))) %>%
  filter(metrodom == "M", !is.na(pieces), surf %in% names(SURF_LIB), !is.na(diplome4)) %>%
  mutate(cantville = sub(" .*", "", hh),
         rang = if_else(nb_enf_avant == 0, "1er enfant", "Rang 2 et plus"),
         surf_cl = factor(SURF_LIB[surf], levels = SURF_LIB),
         sous60 = surf %in% c("1", "2", "3"),
         statut = factor(case_when(stocd == "10" ~ "Proprietaire", stocd == "22" ~ "Locataire HLM",
                                   stocd %in% c("21", "23") ~ "Locataire prive", TRUE ~ "Loge gratuitement"),
                         levels = c("Locataire prive", "Locataire HLM", "Proprietaire", "Loge gratuitement")),
         cs6 = if_else(cs %in% as.character(1:6), cs, "Sans"),
         demenage = iran != "1",
         mobile_an = as.integer(iran %in% as.character(c(0, 2:9))),
         com_cl = factor(case_when(commune == "75056" ~ "Paris",
                                   dept %in% c("92", "93", "94") ~ "Petite couronne",
                                   is.na(pop_commune) ~ "Commune non decoupee en IRIS",
                                   pop_commune < 20000 ~ "Moins de 20 000 hab.",
                                   pop_commune < 100000 ~ "20 000 a 99 999 hab.",
                                   TRUE ~ "100 000 hab. ou plus"), levels = COM_NIV),
         age_cl = cut(age_femme, c(14, 29, 34, 39, 49), labels = c("15-29", "30-34", "35-39", "40-49")),
         enfants = factor(pmin(nb_enf_avant, 3)), nb_enf_cl = enfants,
         taille_avant = pmin(inper - naiss01, 10), tact_conj = coalesce(tact_conj, "Inconnu"))
cat("Couples (France métropolitaine) :\n"); print(count(d, annee, rang))
pond <- function(x, w) 100 * sum(x * w) / sum(w)
coefs <- function(m, motif) {
  ct <- as.data.frame(coeftable(m))
  tibble(terme = rownames(ct), effet_pts = 100 * ct[, 1], se = 100 * ct[, 2], p = ct[, 4]) %>%
    filter(grepl(motif, terme))
}

# ── A1 Répartition et taux ──────────────────────────────────────────────────
desc_place <- d %>% group_by(annee, rang, place) %>%
  summarise(n = n(), naissances = sum(naiss01), taux_pct = pond(naiss01, poids),
            part_sous60_pct = pond(sous60, poids), poids = sum(poids), .groups = "drop") %>%
  group_by(annee, rang) %>% mutate(part_pct = 100 * poids / sum(poids)) %>% ungroup() %>% select(-poids)
desc_surf <- d %>% group_by(annee, rang, surf_cl) %>%
  summarise(n = n(), taux_pct = pond(naiss01, poids), poids = sum(poids), .groups = "drop") %>%
  group_by(annee, rang) %>% mutate(part_pct = 100 * poids / sum(poids)) %>% ungroup() %>% select(-poids)
cat("\nA1 Place pour un enfant de plus (part et taux, %) :\n")
print(as.data.frame(desc_place %>% transmute(annee, rang, place, part = round(part_pct, 1), taux = round(taux_pct, 1)) %>%
  pivot_wider(names_from = annee, values_from = c(part, taux))), row.names = FALSE)
cat("\nA1 Taux de première naissance par tranche de surface (%) :\n")
print(as.data.frame(desc_surf %>% filter(rang == "1er enfant") %>% transmute(annee, surf_cl, taux = round(taux_pct, 1)) %>%
  pivot_wider(names_from = annee, values_from = taux)), row.names = FALSE)

# ── A2-A3 Modèles en coupe et anticipation ─────────────────────────────────
CTRL <- "diplome4 + tact + tact_conj + cs6 + statut + demenage + com_cl"
CTRL_R2 <- paste(CTRL, "+ nb_enf_cl + taille_avant")
SPECS <- list("Place seule" = "place", "Place et surface" = "place + i(surf_cl, ref = '80 a 99 m2')",
              "Surface seule" = "i(surf_cl, ref = '80 a 99 m2')")
estimer <- function(x, rhs, rang) {
  ctrl <- if (rang == "1er enfant") CTRL else CTRL_R2
  m <- feols(as.formula(paste("naiss01 ~", rhs, "+", ctrl, "| age_femme + dept")), data = x,
             weights = ~poids, cluster = ~cantville)
  coefs(m, "^place|surf_cl|^statut|^com_cl") %>%
    mutate(n = nobs(m), naissances = sum(x$naiss01), taux_pct = pond(x$naiss01, x$poids))
}
SOUS <- list("Tous les couples" = function(x) x,
             "Installes 2 ans et plus" = function(x) filter(x, anemr != "00"),
             "Installes 5 ans et plus" = function(x) filter(x, !anemr %in% c("00", "01")),
             "Arrives depuis moins de 2 ans" = function(x) filter(x, anemr == "00"))
coupe <- map_dfr(ANS, function(an) map_dfr(c("1er enfant", "Rang 2 et plus"), function(rg) {
  x <- filter(d, annee == an, rang == rg)
  bind_rows(
    imap_dfr(SPECS, ~ estimer(x, .x, rg) %>% mutate(sous_champ = "Tous les couples", specification = .y)),
    imap_dfr(SOUS[-1], function(f, sc) imap_dfr(SPECS[c("Place seule", "Surface seule")],
      ~ estimer(f(x), .x, rg) %>% mutate(sous_champ = sc, specification = .y)))) %>%
    mutate(annee = an, rang = rg, .before = 1)
}))
cat("\nA2-A3 Absence de place et logement de 40 à 59 m² (points ; en % du taux) :\n")
print(as.data.frame(coupe %>%
  filter((specification == "Place seule" & grepl("Pas de place", terme)) |
         (specification == "Surface seule" & grepl("40 a 59", terme))) %>%
  transmute(rang, sous_champ, mesure = if_else(grepl("place", terme), "Pas de place", "40-59 m2"), annee,
            lib = sprintf("%.1f (%.0f %%)", effet_pts, 100 * effet_pts / taux_pct)) %>%
  pivot_wider(names_from = annee, values_from = lib)), row.names = FALSE)

# ── A4 Taille de la commune ────────────────────────────────────────────────
geo <- map_dfr(ANS, function(an) map_dfr(c("1er enfant", "Rang 2 et plus"), function(rg) {
  x <- filter(d, annee == an, rang == rg)
  ctrl <- if (rg == "1er enfant") CTRL else CTRL_R2
  map_dfr(c("Sans la place" = "1", "Avec la place" = "place"), function(rhs) {
    m <- feols(as.formula(paste("naiss01 ~", rhs, "+", ctrl, "| age_femme")), data = x,
               weights = ~poids, cluster = ~cantville)
    coefs(m, "^com_cl|^place") %>% mutate(specification = if_else(rhs == "1", "Sans la place", "Avec la place"))
  }) %>% mutate(annee = an, rang = rg, .before = 1)
}))
geo_desc <- d %>% group_by(annee, com_cl) %>%
  summarise(taux_pct = pond(naiss01, poids),
            taux_1er_pct = pond(naiss01[rang == "1er enfant"], poids[rang == "1er enfant"]),
            sans_place_1er_pct = pond((place != PLACE[1])[rang == "1er enfant"], poids[rang == "1er enfant"]),
            .groups = "drop")
cat("\nA4 Premier enfant à Paris et dans les villes de 100 000 hab. ou plus (points) :\n")
print(as.data.frame(geo %>% filter(rang == "1er enfant", grepl("Paris|100 000", terme)) %>%
  transmute(terme, specification, annee, effet = round(effet_pts, 2)) %>%
  pivot_wider(names_from = annee, values_from = effet)), row.names = FALSE)

# ── A5 Recomposition des situations ────────────────────────────────────────
recomposer <- function(var, a0, a1) {
  t <- d %>% filter(annee %in% c(a0, a1)) %>% group_by(annee, rang, cle = .data[[var]]) %>%
    summarise(w = sum(poids), taux = pond(naiss01, poids), .groups = "drop") %>%
    group_by(annee, rang) %>% mutate(s = w / sum(w)) %>% ungroup()
  S <- d %>% filter(annee == a1) %>% count(rang, wt = poids, name = "S") %>% mutate(S = S / sum(S))
  j <- t %>% select(annee, rang, cle, s, taux) %>%
    pivot_wider(names_from = annee, values_from = c(s, taux), values_fill = 0) %>% left_join(S, by = "rang")
  obs <- sum(j$S * j[[paste0("s_", a1)]] * j[[paste0("taux_", a1)]])
  cf <- sum(j$S * j[[paste0("s_", a0)]] * j[[paste0("taux_", a1)]])
  t0 <- pond(d$naiss01[d$annee == a0], d$poids[d$annee == a0])
  tibble(variable = var, periode = paste(a0, a1, sep = "-"), taux_debut = t0, taux_fin = obs,
         baisse_pct = 100 * (obs / t0 - 1), taux_fin_situations_debut = cf,
         part_de_la_baisse_pct = 100 * (cf / obs - 1) / (1 - obs / t0))
}
evolution <- bind_rows(map_dfr(c("place", "surf_cl"), ~ bind_rows(recomposer(.x, "2014", "2019"),
                                                                  recomposer(.x, "2019", "2023"),
                                                                  recomposer(.x, "2014", "2023"))))
cat("\nA5 Recomposition des situations de logement :\n")
print(as.data.frame(evolution %>% mutate(across(where(is.double), ~ round(.x, 2)))), row.names = FALSE)

# ── B1 Mobilité : taux ─────────────────────────────────────────────────────
mob <- function(x, ...) x %>% group_by(annee, ...) %>%
  summarise(n = n(), mobiles_an_pct = pond(mobile_an, poids), .groups = "drop")
mobilite <- bind_rows(
  mob(d) %>% mutate(groupe = "Ensemble"),
  mob(d, g = statut) %>% mutate(groupe = paste("Statut actuel :", g)) %>% select(-g),
  mob(d, g = age_cl) %>% mutate(groupe = paste("Femme", g, "ans")) %>% select(-g),
  mob(d, g = diplome4) %>% mutate(groupe = paste("Diplome :", g)) %>% select(-g),
  mob(filter(d, rang == "1er enfant"), g = place) %>% mutate(groupe = paste("Sans enfant :", g)) %>% select(-g),
  mob(filter(d, rang != "1er enfant"), g = place) %>% mutate(groupe = paste("Parents :", g)) %>% select(-g))
cat("\nB1 Couples ayant changé de logement depuis le 1er janvier de l'année précédente (%) :\n")
print(as.data.frame(mobilite %>% transmute(groupe, annee, v = round(mobiles_an_pct, 1)) %>%
  pivot_wider(names_from = annee, values_from = v)), row.names = FALSE)

# ── B2 Écart avec 2014 à caractéristiques données ──────────────────────────
f_mob <- function(enfants = TRUE) as.formula(paste(
  "mobile_an ~ i(annee, ref = '2014') + diplome4 + tact + tact_conj + cs6 +",
  if (enfants) "enfants +" else "", "naiss01 | age_femme + dept"))
SOUS_MOB <- list("Ensemble" = d, "Couples sans enfant" = filter(d, rang == "1er enfant"),
                 "Sans enfant, pas de place" = filter(d, rang == "1er enfant", place != PLACE[1]),
                 "Parents" = filter(d, rang != "1er enfant"),
                 "Parents a qui il manque une piece" = filter(d, rang != "1er enfant", place == PLACE[3]))
ecart_mob <- imap_dfr(SOUS_MOB, function(x, lib) {
  m <- feols(f_mob(!grepl("sans enfant", lib, ignore.case = TRUE)), data = x, weights = ~poids, cluster = ~cantville)
  coefs(m, "annee") %>% mutate(groupe = lib, taux_2014_pct = pond(x$mobile_an[x$annee == "2014"], x$poids[x$annee == "2014"]),
                               n = nobs(m), .before = 1)
})
cat("\nB2 Écart de mobilité avec 2014, à caractéristiques données (points) :\n")
print(as.data.frame(ecart_mob %>% mutate(across(where(is.double), ~ round(.x, 2)))), row.names = FALSE)

# ── B3 Où vont les couples mobiles ─────────────────────────────────────────
destination <- d %>% filter(mobile_an == 1) %>% group_by(annee, rang) %>%
  summarise(n = n(), avec_place_pct = pond(place == PLACE[1], poids),
            proprietaires_pct = pond(statut == "Proprietaire", poids),
            hlm_pct = pond(statut == "Locataire HLM", poids),
            meme_commune_pct = pond(iran == "2", poids), .groups = "drop")
cat("\nB3 Couples mobiles : situation dans le nouveau logement (%) :\n")
print(as.data.frame(destination %>% mutate(across(where(is.double), ~ round(.x, 1)))), row.names = FALSE)

# ── B4 Rotation du parc locatif ────────────────────────────────────────────
rotation <- d %>% filter(statut %in% c("Locataire prive", "Locataire HLM", "Proprietaire")) %>%
  group_by(annee, statut, age_cl) %>%
  summarise(n = n(), arrives_dans_l_annee_pct = pond(mobile_an, poids), .groups = "drop")
cat("\nB4 Part des couples arrivés dans l'année, par statut d'occupation actuel et âge (%) :\n")
print(as.data.frame(rotation %>% transmute(statut, age_cl, annee, v = round(arrives_dans_l_annee_pct, 1)) %>%
  pivot_wider(names_from = annee, values_from = v)), row.names = FALSE)

ecrire(bind_rows(desc_place %>% mutate(bloc = "A1 Place", place = as.character(place), .before = 1),
                 desc_surf %>% mutate(bloc = "A1 Surface", surf_cl = as.character(surf_cl), .before = 1),
                 coupe %>% mutate(bloc = "A2-A3 Coupe et anticipation", .before = 1),
                 geo %>% mutate(bloc = "A4 Commune", .before = 1),
                 geo_desc %>% mutate(bloc = "A4 Commune, descriptif", com_cl = as.character(com_cl), .before = 1),
                 evolution %>% mutate(bloc = "A5 Recomposition", .before = 1),
                 mobilite %>% mutate(bloc = "B1 Mobilite", .before = 1),
                 ecart_mob %>% mutate(bloc = "B2 Ecart avec 2014", .before = 1),
                 destination %>% mutate(bloc = "B3 Destination des mobiles", .before = 1),
                 rotation %>% mutate(bloc = "B4 Rotation", statut = as.character(statut),
                                     age_cl = as.character(age_cl), .before = 1)),
       "Output/recensement_trois_vagues.csv")
