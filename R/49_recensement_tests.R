# ============================================================================
#  SRCV — 49. Les résultats SRCV à l'épreuve du recensement (2019 et 2023)
# ----------------------------------------------------------------------------
#  Sur les couples extraits par le script 48 (femme de 15 à 49 ans, famille
#  principale d'un ménage ordinaire), on refait les tests de l'article qui ne
#  demandent pas de panel :
#   §1 Place pour un enfant de plus (norme de peuplement) : répartition et
#      taux de naissance par rang (article, tableau « pièces », panneau A).
#   §2 Modèles en coupe : place seule, place et surface, surface seule (en
#      tranches du recensement) ; par rang (article, tableaux « pièces » et
#      « seuil »).
#   §3 Anticipation : mêmes modèles chez les couples installés depuis 2 ans
#      ou plus, 5 ans ou plus, et chez les arrivés depuis moins de 2 ans
#      (article, tableau « surface », panneaux B et C).
#   §4 Géographie : taille de la commune avec et sans la place (P6).
#   §5 Évolution 2019 -> 2023 : situations de logement et part de la baisse
#      du taux de naissance des couples imputable à leur recomposition.
#  Naissance : enfant de la famille de moins d'un an (12 derniers mois).
#  Modèles : probabilité linéaire pondérée (fixest), effets en points ;
#  effets fixes d'âge détaillé de la femme et de département ; erreurs-types
#  regroupées par canton-ou-ville. Contrôles : diplôme, activité de la femme
#  et du conjoint, catégorie sociale, statut d'occupation, déménagement depuis
#  le 1er janvier N-1, taille de la commune ; rangs 2+ : nombre d'enfants et
#  taille du ménage avant la naissance.
#  Sortie : Output/recensement_tests.csv
# ============================================================================

source("R/00_utils.R")
source("R/00_config.R")
source("R/00_norme_peuplement.R")
suppressPackageStartupMessages(library(fixest))
setFixest_notes(FALSE)

SURF_LIB <- c("1" = "Moins de 30 m2", "2" = "30 a 39 m2", "3" = "40 a 59 m2", "4" = "60 a 79 m2",
              "5" = "80 a 99 m2", "6" = "100 a 119 m2", "7" = "120 m2 ou plus")
COM_NIV <- c("Commune non decoupee en IRIS", "Moins de 20 000 hab.", "20 000 a 99 999 hab.",
             "100 000 hab. ou plus", "Petite couronne", "Paris")

d <- map_dfr(c("2019", "2023"), ~ readRDS(file.path(DIR_RP, "_extrait", paste0("couples_RP", .x, ".rds")))) %>%
  filter(metrodom == "M", !is.na(pieces), surf %in% names(SURF_LIB)) %>%
  mutate(cantville = sub(" .*", "", hh),
         rang = if_else(nb_enf_avant == 0, "1er enfant", "Rang 2 et plus"),
         surf_cl = factor(SURF_LIB[surf], levels = SURF_LIB),
         sous60 = surf %in% c("1", "2", "3"),
         statut = factor(case_when(stocd == "10" ~ "Proprietaire", stocd == "22" ~ "Locataire HLM",
                                   stocd %in% c("21", "23") ~ "Locataire prive",
                                   TRUE ~ "Loge gratuitement"),
                         levels = c("Locataire prive", "Locataire HLM", "Proprietaire", "Loge gratuitement")),
         cs6 = if_else(cs %in% as.character(1:6), cs, "Sans"),
         demenage = iran != "1",
         anciennete = case_when(anemr == "00" ~ "Moins de 2 ans", anemr == "01" ~ "2 a 4 ans",
                                TRUE ~ "5 ans ou plus"),
         com_cl = factor(case_when(commune == "75056" ~ "Paris",
                                   dept %in% c("92", "93", "94") ~ "Petite couronne",
                                   is.na(pop_commune) ~ "Commune non decoupee en IRIS",
                                   pop_commune < 20000 ~ "Moins de 20 000 hab.",
                                   pop_commune < 100000 ~ "20 000 a 99 999 hab.",
                                   TRUE ~ "100 000 hab. ou plus"), levels = COM_NIV),
         nb_enf_cl = factor(pmin(nb_enf_avant, 4)), taille_avant = pmin(inper - naiss01, 10),
         tact_conj = coalesce(tact_conj, "Inconnu"))
cat("Couples (France métropolitaine) :\n"); print(count(d, annee, rang, wt = NULL))

pond <- function(x, w) 100 * sum(x * w) / sum(w)

# ── §1 Place pour un enfant de plus : répartition et taux ───────────────────
desc_place <- d %>% group_by(annee, rang, place) %>%
  summarise(n = n(), naissances = sum(naiss01), taux_pct = pond(naiss01, poids),
            part_sous60_pct = pond(sous60, poids), poids = sum(poids), .groups = "drop") %>%
  group_by(annee, rang) %>% mutate(part_pct = 100 * poids / sum(poids)) %>% ungroup() %>% select(-poids)
desc_surf <- d %>% group_by(annee, rang, surf_cl) %>%
  summarise(n = n(), naissances = sum(naiss01), taux_pct = pond(naiss01, poids), poids = sum(poids),
            .groups = "drop") %>%
  group_by(annee, rang) %>% mutate(part_pct = 100 * poids / sum(poids)) %>% ungroup() %>% select(-poids)
cat("\n§1 Place pour un enfant de plus :\n")
print(as.data.frame(desc_place %>% mutate(across(where(is.double), ~ round(.x, 1)))), row.names = FALSE)
cat("\nTaux de naissance par tranche de surface :\n")
print(as.data.frame(desc_surf %>% select(annee, rang, surf_cl, part_pct, taux_pct) %>%
  mutate(across(where(is.double), ~ round(.x, 1)))), row.names = FALSE)

# ── Modèles ─────────────────────────────────────────────────────────────────
CTRL <- "diplome + tact + tact_conj + cs6 + statut + demenage + com_cl"
CTRL_R2 <- paste(CTRL, "+ nb_enf_cl + taille_avant")
SPECS <- list("Place seule"               = "place",
              "Place et surface"          = "place + i(surf_cl, ref = '80 a 99 m2')",
              "Surface seule"             = "i(surf_cl, ref = '80 a 99 m2')")
estimer <- function(donnees, rhs, rang, ctrl = NULL) {
  ctrl <- ctrl %||% (if (rang == "1er enfant") CTRL else CTRL_R2)
  f <- as.formula(paste("naiss01 ~", rhs, "+", ctrl, "| age_femme + dept"))
  m <- feols(f, data = donnees, weights = ~poids, cluster = ~cantville)
  ct <- as.data.frame(coeftable(m))
  tibble(terme = rownames(ct), effet_pts = 100 * ct[, 1], se = 100 * ct[, 2], p = ct[, 4]) %>%
    filter(str_detect(terme, "^place|surf_cl|^statut|^com_cl")) %>%
    mutate(n = nobs(m), naissances = sum(donnees$naiss01),
           taux_pct = pond(donnees$naiss01, donnees$poids))
}
modeles <- function(donnees, specs = SPECS, ctrl = NULL) {
  imap_dfr(split(donnees, list(donnees$annee, donnees$rang), sep = "|"), function(x, cle) {
    k <- str_split_1(cle, "\\|")
    imap_dfr(specs, function(rhs, lib)
      estimer(x, rhs, k[2], ctrl) %>% mutate(annee = k[1], echantillon = k[2],
                                             specification = lib, .before = 1))
  })
}

# ── §2 Coupe : place et surface ────────────────────────────────────────────
coupe <- modeles(d) %>% mutate(bloc = "Coupe", .before = 1)
cat("\n§2 Coupe (points) :\n")
print(as.data.frame(coupe %>% filter(!str_detect(terme, "statut|com_cl")) %>%
  select(annee, echantillon, specification, terme, effet_pts, se, n, taux_pct) %>%
  mutate(across(where(is.double), ~ round(.x, 2)))), row.names = FALSE)

# ── §3 Anticipation : installés et arrivés récents ─────────────────────────
SOUS <- list("Installes 2 ans et plus" = quote(anemr != "00"),
             "Installes 5 ans et plus" = quote(!anemr %in% c("00", "01")),
             "Arrives depuis moins de 2 ans" = quote(anemr == "00"))
anticipation <- imap_dfr(SOUS, function(cond, lib)
  modeles(filter(d, !!cond), specs = SPECS[c("Place seule", "Surface seule")]) %>%
    mutate(sous_champ = lib, .before = 1)) %>%
  mutate(bloc = "Anticipation", .before = 1)
cat("\n§3 Anticipation (points) :\n")
print(as.data.frame(anticipation %>% filter(str_detect(terme, "^place|Moins de 30|30 a 39|40 a 59|60 a 79")) %>%
  select(sous_champ, annee, echantillon, specification, terme, effet_pts, se, n, taux_pct) %>%
  mutate(across(where(is.double), ~ round(.x, 2)))), row.names = FALSE)
part_recents <- d %>% group_by(annee, rang, naiss01) %>%
  summarise(part_arrives_moins_2ans = pond(anemr == "00", poids),
            part_demenage_depuis_n1 = pond(demenage, poids), .groups = "drop")
cat("\nPart des couples arrivés depuis moins de deux ans, selon la naissance :\n")
print(as.data.frame(part_recents %>% mutate(across(where(is.double), ~ round(.x, 1)))), row.names = FALSE)

# ── §4 Géographie : taille de la commune, avec et sans la place ─────────────
GEO <- list("Commune, sans logement" = "1", "Commune, avec la place" = "place")
CTRL_GEO <- "diplome + tact + tact_conj + cs6 + statut + demenage + com_cl"
geo_desc <- d %>% group_by(annee, com_cl) %>%
  summarise(n = n(), taux_pct = pond(naiss01, poids),
            taux_1er_pct = pond(naiss01[rang == "1er enfant"], poids[rang == "1er enfant"]),
            part_sans_place_1er_pct = pond((place != PLACE[1])[rang == "1er enfant"], poids[rang == "1er enfant"]),
            part_sous60_pct = pond(sous60, poids), .groups = "drop")
geo <- imap_dfr(GEO, function(rhs, lib) {
  imap_dfr(split(d, list(d$annee, d$rang), sep = "|"), function(x, cle) {
    k <- str_split_1(cle, "\\|")
    ctrl <- if (k[2] == "1er enfant") CTRL_GEO else paste(CTRL_GEO, "+ nb_enf_cl + taille_avant")
    # pas d'effet fixe de département ici : il absorberait le contraste entre communes
    m <- feols(as.formula(paste("naiss01 ~", rhs, "+", ctrl, "| age_femme")), data = x,
               weights = ~poids, cluster = ~cantville)
    ct <- as.data.frame(coeftable(m))
    w <- wald(m, keep = "^com_cl", print = FALSE)
    tibble(terme = rownames(ct), effet_pts = 100 * ct[, 1], se = 100 * ct[, 2], p = ct[, 4]) %>%
      filter(str_detect(terme, "^com_cl|^place")) %>%
      mutate(annee = k[1], echantillon = k[2], specification = lib, p_joint_commune = w$p,
             n = nobs(m), taux_pct = pond(x$naiss01, x$poids), .before = 1)
  })
}) %>% mutate(bloc = "Geographie", .before = 1)
cat("\n§4 Géographie : taux par taille de commune\n")
print(as.data.frame(geo_desc %>% mutate(across(where(is.double), ~ round(.x, 1)))), row.names = FALSE)
cat("\nEffets de la taille de commune (réf. : communes non découpées), points :\n")
print(as.data.frame(geo %>% select(annee, echantillon, specification, terme, effet_pts, se, p_joint_commune) %>%
  mutate(across(where(is.double), ~ round(.x, 3)))), row.names = FALSE)

# ── §5 Évolution 2019 -> 2023 : recomposition des situations de logement ────
#  Taux de naissance des couples de 2023 si, à l'intérieur de chaque rang, les
#  situations (place, ou tranche de surface) avaient gardé leurs parts de 2019,
#  aux taux de naissance de 2023 et à la structure par rang de 2023. Comme dans
#  la décomposition SRCV : écart relatif au taux observé, rapporté à la baisse.
recomposer <- function(var) {
  t <- d %>% group_by(annee, rang, cle = .data[[var]]) %>%
    summarise(w = sum(poids), taux = pond(naiss01, poids), .groups = "drop") %>%
    group_by(annee, rang) %>% mutate(s = w / sum(w)) %>% ungroup()
  s_rang <- d %>% filter(annee == "2023") %>% count(rang, wt = poids, name = "S") %>% mutate(S = S / sum(S))
  j <- t %>% select(annee, rang, cle, s, taux) %>%
    pivot_wider(names_from = annee, values_from = c(s, taux), values_fill = 0) %>%
    left_join(s_rang, by = "rang")
  obs <- sum(j$S * j$s_2023 * j$taux_2023); cf <- sum(j$S * j$s_2019 * j$taux_2023)
  t19 <- pond(d$naiss01[d$annee == "2019"], d$poids[d$annee == "2019"])
  tibble(variable = var, taux_2019 = t19, taux_2023 = obs, baisse_pct = 100 * (obs / t19 - 1),
         taux_2023_situations_2019 = cf, ecart_pct = 100 * (cf / obs - 1),
         part_de_la_baisse_pct = 100 * (cf / obs - 1) / (1 - obs / t19))
}
evolution <- bind_rows(recomposer("place"), recomposer("surf_cl"))
cat("\n§5 Évolution 2019 -> 2023 :\n"); print(as.data.frame(evolution %>% mutate(across(where(is.double), ~ round(.x, 2)))))

ecrire(bind_rows(desc_place %>% mutate(bloc = "Descriptif place", .before = 1),
                 desc_surf %>% mutate(bloc = "Descriptif surface", surf_cl = as.character(surf_cl), .before = 1),
                 coupe, anticipation,
                 part_recents %>% mutate(bloc = "Part des arrives recents", .before = 1),
                 geo_desc %>% mutate(bloc = "Descriptif geographie", com_cl = as.character(com_cl), .before = 1),
                 geo, evolution %>% mutate(bloc = "Evolution 2019-2023", .before = 1)),
       "Output/recensement_tests.csv")
