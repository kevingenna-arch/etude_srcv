# ============================================================================
#  68. Décomposition de la baisse des naissances par femme au recensement,
#      2014 -> 2023 : la référence de la décomposition
# ----------------------------------------------------------------------------
#  SRCV (scripts 34, 39, 41) décompose la baisse de l'ICF 2014-2025 sur
#  quelques milliers de couples. Le recensement en compte 1,9 million par
#  millésime, avec 100 fois plus de naissances ; il contredit SRCV sur le
#  logement (pas de rétrécissement des surfaces) et sur l'âge des couples
#  (script 66, §2 bis). Il devient la référence ; SRCV reste en variante, seul
#  à mesurer le niveau de vie (quartiles).
#
#  Naissances par femme du recensement (somme des taux par âge, 15-49 ans,
#  script 66) : 1,77 (2014) -> 1,50 (2023), −15 % (ICF Insee −16 %).
#  1. Trois termes (script 66, §2) : part des femmes en couple à âge donné,
#     taux des couples, taux hors couple.
#  2. Le terme « taux des couples » se partage :
#     - logement : surface, place pour un enfant, statut d'occupation, et les
#       trois croisés ; répartition de 2014 à âge et nombre d'enfants donnés
#       (nombre d'enfants de 2023), taux de 2023, pondérée par la part en
#       couple de 2023 à chaque âge ; couples de la famille principale
#       (extraits du script 48) ;
#     - politique familiale (barème) : part de la baisse 2014-2023 estimée dans
#       l'ERFS (script 57), même période ; variante avec l'effet 2005-2023 ;
#     - offre d'accueil (script 65) : part du recul 2019-2023 du taux des
#       couples, rapportée au terme « taux des couples » ;
#     - âge des couples : neutralisé par construction (somme de taux par âge) ;
#       à parité donnée, il ne joue pas non plus sur le taux brut (script 66) ;
#     - le reste, non expliqué : moins de premiers enfants à âge donné, qui se
#       répercute sur les rangs suivants (script 66, §2 bis).
#  3. Vie en couple : part liée à la tension immobilière (script 67).
#  4. Par diplôme de la femme, 25-49 ans (avant 25 ans, beaucoup étudient).
#  ⚠️ Naissance = nouveau-né vivant avec sa mère ; nombre d'enfants = enfants
#     présents au logement (sous-estimé après 40 ans).
#  Entrées : cache du script 66 (cohabitation_cellules.rds), extraits du
#            script 48, sorties des scripts 57, 65, 66 et 67
#  Sortie  : Output/decomposition_recensement.csv
# ============================================================================

source("R/00_utils.R")
source("R/00_config.R")

ANS    <- c("2014", "2023")
F_CELL <- file.path(DIR_RP, "_extrait", "cohabitation_cellules.rds")
if (!file.exists(F_CELL)) stop("Lancer d'abord R/66_vie_en_couple_recensement.R (cache des cellules).")
DIPLOMES <- c("Infra CAP", "CAP-BEP", "Bac", "Superieur")

# ── §1 Trois termes ────────────────────────────────────────────────────────
femmes <- as_tibble(readRDS(F_CELL)) %>% filter(annee %in% ANS, sexe == "2") %>% mutate(couple = cohab == "En couple")
par_age <- function(x) x %>% group_by(annee, age, couple) %>% summarise(n = sum(n), naiss = sum(naiss), .groups = "drop") %>%
  group_by(annee, age) %>% mutate(p = n / sum(n)) %>% ungroup() %>%
  mutate(r = if_else(n > 0, naiss / n, 0), k = if_else(couple, "c", "h")) %>% select(annee, age, k, p, r) %>%
  pivot_wider(names_from = k, values_from = c(p, r), values_fill = 0)
trois_termes <- function(l) {
  j <- inner_join(filter(l, annee == "2014"), filter(l, annee == "2023"), by = "age", suffix = c("0", "1"))
  tibble(icf_2014 = sum(j$p_c0 * j$r_c0 + j$p_h0 * j$r_h0), icf_2023 = sum(j$p_c1 * j$r_c1 + j$p_h1 * j$r_h1),
         vie_en_couple = sum((j$p_c1 - j$p_c0) * (j$r_c0 - j$r_h0)), taux_couples = sum(j$p_c1 * (j$r_c1 - j$r_c0)),
         hors_couple = sum(j$p_h1 * (j$r_h1 - j$r_h0))) %>% mutate(variation = icf_2023 - icf_2014, .after = icf_2023)
}
l_ens <- par_age(femmes)
tt <- trois_termes(l_ens)
cat("Naissances par femme du recensement :", round(tt$icf_2014, 3), "->", round(tt$icf_2023, 3),
    sprintf("(%+.1f %%)\n", 100 * tt$variation / tt$icf_2014))
print(as.data.frame(tt %>% mutate(across(c(vie_en_couple, taux_couples, hors_couple), ~ 100 * .x / tt$variation,
                                         .names = "part_{.col}_pct")) %>% mutate(across(where(is.double), ~ round(.x, 3)))))

# ── §2 Logement, à âge et nombre d'enfants donnés ──────────────────────────
cp <- map_dfr(ANS, ~ readRDS(file.path(DIR_RP, "_extrait", paste0("couples_RP", .x, ".rds")))) %>%
  filter(metrodom == "M", surf %in% as.character(1:7), !is.na(place), !is.na(stocd)) %>%
  transmute(annee, poids, naiss01, age = age_femme, parite = pmin(nb_enf_avant, 3L), dipl = as.character(diplome4),
            surface = surf, surface3 = case_when(surf %in% c("1", "2", "3") ~ "Moins de 60 m2",
                                                 surf %in% c("4", "5") ~ "60 a 99 m2", TRUE ~ "100 m2 ou plus"),
            place = as.character(place),
            statut = case_when(stocd == "10" ~ "Proprietaire", stocd == "22" ~ "Locataire HLM",
                               stocd %in% c("21", "23") ~ "Locataire prive", TRUE ~ "Loge gratuitement"),
            croise = paste(statut, surface3, place, sep = " | "))
cat("\nCouples (famille principale, logement renseigné) :", paste(table(cp$annee), collapse = " / "), "\n")
LOGEMENT <- c("Surface (7 tranches)" = "surface", "Place pour un enfant" = "place",
              "Statut d'occupation" = "statut", "Statut x surface x place" = "croise")
#  Effet sur la somme des taux : sum_a p_c,2023(a) x [taux observé(a) − taux à logement de 2014(a)] ;
#  négatif si le logement des couples a évolué dans le sens de moins de naissances.
effet_logement <- function(x, l, var) {
  cel <- x %>% group_by(annee, age, parite, h = .data[[var]]) %>%
    summarise(w = sum(poids), nb = sum(poids * naiss01), .groups = "drop")
  ak <- cel %>% group_by(annee, age, parite) %>% summarise(W = sum(w), R = sum(nb) / sum(w), .groups = "drop") %>%
    group_by(annee, age) %>% mutate(S = W / sum(W)) %>% ungroup()
  a1 <- ak %>% filter(annee == "2023") %>% select(age, parite, S1 = S, R1 = R)
  cf <- cel %>% filter(annee == "2014") %>%
    left_join(ak %>% filter(annee == "2014") %>% select(age, parite, W), by = c("age", "parite")) %>%
    transmute(age, parite, h, s0 = w / W) %>%
    left_join(cel %>% filter(annee == "2023") %>% transmute(age, parite, h, r1 = nb / w), by = c("age", "parite", "h")) %>%
    inner_join(a1, by = c("age", "parite")) %>% mutate(r1 = coalesce(r1, R1)) %>%   # cellule absente en 2023 : taux de l'âge x parité
    group_by(age) %>% summarise(r_cf = sum(S1 * s0 * r1), .groups = "drop")
  a1 %>% group_by(age) %>% summarise(r_obs = sum(S1 * R1), .groups = "drop") %>% inner_join(cf, by = "age") %>%
    inner_join(l %>% filter(annee == "2023") %>% select(age, p_c1 = p_c), by = "age") %>%
    summarise(effet = sum(p_c1 * (r_obs - r_cf))) %>% pull(effet)
}
logement <- imap_dfr(LOGEMENT, ~ tibble(variable = .y, effet = effet_logement(cp, l_ens, .x))) %>%
  mutate(part_pct = 100 * effet / tt$variation)
cat("\nLogement : part de la baisse due à l'évolution du logement des couples, à âge et nombre d'enfants donnés (%) :\n")
print(as.data.frame(logement %>% mutate(across(where(is.double), ~ round(.x, 4)))), row.names = FALSE)
cat("\nRépartition des couples (%) :\n")
for (v in c("surface3", "place", "statut"))
  print(as.data.frame(cp %>% count(annee, m = .data[[v]], wt = poids) %>% group_by(annee) %>%
                        mutate(pct = round(100 * n / sum(n), 1)) %>% select(-n) %>% pivot_wider(names_from = annee, values_from = pct)))

# ── §3 Politique familiale, offre d'accueil, âge, tension ──────────────────
erfs <- read.csv("Output/erfs_politique_familiale.csv", encoding = "UTF-8") %>%
  filter(bloc == "Part de la baisse 2014-2023", mesure == "Gain et gain a bareme fixe", rang == "Ensemble, effet commun") %>%
  select(estimation, part_baisse_icf_pct, ic_bas, ic_haut)
stopifnot(nrow(erfs) == 2)
cat("\nPolitique familiale (ERFS, part de la baisse 2014-2023) :\n"); print(erfs)
acc <- read.csv("Output/accueil_jeune_enfant.csv", encoding = "UTF-8") %>%
  filter(!is.na(part_pct), niveau == "Departement", echantillon == "Ensemble")
stopifnot(nrow(acc) == 1)
k_couples <- tt$taux_couples / tt$variation          # le terme « taux des couples », en part de la baisse
accueil <- tibble(part = acc$part_pct * k_couples, bas = acc$part_bas * k_couples, haut = acc$part_haut * k_couples)
r66 <- read.csv("Output/vie_en_couple_recensement.csv", encoding = "UTF-8")
age_brut <- r66 %>% filter(bloc == "Recomposition du taux brut des couples", cellules == "age dans parite (parite de 2023)") %>%
  pull(part_de_la_baisse_pct)
r67 <- read.csv("Output/vie_en_couple_canton.csv", encoding = "UTF-8")
recul <- r67 %>% filter(startsWith(bloc, "Recul et tension"), sexe == "Femmes")
tens <- r67 %>% filter(bloc == "Modeles", sexe == "Femmes", annee == 2023, groupe == "25-29 ans", issue == "En couple",
                       specification == "+ departement x millesime") %>%
  mutate(moy = if_else(startsWith(tension, "Interne"), recul$tension_int_moy, recul$abc_moy),
         part_du_recul_pct = 100 * effet_pts * moy / 10 / recul$recul_pts,
         part_baisse_pct = part_du_recul_pct * tt$vie_en_couple / tt$variation)
stopifnot(nrow(tens) == 2)
cat("\nTension immobilière (script 67) : part du recul de la vie en couple à 25-29 ans, et de la baisse (%) :\n")
print(as.data.frame(tens %>% select(tension, effet_pts, moy, part_du_recul_pct, part_baisse_pct) %>%
                      mutate(across(where(is.double), ~ round(.x, 2)))), row.names = FALSE)

# ── §4 Synthèse ────────────────────────────────────────────────────────────
pc <- function(x) 100 * x / tt$variation
lg <- logement$part_pct[logement$variable == "Statut x surface x place"]
pf <- erfs %>% filter(estimation == "2014-2023"); pf2 <- erfs %>% filter(estimation == "2005-2023")
synthese <- tribble(
  ~facteur, ~part_pct, ~bas, ~haut, ~lecture,
  "Recul de la vie en couple", pc(tt$vie_en_couple), NA, NA, "Moins de femmes en couple a age donne",
  "  dont tension immobiliere", mean(tens$part_baisse_pct), min(tens$part_baisse_pct), max(tens$part_baisse_pct), "Script 67, deux mesures de la tension",
  "Naissances hors couple", pc(tt$hors_couple), NA, NA, "Taux des femmes qui ne vivent pas en couple",
  "Taux des couples a age donne", pc(tt$taux_couples), NA, NA, "Dont :",
  "  Logement (statut x surface x place)", lg, min(logement$part_pct), max(logement$part_pct), "Fourchette : chaque variable seule ou croisees",
  "  Politique familiale (bareme)", pf$part_baisse_icf_pct, pf$ic_bas, pf$ic_haut, "ERFS, effet 2014-2023, IC a 95 %",
  "  Politique familiale, effet 2005-2023", pf2$part_baisse_icf_pct, pf2$ic_bas, pf2$ic_haut, "Variante",
  "  Offre d'accueil", accueil$part, accueil$bas, accueil$haut, "Script 65, recul 2019-2023",
  "  Age des couples", 0, NA, NA, sprintf("Neutralise par construction ; taux brut, a parite donnee : %.1f %%", age_brut),
  "  Non explique", pc(tt$taux_couples) - lg - pf$part_baisse_icf_pct - accueil$part, NA, NA,
  "Moins de premiers enfants a age donne") %>%
  mutate(enfants_par_femme = -part_pct / 100 * tt$variation)
cat("\nSynthèse (parts en % de la baisse des naissances par femme, 2014-2023) :\n")
print(as.data.frame(synthese %>% mutate(across(where(is.double), ~ round(.x, 1)))), row.names = FALSE)

# ── §5 Par diplôme de la femme, 25-49 ans ──────────────────────────────────
#  Parts rapportées à la baisse propre à chaque groupe (somme des taux de 25 à 49 ans).
par_diplome <- map_dfr(c("Ensemble (25-49 ans)", DIPLOMES), function(g) {
  f <- femmes %>% filter(age >= 25, if (g %in% DIPLOMES) dipl == g else TRUE)
  x <- cp %>% filter(age >= 25, if (g %in% DIPLOMES) dipl == g else TRUE)
  l <- par_age(f); t3 <- trois_termes(l)
  lo <- effet_logement(x, l, "croise")
  t3 %>% mutate(groupe = g, logement = lo, .before = 1)
}) %>% mutate(baisse_pct = 100 * variation / icf_2014,
              across(c(vie_en_couple, logement, taux_couples, hors_couple), ~ 100 * .x / variation, .names = "part_{.col}_pct"),
              part_taux_couples_hors_logement_pct = part_taux_couples_pct - part_logement_pct,
              part_femmes_2014_pct = NA_real_, part_femmes_2023_pct = NA_real_)
poids_dipl <- femmes %>% filter(age >= 25, dipl %in% DIPLOMES) %>% count(annee, dipl, wt = n) %>% group_by(annee) %>%
  mutate(pct = 100 * n / sum(n)) %>% select(-n) %>% pivot_wider(names_from = annee, values_from = pct, names_prefix = "p")
par_diplome <- par_diplome %>% left_join(poids_dipl, by = c("groupe" = "dipl")) %>%
  mutate(part_femmes_2014_pct = p2014, part_femmes_2023_pct = p2023) %>% select(-p2014, -p2023)
cat("\nPar diplôme (25-49 ans ; parts en % de la baisse du groupe) :\n")
print(as.data.frame(par_diplome %>% select(groupe, icf_2014, icf_2023, baisse_pct, part_vie_en_couple_pct, part_logement_pct,
                                           part_taux_couples_hors_logement_pct, part_hors_couple_pct,
                                           part_femmes_2014_pct, part_femmes_2023_pct) %>%
                      mutate(across(where(is.double), ~ round(.x, 2)))), row.names = FALSE)

# ── Sortie ─────────────────────────────────────────────────────────────────
ecrire(bind_rows(tt %>% mutate(bloc = "Trois termes", .before = 1),
                 logement %>% mutate(bloc = "Logement", .before = 1),
                 synthese %>% mutate(bloc = "Synthese", .before = 1),
                 tens %>% select(tension, effet_pts, moy, part_du_recul_pct, part_baisse_pct) %>% mutate(bloc = "Tension", .before = 1),
                 par_diplome %>% mutate(bloc = "Par diplome", .before = 1)),
       "Output/decomposition_recensement.csv")
