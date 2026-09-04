# ============================================================================
#  SRCV — 30. LE SEUIL À 50 M² : BORNE POSÉE OU POINT DE RUPTURE ?
# ----------------------------------------------------------------------------
#  Le script 23 spécifie la surface en tranches 50 / 70 / 90 / 120 m², des
#  nombres ronds posés a priori, et l'étude parle d'une « marche à 50 m² ». Or
#  une marche lue dans des tranches choisies n'est qu'un choix de bornes tant
#  qu'on n'a pas vérifié qu'elle ne dépend pas de la borne. Trois tests :
#
#   (a) TRANCHES FINES de 10 m² sous 70 m² : où la probabilité décroche-t-elle ?
#   (b) BORNE BASSE DÉPLACÉE (40, 45, 50, 55, 60 m²), les autres bornes fixes :
#       l'écart de la tranche la plus petite change-t-il avec la borne ? Avec,
#       pour chaque borne, la part des couples sans enfant qu'elle capte en
#       2010 et 2025 (l'exposition qui alimente la décomposition).
#   (c) RECHERCHE DE POINT DE RUPTURE : une indicatrice « surface < c » pour c
#       de 35 à 75 m², sans puis avec la pente linéaire ; la borne retenue est
#       celle qui minimise la déviance. Si c'est 50, la marche est estimée et
#       non posée ; si la déviance est plate, il n'y a pas de marche nette.
#
#  À LANCER DEPUIS LA RACINE DU PROJET :
#     source("R/30_seuil_surface.R")
#
#  Sortie : Output/seuil_surface_sensibilite.csv
# ============================================================================

source("R/00_prepa_fecondite.R")
library(survey); library(broom); library(marginaleffects)
activer_cache_prepa()

IPC <- charger_ipc()
if (!dir.exists("Output")) dir.create("Output")
ANCIENNETE_MIN <- 3

DONNEES_FEC <- millesimes_prepares(restreindre = TRUE)
df <- bind_rows(imap(DONNEES_FEC, ~ .x %>% select(any_of(c(VARS_INTERET, "datent"))))) %>%
  mutate(annee_num       = as.integer(annee),
         naiss01         = as.integer(naissance == "Oui"),
         cout_m2_reel    = deflater(cout_m2,    annee_num,     IPC, base = 2025),
         niveau_vie_reel = deflater(niveau_vie, annee_num - 1, IPC, base = 2025),
         surface_10 = surface / 10, nv_10k = niveau_vie_reel / 10000,
         anciennete = annee_num - datent,
         rang = factor(if_else(nb_enf_avant == 0, "1er enfant", "Rang 2+"),
                       levels = c("1er enfant", "Rang 2+")),
         across(where(is.factor), ~ factor(as.character(.))),
         statut3 = factor(statut3, levels = c("Locataire", "Proprietaire_accedant",
                                              "Proprietaire_non_accedant")),
         rang = factor(rang, levels = c("1er enfant", "Rang 2+"))) %>%
  filter(!is.na(surface), !is.na(cout_m2_reel), is.finite(cout_m2_reel),
         !is.na(statut3), !is.na(age_femme), !is.na(niveau_vie_reel),
         !is.na(poids), poids > 0)

des <- function(d) svydesign(ids = ~id_cluster, weights = ~poids, data = droplevels(d))
elaguer <- function(f, d) {
  vars <- setdiff(all.vars(f), all.vars(f[[2]]))
  mono <- vars[vapply(vars, function(v) v %in% names(d) &&
                        is.factor(d[[v]]) && nlevels(droplevels(d[[v]])) < 2, logical(1))]
  if (length(mono)) f <- update(f, paste(". ~ . -", paste(mono, collapse = " - ")))
  f
}
ame <- function(m, vars, ...) as_tibble(avg_slopes(m, variables = vars,
                                                   wts = m$prior.weights, ...))
en_points <- function(x) x %>% mutate(across(any_of(c("estimate", "std.error",
                                                      "conf.low", "conf.high")),
                                             ~ 100 * .x))
fit <- function(d, f) {
  d <- droplevels(d)
  svyglm(elaguer(f, d), design = des(d), family = quasibinomial(link = "probit"))
}

F_RANG1 <- naiss01 ~ surface_10 + cout_m2_reel + nv_10k + statut3 +
  age_c + I(age_c^2) + demenage + csp + urbain + zeat + factor(annee)
F_POOLE <- update(F_RANG1, . ~ . + nb_enfants + taille_avant)

d1  <- filter(df, rang == "1er enfant")
d1i <- filter(d1, anciennete >= ANCIENNETE_MIN)
ECH <- list("1er enfant"                           = list(d1,  F_RANG1),
            "1er enfant - installes 3 ans et plus" = list(d1i, F_RANG1),
            "Ensemble"                             = list(df,  F_POOLE))

vide <- tibble(bloc = character(), echantillon = character(), borne = numeric(),
               contraste = character(), n = integer(), naissances = integer(),
               ame_pts = numeric(), se = numeric(), conf.low = numeric(),
               conf.high = numeric(), p.value = numeric(), deviance = numeric(),
               part_2010_pct = numeric(), part_2025_pct = numeric())
ligne <- function(bloc, lib, d, x, borne = NA_real_, deviance = NA_real_) {
  x %>% en_points() %>%
    transmute(bloc = bloc, echantillon = lib, borne = borne,
              contraste = if ("contrast" %in% names(x)) contrast else "surface_10",
              n = nrow(d), naissances = sum(d$naiss01),
              ame_pts = estimate, se = std.error, conf.low, conf.high, p.value,
              deviance = deviance)
}
# Part pondérée des couples sans enfant d'une vague dans une condition logique
part <- function(an, cond) {
  x <- filter(d1, annee == an)
  100 * weighted.mean(cond(x$surface), x$poids)
}

# ============================================================================
#  (a) TRANCHES FINES
# ============================================================================
cat("########## (a) TRANCHES FINES DE 10 M2 ##########\n")
SEUILS_FINS <- c(-Inf, 40, 50, 60, 70, 90, 120, Inf)
LIB_FINS    <- c("Moins de 40 m2", "40 a 49 m2", "50 a 59 m2", "60 a 69 m2",
                 "70 a 89 m2 (ref.)", "90 a 119 m2", "120 m2 et plus")
tranche_fine <- function(s) relevel(factor(cut(s, SEUILS_FINS, labels = LIB_FINS, right = FALSE),
                                          levels = LIB_FINS), ref = "70 a 89 m2 (ref.)")
res_a <- imap_dfr(ECH, function(x, lib) {
  d <- x[[1]] %>% mutate(surf_fin = tranche_fine(surface))
  m <- fit(d, update(x[[2]], . ~ . - surface_10 + surf_fin))
  ligne("(a) Tranches fines", lib, d, ame(m, "surf_fin"))
})
# Exposition : répartition des couples sans enfant par tranche fine
expo_a <- map_dfr(LIB_FINS, function(l) tibble(
  bloc = "(a) Tranches fines", echantillon = "Exposition, couples sans enfant",
  contraste = l,
  part_2010_pct = part("2010", function(s) tranche_fine(s) == l),
  part_2025_pct = part("2025", function(s) tranche_fine(s) == l)))
cat("\nEcart par tranche fine (points de %) :\n")
print(as.data.frame(res_a %>% select(echantillon, contraste, ame_pts, conf.low, conf.high, p.value) %>%
        mutate(across(where(is.numeric), ~ round(.x, 3)))), row.names = FALSE)
cat("\nRepartition des couples sans enfant (%), 2010 -> 2025 :\n")
print(as.data.frame(expo_a %>% select(contraste, part_2010_pct, part_2025_pct) %>%
        mutate(across(where(is.numeric), ~ round(.x, 1)))), row.names = FALSE)

# ============================================================================
#  (b) BORNE BASSE DÉPLACÉE
# ============================================================================
cat("\n\n########## (b) BORNE BASSE DEPLACEE ##########\n")
BORNES <- c(40, 45, 50, 55, 60)
res_b <- map_dfr(BORNES, function(b) {
  s   <- c(-Inf, b, 70, 90, 120, Inf)
  lib <- c(paste("Moins de", b, "m2"), paste(b, "a 69 m2"), "70 a 89 m2 (ref.)",
           "90 a 119 m2", "120 m2 et plus")
  tr  <- function(x) relevel(factor(cut(x, s, labels = lib, right = FALSE), levels = lib),
                             ref = "70 a 89 m2 (ref.)")
  imap_dfr(ECH[1:2], function(x, l) {
    d <- x[[1]] %>% mutate(surf_b = tr(surface))
    m <- fit(d, update(x[[2]], . ~ . - surface_10 + surf_b))
    ligne("(b) Borne basse deplacee", l, d, ame(m, "surf_b"), borne = b) %>%
      filter(str_detect(contraste, "^Moins de|a 69 m2")) %>%
      mutate(part_2010_pct = if_else(str_detect(contraste, "^Moins de"),
                                     part("2010", function(v) v < b), NA_real_),
             part_2025_pct = if_else(str_detect(contraste, "^Moins de"),
                                     part("2025", function(v) v < b), NA_real_))
  })
})
cat("\nEcart de la tranche basse vs 70-89 m2 selon la borne (points de %) :\n")
print(as.data.frame(res_b %>% select(echantillon, borne, contraste, ame_pts, conf.low, conf.high,
                                     p.value, part_2010_pct, part_2025_pct) %>%
        mutate(across(where(is.numeric), ~ round(.x, 3)))), row.names = FALSE)

# ============================================================================
#  (c) RECHERCHE DE POINT DE RUPTURE
# ============================================================================
cat("\n\n########## (c) POINT DE RUPTURE ##########\n")
CANDIDATS <- seq(35, 75, by = 5)
res_c <- map_dfr(CANDIDATS, function(cc) {
  imap_dfr(ECH[1:2], function(x, l) {
    d <- x[[1]] %>% mutate(sous_c = factor(surface < cc, levels = c(FALSE, TRUE),
                                           labels = c("Non", "Oui")))
    bind_rows(
      { m <- fit(d, update(x[[2]], . ~ . - surface_10 + sous_c))
        ligne("(c) Point de rupture, sans pente", l, d, ame(m, "sous_c"), borne = cc,
              deviance = m$deviance) },
      { m <- fit(d, update(x[[2]], . ~ . + sous_c))
        ligne("(c) Point de rupture, avec pente lineaire", l, d, ame(m, "sous_c"), borne = cc,
              deviance = m$deviance) })
  })
})
cat("\nIndicatrice 'surface < c' : ecart (points de %), p et deviance du modele :\n")
print(as.data.frame(res_c %>% select(bloc, echantillon, borne, ame_pts, p.value, deviance) %>%
        mutate(across(where(is.numeric), ~ round(.x, 3)))), row.names = FALSE)
cat("\nBorne de deviance minimale :\n")
print(as.data.frame(res_c %>% group_by(bloc, echantillon) %>%
        slice_min(deviance, n = 1) %>% ungroup() %>%
        select(bloc, echantillon, borne, ame_pts, p.value) %>%
        mutate(across(where(is.numeric), ~ round(.x, 3)))), row.names = FALSE)

write_csv(bind_rows(vide, res_a, expo_a, res_b, res_c), "Output/seuil_surface_sensibilite.csv")

# ============================================================================
#  (d) LA DÉCOMPOSITION 2010-2025 DÉPEND-ELLE DE LA BORNE ?
# ----------------------------------------------------------------------------
#  Le canal « surface » de la décomposition (script 23, §8) est estimé avec les
#  tranches 50/70/90/120. Si la marche est en réalité vers 60 m², la part de la
#  baisse imputable à la surface pourrait en dépendre. On refait le calcul avec
#  trois découpages, sur le modèle « logement prédéterminé ».
# ============================================================================
cat("\n\n########## (d) DECOMPOSITION SELON LES BORNES ##########\n")
BAISSE_RELATIVE <- (ICF_2025 - ICF_2010) / ICF_2010      # ICF_* : 00_config.R
d10 <- filter(df, annee == "2010"); d25 <- filter(df, annee == "2025")
di  <- filter(df, anciennete >= ANCIENNETE_MIN)
part_statut <- function(x) prop.table(tapply(x$poids, droplevels(x$statut3), sum))
p10 <- part_statut(d10); p25 <- part_statut(d25)
poids_statut <- d25$poids * unname((p10 / p25)[as.character(d25$statut3)])
predire <- function(m, d, w = d$poids)
  weighted.mean(as.numeric(predict(m, newdata = d, type = "response")), w)
cf_surface <- d25 %>% mutate(surface = transposer(surface, poids, d10$surface, d10$poids),
                             surface_10 = surface / 10)
cf_tout <- cf_surface %>% mutate(cout_m2_reel = transposer(cout_m2_reel, poids,
                                                           d10$cout_m2_reel, d10$poids))
DECOUPAGES <- list("50 / 70 / 90 / 120 (etude)" = c(-Inf, 50, 70, 90, 120, Inf),
                   "60 / 90 / 120"              = c(-Inf, 60, 90, 120, Inf),
                   "40 / 60 / 90 / 120"         = c(-Inf, 40, 60, 90, 120, Inf))
res_d <- imap_dfr(DECOUPAGES, function(s, nom) {
  lib <- paste0("T", seq_len(length(s) - 1))
  tr  <- function(x) x %>% mutate(surf_tr = factor(cut(surface, s, labels = lib, right = FALSE),
                                                   levels = lib))
  m  <- fit(tr(di), update(F_POOLE, . ~ . - surface_10 + surf_tr))
  p0 <- predire(m, tr(d25))
  ecart <- 100 * (c(predire(m, tr(cf_surface)), predire(m, tr(cf_tout)),
                    predire(m, tr(cf_tout), poids_statut)) / p0 - 1)
  tibble(bornes = nom,
         canal = c("Surface de 2010", "Surface + prix de 2010 (sans les statuts)",
                   "Logement complet de 2010"),
         ecart_relatif_pct = ecart,
         part_de_la_baisse_pct = ecart / (-100 * BAISSE_RELATIVE) * 100)
})
cat("\nPart de la baisse de l'ICF reproduite (%), selon les bornes :\n")
print(as.data.frame(res_d %>% mutate(across(where(is.numeric), ~ round(.x, 2)))), row.names = FALSE)
write_csv(res_d, "Output/seuil_surface_decomposition.csv")

cat("\n=== 30 termine. ===\n")
