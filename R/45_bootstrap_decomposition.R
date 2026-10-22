# ============================================================================
#  SRCV — 45. Intervalles de confiance de la décomposition (bootstrap)
# ----------------------------------------------------------------------------
#  La décomposition des scripts 23 (§5) et 30 (d) donne des valeurs centrales
#  et des bornes selon la spécification, sans intervalle statistique. Ici, on
#  rééchantillonne les ménages (grappes id_cluster, tirées avec remise dans
#  chaque vague), on réestime les modèles « logement prédéterminé » (couples
#  installés depuis 3 ans ou plus) et on refait les contrefactuels :
#  surface, prix au m², statuts d'occupation, surface + prix, logement
#  complet ; en spécification linéaire, puis en tranches 50/70/90/120 et
#  60/90/120. Intervalles à 95 % par les centiles.
#  Les modèles sont estimés par glm pondéré (mêmes coefficients que svyglm ;
#  l'incertitude vient du rééchantillonnage).
#  Période : ANNEE_BASE -> 2025 (SRCV_DEBUT=2014 : 2014-2025 ; sinon 2010-2025).
#  Sortie : Output/decomposition_bootstrap.csv (suffixée selon SRCV_DEBUT)
# ============================================================================

source("R/00_prepa_fecondite.R")
activer_cache_prepa()
IPC <- charger_ipc()
set.seed(2026)
B <- as.integer(Sys.getenv("SRCV_BOOT", "300"))
ANCIENNETE_MIN <- 3
BAISSE <- (ICF_2025 - ICF_BASE) / ICF_BASE

df <- bind_rows(imap(millesimes_prepares(restreindre = TRUE),
                     ~ .x %>% select(any_of(c(VARS_INTERET, "datent"))))) %>%
  mutate(annee_num       = as.integer(annee),
         naiss01         = as.integer(naissance == "Oui"),
         cout_m2_reel    = deflater(cout_m2,    annee_num,     IPC, base = 2025),
         niveau_vie_reel = deflater(niveau_vie, annee_num - 1, IPC, base = 2025),
         surface_10 = surface / 10, nv_10k = niveau_vie_reel / 10000,
         anciennete = annee_num - datent,
         annee_f = factor(annee),
         across(where(is.factor) & !annee_f, ~ factor(as.character(.))),
         statut3 = factor(statut3, levels = c("Locataire", "Proprietaire_accedant",
                                              "Proprietaire_non_accedant"))) %>%
  filter(!is.na(surface), !is.na(cout_m2_reel), is.finite(cout_m2_reel),
         !is.na(statut3), !is.na(age_femme), !is.na(niveau_vie_reel),
         !is.na(poids), poids > 0)
F_POOLE <- naiss01 ~ surface_10 + cout_m2_reel + nv_10k + statut3 + age_c + I(age_c^2) +
  demenage + csp + urbain + zeat + annee_f + nb_enfants + taille_avant
# Même échantillon que les scripts 23 et 30 : les observations incomplètes sont
# écartées de l'estimation (glm), pas des distributions de référence.

DECOUPAGES <- list("Lineaire" = NULL, "Tranches 50/70/90/120" = c(-Inf, 50, 70, 90, 120, Inf),
                   "Tranches 60/90/120" = c(-Inf, 60, 90, 120, Inf))
tranches <- function(d, s) if (is.null(s)) d else
  d %>% mutate(surf_tr = factor(cut(surface, s, labels = paste0("T", seq_len(length(s) - 1)),
                                    right = FALSE), levels = paste0("T", seq_len(length(s) - 1))))
formule <- function(s) if (is.null(s)) F_POOLE else update(F_POOLE, . ~ . - surface_10 + surf_tr)
# Chez les installés, certains facteurs n'ont qu'une modalité (demenage) : retirés.
elaguer <- function(f, d) {
  vars <- setdiff(all.vars(f), all.vars(f[[2]]))
  mono <- vars[vapply(vars, function(v) v %in% names(d) &&
                        is.factor(d[[v]]) && nlevels(droplevels(d[[v]])) < 2, logical(1))]
  if (length(mono)) f <- update(f, paste(". ~ . -", paste(mono, collapse = " - ")))
  f
}
predire <- function(m, d, w = d$poids)
  weighted.mean(predict(m, newdata = d, type = "response"), w, na.rm = TRUE)

decomposer <- function(d) {
  db <- filter(d, annee == ANNEE_BASE); d25 <- filter(d, annee == "2025")
  di <- filter(d, anciennete >= ANCIENNETE_MIN)
  part <- function(x) prop.table(tapply(x$poids, x$statut3, sum))
  w_statut <- d25$poids * unname((part(db) / part(d25))[as.character(d25$statut3)])
  cf_s <- d25 %>% mutate(surface = transposer(surface, poids, db$surface, db$poids),
                         surface_10 = surface / 10)
  cf_p <- d25 %>% mutate(cout_m2_reel = transposer(cout_m2_reel, poids, db$cout_m2_reel, db$poids))
  cf_sp <- cf_s %>% mutate(cout_m2_reel = cf_p$cout_m2_reel)
  imap_dfr(DECOUPAGES, function(s, lib) {
    dt <- droplevels(tranches(di, s))
    m <- suppressWarnings(glm(elaguer(formule(s), dt), data = dt, weights = poids / mean(poids),
                              family = quasibinomial(link = "probit")))
    p0 <- predire(m, tranches(d25, s))
    tibble(specification = lib,
           canal = c("Surface", "Prix au m2", "Statuts d'occupation",
                     "Surface + prix (borne haute, sans les statuts)", "Logement complet"),
           ecart = c(predire(m, tranches(cf_s, s)), predire(m, tranches(cf_p, s)),
                     predire(m, tranches(d25, s), w_statut), predire(m, tranches(cf_sp, s)),
                     predire(m, tranches(cf_sp, s), w_statut)) / p0 - 1) %>%
      mutate(part_de_la_baisse_pct = 100 * ecart / -BAISSE)
  })
}

centrale <- decomposer(df)
cat("Valeurs centrales (", ANNEE_BASE, "-2025) :\n"); print(as.data.frame(centrale))

# Bootstrap : grappes tirées avec remise dans chaque vague, tirages répartis
# sur les cœurs de la machine.
grappes <- df %>% distinct(annee, id_cluster)
tirer <- function(b) {
  g <- grappes %>% group_by(annee) %>% slice_sample(prop = 1, replace = TRUE) %>% ungroup()
  d <- g %>% inner_join(df, by = c("annee", "id_cluster"), relationship = "many-to-many")
  r <- tryCatch(decomposer(d), error = function(e) NULL)
  if (is.null(r)) NULL else mutate(r, tirage = b)
}
cl <- parallel::makeCluster(max(1, parallel::detectCores() - 1))
parallel::clusterEvalQ(cl, suppressPackageStartupMessages(library(tidyverse)))
parallel::clusterExport(cl, c("grappes", "df", "decomposer", "DECOUPAGES", "tranches", "formule",
                              "elaguer", "predire", "transposer", "rang_pondere", "wquantile",
                              "ANNEE_BASE", "BAISSE", "ANCIENNETE_MIN", "F_POOLE"))
parallel::clusterSetRNGStream(cl, 2026)
tirages <- bind_rows(parallel::parLapply(cl, seq_len(B), tirer))
parallel::stopCluster(cl)
ic <- tirages %>% group_by(specification, canal) %>%
  summarise(ic_bas = quantile(part_de_la_baisse_pct, .025),
            ic_haut = quantile(part_de_la_baisse_pct, .975),
            tirages = n(), .groups = "drop")
res <- centrale %>% select(-ecart) %>% left_join(ic, by = c("specification", "canal")) %>%
  mutate(across(where(is.double), ~ round(.x, 1)))
cat("\nPart de la baisse de l'ICF (%), valeur centrale et intervalle à 95 % :\n")
print(as.data.frame(res), row.names = FALSE)
ecrire(res, "Output/decomposition_bootstrap.csv")
