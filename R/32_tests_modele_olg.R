# ============================================================================
#  SRCV — Tests des prédictions du modèle à générations imbriquées
#  (pdf/modele_olg/modele_olg.tex)
# ----------------------------------------------------------------------------
#  1. Le revenu n'agit qu'à travers la surface : effet du niveau de vie sans
#     puis avec la surface (variante E, et mesure Insee pour comparaison).
#  2. Sensibilité à la surface selon le revenu : effet de +10 m² par tercile
#     de niveau de vie (le modèle prédit des ménages modestes plus sensibles).
#  3. Deux régions : surfaces, coût au m², niveau de vie et taux de naissance
#     par tranche d'unité urbaine ; rapports observés et prédits
#     (κ = x_u/x_r = (w_u/w_r)^(-1/γ), p_u/p_r = (w_u/w_r)^((1+γ)/γ), γ = 0,43).
#  4. À surface donnée, la ville n'a pas d'effet : effet de la tranche d'unité
#     urbaine sans puis avec la surface.
#  Période : SRCV_DEBUT (00_config.R). Sortie : Output/tests_modele_olg[_2014].csv
# ============================================================================

source("R/00_prepa_fecondite.R")
library(survey); library(marginaleffects)
activer_cache_prepa()
IPC <- charger_ipc()
GAMMA <- 0.3 / 0.7

df <- bind_rows(imap(millesimes_prepares(restreindre = TRUE),
                     ~ .x %>% select(any_of(c(VARS_INTERET, "datent"))))) %>%
  mutate(annee_num = as.integer(annee), naiss01 = as.integer(naissance == "Oui"),
         cout_m2_reel = deflater(cout_m2, annee_num, IPC, base = 2025),
         nv_e     = deflater(niveau_vie,       annee_num - 1, IPC, base = 2025),
         nv_insee = deflater(niveau_vie_insee, annee_num - 1, IPC, base = 2025),
         surface_10 = surface / 10, nv_10k = nv_e / 1e4, nv_insee_10k = nv_insee / 1e4,
         rang = factor(if_else(nb_enf_avant == 0, "1er enfant", "Rang 2+")),
         across(where(is.factor), ~ factor(as.character(.))),
         statut3 = factor(statut3, levels = c("Locataire", "Proprietaire_accedant",
                                              "Proprietaire_non_accedant"))) %>%
  filter(!is.na(surface), !is.na(cout_m2_reel), is.finite(cout_m2_reel), !is.na(statut3),
         !is.na(age_femme), !is.na(nv_e), !is.na(poids), poids > 0)
cat("Vagues :", paste(sort(unique(df$annee)), collapse = ", "), "|", nrow(df), "obs,",
    sum(df$naiss01), "naissances\n")

des <- function(d) svydesign(ids = ~id_cluster, weights = ~poids, data = droplevels(d))
elaguer <- function(f, d) {
  vars <- setdiff(all.vars(f), all.vars(f[[2]]))
  mono <- vars[vapply(vars, function(v) v %in% names(d) && is.factor(d[[v]]) &&
                        nlevels(droplevels(d[[v]])) < 2, logical(1))]
  if (length(mono)) f <- update(f, paste(". ~ . -", paste(mono, collapse = " - ")))
  f
}
probit <- function(f, d) { d <- droplevels(d)
  svyglm(elaguer(f, d), design = des(d), family = quasibinomial(link = "probit")) }
ame <- function(m, v, ...) as_tibble(avg_slopes(m, variables = v, wts = m$prior.weights, ...)) %>%
  mutate(across(any_of(c("estimate", "std.error", "conf.low", "conf.high")), ~ 100 * .x))
CTRL <- "statut3 + age_c + I(age_c^2) + demenage + csp + urbain + zeat + factor(annee)"
spec <- function(rhs, rang1) as.formula(paste("naiss01 ~", rhs, "+", CTRL,
                                              if (!rang1) "+ nb_enfants + taille_avant" else ""))
champs <- list("Ensemble" = function(d) d, "1er enfant" = function(d) filter(d, rang == "1er enfant"))
res <- list()

# ── 1. Revenu, sans puis avec la surface ────────────────────────────────────
for (ch in names(champs)) {
  d <- champs[[ch]](df); r1 <- ch == "1er enfant"
  for (mes in c("nv_10k", "nv_insee_10k")) for (s in c(FALSE, TRUE)) {
    m <- probit(spec(paste(mes, "+ cout_m2_reel", if (s) "+ surface_10" else ""), r1), d)
    res[[length(res) + 1]] <- ame(m, c(mes, if (s) "surface_10")) %>%
      transmute(test = "1. Revenu et surface", champ = ch,
                specification = paste(if (mes == "nv_10k") "Niveau de vie E" else "Niveau de vie Insee",
                                      if (s) "avec surface" else "sans surface"),
                terme = term, estimate, std.error, p.value)
  }
}

# ── 2. Effet de la surface par tercile de niveau de vie ─────────────────────
df <- df %>% group_by(annee) %>%
  mutate(tercile = factor(cut(nv_e, c(-Inf, wquantile(nv_e, poids, c(1/3, 2/3)), Inf),
                              labels = c("T1 modeste", "T2", "T3 aise")))) %>% ungroup()
for (ch in names(champs)) {
  d <- champs[[ch]](df)
  m <- probit(spec("surface_10 * tercile + cout_m2_reel", ch == "1er enfant"), d)
  w <- regTermTest(m, ~ surface_10:tercile)$p[1]
  res[[length(res) + 1]] <- ame(m, "surface_10", by = "tercile") %>%
    transmute(test = "2. Surface par tercile", champ = ch, specification = as.character(tercile),
              terme = "surface_10", estimate, std.error, p.value, wald_p = w)
}

# ── 3. Deux régions : descriptif par tranche d'unité urbaine ────────────────
dd <- df %>% filter(!is.na(densite)) %>% mutate(densite = factor(densite, levels = DENSITE_LIB))
desc <- dd %>% filter(!annee %in% c("2022", "2023")) %>% group_by(densite) %>%
  summarise(n = n(), nv_e = weighted.mean(nv_e, poids) / 1e3,
            nv_insee = weighted.mean(nv_insee, poids) / 1e3,
            surface = weighted.mean(surface, poids), cout_m2 = weighted.mean(cout_m2_reel, poids),
            taux_tous = 100 * weighted.mean(naiss01, poids),
            taux_rang1 = 100 * weighted.mean(naiss01[rang == "1er enfant"], poids[rang == "1er enfant"]),
            .groups = "drop")
r <- filter(desc, densite == "Rural")
desc <- desc %>% mutate(ratio_revenu = nv_e / r$nv_e, ratio_surface = surface / r$surface,
                        ratio_prix = cout_m2 / r$cout_m2,
                        surface_predite = ratio_revenu^(-1 / GAMMA),
                        prix_predit = ratio_revenu^((1 + GAMMA) / GAMMA))
cat("\nDescriptif par tranche d'unité urbaine (hors 2022-2023) :\n")
print(as.data.frame(desc %>% mutate(across(where(is.numeric), ~ round(.x, 2)))))
res[[length(res) + 1]] <- desc %>%
  tidyr::pivot_longer(-densite, names_to = "terme", values_to = "estimate") %>%
  transmute(test = "3. Deux regions (descriptif)", champ = "Ensemble",
            specification = as.character(densite), terme, estimate)

# ── 4. Effet de la ville, sans puis avec la surface ─────────────────────────
for (ch in names(champs)) {
  d <- champs[[ch]](dd)
  for (s in c(FALSE, TRUE)) {
    m <- probit(update(spec(paste("densite + nv_10k", if (s) "+ surface_10 + cout_m2_reel" else ""),
                            ch == "1er enfant"), . ~ . - urbain), d)
    w <- regTermTest(m, ~ densite)$p[1]
    res[[length(res) + 1]] <- ame(m, "densite") %>%
      transmute(test = "4. Effet ville", champ = ch,
                specification = if (s) "avec surface" else "sans surface",
                terme = contrast, estimate, std.error, p.value, wald_p = w)
  }
}

sortie <- bind_rows(res) %>% mutate(across(where(is.numeric), ~ round(.x, 4)))
print(as.data.frame(sortie %>% filter(test != "3. Deux regions (descriptif)")), row.names = FALSE)
ecrire(sortie, "Output/tests_modele_olg.csv")
cat("\nExporté : Output/tests_modele_olg", SUFFIXE_PERIODE, ".csv\n", sep = "")
