# ============================================================================
#  SRCV — 29. COUPE vs PANEL : LE TEST DÉCISIF
# ----------------------------------------------------------------------------
#  L'effet de la surface sur la probabilité de naissance vaut +0,38 point pour
#  +10 m² en coupe empilée chez les ménages installés depuis trois ans (script
#  23), mais -0,007 point en panel (script 15 : logement en t, naissance entre
#  t et t+1). Le §6 du script 23 a établi que la PARAMÉTRISATION (prix au m²
#  constant ou budget total constant) n'y est pour rien. Restait à savoir si
#  l'écart vient du DESIGN (ordre temporel, échantillon chaîné) ou de la
#  SPÉCIFICATION (probit contre logit, prix au m² contre coût total, jeu de
#  contrôles). Ce test n'avait jamais été fait.
#
#  On prend l'ÉCHANTILLON DU PANEL (couples observés en t ET en t+1) et on y
#  estime EXACTEMENT la spécification de la coupe (probit, cout_m2, mêmes
#  contrôles, mêmes effets marginaux) :
#    (a) sur la naissance CONTEMPORAINE, en t   -> « la coupe sur l'échantillon du panel »
#    (b) sur la naissance SUIVANTE, t -> t+1     -> « le panel avec la spécification de la coupe »
#    (c) pour mémoire, la spécification du script 15 (coût total, diplôme,
#        actifs occupés) sur la naissance t -> t+1, en probit et en logit.
#  Si (a) retrouve l'effet de la coupe et (b) ne le retrouve pas, l'écart tient
#  au design ; si (a) ne le retrouve pas non plus, il tient à l'échantillon.
#
#  Entrée : Output/panel_naissance_logement.csv (script 06)
#  Sortie : Output/coupe_vs_panel.csv
# ============================================================================

source("R/00_prepa_fecondite.R")
library(survey); library(broom); library(marginaleffects)
activer_cache_prepa()

IPC <- charger_ipc()
if (!dir.exists("Output")) dir.create("Output")
ANCIENNETE_MIN <- 3

# Naissance en t+1 telle que construite par le script 06 : un nouvel individu,
# né en t ou t+1 et absent en t, apparaît dans le ménage en t+1.
# L'identifiant DOIT être lu en caractère : "05493400" perdrait son zéro.
panel <- read_csv("Output/panel_naissance_logement.csv", show_col_types = FALSE,
                  col_types = cols(id = col_character(), .default = col_guess())) %>%
  transmute(id = trimws(id), annee = as.character(annee_t),
            naiss_t1 = as.integer(naissance))
ANS_T <- as.character(sort(unique(as.integer(panel$annee))))    # 2022, 2023, 2024

DONNEES_FEC <- millesimes_prepares(restreindre = TRUE)

coupe <- bind_rows(imap(DONNEES_FEC[ANS_T],
                        ~ .x %>% select(any_of(c(VARS_INTERET, "datent"))))) %>%
  mutate(annee_num       = as.integer(annee),
         naiss01         = as.integer(naissance == "Oui"),
         cout_m2_reel    = deflater(cout_m2,    annee_num,     IPC, base = 2025),
         cout_log_reel   = deflater(cout_log,   annee_num,     IPC, base = 2025),
         revenu_reel     = deflater(revenu,     annee_num - 1, IPC, base = 2025),
         niveau_vie_reel = deflater(niveau_vie, annee_num - 1, IPC, base = 2025),
         surface_10 = surface / 10, nv_10k = niveau_vie_reel / 10000,
         cout_100   = cout_log_reel / 100, revenu_10k = revenu_reel / 10000,
         anciennete = annee_num - datent,
         # Rang et contrôles selon l'outcome : pré-naissance de t pour la
         # naissance en t ; parité OBSERVÉE en t pour la naissance en t+1.
         rang_t       = factor(if_else(nb_enf_avant == 0, "1er enfant", "Rang 2+")),
         rang_t1      = factor(if_else(nb_enf == 0,       "1er enfant", "Rang 2+")),
         nb_enfants_t = cut(nb_enf, breaks = c(-Inf, 0, 1, 2, Inf),
                            labels = c("0", "1", "2", "3+")),
         taille_t     = taille,
         across(where(is.factor), ~ factor(as.character(.))),
         statut3 = factor(statut3, levels = c("Locataire", "Proprietaire_accedant",
                                              "Proprietaire_non_accedant")),
         diplome = factor(diplome)) %>%
  filter(!is.na(surface), !is.na(cout_m2_reel), is.finite(cout_m2_reel),
         !is.na(statut3), !is.na(age_femme), !is.na(niveau_vie_reel),
         !is.na(poids), poids > 0)

pan <- coupe %>% inner_join(panel, by = c("annee", "id"))

cat("Coupe 2022-2024 (champ fecond) :", nrow(coupe), "obs,",
    sum(coupe$naiss01), "naissances en t\n")
cat("Echantillon du panel (observes en t et t+1) :", nrow(pan), "obs,",
    sum(pan$naiss01), "naissances en t,", sum(pan$naiss_t1), "naissances en t+1\n")
cat("Part de la coupe retrouvee en t+1 :", round(100 * nrow(pan) / nrow(coupe), 1), "%\n")

# ── Outils (identiques au script 23) ────────────────────────────────────────
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

# Spécification de la COUPE (script 23), outcome en t puis en t+1
F_COUPE_T  <- naiss01  ~ surface_10 + cout_m2_reel + nv_10k + statut3 +
  age_c + I(age_c^2) + demenage + csp + urbain + zeat + factor(annee) +
  nb_enfants + taille_avant
F_COUPE_T1 <- naiss_t1 ~ surface_10 + cout_m2_reel + nv_10k + statut3 +
  age_c + I(age_c^2) + demenage + csp + urbain + zeat + factor(annee) +
  nb_enfants_t + taille_t
F_COUPE_T_R1  <- update(F_COUPE_T,  . ~ . - nb_enfants - taille_avant)
F_COUPE_T1_R1 <- update(F_COUPE_T1, . ~ . - nb_enfants_t - taille_t)
# Spécification du script 15 (panel) : coût total, revenu, diplôme, actifs occupés
F_15_T1 <- naiss_t1 ~ surface_10 + cout_100 + nb_enfants_t + age_c + I(age_c^2) +
  statut3 + revenu_10k + diplome + csp + nactocc + zeat + factor(annee)

estimer <- function(d, f, design, lib, link = "probit") {
  d <- droplevels(d); y <- all.vars(f)[1]
  m <- svyglm(elaguer(f, d), design = des(d), family = quasibinomial(link = link))
  ame(m, "surface_10") %>% en_points() %>%
    transmute(design = design, echantillon = lib, outcome = y, lien = link,
              n = nrow(d), naissances = sum(d[[y]]),
              taux_base_pct = 100 * weighted.mean(d[[y]], d$poids),
              ame_pts = estimate, se = std.error, conf.low, conf.high, p.value)
}

inst <- function(d) filter(d, anciennete >= ANCIENNETE_MIN)

res <- bind_rows(
  # Référence : la coupe 2022-2024 complète (ce que l'étude publie, sur ces vagues)
  estimer(coupe,       F_COUPE_T,    "Coupe 2022-2024 complete, naissance en t", "Tous les couples"),
  estimer(inst(coupe), F_COUPE_T,    "Coupe 2022-2024 complete, naissance en t", "Installes 3 ans et plus"),
  estimer(inst(filter(coupe, rang_t == "1er enfant")), F_COUPE_T_R1,
          "Coupe 2022-2024 complete, naissance en t", "1er enfant, installes 3 ans et plus"),
  # (a) La même coupe, restreinte à l'échantillon du panel
  estimer(pan,         F_COUPE_T,    "(a) Echantillon du panel, naissance en t", "Tous les couples"),
  estimer(inst(pan),   F_COUPE_T,    "(a) Echantillon du panel, naissance en t", "Installes 3 ans et plus"),
  estimer(inst(filter(pan, rang_t == "1er enfant")), F_COUPE_T_R1,
          "(a) Echantillon du panel, naissance en t", "1er enfant, installes 3 ans et plus"),
  # (b) Le design du panel avec la spécification de la coupe
  estimer(pan,         F_COUPE_T1,   "(b) Echantillon du panel, naissance en t+1, specification de la coupe", "Tous les couples"),
  estimer(inst(pan),   F_COUPE_T1,   "(b) Echantillon du panel, naissance en t+1, specification de la coupe", "Installes 3 ans et plus"),
  estimer(inst(filter(pan, rang_t1 == "1er enfant")), F_COUPE_T1_R1,
          "(b) Echantillon du panel, naissance en t+1, specification de la coupe", "1er enfant, installes 3 ans et plus"),
  # (c) Pour mémoire : la spécification du script 15
  estimer(pan,         F_15_T1,      "(c) Echantillon du panel, naissance en t+1, specification du script 15", "Tous les couples"),
  estimer(inst(pan),   F_15_T1,      "(c) Echantillon du panel, naissance en t+1, specification du script 15", "Installes 3 ans et plus"),
  estimer(inst(pan),   F_15_T1,      "(c) Echantillon du panel, naissance en t+1, specification du script 15", "Installes 3 ans et plus", link = "logit"))

cat("\n=== Effet de la surface (+10 m2), points de % ===\n")
print(as.data.frame(res %>% mutate(across(where(is.numeric), ~ round(.x, 4)))), row.names = FALSE)

# ── Lecture automatique ─────────────────────────────────────────────────────
a <- res %>% filter(str_detect(design, "^\\(a\\)"), echantillon == "Installes 3 ans et plus")
b <- res %>% filter(str_detect(design, "^\\(b\\)"), echantillon == "Installes 3 ans et plus")
cat(sprintf("\nInstalles 3 ans et plus, echantillon du panel : naissance en t = %+.3f (p = %.3f) ; naissance en t+1 = %+.3f (p = %.3f)\n",
            a$ame_pts, a$p.value, b$ame_pts, b$p.value))
cat(if (a$p.value < 0.05 && b$p.value >= 0.05)
      "-> L'effet de la coupe est present sur l'echantillon du panel en t et disparait en t+1 : l'ecart tient au DESIGN (ordre temporel), pas a la specification ni a l'echantillon.\n"
    else if (a$p.value >= 0.05)
      "-> L'effet de la coupe disparait des qu'on se restreint a l'echantillon du panel, meme en t : l'ecart tient a l'ECHANTILLON (menages chaines), pas au design.\n"
    else
      "-> L'effet subsiste dans les deux designs sur cet echantillon : l'ecart avec le script 15 tient a la SPECIFICATION.\n")

write_csv(res, "Output/coupe_vs_panel.csv")
cat("\n=== 29 termine. ===\n")
