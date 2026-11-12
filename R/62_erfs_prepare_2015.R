# ============================================================================
#  ERFS — 62. La réforme du congé parental de 2015 (PreParE)
# ----------------------------------------------------------------------------
#  Pour les enfants nés depuis le 1er janvier 2015, la prestation partagée
#  d'éducation de l'enfant (PreParE) remplace le complément de libre choix
#  d'activité (CLCA). À partir de deux enfants, chaque parent peut être
#  indemnisé 24 mois au plus, dans la limite des 3 ans de l'enfant : une mère
#  seule à prendre le congé perd la troisième année, sauf si le père en prend
#  une. Pour un premier enfant, 6 mois par parent au lieu de 6 mois en tout :
#  rien ne change pour la mère.
#  Double différence : familles de deux enfants ou plus (le benjamin compris)
#  contre familles d'un enfant, benjamin né depuis 2015 contre avant.
#  1. Premier étage : quand le benjamin a 12-23, 24-35 ou 36-47 mois, la mère
#     est-elle en congé parental, en retrait (congé parental ou inactive), à
#     temps partiel pour ses enfants ? Le père prend-il le congé ? Couples sans
#     naissance dans l'année.
#  2. Naissance suivante : probabilité d'une naissance dans l'année, selon
#     l'âge du benjamin né avant elle.
#  Effets fixes communs aux deux groupes (année d'enquête, âge du benjamin en
#  mois), groupe, âge de la femme, diplômes ; des effets propres à chaque
#  groupe rendraient la cohorte du benjamin quasi colinéaire à l'année et à
#  l'âge. Gain d'un enfant de plus et gain au barème de 2014 (script 57) en
#  contrôle, pour la modulation des allocations familiales de 2015, qui touche
#  aussi les familles de deux enfants ou plus ; à 36-47 mois, où le congé était
#  fini dans les deux régimes, l'effet sur la mère sert de placebo. ERFS
#  2008-2023 : le congé parental est repéré de façon homogène depuis 2008.
#  Étude d'événement par année d'enquête, référence 2016 : un benjamin de
#  24-35 mois est né après la réforme à partir de l'enquête de 2017.
#  Sorties : Output/erfs_prepare_2015.csv, Output/graph_erfs_prepare_2015.png
# ============================================================================

source("R/57_erfs_preparation.R")   # df : couples de l'ERFS, contrôles, gains

REFORME <- 2015 * 12 + 1             # janvier 2015, en mois (année x 12 + mois)
TRANCHES <- c("12-23 mois" = 12, "24-35 mois" = 24, "36-47 mois" = 36)
FE <- "annee + age_b + deux + age_f + dipl_femme + dipl_homme"

d <- df %>%
  filter(annee >= 2008, nb_enf_avant >= 1, !is.na(benj_am), !is.na(datref)) %>%
  mutate(age_b = datref - benj_am, cohorte = (benj_am - 1) %/% 12,
         post = benj_am >= REFORME, deux = nb_enf_avant >= 2,
         post_n = as.integer(post), traite = as.integer(post & deux),
         tranche = cut(age_b, c(12, 24, 36, 48), right = FALSE, labels = names(TRANCHES)),
         retrait = cpar_femme | emploi_femme == "Inactivite") %>%
  filter(!is.na(tranche), cohorte >= 2005, cohorte <= 2021)
cat("Couples dont le benjamin a 12 à 47 mois :", nrow(d), "| naissances :", sum(d$naiss01), "\n")

# ── §1 Descriptif : part des mères en congé parental, par cohorte du benjamin
descr <- d %>% filter(naiss01 == 0) %>%
  group_by(groupe = if_else(deux, "Deux enfants ou plus", "Un enfant"), tranche,
           periode = if_else(post, "Benjamin ne depuis 2015", "Benjamin ne avant 2015")) %>%
  summarise(couples = n(), meres_conge_parental_pct = 100 * pond(cpar_femme, poids),
            meres_retrait_pct = 100 * pond(retrait, poids), meres_tp_enfants_pct = 100 * pond(tp_fam_femme, poids),
            peres_conge_parental_pct = 100 * pond(cpar_homme, poids), .groups = "drop")
cat("\nSituation de la mère et du père, couples sans naissance dans l'année :\n")
print(as.data.frame(descr %>% mutate(across(where(is.double), ~ round(.x, 1)))), row.names = FALSE)

# ── §2 Double différence ───────────────────────────────────────────────────
dd <- function(dt, y) {
  m <- feols(as.formula(paste(y, "~ post_n + traite + gain_1k + gain_ref_1k |", FE)),
             data = dt, weights = ~poids, vcov = "hetero")
  tibble(effet_pts = 100 * coef(m)[["traite"]], se = 100 * se(m)[["traite"]], p = pvalue(m)[["traite"]], n = nobs(m),
         moyenne_pct = 100 * pond(dt[[y]][dt$deux & !dt$post], dt$poids[dt$deux & !dt$post]))
}
ISSUES_MERE <- c("Mere en conge parental" = "cpar_femme", "Mere en retrait (conge parental ou inactive)" = "retrait",
                 "Mere a temps partiel pour ses enfants" = "tp_fam_femme", "Pere en conge parental" = "cpar_homme")
premier_etage <- imap_dfr(ISSUES_MERE, function(y, nom) imap_dfr(TRANCHES, function(t0, tr)
  dd(filter(d, naiss01 == 0, tranche == tr), y) %>% mutate(issue = nom, tranche = tr, .before = 1)))
naissances <- bind_rows(
  imap_dfr(TRANCHES, function(t0, tr) dd(filter(d, tranche == tr), "naiss01") %>% mutate(tranche = tr)),
  dd(d, "naiss01") %>% mutate(tranche = "12-47 mois")) %>%
  mutate(issue = "Naissance dans l'annee", .before = 1)
cat("\nDouble différence (deux enfants ou plus × benjamin né depuis 2015), points :\n")
print(as.data.frame(bind_rows(premier_etage, naissances) %>% mutate(across(where(is.double), ~ round(.x, 3)))),
      row.names = FALSE)

# ── §3 Étude d'événement par année d'enquête ───────────────────────────────
evenement <- function(dt, y) {
  m <- feols(as.formula(paste(y, "~ i(annee, deux, ref = 2016) + gain_1k + gain_ref_1k |", FE)),
             data = dt, weights = ~poids, vcov = "hetero")
  ct <- as.data.frame(coeftable(m)); ct$terme <- rownames(ct)
  ct <- ct %>% filter(grepl("^annee::", terme)) %>%
    transmute(terme, annee = as.integer(sub("annee::([0-9]+):.*", "\\1", terme)),
              coef_pts = 100 * Estimate, se = 100 * `Std. Error`)
  test <- function(ans) {
    v <- ct$terme[ct$annee %in% ans]
    wald(m, keep = paste0("^(", paste(gsub("([:.])", "\\\\\\1", v), collapse = "|"), ")$"), print = FALSE)$p
  }
  ct %>% select(-terme) %>% mutate(p_joint_avant = test(2008:2014), p_joint_apres = test(2017:2023))
}
evenements <- bind_rows(
  evenement(filter(d, naiss01 == 0, tranche == "24-35 mois"), "retrait") %>%
    mutate(issue = "Mere en retrait, benjamin de 24-35 mois"),
  evenement(d, "naiss01") %>% mutate(issue = "Naissance dans l'annee, benjamin de 12-47 mois"))
cat("\nÉtude d'événement (deux enfants ou plus contre un enfant, par année d'enquête ; référence 2016) :\n")
print(as.data.frame(evenements %>% mutate(across(where(is.double), ~ round(.x, 3)))), row.names = FALSE)

# ── §4 Graphique ───────────────────────────────────────────────────────────
g <- ggplot(evenements, aes(annee, coef_pts)) +
  annotate("rect", xmin = 2016.5, xmax = 2023.5, ymin = -Inf, ymax = Inf, fill = "#efeeec") +
  geom_hline(yintercept = 0, colour = "#52514e", linewidth = 0.4) +
  geom_linerange(aes(ymin = coef_pts - 1.96 * se, ymax = coef_pts + 1.96 * se), colour = "#2a78d6", linewidth = 0.6) +
  geom_point(colour = "#2a78d6", size = 2.2) +
  facet_wrap(~ issue, nrow = 1, scales = "free_y", labeller = labeller(issue = c(
    "Mere en retrait, benjamin de 24-35 mois" = "Mère en retrait, benjamin de 24-35 mois",
    "Naissance dans l'annee, benjamin de 12-47 mois" = "Naissance dans l'année, benjamin de 12-47 mois"))) +
  scale_x_continuous(breaks = seq(2008, 2023, 3)) +
  labs(x = "Année d'enquête", y = "Écart des familles de deux enfants ou plus\n(points, référence 2016)") +
  theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(), panel.spacing = unit(1.4, "lines"),
        panel.grid.major.y = element_line(colour = "#e6e5e2", linewidth = 0.3),
        strip.text = element_text(colour = "#0b0b0b", size = 10.5, face = "bold"),
        axis.text = element_text(colour = "#52514e"), axis.title = element_text(colour = "#52514e", size = 10))
ggsave("Output/graph_erfs_prepare_2015.png", g, width = 9, height = 3.6, dpi = 250, bg = "white")
cat("\nGraphique écrit : Output/graph_erfs_prepare_2015.png\n")

# ── Sortie ─────────────────────────────────────────────────────────────────
bind_rows(descr %>% mutate(bloc = "Descriptif"),
          premier_etage %>% mutate(bloc = "Premier etage"),
          naissances %>% mutate(bloc = "Naissances"),
          evenements %>% mutate(bloc = "Etude d'evenement")) %>%
  relocate(bloc) %>%
  write_csv("Output/erfs_prepare_2015.csv")
cat("Résultats écrits : Output/erfs_prepare_2015.csv\n")
