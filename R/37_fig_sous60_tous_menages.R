# ============================================================================
#  SRCV — 37. Figure de la note de synthèse : part des ménages logés dans
#             moins de 60 m², par statut d'occupation et âge de la PR
# ----------------------------------------------------------------------------
#  Champ : ménages dont la personne de référence a 15 à 49 ans, en couple ou
#  non, avec ou sans enfant ; huit vagues. Moyennes pondérées (DB090 et
#  équivalents) ; point creux si la cellule compte moins de 30 ménages.
#  Surfaces hors [9 ; 400] m² et mensualités > MENSUALITE_MAX écartées par
#  charger_menages(), comme dans tout le projet.
#
#  La figure d'origine (24/09/2026) n'avait pas de script ; celui-ci la
#  reproduit, filtres d'octobre compris.
#  Sorties : Output/fig_sous60_tous_menages.{csv,png}, et une copie du PNG
#            dans pdf/latex_template_2026/ pour la note (note_logement_2026.tex).
# ============================================================================

source("R/00_utils.R")
source("R/00_config.R")
library(ggplot2)

N_MIN <- 30

d <- imap_dfr(VAGUES, function(cfg, annee) {
  charger_menages(annee, cfg) %>%
    mutate(classe = cut(age_pr, breaks = c(14, 29, 39, 49),
                        labels = c("15-29 ans", "30-39 ans", "40-49 ans"))) %>%
    filter(!is.na(classe), !is.na(statut3), !is.na(surface))
})

stats <- d %>%
  group_by(annee, statut3, classe) %>%
  summarise(n = n(), part_sous60 = 100 * weighted.mean(surface < 60, poids),
            .groups = "drop") %>%
  mutate(an = as.integer(annee),
         statut = factor(statut3, c("Locataire", "Proprietaire_accedant",
                                    "Proprietaire_non_accedant"),
                         c("Locataires", "Propriétaires accédants",
                           "Propriétaires non accédants")),
         petit_n = n < N_MIN)
write_csv(stats %>% select(annee, statut3, classe, n, part_sous60),
          "Output/fig_sous60_tous_menages.csv")
print(as.data.frame(stats %>% filter(statut3 == "Locataire") %>%
  select(annee, classe, n, part_sous60) %>% mutate(part_sous60 = round(part_sous60, 1))),
  row.names = FALSE)

COULEURS <- c("15-29 ans" = "#C0392B", "30-39 ans" = "#243443", "40-49 ans" = "#B9770E")

g <- ggplot(stats, aes(an, part_sous60, colour = classe)) +
  geom_line(linewidth = 0.9) +
  geom_point(data = filter(stats, !petit_n), size = 2) +
  geom_point(data = filter(stats, petit_n), size = 2, shape = 21, fill = "white",
             show.legend = FALSE) +
  facet_wrap(~ statut, nrow = 1) +
  scale_colour_manual(values = COULEURS, name = "Âge de la personne de référence") +
  scale_x_continuous(breaks = c(2006, 2010, 2014, 2018, 2022, 2025)) +
  scale_y_continuous(limits = c(0, NA), breaks = seq(0, 100, 10)) +
  labs(x = NULL, y = "Part (%)") +
  theme_minimal(base_size = 11) +
  theme(legend.position = "top", panel.grid.minor = element_blank(),
        strip.text = element_text(face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1))

for (f in c("Output/fig_sous60_tous_menages.png",
            "pdf/latex_template_2026/fig_sous60_tous_menages.png"))
  ggsave(f, g, width = 9, height = 3.6, dpi = 200, bg = "white")
cat("Exporte : Output/fig_sous60_tous_menages.{csv,png} et copie pour la note\n")
