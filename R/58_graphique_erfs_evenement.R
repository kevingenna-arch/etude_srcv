# ============================================================================
#  ERFS — 58. Graphique de l'étude d'événement du script 57
# ----------------------------------------------------------------------------
#  Effet, année par année, de 1 000 € d'exposition aux réformes 2013-2018 sur
#  la probabilité de naissance, relativement à 2012, avec son intervalle de
#  confiance à 95 %. Un panneau par échantillon ; années des réformes grisées.
#  Entrée : Output/erfs_politique_familiale.csv (bloc « Etude d'evenement »)
#  Sortie : Output/graph_erfs_evenement.png
# ============================================================================

source("R/00_utils.R")
ECHANTILLONS <- c("Ensemble", "1er enfant", "Rang 2 et plus")
ev <- read_csv("Output/erfs_politique_familiale.csv", show_col_types = FALSE) %>%
  filter(bloc == "Etude d'evenement", exposition == "Reformes 2013-2018", specification == "Reference",
         echantillon %in% ECHANTILLONS) %>%
  mutate(echantillon = factor(echantillon, levels = ECHANTILLONS),
         bas = coef_pts - 1.96 * se, haut = coef_pts + 1.96 * se)

g <- ggplot(ev, aes(annee, coef_pts)) +
  annotate("rect", xmin = 2012.5, xmax = 2018.5, ymin = -Inf, ymax = Inf, fill = "#efeeec") +
  geom_hline(yintercept = 0, colour = "#52514e", linewidth = 0.4) +
  geom_linerange(aes(ymin = bas, ymax = haut), colour = "#2a78d6", linewidth = 0.6) +
  geom_point(colour = "#2a78d6", size = 2.2) +
  facet_wrap(~ echantillon, nrow = 1) +
  scale_x_continuous(breaks = seq(2005, 2023, 3)) +
  labs(x = NULL, y = "Effet de 1 000 € d'exposition\n(points de probabilité, réf. 2012)") +
  theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(),
        panel.spacing = unit(1.4, "lines"),
        panel.grid.major.y = element_line(colour = "#e6e5e2", linewidth = 0.3),
        strip.text = element_text(colour = "#0b0b0b", size = 11, face = "bold"),
        axis.text = element_text(colour = "#52514e"), axis.title.y = element_text(colour = "#52514e", size = 10))
ggsave("Output/graph_erfs_evenement.png", g, width = 9, height = 3.4, dpi = 250, bg = "white")
cat("Graphique écrit : Output/graph_erfs_evenement.png\n")
