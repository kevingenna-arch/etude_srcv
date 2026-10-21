# ============================================================================
#  SRCV — 40. Graphique : recul du gain d'un enfant supplémentaire entre le
#             barème de 2014 et celui de 2025, par quartile de niveau de vie
# ----------------------------------------------------------------------------
#  Une barre par quartile, découpée par instrument (PAJE, allocations
#  familiales et complément familial, quotient familial) ; sous chaque barre,
#  l'écart total et l'effet estimé sur la natalité (modèle du script 38, non
#  significatif : ordre de grandeur).
#  Entrée : Output/politique_familiale_par_quartile_2014.csv (script 38 lancé
#  avec SRCV_DEBUT=2014).
#  Sortie : Output/graph_pf_quartiles.png (+ copie pour la synthèse Word)
# ============================================================================

source("R/00_utils.R")
library(ggplot2)

q <- read_csv("Output/politique_familiale_par_quartile_2014.csv", show_col_types = FALSE) %>%
  mutate(quartile = factor(q, c("Q1", "Q2", "Q3", "Q4"),
                           c("Q1\n(les plus modestes)", "Q2", "Q3", "Q4\n(les plus aisés)")))

INSTRUMENTS <- c("PAJE (allocation de base et prime)" = "ecart_paje",
                 "Allocations familiales et complément familial" = "ecart_af_cf",
                 "Quotient familial" = "ecart_qf")
long <- q %>% select(quartile, all_of(INSTRUMENTS)) %>%
  pivot_longer(-quartile, names_to = "instrument", values_to = "ecart") %>%
  mutate(instrument = factor(instrument, names(INSTRUMENTS)))

euros <- function(x) paste0(gsub("-", "−", format(round(x), big.mark = " ", trim = TRUE)), " €")
etiq <- q %>% mutate(
  bas = pmin(0, ecart_paje) + pmin(0, ecart_af_cf) + pmin(0, ecart_qf),
  total = paste0(euros(ecart), " (", gsub("-", "−", round(ecart_pct)), " %)"),
  natalite = paste0("natalité : −", format(round(natalite_supplementaire_pct, 1),
                                           decimal.mark = ","), " %"))

COULEURS <- c("PAJE (allocation de base et prime)" = "#2a78d6",
              "Allocations familiales et complément familial" = "#eb6834",
              "Quotient familial" = "#1baf7a")

g <- ggplot(long, aes(quartile, ecart, fill = instrument)) +
  geom_col(width = 0.6, colour = "white", linewidth = 0.8) +
  geom_hline(yintercept = 0, colour = "#52514e", linewidth = 0.4) +
  geom_text(data = etiq, aes(quartile, bas - 110, label = total), inherit.aes = FALSE,
            size = 3.9, fontface = "bold", colour = "#0b0b0b") +
  geom_text(data = etiq, aes(quartile, bas - 300, label = natalite), inherit.aes = FALSE,
            size = 3.4, colour = "#52514e") +
  scale_fill_manual(values = COULEURS, name = NULL) +
  scale_y_continuous(labels = euros, breaks = seq(-2000, 500, 500),
                     expand = expansion(mult = c(0.12, 0.05))) +
  labs(x = "Quartile de niveau de vie", y = "Écart de gain par an (€ de 2025)") +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE)) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "top", legend.text = element_text(size = 10),
        panel.grid.major.x = element_blank(), panel.grid.minor = element_blank(),
        panel.grid.major.y = element_line(colour = "#e6e6e3"),
        axis.text = element_text(colour = "#52514e"),
        axis.title = element_text(colour = "#52514e", size = 10.5))

for (f in c("Output/graph_pf_quartiles.png", "pdf/synthese/graph_pf_quartiles.png"))
  ggsave(f, g, width = 8, height = 4.6, dpi = 250, bg = "white")
cat("Exporte : Output/graph_pf_quartiles.png\n")
