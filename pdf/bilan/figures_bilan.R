# ============================================================================
#  Bilan Word — figures : part de la baisse de l'ICF 2014-2025 par facteur,
#  par quartile de niveau de vie et par diplôme de la femme
# ----------------------------------------------------------------------------
#  Même graphique que le script 42, avec des étiquettes arrondies au plus
#  proche (14,5 -> 15), comme la page partagée et le diaporama ; round() de R
#  arrondit au pair (14,5 -> 14).
#  Usage (depuis la racine du projet) : Rscript pdf/bilan/figures_bilan.R
#  Entrées : Output/decomposition_icf_par_{quartile,diplome}.csv (script 41).
#  Sorties : pdf/bilan/graph_parts_icf_{quartiles,diplome}.png
# ============================================================================

source("R/00_utils.R")
library(ggplot2)

FACTEURS <- c("Recul de la vie en couple" = "vie_en_couple",
              "Logement : petites surfaces" = "logement",
              "Politique familiale" = "politique_familiale",
              "Âge des couples" = "age", "Non expliqué" = "non_explique")
COULEURS <- c("Recul de la vie en couple" = "#2a78d6", "Logement : petites surfaces" = "#eb6834",
              "Politique familiale" = "#8dd7bc", "Âge des couples" = "#eda100",
              "Non expliqué" = "#d9d9d6")
ENCRE <- c("Recul de la vie en couple" = "#ffffff", "Logement : petites surfaces" = "#0b0b0b",
           "Politique familiale" = "#0b0b0b", "Âge des couples" = "#0b0b0b",
           "Non expliqué" = "#0b0b0b")
arrondi <- function(x) sign(x) * floor(abs(x) + 0.5)

graphique <- function(entree, sortie, LIGNES) {
  long <- read_csv(entree, show_col_types = FALSE) %>% select(groupe, all_of(FACTEURS)) %>%
    tidyr::pivot_longer(-groupe, names_to = "facteur", values_to = "valeur") %>%
    mutate(facteur = factor(facteur, names(FACTEURS)),
           ligne = factor(LIGNES[groupe], rev(LIGNES))) %>%
    group_by(ligne) %>% arrange(facteur, .by_group = TRUE) %>%
    mutate(pos = pmax(valeur, 0), neg = pmin(valeur, 0),
           milieu = if_else(valeur >= 0, cumsum(pos) - pos / 2, cumsum(neg) - neg / 2)) %>%
    ungroup()

  g <- ggplot(long, aes(ligne, valeur, fill = facteur)) +
    geom_col(width = 0.62, colour = "white", linewidth = 0.8,
             position = position_stack(reverse = TRUE)) +
    geom_hline(yintercept = 0, colour = "#52514e", linewidth = 0.4) +
    geom_hline(yintercept = 100, colour = "#52514e", linewidth = 0.4, linetype = "dashed") +
    geom_text(data = filter(long, abs(valeur) >= 6),
              aes(y = milieu, label = paste0(gsub("-", "−", arrondi(valeur)), " %"),
                  colour = I(ENCRE[as.character(facteur)])),
              size = 3.7, fontface = "bold") +
    coord_flip(clip = "off") +
    scale_fill_manual(values = COULEURS, name = NULL) +
    scale_y_continuous(breaks = seq(-40, 160, 20),
                       labels = function(x) paste0(gsub("-", "−", x), " %"),
                       expand = expansion(mult = c(0.02, 0.02))) +
    labs(x = NULL, y = "Part de la baisse nationale de l'ICF entre 2014 et 2025 (−22 %)") +
    guides(fill = guide_legend(nrow = 2, byrow = TRUE)) +
    theme_minimal(base_size = 12) +
    theme(panel.grid = element_blank(), legend.position = "bottom",
          legend.text = element_text(size = 10.5),
          axis.text.y = element_text(colour = "#0b0b0b", size = 11),
          axis.text.x = element_text(colour = "#52514e"),
          axis.title.x = element_text(colour = "#52514e", size = 10.5, margin = margin(t = 6)),
          plot.margin = margin(4, 16, 4, 4))

  ggsave(file.path("pdf/bilan", sortie), g, width = 8.5, height = 4.4, dpi = 250, bg = "white")
  cat("Exporte :", sortie, "\n")
}

graphique("Output/decomposition_icf_par_quartile.csv", "graph_parts_icf_quartiles.png",
          c("Ensemble" = "Ensemble", "Q1" = "Q1 (les plus modestes)", "Q2" = "Q2",
            "Q3" = "Q3", "Q4" = "Q4 (les plus aisés)"))
graphique("Output/decomposition_icf_par_diplome.csv", "graph_parts_icf_diplome.png",
          c("Ensemble" = "Ensemble (25-49 ans)",
            "Supérieur (bac+3 ou plus)" = "Supérieur (bac+3 ou plus)", "Bac+2" = "Bac+2",
            "Bac" = "Bac", "CAP-BEP ou moins" = "CAP-BEP ou moins"))
