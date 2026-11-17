# ============================================================================
#  69. Graphique de synthèse : part de la baisse des naissances par femme
#      2014-2023 (recensement, −15 %) associée à chaque facteur
# ----------------------------------------------------------------------------
#  La décomposition de référence (script 68) en une barre : vie en couple,
#  politique familiale (barème, ERFS), naissances hors couple et non expliqué
#  à droite de zéro ; le logement, négatif, à gauche. Remplace, comme figure 1
#  de la synthèse Word, la barre SRCV 2014-2025 du script 39.
#  Entrée : Output/decomposition_recensement.csv (script 68)
#  Sorties : Output/graph_parts_baisse_recensement.png (+ copie pour la synthèse Word)
# ============================================================================

source("R/00_utils.R")
source("R/00_config.R")
library(ggplot2)

syn <- read_csv("Output/decomposition_recensement.csv", show_col_types = FALSE) %>% filter(bloc == "Synthese")
val <- function(f) syn$part_pct[trimws(syn$facteur) == f]
bornes <- function(f) unlist(syn[trimws(syn$facteur) == f, c("bas", "haut")])
pf <- bornes("Politique familiale (bareme)"); lo <- bornes("Logement (statut x surface x place)")
acc <- val("Offre d'accueil")
num <- function(x) gsub("-", "−", round(x))
parts <- tibble(
  facteur = c("Recul de la vie en couple", "Logement", "Politique familiale (barème)",
              "Naissances hors couple", "Non expliqué"),
  part = c(val("Recul de la vie en couple"), val("Logement (statut x surface x place)"),
           val("Politique familiale (bareme)"), val("Naissances hors couple"), val("Non explique")),
  fourchette = c(sprintf("dont tension : %s à %s %%", gsub("\\.", ",", round(bornes("dont tension immobiliere")[1], 1)),
                         gsub("\\.", ",", round(bornes("dont tension immobiliere")[2], 1))),
                 sprintf("%s à %s %%", num(lo[1]), num(lo[2])),
                 sprintf("%s à %s %%", num(pf[1]), num(pf[2])), "", ""))
print(parts)
stopifnot(abs(sum(parts$part) + acc - 100) < 0.5)
write_csv(parts, "Output/graph_parts_baisse_recensement.csv")

COULEURS <- c("Recul de la vie en couple" = "#2a78d6", "Logement" = "#eb6834",
              "Politique familiale (barème)" = "#8dd7bc", "Naissances hors couple" = "#9a9890",
              "Non expliqué" = "#d9d9d6")
ENCRE <- c("Recul de la vie en couple" = "#ffffff", "Logement" = "#0b0b0b", "Politique familiale (barème)" = "#0b0b0b",
           "Naissances hors couple" = "#ffffff", "Non expliqué" = "#0b0b0b")
# Positifs empilés depuis zéro vers la droite, dans l'ordre ; le logement, négatif, à gauche
pos <- parts %>% filter(part > 0) %>% mutate(fin = cumsum(part), debut = fin - part)
neg <- parts %>% filter(part <= 0) %>% mutate(debut = part, fin = 0)
p <- bind_rows(pos, neg) %>%
  mutate(milieu = (debut + fin) / 2, dedans = abs(part) >= 9, encre = ENCRE[facteur],
         etiquette = paste0(num(part), " %"), facteur = factor(facteur, names(COULEURS)))

g <- ggplot(p) +
  geom_rect(aes(xmin = debut, xmax = fin, ymin = 0.72, ymax = 1.28, fill = facteur), colour = "white", linewidth = 0.8) +
  annotate("segment", x = 0, xend = 0, y = 0.66, yend = 1.34, colour = "#52514e", linewidth = 0.4) +
  geom_text(data = filter(p, dedans), aes(x = milieu, y = 1, label = etiquette, colour = I(encre)),
            size = 5.2, fontface = "bold") +
  geom_text(data = filter(p, !dedans, part > 0), aes(x = milieu, y = 1.45, label = etiquette), size = 4.2, colour = "#0b0b0b") +
  # Logement, négatif : étiquette et fourchette à gauche de la barre
  geom_text(data = filter(p, part < 0), aes(x = debut - 1, y = 1.1, label = etiquette), hjust = 1, size = 4.2, colour = "#0b0b0b") +
  geom_text(data = filter(p, part < 0), aes(x = debut - 1, y = 0.9, label = fourchette), hjust = 1, size = 3.4, colour = "#52514e") +
  geom_text(data = filter(p, fourchette != "", part > 0), aes(x = milieu, y = 0.56, label = fourchette),
            size = 3.4, colour = "#52514e") +
  scale_fill_manual(values = COULEURS, breaks = names(COULEURS), name = NULL) +
  scale_x_continuous(breaks = seq(0, 100, 20), labels = function(x) paste0(x, " %"),
                     limits = c(-24, 106), expand = expansion(mult = c(0, 0.01))) +
  scale_y_continuous(limits = c(0.45, 1.55)) +
  labs(x = NULL, y = NULL,
       caption = paste0("Recensement : 1,77 enfant par femme en 2014, 1,50 en 2023 (−15 %). Offre d'accueil ≈ 0 ; âge des couples neutre.\n",
                        "Politique familiale : effet estimé dans l'ERFS 2014-2023 ; 20 % avec l'effet estimé sur 2005-2023.")) +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE)) +
  theme_minimal(base_size = 12) +
  theme(axis.text.y = element_blank(), panel.grid = element_blank(),
        axis.text.x = element_text(colour = "#52514e"),
        legend.position = "bottom", legend.text = element_text(size = 10.5),
        plot.caption = element_text(colour = "#52514e", size = 9.5, hjust = 0),
        plot.margin = margin(4, 16, 4, 16))

for (f in c("Output/graph_parts_baisse_recensement.png", "pdf/synthese/graph_parts_baisse_recensement.png"))
  ggsave(f, g, width = 8, height = 3.4, dpi = 250, bg = "white")
cat("Exporte : Output/graph_parts_baisse_recensement.{csv,png}\n")
