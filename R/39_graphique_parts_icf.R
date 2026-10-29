# ============================================================================
#  SRCV — 39. Graphique de synthèse : part de la baisse de l'ICF 2014-2025
#             (−22 %) associée à chacun des facteurs étudiés
# ----------------------------------------------------------------------------
#  Une barre de 100 % découpée par facteur ; le reste est « non expliqué ».
#  Valeurs centrales, lues dans les sorties des scripts (période 2014-2025) :
#   - Vie en couple : 34 (couple_naissances_individus.csv, composition à âge
#     donné et taux de 2014 ; 33 avec les taux de 2024-2025).
#   - Logement, canal des surfaces : seuil à 60 m², estimation centrale
#     (seuil_surface_decomposition_2014.csv) ; de 4 (linéaire) à 19 (seuil à
#     50 m²). La structure des statuts d'occupation a joué EN SENS INVERSE (−10) : elle
#     n'est pas dans la barre, la note du graphique le dit.
#   - Âge des couples : structure par âge de 2014, modèle retenu
#     (poids_relatif_canaux_2014.csv).
#   - Politique familiale : effet relatif estimé dans l'ERFS 2014-2023
#     (script 57), appliqué au recul du gain d'un enfant des couples SRCV de
#     2025 entre les barèmes 2014 et 2025 (politique_familiale_decomposition_
#     2014.csv, script 38). Négative ou nulle, elle sort de la barre, comme les
#     statuts d'occupation ; la légende du graphique le dit.
#  Les parts ne s'additionnent qu'approximativement (méthodes et champs
#  différents) : ordres de grandeur.
#  Sortie : Output/graph_parts_baisse_icf.png (+ copie pour la synthèse Word)
# ============================================================================

source("R/00_utils.R")
source("R/00_config.R")
library(ggplot2)

lire_part <- function(f, filtre) read_csv(f, show_col_types = FALSE) %>% filter({{ filtre }})

couple <- lire_part("Output/couple_naissances_individus.csv",
                    bloc == "4. Composition a taux fixes" & cle == "Groupe = vie en couple" &
                      grepl("taux 2014", indicateur) & grepl("ICF", indicateur))$valeur
seuil <- read_csv("Output/seuil_surface_decomposition_2014.csv", show_col_types = FALSE)
logement <- seuil$part_de_la_baisse_pct[seuil$bornes == "60 / 90 / 120" & seuil$canal == "Surface de 2014"]
log_bas  <- read_csv("Output/decomposition_natalite_2014.csv", show_col_types = FALSE) %>%
  filter(bloc == "Contrefactuel - Logement predetermine (retenu)", cle == "Surface de 2014") %>% pull(part)
log_haut <- seuil$part_de_la_baisse_pct[seuil$bornes == "50 / 70 / 90 / 120 (etude)" &
                                          seuil$canal == "Surface de 2014"]
age <- read_csv("Output/poids_relatif_canaux_2014.csv", show_col_types = FALSE) %>%
  filter(modele == "Logement predetermine (retenu)", grepl("Structure par age", canal)) %>%
  pull(part_de_la_baisse_pct)
srcv <- read_csv("Output/politique_familiale_decomposition_2014.csv", show_col_types = FALSE) %>%
  filter(bareme == 2014, echantillon == "Ensemble")
erfs <- effet_relatif_erfs("2014-2023", euros = 2025)
recul <- (srcv$gain_bareme - srcv$gain_2025) / 1000          # k€ de 2025 par an
baisse <- (ICF_2025 - ICF_2014) / ICF_2014
pf <- tibble(part = 100 * erfs$effet * recul / -baisse, bas = 100 * erfs$bas * recul / -baisse,
             haut = 100 * erfs$haut * recul / -baisse)
# Variante : effet estimé sur 2005-2023, plus précis (citée dans la légende)
erfs2 <- effet_relatif_erfs("2005-2023", euros = 2025)
pf2 <- tibble(part = 100 * erfs2$effet * recul / -baisse, bas = 100 * erfs2$bas * recul / -baisse,
              haut = 100 * erfs2$haut * recul / -baisse)
print(bind_rows(pf %>% mutate(effet = "2014-2023"), pf2 %>% mutate(effet = "2005-2023")))

parts <- tibble(
  facteur = c("Recul de la vie en couple", "Logement : petites surfaces",
              "Politique familiale", "Âge des couples"),
  part    = c(couple, logement, pf$part, age),
  fourchette = c("33 à 34 %", sprintf("%d à %d %%", round(log_bas), round(log_haut)),
                 gsub("-", "−", sprintf("%d à %d %%", round(pf$bas), round(pf$haut))), "2 à 4 %")) %>%
  mutate(valeur = pmax(part, 0))
parts <- bind_rows(parts, tibble(facteur = "Non expliqué", part = 100 - sum(parts$valeur),
                                 valeur = 100 - sum(parts$valeur), fourchette = ""))
print(parts)
signe <- function(x) gsub("-", "−", sprintf("%+d", round(x)))
hors_barre <- sprintf(paste("Hors de la barre : politique familiale %s %% (effet ERFS 2014-2023, intervalle %s ;",
                            "%s %% avec l'effet 2005-2023) ;\nstatuts d'occupation −10 %%."),
                      gsub("-", "−", round(pf$part)), parts$fourchette[parts$facteur == "Politique familiale"],
                      signe(pf2$part))
write_csv(parts, "Output/graph_parts_baisse_icf.csv")

# Palette catégorielle de référence (ordre fixe), gris pour le reste
COULEURS <- c("Recul de la vie en couple" = "#2a78d6", "Logement : petites surfaces" = "#eb6834",
              "Politique familiale" = "#8dd7bc", "Âge des couples" = "#eda100",
              "Non expliqué" = "#d9d9d6")
ENCRE <- c("#ffffff", "#0b0b0b", "#0b0b0b", "#0b0b0b", "#0b0b0b")

# Le premier facteur est empilé contre l'origine, à gauche.
p <- parts %>% mutate(milieu = cumsum(valeur) - valeur / 2, encre = ENCRE,
                      etiquette = paste0(round(valeur), " %"),
                      facteur = factor(facteur, rev(facteur)))
# Segments assez larges : étiquette dedans ; sinon au-dessus. La politique
# familiale, estimation non significative, est en teinte claire (dans la
# palette, pour que la légende corresponde).
p <- p %>% mutate(dedans = valeur >= 9, visible = valeur > 0)
if (pf$part > 0) hors_barre <- "Hors de la barre : statuts d'occupation −10 %."

g <- ggplot(p) +
  geom_col(aes(x = 1, y = valeur, fill = facteur), width = 0.55,
           colour = "white", linewidth = 0.8) +
  geom_text(data = filter(p, fourchette != "", visible), aes(x = 0.6, y = milieu, label = fourchette),
            size = 3.4, colour = "#52514e") +
  geom_text(data = filter(p, dedans), aes(x = 1, y = milieu, label = etiquette, colour = I(encre)),
            size = 5.2, fontface = "bold") +
  geom_text(data = filter(p, !dedans, visible), aes(x = 1.42, y = milieu, label = etiquette),
            size = 4.2, colour = "#0b0b0b") +
  geom_segment(data = filter(p, !dedans, visible), aes(x = 1.29, xend = 1.36, y = milieu, yend = milieu),
               colour = "#52514e", linewidth = 0.3) +
  coord_flip(clip = "off") +
  scale_fill_manual(values = COULEURS, breaks = names(COULEURS), name = NULL) +
  scale_y_continuous(breaks = seq(0, 100, 20), labels = function(x) paste0(x, " %"),
                     expand = expansion(mult = c(0, 0.01))) +
  scale_x_continuous(limits = c(0.5, 1.5)) +
  labs(x = NULL, y = "Part de la baisse de l'ICF entre 2014 et 2025 (−22 %)", caption = hors_barre) +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE)) +
  theme_minimal(base_size = 12) +
  theme(axis.text.y = element_blank(), panel.grid = element_blank(),
        axis.text.x = element_text(colour = "#52514e"),
        axis.title.x = element_text(colour = "#52514e", size = 10.5, margin = margin(t = 6)),
        legend.position = "bottom", legend.text = element_text(size = 10.5),
        plot.caption = element_text(colour = "#52514e", size = 9.5, hjust = 0),
        plot.margin = margin(4, 16, 4, 16))

for (f in c("Output/graph_parts_baisse_icf.png", "pdf/synthese/graph_parts_baisse_icf.png"))
  ggsave(f, g, width = 8, height = 3.7, dpi = 250, bg = "white")
cat("Exporte : Output/graph_parts_baisse_icf.{csv,png}\n")
