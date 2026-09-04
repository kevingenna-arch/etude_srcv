# ============================================================================
#  SRCV — Graphiques : évolution 2006-2025 par quintile de niveau de vie
# ----------------------------------------------------------------------------
#  Entrée : Output/logement_par_quintile.csv   (objet "evo" produit par 12)
#  Sorties : Output/graph_quintiles.png            (les 6 panneaux en une figure)
#            + selon DECOUPAGE :
#              "statut"  -> graph_quintiles_locataires.png / _accedants.png
#                           (3 panneaux chacun, un par classe d'âge)
#              "cellule" -> les 6 panneaux en 6 fichiers séparés
#
#  Trois barres par quintile : surface, coût au m², taux d'effort.
#  ⚠️ UNITÉS DIFFÉRENTES sur un même axe : surface et coût/m² sont des variations
#     en %, le taux d'effort est un écart en POINTS. Les ordres de grandeur ne
#     sont pas comparables (le coût/m² monte à +41 %, l'effort tient dans
#     ±8 pts) : la barre "effort" paraît écrasée. Mettre FACETTE_INDICATEUR à
#     TRUE pour séparer les trois indicateurs avec une échelle libre.
# ============================================================================

suppressPackageStartupMessages(library(tidyverse))

FACETTE_INDICATEUR <- FALSE   # TRUE : une ligne par indicateur, échelle libre
N_MIN              <- 30      # bandes estompées sous cet effectif
DECOUPAGE          <- "statut"  # "statut"  : 2 fichiers de 3 panneaux (par classe d'âge)
                                # "cellule" : 6 fichiers d'un panneau chacun
PANNEAUX_EN_COLONNE <- TRUE   # découpage "statut" : 3 panneaux empilés (axe des
                              # quintiles aligné) plutôt que côte à côte

evo <- read_csv("Output/logement_par_quintile.csv", show_col_types = FALSE)

# ── Passage en format long : une ligne = un quintile × un indicateur ────────
evo_long <- evo %>%
  transmute(classe, statut3, quintile,
            fragile = pmin(n_2006, n_2025) < N_MIN,
            surf_pct = d_surface_pct,     # variation de surface, en %
            m2_pct   = d_cout_m2_pct,     # variation du coût au m², en %
            eff_pts  = d_effort_pts) %>%  # variation du taux d'effort, en points
  pivot_longer(c(surf_pct, m2_pct, eff_pts),
               names_to = "indicateur", values_to = "valeur") %>%
  mutate(
    indicateur = factor(indicateur,
                        levels = c("surf_pct", "m2_pct", "eff_pts"),
                        labels = c("Surface (%)", "Coût/m² (%)",
                                   "Taux d'effort (points)")),
    classe     = factor(classe, levels = unique(evo$classe)),
    statut3    = factor(statut3,
                        levels = c("Locataire", "Proprietaire_accedant"),
                        labels = c("Locataires", "Propriétaires accédants")))

# ── Le graphique ────────────────────────────────────────────────────────────
#  geom_col() et non geom_histogram()/geom_bar() : les valeurs sont déjà
#  calculées, il n'y a rien à compter ni à agréger.
#  facettes : "grille"  -> classe × statut (les 6 panneaux d'un coup)
#             "classe"  -> une facette par classe d'âge (un statut par figure)
#             "aucune"  -> un seul panneau
graphe <- function(d, titre = NULL, facettes = "grille") {
  p <- ggplot(d, aes(x = quintile, y = valeur, fill = indicateur,
                     alpha = !fragile)) +
    geom_col(position = position_dodge(width = 0.8), width = 0.72) +
    geom_hline(yintercept = 0, linewidth = 0.4, colour = "grey20") +
    scale_alpha_manual(values = c(`TRUE` = 1, `FALSE` = 0.35), guide = "none") +
    scale_fill_manual(values = c("#4C72B0", "#DD8452", "#55A868")) +
    labs(x = "Quintile de niveau de vie", y = "Évolution 2006 → 2025",
         fill = NULL, title = titre,
         caption = paste("SRCV 2006 et 2025. Coût au m² en euros constants 2025.",
                         "Barres estompées : moins de", N_MIN, "observations.")) +
    theme_minimal(base_size = 11) +
    theme(legend.position = "top",
          panel.grid.major.x = element_blank(),
          plot.caption = element_text(size = 7, colour = "grey40", hjust = 0))

  p + switch(facettes,
    grille = if (FACETTE_INDICATEUR) facet_grid(indicateur ~ classe + statut3,
                                                scales = "free_y")
             else                    facet_grid(classe ~ statut3),
    classe = if (FACETTE_INDICATEUR) facet_grid(indicateur ~ classe, scales = "free_y")
             else                    facet_wrap(~ classe,
                                                ncol = if (PANNEAUX_EN_COLONNE) 1 else 3),
    aucune = if (FACETTE_INDICATEUR) facet_wrap(~ indicateur, ncol = 1, scales = "free_y")
             else                    NULL)
}

if (!dir.exists("Output")) dir.create("Output")

# 1) Les 6 panneaux en une figure
ggsave("Output/graph_quintiles.png", graphe(evo_long),
       width = 10, height = 9, dpi = 200, bg = "white")
cat("Écrit : Output/graph_quintiles.png\n")

# 2) Un fichier par statut : 3 panneaux, un par classe d'âge
slug_statut <- function(x) if (grepl("^Loc", x)) "locataires" else "accedants"

if (DECOUPAGE == "statut") {
  evo_long %>%
    group_by(statut3) %>%
    group_walk(function(d, cle) {
      f <- paste0("Output/graph_quintiles_", slug_statut(cle$statut3), ".png")
      ggsave(f, graphe(d, titre = as.character(cle$statut3), facettes = "classe"),
             width  = if (PANNEAUX_EN_COLONNE) 7 else 12,
             height = if (PANNEAUX_EN_COLONNE) 9 else 4.5, dpi = 200, bg = "white")
      cat("Écrit :", f, "\n")
    }, .keep = TRUE)

} else {   # "cellule" : les 6 graphiques séparément
  evo_long %>%
    group_by(classe, statut3) %>%
    group_walk(function(d, cle) {
      f <- paste0("Output/graph_quintiles_", gsub("[^0-9]", "", cle$classe), "_",
                  slug_statut(cle$statut3), ".png")
      ggsave(f, graphe(d, titre = paste0(cle$classe, " — ", cle$statut3),
                       facettes = "aucune"),
             width = 6.5, height = 4.5, dpi = 200, bg = "white")
      cat("Écrit :", f, "\n")
    }, .keep = TRUE)
}
