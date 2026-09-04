# ============================================================================
#  SRCV — Nuages de points des naissances : LOCATAIRES et ACCÉDANTS
# ----------------------------------------------------------------------------
#  Version NON PONDÉRÉE des heatmaps de R/20 : un point = un ménage de
#  l'échantillon ayant connu une naissance récente. Aucun poids de sondage
#  n'intervient, ni dans les points ni dans les médianes tracées.
#
#  Abscisse : âge moyen du ou des parents (fichier individus, enfants exclus).
#  Ordonnées : niveau de vie par UC (euros constants 2025), puis surface.
#
#  ⚠️ NON PONDÉRÉ = NON REPRÉSENTATIF. Le nuage décrit l'ÉCHANTILLON, pas la
#     population : les ménages sur-représentés par le plan de sondage y pèsent
#     autant que les autres. C'est le bon outil pour voir la forme et la
#     dispersion du nuage et pour repérer les points aberrants, pas pour
#     estimer un effectif. Les heatmaps de R/20, elles, sont pondérées.
#
#  ⚠️ Les courbes de niveau sont une densité 2D à noyau : elles aident à lire
#     où se concentre la masse quand les points se superposent. Elles ne sont
#     pas une estimation, seulement un guide de lecture.
#
#  Palette : slots 1 et 2 de la palette catégorielle de référence, dans l'ordre
#  fixe (bleu = locataires, orange = accédants). L'identité n'est jamais portée
#  par la seule couleur : chaque panneau est nommé par son bandeau.
#
#  Sorties : Output/nuage_naissances_<ordonnee>_<champ>.png  (4 fichiers)
#            Output/nuage_naissances.csv
# ============================================================================

source("R/00_prepa_fecondite.R")

ANNEE_SEULE <- "2024"
ANNEES_POOL <- c("2006", "2010", "2014", "2018", "2022", "2025")
AGE_BAS     <- 15
AGE_HAUT    <- 49

STATUTS_LIB <- c(Locataire = "Locataires", Proprietaire_accedant = "Propriétaires accédants")
# Palette catégorielle de référence, ordre fixe : slot 1 puis slot 2.
COULEURS <- c(Locataires = "#2a78d6", `Propriétaires accédants` = "#eb6834")

IPC <- charger_ipc()

charger <- function(ans) {
  ages <- imap_dfr(VAGUES[ans],
                   ~ age_moyen_menage(.x, .y, liens = LIENS_PARENTS) %>% mutate(annee = .y))
  bind_rows(imap(VAGUES[ans], ~ preparer_donnees(.y, .x, restreindre = FALSE))) %>%
    filter(statut3 %in% names(STATUTS_LIB), !is.na(age_femme),
           age_femme >= AGE_BAS, age_femme <= AGE_HAUT,
           naissance == "Oui", !is.na(poids), poids > 0) %>%
    mutate(id = normaliser_id(id)) %>%
    left_join(ages, by = c("annee", "id")) %>%
    filter(!is.na(age_moyen)) %>%
    mutate(nv = deflater(niveau_vie, as.integer(annee) - 1, IPC, base = 2025) / 1000,
           statut = factor(unname(STATUTS_LIB[as.character(statut3)]),
                           levels = unname(STATUTS_LIB)))
}

tracer <- function(d, var_y, lab_y, borne_y, titre, sous_titre, source_txt) {
  d <- d %>% filter(!is.na(.data[[var_y]]))
  hors <- sum(d[[var_y]] > borne_y)      # points au-delà de la borne d'affichage
  med <- d %>% group_by(statut) %>%
    summarise(x = median(age_moyen), y = median(.data[[var_y]]), n = n(), .groups = "drop")

  ggplot(d, aes(age_moyen, .data[[var_y]], colour = statut)) +
    # densité 2D en repère de lecture, sous les points
    geom_density_2d(bins = 6, linewidth = 0.35, alpha = 0.45) +
    geom_point(size = 1.5, alpha = 0.45, stroke = 0) +
    # médiane du nuage, marque plus grosse + étiquette directe (sélective)
    geom_point(data = med, aes(x, y), size = 4.6, shape = 21,
               fill = "white", stroke = 1.4) +
    # Le texte porte une encre neutre, jamais la couleur de la série : c'est la
    # marque à côté de lui qui porte l'identité.
    geom_text(data = med, aes(x, y, label = sprintf("médiane : %.0f ans, %.0f", x, y)),
              colour = "#1a1a18", vjust = -1.9, size = 3.3, fontface = "bold",
              show.legend = FALSE) +
    facet_wrap(~statut, nrow = 1) +
    scale_colour_manual(values = COULEURS, guide = "none") +  # le bandeau nomme la série
    coord_cartesian(xlim = c(18, 50), ylim = c(0, borne_y)) +
    labs(title = titre, subtitle = sous_titre,
         x = "Âge moyen du ou des parents (ans)", y = lab_y,
         caption = paste0("Insee, ", source_txt,
                          " Champ : ménages comportant une femme de ", AGE_BAS, " à ",
                          AGE_HAUT, " ans ayant connu une naissance récente.",
                          "\nUn point = un ménage de l'échantillon. NON PONDÉRÉ : décrit",
                          " l'échantillon, pas la population.",
                          if (hors > 0) paste0(" ", hors,
                            " point(s) au-delà de l'axe ne sont pas affichés.") else "",
                          "\nLes courbes sont une densité 2D, repère de lecture et non",
                          " estimation. Médianes non pondérées.")) +
    theme_minimal(base_size = 11) +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major = element_line(colour = "#ececea", linewidth = 0.3),
          strip.text = element_text(face = "bold", size = 11),
          plot.title = element_text(face = "bold"),
          plot.subtitle = element_text(colour = "#57534e", size = 9),
          plot.caption = element_text(size = 7.5, colour = "#78716c", hjust = 0))
}

if (!dir.exists("Output")) dir.create("Output")

ORDONNEES <- list(
  list(var = "nv", borne = 60, slug = "niveau_vie",
       axe = "Niveau de vie par UC (milliers d'euros constants 2025)"),
  list(var = "surface", borne = 200, slug = "surface",
       axe = "Surface du logement (m2)")
)
CHAMPS <- list(
  list(ans = ANNEE_SEULE, slug = ANNEE_SEULE, suffixe = ANNEE_SEULE,
       src = paste0("SRCV ", ANNEE_SEULE, ".")),
  list(ans = ANNEES_POOL, slug = "poole", suffixe = "6 vagues cumulées",
       src = paste0("SRCV ", paste(ANNEES_POOL, collapse = ", "),
                    " ; 2023 et 2024 exclus (anomalie de pondération)."))
)

tout <- list()
for (ch in CHAMPS) {
  d <- charger(ch$ans)
  cat("== ", ch$slug, " : ", nrow(d), " ménages avec naissance (",
      paste(paste0(levels(d$statut), " ", as.integer(table(d$statut))), collapse = " / "),
      ") ==\n", sep = "")
  tout[[length(tout) + 1]] <- d %>%
    transmute(champ = ch$slug, annee, statut, age_moyen, nv, surface)
  for (o in ORDONNEES) {
    f <- paste0("Output/nuage_naissances_", o$slug, "_", ch$slug, ".png")
    ggsave(f, tracer(d, o$var, o$axe, o$borne,
                     paste0("Naissances : nuage de points, ", ch$suffixe),
                     paste0(nrow(d), " ménages de l'échantillon — non pondéré"),
                     ch$src),
           width = 11, height = 6, dpi = 200, bg = "white")
    cat("    ", f, "\n", sep = "")
  }
}

write_csv(bind_rows(tout), "Output/nuage_naissances.csv")
cat("\nExporté : 4 PNG + Output/nuage_naissances.csv\n")
