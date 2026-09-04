# ============================================================================
#  SRCV — Heatmaps des naissances : LOCATAIRES et PROPRIÉTAIRES ACCÉDANTS
# ----------------------------------------------------------------------------
#  Abscisse commune : âge moyen du ou des parents (fichier individus, enfants
#  exclus), en classes de 2 ans entre 25 et 41, avec deux classes de bord.
#  Deux ordonnées :
#    1. niveau de vie par UC, classes régulières de 3 000 EUR constants 2025
#    2. surface du logement, classes régulières de 10 m2
#  Remplissage : nombre de naissances (estimation pondérée, en milliers).
#
#  ⚠️ GRILLE COMMUNE AUX DEUX STATUTS — les bornes hautes sont calées sur les
#     accédants (surface médiane 105 m2 et 9e décile 160, contre 75 et 103 chez
#     les locataires ; niveau de vie médian 28 k contre 19 k). Les cartes des
#     locataires comportent donc des rangées hautes vides : c'est le prix de la
#     comparabilité entre les deux statuts, qui partagent le même repère.
#
#  ⚠️ CLASSES RÉGULIÈRES ET CASES VIDES — les bornes sont à pas constant, ce qui
#     produit des cases vides aux extrémités. C'est voulu : une grille régulière
#     se lit comme une surface de densité, alors qu'une grille à classes
#     inégales déforme les aires. Conséquence : plus d'une centaine de cases,
#     donc les libellés par case sont supprimés au-delà de
#     MAX_CASES_ETIQUETEES et seule la couleur porte l'information.
#
#  ⚠️ EFFECTIFS — une vague seule ne porte qu'une centaine de naissances par
#     statut. Sur une grille fine cela fait environ une naissance par case : la
#     carte annuelle est une esquisse. Le cumul des six vagues saines (758
#     naissances chez les locataires, 880 chez les accédants) est la seule
#     version qui fasse apparaître une structure.
#
#  ⚠️ CHOIX DE 2024 : vague saine du point de vue de la pondération — rapport du
#     poids moyen naissance / non-naissance de 1,183 chez les locataires, dans
#     la norme historique (1,21 à 1,30 de 2006 à 2018). Les vagues déviantes
#     sont 2022 (0,665) et 2023 (0,634).
#
#  Palette : rampe séquentielle bleue à teinte unique, clair vers foncé, comme
#  l'impose l'encodage d'une magnitude.
#
#  Sorties : Output/heatmap_naissances_<ordonnee>_<statut>_<champ>.png (8 PNG)
#            Output/heatmap_naissances.csv
# ============================================================================

source("R/00_prepa_fecondite.R")

ANNEE_SEULE <- "2024"
ANNEES_POOL <- c("2006", "2010", "2014", "2018", "2022", "2025")
AGE_BAS     <- 15
AGE_HAUT    <- 49
MAX_CASES_ETIQUETEES <- 30      # au-delà, plus de chiffres dans les cases

STATUTS     <- c(Locataire = "locataires", Proprietaire_accedant = "accedants")
STATUTS_LIB <- c(Locataire = "locataires",
                 Proprietaire_accedant = "propriétaires accédants")

# ── Classes RÉGULIÈRES, communes aux deux statuts ───────────────────────────
# Âge des parents : pas de 2 ans entre 25 et 41, là où se concentrent les
# naissances, et deux classes de bord larges en dessous et au-dessus — les
# queues sont trop peu peuplées pour justifier un pas fin.
AGE_BRK <- c(-Inf, seq(25, 41, 2), Inf)
AGE_LAB <- c("< 25", paste0(seq(25, 39, 2), "-", seq(26, 40, 2)), "41 +")
# Niveau de vie : pas de 3 000 EUR de 0 à 48 000, plus une classe haute.
NV_BRK <- c(seq(0, 48000, 3000), Inf)
NV_LAB <- c(paste0(seq(0, 45, 3), "-", seq(3, 48, 3)), "48 +")
# Surface : pas de 10 m2 de 20 à 160, avec deux classes de bord.
SURF_BRK <- c(-Inf, seq(20, 160, 10), Inf)
SURF_LAB <- c("< 20", paste0(seq(20, 150, 10), "-", seq(29, 159, 10)), "160 +")

BLEU_CLAIR <- "#cde2fb"; BLEU_FONCE <- "#0d366b"; SURFACE <- "#ffffff"
IPC <- charger_ipc()

charger <- function(ans, statut) {
  ages <- imap_dfr(VAGUES[ans],
                   ~ age_moyen_menage(.x, .y, liens = LIENS_PARENTS) %>% mutate(annee = .y))
  bind_rows(imap(VAGUES[ans], ~ preparer_donnees(.y, .x, restreindre = FALSE))) %>%
    filter(statut3 == statut, !is.na(age_femme),
           age_femme >= AGE_BAS, age_femme <= AGE_HAUT,
           !is.na(naissance), !is.na(poids), poids > 0) %>%
    mutate(id = normaliser_id(id)) %>%
    left_join(ages, by = c("annee", "id")) %>%
    mutate(nv = deflater(niveau_vie, as.integer(annee) - 1, IPC, base = 2025)) %>%
    filter(!is.na(age_moyen)) %>%
    mutate(cl_age = cut(age_moyen, AGE_BRK, labels = AGE_LAB, right = FALSE))
}

# Agrège sur la grille age x <ordonnee>, cases vides conservées
agreger <- function(d, var_y, brk, lab, champ) {
  d %>%
    filter(!is.na(.data[[var_y]])) %>%
    mutate(cl_y = cut(.data[[var_y]], brk, labels = lab, right = FALSE)) %>%
    filter(!is.na(cl_y), naissance == "Oui") %>%
    group_by(cl_age, cl_y) %>%
    summarise(n_ech = n(), naiss_k = sum(poids) / 1000, .groups = "drop") %>%
    complete(cl_age = factor(AGE_LAB, AGE_LAB), cl_y = factor(lab, lab),
             fill = list(n_ech = 0, naiss_k = 0)) %>%
    mutate(champ = champ, ordonnee = var_y)
}

tracer <- function(a, titre, sous_titre, lab_y, source_txt) {
  etiqueter <- nrow(a) <= MAX_CASES_ETIQUETEES
  seuil <- max(a$naiss_k) * 0.58
  g <- ggplot(a, aes(cl_age, cl_y, fill = naiss_k)) +
    geom_tile(colour = SURFACE, linewidth = 1.1)    # surface entre les cases
  if (etiqueter)
    g <- g +
      geom_text(aes(label = ifelse(n_ech == 0, "--", sprintf("%.0f", naiss_k)),
                    colour = naiss_k > seuil),
                size = 4.2, fontface = "bold", vjust = -0.15) +
      geom_text(aes(label = ifelse(n_ech == 0, "", paste0("n = ", n_ech)),
                    colour = naiss_k > seuil), size = 2.9, vjust = 1.5, alpha = 0.85) +
      scale_colour_manual(values = c(`FALSE` = "#1a1a18", `TRUE` = "#ffffff"),
                          guide = "none")
  legende <- if (etiqueter)
    "\nLe grand chiffre est l'estimation pondérée en milliers ; n est l'effectif d'échantillon."
  else
    "\nClasses régulières : les cases vides sont conservées, la couleur seule porte l'information."
  g +
    scale_fill_gradient(low = BLEU_CLAIR, high = BLEU_FONCE,
                        name = "Naissances\n(milliers)") +
    coord_fixed(ratio = 0.75, expand = FALSE) +
    labs(title = titre, subtitle = sous_titre,
         x = "Âge moyen du ou des parents (ans)", y = lab_y,
         caption = paste0("Insee, ", source_txt,
                          " Champ : ménages comportant une femme de ",
                          AGE_BAS, " à ", AGE_HAUT, " ans.", legende)) +
    theme_minimal(base_size = 11) +
    theme(panel.grid = element_blank(), axis.ticks = element_blank(),
          plot.title = element_text(face = "bold"),
          plot.subtitle = element_text(colour = "#57534e", size = 9),
          plot.caption = element_text(size = 7.5, colour = "#78716c", hjust = 0),
          axis.text.x = element_text(angle = 45, hjust = 1),
          axis.text.y = element_text(size = 8),
          legend.key.height = unit(1.5, "cm"))
}

if (!dir.exists("Output")) dir.create("Output")

ORDONNEES <- list(
  list(var = "nv", brk = NV_BRK, lab = NV_LAB, slug = "niveau_vie",
       axe = "Niveau de vie par UC (milliers d'euros constants 2025)"),
  list(var = "surface", brk = SURF_BRK, lab = SURF_LAB, slug = "surface",
       axe = "Surface du logement (m2)")
)

CHAMPS <- list(
  list(ans = ANNEE_SEULE, slug = ANNEE_SEULE, suffixe = ANNEE_SEULE,
       note = "lecture indicative : une centaine de naissances sur une grille fine",
       src  = paste0("SRCV ", ANNEE_SEULE, ".")),
  list(ans = ANNEES_POOL, slug = "poole", suffixe = "6 vagues cumulées",
       note = "les effectifs cumulent six vagues : ce ne sont pas des naissances annuelles",
       src  = paste0("SRCV ", paste(ANNEES_POOL, collapse = ", "),
                     " ; 2023 et 2024 exclus (anomalie de pondération)."))
)

tout <- list()
for (st in names(STATUTS)) {
  for (ch in CHAMPS) {
    d <- charger(ch$ans, st)
    cat("== ", STATUTS[[st]], " / ", ch$slug, " : ", nrow(d), " ménages, ",
        sum(d$naissance == "Oui"), " naissances ==\n", sep = "")
    titre <- paste0("Naissances chez les ", STATUTS_LIB[[st]], ", ", ch$suffixe)
    for (o in ORDONNEES) {
      a <- agreger(d, o$var, o$brk, o$lab, ch$slug) %>% mutate(statut = STATUTS[[st]])
      tout[[length(tout) + 1]] <- a
      f <- paste0("Output/heatmap_naissances_", o$slug, "_", STATUTS[[st]], "_",
                  ch$slug, ".png")
      ggsave(f, tracer(a, titre,
                       paste0(sum(a$n_ech), " naissances observées sur ", nrow(a),
                              " cases — ", ch$note), o$axe, ch$src),
             width = 9.5, height = 9.5, dpi = 200, bg = "white")
      cat("    ", f, " (", nrow(a), " cases)\n", sep = "")
    }
  }
}

write_csv(bind_rows(tout), "Output/heatmap_naissances.csv")
cat("\nExporté : 8 PNG + Output/heatmap_naissances.csv\n")
