# ============================================================================
#  SRCV — Naissances par tranche de revenu de 500 EUR
# ----------------------------------------------------------------------------
#  Deux lectures, produites côte à côte parce qu'elles ne répondent pas à la
#  même question :
#
#   A. NOMBRE de naissances par tranche (comptage brut, non pondéré).
#      Mesure où se trouvent les naissances. Dominé par la démographie : le pic
#      est là où il y a le plus de ménages, pas là où on fait le plus d'enfants.
#
#   B. TAUX de naissance par tranche = naissances / ménages de la tranche,
#      PONDÉRÉ. Mesure la propension à avoir un enfant, à effectif neutralisé.
#      C'est la lecture pertinente pour un raisonnement causal.
#
#  Abscisse commune : revenu en tranches RÉGULIÈRES de 500 EUR constants 2025.
#  Deux mesures du revenu, "le revenu" pouvant désigner l'une ou l'autre :
#    - REVENU DISPONIBLE du ménage (HY020) : ce qui entre dans le foyer.
#    - NIVEAU DE VIE par UC : le même revenu rapporté à la taille du ménage.
#
#  Champ : ménages comportant une femme de 15 à 49 ans, TOUS STATUTS
#  d'occupation confondus (locataires, accédants, non accédants).
#
#  ⚠️ MILLÉSIMES DIFFÉRENTS SELON LA LECTURE, et c'est volontaire :
#     - Comptage (A), non pondéré : les HUIT vagues. L'anomalie de pondération
#       de 2022-2023 ne déforme que les estimations pondérées, un comptage
#       d'observations n'en souffre pas. On gagne ainsi 660 naissances.
#     - Taux (B), pondéré : 2022 et 2023 sont ÉCARTÉS. Ce sont les deux vagues
#       où le rapport du poids moyen naissance / non-naissance s'inverse (0,665
#       et 0,634 contre 1,18 à 1,30 ailleurs), ce qui biaise directement un taux
#       pondéré. 2024 est conservé : son rapport de 1,183 est dans la norme.
#       NB : ce choix diffère de l'exclusion "2023 et 2024" retenue ailleurs
#       dans le projet, qui visait la lisibilité d'une série annuelle ; ici
#       c'est la corrélation poids/naissance qui commande.
#
#  ⚠️ Sur le graphique de TAUX, la TAILLE DU POINT est proportionnelle au nombre
#     de ménages de la tranche, et le lissage loess est pondéré par ce même
#     effectif : les tranches minces ne tirent donc pas la courbe.
#
#  Sorties : Output/nuage_naissances_par_revenu.png
#            Output/nuage_naissances_par_niveau_vie.png
#            Output/taux_naissance_par_revenu.png
#            Output/taux_naissance_par_niveau_vie.png
#            Output/naissances_par_tranche_revenu.csv
# ============================================================================

source("R/00_prepa_fecondite.R")

PAS              <- 500     # largeur des tranches, en euros constants 2025
AGE_BAS          <- 15
AGE_HAUT         <- 49
ANNEES_COMPTAGE  <- names(VAGUES)                       # les 8 vagues
ANNEES_TAUX      <- setdiff(names(VAGUES), c("2022", "2023"))
BLEU             <- "#2a78d6"   # slot 1 de la palette catégorielle

IPC <- charger_ipc()

base <- bind_rows(imap(VAGUES, ~ preparer_donnees(.y, .x, restreindre = FALSE))) %>%
  filter(!is.na(age_femme), age_femme >= AGE_BAS, age_femme <= AGE_HAUT,
         !is.na(statut3), !is.na(naissance), !is.na(poids), poids > 0) %>%
  mutate(rev = deflater(revenu,     as.integer(annee) - 1, IPC, base = 2025),
         nv  = deflater(niveau_vie, as.integer(annee) - 1, IPC, base = 2025))

cat("Champ 15-49, tous statuts, 8 vagues :", nrow(base), "ménages |",
    sum(base$naissance == "Oui"), "naissances\n")

tranche_de <- function(x) floor(x / PAS) * PAS + PAS / 2   # point au milieu

# ── A. Comptage brut, non pondéré, 8 vagues ────────────────────────────────
compter <- function(var) {
  base %>% filter(annee %in% ANNEES_COMPTAGE, naissance == "Oui") %>%
    mutate(tranche = tranche_de(.data[[var]])) %>%
    filter(!is.na(tranche)) %>% count(tranche, name = "naissances")
}

# ── B. Taux pondéré : naissances / ménages de la tranche ───────────────────
taux <- function(var) {
  base %>% filter(annee %in% ANNEES_TAUX) %>%
    mutate(tranche = tranche_de(.data[[var]])) %>%
    filter(!is.na(tranche)) %>%
    group_by(tranche) %>%
    summarise(n_menages = n(),
              w_tot     = sum(poids),
              w_naiss   = sum(poids[naissance == "Oui"]),
              naiss_n   = sum(naissance == "Oui"),
              taux_pct  = 100 * w_naiss / w_tot, .groups = "drop")
}

theme_srcv <- function() {
  theme_minimal(base_size = 11) +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major = element_line(colour = "#ececea", linewidth = 0.3),
          plot.title = element_text(face = "bold"),
          plot.subtitle = element_text(colour = "#57534e", size = 9),
          plot.caption = element_text(size = 7.5, colour = "#78716c", hjust = 0))
}

tracer_comptage <- function(a, lab_x, borne_x, titre) {
  hors <- sum(a$naissances[a$tranche > borne_x])
  ggplot(a, aes(tranche, naissances)) +
    geom_smooth(method = "loess", span = 0.3, se = TRUE, colour = BLEU,
                fill = BLEU, alpha = 0.12, linewidth = 0.9) +
    geom_point(colour = BLEU, size = 1.9, alpha = 0.75) +
    scale_x_continuous(labels = function(v) format(v, big.mark = " ")) +
    coord_cartesian(xlim = c(0, borne_x), ylim = c(0, NA)) +
    labs(title = titre,
         subtitle = paste0(sum(a$naissances), " naissances observées, tranches de ",
                           PAS, " euros — comptage NON pondéré, 8 vagues"),
         x = lab_x, y = "Nombre de naissances observées",
         caption = paste0("Insee, SRCV 2006 à 2025. Champ : ménages comportant une femme de ",
                          AGE_BAS, " à ", AGE_HAUT, " ans, tous statuts confondus.",
                          "\nUn point = une tranche de ", PAS, " euros constants 2025.",
                          " Comptage d'observations, pas estimation de population.",
                          if (hors > 0) paste0(" ", hors, " naissances hors axe.") else "",
                          "\nATTENTION : cette courbe mêle fécondité et effectif de la",
                          " tranche. Voir le graphique de TAUX pour la propension.")) +
    theme_srcv()
}

tracer_taux <- function(a, lab_x, borne_x, titre) {
  # L'axe des taux est borné sur la plage utile : quelques tranches de très
  # faible effectif atteignent 25 à 100 % et écraseraient tout le reste.
  plafond <- ceiling(max(a$taux_pct[a$n_menages >= 50], na.rm = TRUE) * 1.15)
  masques <- sum(a$taux_pct > plafond & a$tranche <= borne_x)
  ggplot(a, aes(tranche, taux_pct)) +
    # lissage PONDÉRÉ par l'effectif : les tranches minces ne tirent pas la courbe
    geom_smooth(aes(weight = n_menages), method = "loess", span = 0.35, se = TRUE,
                colour = BLEU, fill = BLEU, alpha = 0.12, linewidth = 0.9) +
    geom_point(aes(size = n_menages), colour = BLEU, alpha = 0.55, stroke = 0) +
    scale_size_area(max_size = 5, name = "Ménages\ndans la tranche") +
    scale_x_continuous(labels = function(v) format(v, big.mark = " ")) +
    coord_cartesian(xlim = c(0, borne_x), ylim = c(0, plafond)) +
    labs(title = titre,
         subtitle = paste0("Naissances rapportées aux ménages de la tranche — PONDÉRÉ, ",
                           length(ANNEES_TAUX), " vagues (2022 et 2023 écartées)"),
         x = lab_x, y = "Taux de naissance (%)",
         caption = paste0("Insee, SRCV ", paste(ANNEES_TAUX, collapse = ", "),
                          ". Champ : ménages comportant une femme de ", AGE_BAS, " à ",
                          AGE_HAUT, " ans, tous statuts confondus.",
                          "\nUn point = une tranche de ", PAS,
                          " euros constants 2025 ; sa taille est l'effectif de ménages",
                          " de la tranche, et le lissage est pondéré par cet effectif.",
                          "\n2022 et 2023 sont écartées : la corrélation entre poids de",
                          " sondage et naissance y est inversée (cf. R/17-18).",
                          if (masques > 0) paste0(" ", masques, " tranche(s) de très faible",
                            " effectif dépassent l'axe et ne sont pas affichées.") else "")) +
    theme_srcv() + theme(legend.position = "right")
}

if (!dir.exists("Output")) dir.create("Output")

MESURES <- list(
  list(var = "rev", borne = 120000, slug = "revenu",
       axe = "Revenu disponible du ménage (euros constants 2025)",
       t_a = "Naissances par tranche de revenu disponible",
       t_b = "Taux de naissance par tranche de revenu disponible"),
  list(var = "nv", borne = 70000, slug = "niveau_vie",
       axe = "Niveau de vie par UC (euros constants 2025)",
       t_a = "Naissances par tranche de niveau de vie",
       t_b = "Taux de naissance par tranche de niveau de vie")
)

export <- list()
for (m in MESURES) {
  a <- compter(m$var); b <- taux(m$var)
  ggsave(paste0("Output/nuage_naissances_par_", m$slug, ".png"),
         tracer_comptage(a, m$axe, m$borne, m$t_a),
         width = 10, height = 6, dpi = 200, bg = "white")
  ggsave(paste0("Output/taux_naissance_par_", m$slug, ".png"),
         tracer_taux(b, m$axe, m$borne, m$t_b),
         width = 10.5, height = 6, dpi = 200, bg = "white")
  export[[length(export) + 1]] <- b %>% mutate(mesure = m$slug) %>%
    left_join(a, by = "tranche")
  pic <- b %>% filter(n_menages >= 100) %>% slice_max(taux_pct, n = 1)
  cat("  ", m$slug, " : ", nrow(b), " tranches | max du taux (tranches >= 100 ménages) : ",
      round(pic$taux_pct, 1), " % vers ", pic$tranche, " EUR\n", sep = "")
}

write_csv(bind_rows(export), "Output/naissances_par_tranche_revenu.csv")
cat("\nExporté : 4 PNG + Output/naissances_par_tranche_revenu.csv\n")
