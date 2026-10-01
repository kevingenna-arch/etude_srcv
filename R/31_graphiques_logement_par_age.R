# ============================================================================
#  SRCV — Graphiques des conditions de logement par classe d'âge (2014-2025)
# ----------------------------------------------------------------------------
#  Trois figures, une par indicateur (surface, coût mensuel, taux d'effort),
#  quatre panneaux par classe d'âge de la personne de référence (20-39 ans).
#  Trait : moyenne pondérée ; bande : moyenne ± 1 écart-type pondéré, soit la
#  dispersion entre ménages (et non l'incertitude sur la moyenne).
#  Locataires et propriétaires accédants ; cellules de moins de 30 ménages
#  masquées, comme dans 09 et 11.
#
#  Mêmes conventions que 09 (déflateur IPC) : coût = charges + mensualité, en
#  euros constants de 2025 ; taux d'effort borné à [0 ; 100] points.
#  Sorties : Output/graph_age_{surface,cout,effort}.png
# ============================================================================

source("R/00_utils.R")
source("R/00_config.R")
library(ggplot2)

ANNEES <- c("2014", "2018", "2022", "2023", "2024", "2025")
N_MIN  <- 30
IPC    <- charger_ipc()

d <- imap_dfr(VAGUES[ANNEES], function(cfg, annee) {
  charger_menages(annee, cfg) %>%
    mutate(classe = cut(age_pr, breaks = c(19, 24, 29, 34, 39),
                        labels = c("20-24 ans", "25-29 ans", "30-34 ans", "35-39 ans"))) %>%
    filter(!is.na(classe), statut3 %in% c("Locataire", "Proprietaire_accedant"),
           !is.na(cout), cout >= 0)
}) %>%
  mutate(cout_defl = deflater(cout, as.integer(annee), IPC, base = 2025),
         effort    = if_else(!is.na(revenu) & revenu > 0,
                             pmin(pmax(100 * (cout * 12) / revenu, 0), 100), NA_real_)) %>%
  # Même champ que 09, dont svymean(na.rm = TRUE) ne garde que les ménages
  # renseignés sur toutes les variables à la fois.
  filter(!is.na(surface), surface > 0, !is.na(effort))

# Mensualités invraisemblables (> MENSUALITE_MAX) : supprimées en amont par
# charger_menages() (00_utils.R), comme dans tout le projet depuis le 2026-10-01.
# Sur ce champ, quatre accédants de 35-39 ans (13 667 à 91 000 €/mois).

wsd_ <- function(x, w) { ok <- !is.na(x) & !is.na(w); m <- weighted.mean(x[ok], w[ok])
  sqrt(sum(w[ok] * (x[ok] - m)^2) / sum(w[ok])) }

stats <- d %>%
  group_by(annee, classe, statut3) %>%
  summarise(n = n(),
            across(c(surface, cout_defl, effort),
                   list(moy = ~ weighted.mean(.x, poids, na.rm = TRUE),
                        et  = ~ wsd_(.x, poids))),
            .groups = "drop") %>%
  filter(n >= N_MIN) %>%
  mutate(an = as.integer(annee),
         statut = factor(statut3, c("Locataire", "Proprietaire_accedant"),
                         c("Locataires", "Propriétaires accédants")))

# Contrôle : les moyennes doivent être celles de 09 (Output/logement_par_classe_age_ipc.csv)
ref <- read_csv("Output/logement_par_classe_age_ipc.csv", show_col_types = FALSE) %>%
  mutate(annee = as.character(annee))
ecart <- stats %>% inner_join(ref, by = c("annee", "classe", "statut3")) %>%
  summarise(surface = max(abs(surface_moy - surface)), cout = max(abs(cout_defl_moy - cout_defl)),
            effort = max(abs(effort_moy - effort)))
cat("Écart maximal avec les moyennes de 09 (non nul si des ménages ont été écartés) :\n")
print(as.data.frame(ecart))
cat("\nCellules dont le coût moyen diffère de 09 de plus de 1 € :\n")
print(as.data.frame(stats %>% inner_join(ref, by = c("annee", "classe", "statut3")) %>%
  filter(abs(cout_defl_moy - cout_defl) > 1) %>%
  transmute(annee, classe, statut3, cout_09 = round(cout_defl), cout_ici = round(cout_defl_moy))))
write_csv(stats %>% select(-an, -statut), "Output/graph_age_donnees.csv")

COULEURS <- c("Locataires" = "#1F4E79", "Propriétaires accédants" = "#B9770E")

graphe <- function(var, titre_y, borne = c(-Inf, Inf), format_y = waiver()) {
  s <- stats %>% transmute(an, classe, statut, moy = .data[[paste0(var, "_moy")]],
                           bas = pmax(moy - .data[[paste0(var, "_et")]], borne[1]),
                           haut = pmin(moy + .data[[paste0(var, "_et")]], borne[2]))
  ggplot(s, aes(an, moy, colour = statut, fill = statut)) +
    geom_ribbon(aes(ymin = bas, ymax = haut), alpha = 0.15, colour = NA) +
    geom_line(linewidth = 0.8) +
    geom_point(size = 1.8) +
    facet_wrap(~ classe, nrow = 2) +
    scale_colour_manual(values = COULEURS, name = NULL) +
    scale_fill_manual(values = COULEURS, name = NULL) +
    scale_x_continuous(breaks = c(2014, 2018, 2022, 2025)) +
    scale_y_continuous(labels = format_y) +
    labs(x = NULL, y = titre_y,
         caption = paste0("Trait : moyenne pondérée. Bande : moyenne ± 1 écart-type pondéré.\n",
                          "Accédants de 20-24 ans non représentés (moins de 30 ménages par vague).")) +
    theme_minimal(base_size = 11) +
    theme(legend.position = "top", panel.grid.minor = element_blank(),
          strip.text = element_text(face = "bold"),
          plot.caption = element_text(colour = "grey35", size = 8.5))
}

milliers <- function(x) format(x, big.mark = " ", decimal.mark = ",", trim = TRUE)
virgule  <- function(x) format(x, decimal.mark = ",", trim = TRUE)

ggsave("Output/graph_age_surface.png", graphe("surface", "Surface du logement (m²)", c(0, Inf), virgule),
       width = 7.5, height = 5.5, dpi = 200, bg = "white")
ggsave("Output/graph_age_cout.png", graphe("cout_defl", "Coût mensuel (€ constants de 2025)", c(0, Inf), milliers),
       width = 7.5, height = 5.5, dpi = 200, bg = "white")
ggsave("Output/graph_age_effort.png", graphe("effort", "Taux d'effort (%)", c(0, 100), virgule),
       width = 7.5, height = 5.5, dpi = 200, bg = "white")
cat("Exporté : Output/graph_age_{surface,cout,effort}.png + graph_age_donnees.csv\n")

# ── Écarts 2014 → 2025 par classe d'âge (version graphique du tableau 2) ─────
#  Barre : écart des moyennes pondérées entre 2014 et 2025. Trait : ± 1
#  écart-type de cet écart. Les vagues 2014 et 2025 appartiennent à deux
#  groupes de panel distincts (00_config.R) : échantillons indépendants, donc
#  écart-type de la différence = racine de la somme des variances des moyennes
#  (erreurs-types calculées comme dans 09, plan pondéré sans grappes).
library(survey)
d2  <- d %>% filter(annee %in% c("2014", "2025")) %>% droplevels()
des <- svydesign(ids = ~1, weights = ~poids, data = d2)
moy <- svyby(~surface + cout_defl + effort, ~annee + classe + statut3, des, svymean, na.rm = TRUE) %>%
  as_tibble() %>%
  left_join(count(d2, annee, classe, statut3), by = c("annee", "classe", "statut3"))

VARS <- c("surface", "cout_defl", "effort")
ecarts <- bind_rows(lapply(VARS, function(v) moy %>%
  transmute(annee, classe, statut3, n, var = v, m = .data[[v]], se = .data[[paste0("se.", v)]]))) %>%
  tidyr::pivot_wider(names_from = annee, values_from = c(m, se, n)) %>%
  mutate(masque = n_2014 < N_MIN | n_2025 < N_MIN,
         ecart = if_else(masque, NA_real_, m_2025 - m_2014),
         se_ecart = if_else(masque, NA_real_, sqrt(se_2014^2 + se_2025^2)),
         statut = factor(statut3, c("Locataire", "Proprietaire_accedant"),
                         c("Locataires", "Propriétaires accédants")))
write_csv(ecarts %>% select(-statut), "Output/graph_age_ecarts_2014_2025.csv")
cat("\nÉcarts 2014 → 2025 (moyenne et écart-type de l'écart) :\n")
print(as.data.frame(ecarts %>% transmute(var, classe, statut3, ecart = round(ecart, 1),
                                         se_ecart = round(se_ecart, 1), n_2014, n_2025)))

graphe_ecart <- function(e, titre_y, chiffres, x = "classe") {
  e <- e %>% mutate(x = .data[[x]]) %>%
    mutate(lab = if_else(is.na(ecart), "n < 30",
                         paste0(if_else(ecart > 0, "+", ""),
                                formatC(ecart, format = "f", digits = chiffres, decimal.mark = ","))),
           y_lab = case_when(is.na(ecart) ~ 0, ecart >= 0 ~ ecart + se_ecart, TRUE ~ ecart - se_ecart),
           vj = if_else(!is.na(ecart) & ecart < 0, 1.6, -0.6))
  pos <- position_dodge(width = 0.5)
  ggplot(e, aes(x, ecart, colour = statut)) +
    geom_hline(yintercept = 0, linewidth = 0.4, colour = "grey30") +
    geom_pointrange(aes(ymin = ecart - se_ecart, ymax = ecart + se_ecart), position = pos,
                    size = 0.45, linewidth = 0.8, na.rm = TRUE) +
    geom_text(aes(y = y_lab, label = lab, vjust = vj, group = statut), position = pos,
              size = 3.3, show.legend = FALSE) +
    scale_colour_manual(values = COULEURS, name = NULL, drop = FALSE) +
    scale_y_continuous(labels = function(x) format(x, decimal.mark = ",", big.mark = " ", trim = TRUE),
                       expand = expansion(mult = 0.15)) +
    labs(x = NULL, y = titre_y,
         caption = paste0("Point : écart des moyennes pondérées entre 2014 et 2025. ",
                          "Trait : ± 1 écart-type de l'écart.")) +
    theme_minimal(base_size = 11) +
    theme(legend.position = "top", panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank(),
          plot.caption = element_text(colour = "grey35", size = 8.5))
}

ggsave("Output/graph_age_ecart_surface.png",
       graphe_ecart(filter(ecarts, var == "surface"), "Écart de surface 2014 → 2025 (m²)", 1),
       width = 7, height = 4.3, dpi = 200, bg = "white")
ggsave("Output/graph_age_ecart_cout.png",
       graphe_ecart(filter(ecarts, var == "cout_defl"), "Écart de coût mensuel 2014 → 2025 (€ de 2025)", 0),
       width = 7, height = 4.3, dpi = 200, bg = "white")
ggsave("Output/graph_age_ecart_effort.png",
       graphe_ecart(filter(ecarts, var == "effort"), "Écart de taux d'effort 2014 → 2025 (points)", 1),
       width = 7, height = 4.3, dpi = 200, bg = "white")
cat("Exporté : Output/graph_age_ecart_{surface,cout,effort}.png + graph_age_ecarts_2014_2025.csv\n")

# ── Écart de surface 2014 → 2025 par tranche d'unité urbaine (PR de 20-39 ans) ─
#  Tranche d'unité urbaine de la commune : tuu10 en 2014, TUU2017 en 2025 (même
#  concept, zonage mis à jour entre les deux : quelques communes passent du
#  rural à l'urbain). Cinq tranches si toutes les cellules comptent au moins
#  N_MIN ménages dans chaque vague, deux sinon (200 000 habitants et plus,
#  agglomération parisienne comprise / moins de 200 000 habitants).
dens <- imap_dfr(VAGUES[c("2014", "2025")], function(cfg, annee) {
  raw <- lire_srcv(cfg$men)
  tibble(annee = annee, id = trimws(as.character(raw[[cfg$vars[["id"]]]])),
         densite = recode_densite(raw[[cfg$vars[["tuu"]]]], cfg$tuu_type))
}) %>% distinct(annee, id, .keep_all = TRUE)
d3 <- d2 %>% left_join(dens, by = c("annee", "id")) %>% filter(!is.na(densite))
eff <- count(d3, annee, densite, statut3, .drop = FALSE)
cat("\nEffectifs par tranche d'unité urbaine, statut et vague :\n"); print(as.data.frame(eff))
LIB_DENS <- c("Rural", "Moins de\n20 000 hab.", "20 000 à\n199 999 hab.", "200 000 hab.\nà 2 millions",
              "Agglomération\nde Paris")
if (all(eff$n >= N_MIN)) {
  d3 <- d3 %>% mutate(zone = factor(densite, DENSITE_LIB, LIB_DENS))
} else {
  cat("Au moins une cellule sous", N_MIN, "ménages : regroupement en deux tranches.\n")
  d3 <- d3 %>% mutate(zone = factor(if_else(densite %in% DENSITE_LIB[4:5],
                                            "200 000 hab. et plus\n(dont Paris)", "Moins de\n200 000 hab."),
                                    c("Moins de\n200 000 hab.", "200 000 hab. et plus\n(dont Paris)")))
}
des3 <- svydesign(ids = ~1, weights = ~poids, data = d3)
ecarts_dens <- svyby(~surface, ~annee + zone + statut3, des3, svymean, na.rm = TRUE) %>% as_tibble() %>%
  left_join(count(d3, annee, zone, statut3), by = c("annee", "zone", "statut3")) %>%
  transmute(annee, zone, statut3, n, m = surface, se = se) %>%
  tidyr::pivot_wider(names_from = annee, values_from = c(m, se, n)) %>%
  mutate(masque = n_2014 < N_MIN | n_2025 < N_MIN,
         ecart = if_else(masque, NA_real_, m_2025 - m_2014),
         se_ecart = if_else(masque, NA_real_, sqrt(se_2014^2 + se_2025^2)),
         statut = factor(statut3, c("Locataire", "Proprietaire_accedant"),
                         c("Locataires", "Propriétaires accédants")))
write_csv(ecarts_dens %>% select(-statut), "Output/graph_densite_ecart_surface.csv")
cat("\nÉcart de surface 2014 → 2025 par tranche d'unité urbaine :\n")
print(as.data.frame(ecarts_dens %>% transmute(zone = gsub("\n", " ", zone), statut3,
  m_2014 = round(m_2014, 1), m_2025 = round(m_2025, 1), ecart = round(ecart, 1),
  se_ecart = round(se_ecart, 1), n_2014, n_2025)))
ggsave("Output/graph_densite_ecart_surface.png",
       graphe_ecart(ecarts_dens, "Écart de surface 2014 → 2025 (m²)", 1, x = "zone") +
         labs(caption = paste0("Personne de référence de 20 à 39 ans. Point : écart des moyennes ",
                               "pondérées entre 2014 et 2025. Trait : ± 1 écart-type de l'écart.")),
       width = 7.5, height = 4.5, dpi = 200, bg = "white")
cat("Exporté : Output/graph_densite_ecart_surface.png + .csv\n")

# ── Même écart, par classe d'âge ─────────────────────────────────────────────
#  Croisées avec les classes d'âge, les cinq tranches laissent trop de cellules
#  sous N_MIN (13 locataires de 20-24 ans dans l'agglomération parisienne, 13 à
#  26 accédants de 25-29 ans selon la tranche) : deux groupes, moins de 200 000
#  habitants / 200 000 et plus (agglomération parisienne comprise).
G2 <- c("Moins de\n200 000 hab.", "200 000 hab. et plus\n(dont Paris)")
d4 <- d2 %>% left_join(dens, by = c("annee", "id")) %>% filter(!is.na(densite)) %>%
  mutate(zone = factor(if_else(densite %in% DENSITE_LIB[4:5], G2[2], G2[1]), G2))
des4 <- svydesign(ids = ~1, weights = ~poids, data = d4)
ecarts_age_dens <- svyby(~surface, ~annee + classe + zone + statut3, des4, svymean, na.rm = TRUE) %>%
  as_tibble() %>%
  left_join(count(d4, annee, classe, zone, statut3), by = c("annee", "classe", "zone", "statut3")) %>%
  transmute(annee, classe, zone, statut3, n, m = surface, se = se) %>%
  tidyr::pivot_wider(names_from = annee, values_from = c(m, se, n)) %>%
  tidyr::complete(classe, zone, statut3) %>%
  mutate(masque = is.na(n_2014) | is.na(n_2025) | n_2014 < N_MIN | n_2025 < N_MIN,
         ecart = if_else(masque, NA_real_, m_2025 - m_2014),
         se_ecart = if_else(masque, NA_real_, sqrt(se_2014^2 + se_2025^2)),
         statut = factor(statut3, c("Locataire", "Proprietaire_accedant"),
                         c("Locataires", "Propriétaires accédants")))
write_csv(ecarts_age_dens %>% select(-statut), "Output/graph_age_densite_ecart_surface.csv")
cat("\nÉcart de surface 2014 → 2025 par classe d'âge et taille d'agglomération :\n")
print(as.data.frame(ecarts_age_dens %>% transmute(classe, zone = gsub("\n", " ", zone), statut3,
  ecart = round(ecart, 1), se_ecart = round(se_ecart, 1), n_2014, n_2025)))

lim <- with(ecarts_age_dens, range(c(ecart - se_ecart, ecart + se_ecart, 0), na.rm = TRUE))
lim <- lim + c(-0.12, 0.12) * diff(lim)
LEGENDE <- paste0("Point : écart des moyennes pondérées entre 2014 et 2025. ",
                  "Trait : ± 1 écart-type de l'écart.")
for (cl in levels(ecarts_age_dens$classe)) {
  g <- graphe_ecart(filter(ecarts_age_dens, classe == cl), "Écart de surface 2014 → 2025 (m²)", 1,
                    x = "zone") +
    coord_cartesian(ylim = lim) +
    labs(title = paste("Personne de référence de", cl), caption = LEGENDE) +
    theme(plot.title = element_text(size = 11, face = "bold"))
  ggsave(paste0("Output/graph_age_densite_ecart_surface_", sub(" ans", "", cl), ".png"), g,
         width = 6, height = 4.2, dpi = 200, bg = "white")
}
ggsave("Output/graph_age_densite_ecart_surface.png",
       graphe_ecart(ecarts_age_dens, "Écart de surface 2014 → 2025 (m²)", 1, x = "zone") +
         facet_wrap(~ classe, nrow = 2) +
         labs(caption = LEGENDE) +
         theme(strip.text = element_text(face = "bold")),
       width = 8, height = 6.5, dpi = 200, bg = "white")
cat("Exporté : Output/graph_age_densite_ecart_surface[_classe].png + .csv\n")
