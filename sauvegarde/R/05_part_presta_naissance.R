# ============================================================================
#  SRCV 2006-2025 — Part des prestations familiales dans le revenu des couples
#  avec une NAISSANCE RÉCENTE (N ou N-1), par rang de naissance.
# ----------------------------------------------------------------------------
#  Part = HY050N (prestations familiales, net) / HY020 (revenu disponible),
#  moyenne pondérée. Couples corésidents (COUPLEPR=1) avec un enfant né dans
#  l'année ou l'année précédente.
#    - Naissance : EVENEMEN_C (2006-2018) ; reconstruite depuis le fichier
#      individus (enfant né en N/N-1) pour les fichiers FPR (2022-2025).
#    - Rang de naissance = NENFANTS (nb d'enfants après la naissance),
#      regroupé 1re / 2e / 3e ou +.
#
#  DEUX populations comparées :
#    - "Tous statuts"           : tous les couples avec naissance.
#    - "Deux parents en emploi" : NACTOCCUP == 2 (les deux en emploi au sens du
#      recensement, congé maternité/paternité inclus).
#
#  ⚠️ EFFECTIFS : petits au 3e+ (n ~ 25-50) -> estimations bruitées.
#  ⚠️ PANEL : les vagues 2014-2018 et 2022-2025 revoient largement les MÊMES
#     ménages (~68 % d'une vague FPR à la suivante). Les évolutions d'une année
#     à l'autre ne sont donc pas des échantillons indépendants : ce sont pour
#     partie des variations intra-panel. À garder en tête pour la tendance.
#
#  Sorties : Output/part_presta_naissance.csv  +  .png (graphe comparatif)
# ============================================================================

source("R/00_utils.R")     # lire_srcv(), num(), wsd(), verifier_colonnes()
source("R/00_config.R")    # VAGUES : chemins + mapping des variables

# ── Chargement d'une vague : couples avec naissance récente ─────────────────
# (une seule lecture par millésime — le filtre "deux en emploi" est appliqué
#  ensuite sur l'objet déjà chargé)
charger <- function(annee, cfg) {
  raw <- lire_srcv(cfg$men)

  if (!is.null(cfg$naiss_ind)) {          # FPR : naissance reconstruite (fichier individus)
    ni <- cfg$naiss_ind
    naiss <- lire_srcv(cfg$ind) %>%
      transmute(cle = trimws(as.character(.data[[ni$cle_ind]])),
                ne  = as.integer(num(.data[[ni$annais]]) %in% ni$annees)) %>%
      group_by(cle) %>% summarise(birth = as.integer(any(ne == 1, na.rm = TRUE)), .groups = "drop")
    raw <- raw %>% mutate(cle = trimws(as.character(.data[[ni$cle_men]]))) %>%
      left_join(naiss, by = "cle") %>% mutate(birth = coalesce(birth, 0L))
  } else {                                # 2006-2018 : variable EVENEMEN_C
    raw <- raw %>% mutate(birth = as.integer(num(.data[[cfg$vars[["y_birth"]]]]) == 1))
  }

  v <- cfg$vars
  verifier_colonnes(raw, c("HY050N", "NENFANTS", "NACTOCCUP",
                           v[["revenu"]], v[["poids"]], v[["couple"]]),
                    paste0("(millésime ", annee, ")"))

  raw %>% transmute(
      presta  = num(HY050N),
      revenu  = num(.data[[v[["revenu"]]]]),
      nenf    = num(NENFANTS),
      couple  = trimws(as.character(.data[[v[["couple"]]]])),
      poids   = num(.data[[v[["poids"]]]]),
      birth   = birth,
      nactocc = num(NACTOCCUP)) %>%
    filter(couple == "1", birth == 1, nenf >= 1,
           !is.na(revenu), revenu > 0, !is.na(presta), !is.na(poids), poids > 0) %>%
    mutate(part = 100 * pmin(pmax(presta / revenu, 0), 1),
           rang = cut(nenf, c(0, 1, 2, Inf), labels = c("1re", "2e", "3e ou +")))
}

# ── Statistiques par rang, sur un jeu déjà chargé ───────────────────────────
# NB : la PART (presta/revenu) est un ratio de la même année -> insensible à
# l'inflation. En revanche les MONTANTS sont comparés entre millésimes : ils
# sont donc aussi exprimés en EUROS CONSTANTS 2025 (IPC INSEE). HY050N et HY020
# portent tous deux sur l'année N-1 -> déflatés par l'indice de N-1.
IPC <- charger_ipc()

stats_rang <- function(d, annee, deux_emploi) {
  if (deux_emploi) d <- filter(d, nactocc == 2)
  an <- as.integer(annee)
  d %>% group_by(rang) %>%
    summarise(annee = an, n = n(),
              part_moy        = round(weighted.mean(part, poids), 1),
              part_sd         = round(wsd(part, poids), 1),
              presta_moy      = round(weighted.mean(presta, poids)),
              presta_moy_reel = round(deflater(weighted.mean(presta, poids), an - 1, IPC, 2025)),
              revenu_moy      = round(weighted.mean(revenu, poids)),
              revenu_moy_reel = round(deflater(weighted.mean(revenu, poids), an - 1, IPC, 2025)),
              .groups = "drop")
}

# Chaque millésime n'est lu QU'UNE fois, puis réutilisé pour les 2 populations.
vagues_chargees <- imap(VAGUES, ~ charger(.y, .x))

donnees <- bind_rows(
  imap_dfr(vagues_chargees, ~ stats_rang(.x, .y, FALSE)) %>% mutate(population = "Tous statuts"),
  imap_dfr(vagues_chargees, ~ stats_rang(.x, .y, TRUE))  %>% mutate(population = "Deux parents en emploi")
) %>% relocate(population)

cat("=== PART (%) — Tous statuts ===\n")
donnees %>% filter(population == "Tous statuts") %>% select(rang, annee, part_moy) %>%
  pivot_wider(names_from = annee, values_from = part_moy) %>% arrange(rang) %>% as.data.frame() %>% print()
cat("\n=== PART (%) — Deux parents en emploi ===\n")
donnees %>% filter(population == "Deux parents en emploi") %>% select(rang, annee, part_moy) %>%
  pivot_wider(names_from = annee, values_from = part_moy) %>% arrange(rang) %>% as.data.frame() %>% print()
cat("\n=== Montant moyen des prestations, EUROS CONSTANTS 2025 — Tous statuts ===\n")
donnees %>% filter(population == "Tous statuts") %>% select(rang, annee, presta_moy_reel) %>%
  pivot_wider(names_from = annee, values_from = presta_moy_reel) %>% arrange(rang) %>% as.data.frame() %>% print()

cat("\n=== Effectifs (n) — Tous statuts ===\n")
donnees %>% filter(population == "Tous statuts") %>% select(rang, annee, n) %>%
  pivot_wider(names_from = annee, values_from = n) %>% arrange(rang) %>% as.data.frame() %>% print()

# ── Export CSV ───────────────────────────────────────────────────────────────
if (!dir.exists("Output")) dir.create("Output")
write_csv(donnees, "Output/part_presta_naissance.csv")

# ── Graphe comparatif ────────────────────────────────────────────────────────
g <- ggplot(donnees, aes(annee, part_moy, colour = population, group = population)) +
  geom_line(linewidth = 1) + geom_point(size = 2) +
  facet_wrap(~rang, nrow = 1) +
  scale_x_continuous(breaks = as.integer(names(VAGUES))) +
  scale_colour_manual(values = c("Tous statuts" = "#C0392B", "Deux parents en emploi" = "#2471A3")) +
  labs(title = "Part des prestations familiales dans le revenu des couples avec naissance récente",
       subtitle = "Moyenne pondérée HY050N / HY020, par rang de naissance — SRCV 2006-2025",
       x = "Millésime SRCV", y = "Part dans le revenu disponible (%)", colour = NULL,
       caption = paste("Couples corésidents avec naissance N/N-1. 'Deux parents en emploi' =",
                       "NACTOCCUP=2 (congé mat/pat inclus).\nVagues 2022-2025 : mêmes ménages",
                       "en grande partie (panel) -> évolutions non indépendantes.")) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "top", panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("Output/part_presta_naissance.png", g, width = 11, height = 4.5, dpi = 150)
cat("\nExporté : Output/part_presta_naissance.csv  +  Output/part_presta_naissance.png\n")
