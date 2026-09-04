# ============================================================================
#  SRCV — SCÉNARIO : que prédit le modèle pour +10 m² de surface ?
# ----------------------------------------------------------------------------
#  Modèle : F_DECOMPOSITION (00_prepa_fecondite.R), probit pondéré estimé sur
#  les 8 vagues poolées, erreurs-types regroupées par ménage. C'est le même
#  modèle que celui de 02b, pas une ré-estimation différente.
#
#  On construit un MÉNAGE TYPE (profil fixé) et on lit la probabilité prédite
#  de naissance à surface S, puis à S + 10 m².
#
#  DEUX SCÉNARIOS, qui ne disent pas la même chose :
#   1. "à prix au m² constant" — le ménage prend 10 m² de plus au même prix
#      unitaire, donc il PAIE PLUS (+10 × cout_m2 par mois). C'est le ceteris
#      paribus littéral du coefficient de surface_10.
#   2. "à budget constant" — le ménage obtient 10 m² de plus pour le même coût
#      total, donc son prix au m² BAISSE. C'est le scénario de politique
#      publique (un logement plus grand sans dépenser davantage), et il combine
#      l'effet surface et l'effet prix.
#
#  ⚠️ ASSOCIATION, PAS CAUSALITÉ. Le test des non-déménageurs (15) montre que
#     l'effet de la surface disparaît chez les ménages dont le logement est
#     prédéterminé : ces prédictions décrivent ce qu'on observe, pas ce qui se
#     produirait si on agrandissait les logements.
#
#  ⚠️ L'issue n'a pas la même définition selon la vague : naissance dans
#     l'année (EVENEMEN_C, 2006-2018) contre enfant né en N ou N-1 (FPR,
#     2022-2025, soit ~2 ans). Le scénario est calé sur 2025 : la probabilité
#     lue couvre donc environ deux ans.
#
#  Sortie : Output/scenario_surface.csv
# ============================================================================

source("R/00_prepa_fecondite.R")
library(survey)
library(marginaleffects)

ANNEE_SCENARIO <- "2025"
AGE_FEMME      <- 30      # -> age_c = (30 - 30) / 10 = 0
#  Un ménage type PAR STATUT : le profil de référence (surface, prix au m²,
#  revenu, zone) est recalculé sur les ménages du statut concerné. Les deux
#  lignes ne décrivent donc pas le même ménage — c'est voulu, mais il ne faut
#  pas lire l'écart entre elles comme l'effet du statut.
STATUTS <- c("Locataire", "Proprietaire_accedant")

# ── Estimation (identique à 02b) ────────────────────────────────────────────
df_m2 <- preparer_df_m2(bind_rows(charger_millesimes()))
m <- svyglm(F_DECOMPOSITION,
            design = svydesign(ids = ~id_cluster, weights = ~poids, data = df_m2),
            family = quasibinomial(link = "probit"))

# ── Profil du ménage type, puis scénarios, pour un statut donné ─────────────
#  Continues : médiane pondérée du sous-champ. Qualitatives : modalité la plus
#  fréquente. Tout est affiché.
mode_de <- function(x, w) { t <- tapply(w, x, sum, na.rm = TRUE); names(which.max(t)) }
pred    <- function(d) as.numeric(predict(m, newdata = d, type = "response"))

scenarios <- function(statut) {
  ref <- df_m2 %>% filter(statut3 == statut, annee == ANNEE_SCENARIO)
  surface_ref <- wquantile(ref$surface, ref$poids, 0.5)
  profil <- tibble(
    cout_m2      = wquantile(ref$cout_m2, ref$poids, 0.5),
    surface_10   = surface_ref / 10,
    revenu_10k   = wquantile(ref$revenu, ref$poids, 0.5) / 10000,
    statut3      = factor(statut, levels = levels(df_m2$statut3)),
    taille_avant = round(wquantile(ref$taille_avant, ref$poids, 0.5)),
    nb_enf_avant = round(wquantile(ref$nb_enf_avant, ref$poids, 0.5)),
    demenage     = factor("Non", levels = levels(df_m2$demenage)),
    age_c        = (AGE_FEMME - 30) / 10,
    urbain       = factor(mode_de(ref$urbain, ref$poids), levels = levels(df_m2$urbain)),
    zeat         = factor(mode_de(ref$zeat, ref$poids), levels = levels(df_m2$zeat)),
    annee        = ANNEE_SCENARIO)
  cout_total <- profil$cout_m2 * surface_ref

  cat("=== Ménage type :", statut, "(n =", nrow(ref), "en", ANNEE_SCENARIO, ") ===\n")
  cat(sprintf("  âge de la femme            : %d ans\n", AGE_FEMME))
  cat(sprintf("  surface de référence       : %.0f m2\n", surface_ref))
  cat(sprintf("  prix au m2                 : %.2f EUR/m2/mois (coût total %.0f EUR/mois)\n",
              profil$cout_m2, cout_total))
  cat(sprintf("  revenu disponible          : %.0f EUR/an\n", profil$revenu_10k * 10000))
  cat(sprintf("  taille du ménage / enfants : %d / %d (avant naissance)\n",
              profil$taille_avant, profil$nb_enf_avant))
  cat(sprintf("  urbanisation / ZEAT        : %s / %s\n\n",
              as.character(profil$urbain), as.character(profil$zeat)))

  # Scénario 1 : +10 m2 à PRIX AU M2 CONSTANT (le ménage paie davantage)
  p2 <- profil %>% mutate(surface_10 = surface_10 + 1)     # +1 unité = +10 m2
  # Scénario 2 : +10 m2 à BUDGET CONSTANT (le prix au m2 baisse)
  p3 <- profil %>% mutate(surface_10 = surface_10 + 1,
                          cout_m2 = cout_total / (surface_ref + 10))
  a <- pred(profil); b <- pred(p2); c3 <- pred(p3)

  # Intervalle de confiance du contraste (delta-méthode, plan de sondage inclus)
  ic <- comparisons(m, variables = list(surface_10 = 1), newdata = profil,
                    type = "response")

  tibble(statut = statut,
         scenario = c("1. +10 m2 a prix au m2 constant", "2. +10 m2 a budget constant"),
         surface_ref_m2 = surface_ref,
         p_depart_pct  = 100 * a,
         p_arrivee_pct = 100 * c(b, c3),
         ecart_points  = 100 * (c(b, c3) - a),
         ecart_relatif_pct = 100 * (c(b, c3) / a - 1),
         cout_mensuel_depart  = cout_total,
         cout_mensuel_arrivee = c(profil$cout_m2 * (surface_ref + 10), cout_total),
         ic_bas_points  = c(100 * ic$conf.low,  NA_real_),
         ic_haut_points = c(100 * ic$conf.high, NA_real_),
         p_value        = c(ic$p.value, NA_real_))
}

res <- map_dfr(STATUTS, scenarios)

cat("=== Probabilité prédite d'une naissance ===\n")
print(as.data.frame(res %>% select(statut, scenario, p_depart_pct, p_arrivee_pct,
                                   ecart_points, ecart_relatif_pct) %>%
                      mutate(across(where(is.numeric), ~ round(.x, 2)))),
      row.names = FALSE)

cat("\n=== Scénario 1 : intervalles de confiance à 95 % ===\n")
res %>% filter(!is.na(p_value)) %>%
  pwalk(function(statut, ecart_points, ic_bas_points, ic_haut_points, p_value, ...)
    cat(sprintf("  %-24s %+.2f point [%+.2f ; %+.2f]  p = %.4f\n",
                statut, ecart_points, ic_bas_points, ic_haut_points, p_value)))

if (!dir.exists("Output")) dir.create("Output")
write_csv(res, "Output/scenario_surface.csv")
cat("\nExporté : Output/scenario_surface.csv\n")
cat("\n⚠️ Association, pas causalité : cf. 15_test_non_demenageurs.R.\n")
