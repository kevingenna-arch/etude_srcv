# ============================================================================
#  SRCV — Test des NON-DÉMÉNAGEURS + EFFET MINIMUM DÉTECTABLE (MDE)
# ----------------------------------------------------------------------------
#  Deux questions, un seul script.
#
#  1. L'effet positif de la SURFACE sur la probabilité de naissance est-il de
#     l'anticipation ? Les couples qui auront un enfant en t+1 ont deux fois
#     plus souvent déménagé dans les 2 ans (31 % vs 17 %) : ils ont ajusté leur
#     logement AVANT l'enfant. On ré-estime donc le modèle sur les ménages dont
#     le logement est PRÉDÉTERMINÉ (installés depuis >= ANCIENNETE_MIN années
#     en t, via EMMENAG) : ils n'ont pas pu anticiper par le logement.
#     - si l'effet survit  -> contrainte de logement plausible
#     - s'il disparaît     -> l'effet mesuré était de l'anticipation
#
#  2. Quel effet aurait-on pu détecter ? MDE = (z_{1-a/2} + z_{puissance}) x SE,
#     soit 2,80 x SE à 5 % bilatéral et 80 % de puissance. Calculé sur les SE
#     RÉELLEMENT estimés : il intègre la pondération, le clustering ménage et
#     la colinéarité avec les contrôles, contrairement à une formule ex ante.
#
#  Résultats en EFFETS MARGINAUX MOYENS (points de pourcentage), pas en odds
#  ratios : sur un événement à 5,5 % un OR de 1,10 ne « vaut » que +0,5 point.
#
#  Sortie : Output/test_non_demenageurs.csv
# ============================================================================

suppressPackageStartupMessages({
  library(tidyverse); library(survey); library(broom)
})

ANCIENNETE_MIN <- 3      # années d'occupation du logement en t pour "non-déménageur"
PUISSANCE      <- 0.80
ALPHA          <- 0.05
Z_MDE          <- qnorm(1 - ALPHA / 2) + qnorm(PUISSANCE)   # ≈ 2,802

d0 <- read_csv("Output/panel_naissance_logement.csv", show_col_types = FALSE) %>%
  filter(couple == 1,                       # champ : COUPLES (cf. question posée)
         age_femme >= 15, age_femme <= 49,
         !is.na(surface), !is.na(cout_log), !is.na(statut3),
         !is.na(revenu), revenu > 0, !is.na(poids), poids > 0) %>%
  mutate(annee_t_num = as.integer(as.character(annee_t)),
         anciennete  = annee_t_num - emmenag,
         parite      = factor(parite, levels = c("0", "1", "2", "3+")),
         statut3     = factor(statut3, levels = c("Locataire", "Proprietaire_accedant",
                                                  "Proprietaire_non_accedant")),
         csp = factor(csp), dipl_pr = factor(dipl_pr),
         zeat = factor(zeat), annee_t = factor(annee_t),
         age_c = (age_femme - 30) / 10)

cat("Champ couples, femme 15-49 :", nrow(d0), "transitions |",
    sum(d0$naissance), "naissances\n")
cat("Ancienneté d'occupation manquante :", sum(is.na(d0$anciennete)),
    sprintf("(%.1f %%)\n", 100 * mean(is.na(d0$anciennete))))

# ── Modèle commun (couple_f retiré : champ restreint aux couples) ────────────
F_MOD <- naissance ~ surface_10 + cout_100 + parite + age_c + I(age_c^2) +
  statut3 + revenu_10k + dipl_pr + csp + nactocc + zeat + annee_t

VARS <- c(surface_10 = "Surface (+10 m2)", cout_100 = "Cout logement (+100 EUR/mois)")

# Effets marginaux moyens d'un logit : AME_k = beta_k * moyenne(p(1-p)).
# SE par delta-méthode au premier ordre (on néglige la variation de p via beta).
estimer <- function(d, etiquette) {
  d <- droplevels(d)
  if (nrow(d) < 200 || sum(d$naissance) < 30)
    return(tibble(echantillon = etiquette, n = nrow(d), naissances = sum(d$naissance),
                  variable = "—", ame_pp = NA, se_pp = NA, p_value = NA, mde_pp = NA))
  des <- svydesign(ids = ~id, weights = ~poids, data = d)
  m   <- svyglm(F_MOD, design = des, family = quasibinomial())
  p   <- fitted(m)
  fac <- mean(p * (1 - p))                       # facteur d'échelle logit -> pp
  tidy(m) %>%
    filter(term %in% names(VARS)) %>%
    transmute(echantillon = etiquette,
              n = nrow(d), naissances = sum(d$naissance),
              variable = unname(VARS[term]),
              ame_pp = 100 * estimate * fac,     # en points de pourcentage
              se_pp  = 100 * std.error * fac,
              p_value = p.value,
              mde_pp = Z_MDE * 100 * std.error * fac)
}

ech <- list(d0,
            d0 %>% filter(!is.na(anciennete), anciennete >= ANCIENNETE_MIN),
            d0 %>% filter(!is.na(anciennete), anciennete <  ANCIENNETE_MIN))
names(ech) <- c("Tous les couples",
                paste0("Non-demenageurs (>= ", ANCIENNETE_MIN, " ans)"),
                paste0("Demenageurs recents (< ", ANCIENNETE_MIN, " ans)"))

res <- imap_dfr(ech, ~ estimer(.x, .y))

taux_base <- 100 * mean(d0$naissance)
res <- res %>% mutate(mde_relatif_pct = 100 * mde_pp / taux_base)

cat("\n=== EFFETS MARGINAUX MOYENS (points de pourcentage de probabilite de naissance) ===\n")
print(as.data.frame(res %>%
  transmute(echantillon, n, naissances, variable,
            AME_pp = round(ame_pp, 3), SE = round(se_pp, 3),
            p = round(p_value, 3), MDE_pp = round(mde_pp, 3),
            MDE_en_pct_du_taux = round(mde_relatif_pct, 1))), row.names = FALSE)

cat("\nTaux de naissance de reference :", round(taux_base, 2), "%\n")
cat("MDE = ", round(Z_MDE, 3), " x SE (bilateral ", 100 * ALPHA, " %, puissance ",
    100 * PUISSANCE, " %)\n", sep = "")

# ── Test formel : l'effet de la surface diffère-t-il entre les deux groupes ? ─
#  Comparer deux échantillons scindés ne prouve rien ; c'est l'INTERACTION
#  surface x demenageur qui teste la différence.
d_int <- d0 %>% mutate(recent = factor(anciennete < ANCIENNETE_MIN,
                                       levels = c(FALSE, TRUE),
                                       labels = c("Installe", "Demenageur")))
m_int <- svyglm(update(F_MOD, . ~ . + surface_10:recent + cout_100:recent + recent),
                design = svydesign(ids = ~id, weights = ~poids, data = d_int),
                family = quasibinomial())
cat("\n=== Interaction avec le statut de demenageur recent (echelle logit) ===\n")
print(as.data.frame(tidy(m_int) %>%
  filter(str_detect(term, "surface_10|cout_100|recentDemenageur")) %>%
  transmute(term, estimate = round(estimate, 4), std.error = round(std.error, 4),
            p.value = round(p.value, 4))), row.names = FALSE)

# ── Sensibilite au seuil d'anciennete ───────────────────────────────────────
cat("\n=== Sensibilite : AME de la surface chez les non-demenageurs ===\n")
sens <- map_dfr(2:5, function(s) {
  estimer(d0 %>% filter(anciennete >= s), paste0(">= ", s, " ans")) %>%
    filter(variable == VARS[["surface_10"]])
})
print(as.data.frame(sens %>% transmute(seuil = echantillon, n, naissances,
        AME_pp = round(ame_pp, 3), SE = round(se_pp, 3), p = round(p_value, 3),
        MDE_pp = round(mde_pp, 3))), row.names = FALSE)

if (!dir.exists("Output")) dir.create("Output")
write_csv(bind_rows(res, sens), "Output/test_non_demenageurs.csv")
cat("\nExporte : Output/test_non_demenageurs.csv\n")
