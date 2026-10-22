# ============================================================================
#  SRCV — 47. Repondérer les vagues 2022 et 2023 plutôt que les écarter
# ----------------------------------------------------------------------------
#  Dans ces deux vagues, les ménages avec nouveau-né portent des poids
#  anormalement faibles (annexe C de l'étude) : les statistiques pondérées les
#  écartent. On recale ici leurs poids (calage par raking, package survey)
#  sur deux types de marges :
#   - la STRUCTURE des ménages telle que la donnent les poids d'origine
#     (taille du ménage, âge de la personne de référence, statut d'occupation,
#     ZEAT) : le calage ne la modifie pas ;
#   - le NOMBRE d'enfants nés l'année précédant l'enquête, présents dans les
#     ménages, recalé sur l'état civil (Insee) : naissances de l'année N-1
#     multipliées par le rapport observé dans les vagues saines (2024, 2025)
#     entre ce compte pondéré et l'état civil — ce rapport absorbe les
#     différences de champ (Mayotte, ménages ordinaires, décès infantiles).
#  Contrôles : rapport entre le poids moyen des ménages avec nouveau-né et
#  celui des autres (1,21 à 1,30 de 2006 à 2018), et taux de naissance pondéré
#  des couples du champ, avant et après.
#  Sorties : Output/reponderation_2022_2023.csv (contrôles),
#            Output/poids_corriges_2022_2023.csv (annee, id, poids corrigé)
# ============================================================================

source("R/00_utils.R")
source("R/00_config.R")
library(survey)

# Naissances de l'état civil, France entière (Insee)
NAISSANCES <- c("2021" = 742052, "2022" = 725997, "2023" = 677803, "2024" = 660787)
ANS <- c("2022", "2023", "2024", "2025")

menages <- map_dfr(ANS, function(an) {
  m <- charger_menages(an, VAGUES[[an]])
  i <- lire_srcv(VAGUES[[an]]$ind); names(i) <- toupper(names(i))
  bebes <- tibble(id = cle_menage_ind(i), anais = num(i$ANAIS)) %>%
    group_by(id) %>%
    summarise(nes_n1 = sum(anais == as.integer(an) - 1, na.rm = TRUE),
              nes_n_n1 = sum(anais >= as.integer(an) - 1, na.rm = TRUE), .groups = "drop")
  m %>% mutate(id = normaliser_id(id)) %>% left_join(bebes, by = "id") %>%
    mutate(nes_n1 = coalesce(nes_n1, 0L), nes_n_n1 = coalesce(nes_n_n1, 0L),
           taille_cl = factor(pmin(taille, 5)),
           age_cl = cut(age_pr, c(-Inf, 29, 39, 49, 64, Inf),
                        labels = c("<30", "30-39", "40-49", "50-64", "65+")),
           statut3 = factor(coalesce(as.character(statut3), "Inconnu")),
           zeat = factor(zeat))
}) %>% filter(!is.na(age_cl), !is.na(taille_cl))

# Rapport compte pondéré / état civil dans les vagues saines
couverture <- menages %>% group_by(annee) %>%
  summarise(enfants_nes_n1 = sum(poids * nes_n1), .groups = "drop") %>%
  mutate(etat_civil = NAISSANCES[as.character(as.integer(annee) - 1)],
         rapport = enfants_nes_n1 / etat_civil)
cat("Enfants nés l'année N-1, compte pondéré et état civil :\n"); print(as.data.frame(couverture))
r_sain <- mean(couverture$rapport[couverture$annee %in% c("2024", "2025")])

# Calage des vagues 2022 et 2023
caler <- function(an) {
  d <- filter(menages, annee == an)
  des <- svydesign(ids = ~1, weights = ~poids, data = d)
  marges <- function(v) { t <- tapply(d$poids, d[[v]], sum); t[t > 0] }
  f <- ~ taille_cl + age_cl + statut3 + zeat + nes_n1
  X <- model.matrix(f, d)
  cible <- colSums(X * d$poids)
  cible["nes_n1"] <- NAISSANCES[as.character(as.integer(an) - 1)] * r_sain
  cal <- calibrate(des, f, population = cible, calfun = "raking", maxit = 200)
  d %>% mutate(poids_corrige = weights(cal))
}
corriges <- bind_rows(caler("2022"), caler("2023"),
                      menages %>% filter(annee %in% c("2024", "2025")) %>% mutate(poids_corrige = poids))

controle <- corriges %>% group_by(annee) %>%
  summarise(rapport_poids_avant = mean(poids[nes_n_n1 > 0]) / mean(poids[nes_n_n1 == 0]),
            rapport_poids_apres = mean(poids_corrige[nes_n_n1 > 0]) / mean(poids_corrige[nes_n_n1 == 0]),
            enfants_nes_n1_avant = sum(poids * nes_n1), enfants_nes_n1_apres = sum(poids_corrige * nes_n1),
            menages_avant = sum(poids), menages_apres = sum(poids_corrige),
            facteur_max = max(poids_corrige / poids), facteur_min = min(poids_corrige / poids),
            .groups = "drop") %>%
  mutate(across(where(is.double), ~ round(.x, 3)))
cat("\nContrôle du calage :\n"); print(as.data.frame(controle))

# Taux de naissance pondéré des couples du champ (femme de 15 à 49 ans)
source("R/00_prepa_fecondite.R"); activer_cache_prepa()
couples <- bind_rows(millesimes_prepares(restreindre = TRUE)[ANS]) %>%
  mutate(id = normaliser_id(id), naiss01 = as.integer(naissance == "Oui")) %>%
  left_join(corriges %>% select(annee, id, poids_corrige), by = c("annee", "id"))
taux <- couples %>% filter(!is.na(poids_corrige)) %>% group_by(annee) %>%
  summarise(n = n(), taux_brut = 100 * mean(naiss01),
            taux_poids_origine = 100 * weighted.mean(naiss01, poids),
            taux_poids_corriges = 100 * weighted.mean(naiss01, poids_corrige), .groups = "drop") %>%
  mutate(across(where(is.double), ~ round(.x, 2)))
cat("\nTaux de naissance des couples du champ (%) :\n"); print(as.data.frame(taux))

write_csv(bind_rows(controle %>% mutate(bloc = "Controle du calage", .before = 1),
                    taux %>% mutate(bloc = "Taux de naissance des couples", .before = 1),
                    couverture %>% mutate(bloc = "Couverture des naissances", .before = 1)),
          "Output/reponderation_2022_2023.csv")
write_csv(corriges %>% filter(annee %in% c("2022", "2023")) %>% select(annee, id, poids, poids_corrige),
          "Output/poids_corriges_2022_2023.csv")
