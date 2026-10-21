# ============================================================================
#  SRCV — 33. Un effet de composition géographique ?
# ----------------------------------------------------------------------------
#  Hypothèse : les jeunes couples se déplacent vers les grandes villes, où
#  l'espace est plus contraint et la natalité plus faible ; la natalité
#  agrégée baisse alors par composition, sans que le désir d'enfant change.
#
#  1. Répartition pondérée des couples (femme 15-49 ans) et des couples sans
#     enfant par tranche d'unité urbaine, vagues 2014, 2018, 2024 et 2025.
#  2. Taux de naissance par tranche (toutes naissances ; premières naissances
#     chez les couples sans enfant), par période de mesure de la naissance.
#  3. Effet de composition (shift-share) : variation relative du taux agrégé
#     que produit, à taux par tranche fixés, le passage de la répartition
#     2014 à la répartition 2025 ; brut, puis à structure par âge donnée
#     (répartition géographique comparée à l'intérieur de chaque classe
#     d'âge de la femme). Rapportée à la baisse de l'ICF 2014-2025.
#
#  ⚠️ 2022 et 2023 écartées (anomalie de pondération). Zonage tuu10 (2014,
#     2018) puis TUU2017 (2024, 2025) : une partie du glissement vers
#     l'urbain peut tenir au reclassement de communes entre les deux.
#  Sortie : Output/composition_geographique.csv
# ============================================================================

source("R/00_prepa_fecondite.R")
activer_cache_prepa()

preparer <- function(x) x %>%
  filter(annee %in% c("2014", "2018", "2024", "2025"), !is.na(densite),
         !is.na(poids), poids > 0, !is.na(age_femme), age_femme >= 15, age_femme <= 49) %>%
  mutate(densite = factor(as.character(densite), levels = DENSITE_LIB),
         naiss01 = as.integer(naissance == "Oui"),
         sans_enfant = nb_enf_avant == 0,
         en_couple = couple == "En_couple",
         periode = if_else(annee %in% c("2014", "2018"), "2014-2018", "2024-2025"),
         age_cl = cut(age_femme, c(14, 24, 29, 34, 39, 49),
                      labels = c("15-24", "25-29", "30-34", "35-39", "40-49")))
d  <- preparer(bind_rows(millesimes_prepares(restreindre = TRUE)))    # couples
df <- preparer(bind_rows(millesimes_prepares(restreindre = FALSE)))   # toutes les femmes PR/CJ

# Le champ des couples ignore la mise en couple : si les grandes villes la
# retardent, l'effet de composition passe par là. Le troisième champ prend
# toutes les femmes de 15 à 49 ans personnes de référence ou conjointes, en
# couple ou non (les femmes vivant chez leurs parents restent hors champ).
champs <- list("Tous les couples, toutes naissances" = function() d,
               "Couples sans enfant, premieres naissances" = function() filter(d, sans_enfant),
               "Toutes les femmes PR ou conjointes, toutes naissances" = function() df)

part <- function(x, ...) x %>% group_by(...) %>% summarise(w = sum(poids), .groups = "drop")

res <- list()
res[[length(res) + 1]] <- df %>% group_by(annee, densite) %>%
  summarise(valeur = 100 * weighted.mean(en_couple, poids, na.rm = TRUE), .groups = "drop") %>%
  transmute(champ = "Toutes les femmes PR ou conjointes", bloc = "0. Part en couple (%)",
            cle = as.character(densite), colonne = annee, valeur)

for (ch in names(champs)) {
  x <- champs[[ch]]()

  # 1. Répartition par tranche, par vague
  rep_ <- part(x, annee, densite) %>% group_by(annee) %>%
    mutate(valeur = 100 * w / sum(w)) %>% ungroup()
  res[[length(res) + 1]] <- rep_ %>%
    transmute(champ = ch, bloc = "1. Repartition (%)", cle = as.character(densite),
              colonne = annee, valeur)

  # 2. Taux de naissance par tranche et période
  taux <- x %>% group_by(periode, densite) %>%
    summarise(valeur = 100 * weighted.mean(naiss01, poids), n = n(), .groups = "drop")
  res[[length(res) + 1]] <- taux %>%
    transmute(champ = ch, bloc = "2. Taux de naissance (%)", cle = as.character(densite),
              colonne = periode, valeur)

  # 3. Shift-share brut et à structure par âge donnée, taux de chaque période
  s14 <- rep_ %>% filter(annee == "2014") %>% select(densite, s14 = valeur)
  s25 <- rep_ %>% filter(annee == "2025") %>% select(densite, s25 = valeur)
  # Répartition géographique à l'intérieur de chaque classe d'âge
  sa <- part(x %>% filter(annee %in% c("2014", "2025")), annee, age_cl, densite) %>%
    group_by(annee, age_cl) %>% mutate(s = w / sum(w)) %>% ungroup() %>%
    select(-w) %>% tidyr::pivot_wider(names_from = annee, values_from = s, names_prefix = "s",
                                      values_fill = 0)
  poids_age <- part(x %>% filter(annee == "2025"), age_cl) %>% mutate(pa = w / sum(w)) %>%
    select(age_cl, pa)
  for (per in c("2014-2018", "2024-2025")) {
    r <- taux %>% filter(periode == per) %>% select(densite, r = valeur)
    b <- s14 %>% inner_join(s25, by = "densite") %>% inner_join(r, by = "densite")
    brut <- 100 * (sum(b$s25 * b$r) / sum(b$s14 * b$r) - 1)
    ra <- x %>% filter(periode == per) %>% group_by(age_cl, densite) %>%
      summarise(r = weighted.mean(naiss01, poids), .groups = "drop")
    a <- sa %>% inner_join(ra, by = c("age_cl", "densite")) %>%
      group_by(age_cl) %>% summarise(t25 = sum(s2025 * r), t14 = sum(s2014 * r), .groups = "drop") %>%
      inner_join(poids_age, by = "age_cl")
    age <- 100 * (sum(a$pa * a$t25) / sum(a$pa * a$t14) - 1)
    baisse_icf <- 100 * (ICF_2025 / ICF_2014 - 1)
    res[[length(res) + 1]] <- tibble(
      champ = ch, bloc = "3. Effet de composition 2014 -> 2025", colonne = paste("taux", per),
      cle = c("Variation relative du taux agrege, brute (%)",
              "Variation relative du taux agrege, a structure par age donnee (%)",
              "Part de la baisse de l'ICF, brute (%)",
              "Part de la baisse de l'ICF, a structure par age donnee (%)"),
      valeur = c(brut, age, 100 * brut / baisse_icf, 100 * age / baisse_icf))
  }
}

sortie <- bind_rows(res) %>% mutate(valeur = round(valeur, 2))
print(as.data.frame(sortie %>% tidyr::pivot_wider(names_from = colonne, values_from = valeur)),
      row.names = FALSE)
write_csv(sortie, "Output/composition_geographique.csv")
cat("\nExporte : Output/composition_geographique.csv\n")
