# ============================================================================
#  SRCV — 51. Fluidité résidentielle : à revenu donné, la capacité de changer
#             de logement a-t-elle reculé ?
# ----------------------------------------------------------------------------
#  Hypothèse : c'est l'espace qui compte pour la fécondité (article), et
#  l'enfant passe souvent par un déménagement préalable (script 46 : après un
#  agrandissement, deux fois plus de premières naissances). Si déménager est
#  devenu plus coûteux, le prix au m² agirait indirectement, en ralentissant
#  le parcours résidentiel. Indicateurs, couples du champ (femme 15-49 ans) :
#   §1 Mobilité : part des couples installés depuis moins de deux ans (année
#      d'enquête ou précédente), par vague ; à revenu donné (quartiles de
#      niveau de vie aux seuils FIXES de 2014, euros de 2025) ; effet de la
#      vague à caractéristiques données (modèle linéaire de probabilité).
#   §2 Prime de mobilité des locataires : surloyer au m² des locataires
#      arrivés depuis moins de deux ans, à logement comparable, par vague.
#   §3 Accédants récents : m² obtenus pour 10 000 € de revenu, taux d'effort ;
#      part des couples devenus accédants récemment, à revenu donné.
#   §4 Marché : transactions DVF+ (mutations d'un seul bien) et prix au m²,
#      2014-2025, indice 2014 = 100.
#   §5 Prix local et mobilité, 2014-2025 : coefficient du prix au m² de la
#      ZEAT (DVF+), sans puis avec effets fixes de ZEAT.
#   §6 Recensement 2019 et 2023 (extraits du script 48) : couples ayant
#      changé de logement depuis le 1er janvier de l'année précédente, à âge,
#      diplôme, activité et département donnés. Le RP 2014 s'ajoutera ici.
#  Poids 2022-2023 recalés (script 47). ⚠️ SRCV change de méthode en 2022 :
#  l'écart 2018-2022 peut tenir en partie à la mesure ; comparer les vagues
#  2022-2025 entre elles et au recensement.
#  Sortie : Output/fluidite_residentielle.csv
# ============================================================================

source("R/00_prepa_fecondite.R")
library(survey)
suppressPackageStartupMessages(library(fixest))
setFixest_notes(FALSE)
activer_cache_prepa()
IPC <- charger_ipc()

pc <- read_csv("Output/poids_corriges_2022_2023.csv", show_col_types = FALSE,
               col_types = cols(annee = col_character(), id = col_character(), .default = col_double())) %>%
  select(annee, id_n = id, poids_corrige)
df <- bind_rows(millesimes_prepares(restreindre = TRUE)) %>%
  mutate(annee_num = as.integer(annee), id_n = normaliser_id(id),
         anc = annee_num - datent, mobile = as.integer(anc <= 1),
         niveau_vie_reel = deflater(niveau_vie, annee_num - 1, IPC, base = 2025),
         revenu_reel = deflater(revenu_hp, annee_num - 1, IPC, base = 2025),
         cout_m2_reel = deflater(cout_m2, annee_num, IPC, base = 2025)) %>%
  left_join(pc, by = c("annee", "id_n")) %>%
  mutate(poids = coalesce(poids_corrige, poids)) %>%
  filter(!is.na(anc), anc >= 0, !is.na(niveau_vie_reel), !is.na(poids), poids > 0)
seuils <- with(filter(df, annee == "2014"), wquantile(niveau_vie_reel, poids, c(.25, .5, .75)))
df <- df %>% mutate(q_fixe = cut(niveau_vie_reel, c(-Inf, seuils, Inf), labels = paste0("Q", 1:4)),
                    age_cl = cut(age_femme, c(14, 29, 34, 39, 49), labels = c("15-29", "30-34", "35-39", "40-49")),
                    enfants = factor(pmin(nb_enf_avant, 3)), annee_f = relevel(factor(annee), "2014"))
cat("Couples :", nrow(df), "| seuils de quartile fixes (niveau de vie 2014, € de 2025) :",
    paste(round(seuils), collapse = " / "), "\n")
des <- function(d) svydesign(ids = ~id_cluster, weights = ~poids, data = droplevels(d))
coef_vagues <- function(m, motif = "^annee_f") {
  co <- as.data.frame(summary(m)$coefficients) %>% rownames_to_column("terme") %>%
    filter(grepl(motif, terme))
  tibble(vague = sub(motif, "", co$terme), effet_pts = 100 * co[[2]], se = 100 * co[[3]], p = co[[5]])
}

# ── §1 Mobilité ─────────────────────────────────────────────────────────────
taux <- function(d, ...) d %>% group_by(annee, ...) %>%
  summarise(n = n(), mobiles_pct = 100 * weighted.mean(mobile, poids), .groups = "drop")
mobilite <- bind_rows(taux(df) %>% mutate(groupe = "Ensemble"),
                      taux(df, q_fixe) %>% mutate(groupe = paste("Niveau de vie", q_fixe)) %>% select(-q_fixe),
                      taux(df, age_cl) %>% mutate(groupe = paste("Femme", age_cl, "ans")) %>% select(-age_cl),
                      taux(df, statut3) %>% mutate(groupe = paste("Statut actuel :", statut3)) %>% select(-statut3),
                      taux(filter(df, nb_enf_avant == 0)) %>% mutate(groupe = "Couples sans enfant"))
cat("\n§1 Part des couples installés depuis moins de deux ans (%) :\n")
print(as.data.frame(mobilite %>% select(groupe, annee, mobiles_pct) %>% mutate(mobiles_pct = round(mobiles_pct, 1)) %>%
  pivot_wider(names_from = annee, values_from = mobiles_pct)), row.names = FALSE)

#  Ni ZEAT ni degré d'urbanisation : en 2010 et 2014, la ZEAT manque surtout
#  pour les ménages qui viennent d'emménager (42 % de mobiles contre 4 %), et
#  le degré d'urbanisation n'est pas comparable d'une vague à l'autre.
F_MOB <- mobile ~ annee_f + q_fixe + age_femme + I(age_femme^2) + enfants + naiss01
effet_vague <- bind_rows(
  coef_vagues(svyglm(F_MOB, design = des(df))) %>% mutate(groupe = "Ensemble"),
  map_dfr(levels(df$q_fixe), function(q) {
    d <- filter(df, q_fixe == q)
    coef_vagues(svyglm(update(F_MOB, . ~ . - q_fixe), design = des(d))) %>% mutate(groupe = paste("Niveau de vie", q))
  }),
  coef_vagues(svyglm(update(F_MOB, . ~ . - enfants - naiss01), design = des(filter(df, nb_enf_avant == 0)))) %>%
    mutate(groupe = "Couples sans enfant"))
cat("\n§1 Écart de mobilité avec 2014, à revenu et caractéristiques donnés (points) :\n")
print(as.data.frame(effet_vague %>% mutate(lib = sprintf("%+.1f (%.1f)", effet_pts, se)) %>%
  select(groupe, vague, lib) %>% pivot_wider(names_from = vague, values_from = lib)), row.names = FALSE)

# ── §2 Prime de mobilité des locataires ─────────────────────────────────────
loc <- df %>% filter(statut3 == "Locataire", cout_m2_reel > 0, !is.na(surface), !is.na(pieces),
                     !is.na(maison_appart)) %>% mutate(lc = log(cout_m2_reel), ls = log(surface))
prime <- map_dfr(sort(unique(loc$annee)), function(an) {
  d <- filter(loc, annee == an)
  m <- svyglm(lc ~ mobile + ls + factor(pmin(pieces, 5)) + zeat + urbain + maison_appart, design = des(d))
  b <- coef(summary(m))["mobile", ]
  tibble(annee = an, n = nrow(d), arrives_recents = sum(d$mobile),
         prime_pct = 100 * (exp(b[1]) - 1),
         ic_bas = 100 * (exp(b[1] - 1.96 * b[2]) - 1), ic_haut = 100 * (exp(b[1] + 1.96 * b[2]) - 1))
})
cat("\n§2 Surloyer au m² des locataires arrivés depuis moins de deux ans, à logement comparable (%) :\n")
print(as.data.frame(prime %>% mutate(across(where(is.double), ~ round(.x, 1)))), row.names = FALSE)
# Loyer au m² (euros de 2025) et taux d'effort des nouveaux locataires et des locataires en place
loyers <- loc %>% filter(revenu_reel > 0) %>%
  mutate(arrivee = if_else(mobile == 1, "Arrives depuis moins de 2 ans", "En place depuis 2 ans ou plus")) %>%
  group_by(annee, arrivee) %>%
  summarise(n = n(), loyer_m2_median = wquantile(cout_m2_reel, poids, .5),
            surface_mediane = wquantile(surface, poids, .5),
            effort_median_pct = wquantile(100 * cout_m2_reel * surface * 12 / revenu_reel, poids, .5), .groups = "drop")
cat("\n§2 bis Locataires : loyer au m² (€ de 2025 par mois), surface, taux d'effort médians :\n")
print(as.data.frame(loyers %>% mutate(across(where(is.double), ~ round(.x, 1)))), row.names = FALSE)

# ── §3 Accédants récents ────────────────────────────────────────────────────
acc <- df %>% filter(statut3 == "Proprietaire_accedant", mobile == 1, revenu_reel > 0, !is.na(surface))
accedants <- acc %>% group_by(annee) %>%
  summarise(n = n(), revenu_median = wquantile(revenu_reel, poids, .5),
            surface_mediane = wquantile(surface, poids, .5),
            m2_pour_10k_euros = wquantile(surface / (revenu_reel / 10000), poids, .5),
            taux_effort_median_pct = wquantile(taux_effort_pct, poids, .5), .groups = "drop")
accession <- df %>% group_by(annee, q_fixe) %>%
  summarise(accession_recente_pct = 100 * weighted.mean(mobile == 1 & statut3 == "Proprietaire_accedant", poids),
            .groups = "drop")
cat("\n§3 Accédants arrivés depuis moins de deux ans :\n")
print(as.data.frame(accedants %>% mutate(across(where(is.double), ~ round(.x, 1)))), row.names = FALSE)
cat("\nPart des couples devenus accédants depuis moins de deux ans, par quartile fixe (%) :\n")
print(as.data.frame(accession %>% mutate(accession_recente_pct = round(accession_recente_pct, 1)) %>%
  pivot_wider(names_from = annee, values_from = accession_recente_pct)), row.names = FALSE)

# ── §4 Marché : DVF+ ────────────────────────────────────────────────────────
dvf <- read_csv("Output/dvf_zeat_annee.csv", show_col_types = FALSE)
marche <- dvf %>% group_by(annee, type_bien) %>%
  summarise(transactions = sum(n), prix_m2 = weighted.mean(prix_m2_median, n), .groups = "drop") %>%
  group_by(type_bien) %>%
  mutate(indice_transactions = 100 * transactions / transactions[annee == 2014],
         indice_prix_m2 = 100 * prix_m2 / prix_m2[annee == 2014]) %>% ungroup()
cat("\n§4 Marché (DVF+, mutations d'un seul bien, hors Alsace-Moselle) :\n")
print(as.data.frame(marche %>% mutate(across(where(is.double), ~ round(.x)))), row.names = FALSE)

# ── §5 Prix local et mobilité ───────────────────────────────────────────────
prix_zeat <- dvf %>% group_by(zeat = as.character(zeat), annee = as.character(annee)) %>%
  summarise(prix_k = weighted.mean(prix_m2_median, n) / 1000, .groups = "drop")
#  2014 exclu : la ZEAT y manque surtout pour les ménages mobiles (voir §1).
d5 <- df %>% filter(annee_num >= 2018) %>%
  mutate(zeat_code = recode(as.character(zeat), "Region parisienne" = "1", "Bassin parisien" = "2", "Nord" = "3",
                            "Est" = "4", "Ouest" = "5", "Sud-Ouest" = "7", "Centre-Est" = "8",
                            "Mediterranee" = "9", .default = NA_character_)) %>%
  inner_join(prix_zeat, by = c("zeat_code" = "zeat", "annee"))
F5 <- mobile ~ prix_k + q_fixe + age_femme + I(age_femme^2) + enfants + naiss01 + urbain + annee_f
prix_mobilite <- imap_dfr(list("Tous les couples" = d5,
                               "Couples sans enfant" = filter(d5, nb_enf_avant == 0),
                               "Locataires" = filter(d5, statut3 == "Locataire")), function(d, lib) {
  f <- if (lib == "Couples sans enfant") update(F5, . ~ . - enfants - naiss01) else F5
  bind_rows(coef_vagues(svyglm(f, design = des(d)), "^prix_k") %>% mutate(specification = "Sans effets fixes de ZEAT"),
            coef_vagues(svyglm(update(f, . ~ . + zeat_code), design = des(d)), "^prix_k") %>%
              mutate(specification = "Avec effets fixes de ZEAT")) %>%
    mutate(echantillon = lib, n = nrow(d), .before = 1) %>% select(-vague)
})
cat("\n§5 Effet de +1 000 €/m² du prix local sur la mobilité (points) :\n")
print(as.data.frame(prix_mobilite %>% mutate(across(where(is.double), ~ round(.x, 3)))), row.names = FALSE)

# ── §6 Recensement : mobilité sur un an ─────────────────────────────────────
rp <- map_dfr(c("2019", "2023"), ~ readRDS(file.path(DIR_RP, "_extrait", paste0("couples_RP", .x, ".rds")))) %>%
  filter(metrodom == "M", iran %in% as.character(1:9)) %>%
  mutate(mobile_an = as.integer(iran != "1"), cantville = sub(" .*", "", hh),
         cs6 = if_else(cs %in% as.character(1:6), cs, "Sans"), enfants = factor(pmin(nb_enf_avant, 3)))
rp_desc <- rp %>% group_by(annee, diplome) %>%
  summarise(n = n(), mobiles_an_pct = 100 * weighted.mean(mobile_an, poids), .groups = "drop")
m_rp <- feols(mobile_an ~ i(annee, ref = "2019") + diplome + tact + tact_conj + cs6 + enfants + naiss01 |
                age_femme + dept, data = rp, weights = ~poids, cluster = ~cantville)
rp_effet <- as.data.frame(coeftable(m_rp)) %>% rownames_to_column("terme") %>% filter(grepl("annee", terme)) %>%
  transmute(terme, effet_pts = 100 * Estimate, se = 100 * `Std. Error`, n = nobs(m_rp))
cat("\n§6 Recensement : couples ayant changé de logement depuis le 1er janvier N-1 (%), par diplôme :\n")
print(as.data.frame(rp_desc %>% select(-n) %>% mutate(mobiles_an_pct = round(mobiles_an_pct, 1)) %>%
  pivot_wider(names_from = annee, values_from = mobiles_an_pct)), row.names = FALSE)
cat("Écart 2023 - 2019 à caractéristiques données (points) :\n"); print(rp_effet)

ecrire(bind_rows(mobilite %>% mutate(bloc = "Mobilite (installes < 2 ans)", q_fixe = NULL, .before = 1),
                 effet_vague %>% mutate(bloc = "Ecart avec 2014 a caracteristiques donnees", .before = 1),
                 prime %>% mutate(bloc = "Prime de mobilite des locataires", .before = 1),
                 loyers %>% mutate(bloc = "Loyers et effort des locataires", .before = 1),
                 accedants %>% mutate(bloc = "Accedants recents", .before = 1),
                 accession %>% mutate(bloc = "Accession recente par quartile fixe", q_fixe = as.character(q_fixe), .before = 1),
                 marche %>% mutate(bloc = "Marche DVF+", annee = as.character(annee), .before = 1),
                 prix_mobilite %>% mutate(bloc = "Prix local et mobilite", .before = 1),
                 rp_desc %>% mutate(bloc = "Recensement : mobilite sur un an", diplome = as.character(diplome), .before = 1),
                 rp_effet %>% mutate(bloc = "Recensement : ecart 2023-2019", .before = 1)),
       "Output/fluidite_residentielle.csv")
