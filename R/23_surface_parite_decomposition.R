# ============================================================================
#  SRCV — 23. SURFACE, RANG DE NAISSANCE, STATUT D'OCCUPATION
#            et DÉCOMPOSITION DE LA BAISSE DE NATALITÉ 2010-2025
# ----------------------------------------------------------------------------
#  Ce script déplace la focale du TAUX D'EFFORT (02a/02b, résultat nul) vers
#  la QUANTITÉ de logement (surface) et le STATUT d'occupation, et répond à
#  quatre questions :
#
#   §2  L'effet de la surface diffère-t-il pour le PREMIER enfant et pour les
#       rangs suivants ?
#   §3  Le statut de locataire pèse-t-il, une fois l'ÂGE contrôlé ? Et si un
#       effet propriétaire subsiste, est-ce un effet REVENU sous-jacent ?
#       -> escalier de contrôles, l'effet est lu après chaque ajout.
#   §4  L'effet est-il CONTINU selon la géographie (ZEAT) et le degré
#       d'urbanisation (tranche d'unité urbaine) ?
#   §5  Quelle PART de la baisse de la natalité 2010-2025 peut être imputée au
#       logement ? Contrefactuel : on donne aux ménages de 2025 les conditions
#       de logement de 2010 (surface, prix au m², répartition des statuts) et
#       on re-prédit. Trois jeux d'élasticités : association (borne haute),
#       logement prédéterminé (estimation retenue), MDE (borne de puissance).
#
#  À LANCER DEPUIS LA RACINE DU PROJET :
#     source("R/23_surface_parite_decomposition.R")
#
#  Sorties : natalite_par_rang.csv, escalier_statut.csv, gradient_geo.csv,
#            decomposition_natalite.csv
# ============================================================================

source("R/00_prepa_fecondite.R")
library(survey); library(broom); library(marginaleffects)
activer_cache_prepa()

IPC <- charger_ipc()
if (!dir.exists("Output")) dir.create("Output")

# ── Repères externes : la baisse de natalité à expliquer ────────────────────
#  SRCV ne peut PAS servir à mesurer cette baisse : la naissance est lue dans
#  le module "événements" jusqu'en 2018 et RECONSTRUITE depuis le fichier
#  individus à partir de 2022 (fenêtre de deux millésimes). L'écart de niveau
#  entre 2010 et 2025 mêlerait évolution réelle et changement de mesure.
#  On ancre donc le dénominateur sur l'indicateur conjoncturel de fécondité
#  publié par l'Insee, et on n'utilise SRCV que pour les ÉLASTICITÉS et pour
#  l'ÉVOLUTION DES CONDITIONS DE LOGEMENT (mesurées, elles, à l'identique).
#  ICF_2010 et ICF_2025 sont définis dans 00_config.R (source unique, avec la
#  référence Insee). Le 1,58 utilisé dans une première version pour 2025 n'était
#  pas vérifié : l'Insee publie 1,56 (bilan démographique 2025, février 2026).
BAISSE_RELATIVE <- (ICF_2025 - ICF_2010) / ICF_2010          # ~ -22,8 %

# ============================================================================
#  §0 — DONNÉES
# ============================================================================
DONNEES_FEC <- millesimes_prepares(restreindre = TRUE)   # couples, femme 15-49

df <- bind_rows(imap(DONNEES_FEC, ~ .x %>% select(any_of(c(VARS_INTERET, "datent"))))) %>%
  mutate(
    annee_num       = as.integer(annee),
    naiss01         = as.integer(naissance == "Oui"),
    cout_m2_reel    = deflater(cout_m2,    annee_num,     IPC, base = 2025),
    revenu_reel     = deflater(revenu,     annee_num - 1, IPC, base = 2025),
    niveau_vie_reel = deflater(niveau_vie, annee_num - 1, IPC, base = 2025),
    surface_10      = surface / 10,
    nv_10k          = niveau_vie_reel / 10000,
    anciennete      = annee_num - datent,
    # RANG : le contrôle doit être PRÉ-naissance (nb_enf_avant), sinon une
    # naissance implique mécaniquement au moins un enfant -> bad control.
    rang            = factor(if_else(nb_enf_avant == 0, "1er enfant", "Rang 2+"),
                             levels = c("1er enfant", "Rang 2+")),
    # Les facteurs conservés en mémoire gardent leurs niveaux vides : on refait
    # l'aller-retour caractère -> facteur, comme dans les fichiers compacts.
    across(where(is.factor), ~ factor(as.character(.))),
    statut3 = factor(statut3, levels = c("Locataire", "Proprietaire_accedant",
                                         "Proprietaire_non_accedant")),
    rang    = factor(rang, levels = c("1er enfant", "Rang 2+"))) %>%
  filter(!is.na(surface), !is.na(cout_m2_reel), is.finite(cout_m2_reel),
         !is.na(statut3), !is.na(age_femme), !is.na(niveau_vie_reel),
         !is.na(poids), poids > 0)

cat("Champ : couples, femme", AGE_MIN, "-", AGE_MAX, "ans |",
    nrow(df), "menages x millesimes |", sum(df$naiss01), "naissances\n")
cat("Repartition par rang :\n"); print(table(df$rang, df$naissance))

des <- function(d) svydesign(ids = ~id_cluster, weights = ~poids, data = droplevels(d))
# Sur les sous-champs, certains facteurs deviennent constants — `demenage` est
# forcément "Non" chez les ménages installés depuis 3 ans. On retire ces termes
# de la formule plutôt que de laisser model.matrix() échouer.
elaguer <- function(f, d) {
  vars <- setdiff(all.vars(f), all.vars(f[[2]]))
  mono <- vars[vapply(vars, function(v) v %in% names(d) &&
                        is.factor(d[[v]]) && nlevels(droplevels(d[[v]])) < 2, logical(1))]
  if (length(mono)) f <- update(f, paste(". ~ . -", paste(mono, collapse = " - ")))
  f
}
ame <- function(m, vars, ...) as_tibble(avg_slopes(m, variables = vars,
                                                   wts = m$prior.weights, ...))
# Les effets marginaux sont convertis en POINTS de pourcentage.
en_points <- function(x) x %>% mutate(across(any_of(c("estimate", "std.error",
                                                      "conf.low", "conf.high")),
                                             ~ 100 * .x))

# ============================================================================
#  §1 — FAITS : surface, statut et taux de naissance selon le rang
# ----------------------------------------------------------------------------
#  2022 et 2023 sont écartées des TAUX pondérés (anomalie de pondération
#  documentée dans R/17-18 : les ménages avec nouveau-né y portent un poids
#  anormalement faible). Elles restent dans les MODÈLES, où la distorsion est
#  absorbée par les effets fixes d'année.
# ============================================================================
cat("\n\n########## §1 — FAITS DESCRIPTIFS ##########\n")

VAGUES_TAUX <- setdiff(names(DONNEES_FEC), c("2022", "2023"))

faits <- df %>% filter(annee %in% VAGUES_TAUX) %>%
  group_by(annee, rang, statut3) %>%
  summarise(n = n(),
            taux_naiss_pct = 100 * weighted.mean(naiss01, poids),
            surface_moy    = weighted.mean(surface, poids),
            surf_pers_moy  = weighted.mean(surf_pers, poids, na.rm = TRUE),
            cout_m2_reel   = weighted.mean(cout_m2_reel, poids),
            nv_k           = weighted.mean(niveau_vie_reel, poids) / 1000,
            age_moy        = weighted.mean(age_femme, poids),
            .groups = "drop") %>%
  mutate(across(where(is.numeric), ~ round(.x, 2)))
print(as.data.frame(faits), row.names = FALSE)
write_csv(faits, "Output/faits_par_rang.csv")

# Taux de naissance par quartile de surface (pondéré), séparément par rang.
quartiles_surface <- df %>% filter(annee %in% VAGUES_TAUX) %>%
  group_by(rang) %>%
  mutate(q = cut(surface, c(-Inf, wquantile(surface, poids, c(.25, .5, .75)), Inf),
                 labels = c("Q1", "Q2", "Q3", "Q4"))) %>%
  group_by(rang, q) %>%
  summarise(n = n(), surface_med = round(wquantile(surface, poids, .5)),
            taux_naiss_pct = round(100 * weighted.mean(naiss01, poids), 2),
            age_moy = round(weighted.mean(age_femme, poids), 1),
            nv_k = round(weighted.mean(niveau_vie_reel, poids) / 1000, 1),
            part_locataire_pct = round(100 * weighted.mean(statut3 == "Locataire", poids), 1),
            .groups = "drop")
cat("\nTaux de naissance par quartile de surface :\n")
print(as.data.frame(quartiles_surface), row.names = FALSE)
write_csv(quartiles_surface, "Output/quartiles_surface.csv")

# ============================================================================
#  §2 — EFFET DE LA SURFACE PAR RANG DE NAISSANCE
# ----------------------------------------------------------------------------
#  Décomposition prix x quantité : à revenu et prix au m² donnés, `surface_10`
#  mesure la QUANTITÉ de logement consommée.
#  Le modèle du 1er enfant exclut `nb_enfants` (constant par construction) et
#  `taille_avant` (quasi constant : 2 personnes).
# ============================================================================
cat("\n\n########## §2 — SURFACE ET RANG DE NAISSANCE ##########\n")

F_BASE   <- naiss01 ~ surface_10 + cout_m2_reel + nv_10k + statut3 +
  age_c + I(age_c^2) + demenage + csp + urbain + zeat + factor(annee)
F_POOLE  <- update(F_BASE, . ~ . + nb_enfants + taille_avant)
F_RANG1  <- F_BASE
F_RANG2  <- update(F_BASE, . ~ . + nb_enfants + taille_avant)

VARS_LOG <- c("surface_10", "cout_m2_reel", "nv_10k")

estimer_rang <- function(d, f, lib) {
  d <- droplevels(d)
  m <- svyglm(elaguer(f, d), design = des(d), family = quasibinomial(link = "probit"))
  ame(m, VARS_LOG) %>% en_points() %>%
    transmute(echantillon = lib, n = nrow(d), naissances = sum(d$naiss01),
              taux_base_pct = 100 * weighted.mean(d$naiss01, d$poids),
              variable = term, ame_pts = estimate, se = std.error,
              conf.low, conf.high, p.value)
}

res_rang <- bind_rows(
  estimer_rang(df, F_POOLE, "Ensemble"),
  estimer_rang(filter(df, rang == "1er enfant"), F_RANG1, "1er enfant"),
  estimer_rang(filter(df, rang == "Rang 2+"),    F_RANG2, "Rang 2 et plus"))

cat("\nEffets marginaux moyens (points de %) :\n")
print(as.data.frame(res_rang %>% mutate(across(where(is.numeric), ~ round(.x, 4)))),
      row.names = FALSE)

# ── Test formel de la différence entre rangs : interaction ─────────────────
m_int <- svyglm(update(F_POOLE, . ~ . + surface_10:rang + statut3:rang),
                design = des(df), family = quasibinomial(link = "probit"))
cat("\nInteraction surface x rang (coefficients du modele) :\n")
print(as.data.frame(tidy(m_int) %>% filter(str_detect(term, "rang")) %>%
        mutate(across(where(is.numeric), ~ round(.x, 4)))), row.names = FALSE)
cat("\nEffet marginal de la surface PAR RANG (modele unique avec interaction) :\n")
ame_par_rang <- ame(m_int, "surface_10", by = "rang") %>% en_points()
print(as.data.frame(ame_par_rang %>% mutate(across(where(is.numeric), ~ round(.x, 4)))),
      row.names = FALSE)
cat("\nTest de Wald sur l'interaction surface x rang :\n")
print(regTermTest(m_int, ~ surface_10:rang))

# ── Logement PRÉDÉTERMINÉ : le même exercice chez les non-déménageurs ──────
#  L'anticipation est le biais central : on emménage plus grand PARCE QU'on
#  prépare une naissance. Chez les ménages installés depuis >= 3 ans, le
#  logement ne peut plus être une conséquence de la naissance à venir.
ANCIENNETE_MIN <- 3
res_rang_pred <- bind_rows(
  estimer_rang(filter(df, anciennete >= ANCIENNETE_MIN), F_POOLE,
               "Ensemble - installes 3 ans et plus"),
  estimer_rang(filter(df, anciennete >= ANCIENNETE_MIN, rang == "1er enfant"), F_RANG1,
               "1er enfant - installes 3 ans et plus"),
  estimer_rang(filter(df, anciennete >= ANCIENNETE_MIN, rang == "Rang 2+"), F_RANG2,
               "Rang 2 et plus - installes 3 ans et plus"),
  estimer_rang(filter(df, anciennete < ANCIENNETE_MIN), F_POOLE,
               "Ensemble - demenageurs recents"))
cat("\nLogement predetermine (anciennete >= 3 ans) :\n")
print(as.data.frame(res_rang_pred %>% mutate(across(where(is.numeric), ~ round(.x, 4)))),
      row.names = FALSE)

# Part des ménages ayant déménagé récemment, avec et sans naissance : mesure
# directe de l'anticipation.
cat("\nAnticipation : part de menages installes depuis moins de 3 ans\n")
print(as.data.frame(df %>% filter(!is.na(anciennete)) %>%
  group_by(rang, naissance) %>%
  summarise(n = n(),
            part_recents_pct = round(100 * weighted.mean(anciennete < 3, poids), 1),
            .groups = "drop")), row.names = FALSE)

res_rang_tout <- bind_rows(res_rang, res_rang_pred)
write_csv(res_rang_tout, "Output/natalite_par_rang.csv")

# ============================================================================
#  §3 — LE STATUT D'OCCUPATION : ESCALIER DE CONTRÔLES
# ----------------------------------------------------------------------------
#  On part de l'écart BRUT locataire / propriétaire et on ajoute les contrôles
#  un par un. Si l'écart s'effondre à l'ajout du revenu, l'"effet propriétaire"
#  n'est qu'un effet revenu ; s'il s'effondre à l'ajout de l'âge, c'est un
#  effet de cycle de vie.
# ============================================================================
cat("\n\n########## §3 — ESCALIER DE CONTROLES : STATUT D'OCCUPATION ##########\n")

ESCALIER <- list(
  "M0 : statut seul"            = naiss01 ~ statut3 + factor(annee),
  "M1 : + age"                  = naiss01 ~ statut3 + age_c + I(age_c^2) + factor(annee),
  "M2 : + rang et taille"       = naiss01 ~ statut3 + age_c + I(age_c^2) +
    nb_enfants + taille_avant + factor(annee),
  "M3 : + niveau de vie"        = naiss01 ~ statut3 + age_c + I(age_c^2) +
    nb_enfants + taille_avant + nv_10k + factor(annee),
  "M4 : + CSP et geographie"    = naiss01 ~ statut3 + age_c + I(age_c^2) +
    nb_enfants + taille_avant + nv_10k + csp + urbain + zeat + factor(annee),
  "M5 : + logement (surface, prix au m2)" = naiss01 ~ statut3 + age_c + I(age_c^2) +
    nb_enfants + taille_avant + nv_10k + csp + urbain + zeat +
    surface_10 + cout_m2_reel + factor(annee))

escalier_sur <- function(d, lib) {
  d <- droplevels(d)
  imap_dfr(ESCALIER, function(f, nom) {
    m <- svyglm(elaguer(f, d), design = des(d), family = quasibinomial(link = "probit"))
    ame(m, "statut3") %>% en_points() %>%
      transmute(echantillon = lib, modele = nom, contraste = contrast,
                ame_pts = estimate, conf.low, conf.high, p.value)
  })
}
res_escalier <- bind_rows(
  escalier_sur(df, "Ensemble"),
  escalier_sur(filter(df, rang == "1er enfant"), "1er enfant"),
  escalier_sur(filter(df, rang == "Rang 2+"),    "Rang 2 et plus"))

cat("\nEcart de probabilite de naissance vs LOCATAIRE (points de %) :\n")
print(as.data.frame(res_escalier %>% mutate(across(where(is.numeric), ~ round(.x, 3)))),
      row.names = FALSE)
write_csv(res_escalier, "Output/escalier_statut.csv")

# Écart d'âge et de revenu entre statuts : ce que l'escalier absorbe.
cat("\nCe que les controles absorbent (moyennes ponderees) :\n")
print(as.data.frame(df %>% group_by(statut3) %>%
  summarise(n = n(), age = round(weighted.mean(age_femme, poids), 1),
            nv_k = round(weighted.mean(niveau_vie_reel, poids) / 1000, 1),
            surface = round(weighted.mean(surface, poids), 1),
            part_1er_enfant_pct = round(100 * weighted.mean(rang == "1er enfant", poids), 1),
            .groups = "drop")), row.names = FALSE)

# ============================================================================
#  §4 — GRADIENT GÉOGRAPHIQUE ET DEGRÉ D'URBANISATION
# ----------------------------------------------------------------------------
#  `densite` (tranche d'unité urbaine en 5 classes) n'est disponible qu'à
#  partir de 2014 : avant, les fichiers ne portent que TAU99, qui mesure des
#  AIRES urbaines et n'est pas comparable (cf. 00_utils.R).
# ============================================================================
cat("\n\n########## §4 — GRADIENT GEOGRAPHIQUE ET URBANISATION ##########\n")

df_d <- df %>% filter(!is.na(densite)) %>% droplevels()
cat("Champ densite (2014+) :", nrow(df_d), "menages,",
    length(unique(df_d$annee)), "millesimes\n")

F_DENS <- naiss01 ~ surface_10 * densite + statut3 * densite + cout_m2_reel +
  nv_10k + nb_enfants + taille_avant + age_c + I(age_c^2) + demenage + csp +
  zeat + factor(annee)
m_dens <- svyglm(F_DENS, design = des(df_d), family = quasibinomial(link = "probit"))

grad_surface <- ame(m_dens, "surface_10", by = "densite") %>% en_points()
grad_statut  <- ame(m_dens, "statut3",    by = "densite") %>% en_points()

cat("\nEffet de la surface (+10 m2) par tranche d'unite urbaine :\n")
print(as.data.frame(grad_surface %>%
        select(densite, estimate, conf.low, conf.high, p.value) %>%
        mutate(across(where(is.numeric), ~ round(.x, 3)))), row.names = FALSE)
cat("\nEcart de statut vs locataire, par tranche d'unite urbaine :\n")
print(as.data.frame(grad_statut %>%
        select(densite, contrast, estimate, conf.low, conf.high, p.value) %>%
        mutate(across(where(is.numeric), ~ round(.x, 3)))), row.names = FALSE)

cat("\nTest de Wald sur les interactions surface x densite :\n")
print(regTermTest(m_dens, ~ surface_10:densite))
cat("\nTest de Wald sur les interactions statut x densite :\n")
print(regTermTest(m_dens, ~ statut3:densite))

# ── Même exercice sur les ZEAT (8 vagues) ──────────────────────────────────
F_ZEAT <- naiss01 ~ surface_10 * zeat + statut3 * zeat + cout_m2_reel + nv_10k +
  nb_enfants + taille_avant + age_c + I(age_c^2) + demenage + csp + urbain +
  factor(annee)
df_z <- df %>% filter(!zeat %in% c("Non renseigne", "Code 6 (2014)")) %>% droplevels()
m_zeat <- svyglm(F_ZEAT, design = des(df_z), family = quasibinomial(link = "probit"))
grad_zeat <- ame(m_zeat, "surface_10", by = "zeat") %>% en_points()
cat("\nEffet de la surface (+10 m2) par ZEAT :\n")
print(as.data.frame(grad_zeat %>% select(zeat, estimate, conf.low, conf.high, p.value) %>%
        mutate(across(where(is.numeric), ~ round(.x, 3)))), row.names = FALSE)
cat("\nTest de Wald sur les interactions surface x ZEAT :\n")
print(regTermTest(m_zeat, ~ surface_10:zeat))

write_csv(bind_rows(
  grad_surface %>% transmute(dimension = "Tranche d'unite urbaine",
                             variable = "Surface (+10 m2)",
                             zone = as.character(densite),
                             ame_pts = estimate, conf.low, conf.high, p.value),
  grad_statut  %>% transmute(dimension = "Tranche d'unite urbaine",
                             variable = paste("Statut :", contrast),
                             zone = as.character(densite),
                             ame_pts = estimate, conf.low, conf.high, p.value),
  grad_zeat    %>% transmute(dimension = "ZEAT", variable = "Surface (+10 m2)",
                             zone = as.character(zeat),
                             ame_pts = estimate, conf.low, conf.high, p.value)),
  "Output/gradient_geo.csv")

# ============================================================================
#  §5 — QUELLE PART DE LA BAISSE DE NATALITÉ EST IMPUTABLE AU LOGEMENT ?
# ----------------------------------------------------------------------------
#  Contrefactuel rang-préservant : on donne aux ménages de 2025 la distribution
#  de surface et de prix au m² de 2010 (chaque ménage prend la valeur située au
#  MÊME rang pondéré dans la distribution de 2010), et on repondère pour
#  retrouver la répartition des statuts de 2010. On re-prédit la probabilité de
#  naissance avec le MÊME modèle et le MÊME effet fixe d'année : la rupture de
#  mesure de la naissance entre 2018 et 2022 est donc sans effet sur le
#  résultat, seul le dénominateur (la baisse totale) vient de l'Insee.
# ============================================================================
cat("\n\n########## §5 — DECOMPOSITION DE LA BAISSE DE NATALITE ##########\n")

# rang_pondere() et transposer() : 00_utils.R (partagées avec le script 28).

d10 <- df %>% filter(annee == "2010")
d25 <- df %>% filter(annee == "2025")
cat("2010 : n =", nrow(d10), " | 2025 : n =", nrow(d25), "\n")

# Modèle de référence : le modèle poolé du §2, estimé sur les 8 vagues.
m_ref <- svyglm(F_POOLE, design = des(df), family = quasibinomial(link = "probit"))

# Répartition des statuts et conditions moyennes, 2010 vs 2025.
part_statut <- function(d) prop.table(tapply(d$poids, droplevels(d$statut3), sum))
p10 <- part_statut(d10); p25 <- part_statut(d25)
conditions <- tibble(
  grandeur = c("Surface moyenne (m2)", "Surface mediane (m2)",
               "Cout au m2 (EUR/mois, constants 2025)",
               "Part locataires (%)", "Part accedants (%)", "Part non accedants (%)",
               "Age moyen de la femme", "Niveau de vie (kEUR/an/UC)"),
  v2010 = c(weighted.mean(d10$surface, d10$poids), wquantile(d10$surface, d10$poids, .5),
            weighted.mean(d10$cout_m2_reel, d10$poids), 100 * p10[["Locataire"]],
            100 * p10[["Proprietaire_accedant"]], 100 * p10[["Proprietaire_non_accedant"]],
            weighted.mean(d10$age_femme, d10$poids),
            weighted.mean(d10$niveau_vie_reel, d10$poids) / 1000),
  v2025 = c(weighted.mean(d25$surface, d25$poids), wquantile(d25$surface, d25$poids, .5),
            weighted.mean(d25$cout_m2_reel, d25$poids), 100 * p25[["Locataire"]],
            100 * p25[["Proprietaire_accedant"]], 100 * p25[["Proprietaire_non_accedant"]],
            weighted.mean(d25$age_femme, d25$poids),
            weighted.mean(d25$niveau_vie_reel, d25$poids) / 1000)) %>%
  mutate(ecart = v2025 - v2010, across(where(is.numeric), ~ round(.x, 2)))
cat("\nConditions de logement et de vie, 2010 vs 2025 (champ : couples 15-49) :\n")
print(as.data.frame(conditions), row.names = FALSE)

# ── Contrefactuels ─────────────────────────────────────────────────────────
predire <- function(m, d, w = d$poids)
  weighted.mean(as.numeric(predict(m, newdata = d, type = "response")), w)

cf_surface <- d25 %>% mutate(
  surface    = transposer(surface, poids, d10$surface, d10$poids),
  surface_10 = surface / 10)
cf_prix <- d25 %>% mutate(
  cout_m2_reel = transposer(cout_m2_reel, poids, d10$cout_m2_reel, d10$poids))
# Statut : repondération pour retrouver les parts de 2010 (la composition
# change, pas les caractéristiques individuelles).
poids_statut <- d25$poids * unname((p10 / p25)[as.character(d25$statut3)])
cf_tout <- cf_surface %>% mutate(
  cout_m2_reel = transposer(cout_m2_reel, poids, d10$cout_m2_reel, d10$poids))

# « Surface + prix (sans les statuts) » : BORNE HAUTE du logement. Le canal
# statut est le plus exposé à la causalité inverse (l'accession comme acte par
# lequel un couple signale qu'il s'installe, §3.3 de l'étude) : si une part de
# son coefficient est de l'anticipation, le canal compensateur est surestimé et
# le solde « nul » ne l'est plus. Cette ligne donne ce que vaudrait le logement
# sans lui.
decomposer <- function(m, lib) {
  p_obs <- predire(m, d25)
  tibble(modele = lib,
         canal = c("Observe 2025", "Surface de 2010", "Prix au m2 de 2010",
                   "Statuts de 2010",
                   "Surface + prix de 2010 (sans les statuts)",
                   "Logement complet de 2010 (surface + prix + statuts)"),
         p_pct = 100 * c(p_obs, predire(m, cf_surface), predire(m, cf_prix),
                         predire(m, d25, poids_statut),
                         predire(m, cf_tout),
                         predire(m, cf_tout, poids_statut))) %>%
    mutate(ecart_pts = p_pct - 100 * p_obs,
           ecart_relatif_pct = 100 * (p_pct / (100 * p_obs) - 1),
           # Part de la baisse 2010-2025 de l'ICF que ce canal reproduirait.
           part_de_la_baisse_pct = ecart_relatif_pct / (-100 * BAISSE_RELATIVE) * 100)
}

# Modèle "logement prédéterminé" : c'est l'estimation à retenir, la surface
# n'y peut pas être une conséquence de la naissance à venir.
d_pred <- droplevels(filter(df, anciennete >= ANCIENNETE_MIN))
m_pred <- svyglm(elaguer(F_POOLE, d_pred), design = des(d_pred),
                 family = quasibinomial(link = "probit"))

decompo <- bind_rows(
  decomposer(m_ref,  "Association (borne haute)"),
  decomposer(m_pred, "Logement predetermine (retenu)")) %>%
  mutate(across(where(is.numeric), ~ round(.x, 3)))
cat("\nContrefactuels :\n"); print(as.data.frame(decompo), row.names = FALSE)

# ── Borne de puissance : et si le vrai effet valait le MDE ? ───────────────
#  L'effet le plus petit que l'échantillon des non-déménageurs pourrait
#  détecter (alpha = 5 %, puissance 80 %). Au-delà, on l'aurait vu.
Z_MDE <- qnorm(0.975) + qnorm(0.80)
se_surface_pred <- res_rang_pred %>%
  filter(echantillon == "Ensemble - installes 3 ans et plus",
         variable == "surface_10") %>% pull(se)
mde_surface      <- Z_MDE * se_surface_pred
delta_surface_m2 <- weighted.mean(d25$surface, d25$poids) -
                    weighted.mean(d10$surface, d10$poids)
taux_ref_pct     <- 100 * weighted.mean(d25$naiss01, d25$poids)
effet_max_pts    <- abs(delta_surface_m2 / 10 * mde_surface)
borne_mde <- tibble(
  grandeur = c("Ecart de surface 2025-2010 (m2)",
               "MDE de l'effet surface (pts pour +10 m2)",
               "Effet maximal non detectable (pts)",
               "Taux de naissance de reference (%)",
               "Effet maximal en % du taux de naissance",
               "Part maximale de la baisse imputable a la surface (%)"),
  valeur = c(delta_surface_m2, mde_surface, effet_max_pts, taux_ref_pct,
             100 * effet_max_pts / taux_ref_pct,
             (100 * effet_max_pts / taux_ref_pct) / (-100 * BAISSE_RELATIVE) * 100)) %>%
  mutate(valeur = round(valeur, 3))
cat("\nBorne de puissance (MDE) :\n"); print(as.data.frame(borne_mde), row.names = FALSE)

write_csv(bind_rows(
  conditions %>% transmute(bloc = "Conditions 2010 vs 2025", cle = grandeur,
                           v2010, v2025, valeur = ecart),
  decompo    %>% transmute(bloc = paste("Contrefactuel -", modele), cle = canal,
                           valeur = ecart_relatif_pct, part = part_de_la_baisse_pct),
  borne_mde  %>% transmute(bloc = "Borne de puissance (MDE)", cle = grandeur, valeur)),
  "Output/decomposition_natalite.csv")

cat("\n\n=== 23 termine. Baisse de l'ICF a expliquer :",
    round(100 * BAISSE_RELATIVE, 1), "% entre 2010 et 2025 (Insee). ===\n")

# ============================================================================
#  §6 — CE À QUOI LA CONCLUSION TIENT
# ----------------------------------------------------------------------------
#  (a) PARAMÉTRISATION. « +10 m² » n'a pas le même sens selon ce qu'on tient
#      fixe. À PRIX AU M² constant, c'est « habiter plus grand en payant plus »
#      (arbitrage de niveau de vie). À BUDGET TOTAL constant, c'est « habiter
#      plus grand pour le même loyer », donc plus loin ou moins bien situé.
#      R/15 (panel) tenait le budget total fixe et trouvait zéro ; le §2 tient
#      le prix au m² fixe et trouve un effet. On teste ici les deux
#      paramétrisations sur le MÊME échantillon.
#  (b) POIDS RELATIF. Le logement n'est qu'un candidat parmi d'autres : on
#      refait le contrefactuel du §5 sur l'ÂGE et sur le NIVEAU DE VIE, pour
#      savoir ce que le logement pèse face au vieillissement du champ fécond.
#  (c) SOUS-POPULATION EXPOSÉE. Les conditions de logement ne se sont dégradées
#      que chez les locataires de moins de 40 ans : on y refait le
#      contrefactuel, puis on le ramène au poids du groupe dans le champ.
# ============================================================================
cat("\n\n########## §6 — CE A QUOI LA CONCLUSION TIENT ##########\n")

# ── (a) Les deux paramétrisations, même échantillon ────────────────────────
df <- df %>% mutate(cout_100 = deflater(cout_log, annee_num, IPC, base = 2025) / 100)
F_BUDGET <- update(F_POOLE, . ~ . - cout_m2_reel + cout_100)

comparer_param <- function(d, lib) {
  d <- droplevels(d)
  bind_rows(
    ame(svyglm(elaguer(F_POOLE, d), design = des(d),
               family = quasibinomial(link = "probit")), "surface_10") %>%
      en_points() %>% mutate(parametrisation = "A prix au m2 constant"),
    ame(svyglm(elaguer(F_BUDGET, d), design = des(d),
               family = quasibinomial(link = "probit")), "surface_10") %>%
      en_points() %>% mutate(parametrisation = "A budget total constant")) %>%
    transmute(echantillon = lib, n = nrow(d), naissances = sum(d$naiss01),
              parametrisation, ame_pts = estimate, se = std.error,
              conf.low, conf.high, p.value)
}
res_param <- bind_rows(
  comparer_param(df, "Ensemble"),
  comparer_param(filter(df, anciennete >= ANCIENNETE_MIN), "Installes 3 ans et plus"),
  comparer_param(filter(df, rang == "1er enfant"), "1er enfant"),
  comparer_param(filter(df, rang == "1er enfant", anciennete >= ANCIENNETE_MIN),
                 "1er enfant, installes 3 ans et plus")) %>%
  mutate(across(where(is.numeric), ~ round(.x, 4)))
cat("\n(a) Effet de +10 m2 selon ce qu'on tient fixe :\n")
print(as.data.frame(res_param), row.names = FALSE)
write_csv(res_param, "Output/parametrisation_surface.csv")

# ── (b) Logement, âge, niveau de vie : contrefactuels comparables ──────────
#  L'âge est transposé À PARITÉ DONNÉE (dans chaque classe de nb_enfants) :
#  transposé sur l'ensemble du champ, il donnerait à des ménages rajeunis la
#  parité élevée de leurs aînés, combinaison qui n'existe pas en 2010 et qui
#  gonflerait le contrefactuel. Reste que l'exercice ne dit rien de causal :
#  l'âge et le niveau de vie ne sont pas des leviers, ce sont des repères.
transposer_par <- function(x, w, g, x_ref, w_ref, g_ref, n_min = 30) {
  out <- x
  for (lv in unique(as.character(g[!is.na(g)]))) {
    i <- which(as.character(g) == lv); j <- which(as.character(g_ref) == lv)
    if (length(j) >= n_min) out[i] <- transposer(x[i], w[i], x_ref[j], w_ref[j])
  }
  out
}
cf_age <- d25 %>% mutate(
  age_femme = transposer_par(age_femme, poids, nb_enfants,
                             d10$age_femme, d10$poids, d10$nb_enfants),
  age_c     = (age_femme - 30) / 10)
cf_nv <- d25 %>% mutate(
  niveau_vie_reel = transposer(niveau_vie_reel, poids, d10$niveau_vie_reel, d10$poids),
  nv_10k          = niveau_vie_reel / 10000)

poids_relatif <- function(m, lib) {
  p_obs <- predire(m, d25)
  tibble(modele = lib,
         canal = c("Logement (surface + prix + statuts) de 2010",
                   "Structure par age de 2010", "Niveau de vie de 2010"),
         p_pct = 100 * c(predire(m, cf_tout, poids_statut), predire(m, cf_age),
                         predire(m, cf_nv))) %>%
    mutate(ecart_relatif_pct = 100 * (p_pct / (100 * p_obs) - 1),
           part_de_la_baisse_pct = ecart_relatif_pct / (-100 * BAISSE_RELATIVE) * 100)
}
res_poids <- bind_rows(poids_relatif(m_ref,  "Association (borne haute)"),
                       poids_relatif(m_pred, "Logement predetermine (retenu)")) %>%
  mutate(across(where(is.numeric), ~ round(.x, 3)))
cat("\n(b) Ce que chaque canal reproduirait de la baisse 2010-2025 :\n")
print(as.data.frame(res_poids), row.names = FALSE)
write_csv(res_poids, "Output/poids_relatif_canaux.csv")

# ── (c) Là où le logement s'est vraiment dégradé : locataires de < 40 ans ──
loc10 <- d10 %>% filter(statut3 == "Locataire", age_femme < 40)
loc25 <- d25 %>% filter(statut3 == "Locataire", age_femme < 40)
part_groupe <- sum(loc25$poids) / sum(d25$poids)
cat("\n(c) Locataires de moins de 40 ans : n2010 =", nrow(loc10),
    "| n2025 =", nrow(loc25),
    sprintf("| part du champ en 2025 : %.1f %%\n", 100 * part_groupe))
cat("    Surface moyenne :", round(weighted.mean(loc10$surface, loc10$poids), 1), "->",
    round(weighted.mean(loc25$surface, loc25$poids), 1), "m2\n")
cat("    Cout au m2 :", round(weighted.mean(loc10$cout_m2_reel, loc10$poids), 2), "->",
    round(weighted.mean(loc25$cout_m2_reel, loc25$poids), 2), "EUR/mois\n")

cf_loc <- loc25 %>% mutate(
  surface      = transposer(surface, poids, loc10$surface, loc10$poids),
  surface_10   = surface / 10,
  cout_m2_reel = transposer(cout_m2_reel, poids, loc10$cout_m2_reel, loc10$poids))
res_loc <- map_dfr(list("Association (borne haute)" = m_ref,
                        "Logement predetermine (retenu)" = m_pred),
  function(m) {
    p0 <- predire(m, loc25); p1 <- predire(m, cf_loc)
    tibble(p_obs_pct = 100 * p0, p_cf_pct = 100 * p1,
           ecart_relatif_pct = 100 * (p1 / p0 - 1),
           # Ramené à l'ensemble du champ : le groupe ne pèse qu'une fraction.
           effet_sur_le_champ_pct = 100 * (p1 - p0) * part_groupe / predire(m, d25),
           part_de_la_baisse_pct = (100 * (p1 - p0) * part_groupe / predire(m, d25)) /
             (-100 * BAISSE_RELATIVE) * 100)
  }, .id = "modele") %>% mutate(across(where(is.numeric), ~ round(.x, 3)))
cat("\n    Contrefactuel restreint aux locataires de moins de 40 ans :\n")
print(as.data.frame(res_loc), row.names = FALSE)
write_csv(res_loc, "Output/contrefactuel_locataires_jeunes.csv")

# ── Convergence des modèles utilisés ───────────────────────────────────────
cat("\nConvergence :",
    paste(c("m_ref", "m_pred", "m_int", "m_dens", "m_zeat"),
          c(m_ref$converged, m_pred$converged, m_int$converged,
            m_dens$converged, m_zeat$converged), sep = "=", collapse = " | "), "\n")

# ============================================================================
#  §7 — ROBUSTESSE ET FORME DU LIEN
# ----------------------------------------------------------------------------
#  (a) La définition de la naissance change entre 2018 et 2022 (module
#      « événements » puis reconstruction depuis le fichier individus). On
#      ré-estime séparément de part et d'autre de la rupture : si l'effet de
#      la surface tenait à la définition, il ne survivrait pas aux deux.
#  (b) FORME. Le §1 suggère un SEUIL plutôt qu'une pente : chez les couples
#      sans enfant, le quartile le plus petit décroche, les trois autres se
#      tiennent. On estime donc la surface en TRANCHES plutôt qu'en linéaire.
# ============================================================================
cat("\n\n########## §7 — ROBUSTESSE ET FORME DU LIEN ##########\n")

# ── (a) De part et d'autre de la rupture de mesure ─────────────────────────
res_periode <- bind_rows(
  estimer_rang(filter(df, annee_num <= 2018), F_POOLE, "2006-2018 (EVENEMEN_C)"),
  estimer_rang(filter(df, annee_num >= 2022), F_POOLE, "2022-2025 (fichier individus)"),
  estimer_rang(filter(df, annee_num <= 2018, anciennete >= ANCIENNETE_MIN), F_POOLE,
               "2006-2018, installes 3 ans et plus"),
  estimer_rang(filter(df, annee_num >= 2022, anciennete >= ANCIENNETE_MIN), F_POOLE,
               "2022-2025, installes 3 ans et plus"),
  estimer_rang(filter(df, annee_num <= 2018, rang == "1er enfant"), F_RANG1,
               "2006-2018, 1er enfant"),
  estimer_rang(filter(df, annee_num >= 2022, rang == "1er enfant"), F_RANG1,
               "2022-2025, 1er enfant")) %>%
  filter(variable == "surface_10") %>% mutate(across(where(is.numeric), ~ round(.x, 4)))
cat("\n(a) Effet de la surface (+10 m2) de part et d'autre de 2018 :\n")
print(as.data.frame(res_periode), row.names = FALSE)
write_csv(res_periode, "Output/robustesse_periode.csv")

# ── (b) Surface en tranches : seuil ou pente ? ─────────────────────────────
SEUILS <- c(-Inf, 50, 70, 90, 120, Inf)
LIB_SEUILS <- c("Moins de 50 m2", "50 a 69 m2", "70 a 89 m2 (ref.)",
                "90 a 119 m2", "120 m2 et plus")
df <- df %>% mutate(surf_tr = factor(cut(surface, SEUILS, labels = LIB_SEUILS,
                                         right = FALSE),
                                     levels = LIB_SEUILS))
df$surf_tr <- relevel(df$surf_tr, ref = "70 a 89 m2 (ref.)")

F_TR   <- update(F_POOLE, . ~ . - surface_10 + surf_tr)
F_TR_1 <- update(F_RANG1, . ~ . - surface_10 + surf_tr)

estimer_tranches <- function(d, f, lib) {
  d <- droplevels(d)
  m <- svyglm(elaguer(f, d), design = des(d), family = quasibinomial(link = "probit"))
  ame(m, "surf_tr") %>% en_points() %>%
    transmute(echantillon = lib, n = nrow(d), naissances = sum(d$naiss01),
              contraste = contrast, ame_pts = estimate,
              conf.low, conf.high, p.value)
}
res_tranches <- bind_rows(
  estimer_tranches(df, F_TR, "Ensemble"),
  estimer_tranches(filter(df, rang == "1er enfant"), F_TR_1, "1er enfant"),
  estimer_tranches(filter(df, rang == "1er enfant", anciennete >= ANCIENNETE_MIN),
                   F_TR_1, "1er enfant, installes 3 ans et plus"),
  estimer_tranches(filter(df, rang == "Rang 2+"), F_TR, "Rang 2 et plus")) %>%
  mutate(across(where(is.numeric), ~ round(.x, 3)))
cat("\n(b) Ecart de probabilite de naissance par tranche de surface (pts) :\n")
print(as.data.frame(res_tranches), row.names = FALSE)
write_csv(res_tranches, "Output/surface_en_tranches.csv")

# Répartition des couples sans enfant dans ces tranches, 2010 vs 2025 : c'est
# la part sous le seuil qui compte, pas la surface moyenne.
cat("\nRepartition des couples SANS ENFANT par tranche de surface (%) :\n")
print(as.data.frame(df %>% filter(rang == "1er enfant", annee %in% VAGUES_TAUX) %>%
  group_by(annee, surf_tr) %>% summarise(w = sum(poids), .groups = "drop_last") %>%
  mutate(part_pct = round(100 * w / sum(w), 1)) %>% ungroup() %>%
  select(-w) %>% pivot_wider(names_from = surf_tr, values_from = part_pct)),
  row.names = FALSE)

# ============================================================================
#  §8 — LA DÉCOMPOSITION AVEC LA SPÉCIFICATION EN SEUIL
# ----------------------------------------------------------------------------
#  Le §7(b) montre que le lien n'est pas une pente mais une MARCHE à 50 m² :
#  en dessous, la probabilité de première naissance chute de 8 points ; au-delà,
#  la surface ne départage presque plus. Une spécification linéaire ne peut donc
#  pas capter ce qui s'est réellement passé entre 2010 et 2025 — la surface
#  MOYENNE n'a pas bougé, mais la part des couples sans enfant logés sous 50 m²
#  est passée de 15,1 à 19,1 %. On refait donc le contrefactuel du §5 avec les
#  tranches. Le contrefactuel étant rang-préservant, il transpose toute la
#  distribution et pas seulement sa moyenne : il suffit de recalculer la tranche
#  à partir de la surface transposée.
# ============================================================================
cat("\n\n########## §8 — DECOMPOSITION AVEC SPECIFICATION EN SEUIL ##########\n")

d_pred_tr <- droplevels(filter(df, anciennete >= ANCIENNETE_MIN))
m_seuil <- svyglm(elaguer(F_TR, d_pred_tr), design = des(d_pred_tr),
                  family = quasibinomial(link = "probit"))

retrancher <- function(d) d %>%
  mutate(surf_tr = relevel(factor(cut(surface, SEUILS, labels = LIB_SEUILS,
                                      right = FALSE), levels = LIB_SEUILS),
                           ref = "70 a 89 m2 (ref.)"))
cf_surface_tr <- retrancher(cf_surface)
cf_tout_tr    <- retrancher(cf_tout)

p0 <- predire(m_seuil, retrancher(d25))   # d25 date du §5 : sans surf_tr
res_seuil <- tibble(
  canal = c("Observe 2025", "Surface de 2010 (tranches)", "Prix au m2 de 2010",
            "Statuts de 2010", "Surface + prix de 2010 (sans les statuts)",
            "Logement complet de 2010"),
  p_pct = 100 * c(p0, predire(m_seuil, cf_surface_tr),
                  predire(m_seuil, retrancher(cf_prix)),
                  predire(m_seuil, retrancher(d25), poids_statut),
                  predire(m_seuil, cf_tout_tr),
                  predire(m_seuil, cf_tout_tr, poids_statut))) %>%
  mutate(ecart_relatif_pct = 100 * (p_pct / (100 * p0) - 1),
         part_de_la_baisse_pct = ecart_relatif_pct / (-100 * BAISSE_RELATIVE) * 100,
         across(where(is.numeric), ~ round(.x, 3)))
cat("\nContrefactuel, specification en seuil :\n")
print(as.data.frame(res_seuil), row.names = FALSE)

part_sous50 <- function(d, lib) tibble(
  situation = lib,
  couples_sans_enfant_pct = 100 * weighted.mean(
    filter(d, rang == "1er enfant")$surf_tr == "Moins de 50 m2",
    filter(d, rang == "1er enfant")$poids),
  ensemble_du_champ_pct = 100 * weighted.mean(d$surf_tr == "Moins de 50 m2", d$poids))
sous50 <- bind_rows(
  part_sous50(retrancher(d10), "2010 observe"),
  part_sous50(retrancher(d25), "2025 observe"),
  part_sous50(cf_surface_tr,   "2025, surfaces de 2010")) %>%
  mutate(across(where(is.numeric), ~ round(.x, 1)))
cat("\nPart des menages loges sous 50 m2 :\n")
print(as.data.frame(sous50), row.names = FALSE)

write_csv(bind_rows(
  res_seuil %>% transmute(bloc = "Contrefactuel - seuil", cle = canal,
                          valeur = ecart_relatif_pct, part = part_de_la_baisse_pct),
  sous50 %>% transmute(bloc = "Part sous 50 m2", cle = situation,
                       valeur = couples_sans_enfant_pct, part = ensemble_du_champ_pct)),
  "Output/decomposition_seuil.csv")
