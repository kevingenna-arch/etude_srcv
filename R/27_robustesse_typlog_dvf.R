# ============================================================================
#  SRCV — 27. ROBUSTESSE : TYPE DE LOGEMENT (MAISON/APPARTEMENT) ET INDICE DVF+
# ----------------------------------------------------------------------------
#  Reprend les spécifications DÉJÀ PUBLIÉES du script 23 (tableaux
#  « effet_surface », « seuil » et « escalier » de l'étude) et vérifie qu'elles
#  tiennent quand on ajoute :
#   (a) le type de logement (maison vs appartement, `maison_appart`) ;
#   (b) l'indice de prix DVF+ par ZEAT x année (script 26), un signal de
#       marché indépendant du choix du ménage.
#  Chaque comparaison est faite sur le MÊME échantillon (avec/sans le nouveau
#  contrôle), pour que l'écart ne soit imputable qu'au contrôle ajouté.
#
#  À LANCER DEPUIS LA RACINE DU PROJET, APRÈS 26 :
#     source("R/27_robustesse_typlog_dvf.R")
#
#  Sorties : Output/robustesse_typlog.csv, Output/robustesse_dvf.csv
# ============================================================================

source("R/00_prepa_fecondite.R")
library(survey); library(broom); library(marginaleffects)
activer_cache_prepa()

IPC <- charger_ipc()
if (!dir.exists("Output")) dir.create("Output")

SEUILS <- c(-Inf, 50, 70, 90, 120, Inf)
LIB_SEUILS <- c("Moins de 50 m2", "50 a 69 m2", "70 a 89 m2 (ref.)",
                "90 a 119 m2", "120 m2 et plus")

DONNEES_FEC <- millesimes_prepares(restreindre = TRUE)
df <- bind_rows(imap(DONNEES_FEC, ~ .x %>% select(any_of(c(VARS_INTERET, "datent"))))) %>%
  mutate(annee_num = as.integer(annee),
         naiss01 = as.integer(naissance == "Oui"),
         cout_m2_reel = deflater(cout_m2, annee_num, IPC, base = 2025),
         niveau_vie_reel = deflater(niveau_vie, annee_num - 1, IPC, base = 2025),
         surface_10 = surface / 10, nv_10k = niveau_vie_reel / 10000,
         anciennete = annee_num - datent,
         rang = factor(if_else(nb_enf_avant == 0, "1er enfant", "Rang 2+"),
                       levels = c("1er enfant", "Rang 2+")),
         across(where(is.factor), ~ factor(as.character(.))),
         statut3 = factor(statut3, levels = c("Locataire", "Proprietaire_accedant",
                                              "Proprietaire_non_accedant")),
         rang = factor(rang, levels = c("1er enfant", "Rang 2+")),
         maison_appart = factor(maison_appart, levels = c("Maison", "Appartement"))) %>%
  filter(!is.na(surface), !is.na(cout_m2_reel), is.finite(cout_m2_reel),
         !is.na(statut3), !is.na(age_femme), !is.na(niveau_vie_reel),
         !is.na(poids), poids > 0) %>%
  mutate(surf_tr = relevel(factor(cut(surface, SEUILS, labels = LIB_SEUILS,
                                      right = FALSE), levels = LIB_SEUILS),
                           ref = "70 a 89 m2 (ref.)"))

des <- function(d) svydesign(ids = ~id_cluster, weights = ~poids, data = droplevels(d))
ame <- function(m, vars, ...) as_tibble(avg_slopes(m, variables = vars,
                                                   wts = m$prior.weights, ...))
en_points <- function(x) x %>% mutate(across(any_of(c("estimate", "std.error",
                                                      "conf.low", "conf.high")),
                                             ~ 100 * .x))
elaguer <- function(f, d) {
  vars <- setdiff(all.vars(f), all.vars(f[[2]]))
  mono <- vars[vapply(vars, function(v) v %in% names(d) &&
                        is.factor(d[[v]]) && nlevels(droplevels(d[[v]])) < 2, logical(1))]
  if (length(mono)) f <- update(f, paste(". ~ . -", paste(mono, collapse = " - ")))
  f
}
ANCIENNETE_MIN <- 3

cat("Champ :", nrow(df), "menages, dont", sum(is.na(df$maison_appart)),
    "sans type de logement renseigne\n")

df_t <- df %>% filter(!is.na(maison_appart)) %>% droplevels()
cat("Champ retenu pour la comparaison (type de logement renseigne) :",
    nrow(df_t), "menages (", round(100*nrow(df_t)/nrow(df), 1), "% du total)\n")

# ============================================================================
#  §1 — TABLEAU 5 (surface_10, association vs logement predetermine)
# ============================================================================
cat("\n\n########## §1 — SURFACE : AVEC/SANS TYPE DE LOGEMENT ##########\n")

#  MÊMES formules que le script 23 : F_BASE (poolée) pour l'ensemble, F_RANG1
#  (sans parité ni taille, constantes ou quasi chez les couples sans enfant)
#  pour le premier enfant. Une version antérieure appliquait la formule poolée
#  au premier enfant, d'où un 0,74 « de référence » là où le tableau publié
#  dit 0,73 : la référence doit être exactement le chiffre publié.
F_BASE  <- naiss01 ~ surface_10 + cout_m2_reel + nv_10k + statut3 +
  age_c + I(age_c^2) + demenage + csp + urbain + zeat + factor(annee) +
  nb_enfants + taille_avant
F_RANG1 <- update(F_BASE, . ~ . - nb_enfants - taille_avant)
F_AVEC_TYPE  <- update(F_BASE,  . ~ . + maison_appart)
F_RANG1_TYPE <- update(F_RANG1, . ~ . + maison_appart)

estimer <- function(d, f, lib) {
  d <- droplevels(d)
  m <- svyglm(elaguer(f, d), design = des(d), family = quasibinomial(link = "probit"))
  ame(m, "surface_10") %>% en_points() %>%
    transmute(specification = lib, n = nrow(d), naissances = sum(d$naiss01),
              ame_pts = estimate, se = std.error, conf.low, conf.high, p.value)
}

res1 <- bind_rows(
  estimer(df_t, F_BASE, "Ensemble, SANS type de logement (reference PDF)"),
  estimer(df_t, F_AVEC_TYPE, "Ensemble, AVEC type de logement"),
  estimer(filter(df_t, rang == "1er enfant"), F_RANG1,
          "1er enfant, SANS type de logement (reference PDF)"),
  estimer(filter(df_t, rang == "1er enfant"), F_RANG1_TYPE,
          "1er enfant, AVEC type de logement"),
  estimer(filter(df_t, rang == "1er enfant", anciennete >= ANCIENNETE_MIN), F_RANG1,
          "1er enfant, installes>=3ans, SANS type (reference PDF)"),
  estimer(filter(df_t, rang == "1er enfant", anciennete >= ANCIENNETE_MIN), F_RANG1_TYPE,
          "1er enfant, installes>=3ans, AVEC type"))
cat("\nEffet de la surface, +10 m2 :\n")
print(as.data.frame(res1 %>% mutate(across(where(is.numeric), ~ round(.x, 4)))), row.names = FALSE)

# Effet propre du type de logement
m_type <- svyglm(elaguer(F_RANG1_TYPE, filter(df_t, rang == "1er enfant")),
                 design = des(filter(df_t, rang == "1er enfant")),
                 family = quasibinomial(link = "probit"))
cat("\nEffet propre 'Appartement vs Maison' (1er enfant, a surface egale) :\n")
print(ame(m_type, "maison_appart") %>% en_points() %>%
        mutate(across(where(is.numeric), ~ round(.x, 3))) %>% as.data.frame(), row.names = FALSE)

# ============================================================================
#  §2 — TABLEAU 12 (surface en tranches, seuil a 50 m2)
# ============================================================================
cat("\n\n########## §2 — SEUIL A 50 M2 : AVEC/SANS TYPE DE LOGEMENT ##########\n")

F_TR      <- update(F_RANG1, . ~ . - surface_10 + surf_tr)   # 1er enfant : F_TR_1 du script 23
F_TR_TYPE <- update(F_TR, . ~ . + maison_appart)

estimer_tr <- function(d, f, lib) {
  d <- droplevels(d)
  m <- svyglm(elaguer(f, d), design = des(d), family = quasibinomial(link = "probit"))
  ame(m, "surf_tr") %>% en_points() %>%
    transmute(specification = lib, n = nrow(d), contraste = contrast,
              ame_pts = estimate, conf.low, conf.high, p.value)
}
res2 <- bind_rows(
  estimer_tr(filter(df_t, rang == "1er enfant"), F_TR,
             "1er enfant, SANS type (reference PDF)"),
  estimer_tr(filter(df_t, rang == "1er enfant"), F_TR_TYPE,
             "1er enfant, AVEC type"))
cat("\nEcart de probabilite par tranche de surface :\n")
print(as.data.frame(res2 %>% mutate(across(where(is.numeric), ~ round(.x, 3)))), row.names = FALSE)

# ============================================================================
#  §3 — TABLEAU 6 (escalier de controles du statut d'occupation)
# ============================================================================
cat("\n\n########## §3 — ESCALIER DU STATUT : AVEC/SANS TYPE DE LOGEMENT ##########\n")

F_M5 <- naiss01 ~ statut3 + age_c + I(age_c^2) + nb_enfants + taille_avant +
  nv_10k + csp + urbain + zeat + surface_10 + cout_m2_reel + factor(annee)
F_M5_TYPE <- update(F_M5, . ~ . + maison_appart)

escalier <- function(d, f, lib) {
  d <- droplevels(d)
  m <- svyglm(elaguer(f, d), design = des(d), family = quasibinomial(link = "probit"))
  ame(m, "statut3") %>% en_points() %>%
    transmute(specification = lib, contraste = contrast, ame_pts = estimate,
              conf.low, conf.high, p.value)
}
res3 <- bind_rows(
  escalier(df_t, F_M5, "Ensemble, SANS type (reference PDF, M5)"),
  escalier(df_t, F_M5_TYPE, "Ensemble, AVEC type"),
  escalier(filter(df_t, rang == "1er enfant"), F_M5,
           "1er enfant, SANS type (reference PDF, M5)"),
  escalier(filter(df_t, rang == "1er enfant"), F_M5_TYPE,
           "1er enfant, AVEC type"))
cat("\nEcart vs Locataire :\n")
print(as.data.frame(res3 %>% mutate(across(where(is.numeric), ~ round(.x, 3)))), row.names = FALSE)

write_csv(bind_rows(res1 %>% mutate(bloc = "Table5_surface"),
                    res2 %>% rename(ame_pts_ = ame_pts) %>% mutate(bloc = "Table12_seuil") %>%
                      rename(ame_pts = ame_pts_),
                    res3 %>% mutate(bloc = "Table6_escalier")),
          "Output/robustesse_typlog.csv")

# ============================================================================
#  §4 — CONTRÔLE DVF+ : L'EFFET DE SURFACE TIENT-IL AVEC LE SIGNAL DE MARCHÉ ?
# ----------------------------------------------------------------------------
#  DVF+ ne couvre que 2014-2025 : comparaison sur ce sous-champ uniquement,
#  avec sa PROPRE reference (pas celle du PDF, qui pool les 8 vagues).
# ============================================================================
cat("\n\n########## §4 — CONTROLE DVF+ (2014-2025 uniquement) ##########\n")

if (file.exists("Output/dvf_zeat_annee.csv")) {
  # Indice apparié par TYPE DE BIEN : le prix médian au m2 d'une maison et d'un
  # appartement n'ont rien à voir (Région parisienne 2014 : 3 158 contre
  # 6 111 EUR) ; un indice mélangé serait dominé par la part d'appartements de
  # la ZEAT. `maison_appart` (socle) permet désormais l'appariement.
  dvf <- read_csv("Output/dvf_zeat_annee.csv", show_col_types = FALSE) %>%
    transmute(zeat = as.character(zeat), annee = as.character(annee),
              maison_appart = type_bien, dvf_prix_m2 = prix_m2_median)

  df_dvf <- df_t %>% filter(annee_num >= 2014) %>%
    mutate(zeat_code = recode(as.character(zeat),
      "Region parisienne"="1","Bassin parisien"="2","Nord"="3","Est"="4",
      "Ouest"="5","Sud-Ouest"="7","Centre-Est"="8","Mediterranee"="9",
      "DOM"="0", .default = NA_character_),
      maison_appart = as.character(maison_appart)) %>%
    left_join(dvf, by = c("zeat_code" = "zeat", "annee" = "annee",
                          "maison_appart" = "maison_appart")) %>%
    mutate(dvf_prix_m2_k = dvf_prix_m2 / 1000,        # par +1 000 EUR/m2
           maison_appart = factor(maison_appart, levels = c("Maison", "Appartement")),
           cellule = paste(zeat_code, annee, maison_appart))

  cat("Taux de jointure DVF+ (2014-2025, par ZEAT x annee x type) :",
      round(100 * mean(!is.na(df_dvf$dvf_prix_m2)), 1), "%\n")

  df_dvf_ok <- df_dvf %>% filter(!is.na(dvf_prix_m2)) %>% droplevels()
  F_DVF_AVEC  <- update(F_BASE,  . ~ . + dvf_prix_m2_k)
  F_DVF_AVEC1 <- update(F_RANG1, . ~ . + dvf_prix_m2_k)

  res4 <- bind_rows(
    estimer(df_dvf_ok, F_BASE,     "2014-2025, SANS indice DVF+"),
    estimer(df_dvf_ok, F_DVF_AVEC, "2014-2025, AVEC indice DVF+ (ZEAT x annee x type)"),
    estimer(filter(df_dvf_ok, rang == "1er enfant"), F_RANG1,
            "1er enfant, 2014-2025, SANS indice DVF+"),
    estimer(filter(df_dvf_ok, rang == "1er enfant"), F_DVF_AVEC1,
            "1er enfant, 2014-2025, AVEC indice DVF+"))
  cat("\nEffet de la surface, +10 m2, sous-champ 2014-2025 :\n")
  print(as.data.frame(res4 %>% mutate(across(where(is.numeric), ~ round(.x, 4)))), row.names = FALSE)

  # ── Effet propre de l'indice : un test FAIBLE PAR CONSTRUCTION ─────────────
  #  L'indice ne varie qu'au niveau ZEAT x annee x type ; une fois les effets
  #  fixes ZEAT et annee retirés, il n'est identifié que sur la déformation
  #  différentielle de quelques dizaines de cellules. Les erreurs-types
  #  regroupées au niveau ménage traitent chaque ménage comme une observation
  #  indépendante de l'indice : elles sont très sous-estimées. On les recalcule
  #  en regroupant à la maille de variation de l'indice (ZEAT x annee x type).
  #  Ce qui reste valide, c'est l'effet de la surface INDIVIDUELLE, inchangé
  #  avec ou sans l'indice (res4) ; le coefficient de l'indice lui-même n'est
  #  pas informatif.
  d1 <- filter(df_dvf_ok, rang == "1er enfant") %>% droplevels()
  cat("\nCellules ZEAT x annee x type sur lesquelles l'indice varie (1er enfant) :",
      n_distinct(d1$cellule), "\n")
  m_dvf_men  <- svyglm(elaguer(F_DVF_AVEC1, d1), design = des(d1),
                       family = quasibinomial(link = "probit"))
  m_dvf_cell <- svyglm(elaguer(F_DVF_AVEC1, d1),
                       design = svydesign(ids = ~cellule, weights = ~poids, data = d1),
                       family = quasibinomial(link = "probit"))
  effet_dvf <- bind_rows(
    ame(m_dvf_men,  "dvf_prix_m2_k") %>% en_points() %>%
      mutate(specification = "Effet propre indice DVF+ (+1 000 EUR/m2), ET regroupees menage"),
    ame(m_dvf_cell, "dvf_prix_m2_k") %>% en_points() %>%
      mutate(specification = "Effet propre indice DVF+ (+1 000 EUR/m2), ET regroupees ZEAT x annee x type")) %>%
    transmute(specification, n = nrow(d1), naissances = sum(d1$naiss01),
              ame_pts = estimate, se = std.error, conf.low, conf.high, p.value)
  cat("\nEffet propre de l'indice DVF+ (1er enfant), selon le niveau de regroupement :\n")
  print(as.data.frame(effet_dvf %>% mutate(across(where(is.numeric), ~ round(.x, 3)))), row.names = FALSE)

  write_csv(bind_rows(res4, effet_dvf), "Output/robustesse_dvf.csv")
} else {
  cat("Output/dvf_zeat_annee.csv absent -- lancer 26 d'abord.\n")
}

cat("\n\n=== 27 termine. ===\n")
