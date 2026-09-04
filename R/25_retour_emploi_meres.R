# ============================================================================
#  SRCV — 25. LE RETOUR À L'EMPLOI DES MÈRES APRÈS UNE NAISSANCE
# ----------------------------------------------------------------------------
#  Champ : femmes de 18-45 ans, personne de référence ou conjointe (LIENPREF
#  00/01), vivant avec au moins un enfant (LIENPREF 02) de 0 à 2 ans. Fichiers
#  individus FPR (2022-2025), seuls à porter PL032 (statut économique actuel
#  auto-défini) et NBM_* (calendrier d'activité de l'année N-1).
#
#  ⚠️ CE QUE SRCV NE PERMET PAS DE MESURER : il n'existe AUCUNE variable de
#  MODE DE GARDE (crèche, assistante maternelle, grands-parents...) dans les
#  fichiers de diffusion SRCV — vérifié en balayant toutes les colonnes des
#  fichiers individus 2022-2025. On ne peut donc PAS chiffrer un taux de
#  recours à la crèche par quintile. La variable la plus proche est PL032,
#  le statut d'activité actuel auto-déclaré, dont une modalité (6) est
#  explicitement "tâches domestiques / garde d'enfants ou d'autres personnes
#  à charge" : elle isole les mères qui se déclarent elles-mêmes comme la
#  solution de garde de leur enfant, sans dire ce que font les autres.
#
#  ⚠️ COUPE, PAS PANEL : bien que les fichiers FPR soient chaînés d'une vague
#  à l'autre, une transition "avant/après" au niveau de l'INDIVIDU nécessite
#  d'apparier RB030 entre vagues consécutives, ce qui n'est pas fait ici. Le
#  design retenu est plus simple et plus robuste : une COUPE regroupant les 4
#  vagues, où l'âge du plus jeune enfant (0, 1 ou 2 ans) tient lieu d'axe
#  temporel — la comparaison est ENTRE mères à des étapes différentes, pas le
#  SUIVI d'une même mère.
#
#  À LANCER DEPUIS LA RACINE DU PROJET :
#     source("R/25_retour_emploi_meres.R")
#
#  Sorties : retour_emploi_descriptif.csv, retour_emploi_modele.csv
# ============================================================================

source("R/00_utils.R")
source("R/00_config.R")
library(survey); library(broom); library(marginaleffects)

if (!dir.exists("Output")) dir.create("Output")
VAGUES_FPR <- names(VAGUES)[map_lgl(VAGUES, ~ isTRUE(.x$has_nbm))]

# ── Diplôme : harmonisation sur le 1er chiffre du code DIPDET (2 chiffres) ──
#  Même convention que DIPDETPR/DIP14PR ailleurs dans le projet : le premier
#  chiffre distingue Supérieur (1-2) / Bac+2 (3) / Bac (4) / Infra-bac (5-7).
DIPLOME_LIB <- c("1" = "Superieur_long", "2" = "Superieur_long",
                 "3" = "Bac_2", "4" = "Bac", "5" = "CAP_BEP",
                 "6" = "Infra_CAP", "7" = "Infra_CAP")
NIVEAUX_DIPLOME <- c("Superieur_long", "Bac_2", "Bac", "CAP_BEP", "Infra_CAP", "Non renseigne")
recode_diplome_ind <- function(x) {
  x <- trimws(as.character(x))
  premier <- substr(x, 1, 1)
  lib <- unname(DIPLOME_LIB[premier])
  factor(if_else(is.na(lib), "Non renseigne", lib), levels = NIVEAUX_DIPLOME)
}

# ── Statut d'activité actuel (PL032), regroupé en 4 postes ─────────────────
PL032_LIB <- c("1" = "En_emploi", "2" = "Chomage",
               "6" = "Foyer_garde_enfant")
recode_pl032 <- function(x) {
  x <- trimws(as.character(x))
  lib <- unname(PL032_LIB[x])
  factor(if_else(is.na(lib), "Autre", lib),
         levels = c("En_emploi", "Chomage", "Foyer_garde_enfant", "Autre"))
}

# ── Chargement d'une vague : mères avec enfant 0-2 ans + logement du ménage ─
charger_meres <- function(annee) {
  cfg <- VAGUES[[annee]]
  ind <- lire_srcv(cfg$ind)
  verifier_colonnes(ind, c("RB030","RB040","LIENPREF","SEXE","AGE","PL032",
                           "DIPDET","RB050","NBM_MAT","NBM_FOY"),
                    paste0("(individus ", annee, ")"))

  base <- ind %>% transmute(
    hid = trimws(as.character(RB040)), pid = trimws(as.character(RB030)),
    lien = trimws(as.character(LIENPREF)), sexe = num(SEXE), age = num(AGE),
    activite = recode_pl032(PL032), diplome = recode_diplome_ind(DIPDET),
    nbm_mat = num(NBM_MAT), nbm_foy = num(NBM_FOY), poids = num(RB050))

  enfants <- base %>% filter(lien == "02") %>% group_by(hid) %>%
    summarise(age_min_enf = min(age, na.rm = TRUE), nb_enf_menage = n(), .groups = "drop")

  meres <- base %>% filter(lien %in% c("00", "01"), sexe == 2, age >= 18, age <= 45) %>%
    inner_join(enfants, by = "hid") %>%
    filter(age_min_enf <= 2, !is.na(poids), poids > 0) %>%
    mutate(annee = annee,
           age_enfant_f = factor(age_min_enf, levels = c(0, 1, 2),
                                 labels = c("0 an", "1 an", "2 ans")),
           # Etat l'annee precedente : conge mat/pat vs deja au foyer vs deja actif
           etat_avant = case_when(
             coalesce(nbm_mat, 0) >= 6 ~ "Conge mat/pat (6 m. ou plus, N-1)",
             coalesce(nbm_foy, 0) >= 6 ~ "Deja au foyer (6 m. ou plus, N-1)",
             TRUE ~ "Autre / actif des N-1"))

  men <- charger_menages(annee, cfg) %>%
    select(id, statut3, urbain, zeat, niveau_vie, surface)
  meres %>% left_join(men, by = c("hid" = "id"))
}

meres <- map_dfr(VAGUES_FPR, charger_meres) %>%
  mutate(en_emploi = as.integer(activite == "En_emploi"),
         niveau_vie_10k = niveau_vie / 10000,
         diplome = factor(as.character(diplome), levels = c("Bac", "Superieur_long",
                                                             "Bac_2", "CAP_BEP",
                                                             "Infra_CAP", "Non renseigne")),
         statut3 = factor(as.character(statut3), levels = c("Locataire",
                          "Proprietaire_accedant", "Proprietaire_non_accedant")),
         urbain = factor(as.character(urbain)), zeat = factor(as.character(zeat)))

cat("Champ : meres 18-45 ans avec enfant de 0-2 ans, 4 vagues FPR poolees :",
    nrow(meres), "observations,", round(sum(meres$poids)), "meres representees\n")

# ============================================================================
#  §1 — DESCRIPTIF : statut d'activité selon l'âge du plus jeune enfant
# ============================================================================
cat("\n\n########## §1 — DESCRIPTIF ##########\n")

desc_age <- meres %>% group_by(age_enfant_f) %>%
  summarise(n = n(),
            en_emploi_pct       = round(100 * weighted.mean(activite == "En_emploi", poids), 1),
            chomage_pct         = round(100 * weighted.mean(activite == "Chomage", poids), 1),
            foyer_garde_pct     = round(100 * weighted.mean(activite == "Foyer_garde_enfant", poids), 1),
            autre_pct           = round(100 * weighted.mean(activite == "Autre", poids), 1),
            .groups = "drop")
cat("Statut d'activite actuel, par age du plus jeune enfant (%) :\n")
print(as.data.frame(desc_age), row.names = FALSE)

desc_dip <- meres %>% filter(diplome != "Non renseigne") %>% group_by(diplome) %>%
  summarise(n = n(),
            en_emploi_pct   = round(100 * weighted.mean(activite == "En_emploi", poids), 1),
            chomage_pct     = round(100 * weighted.mean(activite == "Chomage", poids), 1),
            foyer_garde_pct = round(100 * weighted.mean(activite == "Foyer_garde_enfant", poids), 1),
            .groups = "drop") %>%
  arrange(desc(en_emploi_pct))
cat("\nStatut d'activite actuel, par diplome (%) :\n")
print(as.data.frame(desc_dip), row.names = FALSE)

desc_rang <- meres %>% mutate(rang = pmin(nb_enf_menage, 3)) %>% group_by(rang) %>%
  summarise(n = n(),
            en_emploi_pct = round(100 * weighted.mean(activite == "En_emploi", poids), 1),
            .groups = "drop")
cat("\nStatut d'activite actuel, par nombre d'enfants dans le menage :\n")
print(as.data.frame(desc_rang), row.names = FALSE)

desc_statut <- meres %>% filter(!is.na(statut3)) %>% group_by(statut3) %>%
  summarise(n = n(),
            en_emploi_pct   = round(100 * weighted.mean(activite == "En_emploi", poids), 1),
            chomage_pct     = round(100 * weighted.mean(activite == "Chomage", poids), 1),
            foyer_garde_pct = round(100 * weighted.mean(activite == "Foyer_garde_enfant", poids), 1),
            .groups = "drop")
cat("\nStatut d'activite actuel, par statut d'occupation du logement :\n")
print(as.data.frame(desc_statut), row.names = FALSE)

cat("\nEtat l'annee precedente (calendrier NBM_*) x statut actuel :\n")
print(as.data.frame(meres %>% group_by(etat_avant) %>%
  summarise(n = n(),
            en_emploi_pct = round(100 * weighted.mean(activite == "En_emploi", poids), 1),
            .groups = "drop")), row.names = FALSE)

write_csv(bind_rows(
  desc_age %>% transmute(dimension = "Age du plus jeune enfant",
                         categorie = as.character(age_enfant_f), n,
                         en_emploi_pct, foyer_garde_pct),
  desc_dip %>% transmute(dimension = "Diplome", categorie = as.character(diplome), n,
                         en_emploi_pct, foyer_garde_pct),
  desc_statut %>% transmute(dimension = "Statut logement", categorie = as.character(statut3),
                            n, en_emploi_pct, foyer_garde_pct)),
  "Output/retour_emploi_descriptif.csv")

# ============================================================================
#  §2 — MODÈLE : probabilité d'être en emploi, toutes choses égales
# ============================================================================
cat("\n\n########## §2 — MODELE ##########\n")

#  Le niveau de vie du ménage N'EST PAS mis en contrôle : HY020 inclut le
#  salaire de la mère elle-même, donc "contrôler le niveau de vie" reviendrait
#  en partie à contrôler l'outcome par lui-même (bad control / collisionneur).
d_m <- meres %>% filter(diplome != "Non renseigne", !is.na(statut3),
                        !is.na(urbain), !is.na(zeat)) %>% droplevels()
cat("Champ du modele :", nrow(d_m), "observations,", sum(d_m$en_emploi), "en emploi\n")

F_EMPLOI <- en_emploi ~ age_enfant_f + diplome + nb_enf_menage + age + statut3 +
  urbain + zeat + factor(annee)
design <- svydesign(ids = ~pid, weights = ~poids, data = d_m)
m_emploi <- svyglm(F_EMPLOI, design = design, family = quasibinomial(link = "probit"))

ame_emploi <- avg_slopes(m_emploi, variables = c("age_enfant_f", "diplome", "statut3"),
                         wts = m_emploi$prior.weights) %>%
  as_tibble() %>%
  mutate(across(c(estimate, std.error, conf.low, conf.high), ~ 100 * .x))

cat("\nEffets marginaux moyens sur la probabilite d'etre en emploi (points de %) :\n")
print(as.data.frame(ame_emploi %>% select(term, contrast, estimate, std.error,
                                          conf.low, conf.high, p.value) %>%
        mutate(across(where(is.numeric), ~ round(.x, 3)))), row.names = FALSE)

write_csv(ame_emploi, "Output/retour_emploi_modele.csv")

cat("\n\n=== 25 termine. ===\n")
