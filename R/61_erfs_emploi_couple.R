# ============================================================================
#  ERFS — 61. Emploi des conjoints et naissance de l'année suivante
# ----------------------------------------------------------------------------
#  Les inégalités entre femmes et hommes sur le marché du travail pèsent-elles
#  sur les naissances ? On rapporte la situation de chaque conjoint à une date
#  (enquête Emploi du 4e trimestre de l'année N, revenus fiscaux de N) à la
#  naissance qui suit, observée un an plus tard. L'enquête Emploi interroge
#  chaque logement six trimestres de suite : les ménages en 1re ou 2e
#  interrogation au 4e trimestre de N le sont encore au 4e trimestre de N+1,
#  en 5e ou 6e. Les identifiants changent d'un millésime à l'autre : les
#  couples sont appariés sur la région et les années et mois de naissance des
#  deux conjoints, et le rang d'interrogation doit avancer de quatre.
#  Naissance : enfant né après la semaine de référence de N (au plus tard dans
#  les douze mois qui précèdent celle de N+1) ; variante : né au moins neuf
#  mois après, donc conçu après l'enquête de N.
#  Situation en N : emploi stable (CDI, fonctionnaire, non-salarié), emploi
#  temporaire, chômage ou inactivité, pour chaque conjoint ; temps partiel de
#  la femme ; part de la femme dans les revenus d'activité du couple en N.
#  Modèles linéaires pondérés, contrôles du script 57 (niveau de vie, âges,
#  rang, âge du benjamin, diplômes, statut conjugal et d'occupation, taille
#  d'unité urbaine, région, année) ; erreurs-types robustes. Ce sont des
#  associations : un couple qui projette un enfant peut adapter l'emploi de la
#  femme avant la grossesse.
#  Tendances 2005-2023 (tous les couples) : emploi des femmes, part de leurs
#  revenus d'activité, temps partiel pour les enfants, congé parental.
#  Sorties : Output/erfs_emploi_couple.csv
# ============================================================================

source("R/57_erfs_preparation.R")   # df : couples de l'ERFS, contrôles, gains

CTRL   <- "nv_10k + I(nv_10k^2)"     # comme au script 57
FE     <- "annee + age_f + age_h_cl + nb_enf + benjamin_cl + dipl_femme + dipl_homme + conj + statut + tuu + region"
EMPLOI <- c("Emploi stable", "Emploi temporaire", "Chomage", "Inactivite", "Non renseigne")
PARTS  <- c("40 a 60 %", "Aucun revenu d'activite", "Moins de 25 %", "25 a 40 %", "Plus de 60 %")

# ── §0 Tendances 2005-2023, tous les couples ───────────────────────────────
part_femme <- function(f, h) if_else(f + h > 0, f / (f + h), NA_real_)
tendances <- df %>%
  mutate(part = part_femme(act_rev_femme, act_rev_homme), petit = !is.na(benjamin) & benjamin < 3) %>%
  group_by(annee) %>%
  summarise(part_femme_revenus_pct = 100 * pond(part, poids),
            femmes_emploi_stable_pct = 100 * pond(emploi_femme == "Emploi stable", poids),
            femmes_emploi_temporaire_pct = 100 * pond(emploi_femme == "Emploi temporaire", poids),
            femmes_chomage_pct = 100 * pond(emploi_femme == "Chomage", poids),
            femmes_inactives_pct = 100 * pond(emploi_femme == "Inactivite", poids),
            hommes_chomage_pct = 100 * pond(emploi_homme == "Chomage", poids),
            femmes_tp_enfants_pct = 100 * pond(tp_fam_femme, poids),
            meres_conge_parental_pct = 100 * pond(cpar_femme[petit], poids[petit]),
            peres_conge_parental_pct = 100 * pond(cpar_homme[petit], poids[petit]), .groups = "drop")
cat("\nTendances (couples dont la femme a 15 à 49 ans ; congé parental : benjamin de moins de 3 ans) :\n")
print(as.data.frame(tendances %>% mutate(across(-annee, ~ round(.x, 1)))), row.names = FALSE)

# ── §1 Appariement de N à N+1 (R/61_erfs_panel.R) ──────────────────────────
source("R/61_erfs_panel.R")        # appariement, panel

pan <- panel %>%
  mutate(part = part_femme(act_rev_femme, act_rev_homme),
         part_cl = factor(case_when(is.na(part) ~ "Aucun revenu d'activite", part < 0.25 ~ "Moins de 25 %",
                                    part < 0.40 ~ "25 a 40 %", part <= 0.60 ~ "40 a 60 %", TRUE ~ "Plus de 60 %"),
                          levels = PARTS),
         emploi_femme = factor(emploi_femme, levels = EMPLOI), emploi_homme = factor(emploi_homme, levels = EMPLOI),
         rang = factor(case_when(nb_enf_b == 0 ~ "1er enfant", nb_enf_b == 1 ~ "2e enfant", TRUE ~ "3e enfant ou plus")),
         # Composition à la naissance suivante (enfants présents en N+1, hors nouveau-né)
         nb_enf = factor(pmin(nb_enf_b, 4)),
         benjamin_cl = factor(case_when(is.na(benjamin_b) ~ "Aucun", benjamin_b < 3 ~ "0-2 ans",
                                        benjamin_b < 6 ~ "3-5 ans", TRUE ~ "6 ans ou plus")))
cat("\nPanel :", nrow(pan), "couples suivis d'une année sur l'autre |", sum(pan$naiss), "naissances après l'enquête de N |",
    sum(pan$naiss_concue), "conçues après\n")

# ── §2 Taux de naissance selon la situation en N ───────────────────────────
taux <- bind_rows(
  pan %>% group_by(variable = "Emploi de la femme", modalite = as.character(emploi_femme)) %>%
    summarise(couples = n(), naissances = sum(naiss), taux_pct = 100 * pond(naiss, poids), .groups = "drop"),
  pan %>% group_by(variable = "Emploi de l'homme", modalite = as.character(emploi_homme)) %>%
    summarise(couples = n(), naissances = sum(naiss), taux_pct = 100 * pond(naiss, poids), .groups = "drop"),
  pan %>% group_by(variable = "Part de la femme dans les revenus d'activite", modalite = as.character(part_cl)) %>%
    summarise(couples = n(), naissances = sum(naiss), taux_pct = 100 * pond(naiss, poids), .groups = "drop"))
cat("\nTaux de naissance dans l'année qui suit, selon la situation en N :\n")
print(as.data.frame(taux %>% mutate(taux_pct = round(taux_pct, 2))), row.names = FALSE)

# ── §3 Modèles ─────────────────────────────────────────────────────────────
ECHANTILLONS <- list("Ensemble" = function(d) d,
                     "1er enfant" = function(d) filter(d, rang == "1er enfant"),
                     "2e enfant" = function(d) filter(d, rang == "2e enfant"),
                     "3e enfant ou plus" = function(d) filter(d, rang == "3e enfant ou plus"))
modele <- function(d, y) {
  m <- feols(as.formula(paste(y, "~ emploi_femme + emploi_homme + part_cl + tp_femme +", CTRL, "|", FE)),
             data = d, weights = ~poids, vcov = "hetero")
  ct <- as.data.frame(coeftable(m)); ct$terme <- rownames(ct)
  ct %>% filter(grepl("^(emploi_|part_cl|tp_femme)", terme)) %>%
    transmute(terme, coef_pts = 100 * Estimate, se = 100 * `Std. Error`, p = `Pr(>|t|)`) %>%
    mutate(n = nobs(m), naissances = sum(d[[y]]), taux_pct = 100 * pond(d[[y]], d$poids))
}
modeles <- imap_dfr(c("Naissance apres l'enquete de N" = "naiss", "Naissance concue apres l'enquete de N" = "naiss_concue"),
  function(y, issue) imap_dfr(ECHANTILLONS, function(f, ech)
    modele(f(pan), y) %>% mutate(issue = issue, echantillon = ech, .before = 1)))
cat("\nEffet sur la probabilité de naissance (points ; références : emploi stable, femme 40 à 60 % des revenus) :\n")
print(as.data.frame(modeles %>% filter(!grepl("Non renseigne", terme)) %>%
                      mutate(across(where(is.double), ~ round(.x, 3)))), row.names = FALSE)

# ── Sortie ─────────────────────────────────────────────────────────────────
bind_rows(tendances %>% mutate(bloc = "Tendances"),
          appariement %>% select(-donnees) %>% mutate(bloc = "Appariement"),
          taux %>% mutate(bloc = "Taux de naissance"),
          modeles %>% mutate(bloc = "Modeles")) %>%
  relocate(bloc) %>%
  write_csv("Output/erfs_emploi_couple.csv")
cat("Résultats écrits : Output/erfs_emploi_couple.csv\n")
