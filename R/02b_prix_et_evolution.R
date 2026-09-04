# ============================================================================
#  SRCV — PRIX AU M² ET ÉVOLUTION AGRÉGÉE, 2006-2025
# ----------------------------------------------------------------------------
#  Seconde moitié de l'ancien 02_analyse_multimillesimes.R (sauvegardé dans
#  sauvegarde/02_analyse_multimillesimes_avant_scission.R).
#  La préparation des données est dans R/00_prepa_fecondite.R, partagée avec 02a.
#
#  CE QUE FAIT CE SCRIPT :
#   1) modèle POOLÉ sur les 8 vagues (probit, erreurs-types par ménage)
#   2) décomposition du taux d'effort en PRIX au m² × SURFACE
#   3) évolution du prix au m² : (A) nominal, (A') euros constants,
#      (B) rapporté au revenu, (C) comparaison intra-ménage
#   4) évolution agrégée par vague, sur deux champs
#
#  ⚠️ PONDÉRATION / PANEL — les vagues 2014-2018 et 2022-2025 revoient les mêmes
#     ménages : tous les modèles poolés regroupent les erreurs-types par
#     id_cluster. Les écarts entre vagues proches ne sont pas indépendants.
#
#  Sorties : Output/evolution_agregee.csv
#            Output/modele_poole_probit.csv
#            Output/modele_prix_m2_surface.csv
#            Output/densite_cout_m2.png
# ============================================================================

source("R/00_prepa_fecondite.R")   # preparer_donnees(), champ, VARS_INTERET

library(survey)
library(broom)
library(marginaleffects)

IPC <- charger_ipc()

# ── Données : 8 millésimes, champ "couples féconds" (cf. 00_prepa_fecondite) ─
donnees <- charger_millesimes()
dfs     <- imap(donnees, ~ .x %>% select(any_of(VARS_INTERET)))
df_tot  <- bind_rows(dfs)

cat("df_tot :", nrow(df_tot), "obs |", n_distinct(df_tot$annee), "millésimes :",
    paste(sort(unique(df_tot$annee)), collapse = ", "), "\n")

# EUROS CONSTANTS : déflation par l'IPC INSEE (base 2025, hors tabac).
#  Coûts relevés à la date d'enquête -> déflatés par l'indice de l'année N.
#  Revenu HY020 (année N-1) -> déflaté par l'indice de N-1.
#  Inflation cumulée vs 2025 : +33,5 % depuis 2006, +7,7 % depuis 2022.
df_tot <- df_tot %>%
  mutate(annee_num     = as.integer(annee),
         cout_m2_reel  = deflater(cout_m2,  annee_num,     IPC, base = 2025),
         cout_log_reel = deflater(cout_log, annee_num,     IPC, base = 2025),
         revenu_reel   = deflater(revenu,   annee_num - 1, IPC, base = 2025))

# ── 1) MODÈLE POOLÉ sur les 8 vagues ────────────────────────────────────────
# Erreurs-types regroupées par MÉNAGE (id_cluster) : cf. avertissement panel.
design <- svydesign(ids = ~id_cluster, weights = ~poids, data = df_tot)

modele_poole <- svyglm(naissance ~ taux_effort_pct * statut3 +
                         taille_avant + nb_enf_avant + pieces * demenage +
                         age_c + I(age_c^2) + urbain + zeat + factor(annee),
                       design = design,
                       family = quasibinomial(link = "probit"))

cat("\n=== MODÈLE POOLÉ (probit) — effets marginaux moyens ===\n")
ame_poole <- avg_slopes(modele_poole, wts = modele_poole$prior.weights)
print(ame_poole)

cat("\n=== Effet du taux d'effort PAR STATUT D'OCCUPATION ===\n")
ame_statut <- avg_slopes(modele_poole, variables = "taux_effort_pct",
                         by = "statut3", wts = modele_poole$prior.weights)
print(ame_statut)

# ── 2) DÉCOMPOSITION du taux d'effort en PRIX au m² × SURFACE ───────────────
# Identité : taux_effort = (cout_m2 * surface * 12) / revenu. En introduisant
# cout_m2 et surface séparément (revenu contrôlé), on sépare :
#   - cout_m2  = prix unitaire du logement (pression du marché local)
#   - surface  = quantité de logement consommée (choix / contrainte de place)
# Le taux d'effort agrégé confondait les deux.
df_m2 <- preparer_df_m2(df_tot)     # 00_prepa_fecondite.R

cat("\n=== Décomposition prix au m² × surface (couples, femme",
    AGE_MIN, "-", AGE_MAX, "ans) ===\n")
cat("n =", nrow(df_m2), "| naissances :", sum(df_m2$naissance == "Oui"), "\n")
cat("cout_m2 (€/m²/mois) : médiane", round(median(df_m2$cout_m2), 2),
    "| surface médiane", round(median(df_m2$surface)), "m²\n")

modele_m2 <- svyglm(F_DECOMPOSITION,        # 00_prepa_fecondite.R
                    design = svydesign(ids = ~id_cluster, weights = ~poids, data = df_m2),
                    family = quasibinomial(link = "probit"))
ame_m2 <- avg_slopes(modele_m2, variables = c("cout_m2", "surface_10", "revenu_10k"),
                     wts = modele_m2$prior.weights)
print(ame_m2)

if (!dir.exists("Output")) dir.create("Output")
write_csv(bind_rows(as_tibble(ame_poole)  %>% mutate(modele = "poole_taux_effort"),
                    as_tibble(ame_statut) %>% mutate(modele = "poole_par_statut")),
          "Output/modele_poole_probit.csv")
write_csv(as_tibble(ame_m2), "Output/modele_prix_m2_surface.csv")

# ── Densité du prix au m² par vague et statut ───────────────────────────────
# (allait auparavant dans Rplots.pdf à la racine : maintenant un vrai fichier)
g_dens <- df_tot %>%
  group_by(annee) %>% mutate(w = poids / sum(poids)) %>% ungroup() %>%
  ggplot(aes(x = cout_m2, colour = factor(annee), linetype = statut3, weight = w)) +
  geom_density(linewidth = 0.8) +
  labs(x = "Prix au m² (€/mois, euros courants)", y = "Densité",
       colour = "Année", linetype = "Statut") +
  theme_minimal()
ggsave("Output/densite_cout_m2.png", g_dens, width = 10, height = 6, dpi = 150, bg = "white")

# ============================================================================
#  3) ÉVOLUTION DU PRIX AU M²
# ----------------------------------------------------------------------------
#  ⚠️ cout_m2 est en EUROS COURANTS : une comparaison brute entre années
#     confond hausse réelle et inflation. D'où les trois approches ci-dessous.
#  ⚠️ Le contrôle par le revenu corrige l'évolution du pouvoir d'achat des
#     ménages, PAS l'inflation des autres biens. Pour de l'euro constant,
#     c'est (A') qui fait foi.
#
#  NB REVENU : `revenu` = HY020 = REVENU DISPONIBLE TOTAL du ménage. Il inclut
#  les revenus du travail ET du capital ET toutes les prestations sociales,
#  nets d'impôts et de cotisations. Ce n'est pas le seul salaire.
#
#  NB STATUT : HH070 ne mesure pas la même chose selon le statut d'occupation.
#  - LOCATAIRES : loyer + charges -> proche d'un prix de marché du logement.
#  - PROPRIÉTAIRES : intérêts d'emprunt + taxes + charges + entretien, HORS
#    remboursement du capital ; quasi nul pour un propriétaire non accédant.
#  Les NIVEAUX ne sont donc pas comparables entre groupes ; seules les
#  ÉVOLUTIONS au sein de chaque groupe le sont. La série "locataires" est la
#  plus proche d'un indicateur de prix du logement.
# ============================================================================

ANNEE_REF <- "2025"     # année de référence des comparaisons

d_prix <- df_tot %>%
  filter(!is.na(cout_m2), is.finite(cout_m2), cout_m2 > 0,
         !is.na(revenu), revenu > 0, !is.na(surface), !is.na(poids), poids > 0) %>%
  mutate(annee_f          = relevel(factor(annee), ref = ANNEE_REF),
         log_cout_m2      = log(cout_m2),
         log_cout_m2_reel = log(cout_m2_reel),   # euros constants 2025
         log_revenu       = log(revenu))

# statut_occ n'est plus dans la formule quand on travaille dans un groupe
# (il y serait constant).
analyser_prix <- function(d, libelle, avec_statut = FALSE) {
  cat("\n##########", libelle, "— n =", nrow(d), "##########\n")
  des <- svydesign(ids = ~id_cluster, weights = ~poids, data = d)

  cat("\n-- Prix au m² moyen pondéré (€/m²/mois) --\n")
  print(svyby(~cout_m2, ~annee, des, svymean, na.rm = TRUE))

  f_a <- if (avec_statut)
    log_cout_m2 ~ annee_f + log_revenu + statut3 + urbain + zeat + surface + taille_avant
  else
    log_cout_m2 ~ annee_f + log_revenu + urbain + zeat + surface + taille_avant
  mA <- svyglm(f_a, design = des)
  cat("\n-- (A) log(prix au m²) ~ année (réf.", ANNEE_REF, ") + revenu + contrôles --\n")
  print(tidy(mA, conf.int = TRUE) %>%
          filter(str_starts(term, "annee_f") | term == "log_revenu") %>%
          mutate(ecart_pct = round(100 * (exp(estimate) - 1), 1)) %>%
          select(term, ecart_pct, conf.low, conf.high, p.value))

  # (A') Évolution RÉELLE : même modèle sur le coût déflaté par l'IPC ->
  # les coefficients d'année mesurent l'écart de prix HORS inflation générale.
  mA2 <- svyglm(update(f_a, log_cout_m2_reel ~ .), design = des)
  cat("\n-- (A') log(prix au m² RÉEL, € constants 2025) ~ année --\n")
  print(tidy(mA2, conf.int = TRUE) %>%
          filter(str_starts(term, "annee_f")) %>%
          mutate(ecart_pct = round(100 * (exp(estimate) - 1), 1)) %>%
          select(term, ecart_pct, p.value))

  f_b <- if (avec_statut)
    log(taux_effort_m2) ~ annee_f + statut3 + urbain + zeat + surface + taille_avant
  else
    log(taux_effort_m2) ~ annee_f + urbain + zeat + surface + taille_avant
  mB <- svyglm(f_b, design = des)
  cat("\n-- (B) log(effort par m², = prix rapporté au revenu) ~ année --\n")
  print(tidy(mB, conf.int = TRUE) %>%
          filter(str_starts(term, "annee_f")) %>%
          mutate(ecart_pct = round(100 * (exp(estimate) - 1), 1)) %>%
          select(term, ecart_pct, p.value))
  invisible(NULL)
}

# ── (C) Comparaison INTRA-MÉNAGE (mêmes ménages revus) ─────────────────────
# Chaînage possible uniquement à l'intérieur d'un groupe de panel -> 2022-2025.
# On exige le MÊME statut d'occupation aux deux dates, pour ne pas mélanger
# l'évolution des prix avec les changements de statut (accession, etc.).
comparer_paire <- function(a1, a2, statut = NULL) {
  p <- df_tot %>%
    filter(annee %in% c(a1, a2), !is.na(cout_m2), is.finite(cout_m2),
           cout_m2 > 0, !is.na(revenu), revenu > 0) %>%
    select(id_cluster, annee, cout_m2, revenu, poids, statut3) %>%
    pivot_wider(names_from = annee,
                values_from = c(cout_m2, revenu, poids, statut3)) %>%
    rename(c1 = paste0("cout_m2_", a1),   c2 = paste0("cout_m2_", a2),
           r1 = paste0("revenu_", a1),    r2 = paste0("revenu_", a2),
           s1 = paste0("statut3_", a1), s2 = paste0("statut3_", a2),
           w2 = paste0("poids_", a2)) %>%
    filter(!is.na(c1), !is.na(c2), !is.na(w2), !is.na(s1), !is.na(s2), s1 == s2)
  if (!is.null(statut)) p <- p %>% filter(s1 == statut)
  if (nrow(p) < 30) { cat("  (effectif insuffisant :", nrow(p), ")\n"); return(invisible(NULL)) }

  # Évolutions en EUROS CONSTANTS : on retire l'inflation entre a1 et a2.
  # d_relatif est un rapport prix/revenu -> l'inflation s'y annule d'elle-même.
  infl <- log(IPC$ipc[IPC$annee == as.integer(a2)] / IPC$ipc[IPC$annee == as.integer(a1)])
  p <- p %>% mutate(d_log_prix   = log(c2) - log(c1) - infl,
                    d_log_revenu = log(r2) - log(r1) - infl,
                    d_relatif    = (log(c2) - log(c1)) - (log(r2) - log(r1)))
  cat("\n--- ", a1, " -> ", a2, " | ", ifelse(is.null(statut), "ensemble", statut),
      " (n = ", nrow(p), ") ---\n", sep = "")
  des <- svydesign(ids = ~1, weights = ~w2, data = p)
  for (v in c("d_log_prix", "d_log_revenu", "d_relatif")) {
    m <- svyglm(as.formula(paste(v, "~ 1")), design = des)
    est <- coef(m)[1]; ic <- confint(m)[1, ]
    cat(sprintf("  %-13s : %+6.2f %% [%+6.2f ; %+6.2f]  p = %.3g\n",
                v, 100*(exp(est)-1), 100*(exp(ic[1])-1), 100*(exp(ic[2])-1),
                coef(summary(m))[1, 4]))
  }
  invisible(NULL)
}

analyser_prix(d_prix, "ENSEMBLE", avec_statut = TRUE)
analyser_prix(filter(d_prix, statut3 == "Locataire"),                 "LOCATAIRES")
analyser_prix(filter(d_prix, statut3 == "Proprietaire_accedant"),     "PROPRIÉTAIRES ACCÉDANTS")
analyser_prix(filter(d_prix, statut3 == "Proprietaire_non_accedant"), "PROPRIÉTAIRES NON ACCÉDANTS")

cat("\n\n=== (C) Évolution INTRA-MÉNAGE vers", ANNEE_REF, "(statut constant) ===\n")
for (st in list(NULL, "Locataire", "Proprietaire_accedant", "Proprietaire_non_accedant"))
  for (a in c("2022", "2023", "2024")) comparer_paire(a, ANNEE_REF, st)

# ============================================================================
#  4) ÉVOLUTION AGRÉGÉE : moyennes pondérées par vague
# ----------------------------------------------------------------------------
#  Lecture MACRO, complémentaire des modèles (A)/(A')/(B) :
#   - pas de suivi de ménages (contrairement au test intra-ménage (C)),
#   - pas de contrôle de caractéristiques : on prend la MOYENNE PONDÉRÉE de
#     chaque vague, donc une population représentative à chaque date.
#  L'évolution mêle donc effet prix ET déformation de la population (âge,
#  statut d'occupation, taille des logements...) — c'est voulu ici.
#
#  Produite sur DEUX champs :
#   1. TOUS LES MÉNAGES  -> lecture macro d'ensemble.
#   2. COUPLES FÉCONDS (femme AGE_MIN-AGE_MAX ans) -> conditions de logement de
#      la population effectivement susceptible d'avoir un enfant.
# ============================================================================

prep_agrege <- function(d) {
  d %>%
    mutate(annee_num    = as.integer(annee),
           cout_m2_reel = deflater(cout_m2,  annee_num,     IPC, base = 2025),
           revenu_reel  = deflater(revenu,   annee_num - 1, IPC, base = 2025),
           cout_an      = cout_log * 12,                                # coût annuel
           cout_an_reel = deflater(cout_an,  annee_num,     IPC, base = 2025)) %>%
    filter(!is.na(cout_m2), is.finite(cout_m2), cout_m2 > 0,
           !is.na(revenu), revenu > 0, !is.na(poids), poids > 0)
}

analyse_agregee <- function(d, libelle) {
  cat("\n\n##########", libelle, "— n =", nrow(d), "##########\n")
  des <- svydesign(ids = ~1, weights = ~poids, data = d)

  agg <- svyby(~cout_m2 + cout_m2_reel + revenu + revenu_reel + cout_an,
               ~annee, des, svymean, na.rm = TRUE) %>%
    as_tibble() %>%
    transmute(annee,
              cout_m2_nom  = round(cout_m2, 2),  cout_m2_reel = round(cout_m2_reel, 2),
              revenu_nom   = round(revenu),      revenu_reel  = round(revenu_reel),
              # taux d'effort AGRÉGÉ = rapport des moyennes (coût annuel / revenu)
              effort_agrege_pct = round(100 * cout_an / revenu, 1))
  cat("\n-- Moyennes pondérées par vague --\n"); print(as.data.frame(agg))

  base <- agg %>% filter(annee == "2006")
  idx <- agg %>%
    transmute(annee,
              idx_cout_m2_nom  = round(100 * cout_m2_nom  / base$cout_m2_nom),
              idx_revenu_nom   = round(100 * revenu_nom   / base$revenu_nom),
              idx_cout_m2_reel = round(100 * cout_m2_reel / base$cout_m2_reel),
              idx_revenu_reel  = round(100 * revenu_reel  / base$revenu_reel),
              effort_agrege_pct)
  cat("\n-- Indices base 100 en 2006 --\n"); print(as.data.frame(idx))

  cat("\n-- Prix au m² moyen (€ constants 2025) par statut --\n")
  print(svyby(~cout_m2_reel, ~annee + statut3, des, svymean, na.rm = TRUE) %>%
          as_tibble() %>% transmute(annee, statut3, v = round(cout_m2_reel, 2)) %>%
          pivot_wider(names_from = statut3, values_from = v) %>% as.data.frame())

  cat("\n-- Taux d'effort agrégé (coût annuel moyen / revenu moyen, %) par statut --\n")
  print(svyby(~cout_an + revenu, ~annee + statut3, des, svymean, na.rm = TRUE) %>%
          as_tibble() %>% transmute(annee, statut3, v = round(100 * cout_an / revenu, 1)) %>%
          pivot_wider(names_from = statut3, values_from = v) %>% as.data.frame())
  agg
}

# Champ 1 : tous les ménages (indépendant de RESTREINDRE_FECONDITE)
df_tous   <- bind_rows(charger_millesimes(restreindre = FALSE)) %>% prep_agrege()
agg_tous  <- analyse_agregee(df_tous, "TOUS LES MÉNAGES")

# Champ 2 : couples féconds (femme AGE_MIN-AGE_MAX ans)
df_fecond  <- bind_rows(charger_millesimes(restreindre = TRUE)) %>% prep_agrege()
agg_fecond <- analyse_agregee(
  df_fecond, paste0("COUPLES FÉCONDS (femme ", AGE_MIN, "-", AGE_MAX, " ans)"))

write_csv(bind_rows(agg_tous   %>% mutate(champ = "Tous ménages"),
                    agg_fecond %>% mutate(champ = "Couples féconds")),
          "Output/evolution_agregee.csv")
cat("\nExporté : Output/evolution_agregee.csv, modele_poole_probit.csv,",
    "modele_prix_m2_surface.csv, densite_cout_m2.png\n")

# ============================================================================
#  5) FICHIER MÉNAGE : toutes les variables mobilisées dans ce script
# ----------------------------------------------------------------------------
#  Une ligne = un MÉNAGE × MILLÉSIME (et non un ménage unique) : les vagues
#  2014-2018 et 2022-2025 sont des panels, un même `id_cluster` revient donc
#  jusqu'à 4 fois. Pour un fichier de ménages uniques, filtrer sur une vague.
#
#  Champ : celui des modèles de ce script, soit les COUPLES FÉCONDS (femme
#  AGE_MIN-AGE_MAX ans) — cf. RESTREINDRE_FECONDITE dans 00_prepa_fecondite.R.
#  Le champ "tous ménages" ne sert qu'à la lecture macro agrégée ; mettre
#  EXPORT_TOUS_MENAGES à TRUE pour l'exporter aussi.
#
#  Trois indicatrices disent dans quelle analyse chaque ligne est entrée :
#    dans_decomposition : modèle prix au m² × surface (section 2)
#    dans_modeles_prix  : modèles (A)/(A')/(B) d'évolution des prix (section 3)
#    dans_agrege        : moyennes pondérées par vague (section 4)
#
#  ⚠️ MICRO-DONNÉES : `Output/` est gitignoré, donc ce fichier n'ira pas sur le
#     dépôt public. Pour le ranger hors de toute arborescence git, remplacer
#     DEST par "../srcv_prive/".
# ============================================================================

DEST                 <- "Output/"
EXPORT_TOUS_MENAGES  <- FALSE

menages <- df_tot %>%
  mutate(
    # Variables dérivées utilisées par les modèles, rendues explicites
    revenu_10k   = revenu / 10000,
    surface_10   = surface / 10,
    cout_an      = cout_log * 12,
    cout_an_reel = deflater(cout_an, annee_num, IPC, base = 2025),
    log_cout_m2      = if_else(cout_m2 > 0, log(cout_m2), NA_real_),
    log_cout_m2_reel = if_else(cout_m2_reel > 0, log(cout_m2_reel), NA_real_),
    log_revenu       = if_else(revenu > 0, log(revenu), NA_real_),
    # Appartenance aux sous-échantillons d'analyse
    dans_decomposition = !is.na(cout_m2) & !is.na(surface) & is.finite(cout_m2),
    dans_modeles_prix  = !is.na(cout_m2) & is.finite(cout_m2) & cout_m2 > 0 &
                         !is.na(revenu) & revenu > 0 & !is.na(surface) &
                         !is.na(poids) & poids > 0,
    dans_agrege        = !is.na(cout_m2) & is.finite(cout_m2) & cout_m2 > 0 &
                         !is.na(revenu) & revenu > 0 & !is.na(poids) & poids > 0
  ) %>%
  select(
    # identification
    annee, annee_num, id, id_cluster, poids,
    # issue
    naissance, nb_enfants, nb_enf_avant, taille_avant,
    # logement : statut, quantité, prix
    statut_occ, statut3, surface, surface_10, pieces, surface_piece,
    cout_log, cout_log_hors_emprunt, rembours, cout_an,
    cout_m2, log_cout_m2,
    # versions en euros constants 2025
    cout_log_reel, cout_an_reel, cout_m2_reel, log_cout_m2_reel,
    # revenu et effort
    revenu, revenu_reel, revenu_10k, log_revenu,
    taux_effort, taux_effort_pct, taux_effort_m2,
    # démographie, géographie, mobilité
    couple, age_pr, age_femme, age_c, csp, urbain, zeat, demenage,
    # sous-échantillons
    dans_decomposition, dans_modeles_prix, dans_agrege
  ) %>%
  arrange(annee, id)

f_men <- paste0(DEST, "menages_analyse.csv")
write_csv(menages, f_men)

cat("\n=== Fichier ménage exporté ===\n")
cat(sprintf("  %s : %d lignes (ménage x millésime), %d variables\n",
            f_men, nrow(menages), ncol(menages)))
print(as.data.frame(menages %>% count(annee, name = "lignes") %>%
        left_join(menages %>% group_by(annee) %>%
                    summarise(menages_distincts = n_distinct(id_cluster),
                              decomposition = sum(dans_decomposition),
                              modeles_prix  = sum(dans_modeles_prix),
                              agrege        = sum(dans_agrege), .groups = "drop"),
                  by = "annee")), row.names = FALSE)

if (EXPORT_TOUS_MENAGES) {
  f_tous <- paste0(DEST, "menages_analyse_tous.csv")
  write_csv(bind_rows(charger_millesimes(restreindre = FALSE)) %>%
              select(any_of(names(menages))), f_tous)
  cat("  ", f_tous, " : champ 'tous ménages' (lecture macro agrégée)\n", sep = "")
}
