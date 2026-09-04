# ============================================================================
#  SRCV — A. LOGEMENT ET FÉCONDITÉ : les modèles
# ----------------------------------------------------------------------------
#  Regroupe R/02a, R/02b, R/06, R/07, R/15 et R/16.
#
#  POURQUOI CE REGROUPEMENT : ces six scripts partaient tous de la même
#  préparation des 8 vagues (~7 s) et 06 -> 07 -> 15 échangeaient un CSV
#  intermédiaire de 11 Mo. Ici la préparation est faite UNE fois, mise en cache
#  disque, et le panel circule en mémoire. Le CSV du panel reste exporté pour
#  compatibilité, mais n'est plus relu.
#
#  À LANCER DEPUIS LA RACINE DU PROJET :
#     source("R compact/A_fecondite_modeles.R")
#
#  Sorties : resultats_principal.csv, resultats_crosscheck.csv,
#            crosscheck_demenage_cle.csv, evolution_agregee.csv,
#            modele_poole_probit.csv, modele_prix_m2_surface.csv,
#            densite_cout_m2.png, menages_analyse.csv,
#            panel_naissance_logement.csv, modele_naissance_logement.csv,
#            test_non_demenageurs.csv, scenario_surface.csv
# ============================================================================

source("R/00_prepa_fecondite.R")
library(survey); library(broom); library(marginaleffects)

IPC <- charger_ipc()
if (!dir.exists("Output")) dir.create("Output")

# ── PRÉPARATION PARTAGÉE : une seule fois, en cache ─────────────────────────
t0 <- Sys.time()
DONNEES_FEC  <- millesimes_prepares(restreindre = TRUE)    # couples, femme 15-49
DONNEES_TOUS <- millesimes_prepares(restreindre = FALSE)   # tous les ménages
cat("Préparation des 8 vagues :",
    round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 1), "s\n")

df_tot <- bind_rows(imap(DONNEES_FEC, ~ .x %>% select(any_of(VARS_INTERET)))) %>%
  mutate(annee_num     = as.integer(annee),
         cout_m2_reel  = deflater(cout_m2,  annee_num,     IPC, base = 2025),
         cout_log_reel = deflater(cout_log, annee_num,     IPC, base = 2025),
         revenu_reel   = deflater(revenu,   annee_num - 1, IPC, base = 2025))

# ============================================================================
#  §1 — MODÈLES PAR MILLÉSIME  (ex-R/02a)
# ----------------------------------------------------------------------------
#  Contrôles en version PRÉ-NAISSANCE (nb_enfants, taille_avant) : NENFANTS et
#  la taille comptent le nouveau-né, ce serait un bad control.
#  ids = ~1 : dans une vague donnée un ménage n'apparaît qu'une fois.
# ============================================================================
cat("\n\n########## §1 — MODÈLES PAR MILLÉSIME ##########\n")

if (RESTREINDRE_FECONDITE) {
  f_principale <- naissance ~ taux_effort_pct + statut3 + nb_enfants +
    taille_avant + csp + pieces * demenage + age_c + I(age_c^2) + urbain + zeat
  f_crosscheck <- naissance ~ taux_effort_pct * demenage + statut3 +
    nb_enfants + taille_avant + csp + pieces + age_c + I(age_c^2) + urbain + zeat
} else {
  f_principale <- naissance ~ taux_effort_pct + statut3 + nb_enfants +
    taille_avant + csp + pieces * demenage + couple + urbain + zeat
  f_crosscheck <- naissance ~ taux_effort_pct * demenage + statut3 +
    nb_enfants + taille_avant + csp + pieces + couple + urbain + zeat
}

ajuster <- function(d, f)
  svyglm(f, design = svydesign(ids = ~1, weights = ~poids, data = d),
         family = quasibinomial())
resumer <- function(m, an)
  tidy(m, conf.int = TRUE, exponentiate = TRUE) %>%
    mutate(annee = an, n_modele = stats::nobs(m), .before = 1)

iwalk(DONNEES_FEC, function(d, an)
  message("  ", an, " : n = ", nrow(d), " | naissances = ", sum(d$naissance == "Oui"),
          " | effort censuré à 100 % : ", sum(d$taux_effort_pct >= 100)))

resultats_principal  <- imap_dfr(DONNEES_FEC, ~ resumer(ajuster(.x, f_principale), .y))
resultats_crosscheck <- imap_dfr(DONNEES_FEC, ~ resumer(ajuster(.x, f_crosscheck), .y))

cat("\nOdds ratio du TAUX D'EFFORT (par +1 point) :\n")
print(resultats_principal %>% filter(term == "taux_effort_pct") %>%
        select(annee, n_modele, odds_ratio = estimate, conf.low, conf.high, p.value),
      n = Inf)

crosscheck_cle <- resultats_crosscheck %>%
  filter(term %in% c("taux_effort_pct", "demenageOui", "taux_effort_pct:demenageOui")) %>%
  select(annee, term, odds_ratio = estimate, conf.low, conf.high, p.value)
cat("\nCross-check taux d'effort x déménagement :\n"); print(crosscheck_cle, n = Inf)

write_csv(resultats_principal,  "Output/resultats_principal.csv")
write_csv(resultats_crosscheck, "Output/resultats_crosscheck.csv")
write_csv(crosscheck_cle,       "Output/crosscheck_demenage_cle.csv")

# ============================================================================
#  §2 — MODÈLE POOLÉ, PRIX AU M² ET ÉVOLUTION AGRÉGÉE  (ex-R/02b)
# ----------------------------------------------------------------------------
#  Erreurs-types regroupées par ménage (id_cluster) : 2014-2018 et 2022-2025
#  revoient les mêmes ménages.
# ============================================================================
cat("\n\n########## §2 — MODÈLE POOLÉ ET PRIX AU M² ##########\n")

design <- svydesign(ids = ~id_cluster, weights = ~poids, data = df_tot)
modele_poole <- svyglm(naissance ~ taux_effort_pct * statut3 + taille_avant +
                         nb_enf_avant + pieces * demenage + age_c + I(age_c^2) +
                         urbain + zeat + factor(annee),
                       design = design, family = quasibinomial(link = "probit"))
ame_poole  <- avg_slopes(modele_poole, wts = modele_poole$prior.weights)
ame_statut <- avg_slopes(modele_poole, variables = "taux_effort_pct",
                         by = "statut3", wts = modele_poole$prior.weights)
print(ame_statut)

df_m2 <- preparer_df_m2(df_tot)
modele_m2 <- svyglm(F_DECOMPOSITION,
                    design = svydesign(ids = ~id_cluster, weights = ~poids, data = df_m2),
                    family = quasibinomial(link = "probit"))
ame_m2 <- avg_slopes(modele_m2, variables = c("cout_m2", "surface_10", "revenu_10k"),
                     wts = modele_m2$prior.weights)
cat("\nDécomposition prix au m² x surface :\n"); print(ame_m2)

write_csv(bind_rows(as_tibble(ame_poole)  %>% mutate(modele = "poole_taux_effort"),
                    as_tibble(ame_statut) %>% mutate(modele = "poole_par_statut")),
          "Output/modele_poole_probit.csv")
write_csv(as_tibble(ame_m2), "Output/modele_prix_m2_surface.csv")

ggsave("Output/densite_cout_m2.png",
       df_tot %>% group_by(annee) %>% mutate(w = poids / sum(poids)) %>% ungroup() %>%
         ggplot(aes(cout_m2, colour = factor(annee), linetype = statut3, weight = w)) +
         geom_density(linewidth = 0.8) +
         labs(x = "Prix au m² (€/mois, euros courants)", y = "Densité",
              colour = "Année", linetype = "Statut") + theme_minimal(),
       width = 10, height = 6, dpi = 150, bg = "white")

# ── Évolution du prix au m² : nominal, réel, rapporté au revenu ─────────────
#  HH070 ne mesure pas la même chose selon le statut : loyer + charges pour un
#  locataire, intérêts + taxes + charges pour un propriétaire, hors capital.
#  Les NIVEAUX ne sont pas comparables entre groupes, seules les ÉVOLUTIONS
#  au sein d'un groupe le sont.
ANNEE_REF <- "2025"
d_prix <- df_tot %>%
  filter(!is.na(cout_m2), is.finite(cout_m2), cout_m2 > 0, !is.na(revenu),
         revenu > 0, !is.na(surface), !is.na(poids), poids > 0) %>%
  mutate(annee_f = relevel(factor(annee), ref = ANNEE_REF),
         log_cout_m2 = log(cout_m2), log_cout_m2_reel = log(cout_m2_reel),
         log_revenu = log(revenu))

analyser_prix <- function(d, libelle, avec_statut = FALSE) {
  cat("\n##########", libelle, "— n =", nrow(d), "##########\n")
  des <- svydesign(ids = ~id_cluster, weights = ~poids, data = d)
  f_a <- if (avec_statut)
    log_cout_m2 ~ annee_f + log_revenu + statut3 + urbain + zeat + surface + taille_avant
  else log_cout_m2 ~ annee_f + log_revenu + urbain + zeat + surface + taille_avant
  for (nom in c("(A) nominal", "(A') euros constants 2025")) {
    f <- if (nom == "(A) nominal") f_a else update(f_a, log_cout_m2_reel ~ .)
    cat("\n--", nom, "--\n")
    print(tidy(svyglm(f, design = des), conf.int = TRUE) %>%
            filter(str_starts(term, "annee_f")) %>%
            mutate(ecart_pct = round(100 * (exp(estimate) - 1), 1)) %>%
            select(term, ecart_pct, p.value))
  }
}
analyser_prix(d_prix, "ENSEMBLE", avec_statut = TRUE)
for (st in c("Locataire", "Proprietaire_accedant", "Proprietaire_non_accedant"))
  analyser_prix(filter(d_prix, statut3 == st), st)

# ── Évolution agrégée par vague, deux champs ────────────────────────────────
prep_agrege <- function(d) {
  d %>% mutate(annee_num = as.integer(annee),
               cout_m2_reel = deflater(cout_m2, annee_num, IPC, base = 2025),
               revenu_reel  = deflater(revenu, annee_num - 1, IPC, base = 2025),
               cout_an = cout_log * 12) %>%
    filter(!is.na(cout_m2), is.finite(cout_m2), cout_m2 > 0,
           !is.na(revenu), revenu > 0, !is.na(poids), poids > 0)
}
analyse_agregee <- function(d, libelle) {
  cat("\n\n##########", libelle, "— n =", nrow(d), "##########\n")
  des <- svydesign(ids = ~1, weights = ~poids, data = d)
  agg <- svyby(~cout_m2 + cout_m2_reel + revenu + revenu_reel + cout_an,
               ~annee, des, svymean, na.rm = TRUE) %>% as_tibble() %>%
    transmute(annee, cout_m2_nom = round(cout_m2, 2),
              cout_m2_reel = round(cout_m2_reel, 2),
              revenu_nom = round(revenu), revenu_reel = round(revenu_reel),
              effort_agrege_pct = round(100 * cout_an / revenu, 1))
  print(as.data.frame(agg))
  cat("\n-- Taux d'effort agrégé par statut --\n")
  print(svyby(~cout_an + revenu, ~annee + statut3, des, svymean, na.rm = TRUE) %>%
          as_tibble() %>% transmute(annee, statut3, v = round(100 * cout_an / revenu, 1)) %>%
          pivot_wider(names_from = statut3, values_from = v) %>% as.data.frame())
  agg
}
agg_tous   <- analyse_agregee(bind_rows(DONNEES_TOUS) %>% prep_agrege(), "TOUS LES MÉNAGES")
agg_fecond <- analyse_agregee(bind_rows(DONNEES_FEC)  %>% prep_agrege(),
                              paste0("COUPLES FÉCONDS (femme ", AGE_MIN, "-", AGE_MAX, " ans)"))
write_csv(bind_rows(agg_tous %>% mutate(champ = "Tous ménages"),
                    agg_fecond %>% mutate(champ = "Couples féconds")),
          "Output/evolution_agregee.csv")

# ── Fichier ménage : toutes les variables mobilisées ────────────────────────
#  Une ligne = un ménage x millésime (2014-2018 et 2022-2025 sont des panels,
#  un id_cluster revient donc plusieurs fois).
write_csv(df_tot %>%
  mutate(revenu_10k = revenu / 10000, surface_10 = surface / 10,
         cout_an = cout_log * 12,
         cout_an_reel = deflater(cout_an, annee_num, IPC, base = 2025),
         log_cout_m2      = if_else(cout_m2 > 0, log(cout_m2), NA_real_),
         log_cout_m2_reel = if_else(cout_m2_reel > 0, log(cout_m2_reel), NA_real_),
         log_revenu       = if_else(revenu > 0, log(revenu), NA_real_),
         dans_decomposition = !is.na(cout_m2) & !is.na(surface) & is.finite(cout_m2),
         dans_modeles_prix  = dans_decomposition & cout_m2 > 0 & revenu > 0 &
                              !is.na(poids) & poids > 0,
         dans_agrege        = !is.na(cout_m2) & is.finite(cout_m2) & cout_m2 > 0 &
                              !is.na(revenu) & revenu > 0 & !is.na(poids) & poids > 0) %>%
  select(annee, annee_num, id, id_cluster, poids, naissance, nb_enfants, nb_enf_avant,
         taille_avant, statut_occ, statut3, surface, surface_10, pieces, surface_piece,
         cout_log, cout_log_hors_emprunt, rembours, cout_an, cout_m2, log_cout_m2,
         cout_log_reel, cout_an_reel, cout_m2_reel, log_cout_m2_reel,
         revenu, revenu_reel, revenu_10k, log_revenu, taux_effort,
         taux_effort_pct, taux_effort_m2, couple, age_pr, age_femme, age_c, csp,
         urbain, zeat, demenage,
         dans_decomposition, dans_modeles_prix, dans_agrege) %>%
  arrange(annee, id), "Output/menages_analyse.csv")

# ============================================================================
#  §3 — PANEL FPR, MODÈLE, TEST DES NON-DÉMÉNAGEURS ET SCÉNARIO
#       (ex-R/06, R/07, R/15, R/16)
# ----------------------------------------------------------------------------
#  Le panel circule EN MÉMOIRE : 07, 15 et 16 ne relisent plus le CSV de 11 Mo.
# ============================================================================
cat("\n\n########## §3 — PANEL FPR ET MODÈLES DE TRANSITION ##########\n")

waves <- sort(as.integer(vagues_fpr()))
COLS_MEN <- c("DB030","SURFACE","NPIECES","HH070","REMPR","VFEPPMIN","VFEPPMAX","STOC",
              "EMMENAG","NENFANTS","AGEPR","AGECJ","SEXEPR","SEXECJ","HY020","DIPDETPR",
              "PCSM","NACTOCCUP","ZEAT","COUPLEPR","DB090")

charger_men <- function(path, annee) {
  d <- lire_srcv(path); verifier_colonnes(d, COLS_MEN, paste0("(ménages ", annee, ")"))
  d %>% transmute(
    id = trimws(as.character(DB030)), annee = annee,
    surface = num(SURFACE), npieces = num(NPIECES),
    rembours = coalesce(num(REMPR), 0),
    cout_log = num(HH070) + coalesce(num(REMPR), 0),
    vfepp = (num(VFEPPMIN) + num(VFEPPMAX)) / 2,
    stoc = trimws(as.character(STOC)), emmenag = num(EMMENAG), nenf = num(NENFANTS),
    cout_log_hors_emprunt = num(HH070),
    agepr = num(AGEPR), agecj = num(AGECJ),
    sexepr = trimws(as.character(SEXEPR)), sexecj = trimws(as.character(SEXECJ)),
    revenu = num(HY020), dipl_pr = trimws(as.character(DIPDETPR)),
    csp = trimws(as.character(PCSM)), nactocc = num(NACTOCCUP),
    zeat = recode_zeat(ZEAT), couple = trimws(as.character(COUPLEPR)),
    poids = num(DB090)) %>%
  mutate(age_femme = case_when(sexepr == "2" ~ agepr, sexecj == "2" ~ agecj,
                               TRUE ~ NA_real_),
         proprio = if_else(stoc %in% c("1","2","3","6"), 1L,
                           if_else(stoc %in% c("4","5"), 0L, NA_integer_)),
         statut_occ = factor(proprio, levels = c(0, 1),
                             labels = c("Locataire", "Proprietaire_ou_gratuit")),
         proprio_f  = factor(proprio, levels = c(0, 1),
                             labels = c("Locataire", "Proprietaire")),
         statut3 = recode_statut3(statut_occ, rembours))
}
charger_ind <- function(path) lire_srcv(path) %>%
  transmute(id = trimws(as.character(RB040)), pid = trimws(as.character(RB030)),
            anais = num(ANAIS))

men_all <- map2(map_chr(as.character(waves), chemin_men), waves, charger_men)
ind_all <- map(map_chr(as.character(waves), chemin_ind), charger_ind)

# Naissance = un individu né en t ou t+1, présent en t+1 et absent en t
panel <- map_dfr(seq_len(length(waves) - 1), function(k) {
  t <- waves[k]; t1 <- waves[k + 1]
  bebes <- ind_all[[k + 1]] %>%
    filter(anais %in% c(t, t1), !(pid %in% ind_all[[k]]$pid)) %>%
    distinct(id) %>% mutate(naissance = 1L)
  inner_join(men_all[[k]], distinct(men_all[[k + 1]], id), by = "id") %>%
    left_join(bebes, by = "id") %>%
    mutate(naissance = coalesce(naissance, 0L), annee_t = t, annee_t1 = t1)
}) %>%
  mutate(cout_log_reel = deflater(cout_log, annee_t, IPC, base = 2025),
         revenu_reel   = deflater(revenu, annee_t - 1, IPC, base = 2025),
         vfepp_reel    = deflater(vfepp, annee_t, IPC, base = 2025),
         parite = cut(nenf, c(-Inf, 0, 1, 2, Inf), labels = c("0","1","2","3+")),
         surface_10 = surface / 10, cout_100 = cout_log_reel / 100,
         vfepp_100k = vfepp_reel / 100000, revenu_10k = revenu_reel / 10000,
         cout_m2 = cout_log_reel / if_else(surface >= 9 & surface <= 400,
                                           surface, NA_real_),
         couple_f = factor(couple == "1", labels = c("Non", "Oui")),
         csp = factor(csp), dipl_pr = factor(dipl_pr),
         annee_t_num = annee_t, annee_t = factor(annee_t),
         anciennete = annee_t_num - emmenag,
         age_c = (age_femme - 30) / 10) %>%
  relocate(id, annee_t, annee_t1, naissance)

# Les facteurs conservés en mémoire gardent leurs niveaux VIDES (zeat en a 11,
# dont plusieurs jamais observés), ce qui déplace la catégorie de référence des
# modèles par rapport à un panel relu depuis le CSV. On reproduit donc
# explicitement l'aller-retour : caractère puis facteur sur les seuls niveaux
# observés, dans l'ordre alphabétique.
panel <- panel %>% mutate(across(where(is.factor), ~ factor(as.character(.))))

cat("Transitions :", nrow(panel), "| naissances :", sum(panel$naissance),
    sprintf(" (%.2f %%)\n", 100 * mean(panel$naissance)))
write_csv(panel, "Output/panel_naissance_logement.csv")

# ── Modèle sur le panel (ex-07) ─────────────────────────────────────────────
d7 <- panel %>%
  filter(age_femme >= 15, age_femme <= 49, !is.na(surface), !is.na(cout_log),
         !is.na(statut3), !is.na(revenu), revenu > 0, !is.na(poids), poids > 0) %>%
  mutate(statut3 = factor(statut3, levels = c("Locataire", "Proprietaire_accedant",
                                              "Proprietaire_non_accedant")))
m7 <- svyglm(naissance ~ surface_10 + cout_100 + parite + age_c + I(age_c^2) +
               statut3 + couple_f + revenu_10k + dipl_pr + csp + nactocc + zeat + annee_t,
             design = svydesign(ids = ~id, weights = ~poids, data = d7),
             family = quasibinomial())
res7 <- tidy(m7, conf.int = TRUE, exponentiate = TRUE)
cat("\nModèle panel — variables d'intérêt (odds ratios) :\n")
print(res7 %>% filter(term %in% c("surface_10", "cout_100")) %>%
        select(term, odds_ratio = estimate, conf.low, conf.high, p.value))
write_csv(res7, "Output/modele_naissance_logement.csv")

# ── Test des non-déménageurs + effet minimum détectable (ex-15) ─────────────
#  L'ordre temporel ne supprime pas l'ANTICIPATION : les couples qui auront un
#  enfant ont deux fois plus souvent déménagé dans les deux ans. On ré-estime
#  donc sur les ménages au logement PRÉDÉTERMINÉ.
ANCIENNETE_MIN <- 3; Z_MDE <- qnorm(0.975) + qnorm(0.80)
d15 <- panel %>%
  filter(couple == 1, age_femme >= 15, age_femme <= 49, !is.na(surface),
         !is.na(cout_log), !is.na(statut3), !is.na(revenu), revenu > 0,
         !is.na(poids), poids > 0) %>%
  mutate(statut3 = factor(statut3, levels = c("Locataire", "Proprietaire_accedant",
                                              "Proprietaire_non_accedant")))
F15 <- naissance ~ surface_10 + cout_100 + parite + age_c + I(age_c^2) + statut3 +
  revenu_10k + dipl_pr + csp + nactocc + zeat + annee_t
VARS15 <- c(surface_10 = "Surface (+10 m2)", cout_100 = "Cout logement (+100 EUR/mois)")

estimer15 <- function(d, lib) {
  d <- droplevels(d)
  if (nrow(d) < 200 || sum(d$naissance) < 30) return(NULL)
  m <- svyglm(F15, design = svydesign(ids = ~id, weights = ~poids, data = d),
              family = quasibinomial())
  p <- fitted(m); fac <- mean(p * (1 - p))
  tidy(m) %>% filter(term %in% names(VARS15)) %>%
    transmute(echantillon = lib, n = nrow(d), naissances = sum(d$naissance),
              variable = unname(VARS15[term]),
              ame_pp = 100 * estimate * fac, se_pp = 100 * std.error * fac,
              p_value = p.value, mde_pp = Z_MDE * 100 * std.error * fac)
}
res15 <- bind_rows(
  estimer15(d15, "Tous les couples"),
  estimer15(filter(d15, anciennete >= ANCIENNETE_MIN),
            paste0("Non-demenageurs (>= ", ANCIENNETE_MIN, " ans)")),
  estimer15(filter(d15, anciennete <  ANCIENNETE_MIN),
            paste0("Demenageurs recents (< ", ANCIENNETE_MIN, " ans)"))) %>%
  mutate(mde_relatif_pct = 100 * mde_pp / (100 * mean(d15$naissance)))
# Sensibilité au seuil d'ancienneté : le résultat ne doit pas tenir au choix de
# 3 ans. Vérifié de 2 à 5 ans, comme dans R/15.
res15 <- bind_rows(res15, map_dfr(2:5, function(s)
  estimer15(filter(d15, anciennete >= s), paste0(">= ", s, " ans")) %>%
    filter(variable == VARS15[["surface_10"]])))

cat("\nTest des non-déménageurs — effets marginaux (points de %) :\n")
print(as.data.frame(res15 %>% mutate(across(where(is.numeric), ~ round(.x, 3)))),
      row.names = FALSE)
write_csv(res15, "Output/test_non_demenageurs.csv")

# ── Scénario : +10 m² sur la probabilité de naissance (ex-16) ───────────────
#  ⚠️ ASSOCIATION, PAS CAUSALITÉ : le test ci-dessus montre que l'effet de la
#  surface disparaît là où le logement est prédéterminé.
ANNEE_SCENARIO <- "2025"; AGE_FEMME <- 30
STATUTS_SC <- c("Locataire", "Proprietaire_accedant")
mode_de <- function(x, w) names(which.max(tapply(w, x, sum, na.rm = TRUE)))
pred <- function(m, d) as.numeric(predict(m, newdata = d, type = "response"))

res16 <- map_dfr(STATUTS_SC, function(st) {
  ref <- df_m2 %>% filter(statut3 == st, annee == ANNEE_SCENARIO)
  s_ref <- wquantile(ref$surface, ref$poids, 0.5)
  profil <- tibble(
    cout_m2 = wquantile(ref$cout_m2, ref$poids, 0.5), surface_10 = s_ref / 10,
    revenu_10k = wquantile(ref$revenu, ref$poids, 0.5) / 10000,
    statut3 = factor(st, levels = levels(df_m2$statut3)),
    taille_avant = round(wquantile(ref$taille_avant, ref$poids, 0.5)),
    nb_enf_avant = round(wquantile(ref$nb_enf_avant, ref$poids, 0.5)),
    demenage = factor("Non", levels = levels(df_m2$demenage)),
    age_c = (AGE_FEMME - 30) / 10,
    urbain = factor(mode_de(ref$urbain, ref$poids), levels = levels(df_m2$urbain)),
    zeat   = factor(mode_de(ref$zeat, ref$poids), levels = levels(df_m2$zeat)),
    annee  = ANNEE_SCENARIO)
  cout_total <- profil$cout_m2 * s_ref
  a <- pred(modele_m2, profil)
  b <- pred(modele_m2, profil %>% mutate(surface_10 = surface_10 + 1))
  c3 <- pred(modele_m2, profil %>% mutate(surface_10 = surface_10 + 1,
                                          cout_m2 = cout_total / (s_ref + 10)))
  ic <- comparisons(modele_m2, variables = list(surface_10 = 1),
                    newdata = profil, type = "response")
  tibble(statut = st,
         scenario = c("1. +10 m2 a prix au m2 constant", "2. +10 m2 a budget constant"),
         surface_ref_m2 = s_ref,
         p_depart_pct = 100 * a, p_arrivee_pct = 100 * c(b, c3),
         ecart_points = 100 * (c(b, c3) - a),
         ecart_relatif_pct = 100 * (c(b, c3) / a - 1),
         cout_mensuel_depart  = cout_total,
         cout_mensuel_arrivee = c(profil$cout_m2 * (s_ref + 10), cout_total),
         ic_bas_points  = c(100 * ic$conf.low,  NA_real_),
         ic_haut_points = c(100 * ic$conf.high, NA_real_),
         p_value        = c(ic$p.value, NA_real_))
})
cat("\nScénario +10 m² :\n")
print(as.data.frame(res16 %>% mutate(across(where(is.numeric), ~ round(.x, 2)))),
      row.names = FALSE)
write_csv(res16, "Output/scenario_surface.csv")

cat("\n\n=== A terminé. ⚠️ Association, pas causalité : cf. §3, test des",
    "non-déménageurs. ===\n")
