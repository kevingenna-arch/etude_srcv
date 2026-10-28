# ============================================================================
#  ERFS — 57. Politique familiale et naissances dans l'ERFS, 2005-2023
# ----------------------------------------------------------------------------
#  Reprend la démarche des scripts 38 et 50 (SRCV) sur l'ERFS (extraction :
#  script 56) : 16 000 naissances au lieu de 1 519 dans SRCV en 2014-2025, et
#  une série annuelle qui commence huit ans avant les réformes.
#
#  1. Gain d'un enfant supplémentaire simulé par OpenFisca (local, script
#     57_gain_enfant_erfs.py) : allocations familiales, complément familial,
#     allocation de base et prime de naissance de la PAJE, impôt sur le revenu
#     (quotient familial). Revenus imposables de chaque conjoint, statut
#     conjugal observé (inconnu en 2021-2023 pour 16 % des couples : déclaration
#     commune supposée), âges des enfants déjà nés. Gains calculés sous le
#     barème de l'année (`gain`), sous le barème FIXE de 2014 en euros
#     constants (`gain_ref`), et sous les barèmes 2013, 2016, 2017 et 2019.
#  2. Contrôle de la simulation : prestations familiales et impôt simulés
#     contre les montants que l'ERFS tient des sources fiscales et sociales.
#  3. Modèles de gain : naissance ~ gain + gain_ref + contrôles + effets fixes
#     d'année. À barème fixe, la dépendance du gain au revenu et à la famille
#     passe par `gain_ref` ; l'effet de `gain` n'est identifié que par les
#     changements de barème (instruments simulés, Currie et Gruber, 1996).
#     Modèle linéaire de probabilité pondéré, erreurs-types robustes ; probit
#     en contrôle.
#  4. Étude d'événement : exposition de chaque couple aux réformes 2013-2018
#     (abaissement du plafond du quotient familial, PAJE 2014, modulation des
#     allocations familiales en 2015, PAJE 2018) = gain sous le barème 2019
#     moins gain sous le barème 2013, à son revenu. Croisée avec l'année,
#     référence 2012 : les années 2005-2011 servent de placebo.
#  5. Part de la baisse de la fécondité 2014-2023 imputable au barème.
#  6. Classes moyennes : quartiles 2 et 3 de niveau de vie.
#
#  ⚠️ L'ERFS donne les revenus de l'année de l'enquête, celle de la naissance :
#     un congé parental les fait baisser. Cela touche `gain` et `gain_ref` de la
#     même façon ; l'écart entre les deux, qui identifie l'effet, ne tient
#     qu'au barème. Services (crèche), complément de mode de garde et
#     PreParE exclus, comme dans SRCV.
#  Sorties : Output/gain_enfant_erfs.csv (simulations, réutilisées d'un
#            passage à l'autre), Output/erfs_politique_familiale.csv
# ============================================================================

source("R/00_utils.R")
source("R/00_config.R")
suppressPackageStartupMessages(library(fixest))
IPC <- charger_ipc()

PYTHON          <- "C:/Users/kgenn/of/Scripts/python.exe"
PROCESSUS       <- 10
BAREME_REF      <- 2014
BAREMES_REFORME <- c(2013, 2016, 2017, 2019)
EUROS           <- 2023     # euros constants de 2023
pond <- function(x, w) weighted.mean(x, w, na.rm = TRUE)

# ── §0 Données ─────────────────────────────────────────────────────────────
df <- as_tibble(readRDS(file.path(DIR_ERFS, "_extrait", "couples_ERFS.rds"))) %>%
  mutate(cle      = paste(annee, ident, sep = "_"),
         conj_sim = if_else(conj == "inconnu", "marie", conj),
         rev1     = round(pmax(rev_femme, rev_homme)), rev2 = round(pmin(rev_femme, rev_homme)),
         nv_reel  = pmin(pmax(deflater(nv_hp, annee, IPC, base = EUROS), 0), 150000),
         nv_10k   = nv_reel / 1e4,
         rang3    = factor(case_when(nb_enf_avant == 0 ~ "1er enfant", nb_enf_avant == 1 ~ "2e enfant",
                                     TRUE ~ "3e enfant ou plus")),
         nb_enf   = factor(pmin(nb_enf_avant, 4)),
         benjamin_cl = factor(case_when(is.na(benjamin) ~ "Aucun", benjamin < 3 ~ "0-2 ans",
                                        benjamin < 6 ~ "3-5 ans", TRUE ~ "6 ans ou plus")),
         age_f    = factor(age_femme),
         age_h_cl = cut(coalesce(age_homme, age_femme), c(-Inf, 24, 29, 34, 39, 44, 49, 54, Inf)),
         across(c(conj, cs_femme, cs_homme), ~ factor(coalesce(.x, "NR"))),
         across(c(prest_pe, prest_autres, irpp), ~ deflater(.x, annee, IPC, base = EUROS), .names = "{.col}_reel")) %>%
  group_by(annee) %>%
  mutate(q_nv = cut(nv_reel, c(-Inf, wquantile(nv_reel, poids, c(.25, .5, .75)), Inf),
                    labels = c("Q1", "Q2", "Q3", "Q4"))) %>%
  ungroup()
cat("Champ :", nrow(df), "couples |", sum(df$naiss01), "naissances |", n_distinct(df$annee), "millésimes\n")

# ── §1 Simulation OpenFisca (seules les situations manquantes sont simulées)
#  Clés : `cle` = barème de l'année ; `cle_bAAAA` = barème AAAA, revenus
#  ramenés en euros de AAAA.
ligne <- function(d, b = NULL) d %>%
  transmute(cle = if (is.null(b)) cle else paste0(cle, "_b", b), legis = b %||% annee,
            rev1 = if (is.null(b)) rev1 else round(deflater(rev1, annee, IPC, base = b)),
            rev2 = if (is.null(b)) rev2 else round(deflater(rev2, annee, IPC, base = b)),
            conj = conj_sim, ages)
simuler <- function(entree, f_out) {
  deja <- if (file.exists(f_out)) read_csv(f_out, show_col_types = FALSE) else NULL
  manque <- if (is.null(deja)) entree else anti_join(entree, deja, by = c("cle", "legis"))
  if (nrow(manque)) {
    cat("Simulation OpenFisca :", nrow(manque), "situations\n")
    f_in <- "Output/gain_erfs_entree.csv"; f_tmp <- "Output/gain_erfs_lot.csv"
    write_csv(manque, f_in)
    code <- system2(PYTHON, c("-X", "utf8", "R/57_gain_enfant_erfs.py", f_in, f_tmp, PROCESSUS))
    # Les lots terminés sont gardés même en cas d'échec : le passage suivant reprend.
    if (file.exists(f_tmp)) {
      deja <- bind_rows(deja, read_csv(f_tmp, show_col_types = FALSE)) %>% distinct(cle, legis, .keep_all = TRUE)
      write_csv(deja, f_out); file.remove(f_tmp)
    }
    if (code != 0) stop("Simulation OpenFisca en échec (code ", code, ")")
    file.remove(f_in)
  }
  deja %>% semi_join(entree, by = c("cle", "legis"))
}
entree <- bind_rows(ligne(df), map_dfr(c(BAREME_REF, BAREMES_REFORME), ~ ligne(df, .x)))
COMPOSANTES <- c("af", "cf", "paje_base", "paje_naissance", "ir")
sim <- simuler(entree, "Output/gain_enfant_erfs.csv") %>%
  mutate(across(c(all_of(COMPOSANTES), paste0(COMPOSANTES, "_sans"), gain),
                ~ deflater(.x, legis, IPC, base = EUROS)),
         base = sub("_b[0-9]{4}$", "", cle),
         variante = if_else(grepl("_b[0-9]{4}$", cle), sub("^.*_(b[0-9]{4})$", "\\1", cle), "propre"))
propre <- sim %>% filter(variante == "propre") %>%
  select(cle = base, all_of(COMPOSANTES), ends_with("_sans"), gain)
baremes <- sim %>% filter(variante != "propre") %>% select(cle = base, variante, gain) %>%
  pivot_wider(names_from = variante, values_from = gain)
df <- df %>% left_join(propre, by = "cle") %>% left_join(baremes, by = "cle") %>%
  mutate(gain_1k     = gain / 1000,
         gain_ref_1k = .data[[paste0("b", BAREME_REF)]] / 1000,
         expo_1k     = (b2019 - b2013) / 1000,
         expo1415_1k = (b2016 - b2013) / 1000,
         expo18_1k   = (b2019 - b2017) / 1000)
stopifnot(!anyNA(df$gain_1k), !anyNA(df$gain_ref_1k), !anyNA(df$expo_1k))

# ── §2 La simulation face aux montants observés ────────────────────────────
#  Situation observée : avec le nouveau-né s'il y a eu naissance. Prestations
#  observées plus larges que la simulation : allocation de rentrée scolaire,
#  AEEH, ASF dans « autres » ; PreParE dans « petite enfance ». Impôt simulé
#  sur les seuls revenus d'activité et de remplacement.
controle <- df %>%
  mutate(af_cf_sim = af_sans + cf_sans + naiss01 * (af + cf),
         paje_sim  = paje_base_sans + paje_naissance_sans + naiss01 * (paje_base + paje_naissance),
         ir_sim    = -(ir_sans + naiss01 * ir)) %>%
  group_by(annee) %>%
  summarise(af_cf_simule = pond(af_cf_sim, poids), autres_pf_observe = pond(prest_autres_reel, poids),
            correlation_af_cf = cov.wt(cbind(af_cf_sim, prest_autres_reel), poids, cor = TRUE)$cor[1, 2],
            paje_simulee = pond(paje_sim, poids), petite_enfance_observe = pond(prest_pe_reel, poids),
            ir_simule = pond(ir_sim, poids), irpp_observe = pond(irpp_reel, poids),
            correlation_ir = cov.wt(cbind(ir_sim, irpp_reel), poids, cor = TRUE)$cor[1, 2], .groups = "drop")
cat("\nSimulation et montants observés (€ de 2023 par couple et par an) :\n")
print(as.data.frame(controle %>% mutate(across(where(is.double), ~ round(.x, 2)))), row.names = FALSE)

# ── §3 Le gain d'un enfant par année ───────────────────────────────────────
gains_annee <- df %>% group_by(annee) %>%
  summarise(couples = n(), naissances = sum(naiss01), taux_pct = 100 * pond(naiss01, poids),
            gain = pond(gain, poids), af = pond(af, poids), cf = pond(cf, poids),
            paje_base = pond(paje_base, poids), prime = pond(paje_naissance, poids),
            quotient_familial = pond(ir, poids), gain_bareme_2014 = 1000 * pond(gain_ref_1k, poids),
            exposition_reformes = 1000 * pond(expo_1k, poids),
            part_perdants_pct = 100 * pond(expo_1k < -0.1, poids), .groups = "drop")
gains_rang <- df %>% group_by(annee, rang3) %>%
  summarise(gain = pond(gain, poids), gain_bareme_2014 = 1000 * pond(gain_ref_1k, poids), .groups = "drop")
cat("\nGain d'un enfant supplémentaire (€ de 2023 par an) :\n")
print(as.data.frame(gains_annee %>% mutate(across(where(is.double), ~ round(.x, 1)))), row.names = FALSE)

# ── §4 Modèles de gain ─────────────────────────────────────────────────────
CTRL <- "nv_10k + I(nv_10k^2)"
FE   <- "annee + age_f + age_h_cl + nb_enf + benjamin_cl + dipl_femme + dipl_homme + act_homme + conj + statut + tuu + region"
PERIODES <- list("2005-2023" = 2005:2023, "2014-2023" = 2014:2023, "2005-2019" = 2005:2019)
ECHANTILLONS <- list("Ensemble" = function(d) d, "1er enfant" = function(d) filter(d, rang3 == "1er enfant"),
                     "2e enfant" = function(d) filter(d, rang3 == "2e enfant"),
                     "3e enfant ou plus" = function(d) filter(d, rang3 == "3e enfant ou plus"),
                     "Rang 2 et plus" = function(d) filter(d, rang3 != "1er enfant"))
MESURES <- list("Gain et gain a bareme fixe" = c("gain_1k", "gain_ref_1k"),
                "Gain seul (effet de structure)" = "gain_1k")
formule <- function(vars, fe = FE, extra = "")
  as.formula(paste("naiss01 ~", paste(vars, collapse = " + "), "+", CTRL, extra, "|", fe))
modele_gain <- function(d, vars) {
  m <- feols(formule(vars), data = d, weights = ~poids, vcov = "hetero")
  b <- unname(coef(m)[vars[1]]); s <- unname(se(m)[vars[1]])
  taux <- pond(d$naiss01, d$poids); g <- pond(d[[vars[1]]], d$poids)
  tibble(effet_pts = 100 * b, se = 100 * s, p = unname(pvalue(m)[vars[1]]), mde_pts = 2.8 * 100 * s,
         n = nobs(m), naissances = sum(d$naiss01), taux_pct = 100 * taux, gain_moyen = 1000 * g,
         elasticite = b / taux * g)
}
gains_modeles <- imap_dfr(PERIODES, function(ans, per) imap_dfr(MESURES, function(vars, mes)
  imap_dfr(ECHANTILLONS, function(fe, ech)
    modele_gain(fe(filter(df, annee %in% ans)), vars) %>%
      mutate(periode = per, mesure = mes, echantillon = ech, .before = 1))))
cat("\nEffet de 1 000 € de gain annuel sur la probabilité de naissance (points) :\n")
print(as.data.frame(gains_modeles %>% mutate(across(where(is.double), ~ round(.x, 3)))), row.names = FALSE)

# Probit en contrôle (ensemble) : effet marginal moyen = densité moyenne × coefficient
probit <- imap_dfr(PERIODES[1:2], function(ans, per) {
  d <- filter(df, annee %in% ans)
  m <- feglm(formule(c("gain_1k", "gain_ref_1k")), data = d, weights = ~poids, vcov = "hetero",
             family = binomial(link = "probit"))
  dens <- pond(dnorm(m$linear.predictors), d$poids[obs(m)])
  tibble(periode = per, ame_pts = 100 * dens * coef(m)[["gain_1k"]], se = 100 * dens * se(m)[["gain_1k"]],
         p = pvalue(m)[["gain_1k"]], n = nobs(m))
})
cat("\nProbit, effet marginal moyen (points pour 1 000 €) :\n"); print(as.data.frame(probit), row.names = FALSE)

# ── §5 Étude d'événement : exposition aux réformes 2013-2018 ───────────────
#  expo = gain sous le barème 2019 - gain sous le barème 2013 (k€ de 2023),
#  négative pour les couples qui perdent. Coefficient de expo année par
#  année, relativement à 2012 ; gain_ref en contrôle. Variante : effets fixes
#  rang × année et quartile de niveau de vie × année (tendances propres aux
#  familles nombreuses et aux revenus).
FE_ROBUSTE <- paste(FE, "+ annee^nb_enf + annee^q_nv")
evenement <- function(d, expo, fe = FE) {
  m <- feols(as.formula(paste0("naiss01 ~ i(annee, ", expo, ", ref = 2012) + ", expo, " + gain_ref_1k + ",
                               CTRL, " | ", fe)), data = d, weights = ~poids, vcov = "hetero")
  ct <- as.data.frame(coeftable(m))
  # Tests joints : années placebo (2005-2011) et années des réformes et après (2013-2023)
  joint <- function(ans) wald(m, keep = paste0("^annee::(", paste(ans, collapse = "|"), "):"), print = FALSE)$p
  ct %>% rownames_to_column("terme") %>% filter(grepl("^annee::", terme)) %>%
    transmute(annee = as.integer(sub("^annee::([0-9]{4}):.*$", "\\1", terme)),
              coef_pts = 100 * Estimate, se = 100 * `Std. Error`, p = `Pr(>|t|)`, n = nobs(m)) %>%
    bind_rows(tibble(annee = 2012L, coef_pts = 0, se = 0, p = NA_real_, n = nobs(m))) %>% arrange(annee) %>%
    mutate(p_joint_2005_2011 = joint(2005:2011), p_joint_2013_2023 = joint(2013:2023))
}
SPECS_EV <- list("Reference" = FE, "Rang x annee et revenu x annee" = FE_ROBUSTE)
EXPOS <- c("Reformes 2013-2018" = "expo_1k", "Reformes 2014-2015" = "expo1415_1k", "Reforme 2018" = "expo18_1k")
etude_evenement <- bind_rows(
  imap_dfr(SPECS_EV, function(fe, spec) imap_dfr(ECHANTILLONS[c("Ensemble", "1er enfant", "Rang 2 et plus")],
    ~ evenement(.x(df), "expo_1k", fe) %>% mutate(exposition = "Reformes 2013-2018", specification = spec,
                                                 echantillon = .y, .before = 1))),
  imap_dfr(EXPOS[-1], ~ evenement(df, .x) %>% mutate(exposition = .y, specification = "Reference",
                                                    echantillon = "Ensemble", .before = 1)))
cat("\nÉtude d'événement : effet de 1 000 € d'exposition, par année, relativement à 2012 (points) :\n")
print(as.data.frame(etude_evenement %>% filter(exposition == "Reformes 2013-2018") %>%
                      mutate(across(where(is.double), ~ round(.x, 3)))), row.names = FALSE)

# Différence de différences : avant (2005-2012) / après (2014-2017, 2018-2023)
did <- function(d, fe = FE) {
  d <- d %>% filter(annee != 2013) %>%
    mutate(apres_14_17 = as.integer(annee %in% 2014:2017), apres_18_23 = as.integer(annee >= 2018))
  m <- feols(as.formula(paste("naiss01 ~ expo_1k:apres_14_17 + expo_1k:apres_18_23 + expo_1k + gain_ref_1k +",
                              CTRL, "|", fe)), data = d, weights = ~poids, vcov = "hetero")
  ct <- as.data.frame(coeftable(m))
  ct %>% rownames_to_column("terme") %>% filter(grepl("apres", terme)) %>%
    transmute(terme, coef_pts = 100 * Estimate, se = 100 * `Std. Error`, p = `Pr(>|t|)`, n = nobs(m))
}
diff_diff <- imap_dfr(SPECS_EV, function(fe, spec) imap_dfr(ECHANTILLONS[c("Ensemble", "1er enfant", "Rang 2 et plus")],
  ~ did(.x(df), fe) %>% mutate(specification = spec, echantillon = .y, .before = 1)))
cat("\nDifférence de différences (exposition × période, référence 2005-2012, points pour 1 000 €) :\n")
print(as.data.frame(diff_diff %>% mutate(across(where(is.double), ~ round(.x, 3)))), row.names = FALSE)

# ── §6 Part de la baisse de la fécondité 2014-2023 ─────────────────────────
#  Les couples de 2023 reçoivent le gain du barème 2014 (gain_ref inchangé) :
#  hausse relative de leur natalité = effet × écart de gain moyen / taux.
#  Rapportée au recul de l'ICF (2,00 -> 1,68) et à celui du taux de naissance
#  des couples de l'ERFS. Effet estimé sur 2014-2023, commun ou par rang.
d23 <- filter(df, annee == 2023); d14 <- filter(df, annee == 2014)
baisse_icf <- ICF_2023 / ICF_2014 - 1
baisse_couples <- pond(d23$naiss01, d23$poids) / pond(d14$naiss01, d14$poids) - 1
part <- function(d_est, d_cf) {
  m <- feols(formule(c("gain_1k", "gain_ref_1k")), data = d_est, weights = ~poids, vcov = "hetero")
  b <- coef(m)[["gain_1k"]]; s <- se(m)[["gain_1k"]]
  dg <- pond(d_cf$gain_ref_1k - d_cf$gain_1k, d_cf$poids); p0 <- pond(d_cf$naiss01, d_cf$poids)
  tibble(ecart_gain_euros = 1000 * dg, effet_pts = 100 * b, hausse_relative_pct = 100 * b * dg / p0,
         se_rel = 100 * s * dg / p0, naissances_2023 = sum(d_cf$poids * d_cf$naiss01))
}
d1423 <- filter(df, annee %in% 2014:2023)
par_rang <- imap_dfr(ECHANTILLONS[c("1er enfant", "2e enfant", "3e enfant ou plus")],
                     ~ part(.x(d1423), .x(d23)) %>% mutate(rang = .y, .before = 1))
w <- par_rang$naissances_2023 / sum(par_rang$naissances_2023)
part_baisse <- bind_rows(
  part(d1423, d23) %>% mutate(rang = "Ensemble, effet commun", .before = 1),
  par_rang,
  tibble(rang = "Ensemble, effets par rang", ecart_gain_euros = sum(w * par_rang$ecart_gain_euros),
         effet_pts = NA_real_, hausse_relative_pct = sum(w * par_rang$hausse_relative_pct),
         se_rel = sqrt(sum((w * par_rang$se_rel)^2)), naissances_2023 = sum(par_rang$naissances_2023))) %>%
  mutate(part_baisse_icf_pct = hausse_relative_pct / -baisse_icf,
         ic_bas = (hausse_relative_pct - 1.96 * se_rel) / -baisse_icf,
         ic_haut = (hausse_relative_pct + 1.96 * se_rel) / -baisse_icf,
         part_baisse_couples_pct = hausse_relative_pct / -baisse_couples)
cat("\nPart de la baisse 2014-2023 imputable au barème (ICF", ICF_2014, "->", ICF_2023,
    "; taux des couples ERFS", round(100 * baisse_couples, 1), "%) :\n")
print(as.data.frame(part_baisse %>% mutate(across(where(is.double), ~ round(.x, 2)))), row.names = FALSE)

# ── §7 Classes moyennes : quartiles 2 et 3 ─────────────────────────────────
GROUPES_Q <- list("Q2 et Q3" = c("Q2", "Q3"), "Q1 et Q4" = c("Q1", "Q4"), "Q1" = "Q1", "Q2" = "Q2",
                  "Q3" = "Q3", "Q4" = "Q4")
quartiles_modeles <- imap_dfr(PERIODES[1:2], function(ans, per) imap_dfr(GROUPES_Q, function(qs, gq)
  imap_dfr(ECHANTILLONS[c("Ensemble", "1er enfant", "Rang 2 et plus")], function(fe, ech)
    modele_gain(fe(filter(df, annee %in% ans, q_nv %in% qs)), c("gain_1k", "gain_ref_1k")) %>%
      mutate(periode = per, quartiles = gq, echantillon = ech, .before = 1))))
ecart_q <- imap_dfr(PERIODES[1:2], function(ans, per) {
  d <- filter(df, annee %in% ans) %>% mutate(classes_moy = as.integer(q_nv %in% c("Q2", "Q3")))
  m <- feols(formule(c("gain_1k * classes_moy", "gain_ref_1k * classes_moy")), data = d,
             weights = ~poids, vcov = "hetero")
  tibble(periode = per, terme = "gain_1k:classes_moy", coef_pts = 100 * coef(m)[["gain_1k:classes_moy"]],
         se = 100 * se(m)[["gain_1k:classes_moy"]], p = pvalue(m)[["gain_1k:classes_moy"]])
})
recul_q <- d23 %>% group_by(q_nv) %>%
  summarise(recul_gain_euros = 1000 * pond(gain_ref_1k - gain_1k, poids), .groups = "drop")
cat("\nPar quartile de niveau de vie : effet de 1 000 € (points) :\n")
print(as.data.frame(quartiles_modeles %>% mutate(across(where(is.double), ~ round(.x, 3)))), row.names = FALSE)
cat("\nÉcart d'effet Q2-Q3 / Q1-Q4 :\n"); print(as.data.frame(ecart_q), row.names = FALSE)
cat("\nRecul du gain entre les barèmes 2014 et 2023, couples de 2023 (€ de 2023) :\n"); print(as.data.frame(recul_q))

ecrire(bind_rows(controle %>% mutate(bloc = "Simulation et montants observes", .before = 1),
                 gains_annee %>% mutate(bloc = "Gain par annee", .before = 1),
                 gains_rang %>% mutate(bloc = "Gain par annee et rang", .before = 1),
                 gains_modeles %>% mutate(bloc = "Modeles de gain", .before = 1),
                 probit %>% mutate(bloc = "Probit", .before = 1),
                 etude_evenement %>% mutate(bloc = "Etude d'evenement", .before = 1),
                 diff_diff %>% mutate(bloc = "Difference de differences", .before = 1),
                 part_baisse %>% mutate(bloc = "Part de la baisse 2014-2023", .before = 1),
                 quartiles_modeles %>% mutate(bloc = "Quartiles de niveau de vie", .before = 1),
                 ecart_q %>% mutate(bloc = "Ecart Q2-Q3 / Q1-Q4", .before = 1),
                 recul_q %>% mutate(bloc = "Recul du gain par quartile, 2023", .before = 1)),
       "Output/erfs_politique_familiale.csv")
