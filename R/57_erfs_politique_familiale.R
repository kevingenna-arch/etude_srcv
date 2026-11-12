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
#     (FE, R/57_erfs_preparation.R), dont année x décile de niveau de vie x
#     nombre d'enfants x âge de la femme. À barème fixe, la
#     dépendance du gain au revenu et à la famille passe par `gain_ref` ;
#     l'effet de `gain` n'est identifié que par les changements de barème
#     (instruments simulés, Currie et Gruber, 1996). Gain en euros, ou en % du
#     revenu du ménage hors prestations (gain relatif). Modèle linéaire de
#     probabilité pondéré ; erreur-type : la plus grande de la robuste et de
#     la groupée par décile x nombre d'enfants ; probit en contrôle.
#     Comparaison avec les effets d'année communs (ancienne référence) et les
#     spécifications intermédiaires : bloc « Effets temporels ».
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

source("R/57_erfs_preparation.R")   # §0 Données et §1 simulation OpenFisca

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
CTRL <- "nv_10k + I(nv_10k^2)"   # FE : R/57_erfs_preparation.R
PERIODES <- list("2005-2023" = 2005:2023, "2014-2023" = 2014:2023, "2005-2019" = 2005:2019)
ECHANTILLONS <- list("Ensemble" = function(d) d, "1er enfant" = function(d) filter(d, rang3 == "1er enfant"),
                     "2e enfant" = function(d) filter(d, rang3 == "2e enfant"),
                     "3e enfant ou plus" = function(d) filter(d, rang3 == "3e enfant ou plus"),
                     "Rang 2 et plus" = function(d) filter(d, rang3 != "1er enfant"))
#  Gain relatif : effet d'un gain égal à 1 % du revenu (gain_moyen en % du revenu)
MESURES <- list("Gain et gain a bareme fixe" = c("gain_1k", "gain_ref_1k"),
                "Gain relatif (% du revenu)" = c("gain_rel", "gain_rel_ref"),
                "Gain seul (effet de structure)" = "gain_1k")
formule <- function(vars, fe = FE, extra = "")
  as.formula(paste("naiss01 ~", paste(vars, collapse = " + "), "+", CTRL, extra, "|", fe))
#  Erreur-type retenue : la plus grande de la robuste et de la groupée (décile x enfants)
se_prudente <- function(m, v) max(se(m)[[v]], se(m, vcov = ~groupe)[[v]])
modele_gain <- function(d, vars, fe = FE) {
  m <- feols(formule(vars, fe), data = d, weights = ~poids, vcov = "hetero")
  b <- unname(coef(m)[vars[1]]); s <- se_prudente(m, vars[1])
  taux <- pond(d$naiss01, d$poids)
  g <- pond(d[[vars[1]]], d$poids) * if (grepl("_1k$", vars[1])) 1000 else 1
  tibble(effet_pts = 100 * b, se = 100 * s, se_robuste = 100 * se(m)[[vars[1]]], p = 2 * pnorm(-abs(b / s)),
         mde_pts = 2.8 * 100 * s, n = nobs(m), naissances = sum(d$naiss01), taux_pct = 100 * taux,
         gain_moyen = g, elasticite = b / taux * g / if (grepl("_1k$", vars[1])) 1000 else 1)
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
#  année, relativement à 2012 ; gain_ref en contrôle. Variante : effets
#  d'année communs (ancienne référence). Erreurs-types robustes : avec 50
#  grappes, la matrice groupée n'est pas définie positive et ses tests joints
#  (7 à 11 coefficients) rejettent à tort, y compris après les réformes.
evenement <- function(d, expo, fe = FE, ref = "gain_ref_1k") {
  m <- feols(as.formula(paste0("naiss01 ~ i(annee, ", expo, ", ref = 2012) + ", expo, " + ", ref, " + ",
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
SPECS_EV <- list("Reference" = FE, "Effets d'annee communs" = FE_COMMUNS)
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
  map_dfr(c("expo_1k:apres_14_17", "expo_1k:apres_18_23"), function(t) {
    b <- coef(m)[[t]]; s <- se_prudente(m, t)
    tibble(terme = t, coef_pts = 100 * b, se = 100 * s, p = 2 * pnorm(-abs(b / s)), n = nobs(m))
  })
}
#  Placebo à un coefficient (fiable avec 50 grappes) : sur 2005-2012 seulement,
#  exposition x 2005-2008, référence 2009-2012 ; erreur-type prudente.
placebo <- function(d, expo, fe = FE, ref = "gain_ref_1k") {
  x <- filter(d, annee <= 2012) %>% mutate(p0508 = as.integer(annee <= 2008))
  m <- feols(as.formula(paste0("naiss01 ~ ", expo, ":p0508 + ", expo, " + ", ref, " + ", CTRL, " | ", fe)),
             data = x, weights = ~poids, vcov = "hetero")
  t <- paste0(expo, ":p0508"); b <- coef(m)[[t]]; s <- se_prudente(m, t)
  tibble(placebo_pts = 100 * b, placebo_se = 100 * s, placebo_p = 2 * pnorm(-abs(b / s)))
}
diff_diff <- imap_dfr(SPECS_EV, function(fe, spec) imap_dfr(ECHANTILLONS[c("Ensemble", "1er enfant", "Rang 2 et plus")],
  ~ did(.x(df), fe) %>% mutate(specification = spec, echantillon = .y, .before = 1)))
cat("\nDifférence de différences (exposition × période, référence 2005-2012, points pour 1 000 €) :\n")
print(as.data.frame(diff_diff %>% mutate(across(where(is.double), ~ round(.x, 3)))), row.names = FALSE)

# ── §6 Part de la baisse de la fécondité 2014-2023 ─────────────────────────
#  Les couples de 2023 reçoivent le gain du barème 2014 (gain_ref inchangé) :
#  hausse relative de leur natalité = effet × écart de gain moyen / taux.
#  Rapportée au recul de l'ICF (2,00 -> 1,68) et à celui du taux de naissance
#  des couples de l'ERFS. Effet commun ou par rang, estimé sur 2014-2023 (la
#  période des réformes et de la baisse) ou sur 2005-2023 (plus précis) : la
#  part en dépend nettement, d'où les deux. Gain en euros et gain relatif (le
#  recul est alors mesuré en % du revenu : `ecart_mesure`).
d23 <- filter(df, annee == 2023); d14 <- filter(df, annee == 2014)
baisse_icf <- ICF_2023 / ICF_2014 - 1
baisse_couples <- pond(d23$naiss01, d23$poids) / pond(d14$naiss01, d14$poids) - 1
part <- function(d_est, d_cf, vars = MESURES[[1]], fe = FE) {
  m <- feols(formule(vars, fe), data = d_est, weights = ~poids, vcov = "hetero")
  b <- coef(m)[[vars[1]]]; s <- se_prudente(m, vars[1])
  dg <- pond(d_cf[[vars[2]]] - d_cf[[vars[1]]], d_cf$poids); p0 <- pond(d_cf$naiss01, d_cf$poids)
  tibble(ecart_gain_euros = 1000 * pond(d_cf$gain_ref_1k - d_cf$gain_1k, d_cf$poids), ecart_mesure = dg,
         effet_pts = 100 * b, se_pts = 100 * s, hausse_relative_pct = 100 * b * dg / p0,
         se_rel = 100 * s * dg / p0, naissances_2023 = sum(d_cf$poids * d_cf$naiss01))
}
part_periode <- function(ans, vars) {
  d_est <- filter(df, annee %in% ans)
  par_rang <- imap_dfr(ECHANTILLONS[c("1er enfant", "2e enfant", "3e enfant ou plus")],
                       ~ part(.x(d_est), .x(d23), vars) %>% mutate(rang = .y, .before = 1))
  w <- par_rang$naissances_2023 / sum(par_rang$naissances_2023)
  bind_rows(
    part(d_est, d23, vars) %>% mutate(rang = "Ensemble, effet commun", .before = 1),
    par_rang,
    tibble(rang = "Ensemble, effets par rang", ecart_gain_euros = sum(w * par_rang$ecart_gain_euros),
           effet_pts = NA_real_, hausse_relative_pct = sum(w * par_rang$hausse_relative_pct),
           se_rel = sqrt(sum((w * par_rang$se_rel)^2)), naissances_2023 = sum(par_rang$naissances_2023)))
}
part_baisse <- imap_dfr(MESURES[1:2], function(vars, mes) imap_dfr(PERIODES[c("2014-2023", "2005-2023")],
                        ~ part_periode(.x, vars) %>% mutate(mesure = mes, estimation = .y, .before = 1))) %>%
  mutate(part_baisse_icf_pct = hausse_relative_pct / -baisse_icf,
         ic_bas = (hausse_relative_pct - 1.96 * se_rel) / -baisse_icf,
         ic_haut = (hausse_relative_pct + 1.96 * se_rel) / -baisse_icf,
         part_baisse_couples_pct = hausse_relative_pct / -baisse_couples)
cat("\nPart de la baisse 2014-2023 imputable au barème (ICF", ICF_2014, "->", ICF_2023,
    "; taux des couples ERFS", round(100 * baisse_couples, 1), "%) :\n")
print(as.data.frame(part_baisse %>% mutate(across(where(is.double), ~ round(.x, 2)))), row.names = FALSE)

# ── §6 bis Effets temporels : pourquoi la référence a changé ───────────────
#  Des effets d'année communs aux effets d'année par groupe : effet du gain,
#  part de la baisse (effet commun, ensemble) et deux placebos, en euros et
#  en relatif : années 2005-2011 conjointement nulles dans l'étude
#  d'événement (erreurs robustes), et exposition x 2005-2008 sur 2005-2012.
SPECS_T <- c("Effets d'annee communs" = FE_COMMUNS,
             "+ annee x decile x nombre d'enfants" = paste(FE_COMMUNS, "+ annee^d_nv^nb_enf"),
             "+ annee x age de la femme" = paste(FE_COMMUNS, "+ annee^d_nv^nb_enf + annee^age_cl"),
             "+ annee x age x nombre d'enfants" = paste(FE_COMMUNS, "+ annee^d_nv^nb_enf + annee^age_cl^nb_enf"),
             "Annee x decile x nombre d'enfants x age (reference)" = FE,
             "+ annee x chacun des controles" = paste(FE, "+ annee^dipl_femme + annee^dipl_homme + annee^act_homme",
                                                      "+ annee^conj + annee^statut + annee^tuu + annee^region",
                                                      "+ annee^benjamin_cl + annee^age_h_cl"))
EXPO_MESURE <- c("Gain et gain a bareme fixe" = "expo_1k", "Gain relatif (% du revenu)" = "expo_rel")
effets_temporels <- imap_dfr(SPECS_T, function(fe, spec) imap_dfr(MESURES[1:2], function(vars, mes) {
  p_joint <- evenement(df, EXPO_MESURE[[mes]], fe, ref = vars[2])$p_joint_2005_2011[1]
  pl <- placebo(df, EXPO_MESURE[[mes]], fe, ref = vars[2])
  imap_dfr(PERIODES[1:2], function(ans, per)
    part(filter(df, annee %in% ans), d23, vars, fe) %>%
      transmute(periode = per, effet_pts, se = se_pts, p = 2 * pnorm(-abs(effet_pts / se_pts)),
                part_baisse_icf_pct = hausse_relative_pct / -baisse_icf,
                ic_bas = (hausse_relative_pct - 1.96 * se_rel) / -baisse_icf,
                ic_haut = (hausse_relative_pct + 1.96 * se_rel) / -baisse_icf)) %>%
    mutate(specification = spec, mesure = mes, p_placebo_2005_2011 = p_joint, pl, .before = 1)
}))
cat("\nEffets temporels : effet du gain, part de la baisse et placebo :\n")
print(as.data.frame(effets_temporels %>% mutate(across(where(is.double), ~ round(.x, 3)))), row.names = FALSE)

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
  b <- coef(m)[["gain_1k:classes_moy"]]; s <- se_prudente(m, "gain_1k:classes_moy")
  tibble(periode = per, terme = "gain_1k:classes_moy", coef_pts = 100 * b, se = 100 * s, p = 2 * pnorm(-abs(b / s)))
})
recul_q <- d23 %>% group_by(q_nv) %>%
  summarise(recul_gain_euros = 1000 * pond(gain_ref_1k - gain_1k, poids),
            recul_gain_pct_revenu = pond(gain_rel_ref - gain_rel, poids), .groups = "drop")
#  Report des naissances : recul du taux de naissance (2012-2014 -> 2021-2023) et
#  recul du gain dû au barème (couples de 2023), par âge de la femme
recul_age <- df %>% filter(annee %in% c(2012:2014, 2021:2023)) %>%
  mutate(per = if_else(annee <= 2014, "taux_2012_2014", "taux_2021_2023")) %>%
  group_by(age_cl, per) %>% summarise(taux = 100 * pond(naiss01, poids), .groups = "drop") %>%
  pivot_wider(names_from = per, values_from = taux) %>%
  mutate(evolution_taux_pct = 100 * (taux_2021_2023 / taux_2012_2014 - 1)) %>%
  left_join(d23 %>% group_by(age_cl) %>%
              summarise(recul_gain_euros = 1000 * pond(gain_ref_1k - gain_1k, poids),
                        revenu_menage_keur = pond(rev_men, poids) / 1000,
                        recul_gain_pct_revenu = pond(gain_rel_ref - gain_rel, poids), .groups = "drop"),
            by = "age_cl") %>%
  mutate(age_cl = as.character(age_cl))
cat("\nPar âge de la femme : évolution du taux de naissance et recul du gain dû au barème :\n")
print(as.data.frame(recul_age %>% mutate(across(where(is.double), ~ round(.x, 2)))), row.names = FALSE)
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
                 effets_temporels %>% mutate(bloc = "Effets temporels", .before = 1),
                 recul_age %>% mutate(bloc = "Recul par age de la femme", .before = 1),
                 quartiles_modeles %>% mutate(bloc = "Quartiles de niveau de vie", .before = 1),
                 ecart_q %>% mutate(bloc = "Ecart Q2-Q3 / Q1-Q4", .before = 1),
                 recul_q %>% mutate(bloc = "Recul du gain par quartile, 2023", .before = 1)),
       "Output/erfs_politique_familiale.csv")
