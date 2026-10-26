# ============================================================================
#  SRCV — 50. Politique familiale : affiner l'estimation de l'effet du barème
# ----------------------------------------------------------------------------
#  Le script 38 trouve +0,29 point de probabilité de naissance pour 1 000 € de
#  gain annuel d'un enfant supplémentaire (2014-2025, p = 0,26). Trois pistes
#  pour gagner en précision, sur les vagues dont on dispose :
#
#  1. Mieux mesurer l'incitation (simulation R/50_gain_enfant_openfisca.py) :
#     - revenu réparti entre les parents selon leurs revenus d'activité
#       observés (PY010N + PY050N), au lieu d'un partage forfaitaire ;
#     - statut conjugal réel : mariés ou pacsés, une déclaration commune ;
#       sinon deux foyers, enfants à la charge du parent qui gagne le plus ;
#     - gain moyen des trois premières années de l'enfant (même barème), en
#       plus du gain de la première année.
#  2. S'appuyer sur les réformes : exposition de chaque couple aux réformes de
#     2014-2015 (PAJE sous conditions de ressources, modulation des
#     allocations familiales, quotient familial) et de 2018 (PAJE alignée sur
#     le complément familial) = gain sous le barème 2019 moins gain sous le
#     barème 2013, en euros constants. Étude d'événement sur les 8 vagues :
#     2006 et 2010 servent de placebo, 2014 de référence, 2018 et 2022-2025
#     sont après les réformes. Modèle linéaire de probabilité (effets en
#     points, forme réduite).
#  3. Cibler les rangs : premier, deuxième, troisième enfant ou plus.
#
#  Reprend §0-§1 du script 38 (données, gains de référence). À lancer SANS
#  SRCV_DEBUT (8 vagues) ; les modèles de gain sont estimés sur 2014-2025
#  (période de l'article) et sur les 8 vagues.
#  Sorties : Output/gain_enfant_affine.csv (simulations, réutilisées),
#            Output/politique_familiale_precision.csv
# ============================================================================

lignes <- readLines("R/38_politique_familiale.R", encoding = "UTF-8")
eval(parse(text = lignes[1:(grep("# Gain moyen par vague", lignes)[1] - 1)], encoding = "UTF-8"))
PROCESSUS <- 10
BAREMES_REFORME <- c(2013, 2016, 2017, 2019)
des <- function(d) svydesign(ids = ~id_cluster, weights = ~poids, data = droplevels(d))
elaguer <- function(f, d) {
  vars <- setdiff(all.vars(f), all.vars(f[[2]]))
  mono <- vars[vapply(vars, function(v) v %in% names(d) &&
                        is.factor(d[[v]]) && nlevels(droplevels(d[[v]])) < 2, logical(1))]
  if (length(mono)) f <- update(f, paste(". ~ . -", paste(mono, collapse = " - ")))
  f
}
predire <- function(m, d) weighted.mean(as.numeric(predict(m, newdata = d, type = "response")), d$poids)

# ── §1 Statut conjugal et partage des revenus d'activité ───────────────────
conjugal <- imap_dfr(VAGUES[unique(df$annee)], function(cfg, an) {
  i <- lire_srcv(cfg$ind); names(i) <- toupper(names(i))
  code <- function(v) if (v %in% names(i)) trimws(as.character(i[[v]])) else rep(NA_character_, nrow(i))
  tibble(cle = paste(an, cle_menage_ind(i), sep = "_"), lien = code("LIENPREF"),
         marie = if ("SITUMATRI" %in% names(i)) code("SITUMATRI") == "1" else code("ETAMATRI") == "2",
         pacse = if ("SITUMATRI" %in% names(i)) code("SITUMATRI") == "2" else code("PACS") %in% "1",
         activite = coalesce(num(i$PY010N), 0) + coalesce(num(i$PY050N), 0)) %>%
    filter(lien %in% c("00", "0", "01", "1"))
}) %>%
  group_by(cle) %>%
  summarise(conj = case_when(any(marie, na.rm = TRUE) ~ "marie", any(pacse, na.rm = TRUE) ~ "pacse",
                             TRUE ~ "celibataire"),
            act_tot = sum(pmax(activite, 0)), act_max = max(pmax(activite, 0)), .groups = "drop") %>%
  mutate(part1_obs = if_else(act_tot > 0, act_max / act_tot, 1))
df <- df %>% left_join(conjugal, by = "cle") %>%
  mutate(conj = coalesce(conj, "marie"), part1_obs = coalesce(part1_obs, part1),
         rev1 = round(revenu_imp * part1_obs), rev2 = round(revenu_imp * (1 - part1_obs)))
cat("\nStatut conjugal des couples (%) :\n"); print(round(100 * prop.table(table(df$annee, df$conj), 1), 1))
cat("Part du premier apporteur : moyenne", round(mean(df$part1_obs), 3),
    "(forfait du script 38 :", round(mean(df$part1), 3), ")\n")

# ── §2 Simulations ─────────────────────────────────────────────────────────
simuler <- function(entree, f_out) {
  deja <- if (file.exists(f_out)) read_csv(f_out, show_col_types = FALSE) else NULL
  manque <- if (is.null(deja)) entree else anti_join(entree, deja, by = c("cle", "legis", "decalage"))
  if (nrow(manque)) {
    cat("Simulation OpenFisca :", nrow(manque), "situations\n")
    f_in <- "Output/gain_affine_entree.csv"; f_tmp <- "Output/gain_affine_lot.csv"
    write_csv(manque, f_in)
    code <- system2(PYTHON, c("-X", "utf8", "R/50_gain_enfant_openfisca.py", f_in, f_tmp, PROCESSUS))
    if (code != 0) stop("Simulation OpenFisca en échec (code ", code, ")")
    deja <- bind_rows(deja, read_csv(f_tmp, show_col_types = FALSE))
    write_csv(deja, f_out); file.remove(f_tmp, f_in)
  }
  deja %>% semi_join(entree, by = c("cle", "legis", "decalage"))
}

# Contrôle : avec les hypothèses du script 38 (mariés, partage forfaitaire),
# la nouvelle simulation doit redonner ses gains.
set.seed(2026)
test <- df %>% slice_sample(n = 300) %>%
  transmute(cle = paste0(cle, "_test38"), legis = annee_num, decalage = 0L,
            rev1 = round(revenu_imp) * part1, rev2 = round(revenu_imp) - rev1, conj = "marie", ages)
verif <- simuler(test, "Output/gain_enfant_affine_test.csv") %>%
  mutate(cle = sub("_test38$", "", cle)) %>%
  inner_join(deja %>% filter(!grepl("_bareme", cle)) %>% select(cle, legis, gain38 = gain),
             by = c("cle", "legis"))
cat("\nContrôle sur", nrow(verif), "couples : écart maximal avec le script 38 =",
    round(max(abs(verif$gain - verif$gain38)), 2), "€\n")
stopifnot(max(abs(verif$gain - verif$gain38)) < 1)

ligne <- function(d, suffixe, b, k) d %>%
  transmute(cle = paste0(cle, suffixe), legis = b %||% annee_num, decalage = as.integer(k),
            rev1 = if (is.null(b)) rev1 else round(deflater(rev1, annee_num, IPC, base = b)),
            rev2 = if (is.null(b)) rev2 else round(deflater(rev2, annee_num, IPC, base = b)),
            conj, ages)
entree <- bind_rows(
  map_dfr(0:2, ~ ligne(df, "_v1", NULL, .x)),
  map_dfr(0:2, ~ ligne(df, paste0("_v1_b", BAREME_REF), BAREME_REF, .x)),
  map_dfr(BAREMES_REFORME, ~ ligne(df, paste0("_v1_b", .x), .x, 0)))
sim <- simuler(entree, "Output/gain_enfant_affine.csv") %>%
  mutate(gain_2025 = deflater(gain, legis, IPC, base = 2025),
         base = sub("_v1.*$", "", cle), variante = sub("^.*_v1", "v1", cle))

large <- sim %>% select(base, variante, decalage, gain_2025) %>%
  pivot_wider(names_from = c(variante, decalage), values_from = gain_2025)
ref <- paste0("v1_b", BAREME_REF)
df <- df %>% left_join(large, by = c("cle" = "base")) %>%
  mutate(g1_1k = v1_0 / 1000, g1_ref_1k = .data[[paste0(ref, "_0")]] / 1000,
         g3_1k = (v1_0 + v1_1 + v1_2) / 3000,
         g3_ref_1k = (.data[[paste0(ref, "_0")]] + .data[[paste0(ref, "_1")]] + .data[[paste0(ref, "_2")]]) / 3000,
         expo_1k = (v1_b2019_0 - v1_b2013_0) / 1000,
         expo1415_1k = (v1_b2016_0 - v1_b2013_0) / 1000, expo18_1k = (v1_b2019_0 - v1_b2017_0) / 1000,
         rang3 = factor(case_when(nb_enf_avant == 0 ~ "1er enfant", nb_enf_avant == 1 ~ "2e enfant",
                                  TRUE ~ "3e enfant ou plus")))
stopifnot(!anyNA(df$g1_1k), !anyNA(df$g3_1k), !anyNA(df$expo_1k))

desc <- df %>% group_by(annee) %>%
  summarise(gain_38 = 1000 * weighted.mean(gain_1k, poids), gain_affine = 1000 * weighted.mean(g1_1k, poids),
            gain_3ans = 1000 * weighted.mean(g3_1k, poids),
            correlation_38_affine = cov.wt(cbind(gain_1k, g1_1k), poids, cor = TRUE)$cor[1, 2],
            exposition_2014_2018 = 1000 * weighted.mean(expo_1k, poids),
            part_perdants_pct = 100 * weighted.mean(expo_1k < -0.1, poids),
            part_non_maries_pct = 100 * weighted.mean(conj == "celibataire", poids), .groups = "drop")
cat("\nGains par vague (€ de 2025 par an) et exposition aux réformes :\n")
print(as.data.frame(desc %>% mutate(across(where(is.double), ~ round(.x, 2)))), row.names = FALSE)

# ── §3 Modèles de gain : mesure de référence, affinée, sur trois ans ; par rang
F_CTRL <- ~ surface_10 + cout_m2_reel + nv_10k + I(nv_10k^2) + statut3 + age_c + I(age_c^2) +
  demenage + csp + urbain + zeat + factor(annee)
F_PAR  <- ~ . + nb_enfants + taille_avant + benjamin_3
MESURES <- list("Reference (script 38)"            = c("gain_1k", "gain_ref_1k"),
                "Statut conjugal et partage reels" = c("g1_1k", "g1_ref_1k"),
                "Gain moyen des trois premieres annees" = c("g3_1k", "g3_ref_1k"))
ECHANTILLONS <- list("Ensemble" = function(d) d, "1er enfant" = function(d) filter(d, rang3 == "1er enfant"),
                     "2e enfant" = function(d) filter(d, rang3 == "2e enfant"),
                     "3e enfant ou plus" = function(d) filter(d, rang3 == "3e enfant ou plus"),
                     "Rang 2 et plus" = function(d) filter(d, rang3 != "1er enfant"))
PERIODES <- list("2014-2025" = c("2014", "2018", "2022", "2023", "2024", "2025"),
                 "2006-2025" = names(VAGUES))
modele_gain <- function(d, vars, ech) {
  f <- update(F_CTRL, as.formula(paste("naiss01 ~ . +", paste(vars, collapse = " + "))))
  if (ech != "1er enfant") f <- update(f, F_PAR)
  m <- svyglm(elaguer(f, droplevels(d)), design = des(d), family = quasibinomial(link = "probit"))
  a <- as_tibble(avg_slopes(m, variables = vars[1], wts = m$prior.weights))
  taux <- weighted.mean(d$naiss01, d$poids); g <- weighted.mean(d[[vars[1]]], d$poids)
  tibble(ame_pts = 100 * a$estimate, se = 100 * a$std.error, p = a$p.value,
         mde_pts = 2.8 * 100 * a$std.error, n = nrow(m$model), naissances = sum(m$y),
         taux_pct = 100 * taux, gain_moyen = 1000 * g,
         elasticite = a$estimate / taux * g)
}
gains_modeles <- imap_dfr(PERIODES, function(ans, per) imap_dfr(MESURES, function(vars, mes)
  imap_dfr(ECHANTILLONS, function(fe, ech)
    modele_gain(fe(filter(df, annee %in% ans)), vars, ech) %>%
      mutate(periode = per, mesure = mes, echantillon = ech, .before = 1))))
cat("\nEffet de 1 000 € de gain annuel sur la probabilité de naissance (points) :\n")
print(as.data.frame(gains_modeles %>% mutate(across(where(is.double), ~ round(.x, 3)))), row.names = FALSE)

# ── §4 Étude d'événement : exposition aux réformes 2014-2018 (8 vagues) ────
#  expo = gain sous le barème 2019 - gain sous le barème 2013 (k€ de 2025),
#  négatif pour les couples qui perdent. Coefficient de expo par période,
#  relativement à 2014 : 2006 et 2010 = placebo (aucune de ces réformes).
df <- df %>% mutate(periode = factor(case_when(annee %in% c("2022", "2023", "2024", "2025") ~ "2022-2025",
                                               TRUE ~ annee),
                                     levels = c("2014", "2006", "2010", "2018", "2022-2025")))
evenement <- function(d, ech, robuste = FALSE) {
  f <- update(F_CTRL, naiss01 ~ . + expo_1k * periode + g1_ref_1k)
  if (ech != "1er enfant") f <- update(f, F_PAR)
  if (robuste) f <- update(f, . ~ . + nv_10k:periode)
  m <- svyglm(elaguer(f, droplevels(d)), design = des(d), family = gaussian())
  co <- as.data.frame(summary(m)$coefficients)
  co %>% rownames_to_column("terme") %>% filter(grepl("^expo_1k", terme)) %>%
    transmute(terme = if_else(terme == "expo_1k", "expo_1k (niveau, 2014)", terme),
              coef_pts = 100 * Estimate, se = 100 * `Std. Error`, p = `Pr(>|t|)`,
              n = nrow(m$model), naissances = sum(m$y))
}
etude_evenement <- bind_rows(
  imap_dfr(ECHANTILLONS[c("Ensemble", "1er enfant", "Rang 2 et plus")],
           ~ evenement(.x(df), .y) %>% mutate(echantillon = .y, specification = "Reference", .before = 1)),
  imap_dfr(ECHANTILLONS[c("Ensemble", "Rang 2 et plus")],
           ~ evenement(.x(df), .y, robuste = TRUE) %>%
             mutate(echantillon = .y, specification = "Revenu x periode", .before = 1)))
cat("\nÉtude d'événement : effet de 1 000 € d'exposition, par période, relativement à 2014 (points) :\n")
print(as.data.frame(etude_evenement %>% mutate(across(where(is.double), ~ round(.x, 3)))), row.names = FALSE)

# ── §5 Part de la baisse de l'ICF 2014-2025, mesure affinée ────────────────
#  Comme au §3 du script 38 : les couples de 2025 reçoivent le gain du barème
#  2014 (gain de référence inchangé) ; intervalle par l'approximation linéaire.
d25 <- df %>% filter(annee == "2025")
part_baisse <- imap_dfr(MESURES, function(vars, mes) {
  d <- filter(df, annee %in% PERIODES[["2014-2025"]])
  f <- update(update(F_CTRL, as.formula(paste("naiss01 ~ . +", paste(vars, collapse = " + ")))), F_PAR)
  m <- svyglm(elaguer(f, droplevels(d)), design = des(d), family = quasibinomial(link = "probit"))
  a <- as_tibble(avg_slopes(m, variables = vars[1], wts = m$prior.weights))
  o <- d25 %>% filter(if_all(all_of(setdiff(all.vars(f), c("naiss01", "annee"))), ~ !is.na(.x)))
  c1 <- o %>% mutate(!!vars[1] := .data[[vars[2]]])
  p0 <- predire(m, o); rel <- predire(m, c1) / p0 - 1
  baisse <- (ICF_2025 - ICF_2014) / ICF_2014
  dg <- weighted.mean(c1[[vars[1]]] - o[[vars[1]]], o$poids)
  tibble(mesure = mes, ecart_gain_euros = 1000 * dg, ecart_relatif_pct = 100 * rel,
         part_de_la_baisse_pct = 100 * rel / -baisse,
         part_ic_bas = a$conf.low * dg / p0 / -baisse * 100, part_ic_haut = a$conf.high * dg / p0 / -baisse * 100)
})
cat("\nPart de la baisse de l'ICF 2014-2025 imputable au barème :\n")
print(as.data.frame(part_baisse %>% mutate(across(where(is.double), ~ round(.x, 2)))), row.names = FALSE)

# ── §6 Effets par rang : test d'égalité et part de la baisse ───────────────
#  Le recul du barème a surtout porté sur le premier enfant (PAJE), rang où
#  l'effet estimé est nul : on refait le contrefactuel rang par rang (1er,
#  2e, 3e ou plus) et l'on agrège en pondérant par les naissances prédites
#  en 2025. Intervalle : approximation linéaire, rangs supposés indépendants.
d1425 <- filter(df, annee %in% PERIODES[["2014-2025"]]) %>% mutate(premier = as.integer(rang3 == "1er enfant"))
egalite <- imap_dfr(MESURES[1:2], function(vars, mes) {
  f <- update(update(F_CTRL, as.formula(paste("naiss01 ~ . +", vars[1], "* premier +", vars[2], "* premier"))), F_PAR)
  m <- svyglm(elaguer(f, droplevels(d1425)), design = des(d1425), family = quasibinomial(link = "probit"))
  co <- summary(m)$coefficients
  tibble(mesure = mes, terme = paste0(vars[1], ":premier"),
         coef_indice = co[paste0(vars[1], ":premier"), 1], p = co[paste0(vars[1], ":premier"), 4])
})
cat("\nÉgalité de l'effet entre premier enfant et rangs suivants (interaction, indice probit) :\n")
print(as.data.frame(egalite %>% mutate(across(where(is.double), ~ round(.x, 4)))), row.names = FALSE)

par_rang <- imap_dfr(MESURES[1:2], function(vars, mes) {
  r <- imap_dfr(ECHANTILLONS[c("1er enfant", "2e enfant", "3e enfant ou plus")], function(fe, ech) {
    d <- fe(filter(df, annee %in% PERIODES[["2014-2025"]]))
    f <- update(F_CTRL, as.formula(paste("naiss01 ~ . +", paste(vars, collapse = " + "))))
    if (ech != "1er enfant") f <- update(f, F_PAR)
    m <- svyglm(elaguer(f, droplevels(d)), design = des(d), family = quasibinomial(link = "probit"))
    a <- as_tibble(avg_slopes(m, variables = vars[1], wts = m$prior.weights))
    o <- fe(d25) %>% filter(if_all(all_of(setdiff(all.vars(f), c("naiss01", "annee"))), ~ !is.na(.x)))
    c1 <- o %>% mutate(!!vars[1] := .data[[vars[2]]])
    p0 <- predire(m, o); dg <- weighted.mean(c1[[vars[1]]] - o[[vars[1]]], o$poids)
    tibble(rang = ech, naissances_predites = sum(o$poids * as.numeric(predict(m, newdata = o, type = "response"))),
           ecart_gain_euros = 1000 * dg, ame_pts = 100 * a$estimate,
           rel = predire(m, c1) / p0 - 1, se_rel = a$std.error * dg / p0)
  })
  baisse <- (ICF_2025 - ICF_2014) / ICF_2014
  w <- r$naissances_predites / sum(r$naissances_predites)
  tot <- tibble(rang = "Ensemble (somme pondérée)", naissances_predites = sum(r$naissances_predites),
                ecart_gain_euros = sum(w * r$ecart_gain_euros), ame_pts = NA_real_,
                rel = sum(w * r$rel), se_rel = sqrt(sum((w * r$se_rel)^2)))
  bind_rows(r %>% mutate(poids_naissances = w), tot %>% mutate(poids_naissances = 1)) %>%
    mutate(mesure = mes, .before = 1,
           part_de_la_baisse_pct = 100 * rel / -baisse,
           part_ic_bas = 100 * (rel - 1.96 * se_rel) / -baisse, part_ic_haut = 100 * (rel + 1.96 * se_rel) / -baisse)
})
cat("\nPart de la baisse de l'ICF 2014-2025, effets par rang :\n")
print(as.data.frame(par_rang %>% mutate(across(where(is.double), ~ round(.x, 3)))), row.names = FALSE)

ecrire(bind_rows(desc %>% mutate(bloc = "Gains par vague", .before = 1),
                 egalite %>% mutate(bloc = "Egalite premier enfant / rangs suivants", .before = 1),
                 par_rang %>% mutate(bloc = "Part de la baisse par rang", .before = 1),
                 gains_modeles %>% mutate(bloc = "Modeles de gain", .before = 1),
                 etude_evenement %>% mutate(bloc = "Etude d'evenement", .before = 1),
                 part_baisse %>% mutate(bloc = "Part de la baisse", .before = 1)),
       "Output/politique_familiale_precision.csv")
