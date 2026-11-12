# ============================================================================
#  ERFS — 60. Prestations familiales ou quotient familial : lequel compte ?
# ----------------------------------------------------------------------------
#  Question : reverser l'avantage fiscal du quotient familial (QF) en
#  prestations familiales augmenterait-il la natalité ? Trois étapes.
#  1. Valeur d'un enfant sur toute son enfance (0 à 19 ans), en deux parts :
#     prestations (allocations familiales, complément familial, allocation de
#     base et prime de la PAJE) et QF. Simulée par OpenFisca, revenus réels et
#     législation de l'année figés (et barème fixe de 2014), actualisée à 2 % :
#     âge 0 (script 57), âges 1-2 (âge 0 sans la prime), âges 3-19 (grille de
#     quatre âges). Modèle du script 57 avec deux termes : naissance ~
#     prestations + QF + leurs valeurs au barème de 2014 + contrôles | effets
#     fixes de la référence (année x décile x enfants x âge) ; variantes avec
#     effets d'année communs, année x décile, année x décile x enfants.
#     Erreurs-types : la plus grande de la robuste et de la groupée.
#  2. Plafonnement du QF de 2012-2013 (avantage par demi-part de 2 336 € sur
#     les revenus de 2011, 2 000 € en 2012, 1 500 € en 2013) : exposition =
#     QF de l'enfance au barème de 2014 moins au même barème avec le plafond
#     de 2011 (en euros de 2014), pour ne garder que le plafonnement (et pas
#     le gel du barème ni la décote) ; étude d'événement (référence 2011) et
#     double différence.
#  3. Réforme simulée à budget constant, couples de 2023 : avantage du QF par
#     demi-part abaissé à 1 000 € ou annulé, recette reversée en allocation
#     forfaitaire par enfant de moins de 20 ans ; naissances prédites avec les
#     effets estimés à l'étape 1.
#  ⚠️ Revenus et législation figés sur 20 ans ; allocation de rentrée scolaire
#     et modes de garde exclus ; budget équilibré sur les seuls couples du
#     champ (femme de 15 à 49 ans) ; effet revenu des transferts sur les
#     enfants déjà nés non compté. Plafond nul : l'enfant ne réduit plus
#     l'impôt par le quotient, mais les seuils de décote et de réduction
#     d'impôt qui dépendent du nombre de parts restent.
#  Entrées : R/57_erfs_preparation.R (couples, cache du script 57),
#            R/60_gain_vie_enfant.py (OpenFisca, local)
#  Sorties : Output/gain_vie_enfant_erfs.csv (simulations, cache),
#            Output/erfs_qf_prestations.csv
# ============================================================================

source("R/57_erfs_preparation.R")   # df, sim, ligne(), simuler()

F_VIE    <- "Output/gain_vie_enfant_erfs.csv"
GRILLE   <- tibble(k = c(4, 9, 14, 18), debut = c(3, 7, 12, 17), fin = c(6, 11, 16, 19))
TAUX     <- 0.02
PLAFONDS <- c("Avantage du QF plafonne a 1 000 EUR" = "1000", "Avantage du QF des enfants supprime" = "0")
CTRL     <- "nv_10k + I(nv_10k^2)"   # comme au script 57 ; FE et FE_COMMUNS : R/57_erfs_preparation.R
SPECS    <- c("Reference" = FE, "Effets d'annee communs" = FE_COMMUNS,
              "Annee x decile" = paste(FE_COMMUNS, "+ annee^d_nv"),
              "Annee x decile x nombre d'enfants" = paste(FE_COMMUNS, "+ annee^d_nv^nb_enf"))
variante_de <- function(cle) if_else(grepl("_b[0-9]{4}$", cle), sub("^.*_(b[0-9]{4})$", "\\1", cle), "propre")
#  Erreur-type d'une combinaison a'b : la plus grande de la robuste et de la groupée (décile x enfants)
se_comb <- function(m, a, termes) {
  a <- matrix(a, ncol = 1)
  max(sqrt(drop(t(a) %*% vcov(m)[termes, termes] %*% a)),
      sqrt(drop(t(a) %*% vcov(m, vcov = ~groupe)[termes, termes] %*% a)))
}

# ── §0 Simulations ─────────────────────────────────────────────────────────
#  60_gain_vie_enfant.py : âges de la grille aux barèmes de l'année et de
#  2014 ; barème de 2014 avec le plafond du QF de 2011, âge 0 compris ;
#  couples de 2023 sous les deux réformes, âge 0 compris.
PLAFOND_2011 <- as.character(round(deflater(2336, 2011, IPC, base = 2014)))
lire_vie <- function(f) read_csv(f, show_col_types = FALSE,
                                 col_types = cols(cle = "c", plafond = "c", .default = "d"))
simuler_vie <- function(entree) {
  cles <- c("cle", "legis", "k", "plafond")
  deja <- if (file.exists(F_VIE)) lire_vie(F_VIE) else NULL
  manque <- if (is.null(deja)) entree else anti_join(entree, deja, by = cles)
  if (nrow(manque)) {
    cat("Simulation OpenFisca sur l'enfance :", nrow(manque), "situations\n")
    f_in <- "Output/gain_vie_entree.csv"; f_tmp <- "Output/gain_vie_lot.csv"
    write_csv(manque, f_in, na = "")
    code <- system2(PYTHON, c("-X", "utf8", "R/60_gain_vie_enfant.py", f_in, f_tmp, PROCESSUS))
    # Les lots terminés sont gardés même en cas d'échec : le passage suivant reprend.
    if (file.exists(f_tmp)) {
      deja <- bind_rows(deja, lire_vie(f_tmp)) %>% distinct(across(all_of(cles)), .keep_all = TRUE)
      write_csv(deja, F_VIE, na = ""); file.remove(f_tmp)
    }
    if (code != 0) stop("Simulation OpenFisca en échec (code ", code, ")")
    file.remove(f_in)
  }
  deja %>% semi_join(entree, by = cles)
}
grille <- function(lignes, ks, plafond = NA_character_)
  tidyr::expand_grid(lignes, k = ks) %>% mutate(plafond = plafond)
d23 <- filter(df, annee == 2023)
entree <- bind_rows(
  grille(bind_rows(ligne(df), ligne(df, 2014)), GRILLE$k),
  grille(ligne(df, 2014), c(0, GRILLE$k), PLAFOND_2011),
  map_dfr(PLAFONDS, ~ grille(ligne(d23), c(0, GRILLE$k), .x)))
vie <- simuler_vie(entree) %>%
  mutate(across(c(af, cf, paje_base, paje_naissance, ir, ir_sans), ~ deflater(.x, legis, IPC, base = EUROS)),
         base = sub("_b[0-9]{4}$", "", cle),
         variante = if_else(coalesce(plafond, "") == PLAFOND_2011, "plafond2011", variante_de(cle)))

# ── §1 Valeur d'un enfant sur l'enfance ────────────────────────────────────
#  Âge 0 complet ; âges 1-2 = âge 0 sans la prime ; âges 3-19 = valeur de la
#  grille sur sa tranche. k€ de 2023, actualisés au taux r.
valeur_vie <- function(age0, ages, r = TAUX) {
  act <- function(j) (1 + r)^(-j)
  poids_g <- GRILLE %>% mutate(D = map2_dbl(debut, fin, ~ sum(act(.x:.y)))) %>% select(k, D)
  g <- ages %>% inner_join(poids_g, by = "k") %>% group_by(base, variante) %>%
    summarise(pf_g = sum((af + cf) * D), qf_g = sum(ir * D), n_g = n(), .groups = "drop")
  v <- age0 %>% inner_join(g, by = c("base", "variante")) %>%
    transmute(base, variante,
              pf = (af + cf + paje_base + paje_naissance + (af + cf + paje_base) * (act(1) + act(2)) + pf_g) / 1000,
              qf = (ir * (1 + act(1) + act(2)) + qf_g) / 1000, n_g)
  stopifnot(all(v$n_g == nrow(GRILLE)))
  select(v, -n_g)
}
age0 <- bind_rows(sim %>% filter(variante %in% c("propre", "b2014")),
                  vie %>% filter(variante == "plafond2011", k == 0)) %>%
  select(base, variante, af, cf, paje_base, paje_naissance, ir)
ages_loi <- vie %>% filter(is.na(plafond) | variante == "plafond2011", k > 0) %>% select(base, variante, k, af, cf, ir)
v_loi <- valeur_vie(age0, ages_loi) %>%
  pivot_wider(names_from = variante, values_from = c(pf, qf))
d <- df %>% left_join(v_loi, by = c("cle" = "base")) %>%
  rename(pf_vie = pf_propre, qf_vie = qf_propre, pf_vie_ref = pf_b2014, qf_vie_ref = qf_b2014) %>%
  mutate(vie = pf_vie + qf_vie, vie_ref = pf_vie_ref + qf_vie_ref)
stopifnot(!anyNA(select(d, pf_vie, qf_vie, pf_vie_ref, qf_vie_ref, qf_plafond2011)))

valeurs <- bind_rows(
  d %>% group_by(groupe = as.character(annee)) %>%
    summarise(prestations_keur = pond(pf_vie, poids), qf_keur = pond(qf_vie, poids), .groups = "drop"),
  d %>% filter(annee == 2023) %>% group_by(groupe = paste("2023", q_nv)) %>%
    summarise(prestations_keur = pond(pf_vie, poids), qf_keur = pond(qf_vie, poids), .groups = "drop")) %>%
  mutate(part_qf_pct = 100 * qf_keur / (prestations_keur + qf_keur))
cat("\nValeur d'un enfant sur l'enfance (k€ de 2023, actualisée à 2 %) :\n")
print(as.data.frame(valeurs %>% mutate(across(where(is.double), ~ round(.x, 1)))), row.names = FALSE)

# ── §2 Effets séparés des prestations et du QF ─────────────────────────────
estimer <- function(dd, fe, vars = c("pf_vie", "qf_vie")) {
  refs <- paste0(vars, "_ref")
  feols(as.formula(paste0("naiss01 ~ ", paste(c(vars, refs), collapse = " + "), " + ", CTRL, " | ", fe)),
        data = dd, weights = ~poids, vcov = "hetero")
}
resumer <- function(m, m_tot, dd) {
  t2 <- c("pf_vie", "qf_vie")
  dlt <- coef(m)[["pf_vie"]] - coef(m)[["qf_vie"]]; s <- se_comb(m, c(1, -1), t2)
  tibble(prestations_pts = 100 * coef(m)[["pf_vie"]], se_prestations = 100 * se_comb(m, c(1, 0), t2),
         qf_pts = 100 * coef(m)[["qf_vie"]], se_qf = 100 * se_comb(m, c(0, 1), t2),
         ecart_pts = 100 * dlt, se_ecart = 100 * s, p_ecart = 2 * pnorm(-abs(dlt / s)),
         commun_pts = 100 * coef(m_tot)[["vie"]], se_commun = 100 * se_comb(m_tot, 1, "vie"),
         n = nobs(m), naissances = sum(dd$naiss01), taux_pct = 100 * pond(dd$naiss01, dd$poids))
}
ECHANTILLONS <- list("2005-2023" = function(x) x, "2014-2023" = function(x) filter(x, annee >= 2014),
                     "Moitie modeste (Q1-Q2)" = function(x) filter(x, q_nv %in% c("Q1", "Q2")),
                     "Moitie aisee (Q3-Q4)" = function(x) filter(x, q_nv %in% c("Q3", "Q4")),
                     "Rang 2 et plus" = function(x) filter(x, rang3 != "1er enfant"))
MODELES <- list()
effets <- imap_dfr(SPECS, function(fe, spec) imap_dfr(ECHANTILLONS, function(f, ech) {
  dd <- f(d)
  m <- estimer(dd, fe); m_tot <- estimer(dd, fe, "vie")
  MODELES[[paste(spec, ech, sep = " | ")]] <<- list(m = m, m_tot = m_tot)
  resumer(m, m_tot, dd) %>% mutate(specification = spec, echantillon = ech, .before = 1)
}))
cat("\nEffet de 1 000 € de valeur sur l'enfance (points de probabilité de naissance) :\n")
print(as.data.frame(effets %>% mutate(across(where(is.double), ~ round(.x, 3)))), row.names = FALSE)

# ── §3 Le plafonnement du QF de 2012-2013 ──────────────────────────────────
#  Exposition (k€ de l'enfance) = QF au barème de 2014 - QF au même barème
#  avec le plafond de 2011 : négative pour les seuls couples dont l'avantage
#  par enfant dépassait le nouveau plafond. Exposition aux réformes des
#  prestations (âge 0, barème 2019 - barème 2013) en contrôle.
pf_age0 <- sim %>% filter(variante %in% c("b2013", "b2019")) %>%
  transmute(cle = base, variante = paste0("pf0_", variante), pf = (af + cf + paje_base + paje_naissance) / 1000) %>%
  pivot_wider(names_from = variante, values_from = pf)
d <- d %>% left_join(pf_age0, by = "cle") %>%
  mutate(expo_qf = qf_vie_ref - qf_plafond2011, expo_pf = pf0_b2019 - pf0_b2013,
         touche = expo_qf < -0.5, apres = annee >= 2013)
exposition <- d %>% filter(annee %in% 2011:2014) %>% group_by(groupe = as.character(q_nv)) %>%
  summarise(part_touches_pct = 100 * pond(touche, poids), perte_moyenne_keur = -pond(expo_qf, poids),
            perte_touches_keur = -pond(if_else(touche, expo_qf, NA_real_), poids), .groups = "drop")
cat("\nExposition au plafonnement (couples de 2011-2014, k€ de valeur sur l'enfance) :\n")
print(as.data.frame(exposition %>% mutate(across(where(is.double), ~ round(.x, 2)))), row.names = FALSE)

plafonnement <- imap_dfr(SPECS[1:2], function(fe, spec) {
  dd <- filter(d, annee != 2012)
  base_f <- paste0("expo_qf + expo_pf + i(annee, expo_pf, ref = 2011) + pf_vie_ref + qf_vie_ref + ", CTRL, " | ", fe)
  #  Tests joints en erreurs robustes (groupées : matrice non définie positive avec 50 grappes)
  ev <- feols(as.formula(paste0("naiss01 ~ i(annee, expo_qf, ref = 2011) + ", base_f)),
              data = d, weights = ~poids, vcov = "hetero")
  dd_m <- feols(as.formula(paste0("naiss01 ~ expo_qf:apres + ", base_f)), data = dd, weights = ~poids, vcov = ~groupe)
  ct <- as.data.frame(coeftable(ev)); ct$terme <- rownames(ct)
  annees <- ct %>% filter(grepl("^annee::[0-9]+:expo_qf$", terme)) %>%
    transmute(annee = as.integer(sub("annee::([0-9]+):.*", "\\1", terme)),
              coef_pts = 100 * Estimate, se = 100 * `Std. Error`)
  test <- function(ans) wald(ev, keep = paste0("^annee::(", paste(ans, collapse = "|"), "):expo_qf$"), print = FALSE)$p
  dd_t <- as.data.frame(coeftable(dd_m))["expo_qf:apresTRUE", ]
  bind_rows(annees %>% mutate(mesure = "Annee"),
            tibble(mesure = c("p joint 2005-2010", "p joint 2013-2023"), coef_pts = c(test(2005:2010), test(2013:2023))),
            tibble(mesure = "Double difference (apres 2013, sans 2012)", coef_pts = 100 * dd_t$Estimate,
                   se = 100 * dd_t$`Std. Error`, p = dd_t$`Pr(>|t|)`)) %>%
    mutate(specification = spec, .before = 1)
})
cat("\nPlafonnement du QF : effet de 1 000 € de QF sur l'enfance (points), référence 2011 :\n")
print(as.data.frame(plafonnement %>% mutate(across(where(is.double), ~ round(.x, 3)))), row.names = FALSE)

# ── §4 Réforme à budget constant, couples de 2023 ──────────────────────────
#  Recette = hausse de l'impôt des couples sans l'enfant à naître ; forfait
#  par enfant de moins de 20 ans = recette / nombre d'enfants (pondérés).
#  Pour un enfant de plus : prestations + forfait sur 20 ans actualisé,
#  QF sous la réforme. Naissances prédites avec les effets de l'étape 1.
loi23 <- sim %>% filter(variante == "propre", legis == 2023) %>% select(cle = base, ir_sans_loi = ir_sans)
n_enf <- d23 %>% transmute(cle, poids, q_nv, rang3, naiss01,
                           enfants = map_int(strsplit(coalesce(ages, ""), ";"), ~ sum(as.integer(.x) < 20, na.rm = TRUE)))
#  La référence (effets d'année par décile x enfants x âge) d'abord : avec des
#  effets d'année communs, l'effet négatif des prestations (signe non
#  crédible) tient à des tendances propres au revenu, à la taille de la
#  famille et à l'âge.
SPECS_PREDICTION <- c("Reference | 2005-2023", "Annee x decile x nombre d'enfants | 2005-2023",
                      "Effets d'annee communs | 2005-2023", "Reference | 2014-2023")
reforme <- imap_dfr(PLAFONDS, function(pl, nom) {
  vr <- vie %>% filter(plafond == pl)
  qf_r <- valeur_vie(vr %>% filter(k == 0) %>% select(base, variante, af, cf, paje_base, paje_naissance, ir),
                     vr %>% filter(k > 0) %>% select(base, variante, k, af, cf, ir)) %>%
    transmute(cle = base, qf_reforme = qf)
  x <- n_enf %>%
    left_join(vr %>% filter(k == 0) %>% transmute(cle = base, ir_sans_ref = ir_sans), by = "cle") %>%
    left_join(loi23, by = "cle") %>% left_join(qf_r, by = "cle") %>%
    left_join(select(d, cle, qf_vie), by = "cle") %>%
    mutate(hausse_impot = ir_sans_loi - ir_sans_ref)
  stopifnot(!anyNA(x$hausse_impot), !anyNA(x$qf_reforme))
  recette <- sum(x$poids * x$hausse_impot)
  forfait <- recette / sum(x$poids * x$enfants)                       # € par enfant et par an
  x <- x %>% mutate(d_pf = forfait * sum((1 + TAUX)^(-(0:19))) / 1000, d_qf = qf_reforme - qf_vie)
  map_dfr(SPECS_PREDICTION, function(sp) {
    mm <- MODELES[[sp]]; b <- coef(mm$m)[c("pf_vie", "qf_vie")]
    groupes <- list("Ensemble" = x, "Moitie modeste (Q1-Q2)" = filter(x, q_nv %in% c("Q1", "Q2")),
                    "Moitie aisee (Q3-Q4)" = filter(x, q_nv %in% c("Q3", "Q4")))
    imap_dfr(groupes, function(g, nom_g) {
      a <- c(pond(g$d_pf, g$poids), pond(g$d_qf, g$poids))
      dp <- sum(a * b); s <- se_comb(mm$m, a, names(b))
      tibble(scenario = nom, specification = sp, groupe = nom_g,
             recette_md_eur = recette / 1e9, forfait_eur_mois = forfait / 12,
             gain_prestations_keur = a[1], perte_qf_keur = -a[2],
             part_valeur_en_hausse_pct = 100 * pond(g$d_pf + g$d_qf > 0, g$poids),
             taux_pct = 100 * pond(g$naiss01, g$poids),
             effet_pts = 100 * dp, bas_pts = 100 * (dp - 1.96 * s), haut_pts = 100 * (dp + 1.96 * s),
             effet_naissances_pct = 100 * dp / pond(g$naiss01, g$poids),
             bas_naissances_pct = 100 * (dp - 1.96 * s) / pond(g$naiss01, g$poids),
             haut_naissances_pct = 100 * (dp + 1.96 * s) / pond(g$naiss01, g$poids),
             # Même effet par euro pour les deux instruments
             effet_commun_pts = 100 * coef(mm$m_tot)[["vie"]] * sum(a),
             se_commun_pts = 100 * se_comb(mm$m_tot, 1, "vie") * abs(sum(a)))
    })
  })
})
cat("\nRéforme à budget constant (couples de 2023) : effet sur la probabilité de naissance :\n")
print(as.data.frame(reforme %>% mutate(across(where(is.double), ~ round(.x, 3)))), row.names = FALSE)

# ── Sortie ─────────────────────────────────────────────────────────────────
bind_rows(valeurs %>% mutate(bloc = "Valeur sur l'enfance"),
          effets %>% mutate(bloc = "Effets separes"),
          exposition %>% mutate(bloc = "Plafonnement, exposition"),
          plafonnement %>% mutate(bloc = "Plafonnement, effets"),
          reforme %>% mutate(bloc = "Reforme a budget constant")) %>%
  relocate(bloc) %>%
  write_csv("Output/erfs_qf_prestations.csv")
cat("Résultats écrits : Output/erfs_qf_prestations.csv\n")
