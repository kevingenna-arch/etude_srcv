# ============================================================================
#  ERFS — 63. Effet du barème sur les naissances : trois vérifications
# ----------------------------------------------------------------------------
#  Les scripts 57 et 60 concluent que le barème (prestations d'entretien, PAJE,
#  quotient familial) n'a pas d'effet mesurable sur les naissances. Trois
#  points fragiles de cette conclusion sont repris ici.
#
#  1. Inférence. Les erreurs-types des scripts 57 et 60 sont robustes mais non
#     groupées, alors que le barème varie par année, revenu et taille de la
#     famille, et qu'un tiers des couples est interrogé deux années de suite.
#     Les modèles principaux sont réestimés avec des erreurs-types groupées par
#     cellule (année x décile de niveau de vie x nombre d'enfants), par groupe
#     suivi dans le temps (décile x nombre d'enfants, 50 grappes) et par couple
#     (couples appariés d'une année sur l'autre, script 61). Les bornes de la
#     part de la baisse sont recalculées avec la plus prudente.
#  2. Revenu d'avant la naissance. L'ERFS donne les revenus de l'année de la
#     naissance, qu'un congé parental fait baisser. Pour les couples suivis de
#     N à N+1, le gain est resimulé avec les revenus de N (portés en euros de
#     N+1), le statut conjugal de N, le barème de N+1 et les enfants présents
#     avant la naissance. Sur le même échantillon, on compare l'effet estimé
#     avec les revenus de N+1 (la démarche du script 57) et avec ceux de N.
#     Variante : naissances conçues après l'enquête de N.
#  3. Rang de naissance. Où la natalité des couples a-t-elle reculé entre
#     2012-2014 et 2021-2023 : premier, deuxième enfant, suivants ? Recul du
#     taux à chaque rang (brut et à âge de la femme donné), contribution de
#     chaque rang, recul du gain dû au barème à chaque rang, et ce que l'effet
#     estimé par rang en fait attendre.
#
#  ⚠️ Rang = nombre d'enfants de moins de 21 ans présents dans le ménage plus
#     un : ce n'est pas le rang biologique. Seuls les couples sont dans le
#     champ : le recul de la vie en couple n'est pas dans ces décompositions.
#     L'enquête Emploi a été refondue en 2021.
#  Entrées : R/60_erfs_qf_prestations.R (couples, déciles, valeurs sur
#            l'enfance), R/61_erfs_panel.R, R/57_gain_enfant_erfs.py (local)
#  Sorties : Output/gain_enfant_erfs_avant.csv (simulations, cache),
#            Output/erfs_bareme_verifications.csv
# ============================================================================

source("R/60_erfs_qf_prestations.R")   # df (déciles d_nv), d (valeurs sur l'enfance), d23, CTRL, FE, SPECS
source("R/61_erfs_panel.R")            # panel : couples suivis de N à N+1

RANGS <- list("Ensemble" = function(x) x, "1er enfant" = function(x) filter(x, rang3 == "1er enfant"),
              "2e enfant" = function(x) filter(x, rang3 == "2e enfant"),
              "3e enfant ou plus" = function(x) filter(x, rang3 == "3e enfant ou plus"))
baisse_icf <- ICF_2023 / ICF_2014 - 1

# ── §1 Inférence ───────────────────────────────────────────────────────────
#  Couple : les deux observations d'un couple apparié partagent la même
#  grappe ; les couples non appariés (clé incomplète ou ambiguë) restent seuls.
lien <- select(panel, cle, cle_b)
grappes <- function(x) x %>%
  mutate(cellule = paste(annee, d_nv, nb_enf), groupe = paste(d_nv, nb_enf),
         couple = coalesce(lien$cle[match(cle, lien$cle_b)], cle))
df <- grappes(df); d <- grappes(d)
INFERENCES <- list(robuste = "hetero", cellule = ~cellule, groupe = ~groupe, couple = ~couple)

#  Un modèle, plusieurs matrices de variance : coefficient et erreurs-types en points.
ecarts_types <- function(f, data, termes, inferences = INFERENCES) {
  m <- feols(f, data = data, weights = ~poids, vcov = "hetero")
  out <- tibble(terme = termes, coef_pts = 100 * unname(coef(m)[termes]), n = nobs(m))
  for (nom in names(inferences))
    out[[paste0("se_", nom)]] <- 100 * unname(se(m, vcov = inferences[[nom]])[termes])
  out %>% mutate(se_max = do.call(pmax, across(starts_with("se_"))))
}
f_gain <- as.formula(paste("naiss01 ~ gain_1k + gain_ref_1k +", CTRL, "|", FE))
f_vie  <- function(vars, fe) as.formula(paste0("naiss01 ~ ", paste(c(vars, paste0(vars, "_ref")), collapse = " + "),
                                               " + ", CTRL, " | ", fe))
inf_gain <- imap_dfr(list("2005-2023" = 2005:2023, "2014-2023" = 2014:2023), function(ans, per)
  imap_dfr(RANGS, function(f, ech) {
    x <- f(filter(df, annee %in% ans))
    ecarts_types(f_gain, x, "gain_1k") %>%
      mutate(modele = "Gain de la premiere annee", periode = per, echantillon = ech,
             naissances = sum(x$naiss01), .before = 1)
  }))
inf_vie <- imap_dfr(SPECS[c("Reference", "Effets d'annee communs")], function(fe, spec)
  bind_rows(ecarts_types(f_vie(c("pf_vie", "qf_vie"), fe), d, c("pf_vie", "qf_vie")),
            ecarts_types(f_vie("vie", fe), d, "vie")) %>%
    mutate(modele = paste("Valeur sur l'enfance,", spec), periode = "2005-2023", echantillon = "Ensemble",
           naissances = sum(d$naiss01), .before = 1))
inference <- bind_rows(inf_gain, inf_vie) %>%
  mutate(rapport_max_robuste = se_max / se_robuste, p_robuste = 2 * pnorm(-abs(coef_pts / se_robuste)),
         p_max = 2 * pnorm(-abs(coef_pts / se_max)))
cat("\nEffet de 1 000 € (points de probabilité de naissance) et erreurs-types selon les grappes :\n")
print(as.data.frame(inference %>% mutate(across(where(is.double), ~ round(.x, 3)))), row.names = FALSE)
cat("Grappes :", n_distinct(df$cellule), "cellules,", n_distinct(df$groupe), "groupes,",
    n_distinct(df$couple), "couples pour", nrow(df), "observations\n")

#  Part de la baisse de l'ICF 2014-2023 : les couples de 2023 au barème de 2014 (script 57, §6)
dg <- pond(d23$gain_ref_1k - d23$gain_1k, d23$poids); p0 <- pond(d23$naiss01, d23$poids)
#  Points pour 1 000 € -> % de la baisse : effet relatif (au taux p) x recul du gain / recul de l'ICF
en_part <- function(pts, p = p0) pts * dg / p / -baisse_icf
part_baisse <- inf_gain %>% filter(echantillon == "Ensemble") %>%
  pivot_longer(c(se_robuste, se_cellule, se_groupe, se_couple), names_to = "inference", values_to = "se",
               names_prefix = "se_") %>%
  transmute(estimation = periode, inference, part_pct = en_part(coef_pts),
            ic_bas = en_part(coef_pts - 1.96 * se), ic_haut = en_part(coef_pts + 1.96 * se))
cat("\nPart de la baisse de l'ICF 2014-2023 imputable au barème (%), selon l'inférence :\n")
print(as.data.frame(part_baisse %>% mutate(across(where(is.double), ~ round(.x, 1)))), row.names = FALSE)

# ── §2 Revenu d'avant la naissance ─────────────────────────────────────────
#  Clés : `<cle de N+1>_avant` = barème de N+1, revenus de N en euros de N+1 ;
#  `<cle de N+1>_avant_b2014` = barème de 2014, revenus de N en euros de 2014.
ligne_avant <- function(p, a, b = NULL) {
  cible <- b %||% (a + 1L)
  p %>% filter(annee == a) %>%
    transmute(cle = paste0(cle_b, "_avant", if (is.null(b)) "" else paste0("_b", b)), legis = cible,
              rev1 = round(deflater(rev1, annee, IPC, base = cible)),
              rev2 = round(deflater(rev2, annee, IPC, base = cible)), conj = conj_sim, ages = ages_b)
}
entree_avant <- map_dfr(sort(unique(panel$annee)),
                        ~ bind_rows(ligne_avant(panel, .x), ligne_avant(panel, .x, BAREME_REF)))
gain_avant <- simuler(entree_avant, "Output/gain_enfant_erfs_avant.csv") %>%
  transmute(cle_b = sub("_avant.*$", "", cle),
            mesure = if_else(grepl("_b[0-9]{4}$", cle), "gain_av_ref_1k", "gain_av_1k"),
            gain = deflater(gain, legis, IPC, base = EUROS) / 1000) %>%
  pivot_wider(names_from = mesure, values_from = gain)

#  Variables de N+1 (suffixe _n1) : celles du script 57 pour la même naissance
VARS_N1 <- c("annee", "gain_1k", "gain_ref_1k", "nv_10k", "age_f", "age_h_cl", "nb_enf", "benjamin_cl",
             "dipl_femme", "dipl_homme", "act_homme", "conj", "statut", "tuu", "region", "d_nv", "age_cl", "rang3")
pb <- panel %>%
  left_join(df %>% select(cle_b = cle, all_of(VARS_N1)) %>% rename_with(~ paste0(.x, "_n1"), -cle_b), by = "cle_b") %>%
  left_join(gain_avant, by = "cle_b") %>%
  mutate(rang3 = rang3_n1,                                       # rang de la naissance suivie
         groupe = paste(d_nv, nb_enf_n1), groupe_n1 = paste(d_nv_n1, nb_enf_n1))
stopifnot(!anyNA(pb$gain_av_1k), !anyNA(pb$gain_av_ref_1k), !anyNA(pb$gain_1k_n1))
cat("\nPanel :", nrow(pb), "couples |", sum(pb$naiss), "naissances après l'enquête de N |", sum(pb$naiss_concue),
    "conçues après\n")

#  Ce que le revenu de l'année de la naissance change au gain mesuré
mesure_gain <- pb %>% group_by(groupe = if_else(naiss == 1, "Naissance", "Pas de naissance")) %>%
  summarise(couples = n(), gain_revenus_n1 = 1000 * pond(gain_1k_n1, poids), gain_revenus_n = 1000 * pond(gain_av_1k, poids),
            ecart_moyen = 1000 * pond(gain_1k_n1 - gain_av_1k, poids),
            ecart_absolu_moyen = 1000 * pond(abs(gain_1k_n1 - gain_av_1k), poids),
            correlation = cov.wt(cbind(gain_1k_n1, gain_av_1k), poids, cor = TRUE)$cor[1, 2],
            revenu_n1_sur_n_pct = 100 * (pond(nv_10k_n1, poids) / pond(nv_10k, poids) - 1), .groups = "drop")
cat("\nGain d'un enfant selon l'année des revenus (€ de 2023 ; niveau de vie hors prestations) :\n")
print(as.data.frame(mesure_gain %>% mutate(across(where(is.double), ~ round(.x, 2)))), row.names = FALSE)

FE_N1 <- paste0(gsub("(\\w+)", "\\1_n1", FE))                    # tout mesuré en N+1, comme au script 57
#  Mesuré en N, sauf la composition avant la naissance suivie (nombre d'enfants, benjamin : N+1)
FE_N  <- gsub("\\bbenjamin_cl\\b", "benjamin_cl_n1", gsub("\\bnb_enf\\b", "nb_enf_n1", FE, perl = TRUE), perl = TRUE)
MODELES_PANEL <- list(
  "Revenus de N+1 (demarche du script 57)" = list(
    y = "naiss", f = paste("~ gain_1k_n1 + gain_ref_1k_n1 + nv_10k_n1 + I(nv_10k_n1^2) |", FE_N1),
    terme = "gain_1k_n1", grappe = ~groupe_n1, filtre = function(x) x),
  "Revenus de N (avant la naissance)" = list(
    y = "naiss", f = paste("~ gain_av_1k + gain_av_ref_1k +", CTRL, "|", FE_N),
    terme = "gain_av_1k", grappe = ~groupe, filtre = function(x) x),
  "Revenus de N, naissances concues apres l'enquete" = list(
    y = "naiss_concue", f = paste("~ gain_av_1k + gain_av_ref_1k +", CTRL, "|", FE_N),
    terme = "gain_av_1k", grappe = ~groupe, filtre = function(x) x),
  #  Barème connu à la conception : gain du script 57 en N, couples sans naissance en N
  "Revenus et bareme de N, sans naissance en N" = list(
    y = "naiss", f = paste("~ gain_1k + gain_ref_1k +", CTRL, "|", FE_N),
    terme = "gain_1k", grappe = ~groupe, filtre = function(x) filter(x, naiss01 == 0)))
panel_modeles <- imap_dfr(MODELES_PANEL, function(sp, nom) imap_dfr(RANGS, function(f, ech) {
  x <- f(sp$filtre(pb))
  ecarts_types(as.formula(paste(sp$y, sp$f)), x, sp$terme, list(robuste = "hetero", groupe = sp$grappe)) %>%
    mutate(modele = nom, echantillon = ech, naissances = sum(x[[sp$y]]),
           taux_pct = 100 * pond(x[[sp$y]], x$poids), .before = 1)
})) %>%
  #  Part de la baisse : effet rapporté au taux de naissance de l'échantillon (les naissances
  #  conçues après l'enquête ne sont qu'une partie des naissances de l'année)
  mutate(part_baisse_pct = if_else(echantillon == "Ensemble", en_part(coef_pts, taux_pct / 100), NA_real_),
         part_ic_bas = if_else(echantillon == "Ensemble", en_part(coef_pts - 1.96 * se_max, taux_pct / 100), NA_real_),
         part_ic_haut = if_else(echantillon == "Ensemble", en_part(coef_pts + 1.96 * se_max, taux_pct / 100), NA_real_))
cat("\nPanel : effet de 1 000 € de gain annuel sur la probabilité de naissance (points) :\n")
print(as.data.frame(panel_modeles %>% mutate(across(where(is.double), ~ round(.x, 3)))), row.names = FALSE)

# ── §3 Rang de naissance ───────────────────────────────────────────────────
rang_annee <- df %>% group_by(annee) %>% mutate(part = poids / sum(poids)) %>%
  group_by(annee, rang3) %>%
  summarise(part_couples_pct = 100 * sum(part), naissances = sum(naiss01), taux_pct = 100 * pond(naiss01, poids),
            gain = 1000 * pond(gain_1k, poids), gain_bareme_2014 = 1000 * pond(gain_ref_1k, poids), .groups = "drop")
cat("\nTaux de naissance des couples par rang (%) :\n")
print(as.data.frame(rang_annee %>% select(annee, rang3, taux_pct) %>% mutate(taux_pct = round(taux_pct, 2)) %>%
                      pivot_wider(names_from = rang3, values_from = taux_pct)), row.names = FALSE)

#  Décomposition entre deux périodes : T = somme des parts x taux ; pour chaque
#  rang, effet du taux (à part moyenne) et effet de la part (à taux moyen).
AVANT <- 2012:2014; APRES <- 2021:2023
dr <- df %>% filter(annee %in% c(AVANT, APRES)) %>%
  mutate(periode = if_else(annee %in% AVANT, "avant", "apres"),
         age5 = cut(age_femme, c(14, 24, 29, 34, 39, 44, 49)))
parts_taux <- dr %>% group_by(periode) %>% mutate(part = poids / sum(poids)) %>%
  group_by(periode, rang3) %>%
  summarise(part = sum(part), taux = pond(naiss01, poids), naissances = sum(naiss01),
            ecart_bareme = pond(gain_1k - gain_ref_1k, poids), gain_ref = pond(gain_ref_1k, poids), .groups = "drop") %>%
  pivot_wider(names_from = periode, values_from = c(part, taux, naissances, ecart_bareme, gain_ref))
#  Taux d'après à structure par âge de la femme d'avant, dans chaque rang
ages_avant <- dr %>% filter(periode == "avant") %>% group_by(rang3, age5) %>%
  summarise(w = sum(poids), .groups = "drop_last") %>% mutate(w = w / sum(w)) %>% ungroup()
taux_std <- dr %>% filter(periode == "apres") %>% group_by(rang3, age5) %>%
  summarise(taux = pond(naiss01, poids), .groups = "drop") %>%
  inner_join(ages_avant, by = c("rang3", "age5")) %>% group_by(rang3) %>%
  summarise(taux_apres_age_donne = sum(taux * w) / sum(w), .groups = "drop")
total <- with(parts_taux, sum(part_apres * taux_apres) - sum(part_avant * taux_avant))
#  Effet du barème attendu à chaque rang : effet estimé (2005-2023) x recul du gain dû au barème
#  (toutes les naissances, revenus de l'année de la naissance) ; en regard, l'effet estimé sur le
#  panel avec les revenus d'avant la naissance.
effets_rang <- inf_gain %>% filter(periode == "2005-2023", echantillon != "Ensemble") %>%
  select(rang3 = echantillon, effet_pts = coef_pts, effet_se = se_max) %>%
  left_join(panel_modeles %>% filter(modele == "Revenus de N (avant la naissance)") %>%
              select(rang3 = echantillon, effet_revenus_avant_pts = coef_pts, effet_revenus_avant_se = se_max),
            by = "rang3")
rang_decomposition <- parts_taux %>% left_join(taux_std, by = "rang3") %>%
  mutate(rang3 = as.character(rang3)) %>% left_join(effets_rang, by = "rang3") %>%
  transmute(rang = rang3, naissances_avant, naissances_apres,
            part_couples_avant_pct = 100 * part_avant, part_couples_apres_pct = 100 * part_apres,
            taux_avant_pct = 100 * taux_avant, taux_apres_pct = 100 * taux_apres,
            taux_apres_age_donne_pct = 100 * taux_apres_age_donne,
            recul_taux_pct = 100 * (taux_apres / taux_avant - 1),
            recul_taux_age_donne_pct = 100 * (taux_apres_age_donne / taux_avant - 1),
            effet_taux_pts = 100 * (part_avant + part_apres) / 2 * (taux_apres - taux_avant),
            effet_part_pts = 100 * (taux_avant + taux_apres) / 2 * (part_apres - part_avant),
            contribution_pct = 100 * (effet_taux_pts + effet_part_pts) / (100 * total),
            contribution_taux_pct = 100 * effet_taux_pts / (100 * total),
            recul_gain_bareme_eur = -1000 * (ecart_bareme_apres - ecart_bareme_avant),
            recul_gain_bareme_pct = 100 * (ecart_bareme_apres - ecart_bareme_avant) / gain_ref_apres,
            effet_pts, effet_se,
            #  Variation du taux attendue du recul du barème, et sa part dans le recul observé du taux
            attendu_pts = effet_pts * (ecart_bareme_apres - ecart_bareme_avant),
            attendu_bas_pts = (effet_pts + 1.96 * effet_se) * (ecart_bareme_apres - ecart_bareme_avant),
            attendu_haut_pts = (effet_pts - 1.96 * effet_se) * (ecart_bareme_apres - ecart_bareme_avant),
            part_du_recul_pct = 100 * attendu_pts / (100 * (taux_apres - taux_avant)),
            effet_revenus_avant_pts, effet_revenus_avant_se)
cat("\nNatalité des couples par rang,", paste(range(AVANT), collapse = "-"), "->", paste(range(APRES), collapse = "-"),
    ": taux d'ensemble", round(100 * sum(parts_taux$part_avant * parts_taux$taux_avant), 2), "% ->",
    round(100 * sum(parts_taux$part_apres * parts_taux$taux_apres), 2), "% (", round(100 * total, 2), "point )\n")
print(as.data.frame(rang_decomposition %>% mutate(across(where(is.double), ~ round(.x, 2)))), row.names = FALSE)

# ── Sortie ─────────────────────────────────────────────────────────────────
bind_rows(inference %>% mutate(bloc = "Inference"),
          part_baisse %>% mutate(bloc = "Part de la baisse selon l'inference"),
          mesure_gain %>% mutate(bloc = "Panel : mesure du gain"),
          panel_modeles %>% mutate(bloc = "Panel : modeles"),
          rang_annee %>% mutate(rang = as.character(rang3), .keep = "unused", bloc = "Rang : par annee"),
          rang_decomposition %>% mutate(bloc = "Rang : decomposition")) %>%
  relocate(bloc) %>%
  write_csv("Output/erfs_bareme_verifications.csv")
cat("\nRésultats écrits : Output/erfs_bareme_verifications.csv\n")
