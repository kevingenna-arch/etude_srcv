# ============================================================================
#  ERFS — 64. Gain complet d'un enfant : RSA, prime d'activité, aides au logement
# ----------------------------------------------------------------------------
#  Le gain d'un enfant des scripts 57 à 63 ne compte que les prestations
#  familiales (allocations familiales, complément familial, PAJE) et l'impôt.
#  Pour les familles modestes, une naissance change aussi le RSA, la prime
#  d'activité et les aides au logement. Ce script les ajoute (OpenFisca local,
#  script 64_gain_complet_erfs.py) et refait les modèles du script 57.
#
#  1. Simulation : écart entre la situation avec et sans nouveau-né, sous le
#     barème de l'année et sous celui de 2014 (revenus et loyers en euros de
#     2014). Revenus d'activité de chaque conjoint en salaires, le reste de
#     leurs revenus imposables en allocations chômage. Statut d'occupation de
#     l'ERFS : locataires du parc privé ou HLM ; propriétaires et accédants
#     sans aide (l'APL accession n'est pas simulée). Zone d'aide au logement
#     déduite de l'unité urbaine : agglomération parisienne en zone 1, reste
#     de l'Île-de-France et unités urbaines de 200 000 habitants ou plus en
#     zone 2, le reste en zone 3. Loyer : observé en 2014-2016 et 2018-2020 ;
#     sinon médiane de 2014-2020 (euros constants) des locataires de même
#     parc, zone et taille de ménage. Le loyer dépasse en général le loyer
#     plafond : c'est le plafond qui compte.
#  2. Contrôle : aides au logement, RSA et prime d'activité simulés face aux
#     montants observés dans l'ERFS.
#  3. Le gain complet par année et par quartile, et son recul entre les
#     barèmes de 2014 et de 2023.
#  4. Modèles du script 57 avec le gain complet, comparés au gain du script
#     57 sur le même échantillon ; erreurs-types robustes et groupées par
#     décile de niveau de vie x nombre d'enfants (la plus prudente retenue).
#  5. Part de la baisse de l'ICF 2014-2023 imputable au barème complet.
#
#  ⚠️ Le RMI n'a pas ses paramètres dans OpenFisca-France avant juin 2009 :
#     champ 2010-2023. Statut d'occupation inconnu pour près de 80 % des
#     couples en 2013 et 2017 : années simulées mais hors des estimations.
#     Ressources du RSA et de la prime d'activité supposées stables dans
#     l'année ; revenus du patrimoine ignorés.
#  Entrées : R/57_erfs_preparation.R (couples, gain du script 57),
#            R/64_gain_complet_erfs.py (OpenFisca, local)
#  Sorties : Output/gain_complet_erfs.csv (simulations, cache),
#            Output/erfs_gain_complet.csv
# ============================================================================

source("R/57_erfs_preparation.R")   # df, simuler(), IPC, pond(), deflater()

ANNEES      <- 2010:2023
SANS_STATUT <- c(2013L, 2017L)
CTRL        <- "nv_10k + I(nv_10k^2)"   # comme au script 57 ; FE : R/57_erfs_preparation.R
COMP_57     <- c("af", "cf", "paje_base", "paje_naissance", "ir")
COMP_64     <- c(COMP_57, "rsa", "ppa", "aide_logement")
vers_annee  <- function(x, an) x * IPC$ipc[match(an, IPC$annee)] / IPC$ipc[IPC$annee == EUROS]
baisse_icf  <- ICF_2023 / ICF_2014 - 1

d <- df %>% filter(annee %in% ANNEES) %>%
  mutate(femme_1   = rev_femme >= rev_homme,
         act1      = round(if_else(femme_1, act_rev_femme, act_rev_homme)),
         act2      = round(if_else(femme_1, act_rev_homme, act_rev_femme)),
         statut_of = case_when(statut == "Locataire prive" ~ "locataire_vide", statut == "Locataire HLM" ~ "locataire_hlm",
                               statut %in% c("Accedant", "Proprietaire") ~ "proprietaire",
                               statut == "Loge gratuitement" ~ "loge_gratuitement", TRUE ~ "non_renseigne"),
         zone      = case_when(tuu == "Agglomeration parisienne" ~ "zone_1",
                               region == "Ile-de-France" | tuu == "200 000 et plus" ~ "zone_2", TRUE ~ "zone_3"),
         taille    = pmin(2L + nb_enf_avant, 5L),
         locataire = statut_of %in% c("locataire_vide", "locataire_hlm"),
         loyer_obs = if_else(locataire & !is.na(loyer) & loyer > 0, loyer, NA_real_))

# ── §1 Loyers et simulation ────────────────────────────────────────────────
loyers_cellule <- d %>% filter(!is.na(loyer_obs)) %>%
  mutate(loyer_23 = deflater(loyer_obs, annee, IPC, base = EUROS)) %>%
  group_by(statut_of, zone, taille) %>%
  summarise(loyer_cellule_23 = wquantile(loyer_23, poids, 0.5), locataires = n(), .groups = "drop")
cat("\nLoyer médian des locataires, 2014-2020 (€ de 2023 par mois) :\n")
print(as.data.frame(loyers_cellule %>% mutate(loyer_cellule_23 = round(loyer_cellule_23))), row.names = FALSE)
d <- d %>% left_join(loyers_cellule %>% select(-locataires), by = c("statut_of", "zone", "taille")) %>%
  mutate(loyer_sim = case_when(!locataire ~ 0, !is.na(loyer_obs) ~ loyer_obs,
                               TRUE ~ vers_annee(loyer_cellule_23, annee)))
stopifnot(!anyNA(d$loyer_sim))

ligne64 <- function(x, b = NULL) {
  en <- function(v) if (is.null(b)) round(v) else round(deflater(v, x$annee, IPC, base = b))
  x %>% transmute(cle = if (is.null(b)) cle else paste0(cle, "_b", b), legis = b %||% annee,
                  rev1 = en(rev1), rev2 = en(rev2), act1 = en(act1), act2 = en(act2),
                  conj = conj_sim, ages, statut = statut_of, loyer = en(loyer_sim), zone)
}
entree <- bind_rows(ligne64(d), ligne64(d, BAREME_REF))
sim64 <- simuler(entree, "Output/gain_complet_erfs.csv", script = "R/64_gain_complet_erfs.py") %>%
  mutate(across(c(all_of(COMP_64), paste0(COMP_64, "_sans"), gain), ~ deflater(.x, legis, IPC, base = EUROS)),
         base = sub("_b[0-9]{4}$", "", cle), ref = grepl("_b[0-9]{4}$", cle))
d <- d %>%
  left_join(sim64 %>% filter(!ref) %>%
              select(cle = base, all_of(COMP_64), ends_with("_sans"), gain) %>%
              rename_with(~ paste0("c_", .x), -cle), by = "cle") %>%
  left_join(sim64 %>% filter(ref) %>% select(cle = base, c_gain_ref = gain), by = "cle") %>%
  mutate(c_gain_restreint = c_af + c_cf + c_paje_base + c_paje_naissance + c_ir,
         c_minima_logement = c_rsa + c_ppa + c_aide_logement,
         gain_c_1k = c_gain / 1000, gain_c_ref_1k = c_gain_ref / 1000)
stopifnot(!anyNA(d$c_gain), !anyNA(d$c_gain_ref))

# ── §2 Contrôle face aux montants observés ─────────────────────────────────
#  Situation observée : avec le nouveau-né s'il y a eu naissance.
controle <- d %>%
  mutate(al_sim = c_aide_logement_sans + naiss01 * c_aide_logement,
         rsa_sim = c_rsa_sans + naiss01 * c_rsa, ppa_sim = c_ppa_sans + naiss01 * c_ppa,
         across(c(prest_logement, prest_rsa, prest_ppa), ~ deflater(.x, annee, IPC, base = EUROS), .names = "{.col}_23")) %>%
  group_by(annee) %>%
  summarise(al_simule = pond(al_sim, poids), al_observe = pond(prest_logement_23, poids),
            al_simule_pct = 100 * pond(al_sim > 0, poids), al_observe_pct = 100 * pond(prest_logement > 0, poids),
            correlation_al = cov.wt(cbind(al_sim, prest_logement_23), poids, cor = TRUE)$cor[1, 2],
            rsa_simule = pond(rsa_sim, poids), rsa_observe = pond(prest_rsa_23, poids),
            rsa_simule_pct = 100 * pond(rsa_sim > 0, poids), rsa_observe_pct = 100 * pond(prest_rsa > 0, poids),
            ppa_simulee = pond(ppa_sim, poids), ppa_observee = pond(prest_ppa_23, poids),
            ppa_simulee_pct = 100 * pond(ppa_sim > 0, poids), ppa_observee_pct = 100 * pond(prest_ppa > 0, poids),
            gain_57 = pond(gain, poids), gain_restreint_64 = pond(c_gain_restreint, poids),
            correlation_gain = cov.wt(cbind(gain, c_gain_restreint), poids, cor = TRUE)$cor[1, 2], .groups = "drop")
cat("\nSimulation et montants observés (€ de 2023 par couple et par an ; % de bénéficiaires) :\n")
print(as.data.frame(controle %>% mutate(across(where(is.double), ~ round(.x, 2)))), row.names = FALSE)

# ── §3 Le gain complet ─────────────────────────────────────────────────────
resume_gain <- function(x) summarise(x, couples = n(), gain_57 = pond(gain, poids),
  gain_complet = pond(c_gain, poids), prestations_familiales = pond(c_af + c_cf + c_paje_base + c_paje_naissance, poids),
  quotient_familial = pond(c_ir, poids), rsa = pond(c_rsa, poids), prime_activite = pond(c_ppa, poids),
  aides_logement = pond(c_aide_logement, poids), part_gain_reduit_pct = 100 * pond(c_minima_logement < -50, poids),
  part_gain_accru_pct = 100 * pond(c_minima_logement > 50, poids),
  gain_complet_bareme_2014 = pond(c_gain_ref, poids), gain_57_bareme_2014 = 1000 * pond(gain_ref_1k, poids),
  .groups = "drop")
gains <- bind_rows(
  d %>% group_by(groupe_resume = as.character(annee)) %>% resume_gain(),
  d %>% filter(annee %in% c(2014, 2023)) %>% group_by(groupe_resume = paste(annee, q_nv)) %>% resume_gain(),
  d %>% filter(annee == 2023) %>% group_by(groupe_resume = paste("2023", rang3)) %>% resume_gain())
cat("\nGain d'un enfant supplémentaire, complet et du script 57 (€ de 2023 par an) :\n")
print(as.data.frame(gains %>% mutate(across(where(is.double), ~ round(.x, 1)))), row.names = FALSE)

# ── §4 Modèles ─────────────────────────────────────────────────────────────
MESURES <- list("Gain du script 57" = c("gain_1k", "gain_ref_1k"),
                "Gain complet" = c("gain_c_1k", "gain_c_ref_1k"))
PERIODES <- list("2010-2023" = setdiff(ANNEES, SANS_STATUT), "2014-2023" = setdiff(2014:2023, SANS_STATUT))
ECHANTILLONS <- list("Ensemble" = function(x) x, "1er enfant" = function(x) filter(x, rang3 == "1er enfant"),
                     "2e enfant" = function(x) filter(x, rang3 == "2e enfant"),
                     "3e enfant ou plus" = function(x) filter(x, rang3 == "3e enfant ou plus"),
                     "Moitie modeste (Q1-Q2)" = function(x) filter(x, q_nv %in% c("Q1", "Q2")),
                     "Moitie aisee (Q3-Q4)" = function(x) filter(x, q_nv %in% c("Q3", "Q4")))
modele <- function(x, vars) {
  m <- feols(as.formula(paste("naiss01 ~", paste(vars, collapse = " + "), "+", CTRL, "|", FE)),
             data = x, weights = ~poids, vcov = "hetero")
  se_r <- se(m)[[vars[1]]]; se_g <- se(m, vcov = ~groupe)[[vars[1]]]; s <- max(se_r, se_g); b <- coef(m)[[vars[1]]]
  tibble(effet_pts = 100 * b, se_robuste = 100 * se_r, se_groupe = 100 * se_g, se = 100 * s,
         p = 2 * pnorm(-abs(b / s)), n = nobs(m), naissances = sum(x$naiss01),
         taux_pct = 100 * pond(x$naiss01, x$poids), gain_moyen = 1000 * pond(x[[vars[1]]], x$poids))
}
modeles <- imap_dfr(PERIODES, function(ans, per) imap_dfr(MESURES, function(vars, mes)
  imap_dfr(ECHANTILLONS, function(f, ech) modele(f(filter(d, annee %in% ans)), vars) %>%
             mutate(periode = per, mesure = mes, echantillon = ech, .before = 1))))
cat("\nEffet de 1 000 € de gain annuel sur la probabilité de naissance (points) :\n")
print(as.data.frame(modeles %>% mutate(across(where(is.double), ~ round(.x, 3)))), row.names = FALSE)

# ── §5 Part de la baisse de l'ICF 2014-2023 ────────────────────────────────
d23 <- filter(d, annee == 2023)
part_baisse <- modeles %>% filter(echantillon == "Ensemble") %>%
  mutate(ecart_gain_euros = if_else(mesure == "Gain complet", pond(d23$c_gain_ref - d23$c_gain, d23$poids),
                                    1000 * pond(d23$gain_ref_1k - d23$gain_1k, d23$poids)),
         p0 = pond(d23$naiss01, d23$poids),
         part_pct = effet_pts / 100 * ecart_gain_euros / 1000 / p0 / -baisse_icf * 100,
         ic_bas = (effet_pts - 1.96 * se) / 100 * ecart_gain_euros / 1000 / p0 / -baisse_icf * 100,
         ic_haut = (effet_pts + 1.96 * se) / 100 * ecart_gain_euros / 1000 / p0 / -baisse_icf * 100) %>%
  select(periode, mesure, ecart_gain_euros, effet_pts, se, part_pct, ic_bas, ic_haut)
cat("\nPart de la baisse de l'ICF 2014-2023 imputable au barème (%) :\n")
print(as.data.frame(part_baisse %>% mutate(across(where(is.double), ~ round(.x, 2)))), row.names = FALSE)

# ── Sortie ─────────────────────────────────────────────────────────────────
bind_rows(loyers_cellule %>% mutate(bloc = "Loyers des locataires"),
          controle %>% mutate(bloc = "Simulation et montants observes"),
          gains %>% mutate(bloc = "Gain complet"),
          modeles %>% mutate(bloc = "Modeles"),
          part_baisse %>% mutate(bloc = "Part de la baisse 2014-2023")) %>%
  relocate(bloc) %>%
  write_csv("Output/erfs_gain_complet.csv")
cat("\nRésultats écrits : Output/erfs_gain_complet.csv\n")
