# ============================================================================
#  ERFS — 57. Préparation : couples, gain d'un enfant simulé par OpenFisca
# ----------------------------------------------------------------------------
#  §0 et §1 du script 57, partagés avec le script 60 : couples de l'ERFS
#  2005-2023 (script 56), variables de contrôle, simulation OpenFisca du gain
#  d'un enfant sous le barème de l'année et sous les barèmes fixes (cache
#  Output/gain_enfant_erfs.csv). Crée `df` (un couple par ligne, gains en
#  € de 2023), `sim` (toutes les simulations), `ligne()` et `simuler()`, et
#  la spécification commune des modèles de gain (FE, FE_COMMUNS).
# ============================================================================

source("R/00_utils.R")
source("R/00_config.R")
suppressPackageStartupMessages(library(fixest))
#  Un seul fil : avec les effets fixes croisés, le calcul parallèle de fixest
#  (compilé pour R 4.5.2) faisait planter R 4.5.1 (« recursive gc invocation »).
setFixest_nthreads(1); setFixest_notes(FALSE)
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
                    labels = c("Q1", "Q2", "Q3", "Q4")),
         d_nv = factor(findInterval(nv_reel, wquantile(nv_reel, poids, seq(0.1, 0.9, 0.1))))) %>%
  ungroup() %>%
  mutate(age_cl  = cut(age_femme, c(-Inf, 24, 29, 34, 39, Inf)),
         groupe  = paste(d_nv, nb_enf),                         # grappes : décile x nombre d'enfants (50)
         rev_men = nv_reel * pmax(uc_avant, 1))                 # revenu disponible hors prestations, € de 2023 par an
cat("Champ :", nrow(df), "couples |", sum(df$naiss01), "naissances |", n_distinct(df$annee), "millésimes\n")

# ── Spécification des modèles de gain (scripts 57, 60, 63, 64) ─────────────
#  FE_COMMUNS : effets d'année communs à tous les couples (référence jusqu'au
#  12/11/2026). Ils supposent que, hors barème, la natalité évolue de la même
#  façon à tout âge, revenu et rang. Ce n'est pas le cas : de 2012-2014 à
#  2021-2023, les naissances reculent de 22 à 25 % avant 30 ans et progressent
#  de 16 à 18 % après 35 ans (report), quand les plus âgés perdent le plus au
#  barème ; l'effet estimé s'en trouve tiré vers zéro. FE (référence) donne
#  ses effets d'année à chaque cellule décile de niveau de vie x nombre
#  d'enfants x âge de la femme en 5 classes (4 750 cellules) : le report
#  diffère selon le rang, et une tendance d'âge commune aux rangs gonflait
#  l'effet (0,23 contre 0,16 point pour 1 000 €). Le placebo ne départage pas
#  ces spécifications : toutes le passent avec un test fiable (script 57,
#  bloc « Effets temporels »).
FE_COMMUNS <- "annee + age_f + age_h_cl + nb_enf + benjamin_cl + dipl_femme + dipl_homme + act_homme + conj + statut + tuu + region"
FE         <- paste(FE_COMMUNS, "+ annee^d_nv^nb_enf^age_cl")
#  Gain relatif : en % du revenu du ménage hors prestations, plancher au 5e
#  centile (les revenus quasi nuls donneraient des parts démesurées).
PLANCHER_REVENU <- wquantile(df$rev_men, df$poids, 0.05)

# ── §1 Simulation OpenFisca (seules les situations manquantes sont simulées)
#  Clés : `cle` = barème de l'année ; `cle_bAAAA` = barème AAAA, revenus
#  ramenés en euros de AAAA.
ligne <- function(d, b = NULL) d %>%
  transmute(cle = if (is.null(b)) cle else paste0(cle, "_b", b), legis = b %||% annee,
            rev1 = if (is.null(b)) rev1 else round(deflater(rev1, annee, IPC, base = b)),
            rev2 = if (is.null(b)) rev2 else round(deflater(rev2, annee, IPC, base = b)),
            conj = conj_sim, ages)
simuler <- function(entree, f_out, script = "R/57_gain_enfant_erfs.py") {
  deja <- if (file.exists(f_out)) read_csv(f_out, show_col_types = FALSE) else NULL
  manque <- if (is.null(deja)) entree else anti_join(entree, deja, by = c("cle", "legis"))
  if (nrow(manque)) {
    cat("Simulation OpenFisca :", nrow(manque), "situations\n")
    f_in <- "Output/gain_erfs_entree.csv"; f_tmp <- "Output/gain_erfs_lot.csv"
    write_csv(manque, f_in)
    code <- system2(PYTHON, c("-X", "utf8", script, f_in, f_tmp, PROCESSUS))
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
         expo18_1k   = (b2019 - b2017) / 1000,
         rev_d        = pmax(rev_men, PLANCHER_REVENU),
         gain_rel     = 100 * 1000 * gain_1k / rev_d,          # gain en % du revenu
         gain_rel_ref = 100 * 1000 * gain_ref_1k / rev_d,
         expo_rel     = 100 * 1000 * expo_1k / rev_d)
stopifnot(!anyNA(df$gain_1k), !anyNA(df$gain_ref_1k), !anyNA(df$expo_1k), !anyNA(df$gain_rel))
