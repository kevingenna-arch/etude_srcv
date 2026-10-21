# ============================================================================
#  SRCV — 38. Quelle part de la baisse de la fécondité tient à la politique
#             familiale ? Même démarche que pour le logement (23 §5).
# ----------------------------------------------------------------------------
#  Les prestations REÇUES ne peuvent pas entrer dans le modèle : la naissance
#  les déclenche (PAJE, prime, allocations), comme elle faisait baisser le
#  niveau de vie mesuré. On mesure donc l'INCITATION : ce qu'un enfant de plus
#  rapporterait au couple, avant la naissance, selon le barème.
#
#  1. Gain d'un enfant supplémentaire simulé par OpenFisca (local, script
#     38_gain_enfant_openfisca.py) : allocations familiales, complément
#     familial, allocation de base et prime de la PAJE, quotient familial.
#     Entrées : revenu hors prestations (× COEF_IMPOSABLE, approximation du
#     revenu imposable), nombre d'actifs occupés (répartition du revenu entre
#     les parents), âges des enfants déjà présents (fichiers individus).
#     Deux gains par couple : sous le barème de son année (`gain`) et sous un
#     barème FIXE, celui de 2014 en euros constants (`gain_ref`).
#  2. Modèle : celui de 23, plus `gain` ET `gain_ref`. Le gain dépend du
#     revenu, de l'activité et des enfants déjà nés, qui ont leur propre lien
#     avec la natalité : à barème fixe, cette dépendance passe par `gain_ref`.
#     L'effet de `gain` n'est alors identifié que par les RÉFORMES (PAJE 2014
#     et 2018, modulation des allocations en 2015, plafonds du quotient
#     familial), qui ont touché des familles différentes à des dates
#     différentes — démarche des « instruments simulés » (Currie et Gruber,
#     1996). Sans `gain_ref`, l'effet sort négatif : pur effet de structure.
#  3. Contrefactuel : on donne aux couples de 2025 le gain que leur aurait
#     procuré le barème de 2010 ou de 2014 (en euros constants), `gain_ref`
#     inchangé, et on re-prédit.
#
#  ⚠️ Couples supposés mariés (quotient familial commun) ; services (crèche),
#     congé parental et complément de mode de garde exclus : ils dépendent
#     d'un choix et ne sont pas observables dans SRCV.
#  Sorties : Output/gain_enfant_openfisca.csv (simulations, réutilisées d'un
#            passage à l'autre), Output/politique_familiale_*.csv (suffixées
#            selon SRCV_DEBUT)
# ============================================================================

source("R/00_prepa_fecondite.R")
library(survey); library(broom); library(marginaleffects)
activer_cache_prepa()
IPC <- charger_ipc()

PYTHON          <- "C:/Users/kgenn/of/Scripts/python.exe"
COEF_IMPOSABLE  <- 1.10     # revenu imposable / revenu disponible hors prestations
BAREME_REF      <- 2014     # barème fixe de `gain_ref`
BAREMES_CF      <- c(2010, 2014)

# ── §0 Données : champ de 23, plus l'âge des enfants ───────────────────────
df <- bind_rows(millesimes_prepares(restreindre = TRUE)) %>%
  mutate(
    annee_num       = as.integer(annee),
    naiss01         = as.integer(naissance == "Oui"),
    cout_m2_reel    = deflater(cout_m2,    annee_num,     IPC, base = 2025),
    niveau_vie_reel = deflater(niveau_vie, annee_num - 1, IPC, base = 2025),
    surface_10      = surface / 10,
    nv_10k          = niveau_vie_reel / 10000,
    rang            = if_else(nb_enf_avant == 0, "1er enfant", "Rang 2+"),
    cle             = paste(annee, normaliser_id(id), sep = "_"),
    across(where(is.factor), ~ factor(as.character(.))),
    statut3 = factor(statut3, levels = c("Locataire", "Proprietaire_accedant",
                                         "Proprietaire_non_accedant"))) %>%
  filter(!is.na(surface), !is.na(cout_m2_reel), is.finite(cout_m2_reel),
         !is.na(statut3), !is.na(age_femme), !is.na(niveau_vie_reel),
         !is.na(poids), poids > 0)

# Enfants de moins de 21 ans (âge limite des allocations) ; le nouveau-né,
# le plus jeune, est retiré : seuls comptent les enfants d'AVANT la naissance.
enfants <- imap_dfr(VAGUES[unique(df$annee)], function(cfg, an) {
  ind <- lire_srcv(cfg$ind)
  tibble(cle = paste(an, cle_menage_ind(ind), sep = "_"), age = ages_individus(ind, an),
         lien = trimws(as.character(ind$LIENPREF))) %>%
    filter(lien %in% c("02", "2"), !is.na(age), age >= 0, age <= 20)
}) %>%
  inner_join(df %>% select(cle, naiss01), by = "cle") %>%
  arrange(cle, age) %>% group_by(cle) %>%
  filter(row_number() > first(naiss01)) %>%
  summarise(ages = paste(age, collapse = ";"), benjamin = min(age), .groups = "drop")

df <- df %>% left_join(enfants, by = "cle") %>%
  mutate(ages = coalesce(ages, ""),
         benjamin_3 = factor(case_when(is.na(benjamin) ~ "Aucun", benjamin < 3 ~ "Moins de 3 ans",
                                       TRUE ~ "3 ans ou plus")),
         revenu_imp = pmax(revenu_hp, 0) * COEF_IMPOSABLE,
         part1      = if_else(coalesce(nactocc, 0) >= 2, 0.55, 1))
cat("Champ :", nrow(df), "couples |", sum(df$naiss01), "naissances | âges des enfants",
    "trouvés pour", round(100 * mean(df$ages != "" | df$nb_enf_avant == 0), 1), "%\n")

# ── §1 Simulation OpenFisca (seules les situations manquantes sont simulées)
#  Clés : `cle` = barème de l'année ; `cle_baremeAAAA` = barème AAAA, revenus
#  ramenés en euros de AAAA.
sous_bareme <- function(d, b) d %>%
  transmute(cle = paste0(cle, "_bareme", b), legis = b,
            revenu = round(deflater(revenu_imp, annee_num, IPC, base = b)), part1, ages)
entree <- bind_rows(
  df %>% transmute(cle, legis = annee_num, revenu = round(revenu_imp), part1, ages),
  sous_bareme(df, BAREME_REF),
  map_dfr(setdiff(BAREMES_CF, BAREME_REF), ~ sous_bareme(filter(df, annee == "2025"), .x)))

f_in <- "Output/gain_enfant_entree.csv"; f_out <- "Output/gain_enfant_openfisca.csv"
deja <- if (file.exists(f_out)) read_csv(f_out, show_col_types = FALSE) else NULL
manque <- if (is.null(deja)) entree else anti_join(entree, deja, by = c("cle", "legis"))
if (nrow(manque)) {
  cat("Simulation OpenFisca :", nrow(manque), "situations\n")
  write_csv(manque, f_in)
  f_tmp <- "Output/gain_enfant_openfisca_lot.csv"
  code <- system2(PYTHON, c("-X", "utf8", "R/38_gain_enfant_openfisca.py", f_in, f_tmp))
  if (code != 0) stop("Simulation OpenFisca en échec (code ", code, ")")
  deja <- bind_rows(deja, read_csv(f_tmp, show_col_types = FALSE))
  write_csv(deja, f_out); file.remove(f_tmp)
}
gains <- deja %>%
  semi_join(entree, by = c("cle", "legis")) %>%
  mutate(across(c(af, cf, paje_base, paje_naissance, ir, gain),
                ~ deflater(.x, legis, IPC, base = 2025), .names = "{.col}_reel"))

propre <- gains %>% filter(!grepl("_bareme", cle)) %>% select(cle, ends_with("_reel"))
reference <- gains %>% filter(grepl(paste0("_bareme", BAREME_REF, "$"), cle)) %>%
  transmute(cle = sub("_bareme[0-9]+$", "", cle), gain_ref_1k = gain_reel / 1000)
df <- df %>% left_join(propre, by = "cle") %>% left_join(reference, by = "cle") %>%
  mutate(gain_1k = gain_reel / 1000)
stopifnot(!anyNA(df$gain_1k), !anyNA(df$gain_ref_1k))

# Gain moyen par vague et par rang (euros de 2025)
desc <- df %>% group_by(annee, rang) %>%
  summarise(n = n(), gain_moyen = weighted.mean(gain_reel, poids),
            af = weighted.mean(af_reel, poids), cf = weighted.mean(cf_reel, poids),
            paje_base = weighted.mean(paje_base_reel, poids),
            prime = weighted.mean(paje_naissance_reel, poids),
            quotient_familial = weighted.mean(ir_reel, poids),
            gain_bareme_ref = 1000 * weighted.mean(gain_ref_1k, poids), .groups = "drop") %>%
  mutate(across(where(is.double), ~ round(.x)))
cat("\nGain d'un enfant supplémentaire, € de 2025 par an :\n")
print(as.data.frame(desc), row.names = FALSE)
ecrire(desc, "Output/politique_familiale_gain_par_vague.csv")

# ── §2 Modèle : celui de 23, plus le gain et le gain à barème fixe ─────────
des <- function(d) svydesign(ids = ~id_cluster, weights = ~poids, data = droplevels(d))
elaguer <- function(f, d) {
  vars <- setdiff(all.vars(f), all.vars(f[[2]]))
  mono <- vars[vapply(vars, function(v) v %in% names(d) &&
                        is.factor(d[[v]]) && nlevels(droplevels(d[[v]])) < 2, logical(1))]
  if (length(mono)) f <- update(f, paste(". ~ . -", paste(mono, collapse = " - ")))
  f
}
F_BASE  <- naiss01 ~ gain_1k + gain_ref_1k + surface_10 + cout_m2_reel + nv_10k + I(nv_10k^2) +
  statut3 + age_c + I(age_c^2) + demenage + csp + urbain + zeat + factor(annee)
F_POOLE <- update(F_BASE, . ~ . + nb_enfants + taille_avant + benjamin_3)

champs <- list("Ensemble"       = list(d = df,                               f = F_POOLE),
               "1er enfant"     = list(d = filter(df, rang == "1er enfant"), f = F_BASE),
               "Rang 2 et plus" = list(d = filter(df, rang == "Rang 2+"),    f = F_POOLE))
modeles <- map(champs, ~ svyglm(elaguer(.x$f, droplevels(.x$d)), design = des(.x$d),
                                family = quasibinomial(link = "probit")))
# Variante sans le gain à barème fixe : montre l'effet de structure qu'il retire
sans_ref <- map(champs, ~ svyglm(elaguer(update(.x$f, . ~ . - gain_ref_1k), droplevels(.x$d)),
                                 design = des(.x$d), family = quasibinomial(link = "probit")))
effet <- function(ms, lib) imap_dfr(ms, function(m, ch) {
  a <- as_tibble(avg_slopes(m, variables = "gain_1k", wts = m$prior.weights))
  tibble(specification = lib, echantillon = ch, ame_pts = 100 * a$estimate,
         se = 100 * a$std.error, conf.low = 100 * a$conf.low, conf.high = 100 * a$conf.high,
         p.value = a$p.value)
})
ame <- bind_rows(effet(modeles, "Avec gain a bareme fixe (retenu)"),
                 effet(sans_ref, "Sans gain a bareme fixe"))
cat("\nEffet d'un gain de 1 000 € de 2025 par an sur la probabilité de naissance (points) :\n")
print(as.data.frame(ame %>% mutate(across(where(is.double), ~ round(.x, 3)))), row.names = FALSE)
ecrire(ame, "Output/politique_familiale_modele.csv")

# ── §3 Contrefactuel : barèmes de 2010 et de 2014 appliqués aux couples de 2025
#  Couples de 2025 complets sur les variables du modèle (ceux qu'il a retenus).
d25 <- df %>% filter(annee == "2025") %>%
  filter(if_all(all_of(setdiff(all.vars(F_POOLE), c("naiss01", "annee"))), ~ !is.na(.x)))
predire <- function(m, d) weighted.mean(as.numeric(predict(m, newdata = d, type = "response")),
                                        d$poids)
decompo <- map_dfr(BAREMES_CF, function(b) {
  g <- gains %>% filter(cle %in% paste0(d25$cle, "_bareme", b)) %>%
    transmute(cle = sub("_bareme[0-9]+$", "", cle), gain_cf = gain_reel)
  d_cf <- d25 %>% left_join(g, by = "cle") %>% mutate(gain_1k = gain_cf / 1000)
  icf_b <- get(paste0("ICF_", b))
  baisse <- (ICF_2025 - icf_b) / icf_b
  imap_dfr(modeles, function(m, lib) {
    garder <- function(d) if (lib == "Ensemble") d else
      filter(d, rang == if (lib == "1er enfant") "1er enfant" else "Rang 2+")
    o <- garder(d25); c1 <- garder(d_cf)
    p0 <- predire(m, o)
    rel <- predire(m, c1) / p0 - 1
    # Intervalle : écart de gain × bornes de l'effet marginal (approximation
    # linéaire, l'écart de gain étant le même pour tous les tirages)
    a <- ame %>% filter(specification == "Avec gain a bareme fixe (retenu)", echantillon == lib)
    dg <- weighted.mean(c1$gain_1k - o$gain_1k, o$poids)
    tibble(bareme = b, echantillon = lib,
           gain_2025 = weighted.mean(o$gain_1k * 1000, o$poids),
           gain_bareme = weighted.mean(c1$gain_1k * 1000, c1$poids),
           ecart_relatif_pct = 100 * rel,
           baisse_icf_pct = 100 * baisse,
           part_de_la_baisse_pct = 100 * rel / -baisse,
           part_ic_bas  = a$conf.low  / 100 * dg / p0 / -baisse * 100,
           part_ic_haut = a$conf.high / 100 * dg / p0 / -baisse * 100)
  })
}) %>% mutate(across(where(is.double), ~ round(.x, 2)))
cat("\nContrefactuel : couples de 2025 sous les barèmes de 2010 et 2014 (€ constants) :\n")
print(as.data.frame(decompo), row.names = FALSE)
ecrire(decompo, "Output/politique_familiale_decomposition.csv")
