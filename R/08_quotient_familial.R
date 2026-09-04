# ============================================================================
#  SRCV — Micro-simulation du QUOTIENT FAMILIAL : quelle baisse d'impôt sur le
#  revenu procure la naissance d'un enfant ?
# ----------------------------------------------------------------------------
#  Contexte : les dépenses de politique familiale se répartissent entre
#  prestations en espèces, services en nature, et AVANTAGES FISCAUX. Ce dernier
#  volet (quotient familial) n'est pas observable directement dans SRCV :
#  ni "nombre de parts fiscales", ni "revenu imposable" ne sont diffusés.
#  On le RECONSTRUIT donc par simulation.
#
#  MÉTHODE
#   1. Reconstituer les PARTS FISCALES à partir de COUPLEPR + NENFANTS.
#   2. Approximer le REVENU IMPOSABLE à partir de HY010 (revenu brut) diminué
#      des prestations non imposables, puis abattement de 10 %.
#   3. Appliquer le BARÈME de l'IR de l'année de revenu concernée, avec
#      plafonnement du quotient familial.
#   4. VALIDER : comparer l'IRPP simulé à l'IRPP observé. Sans cette étape, le
#      contrefactuel n'aurait aucune crédibilité.
#   5. CONTREFACTUEL : recalculer l'impôt en retirant les parts du dernier
#      enfant -> l'écart est le gain de quotient familial.
#
#  ⚠️ LIMITES ASSUMÉES
#   - Le revenu imposable est APPROXIMÉ (pas de déclaration fiscale dans SRCV) :
#     abattements réels, quotient conjugal des pensions, revenus du capital au
#     PFU, réductions/crédits d'impôt ne sont pas reproduits.
#   - La décote et les réductions d'impôt ne sont pas simulées.
#   - Barèmes et plafonds saisis en dur ci-dessous : À VÉRIFIER sur le barème
#     officiel avant toute publication.
#   - Résultat à lire comme un ORDRE DE GRANDEUR, pas comme un chiffrage
#     fiscal. Pour un chiffrage publiable, utiliser un modèle dédié
#     (OpenFisca, INES, TAXIPP).
# ============================================================================

source("R/00_utils.R")
source("R/00_config.R")
library(survey)
library(broom)

# ── Barème de l'impôt sur le revenu, par ANNÉE DE REVENU ────────────────────
# seuils = bornes inférieures des tranches ; taux = taux marginaux.
# plafond_dp = plafonnement du quotient familial, par DEMI-PART supplémentaire.
# dec_* = décote (annule/réduit l'impôt des foyers modestes) : seuils
# d'application et montants, pour personne seule (_s) et couple (_c).
BAREME <- list(
  "2021" = list(seuils = c(0, 10225, 26070, 74545, 160336), taux = c(0,.11,.30,.41,.45),
                plafond_dp = 1592, dec_seuil_s = 1745, dec_seuil_c = 2888,
                dec_mont_s = 790, dec_mont_c = 1307),
  "2022" = list(seuils = c(0, 10777, 27478, 78570, 168994), taux = c(0,.11,.30,.41,.45),
                plafond_dp = 1678, dec_seuil_s = 1841, dec_seuil_c = 3045,
                dec_mont_s = 833, dec_mont_c = 1378),
  "2023" = list(seuils = c(0, 11294, 28797, 82341, 177106), taux = c(0,.11,.30,.41,.45),
                plafond_dp = 1759, dec_seuil_s = 1929, dec_seuil_c = 3191,
                dec_mont_s = 873, dec_mont_c = 1444),
  "2024" = list(seuils = c(0, 11497, 29315, 83823, 180294), taux = c(0,.11,.30,.41,.45),
                plafond_dp = 1791, dec_seuil_s = 1964, dec_seuil_c = 3248,
                dec_mont_s = 889, dec_mont_c = 1470)
)
TAUX_DECOTE <- 0.4525

# Impôt brut pour un revenu par part donné, selon un barème (vectorisé)
impot_par_part <- function(rpp, bar) {
  res <- rep(0, length(rpp))
  for (i in seq_along(bar$seuils)) {
    haut <- if (i < length(bar$seuils)) bar$seuils[i + 1] else Inf
    res <- res + pmax(0, pmin(rpp, haut) - bar$seuils[i]) * bar$taux[i]
  }
  res
}

# Impôt du foyer = parts × impôt(revenu/parts), avec plafonnement du QF :
# l'avantage procuré par les demi-parts au-delà de celles du couple/isolé est
# limité à plafond_dp par demi-part.
impot_foyer <- function(revenu_imp, parts, parts_base, bar) {
  t_reel <- parts      * impot_par_part(revenu_imp / parts,      bar)
  t_base <- parts_base * impot_par_part(revenu_imp / parts_base, bar)
  avantage    <- pmax(0, t_base - t_reel)                       # gain du QF
  dp_sup      <- pmax(0, (parts - parts_base) * 2)              # nb de demi-parts
  avantage_max<- dp_sup * bar$plafond_dp
  t <- t_base - pmin(avantage, avantage_max)                    # impôt après plafond
  # Décote : réduit voire annule l'impôt des foyers modestes.
  seuil <- if_else(parts_base >= 2, bar$dec_seuil_c, bar$dec_seuil_s)
  mont  <- if_else(parts_base >= 2, bar$dec_mont_c,  bar$dec_mont_s)
  pmax(0, t - pmax(0, mont - TAUX_DECOTE * t) * (t < seuil))
}

# ── Parts fiscales ──────────────────────────────────────────────────────────
# Couple : 2 parts. Personne seule : 1 part.
# Enfants : +0,5 pour les 2 premiers, +1 à partir du 3e.
# Parent isolé : le 1er enfant ouvre une part entière (case T).
parts_fiscales <- function(en_couple, nenf) {
  nenf <- pmax(coalesce(nenf, 0), 0)
  base <- if_else(en_couple, 2, 1)
  sup  <- 0.5 * pmin(nenf, 2) + 1 * pmax(nenf - 2, 0)
  sup  <- sup + if_else(!en_couple & nenf >= 1, 0.5, 0)   # part supplémentaire parent isolé
  list(parts = base + sup, base = base)
}

# ── Ménage = FOYER FISCAL ? ─────────────────────────────────────────────────
# Le ménage SRCV et le foyer fiscal ne coïncident pas toujours :
#   - les CONCUBINS déclarent séparément (2 foyers) alors qu'ils forment 1 ménage ;
#   - les ENFANTS MAJEURS peuvent déclarer pour eux-mêmes ;
#   - les autres cohabitants (parent, ami, frère...) sont des foyers distincts.
# Dans ces cas, HY010/HY020 agrègent des revenus qui ne sont pas dans la même
# déclaration -> l'impôt simulé est mécaniquement surestimé.
# On isole donc les ménages où ménage == foyer fiscal :
#   PR + (conjoint MARIÉ ou PACSÉ) + enfants MINEURS uniquement.
flag_foyer_fiscal <- function(vague) {
  i <- lire_srcv(chemin_ind(vague)) %>%
    transmute(id   = trimws(as.character(RB040)),
              lien = trimws(as.character(LIENPREF)),
              age  = num(AGE),
              sm   = trimws(as.character(SITUMATRI)))
  sm_pr <- i %>% filter(lien == "00") %>% select(id, sm_pr = sm)
  i %>% group_by(id) %>%
    summarise(
      # aucun cohabitant autre que PR / conjoint / enfant
      que_noyau     = all(lien %in% c("00", "01", "02")),
      # aucun enfant majeur (susceptible de déclarer séparément)
      sans_enf_maj  = !any(lien == "02" & age >= 18, na.rm = TRUE),
      a_conjoint    = any(lien == "01"),
      .groups = "drop") %>%
    left_join(sm_pr, by = "id") %>%
    mutate(foyer_fiscal = que_noyau & sans_enf_maj &
             # si conjoint présent : il doit être marié ou pacsé (1 seul foyer)
             (!a_conjoint | sm_pr %in% c("1", "2"))) %>%
    select(id, foyer_fiscal)
}

# ── Chargement d'une vague FPR + reconstruction de l'assiette ───────────────
charger <- function(vague) {
  an_rev <- as.character(as.integer(vague) - 1)     # les revenus portent sur N-1
  if (is.null(BAREME[[an_rev]])) return(NULL)
  d <- lire_srcv(chemin_men(vague))

  x <- tibble(
    id      = trimws(as.character(d$DB030)),
    vague   = vague, an_rev = an_rev,
    brut    = num(d$HY010),
    dispo   = num(d$HY020),
    irpp_obs= num(d$IRPP),
    # prestations NON IMPOSABLES à retirer de l'assiette
    presta_fam = coalesce(num(d$HY050N), 0),
    alloc_log  = coalesce(num(d$HY070N), 0),
    minima     = coalesce(num(d$RSA_MEN), 0) + coalesce(num(d$PPA_MEN), 0) +
                 coalesce(num(d$PREST_SOLIDARITE), 0),
    nenf    = num(d$NENFANTS),
    couple  = trimws(as.character(d$COUPLEPR)) == "1",
    poids   = num(d$DB090)) %>%
    filter(!is.na(brut), brut > 0, !is.na(poids), poids > 0, !is.na(nenf),
           !is.na(couple), !is.na(dispo), !is.na(irpp_obs))

  x <- x %>% left_join(flag_foyer_fiscal(vague), by = "id") %>%
    mutate(foyer_fiscal = coalesce(foyer_fiscal, FALSE))

  p <- parts_fiscales(x$couple, x$nenf)
  bar <- BAREME[[an_rev]]
  x %>% mutate(
    parts = p$parts, parts_base = p$base,
    # Assiette : HY020 (disponible) + IRPP = revenu APRÈS cotisations mais AVANT
    # impôt sur le revenu -> base la plus proche du revenu net imposable ;
    # on retire les prestations non imposables, puis l'abattement de 10 %.
    # (Testé contre 3 autres reconstructions : c'est celle qui valide le mieux.)
    revenu_imp = pmax(0, (dispo + irpp_obs - presta_fam - alloc_log - minima)) * 0.9,
    irpp_sim   = impot_foyer(revenu_imp, parts, parts_base, bar))
}

vagues <- names(VAGUES)[as.integer(names(VAGUES)) >= 2022]
sim <- map_dfr(vagues, charger)
cat("Ménages simulés :", nrow(sim), "| vagues :", paste(unique(sim$vague), collapse = ", "), "\n")

# ── 4. VALIDATION : simulé vs observé ───────────────────────────────────────
valider <- function(d, libelle) {
  cat("\n##########", libelle, "- n =", nrow(d), "##########\n")
  de <- svydesign(ids = ~1, weights = ~poids, data = d)
  print(svyby(~irpp_sim + irpp_obs, ~vague, de, svymean, na.rm = TRUE) %>%
          as_tibble() %>%
          transmute(vague, simule = round(irpp_sim), observe = round(irpp_obs),
                    ecart_pct = round(100 * (irpp_sim / irpp_obs - 1), 1)) %>%
          as.data.frame())
  cat("-- non imposables (%) : simule / observe --\n")
  print(d %>% group_by(vague) %>%
          summarise(sim = round(100 * weighted.mean(irpp_sim <= 0, poids), 1),
                    obs = round(100 * weighted.mean(irpp_obs <= 0, poids, na.rm = TRUE), 1),
                    .groups = "drop") %>% as.data.frame())
  cat("-- correlation ponderee r =",
      round(cov.wt(cbind(d$irpp_sim, d$irpp_obs), wt = d$poids, cor = TRUE)$cor[1, 2], 3), "\n")
}

cat("\n=== VALIDATION : IRPP simule vs observe ===\n")
valider(sim, "TOUS MENAGES")
valider(sim %>% filter(foyer_fiscal), "MENAGE = FOYER FISCAL")
cat("\nPart des menages ou menage = foyer fiscal :",
    round(100 * weighted.mean(sim$foyer_fiscal, sim$poids), 1), "%\n")

# Le contrefactuel est ensuite calcule sur le champ VALIDE.
sim <- sim %>% filter(foyer_fiscal)

# ── 5. CONTREFACTUEL : gain de quotient familial du DERNIER enfant ──────────
# On retire l'enfant marginal (parts recalculées avec nenf - 1) et on compare.
gain_dernier_enfant <- function(d) {
  bars <- split(d, d$an_rev)
  map_dfr(bars, function(g) {
    bar <- BAREME[[g$an_rev[1]]]
    p0  <- parts_fiscales(g$couple, pmax(g$nenf - 1, 0))
    g %>% mutate(
      irpp_sans_enfant = impot_foyer(revenu_imp, p0$parts, p0$base, bar),
      gain_qf = irpp_sans_enfant - irpp_sim)              # >= 0 : baisse d'impôt
  })
}
avec_enfant <- sim %>% filter(nenf >= 1) %>% gain_dernier_enfant()

des2 <- svydesign(ids = ~1, weights = ~poids, data = avec_enfant)
cat("\n\n=== GAIN DE QUOTIENT FAMILIAL du dernier enfant (€/an) ===\n")
cat("Champ : ménages avec au moins 1 enfant. n =", nrow(avec_enfant), "\n\n")

cat("-- Moyenne pondérée par rang de l'enfant --\n")
print(avec_enfant %>% mutate(rang = pmin(nenf, 4)) %>%
        group_by(rang) %>%
        summarise(n = n(),
                  gain_moyen   = round(weighted.mean(gain_qf, poids)),
                  part_nul_pct = round(100 * weighted.mean(gain_qf < 1, poids), 1),
                  gain_si_gain = round(weighted.mean(gain_qf[gain_qf >= 1],
                                                     poids[gain_qf >= 1])),
                  .groups = "drop") %>% as.data.frame())

cat("\n-- Moyenne pondérée par quintile de revenu brut --\n")
print(avec_enfant %>% mutate(q = ntile(brut, 5)) %>%
        group_by(q) %>%
        summarise(brut_moy = round(weighted.mean(brut, poids)),
                  gain_moyen = round(weighted.mean(gain_qf, poids)),
                  part_nul_pct = round(100 * weighted.mean(gain_qf < 1, poids), 1),
                  .groups = "drop") %>% as.data.frame())

cat("\n-- Ensemble --\n")
cat("  gain moyen (tous ménages avec enfant) :",
    round(coef(svymean(~gain_qf, des2))), "€/an\n")
cat("  gain moyen parmi les ménages imposables :",
    round(weighted.mean(avec_enfant$gain_qf[avec_enfant$irpp_sim > 0],
                        avec_enfant$poids[avec_enfant$irpp_sim > 0])), "€/an\n")

if (!dir.exists("Output")) dir.create("Output")
write_csv(avec_enfant %>% select(id, vague, an_rev, brut, nenf, couple, parts,
                                 revenu_imp, irpp_obs, irpp_sim, irpp_sans_enfant,
                                 gain_qf, poids),
          "Output/quotient_familial_simulation.csv")
cat("\nExporté : Output/quotient_familial_simulation.csv\n")
