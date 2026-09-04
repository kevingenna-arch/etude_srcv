# ============================================================================
#  SRCV — Conditions de logement par CLASSE D'ÂGE de la personne de référence
# ----------------------------------------------------------------------------
#  Pour chaque classe d'âge (20-24, 25-29, 30-34, 35-39) et chaque millésime :
#    - surface moyenne (m²)
#    - coût du logement moyen et coût au m², DÉFLATÉS
#    - taux d'effort (coût annuel / revenu disponible)
#  Ventilé entre LOCATAIRES et PROPRIÉTAIRES ACCÉDANTS.
#
#  DEUX DÉFLATEURS, au choix via DEFLATEUR :
#   - "ipc"    : indice des prix à la consommation (INSEE, base 2025).
#                Lecture : coût en euros de pouvoir d'achat constant.
#   - "revenu" : indice du NIVEAU DE VIE MÉDIAN (revenu par UC) de chaque vague.
#                Lecture : coût rapporté à l'évolution des revenus — un coût
#                stable signifie qu'il progresse au même rythme que les revenus.
#                C'est une mesure de "prix relatif au niveau de vie".
#
#  Coût = charges (HH070) + mensualité d'emprunt (REMP/REMPR).
#  Champ : tous les ménages (aucune restriction de couple ni de fécondité).
# ============================================================================

source("R/00_utils.R")
source("R/00_config.R")
library(survey)

# "ipc" ou "revenu". Surchargeable sans éditer le script, pour produire les deux
# sorties d'affilée :  SRCV_DEFLATEUR=ipc Rscript R/09_logement_par_age.R
DEFLATEUR <- Sys.getenv("SRCV_DEFLATEUR", "revenu")
stopifnot(DEFLATEUR %in% c("ipc", "revenu"))
IPC <- charger_ipc()

# ── Chargement d'une vague ──────────────────────────────────────────────────
charger <- function(annee, cfg) {
  v <- cfg$vars
  d <- lire_srcv(cfg$men)
  tibble(annee   = annee,
         age_pr  = num(d[[v[["age_pr"]]]]),
         surface = num(d[[v[["surface"]]]]),
         charges = num(d[[v[["cout_log"]]]]),
         remb    = coalesce(num(d[[v[["rembours"]]]]), 0),
         revenu  = num(d[[v[["revenu"]]]]),
         uc      = num(d$HX050),
         occ     = d[[v[["occ"]]]],
         poids   = num(d[[v[["poids"]]]])) %>%
    mutate(cout    = charges + remb,
           statut3 = recode_statut3(recode_occ(occ, cfg$occ_type), remb),
           surface = if_else(surface >= 9 & surface <= 400, surface, NA_real_),
           niveau_vie = if_else(!is.na(uc) & uc > 0, revenu / uc, revenu),
           classe  = cut(age_pr, breaks = c(19, 24, 29, 34, 39),
                         labels = c("20-24 ans","25-29 ans","30-34 ans","35-39 ans"))) %>%
    filter(!is.na(poids), poids > 0, !is.na(classe), !is.na(statut3),
           !is.na(cout), cout >= 0)
}
d <- imap_dfr(VAGUES, ~ charger(.y, .x))

# ── Indice de NIVEAU DE VIE MÉDIAN par vague (déflateur "revenu") ───────────
# Calculé sur TOUS les ménages de la vague (pas seulement les 20-39 ans), pour
# refléter l'évolution générale des revenus.
idx_revenu <- imap_dfr(VAGUES, function(cfg, y) {
  m <- lire_srcv(cfg$men)
  x <- tibble(nv = num(m[[cfg$vars[["revenu"]]]]) / pmax(num(m$HX050), 1),
              w  = num(m[[cfg$vars[["poids"]]]])) %>%
    filter(!is.na(nv), nv > 0, !is.na(w), w > 0)
  o <- order(x$nv); cw <- cumsum(x$w[o]) / sum(x$w)
  tibble(annee = y, nv_median = x$nv[o][which(cw >= 0.5)[1]])
}) %>% mutate(indice = 100 * nv_median / nv_median[annee == "2025"])

cat("=== Déflateurs par millésime (base 100 = 2025) ===\n")
print(as.data.frame(idx_revenu %>%
  left_join(IPC %>% transmute(annee = as.character(annee),
                              ipc = round(100*ipc/ipc[annee == 2025], 1)), by = "annee") %>%
  transmute(annee, nv_median = round(nv_median), indice_revenu = round(indice, 1),
            indice_ipc = ipc)), row.names = FALSE)

# ── Application du déflateur choisi ─────────────────────────────────────────
d <- d %>% left_join(idx_revenu %>% select(annee, indice), by = "annee") %>%
  mutate(cout_defl = if (DEFLATEUR == "revenu") cout * 100 / indice
                     else deflater(cout, as.integer(annee), IPC, base = 2025),
         cout_m2   = cout_defl / surface,
         # Taux d'effort : revenus <= 0 exclus et taux borné à [0 ; 100] %,
         # comme dans 02 — sinon quelques revenus nuls ou négatifs produisent
         # des valeurs aberrantes qui détruisent la moyenne.
         effort    = if_else(!is.na(revenu) & revenu > 0,
                             pmin(pmax(100 * (cout * 12) / revenu, 0), 100),
                             NA_real_))

cat("\nDéflateur retenu :", DEFLATEUR,
    "| n =", nrow(d), "ménages (PR de 20 à 39 ans)\n")

des <- svydesign(ids = ~1, weights = ~poids, data = d)
for (cl in levels(d$classe)) {
  cat("\n\n##########", cl, "##########\n")
  for (st in c("Locataire", "Proprietaire_accedant")) {
    n <- sum(d$classe == cl & d$statut3 == st, na.rm = TRUE)
    cat("\n--", st, "— n =", n, "--\n")
    if (n < 30) { cat("   effectif insuffisant\n"); next }
    print(as.data.frame(
      svyby(~surface + cout_defl + cout_m2 + effort, ~annee,
            subset(des, classe == cl & statut3 == st), svymean, na.rm = TRUE) %>%
      as_tibble() %>%
      transmute(annee, surface_m2 = round(surface, 1),
                cout_defl_eur = round(cout_defl), cout_m2_eur = round(cout_m2, 2),
                effort_pct = round(effort, 1))), row.names = FALSE)
  }
}

if (!dir.exists("Output")) dir.create("Output")
# Effectifs NON PONDÉRÉS exportés avec les moyennes : sans eux, l'export laisse
# croire à des estimations solides là où la boucle ci-dessus refuse d'afficher
# (n < 30). Cas typique : 20-24 ans accédants, n compris entre 9 et 23 selon la
# vague — moyennes ininterprétables.
effectifs <- count(d, annee, classe, statut3, name = "n")
write_csv(svyby(~surface + cout_defl + cout_m2 + effort, ~annee + classe + statut3,
                des, svymean, na.rm = TRUE) %>% as_tibble() %>%
            left_join(effectifs, by = c("annee", "classe", "statut3")),
          paste0("Output/logement_par_classe_age_", DEFLATEUR, ".csv"))
cat("\n\nExporté : Output/logement_par_classe_age_", DEFLATEUR, ".csv\n", sep = "")
