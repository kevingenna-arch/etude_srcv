# ============================================================================
#  SRCV — 36. Robustesse du modèle principal : degré d'urbanisation et âge
#             du conjoint
# ----------------------------------------------------------------------------
#  Le modèle de 23 (F_POOLE, F_RANG1, F_RANG2) est réestimé sous quatre
#  variantes, et l'effet marginal moyen de la surface comparé :
#   - Référence    : spécification de 23.
#   - Sans DB100   : `urbain` retiré. La nomenclature DEGURBA est révisée vers
#                    2011-2012 ; avec des effets fixes d'année, un contrôle dont
#                    les niveaux glissent n'est pas forcément neutre.
#   - DB100 x période : `urbain` croisé avec un indicateur 2014 et après, qui
#                    laisse la variable jouer différemment de part et d'autre
#                    de la révision (sans objet avec SRCV_DEBUT=2014).
#   - Âge du conjoint : âge de l'homme du couple (centré, + carré) ajouté à
#                    l'âge de la femme ; même champ que la référence, restreint
#                    aux couples où cet âge est connu.
#   - ZEAT 6 -> Sud-Ouest : le code « 6 » de 2014, absent de la nomenclature,
#                    est le Sud-Ouest (cf. 00_utils.R) ; on le fusionne.
#  Sortie : Output/robustesse_urbain_age_conjoint.csv (suffixée selon SRCV_DEBUT)
# ============================================================================

source("R/00_prepa_fecondite.R")
library(survey); library(broom); library(marginaleffects)
activer_cache_prepa()
IPC <- charger_ipc()

# ── Données : même préparation que 23 §0, plus l'âge du conjoint ────────────
df <- bind_rows(millesimes_prepares(restreindre = TRUE)) %>%
  mutate(
    annee_num       = as.integer(annee),
    naiss01         = as.integer(naissance == "Oui"),
    cout_m2_reel    = deflater(cout_m2,    annee_num,     IPC, base = 2025),
    niveau_vie_reel = deflater(niveau_vie, annee_num - 1, IPC, base = 2025),
    surface_10      = surface / 10,
    nv_10k          = niveau_vie_reel / 10000,
    rang            = if_else(nb_enf_avant == 0, "1er enfant", "Rang 2+"),
    age_homme       = case_when(trimws(as.character(sexe_pr)) == "1" ~ age_pr,
                                trimws(as.character(sexe_cj)) == "1" ~ age_cj,
                                TRUE ~ NA_real_),
    age_h_c         = (age_homme - 32) / 10,
    apres_2014      = factor(if_else(annee_num >= 2014, "Apres", "Avant")),
    zeat_corr       = factor(if_else(as.character(zeat) == "Code 6 (2014)", "Sud-Ouest",
                                     as.character(zeat))),
    across(where(is.factor), ~ factor(as.character(.))),
    statut3 = factor(statut3, levels = c("Locataire", "Proprietaire_accedant",
                                         "Proprietaire_non_accedant"))) %>%
  filter(!is.na(surface), !is.na(cout_m2_reel), is.finite(cout_m2_reel),
         !is.na(statut3), !is.na(age_femme), !is.na(niveau_vie_reel),
         !is.na(poids), poids > 0)

cat("Champ :", nrow(df), "observations |", sum(df$naiss01), "naissances |",
    "age du conjoint connu :", sum(!is.na(df$age_homme)), "\n")

des <- function(d) svydesign(ids = ~id_cluster, weights = ~poids, data = droplevels(d))
elaguer <- function(f, d) {
  vars <- setdiff(all.vars(f), all.vars(f[[2]]))
  mono <- vars[vapply(vars, function(v) v %in% names(d) &&
                        is.factor(d[[v]]) && nlevels(droplevels(d[[v]])) < 2, logical(1))]
  if (length(mono)) f <- update(f, paste(". ~ . -", paste(mono, collapse = " - ")))
  f
}

F_BASE  <- naiss01 ~ surface_10 + cout_m2_reel + nv_10k + statut3 +
  age_c + I(age_c^2) + demenage + csp + urbain + zeat + factor(annee)
F_POOLE <- update(F_BASE, . ~ . + nb_enfants + taille_avant)

variantes <- list(
  "Reference"         = function(f) f,
  "Sans DB100"        = function(f) update(f, . ~ . - urbain),
  "DB100 x periode"   = function(f) update(f, . ~ . + urbain:apres_2014),
  "Age du conjoint"   = function(f) update(f, . ~ . + age_h_c + I(age_h_c^2)),
  "ZEAT 6 -> Sud-Ouest" = function(f) update(f, . ~ . - zeat + zeat_corr))

champs <- list(
  "Ensemble"       = list(d = df,                               f = F_POOLE),
  "1er enfant"     = list(d = filter(df, rang == "1er enfant"), f = F_BASE),
  "Rang 2 et plus" = list(d = filter(df, rang == "Rang 2+"),    f = F_POOLE))

res <- list()
for (ch in names(champs)) for (v in names(variantes)) {
  if (v == "DB100 x periode" && ANNEE_DEBUT >= 2014) next
  d <- champs[[ch]]$d
  # La variante âge du conjoint et sa référence portent sur le même champ
  if (v == "Age du conjoint") d <- filter(d, !is.na(age_h_c))
  d <- droplevels(d)
  m <- svyglm(elaguer(variantes[[v]](champs[[ch]]$f), d), design = des(d),
              family = quasibinomial(link = "probit"))
  a <- as_tibble(avg_slopes(m, variables = "surface_10", wts = m$prior.weights))
  res[[length(res) + 1]] <- tibble(
    echantillon = ch, variante = v, n = nrow(d),
    ame_pts = 100 * a$estimate, se = 100 * a$std.error,
    conf.low = 100 * a$conf.low, conf.high = 100 * a$conf.high, p.value = a$p.value)
}

sortie <- bind_rows(res) %>% mutate(across(where(is.double), ~ round(.x, 4)))
print(as.data.frame(sortie), row.names = FALSE)
ecrire(sortie, "Output/robustesse_urbain_age_conjoint.csv")
cat("\nExporte : Output/robustesse_urbain_age_conjoint", SUFFIXE_PERIODE, ".csv\n", sep = "")
