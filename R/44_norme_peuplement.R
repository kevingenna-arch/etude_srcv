# ============================================================================
#  SRCV — 44. La contrainte mesurée en pièces : y a-t-il la place pour un
#             enfant de plus ?
# ----------------------------------------------------------------------------
#  Le seuil de 45-60 m² évoque une indivisibilité (« une chambre de plus »).
#  On la mesure directement avec la NORME DE PEUPLEMENT de l'Insee : une pièce
#  de séjour ; une pièce par couple ; une pièce par personne de 19 ans ou plus
#  hors du couple ; pour les moins de 19 ans, une pièce pour deux enfants s'ils
#  ont moins de 7 ans ou sont de même sexe, sinon une pièce par enfant.
#
#  Pour chaque couple, avant la naissance (le nouveau-né est retiré) :
#   - besoin = nombre de pièces requis par la norme ;
#   - supplément = pièce en plus qu'exigerait un enfant de plus (0 s'il peut
#     partager avec un enfant de moins de 7 ans seul dans sa chambre ; 1/2 s'il
#     ne peut partager qu'avec un aîné seul, selon le sexe ; 1 sinon) ;
#   - « place pour un enfant de plus » si nombre de pièces ≥ besoin +
#     supplément ; « il manque déjà une pièce » si pièces < besoin.
#  Pour un couple sans enfant : place si le logement a au moins 3 pièces.
#
#  Tests : taux de naissance par situation ; modèle de la coupe (script 23)
#  avec la situation à la place de la surface, puis les deux ensemble ; par
#  rang ; et test prospectif sur les couples suivis du script 43.
#  À lancer avec SRCV_DEBUT=2014 après le script 43.
#  Sorties : Output/norme_peuplement.csv
# ============================================================================

source("R/00_prepa_fecondite.R")
source("R/00_norme_peuplement.R")
library(survey); library(broom); library(marginaleffects)
activer_cache_prepa()
IPC <- charger_ipc()

# ── §1 Couples du champ ─────────────────────────────────────────────────────
df <- bind_rows(millesimes_prepares(restreindre = TRUE)) %>%
  select(any_of(c(VARS_INTERET, "datent", "naissance"))) %>%
  mutate(annee_num       = as.integer(annee),
         id_n            = normaliser_id(id),
         naiss01         = as.integer(naissance == "Oui"),
         cout_m2_reel    = deflater(cout_m2,    annee_num,     IPC, base = 2025),
         niveau_vie_reel = deflater(niveau_vie, annee_num - 1, IPC, base = 2025),
         surface_10 = surface / 10, nv_10k = niveau_vie_reel / 10000,
         sous60 = factor(if_else(surface < 60, "Moins de 60 m2", "60 m2 ou plus"),
                         levels = c("60 m2 ou plus", "Moins de 60 m2")),
         rang = if_else(nb_enf_avant == 0, "1er enfant", "Rang 2+"),
         across(where(is.factor), ~ factor(as.character(.))),
         statut3 = factor(statut3, levels = c("Locataire", "Proprietaire_accedant",
                                              "Proprietaire_non_accedant")),
         sous60 = factor(sous60, levels = c("60 m2 ou plus", "Moins de 60 m2"))) %>%
  filter(!is.na(surface), !is.na(cout_m2_reel), is.finite(cout_m2_reel),
         !is.na(statut3), !is.na(age_femme), !is.na(niveau_vie_reel),
         !is.na(poids), poids > 0, !is.na(pieces), pieces >= 1)

# ── §2 Composition des ménages (fichiers individus) ────────────────────────
#  Membres hors couple de référence : enfants du couple (LIENPREF 02) et
#  autres personnes. Le nouveau-né (le plus jeune enfant, si naissance) est
#  retiré. Moins de 19 ans : comptés comme enfants ; 19 ans ou plus : une pièce.
membres <- membres_hors_couple(unique(df$annee))

compo <- membres %>%
  inner_join(df %>% select(annee, id_n, naiss01), by = c("annee", "id_n")) %>%
  arrange(annee, id_n, age) %>% group_by(annee, id_n) %>%
  # le nouveau-né : le plus jeune membre de moins de 2 ans, s'il y a naissance
  mutate(nouveau_ne = row_number() == 1 & naiss01 == 1 & age <= 1) %>%
  ungroup() %>% filter(!nouveau_ne) %>% compter_membres()

df <- df %>% left_join(compo, by = c("annee", "id_n")) %>% ajouter_place()

# ── §3 Descriptif ──────────────────────────────────────────────────────────
VAGUES_TAUX <- setdiff(unique(df$annee), c("2022", "2023"))     # anomalie de pondération
desc <- df %>% filter(annee %in% VAGUES_TAUX) %>% group_by(rang, place) %>%
  summarise(n = n(), naissances = sum(naiss01),
            part_pct = sum(poids), taux_pct = 100 * weighted.mean(naiss01, poids),
            surface_mediane = wquantile(surface, poids, .5), pieces_moy = weighted.mean(pieces, poids),
            .groups = "drop") %>%
  group_by(rang) %>% mutate(part_pct = 100 * part_pct / sum(part_pct)) %>% ungroup()
cat("\nSituation au regard de la norme, par rang :\n")
print(as.data.frame(desc %>% mutate(across(where(is.double), ~ round(.x, 1)))), row.names = FALSE)
par_annee <- df %>% group_by(annee, rang, place) %>% summarise(n = n(), poids = sum(poids), .groups = "drop") %>%
  group_by(annee, rang) %>% mutate(part_pct = 100 * poids / sum(poids)) %>% ungroup() %>% select(-poids)
cat("\nPart des couples par situation, selon l'année :\n")
print(as.data.frame(par_annee %>% select(-n) %>% mutate(part_pct = round(part_pct, 1)) %>%
  tidyr::pivot_wider(names_from = annee, values_from = part_pct)), row.names = FALSE)
cat("\nCouples sans enfant : surface selon la place disponible (T3 = 3 pièces) :\n")
print(as.data.frame(df %>% filter(rang == "1er enfant") %>% count(pieces = pmin(pieces, 5), sous60) %>%
  tidyr::pivot_wider(names_from = sous60, values_from = n)), row.names = FALSE)

# ── §4 Modèles ─────────────────────────────────────────────────────────────
des <- function(d) svydesign(ids = ~id_cluster, weights = ~poids, data = droplevels(d))
elaguer <- function(f, d) {
  vars <- setdiff(all.vars(f), all.vars(f[[2]]))
  mono <- vars[vapply(vars, function(v) v %in% names(d) &&
                        is.factor(d[[v]]) && nlevels(droplevels(d[[v]])) < 2, logical(1))]
  if (length(mono)) f <- update(f, paste(". ~ . -", paste(mono, collapse = " - ")))
  f
}
CTRL <- ~ cout_m2_reel + nv_10k + statut3 + age_c + I(age_c^2) + demenage + csp + urbain +
  zeat + factor(annee)
PAR <- ~ . + nb_enfants + taille_avant
SPECS <- list("Place seule"            = update(CTRL, naiss01 ~ place + .),
              "Place et surface (+10 m2)" = update(CTRL, naiss01 ~ place + surface_10 + .),
              "Place et seuil de 60 m2"   = update(CTRL, naiss01 ~ place + sous60 + .))
CHAMPS <- list("1er enfant" = filter(df, rang == "1er enfant"),
               "Rang 2 et plus" = filter(df, rang == "Rang 2+"))
res <- imap_dfr(CHAMPS, function(d, ch) imap_dfr(SPECS, function(f, sp) {
  if (ch != "1er enfant") f <- update(f, PAR)
  d <- droplevels(d)
  m <- svyglm(elaguer(f, d), design = des(d), family = quasibinomial(link = "probit"))
  vars <- intersect(c("place", "surface_10", "sous60"), all.vars(f))
  as_tibble(avg_slopes(m, variables = vars, wts = m$prior.weights)) %>%
    transmute(echantillon = ch, specification = sp, variable = term, contraste = contrast,
              effet_pts = 100 * estimate, ic_bas = 100 * conf.low, ic_haut = 100 * conf.high,
              p = p.value, taux_pct = 100 * weighted.mean(d$naiss01, d$poids),
              n = nrow(m$model), naissances = sum(m$y), deviance = deviance(m))
}))
cat("\nEffets marginaux (points) :\n")
print(as.data.frame(res %>% mutate(across(where(is.double), ~ round(.x, 3)))), row.names = FALSE)

# ── §5 Test prospectif : la place manquante prédit-elle les naissances
#     suivantes ? (couples suivis du script 43) ─────────────────────────────
suivis <- read_csv("Output/panel_horizon_long_couples.csv", show_col_types = FALSE,
                   col_types = cols(id_n = col_character(), annee = col_character(),
                                    .default = col_guess()))
pan <- df %>% inner_join(suivis, by = c("annee", "id_n")) %>%
  mutate(couple_vagues = factor(couple_vagues), annee_f = factor(annee),
         sans_enfant_t = nb_enf == 0, nb_enfants_t = cut(nb_enf, c(-Inf, 0, 1, 2, Inf)))
# Place mesurée à la vague de base, avec la composition OBSERVÉE (nouveau-né
# compris) pour les naissances suivantes : on la recalcule sans retirer le bébé.
compo_obs <- compter_membres(membres)
pan <- pan %>% mutate(place_base = place) %>%
  select(-c(jg, jf, ag, af, adultes, besoin, supp, marge, place)) %>%
  left_join(compo_obs, by = c("annee", "id_n")) %>% ajouter_place() %>%
  rename(place_t = place, place = place_base)
F_T <- update(CTRL, naiss01 ~ place + . - factor(annee) + annee_f)
F_S <- update(CTRL, naiss_suite ~ place_t + . - factor(annee) + couple_vagues + nb_enfants_t + taille + naiss01)
prosp <- imap_dfr(list("1er enfant" = TRUE, "Rang 2 et plus" = FALSE), function(r1, ch) {
  bind_rows(
    { d <- droplevels(filter(pan, (nb_enf_avant == 0) == r1))
      f <- if (r1) F_T else update(F_T, PAR)
      m <- svyglm(elaguer(f, d), design = des(d), family = quasibinomial(link = "probit"))
      as_tibble(avg_slopes(m, variables = "place", wts = m$prior.weights)) %>%
        mutate(naissance = "A la vague de base", taux = weighted.mean(d$naiss01, d$poids),
               n = nrow(d), naissances = sum(d$naiss01)) },
    { d <- droplevels(filter(pan, sans_enfant_t == r1))
      f <- if (r1) update(F_S, . ~ . - nb_enfants_t - taille - naiss01) else F_S
      m <- svyglm(elaguer(f, d), design = des(d), family = quasibinomial(link = "probit"))
      as_tibble(avg_slopes(m, variables = "place_t", wts = m$prior.weights)) %>%
        mutate(naissance = "Suivantes, jusqu'a la derniere vague",
               taux = weighted.mean(d$naiss_suite, d$poids),
               n = nrow(d), naissances = sum(d$naiss_suite)) }) %>%
    transmute(echantillon = ch, naissance, n, naissances, contraste = contrast, effet_pts = 100 * estimate,
              ic_bas = 100 * conf.low, ic_haut = 100 * conf.high, p = p.value,
              effet_pct_du_taux = 100 * estimate / taux)
})
cat("\nTest prospectif (couples suivis, script 43) :\n")
print(as.data.frame(prosp %>% mutate(across(where(is.double), ~ round(.x, 3)))), row.names = FALSE)

ecrire(bind_rows(desc %>% mutate(bloc = "Descriptif", .before = 1),
                 par_annee %>% mutate(bloc = "Part par annee", .before = 1),
                 res %>% mutate(bloc = "Modeles en coupe", .before = 1),
                 prosp %>% mutate(bloc = "Test prospectif", .before = 1)),
       "Output/norme_peuplement.csv")
