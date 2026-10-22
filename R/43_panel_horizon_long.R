# ============================================================================
#  SRCV — 43. Le test « contrainte ou choix » sur un horizon long
# ----------------------------------------------------------------------------
#  Le panel annuel (06, 29) ne voit que la naissance de l'année suivante :
#  272 naissances chez les installés, effet minimal détectable du même ordre
#  que l'effet mesuré en coupe. Ici, chaque ménage est suivi de sa PREMIÈRE à
#  sa DERNIÈRE observation dans son groupe de panel, et l'on compte toutes les
#  naissances survenues entre les deux :
#   - groupe 2014-2018 : logement observé en 2014, naissances jusqu'en 2018
#     (quatre ans) ;
#   - groupe 2022-2025 : logement observé à la première vague (2022, 2023 ou
#     2024), naissances jusqu'à la dernière (un à trois ans).
#  Naissance = un enfant présent à la dernière vague, né après l'année de base,
#  ou né l'année de base sans figurer dans le ménage à la vague de base.
#  Même spécification que la coupe (script 23) avec la parité OBSERVÉE à la
#  vague de base, une indicatrice de naissance à cette vague et des effets
#  fixes de couple (vague de base × horizon). On compare, sur les mêmes
#  couples, l'effet de la surface sur la naissance CONTEMPORAINE (à la vague
#  de base) et sur les naissances SUIVANTES, en points et en % du taux.
#  Sortie : Output/panel_horizon_long.csv
# ============================================================================

source("R/00_prepa_fecondite.R")
library(survey); library(broom); library(marginaleffects)
activer_cache_prepa()
IPC <- charger_ipc()
ANCIENNETE_MIN <- 3
GROUPES <- list(P1418 = c("2014", "2018"), P2225 = c("2022", "2023", "2024", "2025"))

# ── §1 Couples du champ, à chaque vague ─────────────────────────────────────
DONNEES <- millesimes_prepares(restreindre = TRUE)
coupe <- bind_rows(imap(DONNEES[intersect(names(DONNEES), unlist(GROUPES))],
                        ~ .x %>% select(any_of(c(VARS_INTERET, "datent"))))) %>%
  mutate(annee_num       = as.integer(annee),
         id_n            = normaliser_id(id),
         naiss01         = as.integer(naissance == "Oui"),
         cout_m2_reel    = deflater(cout_m2,    annee_num,     IPC, base = 2025),
         niveau_vie_reel = deflater(niveau_vie, annee_num - 1, IPC, base = 2025),
         surface_10 = surface / 10, nv_10k = niveau_vie_reel / 10000,
         sous60 = factor(if_else(surface < 60, "Moins de 60 m2", "60 m2 ou plus"),
                         levels = c("60 m2 ou plus", "Moins de 60 m2")),
         anciennete = annee_num - datent,
         nb_enfants_t = cut(nb_enf, breaks = c(-Inf, 0, 1, 2, Inf), labels = c("0", "1", "2", "3+")),
         sans_enfant_t = nb_enf == 0,
         across(where(is.factor), ~ factor(as.character(.))),
         statut3 = factor(statut3, levels = c("Locataire", "Proprietaire_accedant",
                                              "Proprietaire_non_accedant")),
         sous60 = factor(sous60, levels = c("60 m2 ou plus", "Moins de 60 m2"))) %>%
  filter(!is.na(surface), !is.na(cout_m2_reel), is.finite(cout_m2_reel),
         !is.na(statut3), !is.na(age_femme), !is.na(niveau_vie_reel),
         !is.na(poids), poids > 0)

# ── §2 Première et dernière observation de chaque ménage, par groupe ───────
#  Présence d'un ménage à une vague : fichier ménages complet (pas seulement le
#  champ des couples), pour suivre aussi les couples dont la femme a passé 49
#  ans ou qui ont changé de situation.
presence <- imap_dfr(VAGUES[unlist(GROUPES)], function(cfg, an)
  tibble(annee = an, id_n = normaliser_id(lire_srcv(cfg$men)[[cfg$vars[["id"]]]])))
enfants_fin <- function(an) {
  i <- lire_srcv(VAGUES[[an]]$ind)
  tibble(id_n = cle_menage_ind(i), anais = as.integer(an) - ages_individus(i, an),
         lien = trimws(as.character(i$LIENPREF)))
}

panel <- imap_dfr(GROUPES, function(ans, g) {
  base <- coupe %>% filter(annee %in% ans) %>% group_by(id_n) %>%
    filter(annee_num == min(annee_num)) %>% ungroup() %>%
    filter(annee != tail(ans, 1))
  fin <- presence %>% filter(annee %in% ans) %>% group_by(id_n) %>%
    summarise(annee_fin = max(as.integer(annee)), .groups = "drop")
  base <- base %>% inner_join(fin, by = "id_n") %>% filter(annee_fin > annee_num)
  # Enfants nés à partir de l'année de base, présents à la dernière vague
  naiss <- map_dfr(unique(base$annee_fin), function(af)
    enfants_fin(as.character(af)) %>% mutate(annee_fin = af)) %>%
    inner_join(base %>% select(id_n, annee_num, annee_fin), by = c("id_n", "annee_fin")) %>%
    filter(anais >= annee_num) %>%
    group_by(id_n) %>%
    summarise(n_apres = sum(anais > annee_num), n_annee_base = sum(anais == annee_num),
              .groups = "drop")
  # Né l'année de base : ne compte que s'il n'était pas déjà là à la vague de base
  bebes_base <- map_dfr(unique(base$annee), function(ab)
    enfants_fin(ab) %>% filter(anais == as.integer(ab)) %>% count(id_n, name = "deja_la"))
  base %>% left_join(naiss, by = "id_n") %>% left_join(bebes_base, by = "id_n") %>%
    mutate(across(c(n_apres, n_annee_base, deja_la), ~ coalesce(.x, 0L)),
           naiss_suite = as.integer(n_apres + pmax(n_annee_base - deja_la, 0) > 0),
           horizon = annee_fin - annee_num,
           groupe = g, couple_vagues = paste0(annee, "-", annee_fin))
}) %>%
  # En facteurs, pour qu'elaguer() retire ces effets fixes sur un seul groupe
  mutate(annee_f = factor(annee), couple_vagues = factor(couple_vagues))

cat("Couples suivis :\n")
print(as.data.frame(panel %>% group_by(groupe, couple_vagues) %>%
  summarise(n = n(), naissances_suivantes = sum(naiss_suite),
            taux_pct = round(100 * mean(naiss_suite), 1), naissances_t = sum(naiss01),
            .groups = "drop")), row.names = FALSE)
cat("Total :", nrow(panel), "couples,", sum(panel$naiss_suite), "avec au moins une naissance",
    "suivante (contre 272 naissances dans le test annuel des installés)\n")

# ── §3 Modèles ──────────────────────────────────────────────────────────────
des <- function(d) svydesign(ids = ~id_cluster, weights = ~poids, data = droplevels(d))
elaguer <- function(f, d) {
  vars <- setdiff(all.vars(f), all.vars(f[[2]]))
  mono <- vars[vapply(vars, function(v) v %in% names(d) &&
                        is.factor(d[[v]]) && nlevels(droplevels(d[[v]])) < 2, logical(1))]
  if (length(mono)) f <- update(f, paste(". ~ . -", paste(mono, collapse = " - ")))
  f
}
CONTROLES <- ~ cout_m2_reel + nv_10k + statut3 + age_c + I(age_c^2) + demenage + csp +
  urbain + zeat
F_T     <- update(CONTROLES, naiss01 ~ surface_10 + . + annee_f + nb_enfants + taille_avant)
F_SUITE <- update(CONTROLES, naiss_suite ~ surface_10 + . + couple_vagues +
                    nb_enfants_t + taille + naiss01)
sans_parite <- function(f) update(f, . ~ . - nb_enfants - taille_avant - nb_enfants_t - taille - naiss01)
seuil <- function(f) update(f, . ~ . - surface_10 + sous60)

estimer <- function(d, f, var, lib_ech, lib_outcome) {
  d <- droplevels(d); y <- all.vars(f)[1]
  m <- svyglm(elaguer(f, d), design = des(d), family = quasibinomial(link = "probit"))
  a <- as_tibble(avg_slopes(m, variables = var, wts = m$prior.weights))
  taux <- 100 * weighted.mean(d[[y]], d$poids)
  tibble(echantillon = lib_ech, naissance = lib_outcome, variable = var, n = nrow(d),
         naissances = sum(d[[y]]), taux_pct = taux,
         effet_pts = 100 * a$estimate, se = 100 * a$std.error,
         ic_bas = 100 * a$conf.low, ic_haut = 100 * a$conf.high, p = a$p.value,
         effet_pct_du_taux = 100 * 100 * a$estimate / taux,
         mde_pts = 2.8 * 100 * a$std.error)
}
inst <- function(d) filter(d, anciennete >= ANCIENNETE_MIN)
r1 <- function(d, base) if (base) filter(d, nb_enf_avant == 0) else filter(d, sans_enfant_t)

res <- bind_rows(
  # Surface linéaire : tous les couples, puis installés
  estimer(panel,       F_T,     "surface_10", "Tous les couples suivis", "A la vague de base"),
  estimer(panel,       F_SUITE, "surface_10", "Tous les couples suivis", "Suivantes, jusqu'a la derniere vague"),
  estimer(inst(panel), F_T,     "surface_10", "Installes 3 ans et plus", "A la vague de base"),
  estimer(inst(panel), F_SUITE, "surface_10", "Installes 3 ans et plus", "Suivantes, jusqu'a la derniere vague"),
  # Premier enfant : surface linéaire, puis seuil de 60 m2
  estimer(r1(panel, TRUE),  sans_parite(F_T),     "surface_10", "Sans enfant", "A la vague de base"),
  estimer(r1(panel, FALSE), sans_parite(F_SUITE), "surface_10", "Sans enfant", "Suivantes, jusqu'a la derniere vague"),
  estimer(r1(panel, TRUE),  seuil(sans_parite(F_T)),     "sous60", "Sans enfant", "A la vague de base"),
  estimer(r1(panel, FALSE), seuil(sans_parite(F_SUITE)), "sous60", "Sans enfant", "Suivantes, jusqu'a la derniere vague"),
  # Par groupe de panel (installés)
  estimer(inst(filter(panel, groupe == "P1418")), F_SUITE, "surface_10",
          "Installes, 2014 -> 2018", "Suivantes, jusqu'a la derniere vague"),
  estimer(inst(filter(panel, groupe == "P2225")), F_SUITE, "surface_10",
          "Installes, 2022 -> 2025", "Suivantes, jusqu'a la derniere vague")) %>%
  mutate(across(where(is.double), ~ round(.x, 3)))

cat("\nEffet de +10 m2 (ou d'un logement de moins de 60 m2), en points et en % du taux :\n")
print(as.data.frame(res), row.names = FALSE)
ecrire(res, "Output/panel_horizon_long.csv")

# Couples suivis, pour le test prospectif de la norme de peuplement (script 44)
write_csv(panel %>% select(id_n, annee, annee_fin, groupe, couple_vagues, naiss_suite),
          "Output/panel_horizon_long_couples.csv")
