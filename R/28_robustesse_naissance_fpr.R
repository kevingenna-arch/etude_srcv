# ============================================================================
#  SRCV — 28. ROBUSTESSE : LA MESURE DE LA NAISSANCE DANS LES VAGUES FPR
# ----------------------------------------------------------------------------
#  Deux fragilités propres aux vagues 2022-2025, jamais testées jusqu'ici, et
#  qui peuvent l'une et l'autre faire bouger la réponse à la question centrale.
#
#  (1) DOUBLE COMPTAGE. Dans les fichiers FPR la naissance est reconstruite
#      comme « un enfant né en N ou N-1 est présent dans le ménage ». Un enfant
#      né en 2022 est donc compté dans la vague 2022 ET dans la vague 2023 pour
#      les ~68 % de ménages réinterrogés d'une vague à la suivante. Les effets
#      fixes d'année absorbent le niveau, mais pas la composition de qui est
#      compté deux fois. Trois variantes :
#        « né en N »    : fenêtre d'un an, cohorte incomplète (seuls les enfants
#                         nés avant la date d'enquête sont observés) ;
#        « né en N-1 »  : fenêtre d'un an, cohorte complète, la plus proche
#                         d'EVENEMEN_C (naissance dans les douze derniers mois) ;
#        « dédupliqué » : l'enfant né en N-1 n'est compté en N que si le ménage
#                         n'était PAS dans l'échantillon d'analyse en N-1, où il
#                         l'a déjà été. Chaque naissance compte une fois.
#      Dans chaque variante les contrôles pré-naissance (nb_enf_avant,
#      taille_avant) et le rang sont recalculés en cohérence : un couple dont
#      l'enfant né en N-1 n'est plus « une naissance » devient un couple qui a
#      déjà un enfant, ce qui est exact.
#  (2) PONDÉRATION 2022-2023. Les poids de ces deux vagues sont distordus
#      précisément sur les ménages avec nouveau-né (annexe C de l'étude,
#      scripts 17-18). Les modèles les gardaient au motif que les effets fixes
#      d'année absorbent la distorsion ; on ré-estime sans elles.
#
#  Pour chaque variante on ré-estime les spécifications PUBLIÉES du script 23 :
#  l'effet de la surface (+10 m², tableau « effet_surface »), le seuil à 50 m²
#  (tableau « seuil »), l'écart accédant/locataire au premier enfant (M5 du
#  tableau « escalier ») et la décomposition 2010-2025 (linéaire et en seuil,
#  modèle « logement prédéterminé »), y compris la borne haute « sans le canal
#  statut ».
#
#  À LANCER DEPUIS LA RACINE DU PROJET :
#     source("R/28_robustesse_naissance_fpr.R")
#
#  Sortie : Output/robustesse_naissance_fpr.csv
# ============================================================================

source("R/00_prepa_fecondite.R")
library(survey); library(broom); library(marginaleffects)
activer_cache_prepa()

IPC <- charger_ipc()
if (!dir.exists("Output")) dir.create("Output")

BAISSE_RELATIVE <- (ICF_2025 - ICF_2010) / ICF_2010      # ICF_* : 00_config.R
ANCIENNETE_MIN  <- 3
SEUILS     <- c(-Inf, 50, 70, 90, 120, Inf)
LIB_SEUILS <- c("Moins de 50 m2", "50 a 69 m2", "70 a 89 m2 (ref.)",
                "90 a 119 m2", "120 m2 et plus")

DONNEES_FEC <- millesimes_prepares(restreindre = TRUE)   # couples, femme 15-49

# ============================================================================
#  §0 — LES VARIANTES DE LA NAISSANCE
# ============================================================================
cat("########## §0 — VARIANTES DE LA MESURE DE LA NAISSANCE ##########\n")

# Par ménage et vague FPR : un enfant né en N ? un enfant né en N-1 ?
# (même clé et même lecture que ajouter_naissance(), 00_utils.R)
naiss_fpr <- imap_dfr(keep(VAGUES, ~ !is.null(.x$naiss_ind)), function(cfg, an) {
  ni <- cfg$naiss_ind; N <- as.integer(an)
  lire_srcv(cfg$ind) %>%
    transmute(id    = trimws(as.character(.data[[ni$cle_ind]])),
              anais = num(.data[[ni$annais]])) %>%
    group_by(id) %>%
    summarise(ne_N  = as.integer(any(anais == N,     na.rm = TRUE)),
              ne_N1 = as.integer(any(anais == N - 1, na.rm = TRUE)),
              .groups = "drop") %>%
    mutate(annee = an)
})

# Ménages présents dans l'échantillon d'analyse à la vague PRÉCÉDENTE : c'est
# là que l'enfant né en N-1 a déjà été compté.
deja_vus <- imap_dfr(DONNEES_FEC, ~ tibble(annee = as.character(as.integer(.y) + 1),
                                          id = unique(.x$id), deja_vu = TRUE))

base <- bind_rows(imap(DONNEES_FEC, ~ .x %>% select(any_of(c(VARS_INTERET, "datent"))))) %>%
  left_join(naiss_fpr, by = c("annee", "id")) %>%
  left_join(deja_vus,  by = c("annee", "id")) %>%
  mutate(annee_num = as.integer(annee),
         est_fpr   = annee %in% vagues_fpr(),
         deja_vu   = coalesce(deja_vu, FALSE),
         ne_N      = coalesce(ne_N, 0L),
         ne_N1     = coalesce(ne_N1, 0L),
         naiss_ref = as.integer(naissance == "Oui"),
         naiss_N   = if_else(est_fpr, ne_N,  naiss_ref),
         naiss_N1  = if_else(est_fpr, ne_N1, naiss_ref),
         naiss_dd  = if_else(est_fpr,
                             as.integer(ne_N == 1L | (ne_N1 == 1L & !deja_vu)),
                             naiss_ref))

# Contrôle de cohérence : la référence DOIT être exactement « né en N ou N-1 ».
ecarts <- with(filter(base, est_fpr), sum(naiss_ref != as.integer(ne_N == 1L | ne_N1 == 1L)))
stopifnot("La reconstruction ne coincide pas avec ajouter_naissance()" = ecarts == 0)

diag <- base %>% filter(est_fpr) %>% group_by(annee) %>%
  summarise(n = n(), reference = sum(naiss_ref), ne_en_N = sum(naiss_N),
            ne_en_N1 = sum(naiss_N1), deduplique = sum(naiss_dd),
            # naissances de la référence qui ne tiennent qu'à un enfant né en
            # N-1 dans un ménage déjà présent en N-1 : comptées deux fois.
            deja_comptees = sum(naiss_ref == 1L & ne_N == 0L & ne_N1 == 1L & deja_vu),
            part_deja_comptees_pct = round(100 * deja_comptees / reference, 1),
            .groups = "drop")
cat("\nAmpleur du double comptage dans les vagues FPR (champ fecond) :\n")
print(as.data.frame(diag), row.names = FALSE)

VARIANTES <- list(
  "Reference (ne en N ou N-1)"   = list(var = "naiss_ref", vagues = names(VAGUES)),
  "Ne en N seulement"            = list(var = "naiss_N",   vagues = names(VAGUES)),
  "Ne en N-1 seulement"          = list(var = "naiss_N1",  vagues = names(VAGUES)),
  "Deduplique (menage x enfant)" = list(var = "naiss_dd",  vagues = names(VAGUES)),
  "Reference, hors 2022-2023"    = list(var = "naiss_ref",
                                        vagues = setdiff(names(VAGUES), c("2022", "2023"))),
  "Deduplique, hors 2022-2023"   = list(var = "naiss_dd",
                                        vagues = setdiff(names(VAGUES), c("2022", "2023"))))

# Applique une variante : outcome, contrôles pré-naissance et rang recalculés,
# puis exactement les transformations et filtres du script 23.
preparer <- function(v) {
  base %>% filter(annee %in% v$vagues) %>%
    mutate(naiss01      = .data[[v$var]],
           nb_enf_avant = pmax(nb_enf - naiss01, 0),
           taille_avant = taille - naiss01,
           nb_enfants   = cut(nb_enf_avant, breaks = c(-Inf, 0, 1, 2, Inf),
                              labels = c("0", "1", "2", "3+")),
           rang         = factor(if_else(nb_enf_avant == 0, "1er enfant", "Rang 2+"),
                                 levels = c("1er enfant", "Rang 2+")),
           cout_m2_reel    = deflater(cout_m2,    annee_num,     IPC, base = 2025),
           niveau_vie_reel = deflater(niveau_vie, annee_num - 1, IPC, base = 2025),
           surface_10   = surface / 10,
           nv_10k       = niveau_vie_reel / 10000,
           anciennete   = annee_num - datent,
           across(where(is.factor), ~ factor(as.character(.))),
           statut3 = factor(statut3, levels = c("Locataire", "Proprietaire_accedant",
                                                "Proprietaire_non_accedant")),
           rang    = factor(rang, levels = c("1er enfant", "Rang 2+")),
           surf_tr = relevel(factor(cut(surface, SEUILS, labels = LIB_SEUILS, right = FALSE),
                                    levels = LIB_SEUILS), ref = "70 a 89 m2 (ref.)")) %>%
    filter(!is.na(surface), !is.na(cout_m2_reel), is.finite(cout_m2_reel),
           !is.na(statut3), !is.na(age_femme), !is.na(niveau_vie_reel),
           !is.na(poids), poids > 0)
}

# ── Outils communs (identiques au script 23) ────────────────────────────────
des <- function(d) svydesign(ids = ~id_cluster, weights = ~poids, data = droplevels(d))
elaguer <- function(f, d) {
  vars <- setdiff(all.vars(f), all.vars(f[[2]]))
  mono <- vars[vapply(vars, function(v) v %in% names(d) &&
                        is.factor(d[[v]]) && nlevels(droplevels(d[[v]])) < 2, logical(1))]
  if (length(mono)) f <- update(f, paste(". ~ . -", paste(mono, collapse = " - ")))
  f
}
ame <- function(m, vars, ...) as_tibble(avg_slopes(m, variables = vars,
                                                   wts = m$prior.weights, ...))
en_points <- function(x) x %>% mutate(across(any_of(c("estimate", "std.error",
                                                      "conf.low", "conf.high")),
                                             ~ 100 * .x))
fit <- function(d, f) {
  d <- droplevels(d)
  svyglm(elaguer(f, d), design = des(d), family = quasibinomial(link = "probit"))
}
predire <- function(m, d, w = d$poids)
  weighted.mean(as.numeric(predict(m, newdata = d, type = "response")), w)

F_BASE  <- naiss01 ~ surface_10 + cout_m2_reel + nv_10k + statut3 +
  age_c + I(age_c^2) + demenage + csp + urbain + zeat + factor(annee)
F_POOLE <- update(F_BASE, . ~ . + nb_enfants + taille_avant)
F_RANG1 <- F_BASE
F_TR    <- update(F_POOLE, . ~ . - surface_10 + surf_tr)
F_TR_1  <- update(F_RANG1, . ~ . - surface_10 + surf_tr)
F_M5    <- naiss01 ~ statut3 + age_c + I(age_c^2) + nb_enfants + taille_avant +
  nv_10k + csp + urbain + zeat + surface_10 + cout_m2_reel + factor(annee)

ligne <- function(bloc, lib, d, x, contraste = "surface_10") {
  x %>% en_points() %>%
    transmute(bloc = bloc, echantillon = lib, n = nrow(d), naissances = sum(d$naiss01),
              taux_base_pct = 100 * weighted.mean(d$naiss01, d$poids),
              contraste = if ("contrast" %in% names(x)) contrast else contraste,
              ame_pts = estimate, se = std.error, conf.low, conf.high, p.value)
}

# ── Décomposition 2010-2025 (§5 et §8 du script 23) sous une variante ───────
decomposer_variante <- function(d, di) {
  d10 <- filter(d, annee == "2010"); d25 <- filter(d, annee == "2025")
  part_statut <- function(x) prop.table(tapply(x$poids, droplevels(x$statut3), sum))
  p10 <- part_statut(d10); p25 <- part_statut(d25)
  poids_statut <- d25$poids * unname((p10 / p25)[as.character(d25$statut3)])
  retrancher <- function(x) x %>%
    mutate(surf_tr = relevel(factor(cut(surface, SEUILS, labels = LIB_SEUILS, right = FALSE),
                                    levels = LIB_SEUILS), ref = "70 a 89 m2 (ref.)"))
  cf_surface <- d25 %>% mutate(surface = transposer(surface, poids, d10$surface, d10$poids),
                               surface_10 = surface / 10) %>% retrancher()
  cf_prix    <- d25 %>% mutate(cout_m2_reel = transposer(cout_m2_reel, poids,
                                                         d10$cout_m2_reel, d10$poids))
  cf_tout    <- cf_surface %>% mutate(cout_m2_reel = transposer(cout_m2_reel, poids,
                                                                d10$cout_m2_reel, d10$poids))
  modeles <- list("lineaire" = fit(di, F_POOLE), "en seuil" = fit(di, F_TR))
  map_dfr(modeles, function(m) {
    p0 <- predire(m, d25)
    tibble(contraste = c("Surface de 2010", "Prix au m2 de 2010", "Statuts de 2010",
                         "Surface + prix de 2010 (sans les statuts)",
                         "Logement complet de 2010"),
           p_cf = c(predire(m, cf_surface), predire(m, cf_prix),
                    predire(m, d25, poids_statut), predire(m, cf_tout),
                    predire(m, cf_tout, poids_statut))) %>%
      mutate(ecart_relatif_pct = 100 * (p_cf / p0 - 1),
             part = ecart_relatif_pct / (-100 * BAISSE_RELATIVE) * 100)
  }, .id = "spec") %>%
    transmute(bloc = paste0("Decomposition 2010-2025, surface ", spec,
                            " (part de la baisse de l'ICF, %)"),
              echantillon = "Installes 3 ans et plus, predit sur 2025",
              n = nrow(di), naissances = sum(di$naiss01), taux_base_pct = NA_real_,
              contraste, ame_pts = part, se = ecart_relatif_pct,
              conf.low = NA_real_, conf.high = NA_real_, p.value = NA_real_)
}

# ============================================================================
#  §1 — RÉ-ESTIMATION SOUS CHAQUE VARIANTE
# ============================================================================
cat("\n\n########## §1 — RE-ESTIMATION SOUS CHAQUE VARIANTE ##########\n")

estimer_variante <- function(v, nom) {
  t0 <- Sys.time()
  cat("\n=== ", nom, " ===\n")
  d   <- preparer(v)
  d1  <- filter(d, rang == "1er enfant")
  di  <- filter(d, anciennete >= ANCIENNETE_MIN)
  d1i <- filter(d1, anciennete >= ANCIENNETE_MIN)
  cat(sprintf("  %d obs, %d naissances | 1er enfant : %d obs, %d naissances\n",
              nrow(d), sum(d$naiss01), nrow(d1), sum(d1$naiss01)))

  # (a) Effet de la surface, +10 m2 (tableau effet_surface)
  ech_surf <- list("Ensemble" = list(d, F_POOLE), "1er enfant" = list(d1, F_RANG1),
                   "Ensemble - installes 3 ans et plus"   = list(di, F_POOLE),
                   "1er enfant - installes 3 ans et plus" = list(d1i, F_RANG1))
  surf <- imap_dfr(ech_surf, function(x, lib)
    ligne("Surface (+10 m2)", lib, x[[1]], ame(fit(x[[1]], x[[2]]), "surface_10")))

  # (b) Tranches de surface (tableau seuil), premier enfant
  tr <- imap_dfr(list("1er enfant" = d1, "1er enfant - installes 3 ans et plus" = d1i),
                 function(x, lib)
                   ligne("Tranches de surface (vs 70-89 m2)", lib, x, ame(fit(x, F_TR_1), "surf_tr")))

  # (c) Écart accédant / locataire, M5, premier enfant (tableau escalier)
  st <- ligne("Statut, M5 (vs locataire)", "1er enfant", d1, ame(fit(d1, F_M5), "statut3"))

  # (d) Décomposition 2010-2025
  dec <- tryCatch(decomposer_variante(d, di), error = function(e) {
    cat("  !! decomposition impossible :", conditionMessage(e), "\n"); tibble() })

  res <- bind_rows(surf, tr, st, dec) %>% mutate(variante = nom, .before = 1)
  cat(sprintf("  termine en %.0f s\n", as.numeric(difftime(Sys.time(), t0, units = "secs"))))
  res
}

resultats <- imap_dfr(VARIANTES, estimer_variante)
write_csv(resultats, "Output/robustesse_naissance_fpr.csv")

# ============================================================================
#  §2 — SYNTHÈSE : les chiffres publiés tiennent-ils ?
# ============================================================================
cat("\n\n########## §2 — SYNTHESE ##########\n")
arrondi <- function(x) x %>% mutate(across(where(is.numeric), ~ round(.x, 3)))

cat("\n(a) Effet de la surface, +10 m2 (points de %) :\n")
print(as.data.frame(resultats %>% filter(bloc == "Surface (+10 m2)") %>%
  select(variante, echantillon, ame_pts, p.value) %>%
  pivot_wider(names_from = echantillon, values_from = c(ame_pts, p.value)) %>% arrondi()),
  row.names = FALSE)

cat("\n(b) Moins de 50 m2 vs 70-89 m2, premier enfant (points de %) :\n")
print(as.data.frame(resultats %>%
  filter(bloc == "Tranches de surface (vs 70-89 m2)", str_detect(contraste, "Moins de 50")) %>%
  select(variante, echantillon, ame_pts, conf.low, conf.high, p.value) %>% arrondi()),
  row.names = FALSE)

cat("\n(c) Accedant vs locataire, M5, premier enfant (points de %) :\n")
print(as.data.frame(resultats %>%
  filter(bloc == "Statut, M5 (vs locataire)", str_detect(contraste, "accedant -")) %>%
  select(variante, contraste, ame_pts, conf.low, conf.high, p.value) %>% arrondi()),
  row.names = FALSE)

cat("\n(d) Decomposition : part de la baisse de l'ICF reproduite (%) :\n")
print(as.data.frame(resultats %>% filter(str_detect(bloc, "Decomposition")) %>%
  mutate(spec = if_else(str_detect(bloc, "lineaire"), "lineaire", "seuil")) %>%
  select(variante, spec, contraste, ame_pts) %>%
  pivot_wider(names_from = spec, values_from = ame_pts) %>% arrondi()),
  row.names = FALSE)

cat("\n=== 28 termine. Baisse de l'ICF a expliquer :",
    round(100 * BAISSE_RELATIVE, 1), "% (", ICF_2010, "->", ICF_2025, "). ===\n")
