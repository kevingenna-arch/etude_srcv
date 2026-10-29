# ============================================================================
#  SRCV — 41. La figure de synthèse par groupe social : part de la baisse de
#             l'ICF 2014-2025 associée à chaque facteur, par quartile de
#             niveau de vie et par diplôme de la femme
# ----------------------------------------------------------------------------
#  À lancer avec SRCV_DEBUT=2014 (réutilise les gains simulés du script 38).
#
#  Deux découpages :
#   - Quartiles du niveau de vie du MÉNAGE des femmes de 15 à 49 ans (revenu
#     disponible hors prestations familiales, minima et aides au logement, par
#     unité de consommation avant la naissance), seuils calculés dans chaque
#     vague sur les femmes, puis appliqués aux couples. ⚠️ Une femme qui vit
#     seule n'a qu'un revenu et tombe en Q1 : le recul de la vie en couple
#     remplit mécaniquement Q1, ce qui gonfle ce segment (66 % de la baisse en
#     Q1 ; même résultat à seuils 2014 fixes en euros constants).
#   - Diplôme de la femme (DDIPL : bac+3 ou plus, bac+2, bac, CAP-BEP ou
#     moins), qui ne dépend pas de la vie en couple. Femmes de 25 à 49 ans
#     seulement : avant 25 ans, beaucoup étudient encore et leur diplôme du
#     moment les classe à tort parmi les moins diplômées.
#
#  Facteurs, dans chaque groupe :
#   - Vie en couple : composition à taux fixés, comme le script 34 — part des
#     femmes en couple à âge donné, 2014 puis 2024-2025, avec les taux de
#     naissance de 2014 par âge et vie en couple (taux nationaux, plus stables
#     que ceux d'un groupe).
#   - Logement, petites surfaces : modèle en tranches 60/90/120 sur les
#     couples installés (script 30) ; surfaces de 2014 du groupe transposées
#     aux couples de 2025 du groupe.
#   - Âge des couples : modèle linéaire sur les installés (script 23) ; recul
#     NATIONAL de l'âge à parité donnée (2014 -> 2025), appliqué aux couples de
#     2025 du groupe. Mesuré groupe par groupe, il reflétait la recomposition
#     des groupes (−48 à +94 %), pas le report des naissances.
#   - Politique familiale : effet relatif de 1 000 € de gain estimé dans
#     l'ERFS 2014-2023 (script 57, 16 000 naissances), appliqué au recul du
#     gain d'un enfant des couples de 2025 du groupe entre les barèmes 2014 et
#     2025 (gains simulés du script 38), avec son intervalle à 95 % ; variante
#     avec l'effet estimé sur 2005-2023 (`pf_0523`), plus précis. L'estimation
#     de SRCV (modèle du script 38), trop imprécise, est gardée pour mémoire
#     (`politique_familiale_srcv`).
#  Toutes les parts sont rapportées à la baisse NATIONALE de l'ICF (−22 %) :
#  le « non expliqué » suppose que chaque groupe a baissé comme la moyenne.
#  Sorties : Output/decomposition_icf_par_quartile.csv,
#            Output/decomposition_icf_par_diplome.csv (graphiques : script 42)
# ============================================================================

lignes <- readLines("R/38_politique_familiale.R", encoding = "UTF-8")
eval(parse(text = lignes[1:(grep("§2 Modèle", lignes)[1] - 1)], encoding = "UTF-8"))

BAISSE <- (ICF_2025 - ICF_2014) / ICF_2014
AGES <- c(14, 24, 29, 34, 39, 49)
ANS_Q <- c("2014", "2024", "2025")
DIPLOMES <- c("1" = "Supérieur (bac+3 ou plus)", "3" = "Bac+2", "4" = "Bac",
              "5" = "CAP-BEP ou moins", "6" = "CAP-BEP ou moins", "7" = "CAP-BEP ou moins")

# ── §1 Femmes de 15 à 49 ans : niveau de vie, quartile, diplôme ────────────
niveau_vie_menages <- function(an) {
  cfg <- VAGUES[[an]]; v <- cfg$vars
  m <- lire_srcv(cfg$men)
  tr <- function(x) { k <- names(m)[toupper(names(m)) == x]; coalesce(num(m[[k[1]]]), 0) }
  tibble(id_men = normaliser_id(m[[v[["id"]]]]), poids = num(m[[v[["poids"]]]]),
         remb = coalesce(num(m[[v[["rembours"]]]]), 0),
         revenu_hp = num(m[[v[["revenu"]]]]) - tr("HY050N") - tr("HY060N") - tr("HY070N"),
         uc = num(m$HX050)) %>%
    filter(!is.na(poids), poids > 0, remb <= MENSUALITE_MAX)
}
individus <- map(set_names(ANS_Q), function(an) {
  i <- lire_srcv(VAGUES[[an]]$ind); names(i) <- toupper(names(i))
  i %>% mutate(id_men = cle_menage_ind(i), rb030 = trimws(as.character(RB030)),
               age = ages_individus(i, an), anais = num(ANAIS),
               sexe = trimws(as.character(SEXE)), couple = num(COUPLE),
               lien = trimws(as.character(LIENPREF)),
               diplome = unname(DIPLOMES[trimws(as.character(DDIPL))]))
})
femmes <- imap_dfr(individus, function(i, an) {
  meres <- i %>% filter(anais >= as.integer(an) - 1, !is.na(RB230)) %>%
    transmute(rb030 = trimws(as.character(RB230))) %>% distinct() %>% mutate(naiss = 1L)
  i %>% filter(sexe == "2", age >= 15, age <= 49) %>%
    left_join(meres, by = "rb030") %>%
    mutate(naiss = coalesce(naiss, 0L), annee = an,
           en_couple = coalesce(couple == 1, FALSE),
           age_cl = cut(age, AGES)) %>%
    inner_join(niveau_vie_menages(an), by = "id_men") %>%
    group_by(id_men) %>% mutate(uc_avant = pmax(uc - 0.3 * sum(naiss), 1)) %>% ungroup() %>%
    mutate(nv = revenu_hp / uc_avant) %>%
    select(annee, age, age_cl, en_couple, naiss, nv, diplome, poids)
}) %>% filter(!is.na(nv))

seuils <- femmes %>% group_by(annee) %>%
  summarise(s1 = wquantile(nv, poids, .25), s2 = wquantile(nv, poids, .5),
            s3 = wquantile(nv, poids, .75), .groups = "drop")
classer <- function(d, nv) d %>% left_join(seuils, by = "annee") %>%
  mutate(q = case_when({{ nv }} < s1 ~ "Q1", {{ nv }} < s2 ~ "Q2",
                       {{ nv }} < s3 ~ "Q3", TRUE ~ "Q4")) %>%
  select(-s1, -s2, -s3)
femmes <- classer(femmes, nv) %>% mutate(periode = if_else(annee == "2014", "A", "B"))
cat("Seuils de niveau de vie (€ courants/an/UC) :\n"); print(as.data.frame(seuils))

# Diplôme de la femme du couple (personne de référence ou conjointe)
diplome_couples <- imap_dfr(individus, function(i, an)
  i %>% filter(sexe == "2", lien %in% c("00", "01", "0", "1")) %>%
    transmute(cle = paste(an, id_men, sep = "_"), diplome) %>% distinct(cle, .keep_all = TRUE))

# ── §2 Vie en couple : composition à taux fixés ────────────────────────────
taux_A <- femmes %>% filter(periode == "A") %>% group_by(age_cl, en_couple) %>%
  summarise(r = weighted.mean(naiss, poids), .groups = "drop")
effet_couple <- function(d) {
  p <- d %>% group_by(periode, age_cl, en_couple) %>% summarise(w = sum(poids), .groups = "drop") %>%
    group_by(periode, age_cl) %>% mutate(pg = w / sum(w)) %>% ungroup() %>%
    group_by(periode) %>% mutate(a = w / sum(w)) %>% ungroup()
  aB <- p %>% filter(periode == "B") %>% group_by(age_cl) %>% summarise(aB = sum(a), .groups = "drop")
  taux <- function(per) p %>% filter(periode == per) %>% select(age_cl, en_couple, pg) %>%
    left_join(taux_A, by = c("age_cl", "en_couple")) %>% group_by(age_cl) %>%
    summarise(R = sum(pg * r), .groups = "drop") %>% left_join(aB, by = "age_cl") %>%
    summarise(t = sum(aB * R, na.rm = TRUE)) %>% pull(t)
  taux("B") / taux("A") - 1
}
en_couple <- function(d) tibble(
  en_couple_2014 = 100 * weighted.mean(d$en_couple[d$periode == "A"], d$poids[d$periode == "A"]),
  en_couple_2425 = 100 * weighted.mean(d$en_couple[d$periode == "B"], d$poids[d$periode == "B"]))

# ── §3 Couples : logement, âge, politique familiale ────────────────────────
des <- function(d) svydesign(ids = ~id_cluster, weights = ~poids, data = droplevels(d))
elaguer <- function(f, d) {
  vars <- setdiff(all.vars(f), all.vars(f[[2]]))
  mono <- vars[vapply(vars, function(v) v %in% names(d) &&
                        is.factor(d[[v]]) && nlevels(droplevels(d[[v]])) < 2, logical(1))]
  if (length(mono)) f <- update(f, paste(". ~ . -", paste(mono, collapse = " - ")))
  f
}
fit <- function(d, f) { d <- droplevels(d)
  svyglm(elaguer(f, d), design = des(d), family = quasibinomial(link = "probit")) }
F_POOLE <- naiss01 ~ surface_10 + cout_m2_reel + nv_10k + statut3 + age_c + I(age_c^2) +
  demenage + csp + urbain + zeat + factor(annee) + nb_enfants + taille_avant
F_PF <- update(F_POOLE, . ~ . + gain_1k + gain_ref_1k + I(nv_10k^2) + benjamin_3)
BORNES <- c(-Inf, 60, 90, 120, Inf)
tr <- function(x) x %>% mutate(surf_tr = factor(cut(surface, BORNES, labels = paste0("T", 1:4),
                                                    right = FALSE), levels = paste0("T", 1:4)))

df <- df %>% mutate(anciennete = annee_num - datent) %>%
  filter(if_all(all_of(setdiff(all.vars(F_PF), "annee")), ~ !is.na(.x)))
di <- filter(df, anciennete >= 3)
m_tr  <- fit(tr(di), update(F_POOLE, . ~ . - surface_10 + surf_tr))
m_lin <- fit(di, F_POOLE)
m_pf  <- fit(df, F_PF)
ERFS  <- effet_relatif_erfs("2014-2023", euros = 2025)   # retenu (période des réformes)
ERFS2 <- effet_relatif_erfs("2005-2023", euros = 2025)   # variante, plus précise
for (e in list(ERFS, ERFS2))
  cat("Effet relatif de 1 000 € (de 2025) de gain, ERFS", e$periode, ":", round(100 * e$effet, 2),
      "% [", round(100 * e$bas, 2), ";", round(100 * e$haut, 2), "]\n")

# `diplome` des données préparées est celui de la personne de référence : on le
# remplace par celui de la femme.
couples <- df %>% filter(annee %in% c("2014", "2025")) %>% classer(niveau_vie) %>%
  select(-any_of("diplome")) %>% left_join(diplome_couples, by = "cle")
predire <- function(m, d) weighted.mean(as.numeric(predict(m, newdata = d, type = "response")),
                                        d$poids)
transposer_par <- function(x, w, g, x_ref, w_ref, g_ref, n_min = 30) {
  out <- x
  for (lv in unique(as.character(g[!is.na(g)]))) {
    i <- which(as.character(g) == lv); j <- which(as.character(g_ref) == lv)
    if (length(j) >= n_min) out[i] <- transposer(x[i], w[i], x_ref[j], w_ref[j])
  }
  out
}
effets_couples <- function(d) {
  d25 <- filter(d, annee == "2025"); d14 <- filter(d, annee == "2014")
  cf_s <- d25 %>% mutate(surface = transposer(surface, poids, d14$surface, d14$poids),
                         surface_10 = surface / 10)
  cf_a <- d25 %>% mutate(age_femme = age_cf, age_c = (age_femme - 30) / 10)
  cf_p <- d25 %>% mutate(gain_1k = gain_ref_1k)
  dg <- weighted.mean(d25$gain_ref_1k - d25$gain_1k, d25$poids)   # k€ de 2025
  tibble(logement = predire(m_tr, tr(cf_s)) / predire(m_tr, tr(d25)) - 1,
         age = predire(m_lin, cf_a) / predire(m_lin, d25) - 1,
         politique_familiale = ERFS$effet * dg, pf_ic_bas = ERFS$bas * dg, pf_ic_haut = ERFS$haut * dg,
         pf_0523 = ERFS2$effet * dg, pf_0523_bas = ERFS2$bas * dg, pf_0523_haut = ERFS2$haut * dg,
         politique_familiale_srcv = predire(m_pf, cf_p) / predire(m_pf, d25) - 1,
         recul_gain_euros = 1000 * dg,
         n_couples_2025 = nrow(d25), naissances_2025 = sum(d25$naiss01))
}

# ── §4 Assemblage, pour chaque découpage ───────────────────────────────────
#  vie en couple : rel < 0 (le recul abaisse la natalité) ; les autres canaux :
#  rel > 0 (le retour à 2014 la relève). Parts de la baisse nationale.
decomposer <- function(f, cpl, groupe, niveaux) {
  # Âge : recul NATIONAL (dans la population du découpage), appliqué à chaque
  # couple de 2025 selon son rang à parité donnée. Transposé groupe par groupe,
  # il mesurait surtout la recomposition des groupes (les jeunes générations
  # plus diplômées vieillissent le groupe CAP-BEP), d'où des valeurs de −48 à
  # +94 % sans rapport avec le report des naissances.
  c25 <- filter(cpl, annee == "2025"); c14 <- filter(cpl, annee == "2014")
  cpl <- cpl %>% mutate(age_cf = age_femme)
  cpl$age_cf[cpl$annee == "2025"] <- transposer_par(c25$age_femme, c25$poids, c25$nb_enfants,
                                                    c14$age_femme, c14$poids, c14$nb_enfants)
  un <- function(ff, cc, lib) bind_cols(
    tibble(groupe = lib, vie_en_couple = 100 * effet_couple(ff) / BAISSE),
    effets_couples(cc) %>% mutate(across(c(logement, age, politique_familiale, pf_ic_bas, pf_ic_haut,
                                           pf_0523, pf_0523_bas, pf_0523_haut, politique_familiale_srcv),
                                         ~ 100 * .x / -BAISSE)),
    en_couple(ff))
  bind_rows(un(f, cpl, "Ensemble"),
            map_dfr(niveaux, ~ un(filter(f, .data[[groupe]] == .x),
                                  filter(cpl, .data[[groupe]] == .x), .x))) %>%
    mutate(non_explique = 100 - vie_en_couple - logement - age - politique_familiale,
           across(where(is.double), ~ round(.x, 1)))
}
par_quartile <- decomposer(femmes, couples, "q", c("Q1", "Q2", "Q3", "Q4"))
par_diplome <- decomposer(filter(femmes, age >= 25), filter(couples, age_femme >= 25),
                          "diplome", unique(unname(DIPLOMES)))
cat("\nPart de la baisse nationale de l'ICF (−22 %), par quartile de niveau de vie :\n")
print(as.data.frame(par_quartile), row.names = FALSE)
cat("\nPar diplôme de la femme (25-49 ans) :\n")
print(as.data.frame(par_diplome), row.names = FALSE)
write_csv(par_quartile, "Output/decomposition_icf_par_quartile.csv")
write_csv(par_diplome, "Output/decomposition_icf_par_diplome.csv")

# Graphiques : R/42_graphique_parts_icf_quartiles.R (lit les sorties ci-dessus).
