# ============================================================================
#  SRCV — 35. Le logement retarde-t-il les naissances ?
# ----------------------------------------------------------------------------
#  Hypothèse : les logements étant plus petits, les couples accèdent plus tard
#  à un logement assez grand pour un enfant, et repoussent les naissances ;
#  l'ICF baisse alors par le calendrier (effet de tempo).
#
#  1. Âge d'accès à un logement familial : part des couples (femme de 20 à 44
#     ans, personne de référence ou conjointe) logés dans au moins 60, 70 ou
#     80 m², par classe d'âge de la femme ; âge auquel la moitié des couples
#     franchit le seuil (logit pondéré en âge et âge au carré, intervalle par
#     rééchantillonnage des ménages).
#  2. Âge de la mère à la naissance : enfants nés l'année d'enquête ou la
#     précédente, reliés à leur mère par RB230 (fichiers individus ; absent en
#     2018). Première naissance : la mère n'a aucun autre enfant dans le
#     ménage (approximation : des aînés partis du foyer feraient compter une
#     naissance de rang 2 comme une première).
#  2022 et 2023 écartées des statistiques pondérées (anomalie de pondération).
#  Sortie : Output/age_acces_logement.csv
# ============================================================================

source("R/00_prepa_fecondite.R")
activer_cache_prepa()
set.seed(2026)

ANS   <- c("2006", "2010", "2014", "2018", "2024", "2025")
SEUILS <- c(60, 70, 80)
res <- list()
ajouter <- function(x, bloc) res[[length(res) + 1]] <<- mutate(x, bloc = bloc, .before = 1)

# ── 1. Âge d'accès à un logement familial ───────────────────────────────────
cpl <- bind_rows(millesimes_prepares(restreindre = TRUE)) %>%
  filter(annee %in% ANS, !is.na(surface), !is.na(age_femme), age_femme >= 20,
         age_femme <= 44, !is.na(poids), poids > 0) %>%
  mutate(age_cl = cut(age_femme, c(19, 24, 27, 30, 33, 36, 39, 44),
                      labels = c("20-24", "25-27", "28-30", "31-33", "34-36", "37-39", "40-44")))

for (s in SEUILS) {
  ajouter(cpl %>% group_by(colonne = annee, cle = as.character(age_cl)) %>%
            summarise(valeur = 100 * weighted.mean(surface >= s, poids), .groups = "drop") %>%
            mutate(indicateur = paste0("Part des couples dans ", s, " m2 ou plus (%)")),
          "1. Part par age")
}

age_median <- function(d, s) {
  m <- suppressWarnings(glm(I(surface >= s) ~ age_femme + I(age_femme^2), data = d,
                            weights = poids / mean(poids), family = quasibinomial()))
  f <- function(a) predict(m, data.frame(age_femme = a), type = "response") - 0.5
  if (f(20) * f(44) > 0) return(NA_real_)
  uniroot(f, c(20, 44))$root
}
for (an in ANS) for (s in SEUILS) {
  d <- filter(cpl, annee == an)
  est <- age_median(d, s)
  boot <- replicate(200, age_median(d[sample(nrow(d), replace = TRUE), ], s))
  ajouter(tibble(cle = paste0(s, " m2 ou plus"), colonne = an,
                 indicateur = c("Age auquel la moitie des couples franchit le seuil",
                                "IC 95 % bas", "IC 95 % haut"),
                 valeur = c(est, quantile(boot, c(0.025, 0.975), na.rm = TRUE))),
          "1. Age median d'acces")
}

# ── 2. Âge de la mère à la naissance ────────────────────────────────────────
naissances <- map_dfr(c("2014", "2022", "2023", "2024", "2025"), function(an) {
  cfg <- VAGUES[[an]]
  m <- lire_srcv(cfg$men)
  men <- tibble(id_men = normaliser_id(m[[cfg$vars[["id"]]]]), poids = num(m[[cfg$vars[["poids"]]]]),
                remb = coalesce(num(m[[cfg$vars[["rembours"]]]]), 0)) %>%
    filter(!is.na(poids), poids > 0, remb <= MENSUALITE_MAX)
  i <- lire_srcv(cfg$ind); names(i) <- toupper(names(i))
  i <- i %>% mutate(id_men = cle_menage_ind(i), rb030 = trimws(as.character(RB030)),
                    anais = num(ANAIS), mere = trimws(as.character(RB230)))
  enfants <- i %>% filter(!is.na(RB230), mere != "")
  fratrie <- enfants %>% count(mere, name = "n_enfants")
  bebes <- enfants %>% filter(anais >= as.integer(an) - 1) %>% count(mere, anais_bebe = anais, name = "n_bebes")
  bebes %>%
    inner_join(i %>% select(mere = rb030, anais_mere = anais, id_men), by = "mere") %>%
    inner_join(fratrie, by = "mere") %>% inner_join(men, by = "id_men") %>%
    transmute(annee = an, age_mere = anais_bebe - anais_mere,
              premier = n_enfants == n_bebes, poids)
}) %>% filter(age_mere >= 15, age_mere <= 49)

stat_age <- function(d) {
  b <- replicate(500, { k <- sample(nrow(d), replace = TRUE); weighted.mean(d$age_mere[k], d$poids[k]) })
  tibble(n = nrow(d), moyenne = weighted.mean(d$age_mere, d$poids),
         ic_bas = quantile(b, 0.025), ic_haut = quantile(b, 0.975))
}
groupes <- naissances %>%
  mutate(periode = case_when(annee == "2014" ~ "2014",
                             annee %in% c("2024", "2025") ~ "2024-2025",
                             TRUE ~ NA_character_)) %>% filter(!is.na(periode))
for (rang in c("Toutes les naissances", "Premieres naissances")) {
  g <- if (rang == "Premieres naissances") filter(groupes, premier) else groupes
  ajouter(g %>% group_by(colonne = periode) %>% group_modify(~ stat_age(.x)) %>% ungroup() %>%
            pivot_longer(-colonne, names_to = "indicateur", values_to = "valeur") %>%
            mutate(cle = rang), "2. Age de la mere a la naissance (ans)")
}
# Contrôle non pondéré, vague par vague (y compris 2022-2023)
ajouter(naissances %>% group_by(colonne = annee) %>%
          summarise(toutes = mean(age_mere), premieres = mean(age_mere[premier]),
                    n_premieres = sum(premier), .groups = "drop") %>%
          pivot_longer(-colonne, names_to = "indicateur", values_to = "valeur") %>%
          mutate(cle = "Non pondere"), "2. Controle par vague")

sortie <- bind_rows(res) %>% mutate(valeur = round(valeur, 2)) %>%
  select(bloc, cle, indicateur, colonne, valeur)
write_csv(sortie, "Output/age_acces_logement.csv")
cat("Exporte : Output/age_acces_logement.csv\n")
