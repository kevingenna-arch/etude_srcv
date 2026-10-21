# ============================================================================
#  SRCV — 34. Vie en couple et naissances, au niveau des individus
# ----------------------------------------------------------------------------
#  Les modèles de l'étude portent sur les couples constitués : ils ne voient
#  pas la marge de la mise en couple. Ce script part des fichiers INDIVIDUS et
#  prend toutes les femmes de 15 à 49 ans, y compris celles qui vivent chez
#  leurs parents ou hors couple.
#
#  - En couple : COUPLE = 1 (en couple avec une personne du logement).
#    COUPLE = 2 (couple non cohabitant) est compté hors couple.
#  - Naissance : la femme est la mère (RB230) d'un enfant du ménage né l'année
#    d'enquête ou la précédente. Même définition à toutes les vagues, ce qui
#    évite la rupture de mesure de 2018-2022 ; RB230 manque en 2018, vague
#    donc exclue des naissances.
#  - Pondération : poids du ménage DB090 (RB050 = DB090 dans les fichiers) ;
#    2022 et 2023 écartées des statistiques pondérées (anomalie de pondération).
#    Ménages à mensualité aberrante écartés (MENSUALITE_MAX).
#  - Décomposition (Kitagawa) de l'écart de taux de naissance entre 2014 et
#    2024-2025 : structure par âge, vie en couple à âge donné, natalité à âge
#    et statut de couple donnés. Variante par taille d'unité urbaine.
#  Sortie : Output/couple_naissances_individus.csv
# ============================================================================

source("R/00_utils.R")
source("R/00_config.R")
suppressPackageStartupMessages(library(tidyverse))

AGES <- c(14, 24, 29, 34, 39, 49)
LIB_AGES <- c("15-24", "25-29", "30-34", "35-39", "40-49")

charger_femmes <- function(an) {
  cfg <- VAGUES[[an]]
  v <- cfg$vars
  m <- lire_srcv(cfg$men)
  men <- tibble(id_men  = normaliser_id(m[[v[["id"]]]]),
                poids   = num(m[[v[["poids"]]]]),
                remb    = coalesce(num(m[[v[["rembours"]]]]), 0),
                densite = recode_densite(m[[v[["tuu"]]]], cfg$tuu_type)) %>%
    filter(!is.na(poids), poids > 0, remb <= MENSUALITE_MAX)

  i <- lire_srcv(cfg$ind)
  names(i) <- toupper(names(i))
  i <- i %>% mutate(id_men = cle_menage_ind(i), rb030 = trimws(as.character(RB030)),
                    age = ages_individus(i, an), anais = num(ANAIS),
                    sexe = trimws(as.character(SEXE)), couple = num(COUPLE),
                    lienpref = trimws(as.character(LIENPREF)),
                    mere_ici = num(MER1E) == 1, pere_ici = num(PER1E) == 1)
  meres <- if ("RB230" %in% names(i))
    i %>% filter(anais >= as.integer(an) - 1, !is.na(RB230)) %>%
      transmute(rb030 = trimws(as.character(RB230))) %>% distinct() %>% mutate(naiss = 1L)
  else NULL

  f <- i %>% filter(sexe == "2", age >= 15, age <= 49) %>%
    inner_join(men, by = "id_men") %>%
    mutate(annee = an,
           en_couple = coalesce(couple == 1, FALSE),
           chez_parents = !lienpref %in% c("00", "01") & (coalesce(mere_ici, FALSE) | coalesce(pere_ici, FALSE)),
           age_cl = cut(age, AGES, labels = LIB_AGES))
  if (is.null(meres)) f$naiss <- NA_integer_
  else f <- f %>% left_join(meres, by = "rb030") %>% mutate(naiss = coalesce(naiss, 0L))
  f %>% select(annee, id_men, age, age_cl, en_couple, chez_parents, naiss, densite, poids)
}

f <- map_dfr(c("2014", "2018", "2022", "2023", "2024", "2025"), charger_femmes)
cat("Femmes 15-49 ans :", nrow(f), "| naissances (N ou N-1) :", sum(f$naiss, na.rm = TRUE), "\n")

fp <- f %>% filter(!annee %in% c("2022", "2023")) %>%
  mutate(periode = if_else(annee %in% c("2024", "2025"), "2024-2025", annee))
wm <- function(x, w) 100 * sum(x * w) / sum(w)
res <- list()
ajouter <- function(x, bloc) res[[length(res) + 1]] <<- mutate(x, bloc = bloc, .before = 1)

# ── 1. Vie en couple et cohabitation parentale ──────────────────────────────
ajouter(fp %>% group_by(annee) %>%
          summarise(n = n(), en_couple = wm(en_couple, poids), chez_parents = wm(chez_parents, poids),
                    femmes_M = sum(poids) / 1e6, .groups = "drop") %>%
          pivot_longer(-annee, names_to = "indicateur", values_to = "valeur") %>%
          rename(colonne = annee) %>% mutate(cle = "Ensemble"), "1. Femmes 15-49 ans")
ajouter(fp %>% group_by(annee, cle = as.character(age_cl)) %>%
          summarise(en_couple = wm(en_couple, poids), chez_parents = wm(chez_parents, poids),
                    .groups = "drop") %>%
          pivot_longer(c(en_couple, chez_parents), names_to = "indicateur", values_to = "valeur") %>%
          rename(colonne = annee), "1. Par classe d'age")
ajouter(fp %>% filter(!is.na(densite)) %>% group_by(annee, cle = as.character(densite)) %>%
          summarise(en_couple = wm(en_couple, poids), .groups = "drop") %>%
          mutate(indicateur = "en_couple") %>% rename(colonne = annee, valeur = en_couple),
        "1. Par taille d'unite urbaine")

# ── 2. Taux de naissance (deux années de naissances), 2014 et 2024-2025 ─────
fn <- fp %>% filter(periode %in% c("2014", "2024-2025"))
ajouter(fn %>% group_by(colonne = periode) %>%
          summarise(n = n(), naissances = sum(naiss), taux = wm(naiss, poids), .groups = "drop") %>%
          pivot_longer(c(n, naissances, taux), names_to = "indicateur", values_to = "valeur") %>%
          mutate(cle = "Ensemble"), "2. Taux de naissance (%)")
ajouter(fn %>% group_by(colonne = periode, cle = if_else(en_couple, "En couple", "Hors couple")) %>%
          summarise(taux = wm(naiss, poids), part_naissances = sum(naiss * poids), .groups = "drop") %>%
          group_by(colonne) %>% mutate(part_naissances = 100 * part_naissances / sum(part_naissances)) %>%
          ungroup() %>% pivot_longer(c(taux, part_naissances), names_to = "indicateur", values_to = "valeur"),
        "2. Selon la vie en couple")

# ── 3. Décomposition de Kitagawa 2014 -> 2024-2025 ──────────────────────────
#  Cellules âge x statut de couple. Écart = effet de composition + effet de
#  taux ; la composition est scindée en structure par âge et vie en couple à
#  âge donné.
kitagawa <- function(d, groupe) {
  cel <- d %>% group_by(periode, age_cl, g = {{ groupe }}) %>%
    summarise(w = sum(poids), b = sum(naiss * poids), .groups = "drop") %>%
    group_by(periode) %>% mutate(s = w / sum(w), r = b / w) %>% ungroup() %>%
    select(periode, age_cl, g, s, r) %>%
    pivot_wider(names_from = periode, values_from = c(s, r), values_fill = 0)
  names(cel) <- sub("2024-2025", "B", sub("2014", "A", names(cel)))
  rA <- sum(cel$s_A * cel$r_A); rB <- sum(cel$s_B * cel$r_B)
  compo <- sum((cel$s_B - cel$s_A) * (cel$r_A + cel$r_B) / 2)
  taux  <- sum((cel$r_B - cel$r_A) * (cel$s_A + cel$s_B) / 2)
  ag <- cel %>% group_by(age_cl) %>%
    summarise(aA = sum(s_A), aB = sum(s_B),
              RA = sum(s_A * r_A) / sum(s_A), RB = sum(s_B * r_B) / sum(s_B), .groups = "drop")
  age <- sum((ag$aB - ag$aA) * (ag$RA + ag$RB) / 2)
  tibble(indicateur = c("Taux 2014 (%)", "Taux 2024-2025 (%)", "Ecart relatif (%)",
                        "Part : structure par age (%)", "Part : groupe a age donne (%)",
                        "Part : natalite a age et groupe donnes (%)"),
         valeur = c(100 * rA, 100 * rB, 100 * (rB / rA - 1),
                    100 * age / (rB - rA), 100 * (compo - age) / (rB - rA), 100 * taux / (rB - rA)))
}
#  Effet de composition à taux fixés : variation relative du taux agrégé que
#  produirait le seul passage des structures 2014 aux structures 2024-2025,
#  les taux par cellule restant ceux d'une période. Ne dépend pas du niveau
#  des taux mesurés, que la refonte de l'échantillon en 2022 (fichiers FPR)
#  semble abaisser : le taux mesuré baisse bien plus que l'ICF entre 2014 et
#  2024-2025, mais à peu près comme lui entre 2022 et 2025.
composition_fixe <- function(d, groupe) {
  cel <- d %>% group_by(periode, age_cl, g = {{ groupe }}) %>%
    summarise(w = sum(poids), b = sum(naiss * poids), .groups = "drop") %>%
    group_by(periode) %>% mutate(s = w / sum(w), r = b / w) %>% ungroup()
  sA <- cel %>% filter(periode == "2014") %>% select(age_cl, g, sA = s)
  sB <- cel %>% filter(periode == "2024-2025") %>% select(age_cl, g, sB = s)
  aB <- sB %>% group_by(age_cl) %>% summarise(aB = sum(sB), .groups = "drop")
  map_dfr(c("2014", "2024-2025"), function(per) {
    r <- cel %>% filter(periode == per) %>% select(age_cl, g, r)
    x <- sA %>% full_join(sB, by = c("age_cl", "g")) %>% left_join(r, by = c("age_cl", "g")) %>%
      left_join(aB, by = "age_cl") %>% mutate(across(c(sA, sB, r), ~ coalesce(.x, 0)))
    # Structure du groupe à âge donné de 2024-2025, âges de 2014 -> isole le groupe
    x <- x %>% group_by(age_cl) %>% mutate(aA = sum(sA), qA = sA / aA, qB = sB / aB) %>% ungroup()
    ref <- sum(x$sA * x$r)
    tibble(indicateur = c(paste0("Composition totale, taux ", per, " (% du taux)"),
                          paste0("Composition par le groupe a age donne, taux ", per, " (% du taux)"),
                          paste0("Idem, en part de la baisse de l'ICF (%)  [taux ", per, "]")),
           valeur = c(100 * (sum(x$sB * x$r) / ref - 1),
                      100 * (sum(x$aA * x$qB * x$r) / ref - 1),
                      100 * (sum(x$aA * x$qB * x$r) / ref - 1) / (ICF_2025 / ICF_2014 - 1)))
  })
}
ajouter(composition_fixe(fn, if_else(en_couple, "En couple", "Hors couple")) %>%
          mutate(cle = "Groupe = vie en couple", colonne = "2014 -> 2024-2025"), "4. Composition a taux fixes")
ajouter(composition_fixe(fn %>% filter(!is.na(densite)), as.character(densite)) %>%
          mutate(cle = "Groupe = taille d'unite urbaine", colonne = "2014 -> 2024-2025"), "4. Composition a taux fixes")

ajouter(kitagawa(fn, if_else(en_couple, "En couple", "Hors couple")) %>%
          mutate(cle = "Groupe = vie en couple", colonne = "2014 -> 2024-2025"), "3. Decomposition")
ajouter(kitagawa(fn %>% filter(!is.na(densite)), as.character(densite)) %>%
          mutate(cle = "Groupe = taille d'unite urbaine", colonne = "2014 -> 2024-2025"), "3. Decomposition")

# Contrôle de la mesure : taux non pondéré par vague, toutes vagues (la
# pondération de 2022-2023 est faussée, pas les comptages).
ajouter(f %>% filter(annee != "2018") %>% group_by(colonne = annee) %>%
          summarise(valeur = 100 * mean(naiss), .groups = "drop") %>%
          mutate(cle = "Ensemble", indicateur = "taux non pondere"), "5. Controle de la mesure")
ajouter(f %>% group_by(colonne = annee) %>%
          summarise(valeur = 100 * mean(en_couple), .groups = "drop") %>%
          mutate(cle = "Ensemble", indicateur = "en couple, non pondere"), "5. Controle de la mesure")

sortie <- bind_rows(res) %>% mutate(valeur = round(valeur, 2)) %>%
  select(bloc, cle, indicateur, colonne, valeur)
print(as.data.frame(sortie), row.names = FALSE)
cat("\nPour comparaison : ICF 2014 =", ICF_2014, ", 2025 =", ICF_2025,
    sprintf("(%.1f %%)\n", 100 * (ICF_2025 / ICF_2014 - 1)))
write_csv(sortie, "Output/couple_naissances_individus.csv")
cat("Exporte : Output/couple_naissances_individus.csv\n")
