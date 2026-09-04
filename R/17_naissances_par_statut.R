# ============================================================================
#  SRCV — Proportion de naissances par STATUT D'OCCUPATION et par millésime
# ----------------------------------------------------------------------------
#  Tableau principal : RÉPARTITION DES NAISSANCES entre les trois statuts —
#  la somme des trois fait 100 % pour chaque millésime. Entre parenthèses, la
#  part du statut dans la POPULATION du champ (elle somme aussi à 100 %), qui
#  sert de référence : un statut est sur-représenté parmi les naissances quand
#  sa part de naissances dépasse sa part de population.
#
#  Le taux de naissance À L'INTÉRIEUR de chaque statut (qui, lui, ne somme pas
#  à 100 %) est conservé en rappel : c'est la mesure de propension, l'autre est
#  une mesure de composition. Les deux répondent à des questions différentes.
#
#  Tout est PONDÉRÉ par les poids de sondage ; la version non pondérée n'est
#  affichée que comme garde-fou.
#
#  DEUX CHAMPS, parce que "restreint aux 15-49 ans" peut se lire de deux façons :
#   A. COUPLES dont la femme a 15-49 ans -> champ des modèles du projet
#      (RESTREINDRE_FECONDITE dans 00_prepa_fecondite.R).
#   B. TOUS les ménages comportant une femme de 15-49 ans (PR ou conjointe),
#      couples et familles monoparentales confondus.
#
#  ⚠️ DÉFINITION DE LA NAISSANCE — EVENEMEN_C (naissance dans l'année) pour
#     2006-2018, contre "enfant né en N ou N-1" reconstruit pour les fichiers
#     FPR (2022-2025), soit une fenêtre d'environ deux ans. Les NIVEAUX ne sont
#     donc PAS comparables de part et d'autre de 2018 ; seules les évolutions
#     à l'intérieur de chaque bloc le sont, et les écarts ENTRE STATUTS d'une
#     même colonne le sont aussi.
#
#  Tout est pondéré (poids de sondage de la vague).
#
#  Sortie : Output/naissances_par_statut.csv
# ============================================================================

source("R/00_prepa_fecondite.R")

AGE_BAS <- 15
AGE_HAUT <- 49

# Un seul chargement : le champ le plus large, filtré ensuite.
base <- bind_rows(charger_millesimes(restreindre = FALSE)) %>%
  filter(!is.na(age_femme), age_femme >= AGE_BAS, age_femme <= AGE_HAUT,
         !is.na(statut3), !is.na(naissance), !is.na(poids), poids > 0)

STATUTS <- c(Locataire                 = "Locataires",
             Proprietaire_accedant     = "Propriétaires accédants",
             Proprietaire_non_accedant = "Propriétaires non accédants")

# Deux grandeurs à ne pas confondre :
#   - part_naiss_pct : RÉPARTITION des naissances entre les 3 statuts. Somme
#     des 3 = 100 % pour un millésime donné. C'est la composition des naissances.
#   - part_pop_pct   : poids démographique du statut dans le champ. Somme = 100 %
#     aussi. Sert de référence : un statut sur-représenté parmi les naissances
#     est celui dont la part de naissances dépasse sa part de population.
#   - taux_pct       : taux de naissance À L'INTÉRIEUR du statut (ne somme pas
#     à 100 %). Conservé car c'est la mesure de propension individuelle.
tableau <- function(d, libelle) {
  res <- d %>%
    group_by(annee, statut3) %>%
    summarise(n = n(),
              w         = sum(poids),
              naiss_n   = sum(naissance == "Oui"),
              w_naiss   = sum(poids[naissance == "Oui"]),
              taux_pct  = 100 * weighted.mean(naissance == "Oui", poids),
              # non pondéré : garde-fou. Un écart important avec la version
              # pondérée signale que le résultat tient à la structure des poids
              # et non aux données brutes.
              taux_pct_brut = 100 * mean(naissance == "Oui"),
              .groups = "drop") %>%
    group_by(annee) %>%
    mutate(part_pop_pct        = 100 * w / sum(w),
           part_naiss_pct      = 100 * w_naiss / sum(w_naiss),
           part_naiss_pct_brut = 100 * naiss_n / sum(naiss_n)) %>%
    ungroup() %>%
    mutate(champ = libelle,
           statut = unname(STATUTS[as.character(statut3)]))

  cat("\n##########", libelle, "##########\n")
  cat("PART DES NAISSANCES par statut (colonne = 100 %), et entre parenthèses",
      "la part du statut dans la population du champ\n\n")
  print(as.data.frame(res %>%
    transmute(annee, statut,
              cellule = sprintf("%.1f (%.1f)", part_naiss_pct, part_pop_pct)) %>%
    pivot_wider(names_from = annee, values_from = cellule)), row.names = FALSE)

  cat("\nContrôle — part des naissances NON pondérée (naissances / total) :\n")
  print(as.data.frame(res %>%
    transmute(annee, statut,
              brut = sprintf("%.1f (%d)", part_naiss_pct_brut, naiss_n)) %>%
    pivot_wider(names_from = annee, values_from = brut)), row.names = FALSE)

  cat("\nRappel — taux de naissance à l'intérieur de chaque statut (ne somme pas à 100) :\n")
  print(as.data.frame(res %>%
    transmute(annee, statut, taux = sprintf("%.1f", taux_pct)) %>%
    pivot_wider(names_from = annee, values_from = taux)), row.names = FALSE)
  res
}

champ_a <- base %>% filter(couple == "En_couple")
champ_b <- base

res <- bind_rows(
  tableau(champ_a, paste0("A. COUPLES, femme de ", AGE_BAS, " a ", AGE_HAUT, " ans")),
  tableau(champ_b, paste0("B. TOUS MENAGES avec une femme de ", AGE_BAS,
                          " a ", AGE_HAUT, " ans"))
)

if (!dir.exists("Output")) dir.create("Output")
write_csv(res %>% select(champ, annee, statut, n, naiss_n, part_naiss_pct, part_naiss_pct_brut, part_pop_pct, taux_pct, taux_pct_brut),
          "Output/naissances_par_statut.csv")
cat("\nExporté : Output/naissances_par_statut.csv\n")
cat("\n⚠️ Rupture de définition de la naissance entre 2018 et 2022 :",
    "ne pas comparer les niveaux de part et d'autre.\n")
