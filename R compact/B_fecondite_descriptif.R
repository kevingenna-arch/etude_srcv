# ============================================================================
#  SRCV — B. FÉCONDITÉ : descriptif, cartes et nuages
# ----------------------------------------------------------------------------
#  Regroupe R/10_acm_naissance, R/17_naissances_par_statut, R/18_reponderation_rb050, R/20_heatmap_naissances, R/21_nuage_naissances, R/22_nuage_naissances_par_revenu.
#
#  POURQUOI CE REGROUPEMENT : ces scripts repartaient chacun de la préparation
#  des 8 vagues (~7 s) et du calcul de l'âge des parents (~5 s). Ici le socle
#  est sourcé UNE fois et activer_cache_prepa() mémoïse preparer_donnees(),
#  charger_millesimes() et age_moyen_menage() sur disque : la première
#  exécution paie le coût, les suivantes sont quasi instantanées.
#
#  Le code des sections est celui des scripts d'origine, inchangé. Chaque
#  section redéfinit ses propres objets ; l'exécution étant séquentielle, il
#  n'y a pas d'interférence entre elles.
#
#  À LANCER DEPUIS LA RACINE DU PROJET :
#     source("R compact/B_fecondite_descriptif.R")
# ============================================================================

source("R/00_prepa_fecondite.R")
activer_cache_prepa()


# ==========================================================================
#  ACM : profils des couples avec naissance
#  (ex-R/10_acm_naissance.R)
# ==========================================================================

# ============================================================================
#  SRCV — ACM : profil des couples connaissant une NAISSANCE
# ----------------------------------------------------------------------------
#  Analyse des Correspondances Multiples sur les caractéristiques structurelles
#  des couples en âge de procréer, avec la NAISSANCE projetée en variable
#  SUPPLÉMENTAIRE (illustrative) : elle ne participe pas à la construction des
#  axes, on regarde seulement où elle se positionne dans l'espace des profils.
#  C'est le bon usage ici — mettre la naissance en variable active reviendrait
#  à laisser l'issue structurer la typologie qu'on veut utiliser pour la décrire.
#
#  Variables ACTIVES (toutes qualitatives) :
#    statut d'occupation (3 postes), degré d'urbanisation, diplôme de la PR,
#    quintile de niveau de vie, classe de surface par personne, taux d'effort
#    en classes, parité pré-naissance, âge de la femme, activité du couple.
#  Variables SUPPLÉMENTAIRES : naissance, millésime.
#
#  Pondération : les poids de sondage sont passés à MCA() via row.w.
#  Champ : couples dont la femme a 15-49 ans, 8 millésimes.
# ============================================================================



library(FactoMineR)

# Millésimes retenus. Restreindre aux vagues FPR (2022-2025) donne une
# nomenclature homogène (diplôme, statut, naissance reconstruite de la même
# façon) et évite l'effet de période observé sur 2006-2025.
VAGUES_ACM <- c("2022", "2023", "2024", "2025")

# ── Préparation : reprend la logique de 02 (coût = charges + emprunt) ───────
charger <- function(annee, cfg) {
  v <- cfg$vars
  # Naissance : EVENEMEN_C ou reconstruction FPR — logique commune (00_utils.R)
  raw <- ajouter_naissance(lire_srcv(cfg$men), cfg)

  tibble(annee = annee,
         y_birth = raw$y_birth,
         cout   = num(raw[[v[["cout_log"]]]]) + coalesce(num(raw[[v[["rembours"]]]]), 0),
         revenu = num(raw[[v[["revenu"]]]]),
         uc     = num(raw$HX050),
         surface= num(raw[[v[["surface"]]]]),
         taille = num(raw[[v[["taille"]]]]),
         nb_enf = num(raw[[v[["nb_enf"]]]]),
         remb   = coalesce(num(raw[[v[["rembours"]]]]), 0),
         # statut recodé ICI : recode_occ() attend un occ_type scalaire,
         # propre à chaque vague (hx070 avant 2018, stoc ensuite).
         statut3 = recode_statut3(recode_occ(raw[[v[["occ"]]]], cfg$occ_type),
                                  coalesce(num(raw[[v[["rembours"]]]]), 0)),
         urbain = raw[[v[["urbain"]]]],
         dipl   = trimws(as.character(raw[[v[["diplome"]]]])),
         nactocc= num(raw$NACTOCCUP),
         couple = raw[[v[["couple"]]]],
         age_pr = num(raw[[v[["age_pr"]]]]), age_cj = num(raw[[v[["age_cj"]]]]),
         sx_pr  = trimws(as.character(raw[[v[["sexe_pr"]]]])),
         sx_cj  = trimws(as.character(raw[[v[["sexe_cj"]]]])),
         poids  = num(raw[[v[["poids"]]]]))
}
d <- imap_dfr(VAGUES[VAGUES_ACM], ~ charger(.y, .x))

d <- d %>%
  mutate(couple_f = recode_couple(couple),
         age_femme = case_when(sx_pr == "2" ~ age_pr, sx_cj == "2" ~ age_cj, TRUE ~ NA_real_),
         surface = if_else(surface >= 9 & surface <= 400, surface, NA_real_),
         niveau_vie = if_else(!is.na(uc) & uc > 0, revenu / uc, revenu),
         effort = if_else(revenu > 0, pmin(pmax(100*(cout*12)/revenu, 0), 100), NA_real_),
         surf_pers = surface / pmax(taille, 1),
         # parité AVANT la naissance (bad control corrigé, cf. 02)
         nb_enf_avant = pmax(nb_enf - y_birth, 0)) %>%
  filter(couple_f == "En_couple", !is.na(age_femme), age_femme >= 15, age_femme <= 49,
         !is.na(poids), poids > 0, !is.na(revenu), revenu > 0,
         !is.na(surface), !is.na(statut3), !is.na(effort), !is.na(niveau_vie))

# ── Recodage en variables qualitatives ──────────────────────────────────────
BORNES <- list()
qcut <- function(x, w, k, labs, nom = "") {
  seuils <- wquantile(x, w, seq_len(k - 1) / k)      # 00_utils.R
  BORNES[[nom]] <<- round(seuils, 1)
  cut(x, unique(c(-Inf, seuils, Inf)), labels = labs, include.lowest = TRUE)
}
# ⚠️ Le poids de sondage est transporté AVEC les lignes (.poids) : il doit
# survivre au filtre des valeurs manquantes. Sans cela, row.w reçoit les
# premiers poids de `d` et non ceux des lignes conservées — les deux vecteurs
# ne coïncident pas (171 lignes écartées, dispersées dans le fichier).
acm_tout <- d %>% transmute(
  Statut   = statut3,
  Urbain   = recode_urbain(urbain),
  # Diplôme harmonisé sur le PREMIER CHIFFRE du code : les deux nomenclatures
  # (DIP14PR avant 2014, DIPDETPR ensuite) sont hiérarchiques et concordantes à
  # ce niveau (1x-3x supérieur, 4x bac, 5x CAP-BEP, 6x+ infra). Harmonisation
  # APPROCHÉE : la distribution est contrôlée par vague ci-dessous.
  Diplome  = factor(case_when(substr(dipl,1,1) %in% c("1","2","3") ~ "Dipl:Superieur",
                              substr(dipl,1,1) == "4"              ~ "Dipl:Bac",
                              substr(dipl,1,1) == "5"              ~ "Dipl:CAP-BEP",
                              substr(dipl,1,1) %in% c("6","7","8") ~ "Dipl:Infra",
                              TRUE                                 ~ NA_character_)),
  NivVie   = qcut(niveau_vie, poids, 4, c("NV:Q1","NV:Q2","NV:Q3","NV:Q4"), "Niveau de vie (EUR/UC)"),
  SurfPers = qcut(surf_pers, poids, 4, c("Surf:Q1","Surf:Q2","Surf:Q3","Surf:Q4"), "Surface par personne (m2)"),
  Effort   = qcut(effort, poids, 4, c("Eff:Q1","Eff:Q2","Eff:Q3","Eff:Q4"), "Taux d effort logement (%)"),
  Parite   = factor(case_when(nb_enf_avant == 0 ~ "Par:0", nb_enf_avant == 1 ~ "Par:1",
                              nb_enf_avant == 2 ~ "Par:2", TRUE ~ "Par:3+")),
  AgeFemme = cut(age_femme, c(14,24,29,34,39,49),
                 labels = c("Age:15-24","Age:25-29","Age:30-34","Age:35-39","Age:40-49")),
  Activite = factor(case_when(nactocc >= 2 ~ "Act:2 actifs", nactocc == 1 ~ "Act:1 actif",
                              TRUE ~ "Act:0 actif")),
  # supplémentaires
  Naissance = factor(if_else(y_birth == 1, "NAISSANCE", "pas de naissance")),
  Millesime = factor(annee),
  .poids    = poids)                       # embarqué, retiré juste avant MCA()

garde <- complete.cases(select(acm_tout, -.poids))
acm   <- select(acm_tout[garde, ], -.poids)
w     <- acm_tout$.poids[garde]

cat("
=== Bornes des quartiles (seuils Q1|Q2|Q3|Q4) ===
")
for (nm in names(BORNES)) cat(sprintf("  %-28s : %s
", nm, paste(BORNES[[nm]], collapse = " | ")))
cat("
Couples retenus :", nrow(acm), "| dont naissance :", sum(acm$Naissance == "NAISSANCE"), "\n")
stopifnot("Poids et lignes désalignés" = length(w) == nrow(acm))

res <- MCA(as.data.frame(acm), quali.sup = c(10, 11), row.w = w, graph = FALSE, ncp = 5)

cat("\n=== Valeurs propres (% de variance) ===\n")
print(round(res$eig[1:5, ], 2))

cat("\n=== Modalités les plus contributives — axe 1 ===\n")
c1 <- res$var$contrib[, 1]; co1 <- res$var$coord[, 1]
print(head(data.frame(contrib = round(sort(c1, decreasing = TRUE), 1),
                      coord = round(co1[order(c1, decreasing = TRUE)], 2)), 12))
cat("\n=== Modalités les plus contributives — axe 2 ===\n")
c2 <- res$var$contrib[, 2]; co2 <- res$var$coord[, 2]
print(head(data.frame(contrib = round(sort(c2, decreasing = TRUE), 1),
                      coord = round(co2[order(c2, decreasing = TRUE)], 2)), 12))

cat("\n=== Position des variables SUPPLÉMENTAIRES (coordonnées axes 1-3) ===\n")
print(round(res$quali.sup$coord[, 1:3], 3))
cat("\n=== v.test des supplémentaires (|v|>2 = position significative) ===\n")
print(round(res$quali.sup$v.test[, 1:3], 2))

if (!dir.exists("Output")) dir.create("Output")
# choix="ind" + invisible="ind" : carte des MODALITÉS (choix="var" n'affiche
# que les rapports de corrélation des variables, pas leur position).
png("Output/acm_naissance.png", width = 1300, height = 1000, res = 110)
print(plot(res, choix = "ind", invisible = "ind", autoLab = "yes", cex = 0.75,
     col.var = "grey35", col.quali.sup = "red",
     title = "ACM - profils des couples (naissance projetee en supplementaire)"))
dev.off()
write_csv(as.data.frame(res$var$coord) %>% rownames_to_column("modalite"),
          "Output/acm_coordonnees.csv")
cat("\nExporté : Output/acm_naissance.png + acm_coordonnees.csv\n")


# ==========================================================================
#  Naissances par statut d'occupation
#  (ex-R/17_naissances_par_statut.R)
# ==========================================================================

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


# ==========================================================================
#  Tests sur l'anomalie de pondération
#  (ex-R/18_reponderation_rb050.R)
# ==========================================================================

# ============================================================================
#  SRCV — Deux tests sur l'anomalie de pondération des vagues 2022-2023
# ----------------------------------------------------------------------------
#  CONSTAT (cf. 17) : le taux de naissance pondéré par DB090 s'effondre en
#  2022-2023 puis rebondit en 2024, alors que le taux brut est régulier. En
#  cause : les ménages avec un nouveau-né portent en 2023 un poids médian de
#  392 contre 1 727 pour les autres. Hypothèse : DB090 est dérivé des poids
#  individuels des membres, et le nouveau-né — absent de l'échantillon initial
#  — tire le poids du ménage vers le bas.
#
#  PISTE 1 — repondérer par le POIDS INDIVIDUEL DE LA FEMME (RB050) au lieu du
#  poids du ménage. C'est aussi plus cohérent avec l'objet mesuré : « cette
#  femme a-t-elle eu un enfant » est un événement individuel. Le poids de la
#  mère, lui, ne peut pas être contaminé par l'arrivée du nouveau-né.
#    -> si la série se lisse, l'hypothèse est confirmée.
#
#  PISTE 3 — se limiter aux comparaisons ENTRE STATUTS à l'intérieur d'une même
#  vague, en retirant 2023 et 2024. La distorsion touche les trois statuts de
#  la même façon dans une vague donnée et s'annule donc en partie.
#
#  ⚠️ RB050 est ABSENT du fichier individus 2018 : cette vague ne peut pas être
#     repondérée et sort de la piste 1.
#
#  Sortie : Output/reponderation_rb050.csv
# ============================================================================



AGE_BAS <- 15; AGE_HAUT <- 49
ANNEES_PISTE3 <- c("2006", "2010", "2014", "2018", "2022", "2025")   # 2023-2024 retirées

STATUTS <- c(Locataire                 = "Locataires",
             Proprietaire_accedant     = "Propriétaires accédants",
             Proprietaire_non_accedant = "Propriétaires non accédants")

# ── Poids individuel de la femme du ménage, par vague ───────────────────────
#  Clé ménage : RB040 quand il existe, sinon dérivée de RB030 (= identifiant
#  ménage + numéro de personne sur 2 chiffres, avec zéros de tête). Le taux
#  d'appariement est contrôlé et affiché : sous 90 %, la vague est écartée.
poids_femme <- function(annee, cfg) {
  ind <- lire_srcv(cfg$ind)
  if (!"RB050" %in% names(ind)) {
    message("  ", annee, " : RB050 absent -> vague écartée de la piste 1")
    return(NULL)
  }
  cle <- if ("RB040" %in% names(ind)) {
    trimws(as.character(ind$RB040))
  } else {
    r <- trimws(as.character(ind$RB030))
    sub("^0+", "", substr(r, 1, nchar(r) - 2))     # retire le n° de personne
  }
  sexe  <- trimws(as.character(ind$SEXE))
  lien  <- trimws(as.character(ind$LIENPREF))
  age_v <- if ("AGE" %in% names(ind)) num(ind$AGE) else
           if ("age" %in% names(ind)) num(ind$age) else
           as.integer(annee) - num(ind[[if ("ANAIS" %in% names(ind)) "ANAIS" else "anais"]])

  tibble(id = cle, sexe, lien, age_v, w = num(ind$RB050)) %>%
    filter(sexe == "2", lien %in% c("00", "01", "0", "1"),
           !is.na(age_v), age_v >= AGE_BAS, age_v <= AGE_HAUT,
           !is.na(w), w > 0) %>%
    group_by(id) %>% summarise(poids_f = max(w), .groups = "drop")
}

base <- bind_rows(charger_millesimes(restreindre = FALSE)) %>%
  filter(!is.na(age_femme), age_femme >= AGE_BAS, age_femme <= AGE_HAUT,
         !is.na(statut3), !is.na(naissance), !is.na(poids), poids > 0,
         couple == "En_couple")

cat("=== Appariement du poids individuel de la femme (RB050) ===\n")
pf <- imap_dfr(VAGUES, function(cfg, an) {
  p <- poids_femme(an, cfg); if (is.null(p)) return(NULL)
  p %>% mutate(annee = an)
})
base <- base %>% mutate(id = normaliser_id(id)) %>% left_join(pf, by = c("annee", "id"))
print(as.data.frame(base %>% group_by(annee) %>%
  summarise(n = n(), apparies_pct = round(100 * mean(!is.na(poids_f)), 1),
            .groups = "drop")), row.names = FALSE)

# Vagues exploitables pour la piste 1
ok <- base %>% group_by(annee) %>% summarise(t = mean(!is.na(poids_f)), .groups = "drop") %>%
  filter(t >= 0.90) %>% pull(annee)
cat("Vagues retenues pour la piste 1 :", paste(ok, collapse = ", "), "\n")

# ── Tableaux ────────────────────────────────────────────────────────────────
resume <- function(d, w_col, libelle) {
  d %>% filter(!is.na(.data[[w_col]]), .data[[w_col]] > 0) %>%
    group_by(annee, statut3) %>%
    summarise(n = n(), naiss_n = sum(naissance == "Oui"),
              taux_pct = 100 * weighted.mean(naissance == "Oui", .data[[w_col]]),
              w_naiss = sum(.data[[w_col]][naissance == "Oui"]),
              w_tot   = sum(.data[[w_col]]), .groups = "drop") %>%
    group_by(annee) %>%
    mutate(part_naiss_pct = 100 * w_naiss / sum(w_naiss),
           part_pop_pct   = 100 * w_tot / sum(w_tot)) %>%
    ungroup() %>%
    mutate(ponderation = libelle, statut = unname(STATUTS[as.character(statut3)]))
}

affiche <- function(r, var, titre) {
  cat("\n--", titre, "--\n")
  print(as.data.frame(r %>%
    transmute(annee, statut, v = sprintf("%.1f", .data[[var]])) %>%
    pivot_wider(names_from = annee, values_from = v)), row.names = FALSE)
}

r_db090 <- resume(base, "poids",   "DB090 (ménage)")
r_rb050 <- resume(filter(base, annee %in% ok), "poids_f", "RB050 (femme)")
r_brut  <- base %>% mutate(un = 1) %>% resume("un", "aucune (brut)")

cat("\n\n########## PISTE 1 — REPONDERATION PAR RB050 ##########\n")
affiche(r_db090, "taux_pct", "Taux de naissance, pondéré par DB090 (ménage)")
affiche(r_rb050, "taux_pct", "Taux de naissance, pondéré par RB050 (femme)")
affiche(r_brut,  "taux_pct", "Taux de naissance, NON pondéré (référence)")

affiche(r_db090, "part_naiss_pct", "Part des naissances (somme = 100), DB090")
affiche(r_rb050, "part_naiss_pct", "Part des naissances (somme = 100), RB050")

cat("\nRapport poids moyen (naissance / pas de naissance) — le signal d'alerte :\n")
print(as.data.frame(base %>% group_by(annee) %>%
  summarise(DB090 = round(mean(poids[naissance == "Oui"]) /
                          mean(poids[naissance == "Non"]), 3),
            RB050 = if (first(annee) %in% ok)
              round(mean(poids_f[naissance == "Oui"], na.rm = TRUE) /
                    mean(poids_f[naissance == "Non"], na.rm = TRUE), 3) else NA_real_,
            .groups = "drop")), row.names = FALSE)

cat("\n\n########## PISTE 3 — SANS 2023 NI 2024 ##########\n")
p3 <- r_db090 %>% filter(annee %in% ANNEES_PISTE3)
affiche(p3, "taux_pct", "Taux de naissance par statut (DB090)")
cat("\nPart des naissances (somme = 100) et part de population :\n")
print(as.data.frame(p3 %>%
  transmute(annee, statut,
            v = sprintf("%.1f (%.1f)", part_naiss_pct, part_pop_pct)) %>%
  pivot_wider(names_from = annee, values_from = v)), row.names = FALSE)

cat("\nÉcart à la parité (part des naissances - part de population, en points) :\n")
print(as.data.frame(p3 %>%
  transmute(annee, statut, v = sprintf("%+.1f", part_naiss_pct - part_pop_pct)) %>%
  pivot_wider(names_from = annee, values_from = v)), row.names = FALSE)

if (!dir.exists("Output")) dir.create("Output")
write_csv(bind_rows(r_db090, r_rb050, r_brut) %>%
            select(ponderation, annee, statut, n, naiss_n, taux_pct,
                   part_naiss_pct, part_pop_pct),
          "Output/reponderation_rb050.csv")
cat("\nExporté : Output/reponderation_rb050.csv\n")


# ==========================================================================
#  Heatmaps des naissances
#  (ex-R/20_heatmap_naissances.R)
# ==========================================================================

# ============================================================================
#  SRCV — Heatmaps des naissances : LOCATAIRES et PROPRIÉTAIRES ACCÉDANTS
# ----------------------------------------------------------------------------
#  Abscisse commune : âge moyen du ou des parents (fichier individus, enfants
#  exclus), en classes de 2 ans entre 25 et 41, avec deux classes de bord.
#  Deux ordonnées :
#    1. niveau de vie par UC, classes régulières de 3 000 EUR constants 2025
#    2. surface du logement, classes régulières de 10 m2
#  Remplissage : nombre de naissances (estimation pondérée, en milliers).
#
#  ⚠️ GRILLE COMMUNE AUX DEUX STATUTS — les bornes hautes sont calées sur les
#     accédants (surface médiane 105 m2 et 9e décile 160, contre 75 et 103 chez
#     les locataires ; niveau de vie médian 28 k contre 19 k). Les cartes des
#     locataires comportent donc des rangées hautes vides : c'est le prix de la
#     comparabilité entre les deux statuts, qui partagent le même repère.
#
#  ⚠️ CLASSES RÉGULIÈRES ET CASES VIDES — les bornes sont à pas constant, ce qui
#     produit des cases vides aux extrémités. C'est voulu : une grille régulière
#     se lit comme une surface de densité, alors qu'une grille à classes
#     inégales déforme les aires. Conséquence : plus d'une centaine de cases,
#     donc les libellés par case sont supprimés au-delà de
#     MAX_CASES_ETIQUETEES et seule la couleur porte l'information.
#
#  ⚠️ EFFECTIFS — une vague seule ne porte qu'une centaine de naissances par
#     statut. Sur une grille fine cela fait environ une naissance par case : la
#     carte annuelle est une esquisse. Le cumul des six vagues saines (758
#     naissances chez les locataires, 880 chez les accédants) est la seule
#     version qui fasse apparaître une structure.
#
#  ⚠️ CHOIX DE 2024 : vague saine du point de vue de la pondération — rapport du
#     poids moyen naissance / non-naissance de 1,183 chez les locataires, dans
#     la norme historique (1,21 à 1,30 de 2006 à 2018). Les vagues déviantes
#     sont 2022 (0,665) et 2023 (0,634).
#
#  Palette : rampe séquentielle bleue à teinte unique, clair vers foncé, comme
#  l'impose l'encodage d'une magnitude.
#
#  Sorties : Output/heatmap_naissances_<ordonnee>_<statut>_<champ>.png (8 PNG)
#            Output/heatmap_naissances.csv
# ============================================================================



ANNEE_SEULE <- "2024"
ANNEES_POOL <- c("2006", "2010", "2014", "2018", "2022", "2025")
AGE_BAS     <- 15
AGE_HAUT    <- 49
MAX_CASES_ETIQUETEES <- 30      # au-delà, plus de chiffres dans les cases

STATUTS     <- c(Locataire = "locataires", Proprietaire_accedant = "accedants")
STATUTS_LIB <- c(Locataire = "locataires",
                 Proprietaire_accedant = "propriétaires accédants")

# ── Classes RÉGULIÈRES, communes aux deux statuts ───────────────────────────
# Âge des parents : pas de 2 ans entre 25 et 41, là où se concentrent les
# naissances, et deux classes de bord larges en dessous et au-dessus — les
# queues sont trop peu peuplées pour justifier un pas fin.
AGE_BRK <- c(-Inf, seq(25, 41, 2), Inf)
AGE_LAB <- c("< 25", paste0(seq(25, 39, 2), "-", seq(26, 40, 2)), "41 +")
# Niveau de vie : pas de 3 000 EUR de 0 à 48 000, plus une classe haute.
NV_BRK <- c(seq(0, 48000, 3000), Inf)
NV_LAB <- c(paste0(seq(0, 45, 3), "-", seq(3, 48, 3)), "48 +")
# Surface : pas de 10 m2 de 20 à 160, avec deux classes de bord.
SURF_BRK <- c(-Inf, seq(20, 160, 10), Inf)
SURF_LAB <- c("< 20", paste0(seq(20, 150, 10), "-", seq(29, 159, 10)), "160 +")

BLEU_CLAIR <- "#cde2fb"; BLEU_FONCE <- "#0d366b"; SURFACE <- "#ffffff"
IPC <- charger_ipc()

charger <- function(ans, statut) {
  ages <- imap_dfr(VAGUES[ans],
                   ~ age_moyen_menage(.x, .y, liens = LIENS_PARENTS) %>% mutate(annee = .y))
  bind_rows(imap(VAGUES[ans], ~ preparer_donnees(.y, .x, restreindre = FALSE))) %>%
    filter(statut3 == statut, !is.na(age_femme),
           age_femme >= AGE_BAS, age_femme <= AGE_HAUT,
           !is.na(naissance), !is.na(poids), poids > 0) %>%
    mutate(id = normaliser_id(id)) %>%
    left_join(ages, by = c("annee", "id")) %>%
    mutate(nv = deflater(niveau_vie, as.integer(annee) - 1, IPC, base = 2025)) %>%
    filter(!is.na(age_moyen)) %>%
    mutate(cl_age = cut(age_moyen, AGE_BRK, labels = AGE_LAB, right = FALSE))
}

# Agrège sur la grille age x <ordonnee>, cases vides conservées
agreger <- function(d, var_y, brk, lab, champ) {
  d %>%
    filter(!is.na(.data[[var_y]])) %>%
    mutate(cl_y = cut(.data[[var_y]], brk, labels = lab, right = FALSE)) %>%
    filter(!is.na(cl_y), naissance == "Oui") %>%
    group_by(cl_age, cl_y) %>%
    summarise(n_ech = n(), naiss_k = sum(poids) / 1000, .groups = "drop") %>%
    complete(cl_age = factor(AGE_LAB, AGE_LAB), cl_y = factor(lab, lab),
             fill = list(n_ech = 0, naiss_k = 0)) %>%
    mutate(champ = champ, ordonnee = var_y)
}

tracer <- function(a, titre, sous_titre, lab_y, source_txt) {
  etiqueter <- nrow(a) <= MAX_CASES_ETIQUETEES
  seuil <- max(a$naiss_k) * 0.58
  g <- ggplot(a, aes(cl_age, cl_y, fill = naiss_k)) +
    geom_tile(colour = SURFACE, linewidth = 1.1)    # surface entre les cases
  if (etiqueter)
    g <- g +
      geom_text(aes(label = ifelse(n_ech == 0, "--", sprintf("%.0f", naiss_k)),
                    colour = naiss_k > seuil),
                size = 4.2, fontface = "bold", vjust = -0.15) +
      geom_text(aes(label = ifelse(n_ech == 0, "", paste0("n = ", n_ech)),
                    colour = naiss_k > seuil), size = 2.9, vjust = 1.5, alpha = 0.85) +
      scale_colour_manual(values = c(`FALSE` = "#1a1a18", `TRUE` = "#ffffff"),
                          guide = "none")
  legende <- if (etiqueter)
    "\nLe grand chiffre est l'estimation pondérée en milliers ; n est l'effectif d'échantillon."
  else
    "\nClasses régulières : les cases vides sont conservées, la couleur seule porte l'information."
  g +
    scale_fill_gradient(low = BLEU_CLAIR, high = BLEU_FONCE,
                        name = "Naissances\n(milliers)") +
    coord_fixed(ratio = 0.75, expand = FALSE) +
    labs(title = titre, subtitle = sous_titre,
         x = "Âge moyen du ou des parents (ans)", y = lab_y,
         caption = paste0("Insee, ", source_txt,
                          " Champ : ménages comportant une femme de ",
                          AGE_BAS, " à ", AGE_HAUT, " ans.", legende)) +
    theme_minimal(base_size = 11) +
    theme(panel.grid = element_blank(), axis.ticks = element_blank(),
          plot.title = element_text(face = "bold"),
          plot.subtitle = element_text(colour = "#57534e", size = 9),
          plot.caption = element_text(size = 7.5, colour = "#78716c", hjust = 0),
          axis.text.x = element_text(angle = 45, hjust = 1),
          axis.text.y = element_text(size = 8),
          legend.key.height = unit(1.5, "cm"))
}

if (!dir.exists("Output")) dir.create("Output")

ORDONNEES <- list(
  list(var = "nv", brk = NV_BRK, lab = NV_LAB, slug = "niveau_vie",
       axe = "Niveau de vie par UC (milliers d'euros constants 2025)"),
  list(var = "surface", brk = SURF_BRK, lab = SURF_LAB, slug = "surface",
       axe = "Surface du logement (m2)")
)

CHAMPS <- list(
  list(ans = ANNEE_SEULE, slug = ANNEE_SEULE, suffixe = ANNEE_SEULE,
       note = "lecture indicative : une centaine de naissances sur une grille fine",
       src  = paste0("SRCV ", ANNEE_SEULE, ".")),
  list(ans = ANNEES_POOL, slug = "poole", suffixe = "6 vagues cumulées",
       note = "les effectifs cumulent six vagues : ce ne sont pas des naissances annuelles",
       src  = paste0("SRCV ", paste(ANNEES_POOL, collapse = ", "),
                     " ; 2023 et 2024 exclus (anomalie de pondération)."))
)

tout <- list()
for (st in names(STATUTS)) {
  for (ch in CHAMPS) {
    d <- charger(ch$ans, st)
    cat("== ", STATUTS[[st]], " / ", ch$slug, " : ", nrow(d), " ménages, ",
        sum(d$naissance == "Oui"), " naissances ==\n", sep = "")
    titre <- paste0("Naissances chez les ", STATUTS_LIB[[st]], ", ", ch$suffixe)
    for (o in ORDONNEES) {
      a <- agreger(d, o$var, o$brk, o$lab, ch$slug) %>% mutate(statut = STATUTS[[st]])
      tout[[length(tout) + 1]] <- a
      f <- paste0("Output/heatmap_naissances_", o$slug, "_", STATUTS[[st]], "_",
                  ch$slug, ".png")
      ggsave(f, tracer(a, titre,
                       paste0(sum(a$n_ech), " naissances observées sur ", nrow(a),
                              " cases — ", ch$note), o$axe, ch$src),
             width = 9.5, height = 9.5, dpi = 200, bg = "white")
      cat("    ", f, " (", nrow(a), " cases)\n", sep = "")
    }
  }
}

write_csv(bind_rows(tout), "Output/heatmap_naissances.csv")
cat("\nExporté : 8 PNG + Output/heatmap_naissances.csv\n")


# ==========================================================================
#  Nuages de points des naissances
#  (ex-R/21_nuage_naissances.R)
# ==========================================================================

# ============================================================================
#  SRCV — Nuages de points des naissances : LOCATAIRES et ACCÉDANTS
# ----------------------------------------------------------------------------
#  Version NON PONDÉRÉE des heatmaps de R/20 : un point = un ménage de
#  l'échantillon ayant connu une naissance récente. Aucun poids de sondage
#  n'intervient, ni dans les points ni dans les médianes tracées.
#
#  Abscisse : âge moyen du ou des parents (fichier individus, enfants exclus).
#  Ordonnées : niveau de vie par UC (euros constants 2025), puis surface.
#
#  ⚠️ NON PONDÉRÉ = NON REPRÉSENTATIF. Le nuage décrit l'ÉCHANTILLON, pas la
#     population : les ménages sur-représentés par le plan de sondage y pèsent
#     autant que les autres. C'est le bon outil pour voir la forme et la
#     dispersion du nuage et pour repérer les points aberrants, pas pour
#     estimer un effectif. Les heatmaps de R/20, elles, sont pondérées.
#
#  ⚠️ Les courbes de niveau sont une densité 2D à noyau : elles aident à lire
#     où se concentre la masse quand les points se superposent. Elles ne sont
#     pas une estimation, seulement un guide de lecture.
#
#  Palette : slots 1 et 2 de la palette catégorielle de référence, dans l'ordre
#  fixe (bleu = locataires, orange = accédants). L'identité n'est jamais portée
#  par la seule couleur : chaque panneau est nommé par son bandeau.
#
#  Sorties : Output/nuage_naissances_<ordonnee>_<champ>.png  (4 fichiers)
#            Output/nuage_naissances.csv
# ============================================================================



ANNEE_SEULE <- "2024"
ANNEES_POOL <- c("2006", "2010", "2014", "2018", "2022", "2025")
AGE_BAS     <- 15
AGE_HAUT    <- 49

STATUTS_LIB <- c(Locataire = "Locataires", Proprietaire_accedant = "Propriétaires accédants")
# Palette catégorielle de référence, ordre fixe : slot 1 puis slot 2.
COULEURS <- c(Locataires = "#2a78d6", `Propriétaires accédants` = "#eb6834")

IPC <- charger_ipc()

charger <- function(ans) {
  ages <- imap_dfr(VAGUES[ans],
                   ~ age_moyen_menage(.x, .y, liens = LIENS_PARENTS) %>% mutate(annee = .y))
  bind_rows(imap(VAGUES[ans], ~ preparer_donnees(.y, .x, restreindre = FALSE))) %>%
    filter(statut3 %in% names(STATUTS_LIB), !is.na(age_femme),
           age_femme >= AGE_BAS, age_femme <= AGE_HAUT,
           naissance == "Oui", !is.na(poids), poids > 0) %>%
    mutate(id = normaliser_id(id)) %>%
    left_join(ages, by = c("annee", "id")) %>%
    filter(!is.na(age_moyen)) %>%
    mutate(nv = deflater(niveau_vie, as.integer(annee) - 1, IPC, base = 2025) / 1000,
           statut = factor(unname(STATUTS_LIB[as.character(statut3)]),
                           levels = unname(STATUTS_LIB)))
}

tracer <- function(d, var_y, lab_y, borne_y, titre, sous_titre, source_txt) {
  d <- d %>% filter(!is.na(.data[[var_y]]))
  hors <- sum(d[[var_y]] > borne_y)      # points au-delà de la borne d'affichage
  med <- d %>% group_by(statut) %>%
    summarise(x = median(age_moyen), y = median(.data[[var_y]]), n = n(), .groups = "drop")

  ggplot(d, aes(age_moyen, .data[[var_y]], colour = statut)) +
    # densité 2D en repère de lecture, sous les points
    geom_density_2d(bins = 6, linewidth = 0.35, alpha = 0.45) +
    geom_point(size = 1.5, alpha = 0.45, stroke = 0) +
    # médiane du nuage, marque plus grosse + étiquette directe (sélective)
    geom_point(data = med, aes(x, y), size = 4.6, shape = 21,
               fill = "white", stroke = 1.4) +
    # Le texte porte une encre neutre, jamais la couleur de la série : c'est la
    # marque à côté de lui qui porte l'identité.
    geom_text(data = med, aes(x, y, label = sprintf("médiane : %.0f ans, %.0f", x, y)),
              colour = "#1a1a18", vjust = -1.9, size = 3.3, fontface = "bold",
              show.legend = FALSE) +
    facet_wrap(~statut, nrow = 1) +
    scale_colour_manual(values = COULEURS, guide = "none") +  # le bandeau nomme la série
    coord_cartesian(xlim = c(18, 50), ylim = c(0, borne_y)) +
    labs(title = titre, subtitle = sous_titre,
         x = "Âge moyen du ou des parents (ans)", y = lab_y,
         caption = paste0("Insee, ", source_txt,
                          " Champ : ménages comportant une femme de ", AGE_BAS, " à ",
                          AGE_HAUT, " ans ayant connu une naissance récente.",
                          "\nUn point = un ménage de l'échantillon. NON PONDÉRÉ : décrit",
                          " l'échantillon, pas la population.",
                          if (hors > 0) paste0(" ", hors,
                            " point(s) au-delà de l'axe ne sont pas affichés.") else "",
                          "\nLes courbes sont une densité 2D, repère de lecture et non",
                          " estimation. Médianes non pondérées.")) +
    theme_minimal(base_size = 11) +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major = element_line(colour = "#ececea", linewidth = 0.3),
          strip.text = element_text(face = "bold", size = 11),
          plot.title = element_text(face = "bold"),
          plot.subtitle = element_text(colour = "#57534e", size = 9),
          plot.caption = element_text(size = 7.5, colour = "#78716c", hjust = 0))
}

if (!dir.exists("Output")) dir.create("Output")

ORDONNEES <- list(
  list(var = "nv", borne = 60, slug = "niveau_vie",
       axe = "Niveau de vie par UC (milliers d'euros constants 2025)"),
  list(var = "surface", borne = 200, slug = "surface",
       axe = "Surface du logement (m2)")
)
CHAMPS <- list(
  list(ans = ANNEE_SEULE, slug = ANNEE_SEULE, suffixe = ANNEE_SEULE,
       src = paste0("SRCV ", ANNEE_SEULE, ".")),
  list(ans = ANNEES_POOL, slug = "poole", suffixe = "6 vagues cumulées",
       src = paste0("SRCV ", paste(ANNEES_POOL, collapse = ", "),
                    " ; 2023 et 2024 exclus (anomalie de pondération)."))
)

tout <- list()
for (ch in CHAMPS) {
  d <- charger(ch$ans)
  cat("== ", ch$slug, " : ", nrow(d), " ménages avec naissance (",
      paste(paste0(levels(d$statut), " ", as.integer(table(d$statut))), collapse = " / "),
      ") ==\n", sep = "")
  tout[[length(tout) + 1]] <- d %>%
    transmute(champ = ch$slug, annee, statut, age_moyen, nv, surface)
  for (o in ORDONNEES) {
    f <- paste0("Output/nuage_naissances_", o$slug, "_", ch$slug, ".png")
    ggsave(f, tracer(d, o$var, o$axe, o$borne,
                     paste0("Naissances : nuage de points, ", ch$suffixe),
                     paste0(nrow(d), " ménages de l'échantillon — non pondéré"),
                     ch$src),
           width = 11, height = 6, dpi = 200, bg = "white")
    cat("    ", f, "\n", sep = "")
  }
}

write_csv(bind_rows(tout), "Output/nuage_naissances.csv")
cat("\nExporté : 4 PNG + Output/nuage_naissances.csv\n")


# ==========================================================================
#  Naissances par tranche de revenu
#  (ex-R/22_nuage_naissances_par_revenu.R)
# ==========================================================================

# ============================================================================
#  SRCV — Naissances par tranche de revenu de 500 EUR
# ----------------------------------------------------------------------------
#  Deux lectures, produites côte à côte parce qu'elles ne répondent pas à la
#  même question :
#
#   A. NOMBRE de naissances par tranche (comptage brut, non pondéré).
#      Mesure où se trouvent les naissances. Dominé par la démographie : le pic
#      est là où il y a le plus de ménages, pas là où on fait le plus d'enfants.
#
#   B. TAUX de naissance par tranche = naissances / ménages de la tranche,
#      PONDÉRÉ. Mesure la propension à avoir un enfant, à effectif neutralisé.
#      C'est la lecture pertinente pour un raisonnement causal.
#
#  Abscisse commune : revenu en tranches RÉGULIÈRES de 500 EUR constants 2025.
#  Deux mesures du revenu, "le revenu" pouvant désigner l'une ou l'autre :
#    - REVENU DISPONIBLE du ménage (HY020) : ce qui entre dans le foyer.
#    - NIVEAU DE VIE par UC : le même revenu rapporté à la taille du ménage.
#
#  Champ : ménages comportant une femme de 15 à 49 ans, TOUS STATUTS
#  d'occupation confondus (locataires, accédants, non accédants).
#
#  ⚠️ MILLÉSIMES DIFFÉRENTS SELON LA LECTURE, et c'est volontaire :
#     - Comptage (A), non pondéré : les HUIT vagues. L'anomalie de pondération
#       de 2022-2023 ne déforme que les estimations pondérées, un comptage
#       d'observations n'en souffre pas. On gagne ainsi 660 naissances.
#     - Taux (B), pondéré : 2022 et 2023 sont ÉCARTÉS. Ce sont les deux vagues
#       où le rapport du poids moyen naissance / non-naissance s'inverse (0,665
#       et 0,634 contre 1,18 à 1,30 ailleurs), ce qui biaise directement un taux
#       pondéré. 2024 est conservé : son rapport de 1,183 est dans la norme.
#       NB : ce choix diffère de l'exclusion "2023 et 2024" retenue ailleurs
#       dans le projet, qui visait la lisibilité d'une série annuelle ; ici
#       c'est la corrélation poids/naissance qui commande.
#
#  ⚠️ Sur le graphique de TAUX, la TAILLE DU POINT est proportionnelle au nombre
#     de ménages de la tranche, et le lissage loess est pondéré par ce même
#     effectif : les tranches minces ne tirent donc pas la courbe.
#
#  Sorties : Output/nuage_naissances_par_revenu.png
#            Output/nuage_naissances_par_niveau_vie.png
#            Output/taux_naissance_par_revenu.png
#            Output/taux_naissance_par_niveau_vie.png
#            Output/naissances_par_tranche_revenu.csv
# ============================================================================



PAS              <- 500     # largeur des tranches, en euros constants 2025
AGE_BAS          <- 15
AGE_HAUT         <- 49
ANNEES_COMPTAGE  <- names(VAGUES)                       # les 8 vagues
ANNEES_TAUX      <- setdiff(names(VAGUES), c("2022", "2023"))
BLEU             <- "#2a78d6"   # slot 1 de la palette catégorielle

IPC <- charger_ipc()

base <- bind_rows(imap(VAGUES, ~ preparer_donnees(.y, .x, restreindre = FALSE))) %>%
  filter(!is.na(age_femme), age_femme >= AGE_BAS, age_femme <= AGE_HAUT,
         !is.na(statut3), !is.na(naissance), !is.na(poids), poids > 0) %>%
  mutate(rev = deflater(revenu,     as.integer(annee) - 1, IPC, base = 2025),
         nv  = deflater(niveau_vie, as.integer(annee) - 1, IPC, base = 2025))

cat("Champ 15-49, tous statuts, 8 vagues :", nrow(base), "ménages |",
    sum(base$naissance == "Oui"), "naissances\n")

tranche_de <- function(x) floor(x / PAS) * PAS + PAS / 2   # point au milieu

# ── A. Comptage brut, non pondéré, 8 vagues ────────────────────────────────
compter <- function(var) {
  base %>% filter(annee %in% ANNEES_COMPTAGE, naissance == "Oui") %>%
    mutate(tranche = tranche_de(.data[[var]])) %>%
    filter(!is.na(tranche)) %>% count(tranche, name = "naissances")
}

# ── B. Taux pondéré : naissances / ménages de la tranche ───────────────────
taux <- function(var) {
  base %>% filter(annee %in% ANNEES_TAUX) %>%
    mutate(tranche = tranche_de(.data[[var]])) %>%
    filter(!is.na(tranche)) %>%
    group_by(tranche) %>%
    summarise(n_menages = n(),
              w_tot     = sum(poids),
              w_naiss   = sum(poids[naissance == "Oui"]),
              naiss_n   = sum(naissance == "Oui"),
              taux_pct  = 100 * w_naiss / w_tot, .groups = "drop")
}

theme_srcv <- function() {
  theme_minimal(base_size = 11) +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major = element_line(colour = "#ececea", linewidth = 0.3),
          plot.title = element_text(face = "bold"),
          plot.subtitle = element_text(colour = "#57534e", size = 9),
          plot.caption = element_text(size = 7.5, colour = "#78716c", hjust = 0))
}

tracer_comptage <- function(a, lab_x, borne_x, titre) {
  hors <- sum(a$naissances[a$tranche > borne_x])
  ggplot(a, aes(tranche, naissances)) +
    geom_smooth(method = "loess", span = 0.3, se = TRUE, colour = BLEU,
                fill = BLEU, alpha = 0.12, linewidth = 0.9) +
    geom_point(colour = BLEU, size = 1.9, alpha = 0.75) +
    scale_x_continuous(labels = function(v) format(v, big.mark = " ")) +
    coord_cartesian(xlim = c(0, borne_x), ylim = c(0, NA)) +
    labs(title = titre,
         subtitle = paste0(sum(a$naissances), " naissances observées, tranches de ",
                           PAS, " euros — comptage NON pondéré, 8 vagues"),
         x = lab_x, y = "Nombre de naissances observées",
         caption = paste0("Insee, SRCV 2006 à 2025. Champ : ménages comportant une femme de ",
                          AGE_BAS, " à ", AGE_HAUT, " ans, tous statuts confondus.",
                          "\nUn point = une tranche de ", PAS, " euros constants 2025.",
                          " Comptage d'observations, pas estimation de population.",
                          if (hors > 0) paste0(" ", hors, " naissances hors axe.") else "",
                          "\nATTENTION : cette courbe mêle fécondité et effectif de la",
                          " tranche. Voir le graphique de TAUX pour la propension.")) +
    theme_srcv()
}

tracer_taux <- function(a, lab_x, borne_x, titre) {
  # L'axe des taux est borné sur la plage utile : quelques tranches de très
  # faible effectif atteignent 25 à 100 % et écraseraient tout le reste.
  plafond <- ceiling(max(a$taux_pct[a$n_menages >= 50], na.rm = TRUE) * 1.15)
  masques <- sum(a$taux_pct > plafond & a$tranche <= borne_x)
  ggplot(a, aes(tranche, taux_pct)) +
    # lissage PONDÉRÉ par l'effectif : les tranches minces ne tirent pas la courbe
    geom_smooth(aes(weight = n_menages), method = "loess", span = 0.35, se = TRUE,
                colour = BLEU, fill = BLEU, alpha = 0.12, linewidth = 0.9) +
    geom_point(aes(size = n_menages), colour = BLEU, alpha = 0.55, stroke = 0) +
    scale_size_area(max_size = 5, name = "Ménages\ndans la tranche") +
    scale_x_continuous(labels = function(v) format(v, big.mark = " ")) +
    coord_cartesian(xlim = c(0, borne_x), ylim = c(0, plafond)) +
    labs(title = titre,
         subtitle = paste0("Naissances rapportées aux ménages de la tranche — PONDÉRÉ, ",
                           length(ANNEES_TAUX), " vagues (2022 et 2023 écartées)"),
         x = lab_x, y = "Taux de naissance (%)",
         caption = paste0("Insee, SRCV ", paste(ANNEES_TAUX, collapse = ", "),
                          ". Champ : ménages comportant une femme de ", AGE_BAS, " à ",
                          AGE_HAUT, " ans, tous statuts confondus.",
                          "\nUn point = une tranche de ", PAS,
                          " euros constants 2025 ; sa taille est l'effectif de ménages",
                          " de la tranche, et le lissage est pondéré par cet effectif.",
                          "\n2022 et 2023 sont écartées : la corrélation entre poids de",
                          " sondage et naissance y est inversée (cf. R/17-18).",
                          if (masques > 0) paste0(" ", masques, " tranche(s) de très faible",
                            " effectif dépassent l'axe et ne sont pas affichées.") else "")) +
    theme_srcv() + theme(legend.position = "right")
}

if (!dir.exists("Output")) dir.create("Output")

MESURES <- list(
  list(var = "rev", borne = 120000, slug = "revenu",
       axe = "Revenu disponible du ménage (euros constants 2025)",
       t_a = "Naissances par tranche de revenu disponible",
       t_b = "Taux de naissance par tranche de revenu disponible"),
  list(var = "nv", borne = 70000, slug = "niveau_vie",
       axe = "Niveau de vie par UC (euros constants 2025)",
       t_a = "Naissances par tranche de niveau de vie",
       t_b = "Taux de naissance par tranche de niveau de vie")
)

export <- list()
for (m in MESURES) {
  a <- compter(m$var); b <- taux(m$var)
  ggsave(paste0("Output/nuage_naissances_par_", m$slug, ".png"),
         tracer_comptage(a, m$axe, m$borne, m$t_a),
         width = 10, height = 6, dpi = 200, bg = "white")
  ggsave(paste0("Output/taux_naissance_par_", m$slug, ".png"),
         tracer_taux(b, m$axe, m$borne, m$t_b),
         width = 10.5, height = 6, dpi = 200, bg = "white")
  export[[length(export) + 1]] <- b %>% mutate(mesure = m$slug) %>%
    left_join(a, by = "tranche")
  pic <- b %>% filter(n_menages >= 100) %>% slice_max(taux_pct, n = 1)
  cat("  ", m$slug, " : ", nrow(b), " tranches | max du taux (tranches >= 100 ménages) : ",
      round(pic$taux_pct, 1), " % vers ", pic$tranche, " EUR\n", sep = "")
}

write_csv(bind_rows(export), "Output/naissances_par_tranche_revenu.csv")
cat("\nExporté : 4 PNG + Output/naissances_par_tranche_revenu.csv\n")
