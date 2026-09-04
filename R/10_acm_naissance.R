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

source("R/00_utils.R")
source("R/00_config.R")
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
