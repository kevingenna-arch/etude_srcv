# ============================================================================
#  SRCV — Configuration des millésimes : SOURCE UNIQUE DE VÉRITÉ
# ============================================================================
#  Pour AJOUTER UNE VAGUE (ex. 2026) : ajouter UN bloc ici, et rien ailleurs.
#  Sourcé par 02 à 07.  À sourcer depuis la RACINE du projet.
#
#  Champs par vague :
#   men        : chemin du fichier MÉNAGES
#   ind        : chemin du fichier INDIVIDUS (NULL si non utilisé)
#   has_nbm    : TRUE si les variables NBM_* (calendrier d'activité) existent
#                -> uniquement les fichiers FPR (2022+). Requis par 03 et 04.
#   panel_grp  : groupe de CHAÎNAGE PANEL. DB030 identifie le même ménage
#                *à l'intérieur* d'un groupe seulement. Vérifié empiriquement :
#                  - 2006 / 2010 : aucun recoupement avec les autres vagues
#                  - 2014 <-> 2018 : 4 057 ménages communs, 88,7 % avec +4 ans
#                    d'âge de la PR  -> vrai chaînage
#                  - 2022..2025 : ~68 % communs d'une vague à la suivante,
#                    92 % avec +1 an d'âge -> vrai chaînage
#                Sert à construire l'identifiant de cluster (erreurs-types).
#   occ_type   : "hx070" (statut binaire 1/2) ou "stoc" (statut détaillé)
#   vars       : mapping c(nom_canonique = "NOM_DANS_LE_FICHIER")
#   naiss_ind  : reconstruction de la naissance depuis le fichier individus
#                (fichiers FPR : pas de module "événements"). NULL sinon.
#
#  ⚠️ Séparateurs : ';' de 2006 à 2023, ',' en 2024-2025 -> géré automatiquement
#     par lire_srcv() (00_utils.R).
# ============================================================================

VAGUES <- list(

  "2006" = list(
    men = "Data/lil-0457/lil-0457.csv/Csv/menages06_diff.csv",
    ind = "Data/lil-0457/lil-0457.csv/Csv/individus06_diff.csv",
    tuu_type = "tau99", has_nbm = FALSE, panel_grp = "P06", occ_type = "hx070",
    typlog_type = "hh010",
    vars = c(id = "DB030", y_birth = "EVENEMEN_c", cout_log = "HH070", revenu = "HY020",
             occ = "HX070", taille = "HX040", nb_enf = "NENFANTS", csp = "CS24PR",
             poids = "DB090", pieces = "HH030", couple = "couplepr", datent = "DATENT",
             age_pr = "agepr", age_cj = "agecj", sexe_pr = "sexepr", sexe_cj = "sexecj",
             urbain = "DB100", zeat = "ZEAT", tuu = "TAU99", surface = "SURFACE", rembours = "REMP", diplome = "DIP14PR",
             typlog = "HH010"),
    naiss_ind = NULL
  ),

  "2010" = list(
    men = "Data/lil-0747/lil-0747.csv/Csv/menages10_diff.csv",
    ind = "Data/lil-0747/lil-0747.csv/Csv/individus10_diff.csv",
    tuu_type = "tau99", has_nbm = FALSE, panel_grp = "P10", occ_type = "hx070",
    typlog_type = "hh010",
    vars = c(id = "DB030", y_birth = "EVENEMEN_C", cout_log = "HH070", revenu = "hy020",
             occ = "HX070", taille = "HX040", nb_enf = "NENFANTS", csp = "cs24pr",
             poids = "DB090", pieces = "HH030", couple = "couplepr", datent = "datent",
             age_pr = "agepr", age_cj = "agecj", sexe_pr = "sexepr", sexe_cj = "sexecj",
             urbain = "DB100", zeat = "ZEAT", tuu = "TAU99", surface = "SURFACE", rembours = "REMP", diplome = "DIP14PR",
             typlog = "HH010"),
    naiss_ind = NULL
  ),

  "2014" = list(
    men = "Data/lil-1090/lil-1090.csv/Csv/MENAGES14_DIFF.csv",
    ind = "Data/lil-1090/lil-1090.csv/Csv/INDIVIDUS14_DIFF.csv",
    tuu_type = "tuu", has_nbm = FALSE, panel_grp = "P1418", occ_type = "hx070",
    typlog_type = "hh010",
    vars = c(id = "DB030", y_birth = "EVENEMEN_c", cout_log = "HH070", revenu = "HY020",
             occ = "HX070", taille = "HX040", nb_enf = "NENFANTS", csp = "CS24PR",
             poids = "DB090", pieces = "HH030", couple = "couplepr", datent = "datent",
             age_pr = "agepr", age_cj = "agecj", sexe_pr = "sexepr", sexe_cj = "sexecj",
             urbain = "DB100", zeat = "ZEAT", tuu = "tuu10", surface = "SURFACE", rembours = "REMP", diplome = "dipdetpr",
             typlog = "HH010"),
    naiss_ind = NULL
  ),

  "2018" = list(
    men = "Data/lil-1374/lil-1374.csv/Csv/MENAGES18_DIFF.csv",
    ind = "Data/lil-1374/lil-1374.csv/Csv/INDIVIDUS18_DIFF.csv",
    tuu_type = "tuu", has_nbm = FALSE, panel_grp = "P1418", occ_type = "stoc",
    typlog_type = "typlog_ancien",
    vars = c(id = "DB030", y_birth = "EVENEMEN_C", cout_log = "HH070", revenu = "HY020",
             occ = "STOC", taille = "npers", nb_enf = "NENFANTS", csp = "CS24PR",
             poids = "pond_18", pieces = "PIECENB", couple = "couplepr", datent = "datent",
             age_pr = "agepr", age_cj = "agecj", sexe_pr = "sexepr", sexe_cj = "sexecj",
             urbain = "DB100", zeat = "ZEAT", tuu = "tuu10", surface = "SURFACE", rembours = "REMP", diplome = "DIPDETPR",
             typlog = "TYPLOG"),
    naiss_ind = NULL
  ),

  "2022" = list(
    men = "Data/lil-1646/lil-1646-Donnees_CSV/tab_men_fpr_2022.csv",
    ind = "Data/lil-1646/lil-1646-Donnees_CSV/tab_ind_fpr_2022.csv",
    tuu_type = "tuu", has_nbm = TRUE, panel_grp = "P2225", occ_type = "stoc",
    typlog_type = "typlog_nouveau",
    vars = c(id = "DB030", y_birth = "y_birth", cout_log = "HH070", revenu = "HY020",
             occ = "STOC", taille = "NPERS", nb_enf = "NENFANTS", csp = "PCSM",
             poids = "DB090", pieces = "NPIECES", couple = "COUPLEPR", datent = "EMMENAG",
             age_pr = "AGEPR", age_cj = "AGECJ", sexe_pr = "SEXEPR", sexe_cj = "SEXECJ",
             urbain = "DB100", zeat = "ZEAT", tuu = "TUU2017", surface = "SURFACE", rembours = "REMPR", diplome = "DIPDETPR",
             typlog = "TYPLOG"),
    naiss_ind = list(cle_men = "DB030", cle_ind = "RB040", annais = "ANAIS",
                     annees = c(2021L, 2022L))
  ),

  "2023" = list(
    men = "Data/lil-1710/lil-1710-Donnees_CSV/TAB_MEN_FPR_2023.csv",
    ind = "Data/lil-1710/lil-1710-Donnees_CSV/TAB_IND_FPR_2023.csv",
    tuu_type = "tuu", has_nbm = TRUE, panel_grp = "P2225", occ_type = "stoc",
    typlog_type = "typlog_nouveau",
    vars = c(id = "DB030", y_birth = "y_birth", cout_log = "HH070", revenu = "HY020",
             occ = "STOC", taille = "NPERS", nb_enf = "NENFANTS", csp = "PCSM",
             poids = "DB090", pieces = "NPIECES", couple = "COUPLEPR", datent = "EMMENAG",
             age_pr = "AGEPR", age_cj = "AGECJ", sexe_pr = "SEXEPR", sexe_cj = "SEXECJ",
             urbain = "DB100", zeat = "ZEAT", tuu = "TUU2017", surface = "SURFACE", rembours = "REMPR", diplome = "DIPDETPR",
             typlog = "TYPLOG"),
    naiss_ind = list(cle_men = "DB030", cle_ind = "RB040", annais = "ANAIS",
                     annees = c(2022L, 2023L))
  ),

  "2024" = list(
    men = "Data/lil-1738/lil-1738-Donnees_CSV/TAB_MEN_FPR_2024.csv",   # séparateur ','
    ind = "Data/lil-1738/lil-1738-Donnees_CSV/TAB_IND_FPR_2024.csv",
    tuu_type = "tuu", has_nbm = TRUE, panel_grp = "P2225", occ_type = "stoc",
    typlog_type = "typlog_nouveau",
    vars = c(id = "DB030", y_birth = "y_birth", cout_log = "HH070", revenu = "HY020",
             occ = "STOC", taille = "NPERS", nb_enf = "NENFANTS", csp = "PCSM",
             poids = "DB090", pieces = "NPIECES", couple = "COUPLEPR", datent = "EMMENAG",
             age_pr = "AGEPR", age_cj = "AGECJ", sexe_pr = "SEXEPR", sexe_cj = "SEXECJ",
             urbain = "DB100", zeat = "ZEAT", tuu = "TUU2017", surface = "SURFACE", rembours = "REMPR", diplome = "DIPDETPR",
             typlog = "TYPLOG"),
    naiss_ind = list(cle_men = "DB030", cle_ind = "RB040", annais = "ANAIS",
                     annees = c(2023L, 2024L))
  ),

  "2025" = list(
    men = "Data/lil-1804/lil-1804-Donnees_CSV/TAB_MEN_FPR_2025.csv",   # séparateur ','
    ind = "Data/lil-1804/lil-1804-Donnees_CSV/TAB_IND_FPR_2025.csv",
    tuu_type = "tuu", has_nbm = TRUE, panel_grp = "P2225", occ_type = "stoc",
    typlog_type = "typlog_nouveau",
    vars = c(id = "DB030", y_birth = "y_birth", cout_log = "HH070", revenu = "HY020",
             occ = "STOC", taille = "NPERS", nb_enf = "NENFANTS", csp = "PCSM",
             poids = "DB090", pieces = "NPIECES", couple = "COUPLEPR", datent = "EMMENAG",
             age_pr = "AGEPR", age_cj = "AGECJ", sexe_pr = "SEXEPR", sexe_cj = "SEXECJ",
             urbain = "DB100", zeat = "ZEAT", tuu = "TUU2017", surface = "SURFACE", rembours = "REMPR", diplome = "DIPDETPR",
             typlog = "TYPLOG"),
    naiss_ind = list(cle_men = "DB030", cle_ind = "RB040", annais = "ANAIS",
                     annees = c(2024L, 2025L))
  )
)

# Raccourcis pratiques
vagues_fpr <- function() names(VAGUES)[map_lgl(VAGUES, ~ isTRUE(.x$has_nbm))]  # 2022+
chemin_ind <- function(annee) VAGUES[[as.character(annee)]]$ind
chemin_men <- function(annee) VAGUES[[as.character(annee)]]$men

# ── Repères externes (source unique) ────────────────────────────────────────
#  Indicateur conjoncturel de fécondité, France entière (Insee, bilan
#  démographique) : dénominateur de la décomposition 2010-2025 (scripts 23, 28).
#   - 2025 : 1,56 (Insee Première n° 2087, bilan démographique 2025, publié le
#     5 février 2026, estimation provisoire). Une première version de l'étude
#     utilisait 1,58, chiffre non vérifié.
#   - 2010 : 2,03 (France entière, série Insee ; point haut depuis la fin du
#     baby-boom, avant le repli à 1,99 en 2013). La métropole seule est à 2,01.
#     Une première version de l'étude utilisait 2,02.
ICF_2010 <- 2.03
ICF_2025 <- 1.56

#  Fichiers DVF+ (Cerema), HORS DÉPÔT : ~8,3 Go, 97 fichiers départementaux.
#  Utilisé par le script 26 uniquement ; le chemin est propre à ce poste.
DIR_DVF <- "C:/Users/kgenn/Documents/TDTE/Immobilier/données Immo fécondité/1_DONNEES_LIVRAISON"
