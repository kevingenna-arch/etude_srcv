# ============================================================================
#  etude_srcv — FIL ROUGE DU PROJET
# ----------------------------------------------------------------------------
#  Ce fichier ne calcule rien. Il décrit QUI produit QUOI, à partir de QUOI, et
#  permet de n'exécuter que ce dont on a besoin.
#
#  UTILISATION (depuis la racine du projet)
#     source("run.R")
#     plan()                      # la carte complète du projet
#     plan("logement-fecondite")  # une seule question de recherche
#     etat()                      # sorties presentes / absentes / perimees
#     lancer("09", "11")          # executer des etapes, dans l'ordre donne
#     lancer(groupe = "conditions-logement")
#     lancer(tout = TRUE)         # tout regenerer (long : ~45 min, dont 25 pour 23)
#     parametres()                # les reglages surchargeables de chaque script
#
#  lancer() verifie les ENTREES avant de demarrer : si un fichier manque, il
#  indique l'etape qui le produit au lieu de planter au milieu du script.
#
#  ⚠️ Data/ et Output/ sont gitignores : apres un clone, il faut relancer les
#  etapes pour reconstituer Output/.
# ============================================================================

if (!requireNamespace("tibble", quietly = TRUE)) stop("Installer tidyverse.")
suppressPackageStartupMessages(library(tidyverse))

# ── Les etapes du projet ────────────────────────────────────────────────────
#  groupe   : question de recherche (c'est l'axe de navigation)
#  entrees  : fichiers necessaires AVANT de lancer (hors Data/, toujours requis)
#  sorties  : fichiers produits
#  reglages : variables surchargeables par variable d'environnement
E <- function(id, titre, script, groupe, entrees = character(), sorties = character(),
              reglages = character(), note = "")
  list(id = id, titre = titre, script = script, groupe = groupe, entrees = entrees,
       sorties = sorties, reglages = reglages, note = note)

ETAPES <- list(
  E("01", "Analyse historique 2006 (archive, ne pas citer)",
    "R/01_analyse.R", "archive",
    sorties = "Output/resultats_logit_naissance.csv",
    note = "Fige l'analyse initiale. Lecture read_csv2() erronee : resultats non fiables."),

  E("02a", "Modeles par millesime : taux d'effort x naissance (8 vagues)",
    "R/02a_modeles_par_vague.R", "logement-fecondite",
    sorties = c("Output/resultats_principal.csv", "Output/resultats_crosscheck.csv",
                "Output/crosscheck_demenage_cle.csv"),
    reglages = "RESTREINDRE_FECONDITE, AGE_MIN, AGE_MAX (dans 00_prepa_fecondite.R)"),

  E("02b", "Prix au m2, modele poole et evolution agregee",
    "R/02b_prix_et_evolution.R", "logement-fecondite",
    sorties = c("Output/evolution_agregee.csv", "Output/modele_poole_probit.csv",
                "Output/modele_prix_m2_surface.csv", "Output/densite_cout_m2.png",
                "Output/menages_analyse.csv"),
    reglages = "ANNEE_REF (annee de reference des comparaisons)",
    note = "Independant de 02a : les deux sourcent 00_prepa_fecondite.R."),

  E("06", "Mise en panel FPR 2022-2025 : logement (t) -> naissance (t+1)",
    "R/06_panel.R", "logement-fecondite",
    sorties = "Output/panel_naissance_logement.csv"),

  E("07", "Modele naissance ~ surface + cout (panel)",
    "R/07_modele_naissance_logement.R", "logement-fecondite",
    entrees = "Output/panel_naissance_logement.csv",
    sorties = "Output/modele_naissance_logement.csv"),

  E("15", "Test des non-demenageurs + effet minimum detectable",
    "R/15_test_non_demenageurs.R", "logement-fecondite",
    entrees = "Output/panel_naissance_logement.csv",
    sorties = "Output/test_non_demenageurs.csv",
    reglages = "ANCIENNETE_MIN, PUISSANCE, ALPHA",
    note = "Repond a l'anticipation : l'effet surface ne tient que chez les demenageurs."),

  E("17", "Naissances par statut d'occupation et millesime (15-49 ans)",
    "R/17_naissances_par_statut.R", "logement-fecondite",
    sorties = "Output/naissances_par_statut.csv",
    reglages = "AGE_BAS, AGE_HAUT",
    note = "Taux pondere ET non pondere : l'ecart entre les deux est un signal."),

  E("16", "Scenario : effet predit de +10 m2 sur la probabilite de naissance",
    "R/16_scenario_surface.R", "logement-fecondite",
    sorties = "Output/scenario_surface.csv",
    reglages = "ANNEE_SCENARIO, AGE_FEMME, STATUTS (en tete de script)",
    note = "Meme modele que 02b (F_DECOMPOSITION). Association, pas causalite."),

  E("10", "ACM : profils des couples avec naissance",
    "R/10_acm_naissance.R", "logement-fecondite",
    sorties = c("Output/acm_coordonnees.csv", "Output/acm_naissance.png"),
    reglages = "VAGUES_ACM",
    note = "Non reproductible au bit pres (MCA) : ecarts ~1e-10 entre executions."),

  E("09", "Conditions de logement par classe d'age (8 vagues)",
    "R/09_logement_par_age.R", "conditions-logement",
    sorties = "Output/logement_par_classe_age_revenu.csv",
    reglages = "SRCV_DEFLATEUR = revenu (defaut) | ipc",
    note = "A lancer DEUX fois (revenu puis ipc) pour alimenter l'etape 11."),

  E("11", "Tableaux LaTeX : logement par classe d'age",
    "R/11_tableaux_latex.R", "conditions-logement",
    entrees = c("Output/logement_par_classe_age_revenu.csv",
                "Output/logement_par_classe_age_ipc.csv"),
    sorties = c("Output/tableaux_logement_par_age.tex",
                "Output/tableaux_logement_par_age_ipc.tex"),
    reglages = "AVEC_SE, AVEC_N, N_MIN, SORTIES"),

  E("12", "Logement par quintile de niveau de vie ET par ZEAT, 2006 vs 2025",
    "R/12_logement_par_quintile.R", "conditions-logement",
    sorties = c("Output/logement_par_quintile.csv", "Output/logement_par_zeat.csv"),
    reglages = "CLASSES (classes d'age, chevauchantes), VENTILATIONS",
    note = "Deux ventilations PARALLELES, jamais croisees (sinon 540 cellules)."),

  E("13", "Tableaux LaTeX : quintiles",
    "R/13_tableaux_quintiles_latex.R", "conditions-logement",
    entrees = "Output/logement_par_quintile.csv",
    sorties = "Output/tableaux_logement_quintiles.tex",
    reglages = "N_MIN, FRAGILE"),

  E("14", "Graphiques : quintiles",
    "R/14_graph_quintiles.R", "conditions-logement",
    entrees = "Output/logement_par_quintile.csv",
    sorties = "Output/graph_quintiles.png",
    reglages = "DECOUPAGE, FACETTE_INDICATEUR, PANNEAUX_EN_COLONNE"),

  E("03", "Arbitrage intra-couple du conge parental",
    "R/03_conge_parental.R", "politique-familiale",
    sorties = "Output/couples_conge_2025.csv",
    reglages = "SRCV_ANNEE = 2025 (defaut) | 2022 | 2023 | 2024"),

  E("04", "Recap financier couples / personnes seules",
    "R/04_recap_financier.R", "politique-familiale",
    sorties = "Output/recap_financier_2025.xlsx",
    reglages = "SRCV_ANNEE = 2025 (defaut) | 2022 | 2023 | 2024"),

  E("05", "Part des prestations familiales par rang de naissance",
    "R/05_part_presta_naissance.R", "politique-familiale",
    sorties = c("Output/part_presta_naissance.csv", "Output/part_presta_naissance.png")),

  E("08", "Micro-simulation du quotient familial",
    "R/08_quotient_familial.R", "politique-familiale",
    sorties = "Output/quotient_familial_simulation.csv",
    note = "Baremes saisis en dur : a verifier avant publication."),

  E("23", "Surface x rang x statut, gradient geographique, decomposition 2010-2025",
    "R/23_surface_parite_decomposition.R", "logement-fecondite",
    sorties = c("Output/faits_par_rang.csv", "Output/quartiles_surface.csv",
                "Output/natalite_par_rang.csv", "Output/escalier_statut.csv",
                "Output/gradient_geo.csv", "Output/decomposition_natalite.csv",
                "Output/parametrisation_surface.csv", "Output/poids_relatif_canaux.csv",
                "Output/contrefactuel_locataires_jeunes.csv", "Output/robustesse_periode.csv",
                "Output/surface_en_tranches.csv", "Output/decomposition_seuil.csv"),
    reglages = "ANCIENNETE_MIN, SEUILS ; ICF_2010, ICF_2025 (00_config.R)",
    note = "Coeur du §3 de l'etude. Long (~25 min : avg_slopes sur facteurs)."),

  E("24", "Patrimoine financier (module MRF*) en controle",
    "R/24_patrimoine_financier_controle.R", "robustesse-fecondite",
    sorties = c("Output/patrimoine_couverture.csv", "Output/controle_patrimoine_surface.csv",
                "Output/controle_patrimoine_statut.csv")),

  E("25", "Meres d'un enfant de 0-2 ans : qui est en emploi ? (FPR 2022-2025)",
    "R/25_retour_emploi_meres.R", "politique-familiale",
    sorties = c("Output/retour_emploi_descriptif.csv", "Output/retour_emploi_modele.csv"),
    note = "PL032 = 1 inclut les meres en conge de maternite (statut auto-declare)."),

  E("26", "Indice de prix DVF+ par ZEAT x annee x type de bien",
    "R/26_dvf_zeat.R", "donnees-externes",
    sorties = c("Output/dvf_zeat_annee.csv", "Output/dvf_zeat_annee_pieces.csv",
                "Output/dvf_gradient_taille_zeat.csv"),
    reglages = "DIR_DVF (00_config.R)",
    note = "Donnees DVF+ hors depot (~8,3 Go, 97 fichiers). Exclu de lancer(tout = TRUE)."),

  E("27", "Robustesse : type de logement (maison/appartement) et indice DVF+",
    "R/27_robustesse_typlog_dvf.R", "robustesse-fecondite",
    entrees = "Output/dvf_zeat_annee.csv",
    sorties = c("Output/robustesse_typlog.csv", "Output/robustesse_dvf.csv"),
    note = "Memes formules que 23 (F_RANG1 pour le 1er enfant)."),

  E("28", "Robustesse : mesure de la naissance FPR (double comptage, hors 2022-2023)",
    "R/28_robustesse_naissance_fpr.R", "robustesse-fecondite",
    sorties = "Output/robustesse_naissance_fpr.csv",
    note = "Re-estime effet_surface, seuil, escalier M5 et la decomposition sous 6 variantes."),

  E("29", "Coupe vs panel : la specification de la coupe sur l'echantillon du panel",
    "R/29_coupe_vs_panel.R", "robustesse-fecondite",
    entrees = "Output/panel_naissance_logement.csv",
    sorties = "Output/coupe_vs_panel.csv",
    note = "Departage design (t -> t+1) et specification (probit, cout_m2, controles)."),

  E("30", "Seuil a 50 m2 : tranches fines, bornes alternatives, point de rupture",
    "R/30_seuil_surface.R", "robustesse-fecondite",
    sorties = c("Output/seuil_surface_sensibilite.csv", "Output/seuil_surface_decomposition.csv"))
)
names(ETAPES) <- map_chr(ETAPES, "id")

#  VERSION COMPACTE — "R compact/" regroupe les mêmes analyses en 4 fichiers
#  (A modèles fécondité, B descriptif fécondité, C conditions de logement,
#  D politique familiale). Sorties identiques, ~40 % plus rapide grâce au cache
#  mémoïsé du socle. Les étapes ci-dessous restent la référence unitaire.
COMPACT <- c(
  "R compact/A_fecondite_modeles.R"    = "02a, 02b, 06, 07, 15, 16",
  "R compact/B_fecondite_descriptif.R" = "10, 17, 18, 20, 21, 22",
  "R compact/C_conditions_logement.R"  = "09, 11, 12, 13, 14, 19",
  "R compact/D_politique_familiale.R"  = "03, 04, 05, 08")

# Lance la version compacte : tout le projet en 4 fichiers.
lancer_compact <- function() {
  for (f in names(COMPACT)) {
    message("
>>> ", f, "  (regroupe R/", COMPACT[[f]], ")")
    t0 <- Sys.time(); source(f, local = new.env(), echo = FALSE)
    message("<<< terminé en ",
            round(as.numeric(difftime(Sys.time(), t0, units = "secs"))), " s")
  }
  invisible(NULL)
}

GROUPES <- c(
  "logement-fecondite"   = "Le logement influence-t-il la fecondite ?",
  "robustesse-fecondite" = "Les resultats du §3 tiennent-ils ? (controles, mesure, design, seuil)",
  "conditions-logement"  = "Comment les conditions de logement ont-elles evolue ?",
  "politique-familiale"  = "Qui recoit quoi de la politique familiale ?",
  "donnees-externes"     = "Sources hors SRCV (DVF+), non versionnees",
  "archive"              = "Conserve pour memoire, ne pas citer"
)

SOCLE <- c(
  "R/00_config.R"         = "Chemins + mapping des variables par vague (SOURCE UNIQUE)",
  "R/00_utils.R"          = "Lecture, recodages, deflateur, briques de chargement partagees",
  "R/00_prepa_fecondite.R"= "Champ fecondite + preparer_donnees() : socle de 02a et 02b")

# ── plan() : la carte ───────────────────────────────────────────────────────
plan <- function(groupe = NULL) {
  cat("\n=== SOCLE (source par tous les scripts) ===\n")
  for (f in names(SOCLE)) cat(sprintf("  %-18s %s\n", basename(f), SOCLE[[f]]))

  gs <- if (is.null(groupe)) names(GROUPES) else groupe
  for (g in gs) {
    cat(sprintf("\n=== %s ===\n    %s\n", toupper(g), GROUPES[[g]]))
    for (e in keep(ETAPES, ~ .x$groupe == g)) {
      cat(sprintf("\n  [%s] %s\n       %s\n", e$id, e$titre, e$script))
      if (length(e$entrees))  cat("       <- ", paste(basename(e$entrees), collapse = ", "), "\n")
      if (length(e$sorties))  cat("       -> ", paste(basename(e$sorties), collapse = ", "), "\n")
      if (nzchar(e$note))     cat("       ! ", e$note, "\n")
    }
  }
  cat("\nlancer(\"09\") pour executer une etape | etat() pour l'etat des sorties\n\n")
  invisible(NULL)
}

# ── etat() : ce qui est a jour, absent ou perime ────────────────────────────
#  "perime" = une sortie plus ancienne que son script ou qu'une de ses entrees.
etat <- function(groupe = NULL) {
  es <- if (is.null(groupe)) ETAPES else keep(ETAPES, ~ .x$groupe == groupe)
  res <- map_dfr(es, function(e) {
    if (!length(e$sorties)) return(NULL)
    presentes <- file.exists(e$sorties)
    ref <- max(c(file.mtime(e$script),
                 if (length(e$entrees)) file.mtime(e$entrees[file.exists(e$entrees)]) else NULL),
               na.rm = TRUE)
    statut <- if (!all(presentes)) "ABSENT" else
      if (min(file.mtime(e$sorties)) < ref) "perime" else "a jour"
    tibble(id = e$id, titre = substr(e$titre, 1, 46), statut = statut,
           sorties = sprintf("%d/%d", sum(presentes), length(e$sorties)))
  })
  print(as.data.frame(res), row.names = FALSE)
  invisible(res)
}

# ── parametres() : les reglages sans ouvrir les scripts ─────────────────────
parametres <- function() {
  for (e in ETAPES) if (nzchar(paste(e$reglages, collapse = "")))
    cat(sprintf("  [%s] %-42s %s\n", e$id, basename(e$script), e$reglages))
  cat("\n  Les reglages en SRCV_* se passent par variable d'environnement :\n",
      "    Sys.setenv(SRCV_DEFLATEUR = \"ipc\"); lancer(\"09\")\n",
      "    ou en shell :  SRCV_ANNEE=2022 Rscript R/03_conge_parental.R\n\n")
  invisible(NULL)
}

# ── lancer() : executer des etapes ──────────────────────────────────────────
#  isole = FALSE : source() dans un environnement neuf (rapide, defaut).
#  isole = TRUE  : sous-processus Rscript (isolation totale, plus lent).
lancer <- function(..., groupe = NULL, tout = FALSE, isole = FALSE) {
  ids <- c(...)
  choix <- if (tout) keep(ETAPES, ~ !.x$groupe %in% c("archive", "donnees-externes"))
           else if (!is.null(groupe)) keep(ETAPES, ~ .x$groupe == groupe)
           else ETAPES[ids]
  if (!length(choix) || any(map_lgl(choix, is.null)))
    stop("Etape inconnue. Voir plan().", call. = FALSE)

  for (e in choix) {
    manquantes <- e$entrees[!file.exists(e$entrees)]
    if (length(manquantes)) {
      producteurs <- map_chr(manquantes, function(f) {
        p <- keep(ETAPES, ~ f %in% .x$sorties)
        if (length(p)) p[[1]]$id else "?"
      })
      stop("[", e$id, "] entrees manquantes :\n  ",
           paste0(manquantes, "  (produit par l'etape ", producteurs, ")", collapse = "\n  "),
           call. = FALSE)
    }
    message("\n>>> [", e$id, "] ", e$titre, "  (", e$script, ")")
    t0 <- Sys.time()
    if (isole) {
      code <- system2(file.path(R.home("bin"), "Rscript"), shQuote(e$script))
      if (code != 0) stop("[", e$id, "] echec (code ", code, ")", call. = FALSE)
    } else {
      source(e$script, local = new.env(), echo = FALSE)
    }
    message("<<< [", e$id, "] termine en ",
            round(as.numeric(difftime(Sys.time(), t0, units = "secs"))), " s")
  }
  invisible(NULL)
}

message("Fil rouge charge. plan() pour la carte, etat() pour l'etat des sorties.")
