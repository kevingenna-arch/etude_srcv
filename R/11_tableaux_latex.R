# ============================================================================
#  SRCV — Génération des TABLEAUX LaTeX "logement par classe d'âge"
# ----------------------------------------------------------------------------
#  Entrées : Output/logement_par_classe_age_revenu.csv
#            Output/logement_par_classe_age_ipc.csv        (produits par 09)
#            -> pour les deux :  SRCV_DEFLATEUR=ipc Rscript R/09_logement_par_age.R
#
#  Sorties : Output/tableaux_logement_par_age.tex      (déflateur REVENU)
#            Output/tableaux_logement_par_age_ipc.tex  (déflateur IPC)
#            un tableau par classe d'âge, colonnes groupées
#            LOCATAIRES | PROPRIÉTAIRES ACCÉDANTS
#
#  Les deux jeux se lisent ensemble et ne disent PAS la même chose :
#   - IPC     : coût en euros constants -> évolution du prix du logement seul.
#   - REVENU  : coût rapporté au niveau de vie médian -> le logement s'est-il
#               renchéri PLUS VITE que les revenus ? Le taux d'effort n'est
#               affiché que là : calculé sur les montants NOMINAUX (coût annuel
#               / revenu disponible), il est identique dans les deux fichiers,
#               donc le répéter côté IPC n'apporterait rien.
#
#  Le .tex produit ne contient que des environnements {table} : il s'insère dans
#  le document avec  \input{../Output/tableaux_logement_par_age.tex}
#  Packages requis dans le préambule :
#    \usepackage{booktabs}                     (à ajouter dans etude_FdBC.tex)
#    \usepackage[flushleft]{threeparttable}    (déjà présent)
#
#  Pour un PDF de contrôle autonome : STANDALONE <- TRUE.
# ============================================================================

suppressPackageStartupMessages(library(tidyverse))

AVEC_SE    <- FALSE      # TRUE : erreur-type entre parenthèses sous chaque valeur
AVEC_N     <- TRUE       # TRUE : colonne d'effectifs (nécessite la colonne n dans le CSV)
N_MIN      <- 30         # cellules masquées (---) sous cet effectif ; 0 = tout afficher
STANDALONE <- FALSE      # TRUE : écrit en plus un .tex compilable seul
STATUTS    <- c(Locataire             = "Locataires",
                Proprietaire_accedant = "Propriétaires accédants")

#  Les deux sorties. Ajouter une variante = ajouter un bloc ici.
#   indicateurs : parmi "surface", "cout", "cout_m2", "effort" (ordre respecté)
#   prefixe     : préfixe des \label, doit différer d'une sortie à l'autre
SORTIES <- list(
  list(defl        = "revenu",
       indicateurs = c("surface", "cout", "cout_m2", "effort"),
       f_out       = "Output/tableaux_logement_par_age.tex",
       prefixe     = "tab:logement_",
       sous_titre  = "coûts rapportés au niveau de vie médian"),
  list(defl        = "ipc",
       indicateurs = c("surface", "cout", "cout_m2"),
       f_out       = "Output/tableaux_logement_par_age_ipc.tex",
       prefixe     = "tab:logementipc_",
       sous_titre  = "coûts en euros constants de 2025")
)

# ── Mise en forme des nombres (convention française : virgule décimale) ─────
#  formatC() n'accepte qu'un big.mark d'UN caractère : on passe par un
#  marqueur temporaire "@" remplacé ensuite par l'espace fine LaTeX "\,".
fmt <- function(x, dec) {
  if (is.na(x)) return("---")
  s <- formatC(round(x, dec), format = "f", digits = dec, big.mark = "@")
  gsub("@", "\\\\,", sub(".", ",", s, fixed = TRUE))
}

# ── Génération d'un jeu de tableaux ─────────────────────────────────────────
construire <- function(defl, indicateurs, f_out, prefixe, sous_titre) {

  f_in <- paste0("Output/logement_par_classe_age_", defl, ".csv")
  if (!file.exists(f_in))
    stop("Fichier introuvable : ", f_in,
         "\nLancer :  SRCV_DEFLATEUR=", defl, " Rscript R/09_logement_par_age.R",
         call. = FALSE)

  d <- read_csv(f_in, show_col_types = FALSE) %>%
    filter(statut3 %in% names(STATUTS)) %>%
    mutate(statut3 = factor(statut3, levels = names(STATUTS)),
           annee   = as.character(annee))

  # Nom de la colonne de coût : cout_defl (09 actuel) ou cout_reel (ancien CSV)
  col_cout <- if ("cout_defl" %in% names(d)) "cout_defl" else "cout_reel"
  canon    <- c(surface = "surface", cout = col_cout, cout_m2 = "cout_m2", effort = "effort")
  vars     <- unname(canon[indicateurs])
  manquant <- setdiff(vars, names(d))
  if (length(manquant))
    stop("Colonnes absentes de ", f_in, " : ", paste(manquant, collapse = ", "),
         call. = FALSE)

  dec     <- c(surface = 1, cout = 0, cout_m2 = 1, effort = 1)[indicateurs]
  entetes <- c(surface = "Surface\\\\(m\\textsuperscript{2})",
               cout    = "Coût\\\\(\\texteuro/mois)",
               cout_m2 = "Coût/m\\textsuperscript{2}\\\\(\\texteuro)",
               effort  = "Effort\\\\(\\%)")[indicateurs]
  names(dec) <- names(entetes) <- vars

  avec_n <- AVEC_N && "n" %in% names(d)
  if (!avec_n && N_MIN > 0)
    warning("Colonne 'n' absente de ", f_in, " : aucun masquage possible. ",
            "Relancer 09_logement_par_age.R.", call. = FALSE)
  cols <- if (avec_n) c(vars, "n") else vars

  # Une ligne de tabular (une année), tous statuts × indicateurs.
  #  Les moyennes d'une cellule dont l'effectif est < N_MIN sont masquées ("---")
  #  mais l'effectif reste affiché : le lecteur voit pourquoi.
  ligne <- function(sub_d, an) {
    cel <- function(st, var, se = FALSE) {
      r <- sub_d[sub_d$statut3 == st & sub_d$annee == an, ]
      if (!nrow(r)) return(if (se) "" else "---")
      if (var == "n") return(if (se) "" else paste0("{\\scriptsize ", r$n[1], "}"))
      if (avec_n && !is.na(r$n[1]) && r$n[1] < N_MIN) return(if (se) "" else "---")
      v <- r[[if (se) paste0("se.", var) else var]][1]
      if (se) { if (is.na(v)) "" else paste0("{\\scriptsize (", fmt(v, dec[[var]]), ")}") }
      else fmt(v, dec[[var]])
    }
    vals <- unlist(lapply(names(STATUTS), function(st) vapply(cols, cel, "", st = st)))
    l <- paste0(an, " & ", paste(vals, collapse = " & "), " \\\\")
    if (AVEC_SE) {
      ses <- unlist(lapply(names(STATUTS),
                           function(st) vapply(cols, cel, "", st = st, se = TRUE)))
      l <- paste0(l, "\n      & ", paste(ses, collapse = " & "), " \\\\[2pt]")
    }
    l
  }

  note_defl <- if (defl == "revenu") {
    paste("coûts déflatés par l'indice du niveau de vie médian de chaque vague",
          "(base 100 = 2025) ; un montant stable signifie une progression au même",
          "rythme que les revenus.")
  } else {
    paste("coûts déflatés par l'indice des prix à la consommation (Insee, base 100 =",
          "2025) ; les montants sont en euros de pouvoir d'achat constant.")
  }

  tableau <- function(cl) {
    sub_d  <- d[d$classe == cl, ]
    annees <- sort(unique(sub_d$annee))
    k      <- length(cols)
    ns     <- length(STATUTS)
    age    <- sub(" ans", "", cl)

    corps <- vapply(annees, function(a) ligne(sub_d, a), "")
    # respiration entre les fichiers de diffusion (2006-2018) et les FPR (2022+)
    i <- which(annees == "2018")
    if (length(i) && !AVEC_SE) corps[i] <- paste0(corps[i], "\n      \\addlinespace")

    paste0(
"\\begin{table}[htbp]\n",
"  \\centering\n  \\footnotesize\n  \\setlength{\\tabcolsep}{4pt}\n",
"  \\begin{threeparttable}\n",
"  \\caption{Conditions de logement des ménages dont la personne de référence a ",
   age, " ans (", sous_titre, ")}\n",
"  \\label{", prefixe, gsub("-", "_", age), "}\n",
"  \\begin{tabular}{l", strrep("r", k * ns), "}\n",
"    \\toprule\n",
"    & ", paste(sprintf("\\multicolumn{%d}{c}{%s}", k, STATUTS), collapse = " & "), " \\\\\n",
"    ", paste(sprintf("\\cmidrule(lr){%d-%d}", 2 + k * (seq_len(ns) - 1), 1 + k * seq_len(ns)),
              collapse = " "), "\n",
"    Année & ",
   paste(rep(sprintf("\\multicolumn{1}{c}{\\shortstack{%s}}",
                     if (avec_n) c(entetes, "\\textit{n}") else entetes), ns),
         collapse = " & "), " \\\\\n",
"    \\midrule\n",
"      ", paste(corps, collapse = "\n      "), "\n",
"    \\bottomrule\n",
"  \\end{tabular}\n",
"  \\begin{tablenotes}[flushleft]\\scriptsize\n",
"    \\item \\textit{Champ} : ménages dont la personne de référence a ", age,
   " ans, France entière.\n",
"    \\item \\textit{Lecture} : moyennes pondérées (poids de sondage SRCV). Coût du logement =\n",
"      charges (\\texttt{HH070}) + mensualité de remboursement (\\texttt{REMP}/\\texttt{REMPR}).\n",
if ("effort" %in% indicateurs)
"      Taux d'effort = coût annuel rapporté au revenu disponible, borné à [0 ; 100]\\,\\%.\n" else "",
"    \\item \\textit{Déflateur} : ", note_defl, "\n",
if (AVEC_SE) "    \\item Erreurs-types entre parenthèses.\n" else "",
if (avec_n && N_MIN > 0)
  paste0("    \\item \\textit{n} : effectif non pondéré de la cellule. Les moyennes reposant sur\n",
         "      moins de ", N_MIN, " observations ne sont pas reportées et sont remplacées par ---.\n")
else if (avec_n) "    \\item \\textit{n} : effectif non pondéré de la cellule.\n" else "",
"    \\item \\textit{Source} : Insee, SRCV 2006-2025, calculs de l'auteur.\n",
"  \\end{tablenotes}\n  \\end{threeparttable}\n\\end{table}\n")
  }

  classes <- sort(unique(d$classe))
  tex <- paste0(
"% ==========================================================================\n",
"%  Tableaux générés par R/11_tableaux_latex.R — NE PAS ÉDITER À LA MAIN\n",
"%  Source : ", f_in, "\n",
"%  Préambule requis : \\usepackage{booktabs} et \\usepackage[flushleft]{threeparttable}\n",
"% ==========================================================================\n\n",
paste(vapply(classes, tableau, ""), collapse = "\n"))

  if (!dir.exists("Output")) dir.create("Output")
  writeLines(tex, f_out, useBytes = TRUE)
  cat("Écrit :", f_out, "—", length(classes), "tableaux (",
      paste(classes, collapse = ", "), ") | déflateur :", defl, "\n")

  # Version compilable seule, pour contrôle visuel
  if (STANDALONE) {
    f_std <- sub("\\.tex$", "_standalone.tex", f_out)
    writeLines(c(
      "\\documentclass[12pt]{article}",
      "\\usepackage[T1]{fontenc}\\usepackage[utf8]{inputenc}\\usepackage[french]{babel}",
      "\\usepackage[left=30mm,right=30mm]{geometry}",
      "\\usepackage{booktabs}\\usepackage[flushleft]{threeparttable}\\usepackage{textcomp}",
      "\\begin{document}", tex, "\\end{document}"), f_std, useBytes = TRUE)
    cat("Écrit :", f_std, "(compilable avec pdflatex)\n")
  }
  invisible(tex)
}

for (s in SORTIES) with(s, construire(defl, indicateurs, f_out, prefixe, sous_titre))
