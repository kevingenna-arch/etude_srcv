# ============================================================================
#  SRCV — Tableaux LaTeX "logement par quintile de niveau de vie", 2006 vs 2025
# ----------------------------------------------------------------------------
#  Entrée : Output/logement_par_quintile.csv          (produit par 12)
#  Sortie : Output/tableaux_logement_quintiles.tex
#           un tableau par classe d'âge (20-29, 30-39), deux panels
#           (locataires / accédants), une ligne par quintile.
#
#  Insertion :  \input{../Output/tableaux_logement_quintiles.tex}
#  Préambule :  \usepackage{booktabs}  \usepackage[flushleft]{threeparttable}
#
#  CELLULES FRAGILES (moins de N_MIN observations sur l'une des deux vagues) :
#   - FRAGILE = "signaler" : valeurs conservées, en italique, quintile suivi de †.
#     Choisi par défaut ici — masquer supprimerait 3 des 5 lignes du panel
#     "20-29 ans accédants" et casserait la structure demandée.
#   - FRAGILE = "masquer"  : valeurs remplacées par ---.
# ============================================================================

suppressPackageStartupMessages(library(tidyverse))

N_MIN      <- 30
FRAGILE    <- "signaler"   # "signaler" ou "masquer"
STANDALONE <- FALSE
STATUTS    <- c(Locataire             = "Locataires",
                Proprietaire_accedant = "Propriétaires accédants")

f_in  <- "Output/logement_par_quintile.csv"
f_out <- "Output/tableaux_logement_quintiles.tex"
if (!file.exists(f_in))
  stop("Fichier introuvable : ", f_in, "\nLancer d'abord R/12_logement_par_quintile.R",
       call. = FALSE)

d <- read_csv(f_in, show_col_types = FALSE) %>%
  filter(statut3 %in% names(STATUTS)) %>%
  mutate(statut3 = factor(statut3, levels = names(STATUTS)))

# ── Mise en forme (virgule décimale, espace fine pour les milliers) ─────────
fmt <- function(x, dec = 1, signe = FALSE) {
  if (is.na(x)) return("---")
  s <- formatC(round(x, dec), format = "f", digits = dec, big.mark = "@",
               flag = if (signe) "+" else "")
  gsub("@", "\\\\,", sub(".", ",", s, fixed = TRUE))
}

#  9 colonnes de valeurs : 3 indicateurs × (2006, 2025, évolution)
COLS <- list(
  c("surface_2006", "surface_2025", "d_surface_pct"),
  c("cout_m2_2006", "cout_m2_2025", "d_cout_m2_pct"),
  c("effort_2006",  "effort_2025",  "d_effort_pts")
)
K <- length(unlist(COLS)) + 1   # + colonne n

ligne <- function(r) {
  fragile <- min(r$n_2006, r$n_2025) < N_MIN
  vals <- unlist(lapply(COLS, function(g)
    vapply(g, function(v) fmt(r[[v]], 1, signe = grepl("^d_", v)), character(1))))
  if (fragile && FRAGILE == "masquer") vals[] <- "---"
  if (fragile && FRAGILE == "signaler") vals <- paste0("\\textit{", vals, "}")
  lab <- paste0(r$quintile, if (fragile) "$^{\\dagger}$" else "")
  paste0("    ", lab, " & ", paste(vals, collapse = " & "),
         " & {\\scriptsize ", r$n_2006, "/", r$n_2025, "} \\\\")
}

panel <- function(sub_d, titre, lettre) {
  lignes <- vapply(seq_len(nrow(sub_d)), function(i) ligne(sub_d[i, ]), character(1))
  paste0("    \\multicolumn{", K + 1, "}{l}{\\textit{Panel ", lettre, " --- ", titre,
         "}} \\\\\n", paste(lignes, collapse = "\n"))
}

# ── Un tableau par classe d'âge ─────────────────────────────────────────────
tableau <- function(cl) {
  sub_cl <- d %>% filter(classe == cl) %>% arrange(statut3, quintile)
  age    <- sub(" ans", "", cl)
  panels <- imap(names(STATUTS), function(st, i)
    panel(sub_cl %>% filter(statut3 == st), STATUTS[[st]], LETTERS[i]))
  frag <- any(pmin(sub_cl$n_2006, sub_cl$n_2025) < N_MIN)

  paste0(
"\\begin{table}[htbp]\n",
"  \\centering\n  \\footnotesize\n  \\setlength{\\tabcolsep}{3.5pt}\n",
"  \\begin{threeparttable}\n",
"  \\caption{Évolution des conditions de logement entre 2006 et 2025, par quintile de\n",
"    niveau de vie --- personne de référence de ", age, " ans}\n",
"  \\label{tab:quintiles_", gsub("-", "_", age), "}\n",
"  \\begin{tabular}{l", strrep("r", K), "}\n",
"    \\toprule\n",
"    & \\multicolumn{3}{c}{Surface (m\\textsuperscript{2})}\n",
"    & \\multicolumn{3}{c}{Coût/m\\textsuperscript{2} (\\texteuro)}\n",
"    & \\multicolumn{3}{c}{Taux d'effort (\\%)} & \\\\\n",
"    \\cmidrule(lr){2-4} \\cmidrule(lr){5-7} \\cmidrule(lr){8-10}\n",
"    Quintile & 2006 & 2025 & \\multicolumn{1}{c}{\\shortstack{$\\Delta$\\\\(\\%)}}\n",
"      & 2006 & 2025 & \\multicolumn{1}{c}{\\shortstack{$\\Delta$\\\\(\\%)}}\n",
"      & 2006 & 2025 & \\multicolumn{1}{c}{\\shortstack{$\\Delta$\\\\(pts)}}\n",
"      & \\multicolumn{1}{c}{\\shortstack{\\textit{n}\\\\06/25}} \\\\\n",
"    \\midrule\n",
paste(panels, collapse = "\n    \\addlinespace\n"), "\n",
"    \\bottomrule\n",
"  \\end{tabular}\n",
"  \\begin{tablenotes}[flushleft]\\scriptsize\n",
"    \\item \\textit{Champ} : ménages dont la personne de référence a ", age,
   " ans, France entière.\n",
"    \\item \\textit{Quintiles} : découpage national du niveau de vie (revenu disponible par\n",
"      unité de consommation) de chaque vague, calculé sur l'ensemble des ménages et pondéré.\n",
"      Q1 = 20\\,\\% les plus modestes. Les seuils sont recalculés en 2025, les quintiles ne\n",
"      contiennent donc pas les mêmes ménages aux deux dates.\n",
"    \\item \\textit{Lecture} : moyennes pondérées. Coût du logement = charges (\\texttt{HH070})\n",
"      + mensualité de remboursement (\\texttt{REMP}/\\texttt{REMPR}), déflaté par l'IPC\n",
"      (base 2025). $\\Delta$ = variation relative pour la surface et le coût au m\\textsuperscript{2},\n",
"      en points de pourcentage pour le taux d'effort.\n",
"    \\item \\textit{Accédants} : propriétaires remboursant un emprunt sur leur résidence\n",
"      principale. SRCV n'identifie pas la primo-accession : un achat ultérieur est compté\n",
"      de la même façon.\n",
if (frag && FRAGILE == "signaler")
  paste0("    \\item $^{\\dagger}$ Moins de ", N_MIN, " observations sur au moins une des deux\n",
         "      vagues : valeurs en italique, non interprétables.\n")
else if (frag)
  paste0("    \\item $^{\\dagger}$ Moins de ", N_MIN, " observations sur au moins une des deux\n",
         "      vagues : valeurs non reportées.\n") else "",
"    \\item \\textit{Source} : Insee, SRCV 2006 et 2025, calculs de l'auteur.\n",
"  \\end{tablenotes}\n  \\end{threeparttable}\n\\end{table}\n")
}

classes <- unique(d$classe)   # ordre du CSV (défini par CLASSES dans 12)
tex <- paste0(
"% ==========================================================================\n",
"%  Tableaux générés par R/13_tableaux_quintiles_latex.R — NE PAS ÉDITER\n",
"%  Source : ", f_in, "\n",
"%  Préambule requis : \\usepackage{booktabs} et \\usepackage[flushleft]{threeparttable}\n",
"% ==========================================================================\n\n",
paste(vapply(classes, tableau, character(1)), collapse = "\n"))

if (!dir.exists("Output")) dir.create("Output")
writeLines(tex, f_out, useBytes = TRUE)
cat("Écrit :", f_out, "—", length(classes), "tableaux (",
    paste(classes, collapse = ", "), ")\n")

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
