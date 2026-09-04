# ============================================================================
#  SRCV — C. CONDITIONS DE LOGEMENT ET TABLEAUX
# ----------------------------------------------------------------------------
#  Regroupe R/09_logement_par_age, R/11_tableaux_latex, R/12_logement_par_quintile, R/13_tableaux_quintiles_latex, R/14_graph_quintiles, R/19_descriptif_par_statut_latex.
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
#     source("R compact/C_conditions_logement.R")
# ============================================================================

source("R/00_prepa_fecondite.R")
activer_cache_prepa()


# ==========================================================================
#  Logement par classe d'âge (2 déflateurs)
#  (ex-R/09_logement_par_age.R)
# ==========================================================================

for (.defl in c("revenu", "ipc")) {
  # ============================================================================
  #  SRCV — Conditions de logement par CLASSE D'ÂGE de la personne de référence
  # ----------------------------------------------------------------------------
  #  Pour chaque classe d'âge (20-24, 25-29, 30-34, 35-39) et chaque millésime :
  #    - surface moyenne (m²)
  #    - coût du logement moyen et coût au m², DÉFLATÉS
  #    - taux d'effort (coût annuel / revenu disponible)
  #  Ventilé entre LOCATAIRES et PROPRIÉTAIRES ACCÉDANTS.
  #
  #  DEUX DÉFLATEURS, au choix via DEFLATEUR :
  #   - "ipc"    : indice des prix à la consommation (INSEE, base 2025).
  #                Lecture : coût en euros de pouvoir d'achat constant.
  #   - "revenu" : indice du NIVEAU DE VIE MÉDIAN (revenu par UC) de chaque vague.
  #                Lecture : coût rapporté à l'évolution des revenus — un coût
  #                stable signifie qu'il progresse au même rythme que les revenus.
  #                C'est une mesure de "prix relatif au niveau de vie".
  #
  #  Coût = charges (HH070) + mensualité d'emprunt (REMP/REMPR).
  #  Champ : tous les ménages (aucune restriction de couple ni de fécondité).
  # ============================================================================
  
  
  
  library(survey)
  
  # "ipc" ou "revenu". Surchargeable sans éditer le script, pour produire les deux
  # sorties d'affilée :  SRCV_DEFLATEUR=ipc Rscript R/09_logement_par_age.R
  DEFLATEUR <- .defl
  stopifnot(DEFLATEUR %in% c("ipc", "revenu"))
  IPC <- charger_ipc()
  
  # ── Chargement d'une vague ──────────────────────────────────────────────────
  #  charger_menages() (00_utils.R) applique les conventions du projet : coût =
  #  charges + mensualité, statut en 3 postes, surfaces bornées, niveau de vie.
  #  Ne reste ici que le découpage en classes d'âge, propre à ce script.
  charger <- function(annee, cfg) {
    charger_menages(annee, cfg) %>%
      mutate(classe = cut(age_pr, breaks = c(19, 24, 29, 34, 39),
                          labels = c("20-24 ans","25-29 ans","30-34 ans","35-39 ans"))) %>%
      filter(!is.na(classe), !is.na(statut3), !is.na(cout), cout >= 0)
  }
  d <- imap_dfr(VAGUES, ~ charger(.y, .x))
  
  # ── Indice de NIVEAU DE VIE MÉDIAN par vague (déflateur "revenu") ───────────
  # Calculé sur TOUS les ménages de la vague (pas seulement les 20-39 ans), pour
  # refléter l'évolution générale des revenus.
  idx_revenu <- imap_dfr(VAGUES, function(cfg, y) {
    m <- lire_srcv(cfg$men)
    x <- tibble(nv = num(m[[cfg$vars[["revenu"]]]]) / pmax(num(m$HX050), 1),
                w  = num(m[[cfg$vars[["poids"]]]])) %>%
      filter(!is.na(nv), nv > 0, !is.na(w), w > 0)
    tibble(annee = y, nv_median = wquantile(x$nv, x$w, 0.5))   # 00_utils.R
  }) %>% mutate(indice = 100 * nv_median / nv_median[annee == "2025"])
  
  cat("=== Déflateurs par millésime (base 100 = 2025) ===\n")
  print(as.data.frame(idx_revenu %>%
    left_join(IPC %>% transmute(annee = as.character(annee),
                                ipc = round(100*ipc/ipc[annee == 2025], 1)), by = "annee") %>%
    transmute(annee, nv_median = round(nv_median), indice_revenu = round(indice, 1),
              indice_ipc = ipc)), row.names = FALSE)
  
  # ── Application du déflateur choisi ─────────────────────────────────────────
  d <- d %>% left_join(idx_revenu %>% select(annee, indice), by = "annee") %>%
    mutate(cout_defl = if (DEFLATEUR == "revenu") cout * 100 / indice
                       else deflater(cout, as.integer(annee), IPC, base = 2025),
           cout_m2   = cout_defl / surface,
           # Taux d'effort : revenus <= 0 exclus et taux borné à [0 ; 100] %,
           # comme dans 02 — sinon quelques revenus nuls ou négatifs produisent
           # des valeurs aberrantes qui détruisent la moyenne.
           effort    = if_else(!is.na(revenu) & revenu > 0,
                               pmin(pmax(100 * (cout * 12) / revenu, 0), 100),
                               NA_real_))
  
  cat("\nDéflateur retenu :", DEFLATEUR,
      "| n =", nrow(d), "ménages (PR de 20 à 39 ans)\n")
  
  des <- svydesign(ids = ~1, weights = ~poids, data = d)
  for (cl in levels(d$classe)) {
    cat("\n\n##########", cl, "##########\n")
    for (st in c("Locataire", "Proprietaire_accedant")) {
      n <- sum(d$classe == cl & d$statut3 == st, na.rm = TRUE)
      cat("\n--", st, "— n =", n, "--\n")
      if (n < 30) { cat("   effectif insuffisant\n"); next }
      print(as.data.frame(
        svyby(~surface + cout_defl + cout_m2 + effort, ~annee,
              subset(des, classe == cl & statut3 == st), svymean, na.rm = TRUE) %>%
        as_tibble() %>%
        transmute(annee, surface_m2 = round(surface, 1),
                  cout_defl_eur = round(cout_defl), cout_m2_eur = round(cout_m2, 2),
                  effort_pct = round(effort, 1))), row.names = FALSE)
    }
  }
  
  if (!dir.exists("Output")) dir.create("Output")
  # Effectifs NON PONDÉRÉS exportés avec les moyennes : sans eux, l'export laisse
  # croire à des estimations solides là où la boucle ci-dessus refuse d'afficher
  # (n < 30). Cas typique : 20-24 ans accédants, n compris entre 9 et 23 selon la
  # vague — moyennes ininterprétables.
  effectifs <- count(d, annee, classe, statut3, name = "n")
  write_csv(svyby(~surface + cout_defl + cout_m2 + effort, ~annee + classe + statut3,
                  des, svymean, na.rm = TRUE) %>% as_tibble() %>%
              left_join(effectifs, by = c("annee", "classe", "statut3")),
            paste0("Output/logement_par_classe_age_", DEFLATEUR, ".csv"))
  cat("\n\nExporté : Output/logement_par_classe_age_", DEFLATEUR, ".csv\n", sep = "")
}



# ==========================================================================
#  Tableaux LaTeX par classe d'âge
#  (ex-R/11_tableaux_latex.R)
# ==========================================================================

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


# ==========================================================================
#  Logement par quintile et par ZEAT
#  (ex-R/12_logement_par_quintile.R)
# ==========================================================================

# ============================================================================
#  SRCV — Logement par QUINTILE DE NIVEAU DE VIE, 2006 vs 2025
# ----------------------------------------------------------------------------
#  Pour trois classes d'âge de la personne de référence (20-29, 30-39 et
#  15-49 ans — cette dernière ENGLOBE les deux autres : les classes se
#  chevauchent, ce n'est pas une partition), deux statuts (locataires /
#  propriétaires accédants) et cinq quintiles :
#    - surface moyenne (m²)
#    - coût au m² en EUROS CONSTANTS 2025 (déflateur IPC)
#    - taux d'effort (coût annuel / revenu disponible), invariant au déflateur
#  et l'évolution 2006 -> 2025 de chacun.
#
#  QUINTILES : découpage NATIONAL du niveau de vie (revenu disponible / UC) de
#  chaque vague, calculé sur TOUS les ménages, pondéré. Q1 = 20 % les plus
#  modestes. Un ménage jeune est donc positionné dans la distribution générale,
#  pas seulement parmi ses pairs — c'est la lecture usuelle d'un « quintile de
#  revenus », mais elle concentre mécaniquement les 20-29 ans dans le bas.
#
#  ⚠️ « Accédant » = propriétaire remboursant un emprunt sur sa résidence
#  principale (REMP/REMPR > 0). SRCV n'identifie PAS la primo-accession : un
#  second achat est compté de la même façon. Aux âges retenus l'accédant est
#  très majoritairement primo-accédant, mais ce n'est qu'une approximation.
#
#  Coût = charges (HH070) + mensualité d'emprunt (REMP/REMPR).
#  Sortie : Output/logement_par_quintile.csv
# ============================================================================



library(survey)

ANNEES <- c("2006", "2025")
IPC    <- charger_ipc()

# Chargement standard (00_utils.R) : coût = charges + mensualité, statut en
# 3 postes, surfaces bornées, niveau de vie, taux d'effort. wquantile() y est
# également défini.
brut <- imap_dfr(VAGUES[ANNEES], ~ charger_menages(.y, .x))

# ── Quintiles nationaux de niveau de vie, par vague ─────────────────────────
seuils <- brut %>%
  filter(!is.na(niveau_vie), niveau_vie > 0) %>%
  group_by(annee) %>%
  summarise(q = list(wquantile(niveau_vie, poids, c(.2, .4, .6, .8))), .groups = "drop")

cat("=== Seuils de niveau de vie (euros courants / an / UC) ===\n")
print(as.data.frame(seuils %>%
  mutate(Q20 = map_dbl(q, 1), Q40 = map_dbl(q, 2),
         Q60 = map_dbl(q, 3), Q80 = map_dbl(q, 4)) %>%
  transmute(annee, across(Q20:Q80, round))), row.names = FALSE)

quintile <- function(nv, an) {
  br <- seuils$q[[match(an, seuils$annee)]]
  cut(nv, breaks = c(-Inf, br, Inf), labels = paste0("Q", 1:5))
}

# ── Champ d'analyse ─────────────────────────────────────────────────────────
#  ⚠️ Les classes se CHEVAUCHENT : 15-49 ans englobe les deux autres. Un ménage
#  apparaît donc dans plusieurs lignes du résultat — ce n'est pas une partition,
#  chaque classe est une analyse séparée. Ajouter une classe = un élément ici.
CLASSES <- list("20-29 ans" = c(20, 29),
                "30-39 ans" = c(30, 39),
                "15-49 ans" = c(15, 49))

d <- imap_dfr(CLASSES, function(bornes, cl)
        brut %>% filter(!is.na(age_pr), age_pr >= bornes[1], age_pr <= bornes[2]) %>%
                 mutate(classe = cl)) %>%
  mutate(classe = factor(classe, levels = names(CLASSES))) %>%
  filter(!is.na(statut3), !is.na(cout), cout >= 0,
         statut3 %in% c("Locataire", "Proprietaire_accedant")) %>%
  group_by(annee) %>%
  mutate(quintile = quintile(niveau_vie, annee[1])) %>%
  ungroup() %>%
  filter(!is.na(quintile)) %>%
  # `effort` (taux d'effort censuré dans [0 ; 100]) vient de charger_menages().
  mutate(statut3   = droplevels(statut3),
         cout_defl = deflater(cout, as.integer(annee), IPC, base = 2025),
         cout_m2   = cout_defl / surface)

cat("\n=== Effectifs non pondérés par cellule ===\n")
print(as.data.frame(d %>% count(classe, statut3, quintile, annee) %>%
                      pivot_wider(names_from = annee, values_from = n, values_fill = 0)),
      row.names = FALSE)

# ── Moyennes pondérées ──────────────────────────────────────────────────────
#  DEUX VENTILATIONS PARALLÈLES, jamais croisées : quintile de niveau de vie et
#  ZEAT. Le croisement donnerait 3 classes × 2 statuts × 5 quintiles × 9 zones
#  × 2 années = 540 cellules, alors que le seul croisement par quintile place
#  déjà les 20-29 ans accédants sous le seuil de 30 observations.
#
#  ⚠️ La ZEAT est exploitable ICI parce que ce script ne compare que 2006 et
#  2025 : 2006 est réparable par REGION et 2025 est complète. Les vagues 2010
#  et 2014, où 21-23 % des ménages n'ont pas de ZEAT, ne sont pas utilisées.
VENTILATIONS <- c(quintile = "Output/logement_par_quintile.csv",
                  zeat     = "Output/logement_par_zeat.csv")

des <- svydesign(ids = ~1, weights = ~poids, data = d)
if (!dir.exists("Output")) dir.create("Output")

for (v in names(VENTILATIONS)) {
  cles <- c("annee", "classe", "statut3", v)
  res <- svyby(~surface + cout_m2 + effort,
               reformulate(cles), des, svymean, na.rm = TRUE) %>%
    as_tibble() %>%
    left_join(count(d, across(all_of(cles)), name = "n"), by = cles)

  evo <- res %>%
    select(all_of(c(cles, "surface", "cout_m2", "effort", "n"))) %>%
    pivot_wider(names_from = annee, values_from = c(surface, cout_m2, effort, n)) %>%
    mutate(d_surface_pct = 100 * (surface_2025 / surface_2006 - 1),
           d_cout_m2_pct = 100 * (cout_m2_2025 / cout_m2_2006 - 1),
           d_effort_pts  = effort_2025 - effort_2006) %>%
    arrange(classe, statut3, .data[[v]])

  write_csv(evo, VENTILATIONS[[v]])

  cat("\n=== Évolutions 2006 -> 2025, ventilation par ", toupper(v), " ===\n", sep = "")
  print(as.data.frame(evo %>%
    transmute(classe, statut3, ventilation = .data[[v]],
              surf06 = round(surface_2006, 1), surf25 = round(surface_2025, 1),
              surf_pct = round(d_surface_pct, 1),
              m2_06 = round(cout_m2_2006, 1), m2_25 = round(cout_m2_2025, 1),
              m2_pct = round(d_cout_m2_pct, 1),
              eff06 = round(effort_2006, 1), eff25 = round(effort_2025, 1),
              eff_pts = round(d_effort_pts, 1),
              n06 = n_2006, n25 = n_2025)), row.names = FALSE)
  cat("Exporté :", VENTILATIONS[[v]], "\n")
}

cat("\nExporté : Output/logement_par_quintile.csv\n")


# ==========================================================================
#  Tableaux LaTeX quintiles
#  (ex-R/13_tableaux_quintiles_latex.R)
# ==========================================================================

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


# ==========================================================================
#  Graphiques quintiles
#  (ex-R/14_graph_quintiles.R)
# ==========================================================================

# ============================================================================
#  SRCV — Graphiques : évolution 2006-2025 par quintile de niveau de vie
# ----------------------------------------------------------------------------
#  Entrée : Output/logement_par_quintile.csv   (objet "evo" produit par 12)
#  Sorties : Output/graph_quintiles.png            (les 6 panneaux en une figure)
#            + selon DECOUPAGE :
#              "statut"  -> graph_quintiles_locataires.png / _accedants.png
#                           (3 panneaux chacun, un par classe d'âge)
#              "cellule" -> les 6 panneaux en 6 fichiers séparés
#
#  Trois barres par quintile : surface, coût au m², taux d'effort.
#  ⚠️ UNITÉS DIFFÉRENTES sur un même axe : surface et coût/m² sont des variations
#     en %, le taux d'effort est un écart en POINTS. Les ordres de grandeur ne
#     sont pas comparables (le coût/m² monte à +41 %, l'effort tient dans
#     ±8 pts) : la barre "effort" paraît écrasée. Mettre FACETTE_INDICATEUR à
#     TRUE pour séparer les trois indicateurs avec une échelle libre.
# ============================================================================

suppressPackageStartupMessages(library(tidyverse))

FACETTE_INDICATEUR <- FALSE   # TRUE : une ligne par indicateur, échelle libre
N_MIN              <- 30      # bandes estompées sous cet effectif
DECOUPAGE          <- "statut"  # "statut"  : 2 fichiers de 3 panneaux (par classe d'âge)
                                # "cellule" : 6 fichiers d'un panneau chacun
PANNEAUX_EN_COLONNE <- TRUE   # découpage "statut" : 3 panneaux empilés (axe des
                              # quintiles aligné) plutôt que côte à côte

evo <- read_csv("Output/logement_par_quintile.csv", show_col_types = FALSE)

# ── Passage en format long : une ligne = un quintile × un indicateur ────────
evo_long <- evo %>%
  transmute(classe, statut3, quintile,
            fragile = pmin(n_2006, n_2025) < N_MIN,
            surf_pct = d_surface_pct,     # variation de surface, en %
            m2_pct   = d_cout_m2_pct,     # variation du coût au m², en %
            eff_pts  = d_effort_pts) %>%  # variation du taux d'effort, en points
  pivot_longer(c(surf_pct, m2_pct, eff_pts),
               names_to = "indicateur", values_to = "valeur") %>%
  mutate(
    indicateur = factor(indicateur,
                        levels = c("surf_pct", "m2_pct", "eff_pts"),
                        labels = c("Surface (%)", "Coût/m² (%)",
                                   "Taux d'effort (points)")),
    classe     = factor(classe, levels = unique(evo$classe)),
    statut3    = factor(statut3,
                        levels = c("Locataire", "Proprietaire_accedant"),
                        labels = c("Locataires", "Propriétaires accédants")))

# ── Le graphique ────────────────────────────────────────────────────────────
#  geom_col() et non geom_histogram()/geom_bar() : les valeurs sont déjà
#  calculées, il n'y a rien à compter ni à agréger.
#  facettes : "grille"  -> classe × statut (les 6 panneaux d'un coup)
#             "classe"  -> une facette par classe d'âge (un statut par figure)
#             "aucune"  -> un seul panneau
graphe <- function(d, titre = NULL, facettes = "grille") {
  p <- ggplot(d, aes(x = quintile, y = valeur, fill = indicateur,
                     alpha = !fragile)) +
    geom_col(position = position_dodge(width = 0.8), width = 0.72) +
    geom_hline(yintercept = 0, linewidth = 0.4, colour = "grey20") +
    scale_alpha_manual(values = c(`TRUE` = 1, `FALSE` = 0.35), guide = "none") +
    scale_fill_manual(values = c("#4C72B0", "#DD8452", "#55A868")) +
    labs(x = "Quintile de niveau de vie", y = "Évolution 2006 → 2025",
         fill = NULL, title = titre,
         caption = paste("SRCV 2006 et 2025. Coût au m² en euros constants 2025.",
                         "Barres estompées : moins de", N_MIN, "observations.")) +
    theme_minimal(base_size = 11) +
    theme(legend.position = "top",
          panel.grid.major.x = element_blank(),
          plot.caption = element_text(size = 7, colour = "grey40", hjust = 0))

  p + switch(facettes,
    grille = if (FACETTE_INDICATEUR) facet_grid(indicateur ~ classe + statut3,
                                                scales = "free_y")
             else                    facet_grid(classe ~ statut3),
    classe = if (FACETTE_INDICATEUR) facet_grid(indicateur ~ classe, scales = "free_y")
             else                    facet_wrap(~ classe,
                                                ncol = if (PANNEAUX_EN_COLONNE) 1 else 3),
    aucune = if (FACETTE_INDICATEUR) facet_wrap(~ indicateur, ncol = 1, scales = "free_y")
             else                    NULL)
}

if (!dir.exists("Output")) dir.create("Output")

# 1) Les 6 panneaux en une figure
ggsave("Output/graph_quintiles.png", graphe(evo_long),
       width = 10, height = 9, dpi = 200, bg = "white")
cat("Écrit : Output/graph_quintiles.png\n")

# 2) Un fichier par statut : 3 panneaux, un par classe d'âge
slug_statut <- function(x) if (grepl("^Loc", x)) "locataires" else "accedants"

if (DECOUPAGE == "statut") {
  evo_long %>%
    group_by(statut3) %>%
    group_walk(function(d, cle) {
      f <- paste0("Output/graph_quintiles_", slug_statut(cle$statut3), ".png")
      ggsave(f, graphe(d, titre = as.character(cle$statut3), facettes = "classe"),
             width  = if (PANNEAUX_EN_COLONNE) 7 else 12,
             height = if (PANNEAUX_EN_COLONNE) 9 else 4.5, dpi = 200, bg = "white")
      cat("Écrit :", f, "\n")
    }, .keep = TRUE)

} else {   # "cellule" : les 6 graphiques séparément
  evo_long %>%
    group_by(classe, statut3) %>%
    group_walk(function(d, cle) {
      f <- paste0("Output/graph_quintiles_", gsub("[^0-9]", "", cle$classe), "_",
                  slug_statut(cle$statut3), ".png")
      ggsave(f, graphe(d, titre = paste0(cle$classe, " — ", cle$statut3),
                       facettes = "aucune"),
             width = 6.5, height = 4.5, dpi = 200, bg = "white")
      cat("Écrit :", f, "\n")
    }, .keep = TRUE)
}


# ==========================================================================
#  Tableau descriptif par statut
#  (ex-R/19_descriptif_par_statut_latex.R)
# ==========================================================================

# ============================================================================
#  SRCV — Statistiques descriptives par STATUT D'OCCUPATION, N millésimes
# ----------------------------------------------------------------------------
#  Une ligne = une variable quantitative, une colonne = un statut d'occupation.
#  Chaque case juxtapose les millésimes de ANNEES, chacun dans sa couleur, plus
#  (en option) l'écart entre le dernier et le premier.
#
#  Champ : ménages comportant une femme de 15-49 ans — couples ET familles
#  monoparentales (la restriction aux couples est levée depuis l’ajout de la
#  ligne "familles monoparentales", qui vaudrait sinon 0 partout).
#  Millésimes : réglés par ANNEES ci-dessous — c'est le seul paramètre à
#  changer, tout le reste (couleurs, titre, notes) s'y adapte.
#
#  ⚠️ NE PAS UTILISER 2023 NI 2024 : anomalie de pondération diagnostiquée dans
#     R/17 et R/18 (rapport du poids moyen naissance / non-naissance de 0,62 en
#     2023 contre 1,09 à 1,30 sur les vagues saines). Le script refuse ces deux
#     millésimes.
#
#  Toutes les moyennes sont PONDÉRÉES. Les montants sont en EUROS CONSTANTS
#  2025 (IPC Insee) : les coûts sont relevés à la date d'enquête et déflatés par
#  l'indice de l'année N, les revenus portent sur N-1 et le sont par celui de N-1.
#
#  Sorties : Output/descriptif_par_statut.csv
#            Output/tableau_descriptif_statut.tex
#  Préambule LaTeX requis : booktabs, xcolor, threeparttable
# ============================================================================



#  2014 et non 2010 comme première borne : le bloc Géographie repose sur la
#  tranche d'UNITÉ urbaine, qui n'existe qu'à partir de 2014. 2006 et 2010 ne
#  portent que TAU99 (AIRE urbaine), non comparable — cf. recode_densite()
#  dans 00_utils.R. Remettre "2010" affiche des --- sur tout le bloc.
ANNEES         <- c("2014", "2018", "2025")
AGE_BAS        <- 15
AGE_HAUT       <- 49
AFFICHER_ECART <- FALSE    # TRUE : ajoute (dernier - premier) en gris dans chaque case
#  Taille de police du tableau. Du plus petit au plus grand :
#  scriptsize < footnotesize < small < normalsize. Vérifier l'absence
#  d'overfull box après changement — le tableau a 10 colonnes.
TAILLE  <- "small"
ESP_GRP <- 5               # espace (pt) entre les groupes de statuts

stopifnot("2023 et 2024 sont inexploitables (anomalie de pondération, cf. R/17-18)" =
            !any(ANNEES %in% c("2023", "2024")))
A_PREM <- ANNEES[1]; A_DERN <- ANNEES[length(ANNEES)]

#  Palette temporelle : bleu -> ambre -> rouge, lisible en niveaux de gris.
PALETTE <- c("1F4E79", "B9770E", "C0392B", "6C3483", "117864")[seq_along(ANNEES)]
COUL    <- paste0("srcv", seq_along(ANNEES))
names(COUL) <- ANNEES

IPC <- charger_ipc()

# ── Âge moyen DU OU DES PARENTS : agrégé depuis le fichier INDIVIDUS ────────
#  Moyenne d'âge de la personne de référence et de son conjoint, enfants
#  EXCLUS (LIENPREF 00/01). Pour une famille monoparentale, c'est l'âge du
#  parent seul. Clé ménage gérée par cle_menage_ind() (00_utils.R).
ages <- imap_dfr(VAGUES[ANNEES],
                 ~ age_moyen_menage(.x, .y, liens = LIENS_PARENTS) %>% mutate(annee = .y))

# ── CHAMP : ménages comportant une femme de 15-49 ans ───────────────────────
#  La restriction aux couples est LEVÉE : sans les familles monoparentales, la
#  ligne "part de familles monoparentales" vaudrait 0 partout.
#  ⚠️ Le champ reste défini par la présence d'une FEMME de 15-49 ans (personne
#  de référence ou conjointe) : les familles monoparentales dirigées par un
#  HOMME en sont donc absentes. La part mesurée est celle des mères isolées.
d <- bind_rows(imap(VAGUES[ANNEES], ~ preparer_donnees(.y, .x, restreindre = FALSE))) %>%
  filter(!is.na(age_femme), age_femme >= AGE_BAS, age_femme <= AGE_HAUT,
         !is.na(statut3), !is.na(naissance), !is.na(poids), poids > 0) %>%
  mutate(id = normaliser_id(id)) %>%
  left_join(ages, by = c("annee", "id")) %>%
  mutate(monoparental = 100 * (couple != "En_couple" & nb_enf_avant >= 1),
         # Une indicatrice 0/100 par classe de densité : la moyenne pondérée
         # d'une indicatrice EST la part de la classe, et les cinq somment à 100.
         dens1 = 100 * (densite == DENSITE_LIB[1]),
         dens2 = 100 * (densite == DENSITE_LIB[2]),
         dens3 = 100 * (densite == DENSITE_LIB[3]),
         dens4 = 100 * (densite == DENSITE_LIB[4]),
         dens5 = 100 * (densite == DENSITE_LIB[5]),
         an = as.integer(annee),
         # En MILLIERS d'euros : sinon ces deux lignes imposent des colonnes de
         # 6 caractères ("59 639") alors que le reste du tableau en compte 4 ou
         # 5, et toutes les valeurs courtes se retrouvent loin de leur barre de
         # séparation, l'alignement à droite creusant l'écart à gauche.
         revenu_reel     = deflater(revenu,     an - 1, IPC, base = 2025) / 1000,
         niveau_vie_reel = deflater(niveau_vie, an - 1, IPC, base = 2025) / 1000,
         cout_log_reel   = deflater(cout_log,   an,     IPC, base = 2025),
         cout_m2_reel    = deflater(cout_m2,    an,     IPC, base = 2025),
         naiss_1000      = 1000 * (naissance == "Oui"))

cat("Appariement du fichier individus (âge moyen du ménage) :\n")
print(as.data.frame(d %>% group_by(annee) %>%
  summarise(n = n(), apparies_pct = round(100 * mean(!is.na(age_moyen)), 1),
            .groups = "drop")), row.names = FALSE)

# ── Variables du tableau : nom interne, libellé LaTeX, décimales, bloc ──────
VARS <- tribble(
  ~var,              ~libelle,                                          ~dec, ~bloc,
  "age_moyen",       "Âge moyen des parents (ans)",                         1, "Démographie",
  "taille_avant",    "Taille du ménage (avant naiss.)",                     2, "Démographie",
  "monoparental",    "Familles monoparentales (\\%)",                       1, "Démographie",
  "naiss_1000",      "Naissances pour 1\\,000 ménages",                     0, "Démographie",
  "revenu_reel",     "Revenu disponible (k\\texteuro/an)",                   1, "Ressources",
  "niveau_vie_reel", "Niveau de vie (k\\texteuro/an/UC)",                    1, "Ressources",
  "surface",         "Surface du logement (m\\textsuperscript{2})",         1, "Logement",
  "pieces",          "Nombre de pièces",                                    2, "Logement",
  "surf_pers",       "Surface par personne (m\\textsuperscript{2})",        1, "Logement",
  "surface_piece",   "Surface par pièce (m\\textsuperscript{2})",           1, "Logement",
  "cout_log_reel",   "Coût du logement (\\texteuro/mois)",                  0, "Coût",
  "cout_m2_reel",    "Coût au m\\textsuperscript{2} (\\texteuro/mois)",     2, "Coût",
  "taux_effort_pct", "Taux d'effort (\\%)",                                 1, "Coût",
  "dens1",           "Rural (\\%)",                                          1, "Géographie",
  "dens2",           "Unité urbaine < 20\\,000 hab. (\\%)",                  1, "Géographie",
  "dens3",           "20\\,000 à 199\\,999 hab. (\\%)",                      1, "Géographie",
  "dens4",           "200\\,000 hab. à 2 M (\\%)",                           1, "Géographie",
  "dens5",           "Agglomération de Paris (\\%)",                         1, "Géographie"
)

#  Libellés d'en-tête volontairement COURTS : chaque titre coiffe un
#  \multicolumn de 3 colonnes ; s'il est plus large que les données qu'il
#  surplombe, c'est lui qui fixe la largeur du groupe. "Propriétaires non
#  accédants" imposait à lui seul 16 pt de débord. Le sens complet est rappelé
#  dans les notes du tableau.
STATUTS <- c(Locataire                 = "Locataires",
             Proprietaire_accedant     = "Accédants",
             Proprietaire_non_accedant = "Non accédants")

# ── Moyennes pondérées par statut x millésime ───────────────────────────────
stats <- d %>%
  select(annee, statut3, poids, all_of(VARS$var)) %>%
  pivot_longer(all_of(VARS$var), names_to = "var", values_to = "x") %>%
  filter(!is.na(x)) %>%
  group_by(annee, statut3, var) %>%
  summarise(moyenne = weighted.mean(x, poids), n = n(), .groups = "drop")

parts <- d %>% group_by(annee, statut3) %>%
  summarise(w = sum(poids), n = n(), .groups = "drop") %>%
  group_by(annee) %>% mutate(part_pct = 100 * w / sum(w)) %>% ungroup()

if (!dir.exists("Output")) dir.create("Output")
write_csv(stats %>% left_join(parts %>% select(annee, statut3, part_pct, n_champ = n),
                              by = c("annee", "statut3")),
          "Output/descriptif_par_statut.csv")

# ── Mise en forme LaTeX ─────────────────────────────────────────────────────
fmt <- function(x, dec) {
  if (is.na(x)) return("---")
  s <- formatC(round(x, dec), format = "f", digits = dec, big.mark = "@")
  gsub("@", "\\\\,", sub(".", ",", s, fixed = TRUE))
}
fmt_signe <- function(x, dec) {
  s <- formatC(round(x, dec), format = "f", digits = dec, big.mark = "@", flag = "+")
  gsub("@", "\\\\,", sub(".", ",", s, fixed = TRUE))
}

# ── Colonnes : un millésime = une SOUS-COLONNE ──────────────────────────────
#  Les séparateurs "|" sont placés en @{...} entre deux colonnes alignées à
#  droite : ils tombent donc exactement au même endroit sur toutes les lignes,
#  ce qu'une case unique centrée ne permet pas. Chaque statut occupe
#  NCOL_GRP colonnes (un millésime chacune, plus l'écart si demandé).
NCOL_GRP <- length(ANNEES) + as.integer(AFFICHER_ECART)
NCOL_TOT <- 1 + NCOL_GRP * length(STATUTS)

spec_grp <- paste(rep("r", length(ANNEES)), collapse = "@{\\,\\textbar\\,}")
if (AFFICHER_ECART) spec_grp <- paste0(spec_grp, "@{~}l")
SPEC <- paste0("l@{\\hspace{", ESP_GRP, "pt}}",
               paste(rep(spec_grp, length(STATUTS)),
                     collapse = paste0("@{\\hspace{", ESP_GRP + 2, "pt}}")))

# Bornes de chaque groupe de colonnes (pour \multicolumn et \cmidrule)
grp_debut <- 2 + NCOL_GRP * (seq_along(STATUTS) - 1)
grp_fin   <- 1 + NCOL_GRP * seq_along(STATUTS)

# Une case = NCOL_GRP cellules séparées par & (un millésime par colonne)
case <- function(v, dec) {
  cel <- sprintf("\\textcolor{%s}{%s}", COUL, map_chr(v, fmt, dec = dec))
  if (!AFFICHER_ECART) return(paste(cel, collapse = " & "))
  ec <- v[length(v)] - v[1]
  ecart <- if (is.na(ec)) "" else
    paste0("{\\scriptsize\\color{srcvE}(", fmt_signe(ec, dec), ")}")
  paste(c(cel, ecart), collapse = " & ")
}

large <- stats %>%
  pivot_wider(names_from = annee, values_from = moyenne, id_cols = c(var, statut3)) %>%
  left_join(VARS, by = "var")

ligne_var <- function(v) {
  r <- large %>% filter(var == v)
  cellules <- map_chr(names(STATUTS), function(st) {
    x <- r %>% filter(statut3 == st)
    if (!nrow(x)) return(paste(rep("---", NCOL_GRP), collapse = " & "))
    case(as.numeric(x[1, ANNEES]), x$dec[1])
  })
  paste0("    ", VARS$libelle[VARS$var == v], " & ", paste(cellules, collapse = " & "), " \\\\")
}

# Corps du tableau, regroupé par bloc thématique
corps <- map_chr(unique(VARS$bloc), function(b) {
  vs <- VARS$var[VARS$bloc == b]
  paste0("    \\multicolumn{", NCOL_TOT,
         "}{l}{\\textit{", b, "}} \\\\\n", paste(map_chr(vs, ligne_var), collapse = "\n"))
}) %>% paste(collapse = "\n    \\addlinespace\n")

ligne_bas <- function(champ, dec, libelle) {
  paste0("    ", libelle, " & ",
    paste(map_chr(names(STATUTS), function(st) {
      p <- parts %>% filter(statut3 == st)
      case(map_dbl(ANNEES, ~ p[[champ]][p$annee == .x]), dec)
    }), collapse = " & "), " \\\\")
}

# Ligne d'en-tête des millésimes : UNE CELLULE PAR SOUS-COLONNE, et non un
# \multicolumn centré. Les années tombent donc exactement au-dessus de leurs
# valeurs, et les séparateurs affichés sont ceux du tabular (@{\,\textbar\,}).
entete_annees <- paste(rep(
  paste(c(sprintf("\\textcolor{%s}{%s}", COUL, ANNEES),
          if (AFFICHER_ECART) "" else NULL), collapse = " & "),
  length(STATUTS)), collapse = " & ")

entete_couleurs <- paste(sprintf("\\textcolor{%s}{%s}", COUL, ANNEES),
                         collapse = "\\,\\textbar\\,")

tex <- paste0(
"% ==========================================================================\n",
"%  Généré par R/19_descriptif_par_statut_latex.R — NE PAS ÉDITER À LA MAIN\n",
"%  Préambule requis : \\usepackage{booktabs} \\usepackage{xcolor}\n",
"%                     \\usepackage[flushleft]{threeparttable}\n",
"% ==========================================================================\n",
paste(sprintf("\\definecolor{%s}{HTML}{%s}", COUL, PALETTE), collapse = "\n"), "\n",
"\\definecolor{srcvE}{HTML}{707070}   % écart — gris\n\n",
"\\begin{table}[htbp]\n  \\centering\n  \\", TAILLE, "\n",
"  \\setlength{\\tabcolsep}{2pt}\n  \\begin{threeparttable}\n",
"  \\caption{Caractéristiques des ménages comportant une femme de ", AGE_BAS, " à ", AGE_HAUT,
   " ans, selon le statut d'occupation : ", entete_couleurs, "}\n",
"  \\label{tab:descriptif_statut}\n",
"  \\begin{tabular}{", SPEC, "}\n",
"    \\toprule\n",
"    & ", paste(sprintf("\\multicolumn{%d}{c}{%s}", NCOL_GRP, STATUTS),
               collapse = " & "), " \\\\\n",
paste(sprintf("    \\cmidrule(lr){%d-%d}", grp_debut, grp_fin), collapse = " "), "\n",
"    & ", entete_annees, " \\\\\n",
"    \\midrule\n",
corps, "\n",
"    \\addlinespace\n    \\midrule\n",
ligne_bas("part_pct", 1, "Part dans le champ (\\%)"), "\n",
ligne_bas("n", 0, "Effectif (non pondéré)"), "\n",
"    \\bottomrule\n  \\end{tabular}\n",
"  \\begin{tablenotes}[flushleft]\\scriptsize\n",
"    \\item \\textit{Lecture} : chaque case donne les moyennes pondérées de ",
   entete_couleurs,
   if (AFFICHER_ECART) paste0(", puis entre parenthèses l'écart ", A_PREM, "--", A_DERN)
   else "", ".\n",
"    \\item \\textit{Statuts} : \\textit{Accédants} = propriétaires remboursant un emprunt sur\n",
"      leur résidence principale ; \\textit{Non accédants} = propriétaires sans emprunt en cours.\n",
"    \\item \\textit{Champ} : ménages comportant une femme de ", AGE_BAS, " à ", AGE_HAUT,
"      ans (personne de référence ou conjointe), France entière --- couples et familles\n",
"      monoparentales. Les familles monoparentales dirigées par un homme en sont absentes\n",
"      par construction : la part reportée est celle des mères isolées.\n",
"    \\item \\textit{Âge moyen du ou des parents} : moyenne d'âge de la personne de référence\n",
"      et de son conjoint, \\textbf{enfants exclus} (fichier individus). Pour une famille\n",
"      monoparentale, c'est l'âge du parent seul.\n",
"    \\item \\textit{Familles monoparentales} : ménages hors couple ayant au moins un enfant.\n",
"    \\item \\textit{Géographie} : tranche d'unité urbaine de la commune de résidence, regroupée\n",
"      en cinq classes ; les cinq parts somment à 100\\,\\% dans chaque colonne. Source :\n",
"      \\texttt{tuu10} (2014, 2018, population 2010) puis \\texttt{TUU2017} (2025, population\n",
"      2017) --- même concept, seule l'année de référence du recensement change.\n",
"      Le degré d'urbanisation \\texttt{DB100} n'est pas utilisé : la nomenclature Eurostat\n",
"      DEGURBA a été révisée vers 2011-2012 et ses niveaux ne sont pas comparables entre\n",
"      vagues. Les millésimes antérieurs à 2014 ne portent que \\texttt{TAU99}, qui mesure des\n",
"      \\textit{aires} urbaines et non des \\textit{unités} urbaines.\n",
"    \\item \\textit{Montants} : euros constants 2025 (IPC Insee) ; k\\texteuro{} = milliers d'euros.\n",
"      Coût du logement =\n",
"      charges (\\texttt{HH070}) + mensualité de remboursement (\\texttt{REMP}/\\texttt{REMPR}).\n",
"      Taux d'effort = coût annuel rapporté au revenu disponible, borné à [0 ; 100]\\,\\%.\n",
"    \\item \\textit{Naissances} : ménages déclarant une naissance récente, pour 1\\,000\n",
"      ménages du statut.",
if (any(as.integer(ANNEES) <= 2018) && any(as.integer(ANNEES) >= 2022))
  paste0(" \\textbf{Attention} : la naissance est mesurée par \\texttt{EVENEMEN\\_C}\n",
         "      (naissance dans l'année) jusqu'en 2018 et reconstruite depuis le fichier individus\n",
         "      (enfant né en N ou N-1) à partir de 2022. L'écart franchissant cette rupture mélange\n",
         "      évolution réelle et changement de mesure ; les sous-périodes situées d'un même côté\n",
         "      de 2018 sont, elles, comparables.\n")
else " Même définition sur tous les millésimes retenus.\n",
"    \\item \\textit{Millésimes} : 2023 et 2024 sont exclus (anomalie de pondération\n",
"      documentée dans R/17--18).\n",
"    \\item \\textit{Source} : Insee, SRCV ", paste(ANNEES, collapse = ", "),
   ", calculs de l'auteur.\n",
"  \\end{tablenotes}\n  \\end{threeparttable}\n\\end{table}\n")

writeLines(tex, "Output/tableau_descriptif_statut.tex", useBytes = TRUE)
cat("\nÉcrit : Output/tableau_descriptif_statut.tex et descriptif_par_statut.csv\n\n")

cat("=== Aperçu (", paste(ANNEES, collapse = " | "), " | écart) ===\n", sep = "")
print(as.data.frame(large %>%
  rowwise() %>%
  mutate(v = paste0(paste(round(c_across(all_of(ANNEES)), 1), collapse = " | "),
                    sprintf("  (%+.1f)", .data[[A_DERN]] - .data[[A_PREM]]))) %>%
  ungroup() %>%
  transmute(Variable = libelle, statut3, v) %>%
  pivot_wider(names_from = statut3, values_from = v)), row.names = FALSE)
