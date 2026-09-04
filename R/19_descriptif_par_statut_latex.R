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

source("R/00_prepa_fecondite.R")

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
