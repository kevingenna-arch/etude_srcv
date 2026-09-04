# ============================================================================
#  SRCV — 26. INDICE DE PRIX DVF+ PAR ZEAT, ANNÉE, TYPE ET NOMBRE DE PIÈCES
# ----------------------------------------------------------------------------
#  Construit un indice de prix au m² à partir des transactions réelles DVF+
#  (Cerema), agrégé à la maille ZEAT — la plus fine que SRCV permette sur les
#  8 vagues — pour servir de signal de MARCHÉ, indépendant du logement que
#  chaque ménage SRCV occupe réellement (contrairement à cout_m2, déclaratif
#  et donc en partie endogène au choix du ménage). Sert de CONTRÔLE DE
#  ROBUSTESSE, pas de variable centrale : DVF+ ne couvre que 2014-2025 (6 des
#  8 vagues), et ne porte que sur le marché de l'ACHAT (pas les loyers).
#
#  SOURCE : livraison DVF+ complète, 97 fichiers départementaux, pipe-délimité,
#  `Immobilier/données Immo fécondité/1_DONNEES_LIVRAISON/dvf_plus_dNN.csv`.
#  Format validé sur Paris (75) et la Lozère (48) : en-têtes identiques sur
#  les 97 fichiers, cohérence parfaite des champs nbaptXpp/nbmaiXpp (leur
#  somme vaut toujours 1 sur les mutations d'un seul bien), niveaux de prix
#  conformes à l'historique connu du marché parisien (pic 2020 à ~10 900 €/m²).
#
#  MÉTHODE :
#   1. Champ : mutations d'UN SEUL bien (nblot=1, nbcomm=1), codtypbien 111
#      (UNE MAISON) ou 121 (UN APPARTEMENT) -- exclut les ventes multi-lots,
#      terrains, locaux d'activité, où le prix ne s'attribue pas proprement
#      à une surface/un nombre de pièces.
#   2. Pièces et surface lues dans nbapt1pp..5pp/sapt1pp..5pp (appartements)
#      ou nbmai1pp..5pp/smai1pp..5pp (maisons) -- la catégorie à 1 dans
#      nbXXXpp donne le nombre de pièces, sXXXpp la surface correspondante.
#   3. Agrégation à (ZEAT, année, type de bien, tranche de pièces) : médiane
#      du prix au m², nombre de transactions.
#   4. Département -> ZEAT via la même table que SRCV (ancienne région ->
#      ZEAT, 00_utils.R), en passant par une table statique département ->
#      ancienne région (nomenclature stable, antérieure à la réforme
#      régionale de 2016 -- la même contrainte qui explique pourquoi SRCV
#      n'expose que la ZEAT).
#
#  ⚠️ LIMITES ASSUMÉES :
#   - Couvre 2014-2025 seulement (pas 2006/2010) : inutilisable pour la
#     décomposition 2010-2025 du script 23 telle quelle.
#   - Prix d'ACHAT, pas de loyer : pertinent pour le canal accession, proxy
#     imparfait pour les locataires (marchés corrélés localement, pas
#     identiques).
#   - Granularité écrasée à la ZEAT (9 zones) pour être joignable à SRCV,
#     alors que DVF+ descend à la parcelle -- l'essentiel de la richesse
#     géographique de la donnée n'est pas exploité ici.
#
#  À LANCER DEPUIS LA RACINE DU PROJET (long : ~97 fichiers, 8,3 Go) :
#     source("R/26_dvf_zeat.R")
#
#  Sortie : Output/dvf_zeat_annee.csv
# ============================================================================

source("R/00_utils.R")
library(readr)
if (!dir.exists("Output")) dir.create("Output")

source("R/00_config.R")   # DIR_DVF : chemin des fichiers DVF+ (source unique des chemins)

# ── Table statique : département -> ancienne région (pré-2016) ─────────────
#  Nomenclature stable, INSEE. Réutilise REGION_VERS_ZEAT (00_utils.R) pour
#  le dernier pas ancienne région -> ZEAT, exactement la table qui répare
#  ZEAT en 2006 dans le pipeline SRCV.
DEPT_VERS_REGION <- c(
  "01"="82","02"="22","03"="83","04"="93","05"="93","06"="93","07"="82",
  "08"="21","09"="73","10"="21","11"="91","12"="73","13"="93","14"="25",
  "15"="83","16"="54","17"="54","18"="24","19"="74","2A"="94","2B"="94",
  "21"="26","22"="53","23"="74","24"="72","25"="43","26"="82","27"="23",
  "28"="24","29"="53","30"="91","31"="73","32"="73","33"="72","34"="91",
  "35"="53","36"="24","37"="24","38"="82","39"="43","40"="72","41"="24",
  "42"="82","43"="83","44"="52","45"="24","46"="73","47"="72","48"="91",
  "49"="52","50"="25","51"="21","52"="21","53"="52","54"="41","55"="41",
  "56"="53","57"="41","58"="26","59"="31","60"="22","61"="25","62"="31",
  "63"="83","64"="72","65"="73","66"="91","67"="42","68"="42","69"="82",
  "70"="43","71"="26","72"="52","73"="82","74"="82","75"="11","76"="23",
  "77"="11","78"="11","79"="54","80"="22","81"="73","82"="73","83"="93",
  "84"="93","85"="52","86"="54","87"="74","88"="41","89"="26","90"="43",
  "91"="11","92"="11","93"="11","94"="11","95"="11",
  "971"="0","972"="0","973"="0","974"="0","976"="0")

dept_vers_zeat <- function(dept) {
  dept <- toupper(trimws(dept))
  reg <- unname(DEPT_VERS_REGION[dept])
  unname(REGION_VERS_ZEAT[reg])
}

# ── Traitement d'un département ─────────────────────────────────────────────
COLS_DVF <- cols_only(
  anneemut = col_integer(), coddep = col_character(),
  valeurfonc = col_double(), nblot = col_double(), nbcomm = col_double(),
  nbapt1pp = col_double(), nbapt2pp = col_double(), nbapt3pp = col_double(),
  nbapt4pp = col_double(), nbapt5pp = col_double(),
  sapt1pp = col_double(), sapt2pp = col_double(), sapt3pp = col_double(),
  sapt4pp = col_double(), sapt5pp = col_double(),
  nbmai1pp = col_double(), nbmai2pp = col_double(), nbmai3pp = col_double(),
  nbmai4pp = col_double(), nbmai5pp = col_double(),
  smai1pp = col_double(), smai2pp = col_double(), smai3pp = col_double(),
  smai4pp = col_double(), smai5pp = col_double(),
  codtypbien = col_character())

traiter_dept <- function(fichier) {
  d <- read_delim(fichier, delim = "|", col_types = COLS_DVF,
                  locale = locale(decimal_mark = "."), progress = FALSE)

  appart <- d %>% filter(codtypbien == "121", nblot == 1, nbcomm == 1) %>%
    mutate(
      pieces = case_when(nbapt1pp == 1 ~ 1L, nbapt2pp == 1 ~ 2L,
                         nbapt3pp == 1 ~ 3L, nbapt4pp == 1 ~ 4L,
                         nbapt5pp == 1 ~ 5L, TRUE ~ NA_integer_),
      surface = case_when(pieces == 1 ~ sapt1pp, pieces == 2 ~ sapt2pp,
                          pieces == 3 ~ sapt3pp, pieces == 4 ~ sapt4pp,
                          pieces == 5 ~ sapt5pp, TRUE ~ NA_real_),
      type_bien = "Appartement")

  maison <- d %>% filter(codtypbien == "111", nblot == 1, nbcomm == 1) %>%
    mutate(
      pieces = case_when(nbmai1pp == 1 ~ 1L, nbmai2pp == 1 ~ 2L,
                         nbmai3pp == 1 ~ 3L, nbmai4pp == 1 ~ 4L,
                         nbmai5pp == 1 ~ 5L, TRUE ~ NA_integer_),
      surface = case_when(pieces == 1 ~ smai1pp, pieces == 2 ~ smai2pp,
                          pieces == 3 ~ smai3pp, pieces == 4 ~ smai4pp,
                          pieces == 5 ~ smai5pp, TRUE ~ NA_real_),
      type_bien = "Maison")

  bind_rows(appart, maison) %>%
    filter(!is.na(pieces), !is.na(surface), surface >= 9, surface <= 300,
          !is.na(valeurfonc), valeurfonc > 1000, anneemut >= 2014) %>%
    transmute(coddep = coddep, zeat = dept_vers_zeat(coddep),
             annee = as.character(anneemut), type_bien, pieces,
             prix_m2 = valeurfonc / surface)
}

fichiers <- list.files(DIR_DVF, pattern = "^dvf_plus_d.*\\.csv$", full.names = TRUE)
cat("Fichiers departementaux DVF+ trouves :", length(fichiers), "\n")

t0 <- Sys.time()
transactions <- map_dfr(fichiers, function(f) {
  res <- tryCatch(traiter_dept(f), error = function(e) {
    message("  echec sur ", basename(f), " : ", conditionMessage(e)); NULL
  })
  cat(".")
  res
})
cat("\nTemps de traitement :", round(as.numeric(difftime(Sys.time(), t0, units = "mins")), 1), "min\n")
cat("Transactions exploitables (maison + appartement, mutation simple) :", nrow(transactions), "\n")

cat("\nRepartition par ZEAT (verification du passage departement -> ZEAT) :\n")
print(table(transactions$zeat, useNA = "ifany"))

# ── Agrégation ZEAT x annee x type x pieces ─────────────────────────────────
dvf_zeat <- transactions %>%
  filter(!is.na(zeat)) %>%
  group_by(zeat, annee, type_bien, pieces) %>%
  summarise(n = n(), prix_m2_median = median(prix_m2), .groups = "drop")

cat("\nAperçu (Region parisienne, appartements) :\n")
print(as.data.frame(dvf_zeat %>%
  filter(zeat == "1", type_bien == "Appartement") %>%
  arrange(pieces, annee) %>%
  mutate(prix_m2_median = round(prix_m2_median))), row.names = FALSE)

# ── Indice complémentaire : ZEAT x annee x type, tous nombres de pièces ─────
#  Utile pour un contrôle contextuel simple (sans distinguer par pièces).
dvf_zeat_global <- transactions %>%
  filter(!is.na(zeat)) %>%
  group_by(zeat, annee, type_bien) %>%
  summarise(n = n(), prix_m2_median = median(prix_m2), .groups = "drop")

# ── Indice "grand vs petit logement" : ratio 4-5 pieces / 1-2 pieces ───────
gradient_taille <- transactions %>%
  filter(!is.na(zeat), pieces != 3) %>%
  mutate(grp = if_else(pieces <= 2, "1-2p", "4-5p")) %>%
  group_by(zeat, annee, grp) %>%
  summarise(prix_m2_median = median(prix_m2), n = n(), .groups = "drop") %>%
  tidyr::pivot_wider(names_from = grp, values_from = c(prix_m2_median, n)) %>%
  mutate(ratio_grand_petit = `prix_m2_median_4-5p` / `prix_m2_median_1-2p`)

cat("\nGradient taille (ratio prix/m2 4-5p / 1-2p), 2025, par ZEAT :\n")
print(as.data.frame(gradient_taille %>% filter(annee == "2025") %>%
  select(zeat, ratio_grand_petit) %>%
  mutate(ratio_grand_petit = round(ratio_grand_petit, 2))), row.names = FALSE)

write_csv(dvf_zeat, "Output/dvf_zeat_annee_pieces.csv")
write_csv(dvf_zeat_global, "Output/dvf_zeat_annee.csv")
write_csv(gradient_taille, "Output/dvf_gradient_taille_zeat.csv")

cat("\n\n=== 26 termine. ===\n")
