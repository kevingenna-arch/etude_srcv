# ============================================================================
#  SRCV — Fonctions utilitaires communes (sourcé par 02 à 07)
# ============================================================================
#  À sourcer depuis la RACINE du projet :  source("R/00_utils.R")
# ============================================================================

suppressPackageStartupMessages(library(tidyverse))

# ── Lecture robuste d'un fichier SRCV ────────────────────────────────────────
#  - décimale '.' (tous les millésimes)
#  - AUTO-DÉTECTION du séparateur de colonnes : ';' (2006-2023) ou ',' (2024-2025)
#  - CACHE .rds : la 1re lecture met en cache dans Data/_cache/, les suivantes
#    relisent le .rds (≈5-10x plus rapide). Le cache est invalidé si le CSV
#    change (clé = chemin + date de modification).
lire_srcv <- function(path, cache = TRUE) {
  if (!file.exists(path))
    stop("Fichier introuvable : ", path,
         "\n(le working directory doit être la racine du projet etude_srcv)", call. = FALSE)

  f_cache <- NULL
  if (cache) {
    dir_cache <- file.path("Data", "_cache")
    if (!dir.exists(dir_cache)) dir.create(dir_cache, recursive = TRUE)
    cle <- paste0(gsub("[^A-Za-z0-9]+", "_", path), "__", as.integer(file.mtime(path)), ".rds")
    f_cache <- file.path(dir_cache, cle)
    if (file.exists(f_cache)) return(readRDS(f_cache))
  }

  l1    <- readLines(path, n = 1, warn = FALSE)
  delim <- if (str_count(l1, ";") >= str_count(l1, ",")) ";" else ","
  d <- suppressWarnings(read_delim(
    path, delim = delim,
    locale = locale(decimal_mark = ".", grouping_mark = ""),
    show_col_types = FALSE, progress = FALSE))

  if (cache) saveRDS(d, f_cache)
  d
}

# ── Petits utilitaires ───────────────────────────────────────────────────────
# Conversion numérique (sécurise les espaces parasites des colonnes caractère).
num <- function(x) as.numeric(trimws(as.character(x)))

# Écart-type pondéré.
wsd <- function(x, w) { m <- weighted.mean(x, w); sqrt(weighted.mean((x - m)^2, w)) }

# Garde-fou : stoppe avec un message clair si des colonnes attendues manquent.
verifier_colonnes <- function(d, cols, contexte = "") {
  manq <- setdiff(unname(cols), names(d))
  if (length(manq))
    stop("Colonnes absentes ", contexte, " : ", paste(manq, collapse = ", "), call. = FALSE)
  invisible(TRUE)
}

# ── Indice des prix à la consommation (INSEE, base 2025, hors tabac) ────────
# Fichier "valeurs mensuelles" INSEE : 4 lignes d'en-tête, puis
# "AAAA-MM";"valeur";"code" répétés (colonnes = millésimes successifs de
# publication ; la colonne 2 est le millésime le plus récent).
# Renvoie l'indice MOYEN ANNUEL par année.
charger_ipc <- function(path = "Data/valeurs_mensuelles.csv") {
  br <- suppressWarnings(read_delim(path, delim = ";", skip = 4, col_names = FALSE,
          locale = locale(decimal_mark = "."), show_col_types = FALSE, progress = FALSE))
  tibble(periode = as.character(br[[1]]), val = as.numeric(br[[2]])) %>%
    filter(!is.na(val), grepl("^[0-9]{4}-[0-9]{2}$", periode)) %>%
    mutate(annee = as.integer(substr(periode, 1, 4))) %>%
    group_by(annee) %>%
    summarise(ipc = mean(val), n_mois = n(), .groups = "drop") %>%
    filter(n_mois == 12) %>%          # écarte les années incomplètes
    select(annee, ipc)
}

# Passage en euros constants d'une année de base.
#   x      : montant en euros courants
#   annee  : année à laquelle le montant est exprimé
#   ipc    : table issue de charger_ipc()
deflater <- function(x, annee, ipc, base = 2025) {
  i_base <- ipc$ipc[ipc$annee == base]
  if (length(i_base) != 1) stop("Année de base absente de l'IPC : ", base, call. = FALSE)
  idx <- ipc$ipc[match(as.integer(annee), ipc$annee)]
  x * i_base / idx
}

# ── Recodages métier ─────────────────────────────────────────────────────────
# Statut d'occupation -> binaire, selon le type de variable source.
recode_occ <- function(x, type) {
  x <- trimws(as.character(x))
  if (type == "hx070") {
    # HX070 : 1 = Propriétaire/logé gratuit, 2 = Locataire
    factor(x, levels = c("1", "2"), labels = c("Proprietaire_ou_gratuit", "Locataire"))
  } else if (type == "stoc") {
    # STOC (6 modalités) : {1,2,3,6} = proprio/gratuit ; {4,5} = locataire
    out <- rep(NA_character_, length(x))
    out[x %in% c("1", "2", "3", "6")] <- "Proprietaire_ou_gratuit"
    out[x %in% c("4", "5")]           <- "Locataire"
    factor(out, levels = c("Proprietaire_ou_gratuit", "Locataire"))
  } else stop("occ_type inconnu : ", type, call. = FALSE)
}

# Statut d'occupation en 3 postes : locataire / propriétaire ACCÉDANT (qui
# rembourse un emprunt sur sa résidence principale) / propriétaire NON accédant.
# Critère d'accession = REMP/REMPR > 0 (cumul mensuel des emprunts liés à la
# résidence principale). Cohérent sur les 8 vagues, alors que la modalité
# "accédant" de STOC n'existe pas avant 2018.
# Vérifié sur 2025 : 4 300 des 4 395 ménages STOC=1 (accédant) ont REMPR > 0,
# et aucun locataire ni propriétaire non accédant n'a de remboursement. Les ~91
# accédants sans montant renseigné sont classés "non accédant" (2 % du groupe).
# NB : REMP/REMPR est NA quand il n'y a pas d'emprunt -> coalesce(., 0) en amont.
recode_statut3 <- function(statut_occ, rembours) {
  loc <- as.character(statut_occ) == "Locataire"
  acc <- coalesce(rembours, 0) > 0
  factor(case_when(is.na(statut_occ) ~ NA_character_,
                   loc              ~ "Locataire",
                   acc              ~ "Proprietaire_accedant",
                   TRUE             ~ "Proprietaire_non_accedant"),
         levels = c("Locataire", "Proprietaire_accedant", "Proprietaire_non_accedant"))
}

# Degré d'urbanisation (DB100, variable EU-SILC harmonisée, présente dans les
# 8 vagues) : 1 = forte densité (urbain dense), 2 = densité intermédiaire
# (périurbain / villes moyennes), 3 = faible densité (rural).
# ⚠️ La nomenclature Eurostat DEGURBA a été révisée vers 2011-2012 : la
# répartition change nettement entre 2010 et 2014. En contrôle avec effets
# fixes année c'est acceptable, mais ne pas comparer les NIVEAUX de part
# "intermédiaire"/"rural" de part et d'autre de cette rupture.
recode_urbain <- function(x) {
  x <- trimws(as.character(x))
  factor(case_when(x == "1" ~ "Dense",
                   x == "2" ~ "Intermediaire",
                   x == "3" ~ "Peu_dense",
                   TRUE     ~ NA_character_),
         levels = c("Dense", "Intermediaire", "Peu_dense"))
}

# Vie en couple (COUPLEPR) : 1 = conjoint dans le logement (réf.) ; 2,3 = hors couple.
recode_couple <- function(x) {
  x <- trimws(as.character(x))
  factor(case_when(x == "1"           ~ "En_couple",
                   x %in% c("2", "3") ~ "Hors_couple",
                   TRUE               ~ NA_character_),
         levels = c("En_couple", "Hors_couple"))
}

# ── Statut d'activité dominant sur l'année N-1 (fichiers FPR ≥ 2022) ─────────
# = le poste NBM_* où la personne a passé le plus de mois.
ETATS_NBM <- c(SALFT = "TP",      SALPT = "PT",      INDFT = "TP", INDPT = "PT",
               CHO   = "Chomage", FOY   = "Inactif", RET   = "Inactif",
               ETU   = "Inactif", INA   = "Inactif", INV   = "Inactif", MAT = "Inactif")

statut_dominant <- function(d, etats = ETATS_NBM) {
  cols <- paste0("NBM_", names(etats))
  verifier_colonnes(d, cols, "(fichier individus, variables NBM_*)")
  M <- d %>% select(all_of(cols)) %>%
    mutate(across(everything(), ~ coalesce(num(.), 0))) %>% as.matrix()
  vide <- rowSums(M) == 0
  idx  <- max.col(M, ties.method = "first")
  tibble(
    statut = ifelse(vide, NA_character_, unname(etats[idx])),   # TP/PT/Chomage/Inactif
    brut   = ifelse(vide, NA_character_, names(etats)[idx])     # SALFT/FOY/RET/...
  )
}
