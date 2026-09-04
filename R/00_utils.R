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

# ── Type de logement : maison vs appartement ────────────────────────────────
#  ⚠️ PIÈGE DE NOMENCLATURE : la variable change de NOM et de CODES entre
#  vagues, et "TYPLOG" porte deux nomenclatures DIFFÉRENTES sous le même nom :
#    - "hh010"        (2006, 2010, 2014, variable HH010) : 1-2 = maison,
#      3-4 = appartement, 5 = autre. Stable sur les 3 vagues (vérifié via les
#      catalogues de formats SAS de chaque millésime).
#    - "typlog_ancien" (2018, variable TYPLOG) : 1-2 = maison, 3-4-5 =
#      appartement (5 = "immeuble de 10 logements ou plus" dans CETTE
#      nomenclature), 6-7 = autre.
#    - "typlog_nouveau" (2022-2025, variable TYPLOG) : 1-2 = maison, 3-4 =
#      appartement, 5-9 = autre (5 = "foyer" dans CETTE nomenclature -- un
#      sens complètement différent du "5" de 2018, malgré le même nom de
#      variable et code SRCV lil ne le signalant nulle part).
#  Vérifié par un croisement HH010 x TYPLOG sur 2014 (seule vague où les deux
#  coexistent) : bijection exacte une fois les bons codes appliqués, aucune
#  discordance sur 11 384 ménages.
recode_typlog <- function(x, type) {
  x <- trimws(as.character(x))
  appart_codes <- switch(type,
    hh010          = c("3", "4"),
    typlog_ancien  = c("3", "4", "5"),
    typlog_nouveau = c("3", "4"),
    stop("typlog_type inconnu : ", type, call. = FALSE))
  factor(case_when(x %in% c("1", "2")   ~ "Maison",
                   x %in% appart_codes ~ "Appartement",
                   TRUE                ~ NA_character_),
         levels = c("Maison", "Appartement"))
}

# ── ZEAT : seule maille géographique commune aux 8 vagues ───────────────────
#  La région (DB040/REGION) n'existe qu'en 2006 : l'Insee l'a retirée des
#  fichiers de diffusion dès 2010 et ne l'a jamais rétablie. ZEAT (8 grandes
#  zones + DOM) est donc la seule géographie utilisable en série longue. Elle a
#  au passage l'avantage d'être insensible à la réforme régionale de 2016.
#
#  ⚠️ NON-RÉPONSE — ZEAT est vide pour 13,5 % des ménages (pondéré) en 2006,
#     21,4 % en 2010 et 22,9 % en 2014, puis 0 % à partir de 2018. Ces ménages
#     sont répartis sur toutes les régions : c'est une non-réponse, pas une
#     modalité. En 2006 elle est RÉPARABLE (REGION est renseignée) ; en 2010 et
#     2014 elle ne l'est pas -> modalité explicite "Non renseigne".
#
#  ⚠️ CODE 6 — 2014 porte 403 ménages codés "6", modalité absente de la
#     nomenclature officielle (Insee 2025 : 0,1,2,3,4,5,7,8,9). Conservée comme
#     niveau distinct plutôt que fusionnée arbitrairement : à élucider dans la
#     documentation 2014 avant toute publication par ZEAT.
ZEAT_LIB <- c("0" = "DOM",           "1" = "Region parisienne",
              "2" = "Bassin parisien","3" = "Nord",
              "4" = "Est",            "5" = "Ouest",
              "6" = "Code 6 (2014)",  "7" = "Sud-Ouest",
              "8" = "Centre-Est",     "9" = "Mediterranee")

# Anciennes régions (codes Insee) -> ZEAT. Sert à réparer 2006.
REGION_VERS_ZEAT <- c("11"="1",
                      "21"="2","22"="2","23"="2","24"="2","25"="2","26"="2",
                      "31"="3",
                      "41"="4","42"="4","43"="4",
                      "52"="5","53"="5","54"="5",
                      "72"="7","73"="7","74"="7",
                      "82"="8","83"="8",
                      "91"="9","93"="9","94"="9")

recode_zeat <- function(x, region = NULL) {
  x <- trimws(as.character(x))
  x[x == ""] <- NA_character_
  if (!is.null(region)) {                      # réparation par la région (2006)
    r <- trimws(as.character(region))
    x <- if_else(is.na(x) & !is.na(r) & r %in% names(REGION_VERS_ZEAT),
                 unname(REGION_VERS_ZEAT[r]), x)
  }
  lib <- unname(ZEAT_LIB[x])
  factor(if_else(is.na(lib), "Non renseigne", lib),
         levels = c(unname(ZEAT_LIB), "Non renseigne"))
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

# ============================================================================
#  BRIQUES PARTAGÉES — factorisées depuis 05, 09, 10 et 12 (mêmes 20 lignes
#  recopiées dans chaque script). Toute correction se fait désormais ici.
# ============================================================================

# ── Taux d'effort logement, en POINTS de % ──────────────────────────────────
#  Coût MENSUEL annualisé / revenu disponible ANNUEL. Revenus <= 0 -> NA (un
#  taux d'effort n'a pas de sens), valeurs extrêmes CENSURÉES dans [min ; max]
#  plutôt que supprimées.
#  ⚠️ 02_analyse_multimillesimes.R utilise une autre convention : il SUPPRIME
#  les lignes hors [0 ; 100] au lieu de les censurer. Les champs ne sont donc
#  pas identiques entre 02 et 09/12 — ne pas comparer les n directement.
taux_effort <- function(cout, revenu, min = 0, max = 100) {
  if_else(!is.na(revenu) & revenu > 0,
          pmin(pmax(100 * (cout * 12) / revenu, min), max),
          NA_real_)
}

# ── Quantiles pondérés ──────────────────────────────────────────────────────
#  Remplace trois implémentations identiques (médiane de 09, wquantile de 12,
#  qcut de 10). Renvoie la plus petite valeur dont le poids cumulé atteint p.
wquantile <- function(x, w, probs) {
  ok <- !is.na(x) & !is.na(w) & w > 0
  x <- x[ok]; w <- w[ok]
  o  <- order(x); x <- x[o]; w <- w[o]
  cw <- cumsum(w) / sum(w)
  vapply(probs, function(p) x[which(cw >= p)[1]], numeric(1))
}

# ── Rang pondéré et transposition rang-préservante ──────────────────────────
#  Briques des contrefactuels (23, 28) : chaque observation reçoit la valeur
#  située au MÊME rang pondéré dans une distribution de référence. Définies ici
#  pour que les deux scripts ne puissent pas diverger.
rang_pondere <- function(x, w) {
  r  <- rep(NA_real_, length(x)); ok <- !is.na(x) & !is.na(w) & w > 0
  xo <- x[ok]; wo <- w[ok]; o <- order(xo)
  cw <- cumsum(wo[o])
  r[which(ok)[o]] <- (cw - wo[o] / 2) / sum(wo)
  r
}
transposer <- function(x, w, x_ref, w_ref) {
  p <- rang_pondere(x, w); out <- x; ok <- !is.na(p)
  out[ok] <- wquantile(x_ref, w_ref, p[ok]); out
}

# ── Naissance récente : une seule définition pour les 8 vagues ──────────────
#  2006-2018 : variable "événements" du fichier ménages (EVENEMEN_C).
#  2022-2025 : module absent des fichiers FPR -> RECONSTRUITE depuis le fichier
#  individus (un enfant né en N ou N-1 est présent dans le ménage).
#  ⚠️ Les deux définitions ne sont pas strictement équivalentes : comparer les
#  niveaux entre vagues anciennes et FPR demande de la prudence.
ajouter_naissance <- function(raw, cfg, nom = "y_birth") {
  if (!is.null(cfg$naiss_ind)) {
    ni <- cfg$naiss_ind
    naiss <- lire_srcv(cfg$ind) %>%
      transmute(.cle = trimws(as.character(.data[[ni$cle_ind]])),
                .ne  = as.integer(num(.data[[ni$annais]]) %in% ni$annees)) %>%
      group_by(.cle) %>%
      summarise(.yb = as.integer(any(.ne == 1L, na.rm = TRUE)), .groups = "drop")
    raw <- raw %>%
      mutate(.cle = trimws(as.character(.data[[ni$cle_men]]))) %>%
      left_join(naiss, by = ".cle") %>%
      mutate(.yb = replace_na(.yb, 0L)) %>%
      select(-.cle)
  } else {
    raw <- raw %>% mutate(.yb = as.integer(num(.data[[cfg$vars[["y_birth"]]]]) == 1))
  }
  raw[[nom]] <- raw$.yb
  select(raw, -.yb)
}

# ── Chargement standard d'une vague, niveau MÉNAGE ──────────────────────────
#  Applique les conventions du projet, une fois pour toutes :
#   - COÛT = charges (HH070) + mensualité d'emprunt (REMP/REMPR, NA -> 0)
#   - statut en 3 postes (locataire / accédant / non accédant)
#   - surfaces implausibles -> NA (et non ligne supprimée)
#   - niveau de vie = revenu disponible / UC (HX050)
#   - âge de la FEMME du ménage (PR ou conjointe)
#   - taux d'effort censuré dans [0 ; 100]
#  Seul filtre appliqué : poids valide. Les restrictions de champ (âge, statut,
#  couple...) restent la responsabilité du script appelant.
charger_menages <- function(annee, cfg, surface_min = 9, surface_max = 400) {
  v <- cfg$vars
  d <- lire_srcv(cfg$men)
  tibble(
    annee   = annee,
    id      = trimws(as.character(d[[v[["id"]]]])),
    age_pr  = num(d[[v[["age_pr"]]]]),
    age_cj  = num(d[[v[["age_cj"]]]]),
    sexe_pr = trimws(as.character(d[[v[["sexe_pr"]]]])),
    sexe_cj = trimws(as.character(d[[v[["sexe_cj"]]]])),
    surface = num(d[[v[["surface"]]]]),
    pieces  = num(d[[v[["pieces"]]]]),
    taille  = num(d[[v[["taille"]]]]),
    nb_enf  = num(d[[v[["nb_enf"]]]]),
    charges = num(d[[v[["cout_log"]]]]),
    remb    = coalesce(num(d[[v[["rembours"]]]]), 0),
    revenu  = num(d[[v[["revenu"]]]]),
    uc      = num(d$HX050),
    occ     = d[[v[["occ"]]]],
    urbain  = d[[v[["urbain"]]]],
    couple  = d[[v[["couple"]]]],
    # ZEAT réparée par la région quand celle-ci existe (2006 seulement)
    zeat    = recode_zeat(d[[v[["zeat"]]]],
                          if ("REGION" %in% names(d)) d$REGION else NULL),
    poids   = num(d[[v[["poids"]]]])
  ) %>%
    mutate(
      cout       = charges + remb,
      statut3    = recode_statut3(recode_occ(occ, cfg$occ_type), remb),
      surface    = if_else(surface >= surface_min & surface <= surface_max,
                           surface, NA_real_),
      niveau_vie = if_else(!is.na(uc) & uc > 0, revenu / uc, revenu),
      age_femme  = case_when(sexe_pr == "2" ~ age_pr,
                             sexe_cj == "2" ~ age_cj, TRUE ~ NA_real_),
      effort     = taux_effort(cout, revenu)
    ) %>%
    filter(!is.na(poids), poids > 0)
}

# ── Fichiers INDIVIDUS : clé ménage et âges ─────────────────────────────────
#  RB040 (identifiant ménage) existe en 2006, 2018 et dans les fichiers FPR,
#  mais PAS en 2010 ni 2014. Là, RB030 = identifiant ménage + numéro de
#  personne sur 2 chiffres, avec zéros de tête : on retire les 2 derniers
#  caractères puis les zéros initiaux. Taux d'appariement vérifié : 100 % en
#  2010, 96,6 % en 2014.
#  ⚠️ Les zéros de tête ne sont pas cohérents entre fichiers : en 2018, RB040
#  vaut "03706700" quand DB030 vaut "3706700" (0 % d'appariement brut, 100 %
#  après normalisation). D'où normaliser_id(), à appliquer DES DEUX CÔTÉS de
#  toute jointure ménages <-> individus.
normaliser_id <- function(x) sub("^0+", "", trimws(as.character(x)))

cle_menage_ind <- function(ind) {
  if ("RB040" %in% names(ind)) return(normaliser_id(ind$RB040))
  r <- trimws(as.character(ind$RB030))
  normaliser_id(substr(r, 1, nchar(r) - 2))
}

# Âge des individus : colonne AGE (2006, 2010, FPR) ou age (2014, 2018),
# à défaut reconstruit depuis l'année de naissance.
ages_individus <- function(ind, annee) {
  nm <- names(ind)
  if ("AGE" %in% nm) return(num(ind$AGE))
  if ("age" %in% nm) return(num(ind$age))
  a <- if ("ANAIS" %in% nm) num(ind$ANAIS) else num(ind$anais)
  as.integer(annee) - a
}

# Âge MOYEN des membres du ménage, agrégé au niveau ménage.
#   liens = NULL             -> tous les membres, enfants compris
#   liens = c("00","01",...) -> personne de référence et conjoint uniquement,
#                               soit l'âge moyen du ou DES PARENTS. Pour une
#                               famille monoparentale, c'est l'âge du parent.
# LIENPREF est codé sur 2 chiffres selon les vagues ("00") ou 1 ("0") : les
# deux écritures sont acceptées.
LIENS_PARENTS <- c("00", "01", "0", "1")

age_moyen_menage <- function(cfg, annee, liens = LIENS_PARENTS) {
  ind <- lire_srcv(cfg$ind)
  d <- tibble(id  = cle_menage_ind(ind),
              age = ages_individus(ind, annee),
              lien = trimws(as.character(ind$LIENPREF))) %>%
    filter(!is.na(age), age >= 0, age < 120)
  if (!is.null(liens)) d <- filter(d, lien %in% liens)
  d %>% group_by(id) %>%
    summarise(age_moyen = mean(age), n_membres = n(), .groups = "drop")
}

# ── DENSITÉ : tranche d'unité urbaine, harmonisée en 5 classes ──────────────
#  ⚠️ POURQUOI PAS DB100 (DEGURBA) : la nomenclature Eurostat a été révisée
#     vers 2011-2012 et la répartition saute artificiellement — part
#     "intermédiaire" : 34,1 % en 2010, 20,0 % en 2018, 30,9 % en 2025. Les
#     niveaux ne sont pas comparables entre vagues.
#
#  ⚠️ POURQUOI PAS 2006 NI 2010 : ces vagues ne portent que TAU99, qui mesure
#     des AIRES urbaines (bassin de vie, couronnes périurbaines incluses) et
#     non des UNITÉS urbaines (tache bâtie continue). Mesuré : la tranche
#     "moins de 20 000 habitants" pèse 1,9 % avec TAU99 contre 17 à 19 % avec
#     tuu10/TUU2017, l'aire urbaine vidant les petites tranches au profit des
#     grandes. recode_densite() renvoie donc NA pour type = "tau99" : la série
#     de densité commence en 2014.
#
#  Les vagues exploitables utilisent tuu10 (2014, 2018, population 2010) puis
#  TUU2017 (2022+, population 2017) : même concept, seule l'année de référence
#  du recensement change, ce qui ne produit qu'une dérive lente.
DENSITE_LIB <- c("Rural", "Moins de 20 000 hab.", "20 000 a 199 999 hab.",
                 "200 000 hab. a 2 M", "Agglomeration de Paris")

recode_densite <- function(x, type) {
  if (is.null(type) || type != "tuu")
    return(factor(rep(NA_character_, length(x)), levels = DENSITE_LIB))
  x <- trimws(as.character(x)); x[x == ""] <- NA
  # TUU en 9 postes : 0 rural | 1-3 < 20 k | 4-6 20-200 k | 7 200 k-2 M | 8 Paris
  factor(cut(match(x, as.character(0:8)), breaks = c(0, 1, 4, 7, 8, 9),
             labels = DENSITE_LIB), levels = DENSITE_LIB)
}
