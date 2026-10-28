# ============================================================================
#  ERFS — 56. Extraction des couples féconds de l'ERFS, 2005 à 2023
# ----------------------------------------------------------------------------
#  Enquête Revenus fiscaux et sociaux, fichiers de production et de recherche
#  (Progedo-Adisp) : enquête Emploi du 4e trimestre appariée aux déclarations
#  de revenus et aux prestations sociales. Deux formats :
#   - 2005-2020 : tables irf (individus de l'enquête Emploi), mrf (ménages de
#     l'enquête Emploi), indiv (revenus individuels), menage (revenus et
#     prestations du ménage) ;
#   - 2021-2023 : tables indiv et menage, variables de l'enquête Emploi incluses.
#
#  Champ : ménages du champ de diffusion (champ_calcul = 1) dont la personne de
#  référence et son conjoint sont de sexe différent, la femme ayant de 15 à 49
#  ans au 31 décembre.
#  Naissance : un enfant de la personne de référence ou de son conjoint né dans
#  les douze mois qui précèdent la semaine de référence (NAIA, NAIM, DATDEB) ; à
#  défaut de mois de naissance, âge 0 au 31 décembre.
#  Composition AVANT la naissance : nouveau-nés retirés ; âges au 31 décembre
#  des autres enfants de moins de 21 ans (âge limite des prestations).
#  Revenus de l'année N, par conjoint, au plus près du revenu imposable :
#  salaires, chômage, pensions et revenus d'indépendant, plus la CSG non
#  déductible et la CRDS que l'ERFS en a retranchées.
#  Statut conjugal : marié ; pacsé (repéré à partir de 2013 seulement, avant
#  quoi les pacsés sont comptés comme non mariés) ; ni l'un ni l'autre.
#
#  Sortie (dans Data/, non versionné) : Data/ERFS/_extrait/couples_ERFS.rds
# ============================================================================

source("R/00_utils.R")
source("R/00_config.R")
suppressPackageStartupMessages(library(data.table))

DIR_EXTRAIT <- file.path(DIR_ERFS, "_extrait")
dir.create(DIR_EXTRAIT, showWarnings = FALSE)

# Fichier CSV d'une table, repéré par motif dans le dossier du millésime
fichier <- function(an, motif) {
  f <- list.files(file.path(DIR_ERFS, ERFS_LIL[[an]]), pattern = motif, recursive = TRUE,
                  full.names = TRUE, ignore.case = TRUE)
  f <- f[grepl("\\.csv$", f, ignore.case = TRUE)]
  if (!length(f)) return(NULL)
  f[1]
}
# Lecture en texte des seules colonnes voulues (noms mis en capitales,
# colonnes absentes créées vides) ; l'identifiant ménage devient IDENT.
# Les tables irf de 2008 à 2011 contiennent quelques champs texte coupés par
# un saut de ligne nu (LF, les lignes finissant par CRLF) ou contenant un
# point-virgule : les sauts nus sont remplacés par une espace, et les rares
# lignes qui n'ont toujours pas le bon nombre de champs sont écartées.
lire <- function(f, cols) {
  h <- names(fread(f, nrows = 0))
  ident <- h[grepl("^ident[0-9]{2}$", h, ignore.case = TRUE)]
  sel <- h[toupper(h) %in% toupper(cols) | h %in% ident]
  brut <- readBin(f, "raw", file.size(f))
  nul <- brut == as.raw(0)
  if (any(nul)) { cat("   ", basename(f), ":", sum(nul), "octet(s) nul(s) remplacé(s)\n"); brut[nul] <- as.raw(32) }
  brut <- rawToChar(brut)
  source_lue <- f
  if (grepl("\r\n", substr(brut, 1, 1e5), fixed = TRUE)) {
    brut <- gsub("(?<!\r)\n", " ", brut, perl = TRUE, useBytes = TRUE)
    lignes <- strsplit(brut, "\r\n", fixed = TRUE, useBytes = TRUE)[[1]]
    sep <- if (grepl(";", lignes[1], fixed = TRUE)) ";" else ","
    n <- nchar(lignes, type = "bytes") -
      nchar(gsub(sep, "", lignes, fixed = TRUE, useBytes = TRUE), type = "bytes")
    ok <- n == n[1]
    if (any(!ok)) cat("   ", basename(f), ":", sum(!ok), "ligne(s) mal formée(s) écartée(s)\n")
    source_lue <- paste(lignes[ok & nzchar(lignes)], collapse = "\n")
  }
  if (any(nul) && identical(source_lue, f)) source_lue <- brut
  rm(brut)
  x <- if (identical(source_lue, f)) fread(f, select = sel, colClasses = "character", showProgress = FALSE,
                                           na.strings = c("", "NA"))
       else fread(text = source_lue, select = sel, colClasses = "character", showProgress = FALSE,
                  na.strings = c("", "NA"))
  setnames(x, toupper(names(x)))
  if (length(ident)) setnames(x, toupper(ident), "IDENT")
  for (v in setdiff(toupper(cols), names(x))) set(x, j = v, value = NA_character_)
  x
}
nombre <- function(x) { x <- suppressWarnings(as.numeric(x)); fifelse(is.na(x), 0, x) }

COLS_IND <- c("NOI", "NOINDIV", "LPR", "LPRM", "LPRL", "SEXE", "AG", "NAIA", "NAIM", "DATDEB", "NOICON",
              "NOIMER", "NOIPAR1", "NOIPAR2", "MATRI", "PACS", "ETAMATRI_Y_COMP", "DDIPL", "DIP7", "ACTEU", "SO")
REVENUS <- c("SALAIRES_I", "CHOMAGE_I", "RETRAITES_I", "PENS_INVALIDITE_I", "RAG_I", "RIC_I", "RNC_I",
             "CSG_ND_CRDS_SAL_I", "CSG_ND_CRDS_CHO_I", "CSG_ND_CRDS_RET_I", "CSG_ND_CRDS_INV_I",
             "CSG_ND_CRDS_RAG_I", "CSG_ND_CRDS_RIC_I", "CSG_ND_CRDS_RNC_I")
ACTIVITE <- c("SALAIRES_I", "RAG_I", "RIC_I", "RNC_I", "CSG_ND_CRDS_SAL_I", "CSG_ND_CRDS_RAG_I",
              "CSG_ND_CRDS_RIC_I", "CSG_ND_CRDS_RNC_I")
PRESTATIONS <- c("PREST_FAM_PETITE_ENFANCE", "PREST_FAM_AUTRES", "PREST_LOGEMENT", "PREST_PRECARITE_RMI",
                 "PREST_PRECARITE_RSA", "PREST_PRECARITE_HAND", "PREST_PRECARITE_VIEIL",
                 "PREST_AIDE_SOLIDARITE", "M_RSA_ACTM", "PPA", "GJ", "CEJ")
COLS_MEN <- c("WPRM", "CHAMP_CALCUL", "NB_UCI", "REVDISPM", "IRPP", "CS8CORPR", "CS8CORCJ", "LOGT", "REG",
              "TUR5", "TUU2010R", "TUU2020", PRESTATIONS)

diplome4 <- function(ddipl, dip7) {
  d <- coalesce(ddipl, dip7); ancien <- !is.na(ddipl)
  factor(fcase(ancien & d %in% c("1", "3"), "Superieur", !ancien & d %in% c("1", "2", "3"), "Superieur",
               d == "4", "Bac", d == "5", "CAP-BEP", d %in% c("6", "7"), "Infra CAP",
               default = "Non renseigne"),
         levels = c("Bac", "Infra CAP", "CAP-BEP", "Superieur", "Non renseigne"))
}
activite <- function(x) factor(fcase(x == "1", "Emploi", x == "2", "Chomage", x == "3", "Inactif",
                                     default = "Non renseigne"),
                               levels = c("Emploi", "Chomage", "Inactif", "Non renseigne"))
# Statut d'occupation : statut consolidé LOGT quand il existe (2014-2016,
# 2018-2023), sinon SO de l'enquête Emploi (complet avant 2013, première
# interrogation seulement ensuite).
statut <- function(logt, so) {
  s <- coalesce(logt, so)
  factor(fcase(s == "1", "Accedant", s == "2", "Proprietaire", s == "3", "Locataire HLM",
               s %in% c("4", "5"), "Locataire prive", s %in% c("6", "7"), "Loge gratuitement",
               default = "Non renseigne"),
         levels = c("Locataire prive", "Accedant", "Proprietaire", "Locataire HLM",
                    "Loge gratuitement", "Non renseigne"))
}
# Taille d'unité urbaine en 5 postes : TUR5 (jusqu'en 2012) et TUU2010R
# (2013-2020) ont les mêmes codes ; TUU2020 (2021-2023) est regroupé.
taille_uu <- function(tur5, tuu2010r, tuu2020) {
  t <- coalesce(tur5, tuu2010r)
  t <- fifelse(is.na(t), fcase(tuu2020 == "0", "1", tuu2020 %in% c("1", "2", "3"), "2",
                               tuu2020 %in% c("4", "5", "6"), "3", tuu2020 == "7", "4",
                               tuu2020 == "8", "5"), t)
  factor(fcase(t == "1", "Rural", t == "2", "Moins de 20 000", t == "3", "20 000 a 200 000",
               t == "4", "200 000 et plus", t == "5", "Agglomeration parisienne", default = "Non renseigne"),
         levels = c("Rural", "Moins de 20 000", "20 000 a 200 000", "200 000 et plus",
                    "Agglomeration parisienne", "Non renseigne"))
}
# Régions : anciennes régions (codes 11 à 94) regroupées en régions de 2016
REGION_2016 <- c("11" = "Ile-de-France", "21" = "Grand Est", "41" = "Grand Est", "42" = "Grand Est",
                 "22" = "Hauts-de-France", "31" = "Hauts-de-France", "23" = "Normandie", "25" = "Normandie",
                 "24" = "Centre-Val de Loire", "26" = "Bourgogne-Franche-Comte", "43" = "Bourgogne-Franche-Comte",
                 "52" = "Pays de la Loire", "53" = "Bretagne", "54" = "Nouvelle-Aquitaine",
                 "72" = "Nouvelle-Aquitaine", "74" = "Nouvelle-Aquitaine", "73" = "Occitanie", "91" = "Occitanie",
                 "82" = "Auvergne-Rhone-Alpes", "83" = "Auvergne-Rhone-Alpes",
                 "93" = "Provence-Alpes-Cote d'Azur", "94" = "Corse",
                 "27" = "Bourgogne-Franche-Comte", "28" = "Normandie", "32" = "Hauts-de-France",
                 "44" = "Grand Est", "75" = "Nouvelle-Aquitaine", "76" = "Occitanie",
                 "84" = "Auvergne-Rhone-Alpes")

extraire <- function(an) {
  t0 <- Sys.time(); a <- as.integer(an); yy <- substr(an, 3, 4)
  ancien <- a <= 2020
  f_ind <- fichier(an, paste0("^fpr_indiv_", an, ".*\\.csv$"))
  f_men <- fichier(an, paste0("^fpr_menage_", an, ".*\\.csv$"))
  if (ancien) {
    eec <- lire(fichier(an, paste0("^fpr_irf", yy, "e", yy, "t4\\.csv$")), COLS_IND)
    fis <- lire(f_ind, c("NOINDIV", REVENUS))
    eec <- merge(eec, fis[, !"IDENT"], by = "NOINDIV", all.x = TRUE)
    geo <- c("SO", "REG", "TUR5", "TUU2010R")
    mrf <- lire(fichier(an, paste0("^fpr_mrf", yy, "e", yy, "t4\\.csv$")), geo)
    men <- lire(f_men, c(COLS_MEN, "SO"))
    men <- merge(men, mrf, by = "IDENT", all.x = TRUE, suffixes = c("", ".mrf"))
    for (v in geo) set(men, j = v, value = fcoalesce(men[[paste0(v, ".mrf")]], men[[v]]))
    men[, paste0(geo, ".mrf") := NULL]
  } else {
    eec <- lire(f_ind, c(COLS_IND, REVENUS))
    men <- lire(f_men, COLS_MEN)
    men <- merge(men, eec[LPRL == "1", .(IDENT, SO)], by = "IDENT", all.x = TRUE)  # SO individuel en 2023
  }
  men[, `:=`(poids = nombre(WPRM), champ = CHAMP_CALCUL == "1")]

  # Lien à la personne de référence : LPR (jusqu'en 2012), LPRM ensuite ; à partir de
  # 2021, le ménage de l'ERFS est le logement (un identifiant par logement, qui peut
  # abriter plusieurs ménages de l'enquête Emploi) : lien à la personne de
  # référence du logement, LPRL.
  eec[, lien := coalesce(LPRL, LPRM, LPR)]
  eec[, `:=`(age = as.integer(AG), naia = as.integer(NAIA), naim = suppressWarnings(as.integer(NAIM)),
             an_ref = as.integer(substr(DATDEB, 1, 4)), mois_ref = as.integer(substr(DATDEB, 5, 6)))]
  eec[naim > 12 | naim < 1, naim := NA_integer_]
  eec[, mois_depuis := (an_ref * 12L + mois_ref) - (naia * 12L + naim)]
  eec[, nouveau_ne := lien == "3" & fifelse(is.na(mois_depuis), age == 0, mois_depuis >= -1 & mois_depuis <= 11)]
  eec[, `:=`(noi = as.integer(NOI), par1 = as.integer(coalesce(NOIMER, NOIPAR1)), par2 = as.integer(NOIPAR2))]

  # Couples : personne de référence et conjoint de sexe différent
  pc <- eec[lien %in% c("1", "2")]
  pc <- pc[, if (.N == 2 && sum(SEXE == "2") == 1) .SD, by = IDENT]
  revenu_imp <- function(d, vars) Reduce(`+`, lapply(intersect(vars, names(d)), function(v) pmax(nombre(d[[v]]), 0)))
  pc[, `:=`(rev_imp = revenu_imp(.SD, REVENUS), rev_act = revenu_imp(.SD, ACTIVITE))]
  pc[, conj := fcase(MATRI == "2" | ETAMATRI_Y_COMP == "1", "marie",
                     PACS == "1" | ETAMATRI_Y_COMP == "2", "pacse",
                     is.na(MATRI) & (is.na(ETAMATRI_Y_COMP) | ETAMATRI_Y_COMP == "99"), "inconnu",
                     default = "celibataire")]
  f <- pc[SEXE == "2", .(IDENT, noi_femme = noi, lien_femme = lien, age_femme = age, conj_f = conj,
                         dipl_femme = diplome4(DDIPL, DIP7), act_femme = activite(ACTEU),
                         rev_femme = rev_imp, act_rev_femme = rev_act)]
  h <- pc[SEXE == "1", .(IDENT, age_homme = age, conj_h = conj,
                         dipl_homme = diplome4(DDIPL, DIP7), act_homme = activite(ACTEU),
                         rev_homme = rev_imp, act_rev_homme = rev_act)]
  cp <- merge(f, h, by = "IDENT")[age_femme >= 15 & age_femme <= 49]
  cp[, conj := fcase(conj_f == "marie" | conj_h == "marie", "marie",
                     conj_f == "pacse" | conj_h == "pacse", "pacse",
                     conj_f == "inconnu" & conj_h == "inconnu", "inconnu", default = "celibataire")]

  # Enfants de la personne de référence ou de son conjoint
  enf <- eec[lien == "3" & IDENT %in% cp$IDENT & !is.na(age) & age <= 20]
  enf <- merge(enf, cp[, .(IDENT, noi_femme)], by = "IDENT")
  nn <- enf[nouveau_ne == TRUE, .(n_naiss = .N, naiss_mere = any((is.na(par1) & is.na(par2)) |
                                                                 par1 %in% noi_femme | par2 %in% noi_femme)),
            by = IDENT]
  av <- enf[nouveau_ne == FALSE][order(IDENT, age),
            .(nb_enf_avant = .N, benjamin = min(age), ages = paste(age, collapse = ";")), by = IDENT]
  cp <- merge(cp, nn, by = "IDENT", all.x = TRUE)
  cp <- merge(cp, av, by = "IDENT", all.x = TRUE)
  cp[, `:=`(n_naiss = fcoalesce(n_naiss, 0L), nb_enf_avant = fcoalesce(nb_enf_avant, 0L),
            ages = fcoalesce(ages, ""))]
  cp[, naiss01 := as.integer(n_naiss > 0)]

  # Ménage : poids, prestations, niveau de vie hors prestations avant la naissance
  cp <- merge(cp, men, by = "IDENT")[champ == TRUE & poids > 0]
  prest <- intersect(PRESTATIONS, names(cp))
  cp[, prestations := Reduce(`+`, lapply(prest, function(v) nombre(cp[[v]])))]
  cp[, `:=`(prest_pe = nombre(PREST_FAM_PETITE_ENFANCE), prest_autres = nombre(PREST_FAM_AUTRES),
            revdisp = nombre(REVDISPM), irpp = nombre(IRPP),
            uc_avant = nombre(NB_UCI) - 0.3 * n_naiss)]
  cp[, nv_hp := (revdisp - prestations) / pmax(uc_avant, 1)]
  cp[, `:=`(cs_femme = fifelse(lien_femme == "1", CS8CORPR, CS8CORCJ),
            cs_homme = fifelse(lien_femme == "1", CS8CORCJ, CS8CORPR),
            statut = statut(LOGT, SO), tuu = taille_uu(TUR5, TUU2010R, TUU2020),
            region = factor(fcoalesce(unname(REGION_2016[REG]), "Non renseigne")))]
  out <- cp[, .(annee = a, ident = IDENT, poids, age_femme, age_homme, naiss01, n_naiss, naiss_mere,
                nb_enf_avant, benjamin, ages, conj, rev_femme, rev_homme, act_rev_femme, act_rev_homme,
                dipl_femme, dipl_homme, act_femme, act_homme, cs_femme, cs_homme, statut, tuu, region,
                prest_pe, prest_autres, prestations, revdisp, irpp, uc_avant, nv_hp)]
  cat(an, ":", nrow(out), "couples,", sum(out$naiss01), "naissances (",
      round(100 * weighted.mean(out$naiss01, out$poids), 2), "% pondéré ), lu en",
      round(difftime(Sys.time(), t0, units = "secs")), "s\n")
  out
}

couples <- rbindlist(lapply(names(ERFS_LIL), extraire))
stopifnot(!anyDuplicated(couples[, .(annee, ident)]))
saveRDS(couples, file.path(DIR_EXTRAIT, "couples_ERFS.rds"))

# Contrôles
cat("\nNaissances dont la mère n'est pas la femme du couple :",
    sum(couples$naiss01 == 1 & !couples$naiss_mere, na.rm = TRUE), "\n")
print(couples[, .(couples = .N, naissances = sum(naiss01), taux_pct = round(100 * weighted.mean(naiss01, poids), 2),
                  maries_pct = round(100 * weighted.mean(conj == "marie", poids), 1),
                  pacses_pct = round(100 * weighted.mean(conj == "pacse", poids), 1),
                  conj_inconnu_pct = round(100 * weighted.mean(conj == "inconnu", poids), 1),
                  mere_autre = sum(naiss01 == 1 & !naiss_mere, na.rm = TRUE),
                  jumeaux = sum(n_naiss > 1),
                  statut_nr_pct = round(100 * weighted.mean(statut == "Non renseigne", poids), 1),
                  tuu_nr_pct = round(100 * weighted.mean(tuu == "Non renseigne", poids), 1),
                  region_nr_pct = round(100 * weighted.mean(region == "Non renseigne", poids), 1),
                  rang1_pct = round(100 * weighted.mean(nb_enf_avant == 0, poids), 1)), by = annee])
