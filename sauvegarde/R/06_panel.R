# ============================================================================
#  SRCV — Mise en PANEL des vagues FPR 2022-2025 + construction du fichier
#  d'analyse "logement (t) -> naissance (t+1)".
# ----------------------------------------------------------------------------
#  Chaînage des ménages sur DB030 (identifiant panel stable, vérifié : +1 an
#  d'âge de la PR et région constante entre vagues).
#
#  Une ligne = une TRANSITION ménage × (t -> t+1), pour t ∈ {2022,2023,2024} :
#    - Outcome    : naissance entre t et t+1 = un NOUVEL enfant (né en t ou t+1,
#      absent en t) apparaît dans le ménage en t+1  (détection par l'identifiant
#      individu RB030 : présent en t+1, absent en t, ANAIS ∈ {t, t+1}).
#    - Prédicteurs LOGEMENT mesurés en t (donc AVANT la naissance) : surface,
#      nb pièces, coût (HH070), valeur estimée (VFEPP, propriétaires), statut.
#    - Contrôles en t : parité PRÉ-naissance (NENFANTS_t), âge de la femme,
#      revenu, diplôme PR, CSP, nb d'actifs occupés, région, couple.
#
#  ⚠️ Un même ménage apparaît dans plusieurs transitions -> en modélisation
#  (07), regrouper les erreurs-types par ménage (svydesign ids=~id).
#  ⚠️ L'ordre temporel réduit la causalité inverse mais PAS l'anticipation
#  (déménagement en t en prévision d'un enfant en t+1).
#
#  Sortie : Output/panel_naissance_logement.csv
# ============================================================================

source("R/00_utils.R")     # lire_srcv(), num(), verifier_colonnes()
source("R/00_config.R")    # VAGUES : chemins des fichiers

# Vagues FPR (2022+), détectées automatiquement depuis 00_config.R : ajouter
# une vague dans la config l'intègre ici sans toucher à ce script.
waves    <- sort(as.integer(vagues_fpr()))
men_path <- map_chr(as.character(waves), chemin_men)
ind_path <- map_chr(as.character(waves), chemin_ind)
stopifnot("Au moins 2 vagues FPR sont nécessaires pour construire des transitions" =
            length(waves) >= 2)

# ── Chargement ménages (variables logement + contrôles), niveau ménage ──────
COLS_MEN <- c("DB030","SURFACE","NPIECES","HH070","REMPR","VFEPPMIN","VFEPPMAX","STOC","EMMENAG",
              "NENFANTS","AGEPR","AGECJ","SEXEPR","SEXECJ","HY020","DIPDETPR","PCSM",
              "NACTOCCUP","ZEAT","COUPLEPR","DB090")

charger_men <- function(path, annee) {
  d <- lire_srcv(path)
  verifier_colonnes(d, COLS_MEN, paste0("(ménages ", annee, ")"))
  d %>% transmute(
    id       = trimws(as.character(DB030)),
    annee    = annee,
    surface  = num(SURFACE),
    npieces  = num(NPIECES),
    # COÛT = charges (HH070) + mensualité d'emprunt (REMPR, NA = pas d'emprunt).
    # HH070 seul exclut le remboursement du capital et sous-estime largement
    # la charge des accédants (438 € vs 850 € de mensualité en 2025).
    rembours = coalesce(num(REMPR), 0),
    cout_log_hors_emprunt = num(HH070),
    cout_log = num(HH070) + coalesce(num(REMPR), 0),
    vfepp    = (num(VFEPPMIN) + num(VFEPPMAX)) / 2,      # valeur estimée résidence (proprios)
    stoc     = trimws(as.character(STOC)),
    emmenag  = num(EMMENAG),
    nenf     = num(NENFANTS),
    agepr    = num(AGEPR), agecj = num(AGECJ),
    sexepr   = trimws(as.character(SEXEPR)), sexecj = trimws(as.character(SEXECJ)),
    revenu   = num(HY020),
    dipl_pr  = trimws(as.character(DIPDETPR)),
    csp      = trimws(as.character(PCSM)),
    nactocc  = num(NACTOCCUP),
    zeat     = trimws(as.character(ZEAT)),
    couple   = trimws(as.character(COUPLEPR)),
    poids    = num(DB090)
  ) %>% mutate(
    # âge de la femme du ménage (PR ou conjointe ; SEXE : 1=H, 2=F)
    age_femme = case_when(sexepr == "2" ~ agepr, sexecj == "2" ~ agecj, TRUE ~ NA_real_),
    # statut d'occupation binaire (STOC : {1,2,3,6}=proprio/gratuit, {4,5}=locataire)
    proprio = if_else(stoc %in% c("1","2","3","6"), 1L,
                      if_else(stoc %in% c("4","5"), 0L, NA_integer_)),
    statut_occ = factor(proprio, levels = c(0, 1),
                        labels = c("Locataire", "Proprietaire_ou_gratuit")),
    # Statut en 3 postes : locataire / accédant (rembourse) / non accédant
    statut3 = recode_statut3(statut_occ, rembours)
  )
}

# ── Chargement individus (juste ce qu'il faut pour détecter les naissances) ─
charger_ind <- function(path) {
  d <- lire_srcv(path)
  verifier_colonnes(d, c("RB030", "RB040", "ANAIS"), "(fichier individus)")
  d %>% transmute(id  = trimws(as.character(RB040)),
                  pid = trimws(as.character(RB030)),
                  anais = num(ANAIS))
}

men_all <- map2(men_path, waves, charger_men)
ind_all <- map(ind_path, charger_ind)

# ── Construction des transitions t -> t+1 ───────────────────────────────────
transitions <- map_dfr(seq_len(length(waves) - 1), function(k) {
  t <- waves[k]; t1 <- waves[k + 1]
  men_t  <- men_all[[k]]
  men_t1 <- men_all[[k + 1]]
  ind_t  <- ind_all[[k]]
  ind_t1 <- ind_all[[k + 1]]

  # Ménages observés en t ET en t+1
  both <- inner_join(men_t, distinct(men_t1, id), by = "id")

  # Naissance = un individu apparaît en t+1 (absent en t), né en t ou t+1
  pids_t <- ind_t$pid
  bebes  <- ind_t1 %>%
    filter(anais %in% c(t, t1), !(pid %in% pids_t)) %>%
    distinct(id) %>% mutate(naissance = 1L)

  both %>%
    left_join(bebes, by = "id") %>%
    mutate(naissance = coalesce(naissance, 0L),
           annee_t = t, annee_t1 = t1)
})

# ── Variables dérivées pour le modèle ───────────────────────────────────────
# EUROS CONSTANTS 2025 : les transitions couvrent des années différentes ; sans
# déflation, "coût élevé" confondrait niveau réel et millésime. Coûts déflatés
# par l'IPC de l'année N, revenu (HY020, qui porte sur N-1) par celui de N-1.
IPC <- charger_ipc()

panel <- transitions %>%
  mutate(
    cout_log_reel = deflater(cout_log, annee_t,     IPC, base = 2025),
    revenu_reel   = deflater(revenu,   annee_t - 1, IPC, base = 2025),
    vfepp_reel    = deflater(vfepp,    annee_t,     IPC, base = 2025),
  ) %>%
  mutate(
    parite = cut(nenf, breaks = c(-Inf, 0, 1, 2, Inf), labels = c("0","1","2","3+")),  # PRÉ-naissance
    surface_10 = surface / 10,           # pour OR "par +10 m²"
    # Variables monétaires rééchelonnées, en EUROS CONSTANTS 2025
    cout_100   = cout_log_reel / 100,    # par +100 €/mois (réel)
    vfepp_100k = vfepp_reel / 100000,    # par +100 000 € (réel)
    revenu_10k = revenu_reel / 10000,    # par +10 000 €/an (réel)
    cout_m2    = cout_log_reel / if_else(surface >= 9 & surface <= 400, surface, NA_real_),
    proprio_f  = factor(proprio, levels = c(0,1), labels = c("Locataire","Proprietaire")),
    couple_f   = factor(couple == "1", labels = c("Non","Oui")),
    csp        = factor(csp), dipl_pr = factor(dipl_pr), zeat = factor(zeat),
    annee_t    = factor(annee_t)
  ) %>%
  relocate(id, annee_t, annee_t1, naissance)

# ── Diagnostics ──────────────────────────────────────────────────────────────
cat("Transitions (ménages × t->t+1) :", nrow(panel), "\n")
cat("Naissances (t+1) :", sum(panel$naissance), sprintf("(%.2f %%)\n", 100*mean(panel$naissance)))
cat("\nNaissances par transition :\n")
panel %>% group_by(annee_t) %>% summarise(n = n(), naiss = sum(naissance),
          taux = round(100*mean(naissance),2), .groups="drop") %>% print()
cat("\nÀ risque : femmes 15-49 ans, couple :",
    sum(panel$age_femme >= 15 & panel$age_femme <= 49 & panel$couple == "1", na.rm=TRUE), "\n")

# ── Export ───────────────────────────────────────────────────────────────────
if (!dir.exists("Output")) dir.create("Output")
write_csv(panel, "Output/panel_naissance_logement.csv")
cat("\nExporté : Output/panel_naissance_logement.csv\n")
