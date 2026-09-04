# ============================================================================
#  SRCV 2022 — Récap financier : couples & personnes seules
# ----------------------------------------------------------------------------
#  REVENU = revenu du travail + prestations sociales INDIVIDUELLES (hors
#  prestations familiales), net, par personne :
#     PY010N (salaire) + PY050N (indépendant)
#   + PY090N (chômage) + PY100N (retraite) + PY110N (survie)
#   + PY120N (maladie) + PY130N (invalidité) + PY140N (bourses/éducation)
#  EXCLUS : prestations familiales (HY050) et prestations de niveau MÉNAGE
#     (logement HY070, RSA/minima).
#
#  DEUX tableaux :
#   - Tableau A : tous les couples + personnes seules.
#   - Tableau B : échantillon TOTALEMENT SANS RETRAITÉS — on exclut les couples
#     dont au moins un membre touche une pension de retraite (PY100N > 0) ET les
#     personnes seules pensionnées.
#
#  Revenu annuel moyen (pondéré RB050) + part pondérée de chaque situation, par
#  sexe et présence d'enfant. Dénominateur des parts = adultes PR/conjoints de
#  l'échantillon (couple hétéro ou seul). Statut = activité dominante 2021.
# ============================================================================

source("R/00_utils.R")     # lire_srcv(), num(), statut_dominant()...
source("R/00_config.R")    # VAGUES : chemins des fichiers

library(writexl)

# ── Millésime (fichiers FPR individus : 2022, 2023, 2024, 2025) ─────────────
ANNEE <- Sys.getenv("SRCV_ANNEE", "2025")   # surchargeable : SRCV_ANNEE=2022 Rscript ...
stopifnot("Millésime sans variables NBM_* (fichiers FPR ≥ 2022 requis)" =
            isTRUE(VAGUES[[ANNEE]]$has_nbm))
ind <- lire_srcv(chemin_ind(ANNEE)) %>%
  mutate(hid = trimws(as.character(RB040)), sexe = num(SEXE))

# EUROS CONSTANTS : le tableau est produit millésime par millésime ; pour
# comparer deux exécutions (ex. 2022 vs 2025), les revenus sont aussi exprimés
# en euros constants 2025 (IPC INSEE). Les revenus PY0xx portent sur l'année
# N-1 -> déflatés par l'indice de N-1.
IPC        <- charger_ipc()
COEF_DEFL  <- deflater(1, as.integer(ANNEE) - 1, IPC, base = 2025)
cat("Millésime", ANNEE, ": coefficient de passage en euros constants 2025 =",
    round(COEF_DEFL, 4), "\n")

# ── Statut d'activité dominant en N-1 (poste NBM_* avec le plus de mois) ────
grp <- statut_dominant(ind)$statut          # TP / PT / Chomage / Inactif

prestations_indiv <- c("PY090N","PY100N","PY110N","PY120N","PY130N","PY140N")
verifier_colonnes(ind, c(prestations_indiv, "PY010N","PY050N","RB050","LIENPREF"),
                  paste0("(individus ", ANNEE, ")"))
ind <- ind %>% mutate(
  statut   = grp,
  retraite = coalesce(num(PY100N), 0) > 0,                  # touche une pension de retraite
  actif    = statut %in% c("TP", "PT"),
  revenu   = coalesce(num(PY010N),0) + coalesce(num(PY050N),0) +
             rowSums(across(all_of(prestations_indiv), ~coalesce(num(.),0))),
  poids    = num(RB050))

enf <- ind %>% group_by(hid) %>%
  summarise(enfant = any(LIENPREF == "02", na.rm = TRUE), .groups = "drop")

adultes <- ind %>% filter(LIENPREF %in% c("00", "01")) %>% left_join(enf, by = "hid")
type <- adultes %>% group_by(hid) %>%
  summarise(couple = n() == 2 & n_distinct(sexe) == 2, seul = n() == 1, .groups = "drop")
adultes <- adultes %>% left_join(type, by = "hid")

# Couples : 1 ligne par couple (+ flag retraité)
cpl_all <- adultes %>% filter(couple) %>% group_by(hid) %>%
  summarise(sh = statut[sexe==1], sf = statut[sexe==2],
            ah = actif[sexe==1],  af = actif[sexe==2],
            rh = retraite[sexe==1], rf = retraite[sexe==2],
            xh = revenu[sexe==1], xf = revenu[sexe==2],
            poids = poids[sexe==1], enfant = first(enfant), .groups = "drop") %>%
  mutate(ret_couple = coalesce(rh, FALSE) | coalesce(rf, FALSE))
seuls <- adultes %>% filter(seul) %>%
  mutate(cat = case_when(actif ~ "en activité", statut == "Chomage" ~ "au chômage",
                         TRUE ~ "inactive/inactif"))

# ── Fonction qui construit un tableau à partir d'un jeu de couples ──────────
wm   <- function(x, w) if (sum(w, na.rm=TRUE)==0) NA_real_ else sum(x*w, na.rm=TRUE)/sum(w, na.rm=TRUE)
moy  <- function(x, w) { v <- wm(x, w); if (is.na(v)) NA_real_ else round(v) }
moy_reel <- function(x, w) { v <- wm(x, w); if (is.na(v)) NA_real_ else round(v * COEF_DEFL) }
part <- function(w, d)  if (d==0) NA_real_ else round(100*sum(w, na.rm=TRUE)/d, 1)

construire <- function(cpl, seuls_df) {
  # base (dénominateurs) = adultes des couples de 'cpl' + toutes les personnes seules
  femmes_cpl <- sum(cpl$poids, na.rm=TRUE); hommes_cpl <- sum(cpl$poids, na.rm=TRUE)
  fe_cpl <- sum(cpl$poids[cpl$enfant], na.rm=TRUE); he_cpl <- sum(cpl$poids[cpl$enfant], na.rm=TRUE)
  sf <- seuls_df %>% filter(sexe==2); sh <- seuls_df %>% filter(sexe==1)
  D_F  <- femmes_cpl + sum(sf$poids, na.rm=TRUE);            D_H  <- hommes_cpl + sum(sh$poids, na.rm=TRUE)
  D_Fe <- fe_cpl + sum(sf$poids[sf$enfant], na.rm=TRUE);     D_He <- he_cpl + sum(sh$poids[sh$enfant], na.rm=TRUE)

  cfg <- list(
   "1. Couple — les deux en activité (indép. compris)" = with(cpl, ah & af),
   "2. Couple — femme à temps partiel"                 = with(cpl, sf=="PT" & ah),
   "3. Couple — homme (uniquement) à temps partiel"    = with(cpl, sh=="PT" & sf=="TP"),
   "4. Couple — homme travaille, femme au chômage"     = with(cpl, ah & sf=="Chomage"),
   "5. Couple — femme travaille, homme au chômage"     = with(cpl, af & sh=="Chomage"),
   "6. Couple — homme travaille, femme inactive"       = with(cpl, ah & sf=="Inactif"),
   "7. Couple — femme travaille, homme inactif"        = with(cpl, af & sh=="Inactif"),
   "8. Couple — les deux au chômage"                   = with(cpl, sh=="Chomage" & sf=="Chomage"),
   "9. Couple — les deux inactifs"                     = with(cpl, sh=="Inactif" & sf=="Inactif"))
  L1 <- imap_dfr(cfg, function(sel, nm) { d <- cpl[sel,]; de <- cpl[sel & cpl$enfant,]
    tibble(Situation=nm,
      Femme_revenu=moy(d$xf,d$poids), Femme_part=part(d$poids,D_F),
      Homme_revenu=moy(d$xh,d$poids), Homme_part=part(d$poids,D_H),
      FemmeEnf_revenu=moy(de$xf,de$poids), FemmeEnf_part=part(de$poids,D_Fe),
      HommeEnf_revenu=moy(de$xh,de$poids), HommeEnf_part=part(de$poids,D_He))})
  L2 <- map_dfr(list(list(2,"Femme seule"), list(1,"Homme seul")), function(z){
    sx<-z[[1]]; lab<-z[[2]]; femme <- sx==2
    map_dfr(c("en activité","au chômage","inactive/inactif"), function(c){
      d <- seuls_df %>% filter(sexe==sx, cat==c); de <- d %>% filter(enfant)
      tibble(Situation=paste(lab,"—",c),
        Femme_revenu   =if(femme) moy(d$revenu,d$poids) else NA_real_, Femme_part=if(femme) part(d$poids,D_F) else NA_real_,
        Homme_revenu   =if(!femme) moy(d$revenu,d$poids) else NA_real_, Homme_part=if(!femme) part(d$poids,D_H) else NA_real_,
        FemmeEnf_revenu=if(femme) moy(de$revenu,de$poids) else NA_real_, FemmeEnf_part=if(femme) part(de$poids,D_Fe) else NA_real_,
        HommeEnf_revenu=if(!femme) moy(de$revenu,de$poids) else NA_real_, HommeEnf_part=if(!femme) part(de$poids,D_He) else NA_real_)})})
  bind_rows(L1, L2)
}

cell <- function(s, p) ifelse(is.na(s), "-", sprintf("%s € (%.1f %%)", format(s, big.mark=" "), p))
en_tableau <- function(d) d %>% transmute(Situation,
  Femme=cell(Femme_revenu,Femme_part), Homme=cell(Homme_revenu,Homme_part),
  `Femme+enfant`=cell(FemmeEnf_revenu,FemmeEnf_part), `Homme+enfant`=cell(HommeEnf_revenu,HommeEnf_part))

# ── Tableau A (tous) et Tableau B (hors couples avec un retraité) ────────────
# Ajoute pour chaque colonne de revenu sa version en euros constants 2025,
# afin que deux millésimes exécutés séparément restent comparables.
ajouter_reel <- function(d)
  d %>% mutate(across(ends_with("_revenu"), ~ round(.x * COEF_DEFL), .names = "{.col}_reel"))

donnees_A <- construire(cpl_all, seuls) %>% ajouter_reel()
donnees_B <- construire(cpl_all %>% filter(!ret_couple), seuls %>% filter(!retraite)) %>% ajouter_reel()
tableau_A <- en_tableau(donnees_A); tableau_B <- en_tableau(donnees_B)

cat("Couples :", nrow(cpl_all), "| dont >=1 retraité :", sum(cpl_all$ret_couple),
    "| gardés :", sum(!cpl_all$ret_couple), "\n")
cat("Personnes seules :", nrow(seuls), "| dont retraitées :", sum(seuls$retraite),
    "| gardées :", sum(!seuls$retraite), "\n\n")
cat("===== TABLEAU B — hors couples avec au moins un retraité =====\n")
print(as.data.frame(tableau_B), right = FALSE)

# ── Export Excel (4 feuilles) ────────────────────────────────────────────────
if (!dir.exists("Output")) dir.create("Output")
out <- paste0("Output/recap_financier_", ANNEE, ".xlsx")
feuilles <- list(A_Tableau = tableau_A, A_Donnees = donnees_A,
                 B_Tableau_hors_retraites = tableau_B, B_Donnees_hors_retraites = donnees_B)
if (!isTRUE(tryCatch({ write_xlsx(feuilles, out); TRUE }, error = function(e) FALSE))) {
  out <- paste0("Output/recap_financier_", ANNEE, "_MAJ.xlsx")
  write_xlsx(feuilles, out)
  message("Fichier principal verrouillé (ouvert dans Excel) -> écrit dans ", out)
}
cat("\nExporté :", out, "\n")
