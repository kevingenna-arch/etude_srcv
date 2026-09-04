# ============================================================================
#  SRCV — D. POLITIQUE FAMILIALE
# ----------------------------------------------------------------------------
#  Regroupe R/03_conge_parental, R/04_recap_financier, R/05_part_presta_naissance, R/08_quotient_familial.
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
#     source("R compact/D_politique_familiale.R")
# ============================================================================

source("R/00_prepa_fecondite.R")
activer_cache_prepa()


# ==========================================================================
#  Arbitrage intra-couple du congé
#  (ex-R/03_conge_parental.R)
# ==========================================================================

# ============================================================================
#  SRCV 2022 — Arbitrage intra-couple du congé selon les revenus relatifs
# ----------------------------------------------------------------------------
#  Question : dans un couple où la FEMME gagne plus que l'homme, l'homme
#  prend-il davantage son congé ?
#
#  Niveau INDIVIDUS (fichier tab_ind_fpr_2022). On apparie les 2 conjoints
#  (personne de référence LIENPREF="00" + conjoint LIENPREF="01"), on ne garde
#  que les couples hétéro avec un jeune enfant (né 2020-2022).
#
#  DEUX outcomes (côté HOMME) :
#    - congé maternité/paternité : NBM_MAT > 0  (mois de congé mat/pat en N-1)
#    - congé parental (proxy)     : NBM_FOY > 0  (mois "au foyer / garde d'enfants")
#
#  Prédicteur : la femme gagne plus (revenu individuel PY010N + PY050N).
#
#  ⚠️ CAVEATS :
#   - Revenu COURANT contaminé : un conjoint en congé gagne mécaniquement moins
#     -> "la femme gagne plus" est en partie une CONSÉQUENCE du congé (endogène).
#   - NBM_FOY mélange congé parental et "au foyer" au sens large.
#   - Petits effectifs (surtout congé mat/pat côté homme) -> exploratoire.
# ============================================================================




library(survey)
library(broom)

# ── Millésime (fichiers FPR individus : 2022, 2023, 2024, 2025) ─────────────
# NB : analyse sur UNE vague à la fois — les vagues FPR sont un PANEL (~68 % de
# ménages communs d'une année à la suivante), les empiler compterait plusieurs
# fois les mêmes couples.
ANNEE <- Sys.getenv("SRCV_ANNEE", "2025")   # surchargeable : SRCV_ANNEE=2022 Rscript ...
stopifnot("Millésime sans variables NBM_* (fichiers FPR ≥ 2022 requis)" =
            isTRUE(VAGUES[[ANNEE]]$has_nbm))
an  <- as.integer(ANNEE)
ind <- lire_srcv(chemin_ind(ANNEE))
verifier_colonnes(ind, c("RB030","RB040","RB050","ANAIS","LIENPREF","SEXE",
                         "PY010N","PY050N","NBM_MAT","NBM_FOY"),
                  paste0("(individus ", ANNEE, ")"))

# ── Ménages avec un enfant né récemment (année N à N-2) ─────────────────────
hh_young <- ind %>%
  mutate(hid = trimws(as.character(RB040)), anais = num(ANAIS)) %>%
  group_by(hid) %>%
  summarise(jeune_enfant = any(anais %in% (an - 2):an, na.rm = TRUE), .groups = "drop") %>%
  filter(jeune_enfant)

# ── Les 2 conjoints (PR + conjoint) de ces ménages ──────────────────────────
partenaires <- ind %>%
  mutate(hid = trimws(as.character(RB040))) %>%
  semi_join(hh_young, by = "hid") %>%
  filter(LIENPREF %in% c("00", "01")) %>%          # personne de référence + conjoint
  transmute(
    hid,
    sexe   = num(SEXE),                             # 1 = homme, 2 = femme (INSEE)
    revenu = coalesce(num(PY010N), 0) + coalesce(num(PY050N), 0),  # salaire + indép.
    mois_matpat = num(NBM_MAT),                     # mois congé maternité/paternité
    mois_foyer  = num(NBM_FOY),                     # mois au foyer / garde d'enfants
    poids  = num(RB050)
  )

# ── Un couple = 1 ligne (homme vs femme) ; couples hétéro uniquement ────────
couples <- partenaires %>%
  group_by(hid) %>%
  filter(n() == 2, n_distinct(sexe) == 2) %>%       # 2 partenaires, sexes différents
  summarise(
    revenu_h   = revenu[sexe == 1], revenu_f   = revenu[sexe == 2],
    matpat_h   = mois_matpat[sexe == 1], matpat_f = mois_matpat[sexe == 2],
    foyer_h    = mois_foyer[sexe == 1],  foyer_f  = mois_foyer[sexe == 2],
    poids      = poids[sexe == 1],
    .groups = "drop"
  ) %>%
  mutate(
    femme_gagne_plus  = factor(if_else(revenu_f > revenu_h, "Oui", "Non"),
                               levels = c("Non", "Oui")),
    # Outcomes côté HOMME
    homme_matpat      = as.integer(coalesce(matpat_h, 0) > 0),
    homme_parental    = as.integer(coalesce(foyer_h, 0) > 0)
  )

cat("Nombre de couples hétéro avec jeune enfant :", nrow(couples), "\n")
cat("dont la femme gagne plus :", sum(couples$femme_gagne_plus == "Oui"),
    sprintf(" (%.1f %%)\n", 100 * mean(couples$femme_gagne_plus == "Oui")))

# ── Descriptif : part d'hommes prenant un congé selon qui gagne le plus ─────
cat("\n=== Part d'HOMMES prenant un congé, selon 'la femme gagne plus' ===\n")
couples %>%
  group_by(femme_gagne_plus) %>%
  summarise(
    n                  = n(),
    conge_matpat_homme = mean(homme_matpat == 1),
    conge_parental_homme = mean(homme_parental == 1),
    .groups = "drop"
  ) %>%
  print()

# ── Modèles logistiques pondérés ────────────────────────────────────────────
design <- svydesign(ids = ~1, weights = ~poids, data = couples)

cat("\n=== Congé MATERNITÉ/PATERNITÉ (homme) ~ femme gagne plus ===\n")
m_matpat <- svyglm(homme_matpat ~ femme_gagne_plus, design = design,
                   family = quasibinomial())
print(tidy(m_matpat, conf.int = TRUE, exponentiate = TRUE))

cat("\n=== Congé PARENTAL / au foyer (homme) ~ femme gagne plus ===\n")
m_parental <- svyglm(homme_parental ~ femme_gagne_plus, design = design,
                     family = quasibinomial())
print(tidy(m_parental, conf.int = TRUE, exponentiate = TRUE))

# ── Export ───────────────────────────────────────────────────────────────────
if (!dir.exists("Output")) dir.create("Output")
write_csv(couples, paste0("Output/couples_conge_", ANNEE, ".csv"))


# ==========================================================================
#  Récap financier couples / personnes seules
#  (ex-R/04_recap_financier.R)
# ==========================================================================

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


# ==========================================================================
#  Prestations familiales par rang de naissance
#  (ex-R/05_part_presta_naissance.R)
# ==========================================================================

# ============================================================================
#  SRCV 2006-2025 — Part des prestations familiales dans le revenu des couples
#  avec une NAISSANCE RÉCENTE (N ou N-1), par rang de naissance.
# ----------------------------------------------------------------------------
#  Part = HY050N (prestations familiales, net) / HY020 (revenu disponible),
#  moyenne pondérée. Couples corésidents (COUPLEPR=1) avec un enfant né dans
#  l'année ou l'année précédente.
#    - Naissance : EVENEMEN_C (2006-2018) ; reconstruite depuis le fichier
#      individus (enfant né en N/N-1) pour les fichiers FPR (2022-2025).
#    - Rang de naissance = NENFANTS (nb d'enfants après la naissance),
#      regroupé 1re / 2e / 3e ou +.
#
#  DEUX populations comparées :
#    - "Tous statuts"           : tous les couples avec naissance.
#    - "Deux parents en emploi" : NACTOCCUP == 2 (les deux en emploi au sens du
#      recensement, congé maternité/paternité inclus).
#
#  ⚠️ EFFECTIFS : petits au 3e+ (n ~ 25-50) -> estimations bruitées.
#  ⚠️ PANEL : les vagues 2014-2018 et 2022-2025 revoient largement les MÊMES
#     ménages (~68 % d'une vague FPR à la suivante). Les évolutions d'une année
#     à l'autre ne sont donc pas des échantillons indépendants : ce sont pour
#     partie des variations intra-panel. À garder en tête pour la tendance.
#
#  Sorties : Output/part_presta_naissance.csv  +  .png (graphe comparatif)
# ============================================================================




# ── Chargement d'une vague : couples avec naissance récente ─────────────────
# (une seule lecture par millésime — le filtre "deux en emploi" est appliqué
#  ensuite sur l'objet déjà chargé)
charger <- function(annee, cfg) {
  # Naissance récente : EVENEMEN_C (2006-2018) ou reconstruction depuis le
  # fichier individus (FPR 2022-2025). Logique commune -> 00_utils.R.
  raw <- ajouter_naissance(lire_srcv(cfg$men), cfg, nom = "birth")

  v <- cfg$vars
  verifier_colonnes(raw, c("HY050N", "NENFANTS", "NACTOCCUP",
                           v[["revenu"]], v[["poids"]], v[["couple"]]),
                    paste0("(millésime ", annee, ")"))

  raw %>% transmute(
      presta  = num(HY050N),
      revenu  = num(.data[[v[["revenu"]]]]),
      nenf    = num(NENFANTS),
      couple  = trimws(as.character(.data[[v[["couple"]]]])),
      poids   = num(.data[[v[["poids"]]]]),
      birth   = birth,
      nactocc = num(NACTOCCUP)) %>%
    filter(couple == "1", birth == 1, nenf >= 1,
           !is.na(revenu), revenu > 0, !is.na(presta), !is.na(poids), poids > 0) %>%
    mutate(part = 100 * pmin(pmax(presta / revenu, 0), 1),
           rang = cut(nenf, c(0, 1, 2, Inf), labels = c("1re", "2e", "3e ou +")))
}

# ── Statistiques par rang, sur un jeu déjà chargé ───────────────────────────
# NB : la PART (presta/revenu) est un ratio de la même année -> insensible à
# l'inflation. En revanche les MONTANTS sont comparés entre millésimes : ils
# sont donc aussi exprimés en EUROS CONSTANTS 2025 (IPC INSEE). HY050N et HY020
# portent tous deux sur l'année N-1 -> déflatés par l'indice de N-1.
IPC <- charger_ipc()

stats_rang <- function(d, annee, deux_emploi) {
  if (deux_emploi) d <- filter(d, nactocc == 2)
  an <- as.integer(annee)
  d %>% group_by(rang) %>%
    summarise(annee = an, n = n(),
              part_moy        = round(weighted.mean(part, poids), 1),
              part_sd         = round(wsd(part, poids), 1),
              presta_moy      = round(weighted.mean(presta, poids)),
              presta_moy_reel = round(deflater(weighted.mean(presta, poids), an - 1, IPC, 2025)),
              revenu_moy      = round(weighted.mean(revenu, poids)),
              revenu_moy_reel = round(deflater(weighted.mean(revenu, poids), an - 1, IPC, 2025)),
              .groups = "drop")
}

# Chaque millésime n'est lu QU'UNE fois, puis réutilisé pour les 2 populations.
vagues_chargees <- imap(VAGUES, ~ charger(.y, .x))

donnees <- bind_rows(
  imap_dfr(vagues_chargees, ~ stats_rang(.x, .y, FALSE)) %>% mutate(population = "Tous statuts"),
  imap_dfr(vagues_chargees, ~ stats_rang(.x, .y, TRUE))  %>% mutate(population = "Deux parents en emploi")
) %>% relocate(population)

cat("=== PART (%) — Tous statuts ===\n")
donnees %>% filter(population == "Tous statuts") %>% select(rang, annee, part_moy) %>%
  pivot_wider(names_from = annee, values_from = part_moy) %>% arrange(rang) %>% as.data.frame() %>% print()
cat("\n=== PART (%) — Deux parents en emploi ===\n")
donnees %>% filter(population == "Deux parents en emploi") %>% select(rang, annee, part_moy) %>%
  pivot_wider(names_from = annee, values_from = part_moy) %>% arrange(rang) %>% as.data.frame() %>% print()
cat("\n=== Montant moyen des prestations, EUROS CONSTANTS 2025 — Tous statuts ===\n")
donnees %>% filter(population == "Tous statuts") %>% select(rang, annee, presta_moy_reel) %>%
  pivot_wider(names_from = annee, values_from = presta_moy_reel) %>% arrange(rang) %>% as.data.frame() %>% print()

cat("\n=== Effectifs (n) — Tous statuts ===\n")
donnees %>% filter(population == "Tous statuts") %>% select(rang, annee, n) %>%
  pivot_wider(names_from = annee, values_from = n) %>% arrange(rang) %>% as.data.frame() %>% print()

# ── Export CSV ───────────────────────────────────────────────────────────────
if (!dir.exists("Output")) dir.create("Output")
write_csv(donnees, "Output/part_presta_naissance.csv")

# ── Graphe comparatif ────────────────────────────────────────────────────────
g <- ggplot(donnees, aes(annee, part_moy, colour = population, group = population)) +
  geom_line(linewidth = 1) + geom_point(size = 2) +
  facet_wrap(~rang, nrow = 1) +
  scale_x_continuous(breaks = as.integer(names(VAGUES))) +
  scale_colour_manual(values = c("Tous statuts" = "#C0392B", "Deux parents en emploi" = "#2471A3")) +
  labs(title = "Part des prestations familiales dans le revenu des couples avec naissance récente",
       subtitle = "Moyenne pondérée HY050N / HY020, par rang de naissance — SRCV 2006-2025",
       x = "Millésime SRCV", y = "Part dans le revenu disponible (%)", colour = NULL,
       caption = paste("Couples corésidents avec naissance N/N-1. 'Deux parents en emploi' =",
                       "NACTOCCUP=2 (congé mat/pat inclus).\nVagues 2022-2025 : mêmes ménages",
                       "en grande partie (panel) -> évolutions non indépendantes.")) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "top", panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("Output/part_presta_naissance.png", g, width = 11, height = 4.5, dpi = 150)
cat("\nExporté : Output/part_presta_naissance.csv  +  Output/part_presta_naissance.png\n")


# ==========================================================================
#  Micro-simulation du quotient familial
#  (ex-R/08_quotient_familial.R)
# ==========================================================================

# ============================================================================
#  SRCV — Micro-simulation du QUOTIENT FAMILIAL : quelle baisse d'impôt sur le
#  revenu procure la naissance d'un enfant ?
# ----------------------------------------------------------------------------
#  Contexte : les dépenses de politique familiale se répartissent entre
#  prestations en espèces, services en nature, et AVANTAGES FISCAUX. Ce dernier
#  volet (quotient familial) n'est pas observable directement dans SRCV :
#  ni "nombre de parts fiscales", ni "revenu imposable" ne sont diffusés.
#  On le RECONSTRUIT donc par simulation.
#
#  MÉTHODE
#   1. Reconstituer les PARTS FISCALES à partir de COUPLEPR + NENFANTS.
#   2. Approximer le REVENU IMPOSABLE à partir de HY010 (revenu brut) diminué
#      des prestations non imposables, puis abattement de 10 %.
#   3. Appliquer le BARÈME de l'IR de l'année de revenu concernée, avec
#      plafonnement du quotient familial.
#   4. VALIDER : comparer l'IRPP simulé à l'IRPP observé. Sans cette étape, le
#      contrefactuel n'aurait aucune crédibilité.
#   5. CONTREFACTUEL : recalculer l'impôt en retirant les parts du dernier
#      enfant -> l'écart est le gain de quotient familial.
#
#  ⚠️ LIMITES ASSUMÉES
#   - Le revenu imposable est APPROXIMÉ (pas de déclaration fiscale dans SRCV) :
#     abattements réels, quotient conjugal des pensions, revenus du capital au
#     PFU, réductions/crédits d'impôt ne sont pas reproduits.
#   - La décote et les réductions d'impôt ne sont pas simulées.
#   - Barèmes et plafonds saisis en dur ci-dessous : À VÉRIFIER sur le barème
#     officiel avant toute publication.
#   - Résultat à lire comme un ORDRE DE GRANDEUR, pas comme un chiffrage
#     fiscal. Pour un chiffrage publiable, utiliser un modèle dédié
#     (OpenFisca, INES, TAXIPP).
# ============================================================================



library(survey)
library(broom)

# ── Barème de l'impôt sur le revenu, par ANNÉE DE REVENU ────────────────────
# seuils = bornes inférieures des tranches ; taux = taux marginaux.
# plafond_dp = plafonnement du quotient familial, par DEMI-PART supplémentaire.
# dec_* = décote (annule/réduit l'impôt des foyers modestes) : seuils
# d'application et montants, pour personne seule (_s) et couple (_c).
BAREME <- list(
  "2021" = list(seuils = c(0, 10225, 26070, 74545, 160336), taux = c(0,.11,.30,.41,.45),
                plafond_dp = 1592, dec_seuil_s = 1745, dec_seuil_c = 2888,
                dec_mont_s = 790, dec_mont_c = 1307),
  "2022" = list(seuils = c(0, 10777, 27478, 78570, 168994), taux = c(0,.11,.30,.41,.45),
                plafond_dp = 1678, dec_seuil_s = 1841, dec_seuil_c = 3045,
                dec_mont_s = 833, dec_mont_c = 1378),
  "2023" = list(seuils = c(0, 11294, 28797, 82341, 177106), taux = c(0,.11,.30,.41,.45),
                plafond_dp = 1759, dec_seuil_s = 1929, dec_seuil_c = 3191,
                dec_mont_s = 873, dec_mont_c = 1444),
  "2024" = list(seuils = c(0, 11497, 29315, 83823, 180294), taux = c(0,.11,.30,.41,.45),
                plafond_dp = 1791, dec_seuil_s = 1964, dec_seuil_c = 3248,
                dec_mont_s = 889, dec_mont_c = 1470)
)
TAUX_DECOTE <- 0.4525

# Impôt brut pour un revenu par part donné, selon un barème (vectorisé)
impot_par_part <- function(rpp, bar) {
  res <- rep(0, length(rpp))
  for (i in seq_along(bar$seuils)) {
    haut <- if (i < length(bar$seuils)) bar$seuils[i + 1] else Inf
    res <- res + pmax(0, pmin(rpp, haut) - bar$seuils[i]) * bar$taux[i]
  }
  res
}

# Impôt du foyer = parts × impôt(revenu/parts), avec plafonnement du QF :
# l'avantage procuré par les demi-parts au-delà de celles du couple/isolé est
# limité à plafond_dp par demi-part.
impot_foyer <- function(revenu_imp, parts, parts_base, bar) {
  t_reel <- parts      * impot_par_part(revenu_imp / parts,      bar)
  t_base <- parts_base * impot_par_part(revenu_imp / parts_base, bar)
  avantage    <- pmax(0, t_base - t_reel)                       # gain du QF
  dp_sup      <- pmax(0, (parts - parts_base) * 2)              # nb de demi-parts
  avantage_max<- dp_sup * bar$plafond_dp
  t <- t_base - pmin(avantage, avantage_max)                    # impôt après plafond
  # Décote : réduit voire annule l'impôt des foyers modestes.
  seuil <- if_else(parts_base >= 2, bar$dec_seuil_c, bar$dec_seuil_s)
  mont  <- if_else(parts_base >= 2, bar$dec_mont_c,  bar$dec_mont_s)
  pmax(0, t - pmax(0, mont - TAUX_DECOTE * t) * (t < seuil))
}

# ── Parts fiscales ──────────────────────────────────────────────────────────
# Couple : 2 parts. Personne seule : 1 part.
# Enfants : +0,5 pour les 2 premiers, +1 à partir du 3e.
# Parent isolé : le 1er enfant ouvre une part entière (case T).
parts_fiscales <- function(en_couple, nenf) {
  nenf <- pmax(coalesce(nenf, 0), 0)
  base <- if_else(en_couple, 2, 1)
  sup  <- 0.5 * pmin(nenf, 2) + 1 * pmax(nenf - 2, 0)
  sup  <- sup + if_else(!en_couple & nenf >= 1, 0.5, 0)   # part supplémentaire parent isolé
  list(parts = base + sup, base = base)
}

# ── Ménage = FOYER FISCAL ? ─────────────────────────────────────────────────
# Le ménage SRCV et le foyer fiscal ne coïncident pas toujours :
#   - les CONCUBINS déclarent séparément (2 foyers) alors qu'ils forment 1 ménage ;
#   - les ENFANTS MAJEURS peuvent déclarer pour eux-mêmes ;
#   - les autres cohabitants (parent, ami, frère...) sont des foyers distincts.
# Dans ces cas, HY010/HY020 agrègent des revenus qui ne sont pas dans la même
# déclaration -> l'impôt simulé est mécaniquement surestimé.
# On isole donc les ménages où ménage == foyer fiscal :
#   PR + (conjoint MARIÉ ou PACSÉ) + enfants MINEURS uniquement.
flag_foyer_fiscal <- function(vague) {
  i <- lire_srcv(chemin_ind(vague)) %>%
    transmute(id   = trimws(as.character(RB040)),
              lien = trimws(as.character(LIENPREF)),
              age  = num(AGE),
              sm   = trimws(as.character(SITUMATRI)))
  sm_pr <- i %>% filter(lien == "00") %>% select(id, sm_pr = sm)
  i %>% group_by(id) %>%
    summarise(
      # aucun cohabitant autre que PR / conjoint / enfant
      que_noyau     = all(lien %in% c("00", "01", "02")),
      # aucun enfant majeur (susceptible de déclarer séparément)
      sans_enf_maj  = !any(lien == "02" & age >= 18, na.rm = TRUE),
      a_conjoint    = any(lien == "01"),
      .groups = "drop") %>%
    left_join(sm_pr, by = "id") %>%
    mutate(foyer_fiscal = que_noyau & sans_enf_maj &
             # si conjoint présent : il doit être marié ou pacsé (1 seul foyer)
             (!a_conjoint | sm_pr %in% c("1", "2"))) %>%
    select(id, foyer_fiscal)
}

# ── Chargement d'une vague FPR + reconstruction de l'assiette ───────────────
charger <- function(vague) {
  an_rev <- as.character(as.integer(vague) - 1)     # les revenus portent sur N-1
  if (is.null(BAREME[[an_rev]])) return(NULL)
  d <- lire_srcv(chemin_men(vague))

  x <- tibble(
    id      = trimws(as.character(d$DB030)),
    vague   = vague, an_rev = an_rev,
    brut    = num(d$HY010),
    dispo   = num(d$HY020),
    irpp_obs= num(d$IRPP),
    # prestations NON IMPOSABLES à retirer de l'assiette
    presta_fam = coalesce(num(d$HY050N), 0),
    alloc_log  = coalesce(num(d$HY070N), 0),
    minima     = coalesce(num(d$RSA_MEN), 0) + coalesce(num(d$PPA_MEN), 0) +
                 coalesce(num(d$PREST_SOLIDARITE), 0),
    nenf    = num(d$NENFANTS),
    couple  = trimws(as.character(d$COUPLEPR)) == "1",
    poids   = num(d$DB090)) %>%
    filter(!is.na(brut), brut > 0, !is.na(poids), poids > 0, !is.na(nenf),
           !is.na(couple), !is.na(dispo), !is.na(irpp_obs))

  x <- x %>% left_join(flag_foyer_fiscal(vague), by = "id") %>%
    mutate(foyer_fiscal = coalesce(foyer_fiscal, FALSE))

  p <- parts_fiscales(x$couple, x$nenf)
  bar <- BAREME[[an_rev]]
  x %>% mutate(
    parts = p$parts, parts_base = p$base,
    # Assiette : HY020 (disponible) + IRPP = revenu APRÈS cotisations mais AVANT
    # impôt sur le revenu -> base la plus proche du revenu net imposable ;
    # on retire les prestations non imposables, puis l'abattement de 10 %.
    # (Testé contre 3 autres reconstructions : c'est celle qui valide le mieux.)
    revenu_imp = pmax(0, (dispo + irpp_obs - presta_fam - alloc_log - minima)) * 0.9,
    irpp_sim   = impot_foyer(revenu_imp, parts, parts_base, bar))
}

vagues <- names(VAGUES)[as.integer(names(VAGUES)) >= 2022]
sim <- map_dfr(vagues, charger)
cat("Ménages simulés :", nrow(sim), "| vagues :", paste(unique(sim$vague), collapse = ", "), "\n")

# ── 4. VALIDATION : simulé vs observé ───────────────────────────────────────
valider <- function(d, libelle) {
  cat("\n##########", libelle, "- n =", nrow(d), "##########\n")
  de <- svydesign(ids = ~1, weights = ~poids, data = d)
  print(svyby(~irpp_sim + irpp_obs, ~vague, de, svymean, na.rm = TRUE) %>%
          as_tibble() %>%
          transmute(vague, simule = round(irpp_sim), observe = round(irpp_obs),
                    ecart_pct = round(100 * (irpp_sim / irpp_obs - 1), 1)) %>%
          as.data.frame())
  cat("-- non imposables (%) : simule / observe --\n")
  print(d %>% group_by(vague) %>%
          summarise(sim = round(100 * weighted.mean(irpp_sim <= 0, poids), 1),
                    obs = round(100 * weighted.mean(irpp_obs <= 0, poids, na.rm = TRUE), 1),
                    .groups = "drop") %>% as.data.frame())
  cat("-- correlation ponderee r =",
      round(cov.wt(cbind(d$irpp_sim, d$irpp_obs), wt = d$poids, cor = TRUE)$cor[1, 2], 3), "\n")
}

cat("\n=== VALIDATION : IRPP simule vs observe ===\n")
valider(sim, "TOUS MENAGES")
valider(sim %>% filter(foyer_fiscal), "MENAGE = FOYER FISCAL")
cat("\nPart des menages ou menage = foyer fiscal :",
    round(100 * weighted.mean(sim$foyer_fiscal, sim$poids), 1), "%\n")

# Le contrefactuel est ensuite calcule sur le champ VALIDE.
sim <- sim %>% filter(foyer_fiscal)

# ── 5. CONTREFACTUEL : gain de quotient familial du DERNIER enfant ──────────
# On retire l'enfant marginal (parts recalculées avec nenf - 1) et on compare.
gain_dernier_enfant <- function(d) {
  bars <- split(d, d$an_rev)
  map_dfr(bars, function(g) {
    bar <- BAREME[[g$an_rev[1]]]
    p0  <- parts_fiscales(g$couple, pmax(g$nenf - 1, 0))
    g %>% mutate(
      irpp_sans_enfant = impot_foyer(revenu_imp, p0$parts, p0$base, bar),
      gain_qf = irpp_sans_enfant - irpp_sim)              # >= 0 : baisse d'impôt
  })
}
avec_enfant <- sim %>% filter(nenf >= 1) %>% gain_dernier_enfant()

des2 <- svydesign(ids = ~1, weights = ~poids, data = avec_enfant)
cat("\n\n=== GAIN DE QUOTIENT FAMILIAL du dernier enfant (€/an) ===\n")
cat("Champ : ménages avec au moins 1 enfant. n =", nrow(avec_enfant), "\n\n")

cat("-- Moyenne pondérée par rang de l'enfant --\n")
print(avec_enfant %>% mutate(rang = pmin(nenf, 4)) %>%
        group_by(rang) %>%
        summarise(n = n(),
                  gain_moyen   = round(weighted.mean(gain_qf, poids)),
                  part_nul_pct = round(100 * weighted.mean(gain_qf < 1, poids), 1),
                  gain_si_gain = round(weighted.mean(gain_qf[gain_qf >= 1],
                                                     poids[gain_qf >= 1])),
                  .groups = "drop") %>% as.data.frame())

cat("\n-- Moyenne pondérée par quintile de revenu brut --\n")
print(avec_enfant %>% mutate(q = ntile(brut, 5)) %>%
        group_by(q) %>%
        summarise(brut_moy = round(weighted.mean(brut, poids)),
                  gain_moyen = round(weighted.mean(gain_qf, poids)),
                  part_nul_pct = round(100 * weighted.mean(gain_qf < 1, poids), 1),
                  .groups = "drop") %>% as.data.frame())

cat("\n-- Ensemble --\n")
cat("  gain moyen (tous ménages avec enfant) :",
    round(coef(svymean(~gain_qf, des2))), "€/an\n")
cat("  gain moyen parmi les ménages imposables :",
    round(weighted.mean(avec_enfant$gain_qf[avec_enfant$irpp_sim > 0],
                        avec_enfant$poids[avec_enfant$irpp_sim > 0])), "€/an\n")

if (!dir.exists("Output")) dir.create("Output")
write_csv(avec_enfant %>% select(id, vague, an_rev, brut, nenf, couple, parts,
                                 revenu_imp, irpp_obs, irpp_sim, irpp_sans_enfant,
                                 gain_qf, poids),
          "Output/quotient_familial_simulation.csv")
cat("\nExporté : Output/quotient_familial_simulation.csv\n")
