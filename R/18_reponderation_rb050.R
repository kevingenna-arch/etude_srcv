# ============================================================================
#  SRCV — Deux tests sur l'anomalie de pondération des vagues 2022-2023
# ----------------------------------------------------------------------------
#  CONSTAT (cf. 17) : le taux de naissance pondéré par DB090 s'effondre en
#  2022-2023 puis rebondit en 2024, alors que le taux brut est régulier. En
#  cause : les ménages avec un nouveau-né portent en 2023 un poids médian de
#  392 contre 1 727 pour les autres. Hypothèse : DB090 est dérivé des poids
#  individuels des membres, et le nouveau-né — absent de l'échantillon initial
#  — tire le poids du ménage vers le bas.
#
#  PISTE 1 — repondérer par le POIDS INDIVIDUEL DE LA FEMME (RB050) au lieu du
#  poids du ménage. C'est aussi plus cohérent avec l'objet mesuré : « cette
#  femme a-t-elle eu un enfant » est un événement individuel. Le poids de la
#  mère, lui, ne peut pas être contaminé par l'arrivée du nouveau-né.
#    -> si la série se lisse, l'hypothèse est confirmée.
#
#  PISTE 3 — se limiter aux comparaisons ENTRE STATUTS à l'intérieur d'une même
#  vague, en retirant 2023 et 2024. La distorsion touche les trois statuts de
#  la même façon dans une vague donnée et s'annule donc en partie.
#
#  ⚠️ RB050 est ABSENT du fichier individus 2018 : cette vague ne peut pas être
#     repondérée et sort de la piste 1.
#
#  Sortie : Output/reponderation_rb050.csv
# ============================================================================

source("R/00_prepa_fecondite.R")

AGE_BAS <- 15; AGE_HAUT <- 49
ANNEES_PISTE3 <- c("2006", "2010", "2014", "2018", "2022", "2025")   # 2023-2024 retirées

STATUTS <- c(Locataire                 = "Locataires",
             Proprietaire_accedant     = "Propriétaires accédants",
             Proprietaire_non_accedant = "Propriétaires non accédants")

# ── Poids individuel de la femme du ménage, par vague ───────────────────────
#  Clé ménage : RB040 quand il existe, sinon dérivée de RB030 (= identifiant
#  ménage + numéro de personne sur 2 chiffres, avec zéros de tête). Le taux
#  d'appariement est contrôlé et affiché : sous 90 %, la vague est écartée.
poids_femme <- function(annee, cfg) {
  ind <- lire_srcv(cfg$ind)
  if (!"RB050" %in% names(ind)) {
    message("  ", annee, " : RB050 absent -> vague écartée de la piste 1")
    return(NULL)
  }
  cle <- if ("RB040" %in% names(ind)) {
    trimws(as.character(ind$RB040))
  } else {
    r <- trimws(as.character(ind$RB030))
    sub("^0+", "", substr(r, 1, nchar(r) - 2))     # retire le n° de personne
  }
  sexe  <- trimws(as.character(ind$SEXE))
  lien  <- trimws(as.character(ind$LIENPREF))
  age_v <- if ("AGE" %in% names(ind)) num(ind$AGE) else
           if ("age" %in% names(ind)) num(ind$age) else
           as.integer(annee) - num(ind[[if ("ANAIS" %in% names(ind)) "ANAIS" else "anais"]])

  tibble(id = cle, sexe, lien, age_v, w = num(ind$RB050)) %>%
    filter(sexe == "2", lien %in% c("00", "01", "0", "1"),
           !is.na(age_v), age_v >= AGE_BAS, age_v <= AGE_HAUT,
           !is.na(w), w > 0) %>%
    group_by(id) %>% summarise(poids_f = max(w), .groups = "drop")
}

base <- bind_rows(charger_millesimes(restreindre = FALSE)) %>%
  filter(!is.na(age_femme), age_femme >= AGE_BAS, age_femme <= AGE_HAUT,
         !is.na(statut3), !is.na(naissance), !is.na(poids), poids > 0,
         couple == "En_couple")

cat("=== Appariement du poids individuel de la femme (RB050) ===\n")
pf <- imap_dfr(VAGUES, function(cfg, an) {
  p <- poids_femme(an, cfg); if (is.null(p)) return(NULL)
  p %>% mutate(annee = an)
})
base <- base %>% mutate(id = normaliser_id(id)) %>% left_join(pf, by = c("annee", "id"))
print(as.data.frame(base %>% group_by(annee) %>%
  summarise(n = n(), apparies_pct = round(100 * mean(!is.na(poids_f)), 1),
            .groups = "drop")), row.names = FALSE)

# Vagues exploitables pour la piste 1
ok <- base %>% group_by(annee) %>% summarise(t = mean(!is.na(poids_f)), .groups = "drop") %>%
  filter(t >= 0.90) %>% pull(annee)
cat("Vagues retenues pour la piste 1 :", paste(ok, collapse = ", "), "\n")

# ── Tableaux ────────────────────────────────────────────────────────────────
resume <- function(d, w_col, libelle) {
  d %>% filter(!is.na(.data[[w_col]]), .data[[w_col]] > 0) %>%
    group_by(annee, statut3) %>%
    summarise(n = n(), naiss_n = sum(naissance == "Oui"),
              taux_pct = 100 * weighted.mean(naissance == "Oui", .data[[w_col]]),
              w_naiss = sum(.data[[w_col]][naissance == "Oui"]),
              w_tot   = sum(.data[[w_col]]), .groups = "drop") %>%
    group_by(annee) %>%
    mutate(part_naiss_pct = 100 * w_naiss / sum(w_naiss),
           part_pop_pct   = 100 * w_tot / sum(w_tot)) %>%
    ungroup() %>%
    mutate(ponderation = libelle, statut = unname(STATUTS[as.character(statut3)]))
}

affiche <- function(r, var, titre) {
  cat("\n--", titre, "--\n")
  print(as.data.frame(r %>%
    transmute(annee, statut, v = sprintf("%.1f", .data[[var]])) %>%
    pivot_wider(names_from = annee, values_from = v)), row.names = FALSE)
}

r_db090 <- resume(base, "poids",   "DB090 (ménage)")
r_rb050 <- resume(filter(base, annee %in% ok), "poids_f", "RB050 (femme)")
r_brut  <- base %>% mutate(un = 1) %>% resume("un", "aucune (brut)")

cat("\n\n########## PISTE 1 — REPONDERATION PAR RB050 ##########\n")
affiche(r_db090, "taux_pct", "Taux de naissance, pondéré par DB090 (ménage)")
affiche(r_rb050, "taux_pct", "Taux de naissance, pondéré par RB050 (femme)")
affiche(r_brut,  "taux_pct", "Taux de naissance, NON pondéré (référence)")

affiche(r_db090, "part_naiss_pct", "Part des naissances (somme = 100), DB090")
affiche(r_rb050, "part_naiss_pct", "Part des naissances (somme = 100), RB050")

cat("\nRapport poids moyen (naissance / pas de naissance) — le signal d'alerte :\n")
print(as.data.frame(base %>% group_by(annee) %>%
  summarise(DB090 = round(mean(poids[naissance == "Oui"]) /
                          mean(poids[naissance == "Non"]), 3),
            RB050 = if (first(annee) %in% ok)
              round(mean(poids_f[naissance == "Oui"], na.rm = TRUE) /
                    mean(poids_f[naissance == "Non"], na.rm = TRUE), 3) else NA_real_,
            .groups = "drop")), row.names = FALSE)

cat("\n\n########## PISTE 3 — SANS 2023 NI 2024 ##########\n")
p3 <- r_db090 %>% filter(annee %in% ANNEES_PISTE3)
affiche(p3, "taux_pct", "Taux de naissance par statut (DB090)")
cat("\nPart des naissances (somme = 100) et part de population :\n")
print(as.data.frame(p3 %>%
  transmute(annee, statut,
            v = sprintf("%.1f (%.1f)", part_naiss_pct, part_pop_pct)) %>%
  pivot_wider(names_from = annee, values_from = v)), row.names = FALSE)

cat("\nÉcart à la parité (part des naissances - part de population, en points) :\n")
print(as.data.frame(p3 %>%
  transmute(annee, statut, v = sprintf("%+.1f", part_naiss_pct - part_pop_pct)) %>%
  pivot_wider(names_from = annee, values_from = v)), row.names = FALSE)

if (!dir.exists("Output")) dir.create("Output")
write_csv(bind_rows(r_db090, r_rb050, r_brut) %>%
            select(ponderation, annee, statut, n, naiss_n, taux_pct,
                   part_naiss_pct, part_pop_pct),
          "Output/reponderation_rb050.csv")
cat("\nExporté : Output/reponderation_rb050.csv\n")
