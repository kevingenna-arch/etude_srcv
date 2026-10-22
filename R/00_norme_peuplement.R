# ============================================================================
#  SRCV — Norme de peuplement de l'Insee : y a-t-il la place pour un enfant
#  de plus ? (sourcé par les scripts 44 et 46)
# ----------------------------------------------------------------------------
#  Norme : une pièce de séjour ; une pièce par couple ; une pièce par personne
#  de 19 ans ou plus hors du couple ; pour les moins de 19 ans, une pièce pour
#  deux enfants s'ils ont moins de 7 ans ou sont de même sexe, sinon une pièce
#  par enfant.
#  - besoin : pièces requises par le ménage ;
#  - supplément : pièce en plus qu'exigerait un enfant de plus (0 s'il peut
#    partager avec un jeune enfant seul dans sa chambre ; 1/2 s'il ne peut
#    partager qu'avec un aîné seul, selon son sexe ; 1 sinon) ;
#  - place : « place pour un enfant de plus » si pièces ≥ besoin + supplément,
#    « il manque déjà une pièce » si pièces < besoin.
# ============================================================================

PLACE <- c("Place pour un enfant de plus", "Pas de place pour un enfant de plus",
           "Il manque deja une piece")

# Membres hors couple de référence (enfants et autres personnes), toutes vagues.
membres_hors_couple <- function(ans) imap_dfr(VAGUES[ans], function(cfg, an) {
  i <- lire_srcv(cfg$ind); names(i) <- toupper(names(i))
  tibble(annee = an, id_n = cle_menage_ind(i), age = ages_individus(i, an),
         sexe = trimws(as.character(i$SEXE)), lien = trimws(as.character(i$LIENPREF)))
}) %>% filter(!lien %in% c("00", "01", "0", "1"), !is.na(age))

# Comptes par ménage : jeunes (< 7 ans) et aînés (7-18 ans) par sexe, adultes.
compter_membres <- function(m) m %>% group_by(annee, id_n) %>%
  summarise(jg = sum(age < 7 & sexe == "1"), jf = sum(age < 7 & sexe == "2"),
            ag = sum(age >= 7 & age < 19 & sexe == "1"), af = sum(age >= 7 & age < 19 & sexe == "2"),
            adultes = sum(age >= 19), .groups = "drop")

# Pièces requises par les moins de 19 ans : aînés de même sexe par deux ; un
# aîné seul peut partager avec un jeune de même sexe ; les jeunes entre eux.
pieces_enfants <- function(jg, jf, ag, af) {
  r <- ceiling(ag / 2) + ceiling(af / 2)
  jg2 <- pmax(jg - (ag %% 2 == 1 & jg > 0), 0)
  jf2 <- pmax(jf - (af %% 2 == 1 & jf > 0), 0)
  r + ceiling((jg2 + jf2) / 2)
}

# Ajoute besoin, supplément, marge et place à des données qui portent pieces et
# les comptes jg, jf, ag, af, adultes (NA = aucun membre hors couple).
ajouter_place <- function(d) d %>%
  mutate(across(c(jg, jf, ag, af, adultes), ~ coalesce(.x, 0L)),
         besoin = 2 + adultes + pieces_enfants(jg, jf, ag, af),
         supp = (pieces_enfants(jg + 1, jf, ag, af) + pieces_enfants(jg, jf + 1, ag, af)) / 2 -
           pieces_enfants(jg, jf, ag, af),
         marge = pieces - besoin,
         place = factor(case_when(marge < 0 ~ PLACE[3], marge < supp ~ PLACE[2], TRUE ~ PLACE[1]),
                        levels = PLACE))
