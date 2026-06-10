# ── Packages ──────────────────────────────────────────────
library(tidyverse)   # manipulation + ggplot
library(survey)      # régression pondérée tenant compte du plan de sondage
library(broom)       # résultats de modèles propres

# ── Import ────────────────────────────────────────────────
srcv06 <- read_csv2("Data/lil-0457/lil-0457.csv/Csv/menages06_diff.csv")   # adapte le nom du fichier
srcv10 <- read_csv2("Data/lil-0747/lil-0747.csv/Csv/menages10_diff.csv")
srcv14 <- read_csv2("Data/lil-1090/lil-1090.csv/Csv/MENAGES14_DIFF.csv")
srcv18 <- read_csv2("Data/lil-1374/lil-1374.csv/Csv/MENAGES18_DIFF.csv")
srcv22 <- read_csv2("Data/lil-1646/lil-1646-Donnees_CSV/tab_men_fpr_2022.csv")

summary(srcv06)

# ── Sélection des variables utiles ──────────────────────────────────────────
# On ne garde que ce dont on a besoin pour alléger le jeu de données.
# Toutes ces variables sont issues du dictionnaire SRCV 2006 (table ménages).
vars <- c(
  "EVENEMEN_c",  # Y : naissance récente (0/1)
  "HH070",       # coût total mensuel du logement (€)
  "HY020",       # revenu total disponible annuel du ménage (€)
  "HX070",       # statut d'occupation (1 = proprio/gratuit, 2 = locataire)
  "HX040",       # nombre de personnes du ménage
  "NENFANTS",    # nombre d'enfants déjà présents
  "CS24PR",      # CSP regroupée de la personne de référence
  "DIP14PR",     # diplôme le plus élevé de la PR
  "DB040",       # région (NUTS2)
  "DB090"        # poids de sondage
)

# Remarque : les colonnes du fichier sont de type caractère pour beaucoup de
# variables (codées en texte). On convertit explicitement les variables
# numériques pour éviter les surprises.
d <- srcv06 %>%
  select(any_of(vars)) %>%
  mutate(
    HH070    = as.numeric(HH070),
    HY020    = as.numeric(HY020),
    HX040    = as.numeric(HX040),
    NENFANTS = as.numeric(NENFANTS),
    DB090    = as.numeric(DB090)
  )

# ── Construction de la variable d'intérêt : taux d'effort logement ──────────
# Coût du logement : HH070 est MENSUEL -> on l'annualise (x12) pour le rapporter
# au revenu HY020 qui est ANNUEL.
# Taux d'effort = (coût logement annuel) / (revenu disponible annuel)
#
# Précautions :
#  - on écarte les revenus <= 0 (HY020 peut être négatif) : un taux d'effort
#    n'a pas de sens avec un revenu nul ou négatif.
#  - on borne le taux d'effort à 1 (100 %) pour neutraliser quelques valeurs
#    extrêmes peu plausibles. À ajuster selon ton choix méthodologique.

d <- d %>%
  filter(!is.na(HY020), HY020 > 0,
         !is.na(HH070)) %>%
  mutate(
    cout_logement_annuel = HH070 * 12,
    taux_effort = cout_logement_annuel / HY020
  ) %>%
  filter(taux_effort >= 0, taux_effort <= 1)   # garde les taux d'effort plausibles

# Aperçu de la distribution du taux d'effort
summary(d$taux_effort)
quantile(d$taux_effort, probs = c(.1, .25, .5, .75, .9, .95, .99), na.rm = TRUE)

# ── Recodages des variables explicatives ────────────────────────────────────
d <- d %>%
  mutate(
    # Variable à expliquer en facteur (0/1)
    naissance = factor(EVENEMEN_c, levels = c(0, 1),
                       labels = c("Non", "Oui")),
    
    # Statut d'occupation lisible
    statut_occ = factor(HX070, levels = c(1, 2),
                        labels = c("Proprietaire_ou_gratuit", "Locataire")),
    
    # Nombre d'enfants déjà présents, en classes (0 / 1 / 2 / 3+)
    nb_enfants = cut(NENFANTS,
                     breaks = c(-Inf, 0, 1, 2, Inf),
                     labels = c("0", "1", "2", "3+")),
    
    # CSP, diplôme et région en facteurs
    csp     = factor(CS24PR),
    diplome = factor(DIP14PR),
    region  = factor(DB040)
  )

# ── Statistiques descriptives avant modélisation ────────────────────────────
# Taux de naissance selon le quartile de taux d'effort : un premier coup d'œil
d %>%
  mutate(q_effort = ntile(taux_effort, 4)) %>%
  group_by(q_effort) %>%
  summarise(
    n             = n(),
    taux_naissance = mean(EVENEMEN_c == 1, na.rm = TRUE)
  )

# ── Plan de sondage pondéré ─────────────────────────────────────────────────
# On déclare le design : pondération par DB090.
# NB : SRCV a un plan de sondage complexe (strates, grappes) ; idéalement il
# faudrait les identifiants de strate/grappe. À défaut, on pondère au moins par
# DB090, ce qui corrige déjà le principal biais. À affiner si tu disposes des
# variables de strate.
design <- svydesign(
  ids     = ~1,        # pas d'identifiant de grappe disponible ici
  weights = ~DB090,
  data    = d
)

# ── Modèle logistique pondéré ───────────────────────────────────────────────
# Probabilité d'une naissance expliquée par le taux d'effort logement
# + variables de contrôle.
modele <- svyglm(
  naissance ~ taux_effort + statut_occ + nb_enfants +
    HX040 + csp,
  design = design,
  family = quasibinomial()   # quasibinomial : adapté aux données pondérées
)

# ── Résultats ───────────────────────────────────────────────────────────────
summary(modele)

# Odds ratios (exp des coefficients) + intervalles de confiance, plus lisibles
resultats <- broom::tidy(modele, conf.int = TRUE, exponentiate = TRUE)
print(resultats, n = Inf)

# Export des résultats
if (!dir.exists("output")) dir.create("output")
write_csv(resultats, "output/resultats_logit_naissance.csv")

# ── Lecture rapide ──────────────────────────────────────────────────────────
# Pour la variable taux_effort :
#  - odds ratio > 1  -> un taux d'effort plus élevé est associé à PLUS de
#                       naissances récentes
#  - odds ratio < 1  -> associé à MOINS de naissances
# ATTENTION : association, pas causalité (cf. discussion sur le sens du lien
# et la nature transversale de SRCV 2006).