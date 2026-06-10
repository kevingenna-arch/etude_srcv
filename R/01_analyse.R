# ── Packages ──────────────────────────────────────────────
library(tidyverse)   # manipulation + ggplot
library(broom)       # résultats de modèles propres

# ── Import ────────────────────────────────────────────────
srcv <- read_csv("data/srcv.csv")   # adapte le nom du fichier

glimpse(srcv)        # inspecter la structure
summary(srcv)

# ── Préparation ───────────────────────────────────────────
# Exemple : recoder, filtrer, sélectionner les variables utiles
# srcv <- srcv %>% filter(!is.na(revenu)) %>% mutate(...)

# ── Modèle ────────────────────────────────────────────────
# Régression linéaire : remplace par tes vraies variables
modele <- lm(revenu ~ age + diplome + categorie_socio, data = srcv)

summary(modele)
tidy(modele)         # coefficients en data.frame
glance(modele)       # R², AIC, etc.

# ── Export ────────────────────────────────────────────────
tidy(modele) %>% write_csv("output/coefficients.csv")