# ── Packages ──────────────────────────────────────────────
library(tidyverse)   # manipulation + ggplot
library(broom)       # résultats de modèles propres

# ── Import ────────────────────────────────────────────────
srcv06 <- read_csv2("Data/lil-0457/lil-0457.csv/Csv/menages06_diff.csv")   # adapte le nom du fichier
srcv10 <- read_csv2("Data/lil-0747/lil-0747.csv/Csv/menages10_diff.csv")
srcv14 <- read_csv2("Data/lil-1090/lil-1090.csv/Csv/MENAGES14_DIFF.csv")
srcv18 <- read_csv2("Data/lil-1374/lil-1374.csv/Csv/MENAGES18_DIFF.csv")
srcv22 <- read_csv2("Data/lil-1646/lil-1646-Donnees_CSV/tab_men_fpr_2022.csv")

glimpse(srcv06)        # inspecter la structure
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