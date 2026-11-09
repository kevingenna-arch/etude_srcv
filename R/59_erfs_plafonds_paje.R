# ============================================================================
#  ERFS — 59. Autour des plafonds de la PAJE : réformes de 2014 et de 2018
# ----------------------------------------------------------------------------
#  Elmallakh (2023, Demography 60(5) ; document de travail GLO 984, 2021)
#  compare, dans SRCV 2014-2015, les couples juste au-dessus et juste en
#  dessous des plafonds de l'allocation de base de la PAJE fixés en avril 2014
#  (régression sur discontinuité) : -2 points de probabilité de naissance
#  au-dessus du plafond « zéro », rien au plafond « moitié ». L'ERFS permet de
#  refaire l'exercice et de le soumettre à des contrôles qu'une coupe de deux
#  ans n'autorise pas :
#  1. Réplication : mêmes seuils, mêmes années (2014-2015, puis 2014-2017),
#     même spécification (fonction de contrôle linéaire de part et d'autre du
#     seuil, effets fixes d'année, erreurs-types robustes, sans pondération),
#     couples ayant déjà au moins un enfant.
#  2. Par période : les mêmes seuils avant 2014 et après 2018, quand la
#     falaise de prestations était ailleurs, plus haute ou absente (la réforme
#     de 2018 a abaissé les plafonds), et les nouveaux seuils de 2018.
#  3. Différence de discontinuités : la naissance régressée sur le droit à la
#     PAJE d'un enfant à naître (allocation de base sur 36 mois et prime de
#     naissance, k€ de 2023), avec un saut propre à chaque seuil, constant dans
#     le temps, et une courbe du revenu propre à chaque période. Seuls les
#     déplacements des falaises en 2014 et en 2018 identifient l'effet.
#  Le plafond « zéro » de 2014 reprend l'ancien plafond pour un couple à un
#  revenu et un enfant, et l'abaisse de 2 à 10 % au-delà : la falaise
#  qu'étudie Elmallakh existait, plus haute, avant la réforme.
#  ⚠️ Les plafonds portent sur les revenus de N-2 ; l'ERFS ne donne que ceux
#     de l'année N, que le congé d'une naissance fait baisser (premier étage
#     flou, §2). Ressources = 90 % des revenus imposables (abattement de
#     10 %), 72 % jusqu'en 2007 (abattement de 20 % sur les revenus de 2005 et
#     avant). Biactifs : chaque conjoint a au moins 12 BMAF de revenus
#     d'activité. Enfants à charge : moins de 20 ans, plus l'enfant à naître.
#  ⚠️ Premier étage : l'ERFS ne donne que l'agrégat des prestations petite
#     enfance ; avant avril 2014, le CLCA des familles privées d'allocation
#     de base était majoré du montant de celle-ci (1,43 BMAF au lieu de 0,97).
#  Entrées : couples_ERFS.rds (script 56), Output/gain_enfant_erfs.csv (57),
#            paramètres d'OpenFisca (59_plafonds_paje.py, local)
#  Sorties : Output/plafonds_paje.csv, Output/erfs_plafonds_paje.csv,
#            Output/graph_erfs_plafonds_paje.png (sauts année par année),
#            Output/graph_erfs_plafonds_paje_classes.png (natalité par classe)
# ============================================================================

source("R/00_utils.R")
source("R/00_config.R")
suppressPackageStartupMessages(library(fixest))
IPC <- charger_ipc()

PYTHON  <- "C:/Users/kgenn/of/Scripts/python.exe"
EUROS   <- 2023
PERIODES <- list("2005-2013" = 2005:2013, "2015-2017" = 2015:2017, "2019-2023" = 2019:2023)
SEUILS  <- c("Ancien plafond" = "S_av", "Zero 2014" = "S14_pa", "Moitie 2014" = "S14_pl",
             "Zero 2018" = "S18_pa", "Moitie 2018" = "S18_pl")
ELMALLAKH_PTS <- -2.0    # saut au plafond « zéro », SRCV 2014-2015 (tableau 4)

# ── §0 Paramètres et données ───────────────────────────────────────────────
f_par <- "Output/plafonds_paje.csv"
if (!file.exists(f_par)) stopifnot(system2(PYTHON, c("-X", "utf8", "R/59_plafonds_paje.py", f_par)) == 0)
par <- read_csv(f_par, show_col_types = FALSE) %>% arrange(annee) %>%
  mutate(across(-annee, ~ if_else(is.na(.x) | .x == 0, NA_real_, .x)))

#  Plafonds d'un régime hors de ses années d'existence : revalorisés comme le
#  plafond en vigueur (ancien régime jusqu'en 2017, régime 2014 jusqu'en 2020,
#  régime 2018 ensuite). L'indice saute en 2007 avec la fin de l'abattement de
#  20 %, comme les plafonds eux-mêmes.
indice <- par$avant_p0
for (i in which(par$annee %in% 2018:2020)) indice[i] <- indice[i - 1] * par$r2014_taux_partiel_p0[i] / par$r2014_taux_partiel_p0[i - 1]
for (i in which(par$annee >= 2021))       indice[i] <- indice[i - 1] * par$r2018_taux_partiel_p0[i] / par$r2018_taux_partiel_p0[i - 1]
prolonger <- function(x, indexer = TRUE) {
  ok <- which(!is.na(x))
  j  <- vapply(seq_along(x), function(i) ok[which.min(abs(ok - i))], integer(1))
  if (indexer) x[j] * indice / indice[j] else x[j]
}
PLAFONDS <- grep("^(avant|r201[48]_taux_(plein|partiel))_(p0|biact)$", names(par), value = TRUE)
par <- par %>% mutate(across(all_of(PLAFONDS), prolonger),
                      across(c(avant_maj12, avant_maj3, r2014_maj, r2018_maj12, r2018_maj3), ~ prolonger(.x, FALSE)))

#  Plafonds pour n enfants à charge, comme dans paje_base d'OpenFisca : ancien
#  régime, taux plein (pl) et taux partiel (pa) des régimes 2014 et 2018.
maj <- function(n, a12, a3) a12 * pmin(n, 2) + a3 * pmax(n - 2, 0)
plafonds <- function(d, n) mutate(d,
  S_av   = avant_p0 * (1 + maj(n, avant_maj12, avant_maj3)) + biactif * avant_biact,
  S14_pl = r2014_taux_plein_p0 * (1 + r2014_maj * n) + biactif * r2014_taux_plein_biact,
  S14_pa = r2014_taux_partiel_p0 * (1 + r2014_maj * n) + biactif * r2014_taux_partiel_biact,
  S18_pl = r2018_taux_plein_p0 * (1 + maj(n, r2018_maj12, r2018_maj3)) + biactif * r2018_taux_plein_biact,
  S18_pa = r2018_taux_partiel_p0 * (1 + maj(n, r2018_maj12, r2018_maj3)) + biactif * r2018_taux_partiel_biact)
df <- as_tibble(readRDS(file.path(DIR_ERFS, "_extrait", "couples_ERFS.rds"))) %>%
  mutate(cle     = paste(annee, ident, sep = "_"),
         n_pf    = map_int(strsplit(coalesce(ages, ""), ";"), ~ sum(as.integer(.x) < 20, na.rm = TRUE)) + 1L,
         k_abat  = if_else(annee <= 2007, 0.72, 0.9),
         res     = k_abat * (rev_femme + rev_homme),
         regime  = case_when(annee <= 2013 ~ "avant", annee <= 2017 ~ "r2014", TRUE ~ "r2018"),
         avec_enfant = nb_enf_avant >= 1,
         age_f   = factor(age_femme), rang = factor(pmin(nb_enf_avant, 3))) %>%
  left_join(par, by = "annee") %>%
  mutate(biactif = as.integer(k_abat * coalesce(act_rev_femme, 0) >= seuil_biactif &
                                k_abat * coalesce(act_rev_homme, 0) >= seuil_biactif),
         ab     = case_when(regime == "avant" ~ avant_ab_mois, regime == "r2014" ~ r2014_ab_mois, TRUE ~ r2018_ab_mois))
df <- plafonds(df, df$n_pf)
stopifnot(!anyNA(select(df, starts_with("S_"), starts_with("S1"), ab, prime, res)))

#  Droit à la PAJE d'un enfant à naître sous le régime de l'année, pour des
#  ressources r : allocation de base sur 36 mois (moitié au taux partiel) et
#  prime de naissance, en k€ de 2023.
droit <- function(d, r) {
  v <- with(d, case_when(
    regime == "avant" ~ (r <= S_av) * (36 * ab + prime),
    regime == "r2014" ~ (r <= S14_pl) * 18 * ab + (r <= S14_pa) * (18 * ab + prime),
    TRUE              ~ (r <= S18_pl) * 18 * ab + (r <= S18_pa) * (18 * ab + prime)))
  deflater(v, d$annee, IPC, base = EUROS) / 1000
}
df <- df %>% mutate(droit_k = droit(df, res))
for (s in SEUILS) df[[paste0("fal_", s)]] <- droit(df, df[[s]] - 1) - droit(df, df[[s]] + 1)
cat("Champ :", nrow(df), "couples |", sum(df$naiss01), "naissances\n")
cat("\nPlafonds rapportés à l'ancien plafond, 2016 (moyenne par nombre d'enfants à charge et biactivité) :\n")
print(as.data.frame(df %>% filter(annee == 2016) %>% group_by(n_pf = pmin(n_pf, 4), biactif) %>%
  summarise(ancien_plafond = round(mean(S_av)), across(c(S14_pa, S14_pl, S18_pa, S18_pl), ~ round(mean(.x / S_av), 3)),
            couples = n(), .groups = "drop")), row.names = FALSE)

# ── §1 Contrôle : bandes calculées contre la simulation OpenFisca (script 57)
#  Barème de l'année, couples sans enfant de moins de 3 ans (l'allocation de
#  base du nouveau-né n'y double pas celle d'un aîné) ; enfant simulé né le
#  1er janvier, donc régime 2014 en 2015-2017 et régime 2018 ensuite.
sim <- read_csv("Output/gain_enfant_erfs.csv", show_col_types = FALSE, col_select = c(cle, paje_base)) %>%
  filter(!grepl("_b[0-9]{4}$", cle))
controle <- df %>% filter(annee %in% c(2015:2017, 2019:2023), is.na(benjamin) | benjamin >= 3) %>%
  inner_join(sim, by = "cle") %>%
  mutate(pl = if_else(regime == "r2014", S14_pl, S18_pl), pa = if_else(regime == "r2014", S14_pa, S18_pa),
         calculee = case_when(res <= pl ~ "Taux plein", res <= pa ~ "Taux partiel", TRUE ~ "Rien"),
         simulee  = case_when(paje_base / ab > 7 ~ "Taux plein", paje_base / ab > 2 ~ "Taux partiel", TRUE ~ "Rien")) %>%
  count(regime, calculee, simulee) %>%
  group_by(regime) %>% mutate(part_pct = 100 * n / sum(n)) %>% ungroup()
cat("\nBande calculée contre bande simulée (", round(100 * sum(controle$n[controle$calculee == controle$simulee]) /
    sum(controle$n), 1), "% d'accord) :\n")
print(as.data.frame(controle), row.names = FALSE)

# ── §2 Premier étage : prestations petite enfance observées ────────────────
#  Couples dont le benjamin a 1 ou 2 ans, sans naissance dans l'année, et
#  plafonds pour les enfants présents : saut des prestations observées
#  (allocation de base, complément de mode de garde, PreParE, k€ de 2023 par
#  an) à chaque seuil, contre le saut attendu de la seule allocation de base si
#  le revenu de N était celui de N-2. Le premier étage de la différence de
#  discontinuités vient au §5.
rd <- function(d, seuil, h = Inf, y = "naiss01", fe = "annee", ponderer = FALSE, ctrl = "") {
  d <- d %>% mutate(x = (res - .data[[seuil]]) / 1000, T = as.integer(x > 0), w = if (ponderer) poids else 1) %>%
    filter(abs(x) <= h)
  m <- feols(as.formula(paste(y, "~ T + x + T:x", ctrl, "|", fe)), data = d, vcov = "hetero", weights = ~w)
  tibble(saut = coef(m)[["T"]], se = se(m)[["T"]], p = pvalue(m)[["T"]], n = nobs(m),
         naissances = sum(d$naiss01), taux_pct = 100 * mean(d$naiss01),
         falaise_k = mean(d[[paste0("fal_", seuil)]]))
}
pe <- df %>% filter(benjamin %in% 1:2, naiss01 == 0) %>%
  mutate(pe_k = deflater(prest_pe, annee, IPC, base = EUROS) / 1000,
         ab_an_k = deflater(12 * ab, annee, IPC, base = EUROS) / 1000)
pe <- plafonds(pe, pe$n_pf - 1L) %>%
  mutate(ab_droit_k = ab_an_k * case_when(regime == "avant" ~ as.numeric(res <= S_av),
                                          regime == "r2014" ~ 0.5 * (res <= S14_pl) + 0.5 * (res <= S14_pa),
                                          TRUE              ~ 0.5 * (res <= S18_pl) + 0.5 * (res <= S18_pa)))
premier_etage <- bind_rows(
  rd(filter(pe, annee %in% 2009:2013), "S_av", 5, "pe_k") %>%
    mutate(seuil = "Ancien plafond", periode = "2009-2013", attendu = -mean(pe$ab_an_k[pe$annee %in% 2009:2013])),
  rd(filter(pe, annee %in% 2016:2017), "S14_pl", 5, "pe_k") %>%
    mutate(seuil = "Moitie 2014", periode = "2016-2017", attendu = -mean(pe$ab_an_k[pe$annee %in% 2016:2017]) / 2),
  rd(filter(pe, annee %in% 2016:2017), "S14_pa", 5, "pe_k") %>%
    mutate(seuil = "Zero 2014", periode = "2016-2017", attendu = -mean(pe$ab_an_k[pe$annee %in% 2016:2017]) / 2),
  rd(filter(pe, annee %in% 2021:2023), "S18_pa", 5, "pe_k") %>%
    mutate(seuil = "Zero 2018", periode = "2021-2023", attendu = -mean(pe$ab_an_k[pe$annee %in% 2021:2023]) / 2)) %>%
  transmute(seuil, periode, saut_keur = saut, se, p, attendu_keur = attendu, part_attendue_pct = 100 * saut / attendu, n)
cat("\nPremier étage, prestations petite enfance observées (k€ de 2023 par an, fenêtre ±5 k€) :\n")
print(as.data.frame(premier_etage %>% mutate(across(where(is.double), ~ round(.x, 3)))), row.names = FALSE)

# ── §3 Réplication d'Elmallakh ─────────────────────────────────────────────
#  « Moitié » : couples sous le plafond zéro, saut au plafond du taux plein ;
#  « zéro » : tous les couples, saut au plafond du taux partiel.
ECH_REPLI <- list("Couples avec enfant(s)" = function(d) filter(d, avec_enfant),
                  "Recevant des prestations familiales" = function(d) filter(d, avec_enfant, prest_pe + prest_autres > 0),
                  "Tous les couples" = function(d) d)
FENETRES <- c("Tout l'echantillon" = Inf, "+/-10 k€" = 10, "+/-7,5 k€" = 7.5, "+/-5 k€" = 5, "+/-2,5 k€" = 2.5)
replication <- imap_dfr(list("2014-2015" = 2014:2015, "2014-2017" = 2014:2017), function(ans, per)
  imap_dfr(ECH_REPLI, function(f, ech) imap_dfr(FENETRES, function(h, fen) {
    d <- f(filter(df, annee %in% ans))
    bind_rows(rd(filter(d, res <= S14_pa), "S14_pl", h) %>% mutate(seuil = "Moitie 2014"),
              rd(d, "S14_pa", h) %>% mutate(seuil = "Zero 2014")) %>%
      mutate(annees = per, echantillon = ech, fenetre = fen)
  })))
replication <- bind_rows(replication,
  imap_dfr(list("Ponderee" = list(poids = TRUE, ctrl = ""),
                "Controles (age, rang, biactivite)" = list(poids = FALSE, ctrl = "+ biactif | annee + age_f + rang")),
    function(v, nom) {
      d <- filter(df, annee %in% 2014:2015, avec_enfant)
      fe <- if (v$ctrl == "") "annee" else "annee + age_f + rang"
      ctrl <- if (v$ctrl == "") "" else "+ biactif"
      bind_rows(rd(filter(d, res <= S14_pa), "S14_pl", 10, fe = fe, ponderer = v$poids, ctrl = ctrl) %>% mutate(seuil = "Moitie 2014"),
                rd(d, "S14_pa", 10, fe = fe, ponderer = v$poids, ctrl = ctrl) %>% mutate(seuil = "Zero 2014")) %>%
        mutate(annees = "2014-2015", echantillon = paste("Couples avec enfant(s),", tolower(nom)), fenetre = "+/-10 k€")
    })) %>%
  transmute(annees, echantillon, fenetre, seuil, saut_pts = 100 * saut, se = 100 * se, p, n, naissances, taux_pct, falaise_k)
cat("\nRéplication d'Elmallakh (saut de la probabilité de naissance au-dessus du seuil, points) :\n")
print(as.data.frame(replication %>% mutate(across(where(is.double), ~ round(.x, 3)))), row.names = FALSE)

# ── §4 Les mêmes seuils, période par période ───────────────────────────────
par_periode <- imap_dfr(PERIODES, function(ans, per) imap_dfr(SEUILS, function(s, nom)
  imap_dfr(c("+/-5 k€" = 5, "+/-10 k€" = 10), function(h, fen)
    imap_dfr(list("Couples avec enfant(s)" = TRUE, "Tous les couples" = FALSE), function(avec, ech) {
      d <- filter(df, annee %in% ans, avec_enfant | !avec)
      rd(d, s, h) %>% mutate(periode = per, seuil = nom, fenetre = fen, echantillon = ech)
    })))) %>%
  transmute(echantillon, fenetre, seuil, periode, saut_pts = 100 * saut, se = 100 * se, p, n, naissances, taux_pct, falaise_k)
cat("\nSaut à chaque seuil par période, et falaise de prestations à ce seuil (k€ de 2023) :\n")
print(as.data.frame(par_periode %>% filter(echantillon == "Couples avec enfant(s)") %>%
                      mutate(across(where(is.double), ~ round(.x, 3)))), row.names = FALSE)

#  Année par année, spécification d'Elmallakh (±10 k€) aux plafonds zéro de
#  2014 et de 2018 : si la PAJE joue, le saut se creuse quand la falaise
#  apparaît (2014-2017 pour le premier, 2019-2023 pour le second) ; sinon,
#  l'écart-type des sauts d'une année à l'autre doit être celui du bruit
#  d'échantillonnage (l'erreur-type moyenne).
annuel <- map_dfr(2005:2023, function(a) imap_dfr(SEUILS[c("Zero 2014", "Zero 2018")], function(s, nom)
  rd(filter(df, annee == a, avec_enfant), s, 10) %>% mutate(annee = a, seuil = nom))) %>%
  transmute(seuil, annee, saut_pts = 100 * saut, se = 100 * se, p, n, naissances, taux_pct, falaise_k)
cat("\nSaut année par année (points, ±10 k€) ; écart-type des sauts et erreur-type moyenne :\n")
print(as.data.frame(annuel %>% group_by(seuil) %>%
  summarise(ecart_type_sauts = sd(saut_pts), erreur_type_moyenne = sqrt(mean(se^2)),
            sauts_sous_moins_2 = sum(saut_pts <= ELMALLAKH_PTS), annees = n(), .groups = "drop") %>%
  mutate(across(where(is.double), ~ round(.x, 2)))), row.names = FALSE)
print(as.data.frame(annuel %>% mutate(across(where(is.double), ~ round(.x, 2)))), row.names = FALSE)

# ── §5 Différence de discontinuités : effet de 1 000 € de droit à la PAJE ──
#  naiss ~ droit + sauts propres aux cinq seuils (constants dans le temps)
#  + polynôme de degré 3 du revenu rapporté à l'ancien plafond, propre à
#  chaque période | année + nombre d'enfants × biactivité. Fenêtre : 50 à
#  150 % de l'ancien plafond. Un seul couple-année par ligne : erreurs-types
#  robustes.
#  Premier étage dans le même cadre : prestations petite enfance observées
#  (couples dont le benjamin a 1 ou 2 ans) sur l'allocation de base annuelle
#  à laquelle leurs ressources de N donnent droit. Sa pente mesure la part
#  du droit calculé qui se retrouve dans les prestations versées ; l'effet sur
#  les naissances, divisé par cette pente, est l'effet corrigé de l'erreur sur
#  les ressources (revenus de N au lieu de N-2). Années 2016-2017 et 2020-2023
#  pour que le benjamin soit né sous le régime de l'année.
prep_dd <- function(d) d %>% filter(annee %in% unlist(PERIODES)) %>%
  mutate(periode = names(PERIODES)[findInterval(annee, c(2005, 2015, 2019))],
         r = res / S_av, r2 = r^2, r3 = r^3,
         au_av = as.integer(res > S_av), au_14pa = as.integer(res > S14_pa), au_14pl = as.integer(res > S14_pl),
         au_18pa = as.integer(res > S18_pa), au_18pl = as.integer(res > S18_pl),
         petit = !is.na(benjamin) & benjamin < 3)
diff_disc <- function(d, fenetre = c(0.5, 1.5), poly = "r + r2 + r3", ponderer = FALSE, fe_extra = "",
                      y = "naiss01", x = "droit_k") {
  d <- filter(d, r >= fenetre[1], r <= fenetre[2]) %>% mutate(w = if (ponderer) poids else 1)
  m <- feols(as.formula(paste0(y, " ~ ", x, " + au_av + au_14pa + au_14pl + au_18pa + au_18pl | annee + n_pf^biactif",
                               fe_extra, " + periode[", gsub(" \\+ ", ", ", poly), "]")),
             data = d, vcov = "hetero", weights = ~w)
  tibble(effet = coef(m)[[x]], se = se(m)[[x]], p = pvalue(m)[[x]], n = nobs(m),
         naissances = sum(d$naiss01), taux_pct = 100 * mean(d$naiss01))
}
dd_base <- prep_dd(df)
pe_dd <- prep_dd(filter(pe, annee %in% c(2005:2013, 2016:2017, 2020:2023)))
pente_pe <- bind_rows(
  diff_disc(pe_dd, y = "pe_k", x = "ab_droit_k") %>% mutate(variante = "Toutes periodes"),
  diff_disc(filter(pe_dd, periode != "2019-2023"), y = "pe_k", x = "ab_droit_k") %>% mutate(variante = "Reforme 2014"),
  diff_disc(filter(pe_dd, periode != "2005-2013"), y = "pe_k", x = "ab_droit_k") %>% mutate(variante = "Reforme 2018"),
  feols(pe_k ~ ab_droit_k | annee + n_pf^biactif, data = pe_dd, vcov = "hetero") %>%
    { tibble(effet = coef(.)[[1]], se = se(.)[[1]], p = pvalue(.)[[1]], n = nobs(.)) } %>%
    mutate(variante = "Sans controle du revenu (variation totale)")) %>%
  select(variante, pente = effet, se, p, n)
cat("\nPremier étage de la différence de discontinuités (€ versés par € de droit calculé) :\n")
print(as.data.frame(pente_pe %>% mutate(across(where(is.double), ~ round(.x, 3)))), row.names = FALSE)

#  Pente qu'implique Elmallakh : -2 points pour la falaise du plafond zéro en 2014-2015
falaise_E <- mean(filter(df, annee %in% 2014:2015, avec_enfant, abs(res - S14_pa) <= 10000)$fal_S14_pa)
pente_E <- -ELMALLAKH_PTS / falaise_E
VARIANTES <- list(
  "Reference"                                  = list(d = function(d) filter(d, avec_enfant)),
  "Ponderee"                                   = list(d = function(d) filter(d, avec_enfant), poids = TRUE),
  "Fenetre 70-130 %"                           = list(d = function(d) filter(d, avec_enfant), fenetre = c(0.7, 1.3)),
  "Polynome de degre 2"                        = list(d = function(d) filter(d, avec_enfant), poly = "r + r2"),
  "Controles (age, rang)"                      = list(d = function(d) filter(d, avec_enfant), fe_extra = " + age_f + rang"),
  "Sans enfant de moins de 3 ans"              = list(d = function(d) filter(d, avec_enfant, !petit)),
  "Tous les couples (dont 1er enfant)"         = list(d = function(d) d),
  "Periodes 2005-2013 et 2015-2017 (reforme 2014)" = list(d = function(d) filter(d, avec_enfant, periode != "2019-2023")),
  "Periodes 2015-2017 et 2019-2023 (reforme 2018)" = list(d = function(d) filter(d, avec_enfant, periode != "2005-2013")))
dose <- imap_dfr(VARIANTES, function(v, nom)
  diff_disc(v$d(dd_base), v$fenetre %||% c(0.5, 1.5), v$poly %||% "r + r2 + r3", v$poids %||% FALSE, v$fe_extra %||% "") %>%
    mutate(variante = nom, .before = 1)) %>%
  transmute(variante, effet_pts_keur = 100 * effet, se = 100 * se, p, mde = 2.8 * se, n, naissances, taux_pct,
            pente_elmallakh = pente_E, p_egal_elmallakh = 2 * pnorm(-abs(effet_pts_keur - pente_E) / se),
            # Part du droit calculé qui doit atteindre les ménages pour que la
            # pente d'Elmallakh soit rejetée à 5 % (borne haute / pente)
            transmission_min_pct = 100 * (effet_pts_keur + 1.96 * se) / pente_E)
cat("\nDifférence de discontinuités : effet de 1 000 € de droit à la PAJE (points) ; Elmallakh implique",
    round(pente_E, 2), "point par k€ (falaise de", round(falaise_E, 2), "k€) :\n")
print(as.data.frame(dose %>% mutate(across(where(is.double), ~ round(.x, 3)))), row.names = FALSE)

# ── §6 Graphiques ──────────────────────────────────────────────────────────
#  (a) Saut année par année aux plafonds zéro de 2014 et 2018, intervalle à
#      95 %, avec le saut d'Elmallakh en pointillé ; années où la falaise
#      existe grisées.
theme_59 <- theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank(), panel.spacing = unit(1.4, "lines"),
        panel.grid.major = element_line(colour = "#e6e5e2", linewidth = 0.3), panel.grid.major.x = element_blank(),
        strip.text = element_text(colour = "#0b0b0b", size = 10.5, face = "bold"),
        axis.text = element_text(colour = "#52514e"), axis.title = element_text(colour = "#52514e", size = 10))
LIB_SEUILS <- c("Zero 2014" = "Plafond zéro de 2014", "Zero 2018" = "Plafond zéro de 2018")
zones <- tibble(seuil = c("Zero 2014", "Zero 2018"), debut = c(2013.5, 2017.5), fin = c(2017.5, 2023.5))
g1 <- ggplot(annuel, aes(annee, saut_pts)) +
  geom_rect(data = zones, aes(xmin = debut, xmax = fin, ymin = -Inf, ymax = Inf), inherit.aes = FALSE, fill = "#efeeec") +
  geom_hline(yintercept = 0, colour = "#52514e", linewidth = 0.4) +
  geom_hline(yintercept = ELMALLAKH_PTS, colour = "#d6582a", linewidth = 0.5, linetype = "22") +
  geom_linerange(aes(ymin = saut_pts - 1.96 * se, ymax = saut_pts + 1.96 * se), colour = "#2a78d6", linewidth = 0.6) +
  geom_point(colour = "#2a78d6", size = 2.2) +
  facet_wrap(~ seuil, nrow = 1, labeller = labeller(seuil = LIB_SEUILS)) +
  scale_x_continuous(breaks = seq(2005, 2023, 3)) +
  labs(x = NULL, y = "Saut de la natalité au-dessus du plafond\n(points, couples avec enfant(s))") +
  theme_59
ggsave("Output/graph_erfs_plafonds_paje.png", g1, width = 9, height = 3.6, dpi = 250, bg = "white")

#  (b) Taux de natalité par classe de 2 k€ autour des mêmes plafonds, par période
pts <- imap_dfr(PERIODES, function(ans, per) imap_dfr(SEUILS[c("Zero 2014", "Zero 2018")], function(s, nom) {
  d <- filter(df, annee %in% ans, avec_enfant) %>% mutate(x = (res - .data[[s]]) / 1000) %>% filter(abs(x) <= 10)
  d %>% mutate(classe = floor(x / 2) * 2 + 1) %>% group_by(classe) %>%
    summarise(taux = 100 * mean(naiss01), n = n(), .groups = "drop") %>%
    mutate(periode = per, seuil = nom, falaise = mean(d[[paste0("fal_", s)]]))
}))
ajust <- pts %>% mutate(cote = classe > 0) %>% group_by(periode, seuil, cote) %>%
  group_modify(~ { m <- lm(taux ~ classe, data = .x, weights = n)
                   tibble(classe = if (.y$cote) c(0, 10) else c(-10, 0), taux = predict(m, tibble(classe = classe))) }) %>%
  ungroup()
etiq <- pts %>% distinct(periode, seuil, falaise) %>%
  mutate(lib = paste0("Falaise : ", formatC(round(1000 * falaise, -1), format = "d", big.mark = " "), " €"))
g <- ggplot(pts, aes(classe, taux)) +
  geom_vline(xintercept = 0, colour = "#c8c6c1", linewidth = 0.5) +
  geom_line(data = ajust, aes(group = cote), colour = "#2a78d6", linewidth = 0.7) +
  geom_point(colour = "#2a78d6", size = 1.9) +
  geom_text(data = etiq, aes(x = -9.6, y = Inf, label = lib), hjust = 0, vjust = 1.6, size = 3, colour = "#52514e") +
  facet_grid(seuil ~ periode, labeller = labeller(seuil = LIB_SEUILS)) +
  scale_x_continuous(breaks = seq(-10, 10, 5)) +
  labs(x = "Ressources moins le plafond (k€)", y = "Naissances pour 100 couples avec enfant(s)") +
  theme_59 + theme(panel.grid.major.x = element_line(colour = "#e6e5e2", linewidth = 0.3))
ggsave("Output/graph_erfs_plafonds_paje_classes.png", g, width = 9, height = 5.4, dpi = 250, bg = "white")
cat("\nGraphiques écrits : Output/graph_erfs_plafonds_paje.png, Output/graph_erfs_plafonds_paje_classes.png\n")

# ── Sortie ─────────────────────────────────────────────────────────────────
bind_rows(controle %>% mutate(bloc = "Controle de la simulation"),
          premier_etage %>% mutate(bloc = "Premier etage"),
          pente_pe %>% mutate(bloc = "Premier etage, difference de discontinuites"),
          replication %>% mutate(bloc = "Replication Elmallakh"),
          par_periode %>% mutate(bloc = "Par periode"),
          annuel %>% mutate(bloc = "Saut annee par annee"),
          dose %>% mutate(bloc = "Difference de discontinuites")) %>%
  relocate(bloc) %>%
  write_csv("Output/erfs_plafonds_paje.csv")
cat("Résultats écrits : Output/erfs_plafonds_paje.csv\n")
