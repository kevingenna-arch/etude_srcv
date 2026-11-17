# ============================================================================
#  70. Tension du logement et voisinage : modèle spatial sur une géographie
#      mixte (recensements 2014, 2019, 2023)
# ----------------------------------------------------------------------------
#  Le script 67 compare les cantons-ou-villes selon leur propre tension. Ici on
#  ajoute la tension des territoires voisins, sur trois échelles :
#   - MIXTE : la commune quand le recensement la donne (communes découpées en
#     IRIS, connues aux trois millésimes), sinon le reste du canton-ou-ville
#     (ses autres communes) ;
#   - canton-ou-ville ;
#   - aire d'attraction des villes (AAV 2020) ; hors attraction des villes, le
#     canton reste une unité. Les départs du centre vers la périphérie restent
#     à l'intérieur d'une aire : un effet qui tient à cette échelle n'est pas
#     qu'un tri résidentiel. Un effet qui grandit quand l'unité rétrécit en est
#     un.
#  Modèle SLX (décalage spatial des explicatives) : l'évolution depuis 2014
#  dépend de la tension de l'unité et de celle des unités voisines, en
#  couronnes autour de leur centre (chef-lieu de la commune, ou centre de
#  population du reste du canton) : 0-20 km et 20-50 km (0-50 km pour les
#  aires), moyennes pondérées par la population légale. Effet « uniforme » :
#  somme des coefficients, l'effet d'une tension plus forte partout.
#  Issues : femmes et hommes de 25 à 29 ans en couple ; taux de première
#  naissance des couples sans enfant (femme de 20 à 44 ans), tous puis
#  installés (même logement un an plus tôt, IRAN = 1).
#  Tension : part de la population en zone A bis, A ou B1 (zonage A/B/C 2022) ;
#  variante interne (couples sans enfant de 25-34 ans sans place, 2014) aux
#  échelles du canton et de l'aire. Effets fixes : unité + âge x millésime,
#  puis + département x millésime ; erreurs-types groupées par département.
#  Interdépendance (§5) : I de Moran et modèle de Durbin spatial (SDM) sur
#  l'évolution 2014-2023 de la part en couple des femmes de 25-29 ans, par
#  unité mixte (voisines à moins de 20 km, au moins les 4 plus proches).
#
#  ⚠️ Comparaison de territoires : la tension (zonage de 2022) est fixe ; on
#     identifie des évolutions différentes selon la tension, pas un effet
#     causal. Le SDM distingue mal une vraie diffusion d'un choc commun.
#  Entrées : Data/recensement/FD_INDCVI_*, extraits du script 48,
#            Data/zonage/ (zonage A/B/C 2022, table d'appartenance Insee 2022),
#            Admin Express COG CARTO 2025 (IGN, F_ADMIN_EXPRESS)
#  Sorties : Data/recensement/_extrait/cohabitation_mixte.rds (cache),
#            Output/tension_voisinage.csv
# ============================================================================

source("R/00_utils.R")
source("R/00_config.R")
source("R/00_norme_peuplement.R")
suppressPackageStartupMessages({library(data.table); library(fixest); library(readxl); library(sf)
                                library(spdep); library(spatialreg)})
setFixest_nthreads(1); setFixest_notes(FALSE)   # un seul fil : voir R/57_erfs_preparation.R

ANS      <- c("2014", "2019", "2023")
FICHIERS <- c("2014" = "FD_INDCVI_2014.txt", "2019" = "FD_INDCVI_2019.csv", "2023" = "FD_INDCVI_2023.csv")
F_CELL   <- file.path(DIR_RP, "_extrait", "cohabitation_mixte.rds")
ville    <- function(code) fcase(startsWith(code, "751"), "75056", startsWith(code, "6938"), "69123",
                                 startsWith(code, "132"), "13055", default = code)
harmoniser <- function(x) dplyr::recode(x, "75ZZ" = "7599", "69ZZ" = "6999")   # comme au script 67
TENDU    <- c("Abis", "A", "B1")

# ── §0 Extraction : personnes de 20 à 34 ans, par commune connue et canton ─
if (!file.exists(F_CELL)) {
  ext <- lapply(ANS, function(an) {
    t0 <- Sys.time()
    x <- fread(file.path(DIR_RP, FICHIERS[[an]]), sep = ";", colClasses = "character", showProgress = FALSE,
               select = c("CANTVILLE", "AGEREV", "SEXE", "IPONDI", "DEPT", "IRIS", "METRODOM", "MOCO"))
    x <- x[METRODOM == "M"]
    x[, `:=`(age = as.integer(AGEREV), w = as.numeric(IPONDI), cantville = harmoniser(CANTVILLE),
             commune = fifelse(substr(IRIS, 1, 1) == "Z", NA_character_, ville(substr(IRIS, 1, 5))))]
    pop <- x[, .(pop = sum(w)), by = .(cantville, commune)][, annee := an]
    x <- x[age >= 20 & age <= 34]
    x[, cohab := fcase(MOCO %in% c("21", "22"), "En couple", MOCO %in% c("11", "12"), "Chez ses parents",
                       MOCO == "32", "Seul", default = "Autre")]
    cel <- x[, .(n = sum(w), obs = .N), by = .(sexe = SEXE, age, cohab, cantville, commune, dept = DEPT)][, annee := an]
    cat(an, ":", nrow(x), "personnes de 20 à 34 ans,", nrow(cel), "cellules ;",
        round(difftime(Sys.time(), t0, units = "secs")), "s\n")
    rm(x); gc(verbose = FALSE)
    list(cel = cel, pop = pop)
  })
  saveRDS(list(cel = rbindlist(lapply(ext, `[[`, "cel")), pop = rbindlist(lapply(ext, `[[`, "pop"))), F_CELL)
}
E <- readRDS(F_CELL)

# ── §1 Géographie : unités mixtes, cantons, aires ──────────────────────────
#  Communes connues aux trois millésimes : unités propres ; les autres forment le reste de leur canton.
K <- as_tibble(E$pop) %>% filter(!is.na(commune)) %>% distinct(annee, commune) %>% count(commune) %>%
  filter(n == length(ANS)) %>% pull(commune)
unite_de <- function(commune, cantville) if_else(!is.na(commune) & commune %in% K, paste0("C", commune), paste0("R", cantville))
cat("Communes connues aux trois millésimes :", length(K), "\n")

pts <- st_read(F_ADMIN_EXPRESS, layer = "chef_lieu_de_commune", quiet = TRUE)
xy  <- st_coordinates(pts) / 1000                                    # Lambert-93, en km
com <- read_excel(file.path("Data", "zonage", "table-appartenance-geo-communes-2022.xlsx"), sheet = "COM", skip = 5) %>%
  filter(substr(CODGEO, 1, 2) != "97") %>% transmute(code = CODGEO, cantville = CANOV, dep = DEP, aav = AAV2020) %>%
  left_join(tibble(code = pts$code_insee_de_la_commune, x = xy[, 1], y = xy[, 2]), by = "code") %>%
  left_join(st_read(F_ADMIN_EXPRESS, query = "SELECT code_insee, population FROM commune", quiet = TRUE) %>%
              st_drop_geometry() %>% transmute(code = code_insee, pop_ae = population), by = "code") %>%
  left_join(read.csv(file.path("Data", "zonage", "zonage-abc-communes-2022.csv"), colClasses = "character",
                     encoding = "UTF-8") %>% transmute(code = id, tendu = 100 * (zonage %in% TENDU)), by = "code") %>%
  mutate(unite = if_else(code %in% K, paste0("C", code), paste0("R", cantville)))
cat("Communes de la table 2022 sans chef-lieu ou population en 2025 (fusionnées depuis) :",
    sum(is.na(com$x) | is.na(com$pop_ae)), "sur", nrow(com), "\n")
com <- com %>% filter(!is.na(x), !is.na(pop_ae), pop_ae > 0)
majo <- function(v, w) names(which.max(tapply(w, v, sum)))
agreger <- function(d, cle) d %>% group_by(unite = .data[[cle]]) %>%
  summarise(x = weighted.mean(x, pop_ae), y = weighted.mean(y, pop_ae), tendu = weighted.mean(tendu, pop_ae, na.rm = TRUE),
            dep = majo(dep, pop_ae), cantville = majo(cantville, pop_ae), aav = majo(aav, pop_ae),
            pop_ae = sum(pop_ae), .groups = "drop")
u_mixte  <- agreger(com, "unite") %>%
  mutate(aire = if_else(aav == "000", paste0("H", cantville), paste0("A", aav)))   # hors attraction : le canton
u_canton <- agreger(com %>% mutate(k = paste0("K", cantville)), "k")
u_aire   <- u_mixte %>% group_by(unite = aire) %>%
  summarise(x = weighted.mean(x, pop_ae), y = weighted.mean(y, pop_ae), tendu = weighted.mean(tendu, pop_ae, na.rm = TRUE),
            dep = majo(dep, pop_ae), pop_ae = sum(pop_ae), .groups = "drop")

#  Couronnes : moyenne de la tension des autres unités dont le centre est à moins de b km, pondérée par la population.
couronnes <- function(u, bornes, var = "tendu") {
  D <- sqrt(outer(u$x, u$x, "-")^2 + outer(u$y, u$y, "-")^2); diag(D) <- Inf
  t <- u[[var]]; ok <- !is.na(t); lo <- c(0, head(bornes, -1))
  for (k in seq_along(bornes)) {
    M <- (D >= lo[k] & D < bornes[k]) * matrix(u$pop_ae * ok, nrow(u), nrow(u), byrow = TRUE)
    s <- rowSums(M)
    u[[paste0(var, "_r", k)]] <- if_else(s > 0, as.vector(M %*% coalesce(t, 0)) / s, 0)   # couronne vide : 0
    cat(sprintf("  %s, couronne %g-%g km : %d unités sans voisine\n", var, lo[k], bornes[k], sum(s == 0)))
  }
  u
}
cat("\nCouronnes :\n")
u_mixte <- couronnes(u_mixte, c(20, 50)); u_canton <- couronnes(u_canton, c(20, 50)); u_aire <- couronnes(u_aire, 50)

#  Tension interne (2014), aux échelles du canton et de l'aire
cp14 <- readRDS(file.path(DIR_RP, "_extrait", "couples_RP2014.rds")) %>%
  filter(metrodom == "M", !is.na(pieces), nb_enf_avant == 0, age_femme >= 25, age_femme <= 34) %>%
  mutate(cantville = harmoniser(sub(" .*", "", hh)), unite = unite_de(commune, cantville)) %>%
  left_join(u_mixte %>% select(unite, aire), by = "unite")
interne <- function(cle) cp14 %>% group_by(u = .data[[cle]]) %>%
  summarise(interne = 100 * sum(poids[place != PLACE[1]]) / sum(poids), .groups = "drop")
u_canton <- u_canton %>% left_join(interne("cantville") %>% mutate(u = paste0("K", u)), by = c("unite" = "u")) %>%
  couronnes(c(20, 50), "interne")
u_aire <- u_aire %>% left_join(interne("aire"), by = c("unite" = "u")) %>% couronnes(50, "interne")
for (u in list(u_mixte, u_canton, u_aire)) cat("Unités :", nrow(u), "| corrélation tension / couronne 1 :",
                                               round(cor(u$tendu, u$tendu_r1, use = "complete.obs"), 2), "\n")

# ── §2 Données : personnes de 25 à 29 ans, couples sans enfant ─────────────
attacher <- function(d) d %>%
  mutate(unite = unite_de(commune, cantville), canton = paste0("K", cantville)) %>%
  left_join(u_mixte %>% select(unite, aire, m_t = tendu, m_r1 = tendu_r1, m_r2 = tendu_r2), by = "unite") %>%
  left_join(u_canton %>% select(canton = unite, c_t = tendu, c_r1 = tendu_r1, c_r2 = tendu_r2,
                                c_i = interne, c_i1 = interne_r1, c_i2 = interne_r2), by = "canton") %>%
  left_join(u_aire %>% select(aire = unite, a_t = tendu, a_r1 = tendu_r1, a_i = interne, a_i1 = interne_r1), by = "aire") %>%
  mutate(across(c(m_t:m_r2, c_t:c_i2, a_t:a_i1), ~ .x / 10))        # pour 10 points de tension
pers <- as_tibble(E$cel) %>% filter(age %in% 25:29) %>%
  mutate(sexe = if_else(sexe == "2", "Femmes", "Hommes"), couple = cohab == "En couple", seul = cohab == "Seul",
         parents = cohab == "Chez ses parents") %>% attacher()
naiss <- map_dfr(ANS, ~ readRDS(file.path(DIR_RP, "_extrait", paste0("couples_RP", .x, ".rds")))) %>%
  filter(metrodom == "M", nb_enf_avant == 0, age_femme >= 20, age_femme <= 44) %>%
  transmute(annee, n = poids, naiss01, age = age_femme, installe = iran == "1", commune, dept,
            cantville = harmoniser(sub(" .*", "", hh))) %>% attacher()
for (nm in c("pers", "naiss")) {
  d <- get(nm)
  cat(sprintf("%s : poids sans unité mixte %.2f %%, sans canton %.2f %%, sans aire %.2f %%\n", nm,
              100 * sum(d$n[is.na(d$m_t)]) / sum(d$n), 100 * sum(d$n[is.na(d$c_t)]) / sum(d$n), 100 * sum(d$n[is.na(d$a_t)]) / sum(d$n)))
}

# ── §3 Descriptif : unités peu tendues, près ou loin d'une zone tendue ─────
classe <- function(t, r) case_when(t >= 5 ~ "Unite tendue", r >= 5 ~ "Peu tendue, a moins de 20 km d'une zone tendue",
                                   TRUE ~ "Peu tendue, loin")
desc <- bind_rows(
  pers %>% filter(sexe == "Femmes", !is.na(m_t)) %>% mutate(cl = classe(m_t, m_r1)) %>% group_by(cl, annee) %>%
    summarise(valeur = 100 * sum(n[couple]) / sum(n), .groups = "drop") %>% mutate(indicateur = "Femmes 25-29 en couple (%)"),
  naiss %>% filter(!is.na(m_t)) %>% mutate(cl = classe(m_t, m_r1)) %>% group_by(cl, annee) %>%
    summarise(valeur = 100 * sum(n * naiss01) / sum(n), .groups = "drop") %>%
    mutate(indicateur = "Premiere naissance, couples sans enfant 20-44 (%)"))
cat("\nSelon la tension de l'unité mixte et celle de sa couronne de 20 km (5 = moitié de la population en zone tendue) :\n")
print(as.data.frame(desc %>% mutate(valeur = round(valeur, 2)) %>% pivot_wider(names_from = annee, values_from = valeur)), row.names = FALSE)

# ── §4 Modèles SLX ─────────────────────────────────────────────────────────
ECHELLES <- list(
  "Mixte" = list(u = "unite", t = c("m_t", "m_r1", "m_r2")),
  "Canton-ou-ville" = list(u = "canton", t = c("c_t", "c_r1", "c_r2")),
  "Aire d'attraction" = list(u = "aire", t = c("a_t", "a_r1")),
  "Canton, tension interne" = list(u = "canton", t = c("c_i", "c_i1", "c_i2")),
  "Aire, tension interne" = list(u = "aire", t = c("a_i", "a_i1")))
SPECS <- c("Unite + age x millesime" = "", "+ departement x millesime" = " + dept^annee")
ISSUES <- list("Femmes 25-29 en couple" = list(d = "pers", f = quote(sexe == "Femmes"), y = "couple"),
               "Hommes 25-29 en couple" = list(d = "pers", f = quote(sexe == "Hommes"), y = "couple"),
               "Femmes 25-29 seules" = list(d = "pers", f = quote(sexe == "Femmes"), y = "seul"),
               "Premiere naissance (couples sans enfant)" = list(d = "naiss", f = quote(TRUE), y = "naiss01"),
               "Premiere naissance, couples installes" = list(d = "naiss", f = quote(installe), y = "naiss01"))
NOMS <- c("Unite", "Couronne 0-20 km", "Couronne 20-50 km")
slx <- function(x, y, ech, fe) {
  rhs <- paste(sprintf("i(annee, %s, ref = '2014')", ech$t), collapse = " + ")
  m <- feols(as.formula(sprintf("%s ~ %s | %s + age^annee%s", y, rhs, ech$u, fe)), data = x, weights = ~n, cluster = ~dept)
  b <- coef(m); V <- vcov(m)
  noms <- if (length(ech$t) == 2) c("Unite", "Couronne 0-50 km") else NOMS
  res <- map_dfr(c("2019", "2023"), function(a) {
    k <- paste0("annee::", a, ":", ech$t)
    bind_rows(tibble(terme = noms, effet_pts = 100 * b[k], se = 100 * sqrt(diag(V)[k])),
              tibble(terme = "Effet uniforme (somme)", effet_pts = 100 * sum(b[k]), se = 100 * sqrt(sum(V[k, k])))) %>%
      mutate(annee = a, .before = 1)
  }) %>% mutate(p = 2 * pnorm(-abs(effet_pts / se)))
  # Part du recul 2014-2023 associée à la tension (unité et couronnes), contre un monde sans tension
  x23 <- filter(x, annee == "2023"); k <- paste0("annee::2023:", ech$t)
  pred <- 100 * sum(b[k] * map_dbl(ech$t, ~ weighted.mean(x23[[.x]], x23$n)))
  moy <- x %>% group_by(annee) %>% summarise(v = 100 * sum(n * .data[[y]]) / sum(n), .groups = "drop")
  recul <- moy$v[moy$annee == "2023"] - moy$v[moy$annee == "2014"]
  res %>% mutate(unites = n_distinct(x[[ech$u]]), tension_pts = pred, recul_pts = recul,
                 part_du_recul_pct = 100 * pred / recul)
}
modeles <- imap_dfr(ISSUES, function(is, il) {
  base <- get(is$d) %>% filter(!!is$f)
  imap_dfr(ECHELLES, function(ech, el) imap_dfr(SPECS, function(fe, sl) {
    x <- base %>% filter(if_all(all_of(ech$t), ~ !is.na(.x)))
    slx(x, is$y, ech, fe) %>% mutate(issue = il, echelle = el, specification = sl, .before = 1)
  }))
})
cat("\nModèles SLX, 10 points de tension, évolution depuis 2014 (points) :\n")
print(as.data.frame(modeles %>% filter(annee == "2023") %>%
                      select(issue, echelle, specification, terme, effet_pts, se, p, part_du_recul_pct) %>%
                      mutate(across(where(is.double), ~ round(.x, 3)))), row.names = FALSE)

# ── §5 Interdépendance : Moran et Durbin spatial, unités mixtes ────────────
#  Évolution 2014-2023 de la part en couple des femmes de 25 à 29 ans, unités observées sur 30 femmes au moins.
dlt <- pers %>% filter(sexe == "Femmes", annee %in% c("2014", "2023"), !is.na(m_t)) %>% group_by(unite, annee) %>%
  summarise(v = 100 * sum(n[couple]) / sum(n), obs = sum(obs), .groups = "drop") %>%
  pivot_wider(names_from = annee, values_from = c(v, obs)) %>% filter(obs_2014 >= 30, obs_2023 >= 30) %>%
  transmute(unite, delta = v_2023 - v_2014, w = (obs_2014 + obs_2023) / 2) %>%
  inner_join(u_mixte %>% transmute(unite, x, y, dep, t = tendu / 10), by = "unite")
xy_u <- as.matrix(dlt[, c("x", "y")])
nb <- union.nb(dnearneigh(xy_u, 0, 20), make.sym.nb(knn2nb(knearneigh(xy_u, k = 4))))
lw <- nb2listw(nb, style = "W")
mo_brut <- moran.test(dlt$delta, lw)
#  Non pondéré (lagsarlm n'accepte pas de poids) : les unités de moins de 30 femmes observées sont écartées.
ols <- lm(delta ~ t + factor(dep), data = dlt)
mo_res <- lm.morantest(ols, lw)
sdm <- lagsarlm(delta ~ t + factor(dep), data = dlt, listw = lw, Durbin = ~ t, method = "eigen", quiet = TRUE)
imp <- summary(impacts(sdm, listw = lw, R = 500), zstats = TRUE, short = TRUE)
inter <- bind_rows(
  tibble(mesure = c("I de Moran, evolution brute", "I de Moran, residus (tension + departement)"),
         valeur = c(mo_brut$estimate[1], mo_res$estimate[1]), se = NA, p = c(mo_brut$p.value, mo_res$p.value)),
  tibble(mesure = "rho (SDM)", valeur = sdm$rho, se = sdm$rho.se, p = 2 * pnorm(-abs(sdm$rho / sdm$rho.se))),
  tibble(mesure = paste("Impact", c("direct", "indirect", "total"), "(SDM, points pour 10 points de tension)"),
         # impacts() renvoie des vecteurs sans noms, dans l'ordre des explicatives : la tension est la première
         valeur = c(imp$res$direct[1], imp$res$indirect[1], imp$res$total[1]),
         se = imp$semat["t dy/dx", c("Direct", "Indirect", "Total")], p = imp$pzmat["t dy/dx", c("Direct", "Indirect", "Total")]),
  tibble(mesure = "Effet de la tension (MCO, sans voisinage)", valeur = coef(ols)["t"],
         se = summary(ols)$coefficients["t", 2], p = summary(ols)$coefficients["t", 4])) %>%
  mutate(unites = nrow(dlt))
cat("\nInterdépendance, évolution de la part en couple des femmes de 25-29 ans (", nrow(dlt), "unités mixtes) :\n")
print(as.data.frame(inter %>% mutate(across(where(is.double), ~ signif(.x, 3)))), row.names = FALSE)

# ── Sortie ─────────────────────────────────────────────────────────────────
ecrire(bind_rows(desc %>% mutate(bloc = "Descriptif, unites mixtes", .before = 1),
                 modeles %>% mutate(bloc = "Modeles SLX", .before = 1),
                 inter %>% mutate(bloc = "Interdependance", .before = 1)),
       "Output/tension_voisinage.csv")
