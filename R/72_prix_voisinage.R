# ============================================================================
#  72. Prix des logements, vie en couple et premières naissances : modèle
#      spatial avec une tension qui varie dans le temps (DVF+, recensements)
# ----------------------------------------------------------------------------
#  Le script 70 croise une tension FIXE (zonage A/B/C de 2022) avec le
#  millésime. Ici la tension varie : indice de prix au m² des logements
#  anciens par unité et par période, à type de bien et nombre de pièces donnés
#  (régression hédonique : effets fixes unité x période et type x pièces),
#  autour de chaque recensement : 2014-2016 (RP 2014), 2018-2020 (RP 2019),
#  2022-2024 (RP 2023). Indice non calculé sous 30 ventes dans la période.
#  Modèle SLX en niveaux, avec effets fixes d'unité : seule compte l'évolution
#  RELATIVE des prix de l'unité et de ses voisines (couronnes 0-20 et 20-50 km,
#  0-50 km pour les aires ; voisines dont l'indice existe aux trois périodes,
#  pondérées par la population légale) ; la hausse nationale est absorbée par
#  les effets fixes de millésime. Mêmes issues, échelles, effets fixes et
#  erreurs-types qu'au script 70, dont on reprend les §0 à §2.
#  Ordre de grandeur : effet de prix x hausse nationale des prix RÉELS entre
#  2014-2016 et 2022-2024 (indice hédonique national à commune donnée, déflaté
#  par l'IPC) — extrapolation : elle suppose qu'une hausse générale agit comme
#  une hausse relative.
#  ⚠️ Les prix montent là où l'on veut habiter : l'arrivée de jeunes seuls fait
#     monter les prix ET baisser la part en couple (causalité inverse, tri).
#  Entrées : script 70 (§0-§2), Data/dvf/ventes_logements_2014_2024.rds (script 71)
#  Sortie  : Output/prix_voisinage.csv
# ============================================================================

lignes <- readLines("R/70_tension_voisinage.R", encoding = "UTF-8")
eval(parse(text = lignes[1:(grep("^# ── §3", lignes)[1] - 1)], encoding = "UTF-8"))

# ── §1 Ventes rattachées aux unités, indices hédoniques ────────────────────
v <- readRDS(file.path("Data", "dvf", "ventes_logements_2014_2024.rds"))
v[, annee := fcase(annee %in% 2014:2016, "2014", annee %in% 2018:2020, "2019", annee %in% 2022:2024, "2023")]
v <- v[!is.na(annee)]
tab <- read_excel(file.path("Data", "zonage", "table-appartenance-geo-communes-2022.xlsx"), sheet = "COM", skip = 5) %>%
  transmute(code22 = CODGEO, cv = CANOV)
v <- merge(v, as.data.table(tab), by = "code22")
v[, `:=`(unite = fifelse(code22 %in% K, paste0("C", code22), paste0("R", cv)), canton = paste0("K", cv))]
v <- merge(v, as.data.table(u_mixte %>% select(unite, aire)), by = "unite", all.x = TRUE)
cat("Ventes 2014-2016, 2018-2020, 2022-2024 rattachées :", nrow(v), "; sans aire :", v[is.na(aire), .N], "\n")

indice <- function(cle) {
  d <- v[!is.na(get(cle))]; d[, u := get(cle)]
  m <- feols(lpm2 ~ 1 | u^annee + type^pieces, data = d)
  fe <- fixef(m)[["u^annee"]]
  n <- d[, .N, by = .(u, annee)]
  tibble(cle = names(fe), lp = unname(fe)) %>%
    mutate(u = sub("_[0-9]{4}$", "", cle), annee = sub(".*_", "", cle)) %>%
    inner_join(as_tibble(n), by = c("u", "annee")) %>% filter(N >= 30) %>% select(u, annee, lp, ventes = N)
}
#  Couronnes de prix, période par période, avec les voisines dont l'indice existe aux trois périodes
couronnes_prix <- function(u, ix, bornes) {
  complet <- ix %>% count(u) %>% filter(n == 3) %>% pull(u)
  map_dfr(c("2014", "2019", "2023"), function(a) {
    w <- u %>% left_join(ix %>% filter(annee == a) %>% select(u, lp), by = c("unite" = "u"))
    D <- sqrt(outer(w$x, w$x, "-")^2 + outer(w$y, w$y, "-")^2); diag(D) <- Inf
    ok <- w$unite %in% complet & !is.na(w$lp); lo <- c(0, head(bornes, -1))
    out <- tibble(u = w$unite, annee = a)
    for (k in seq_along(bornes)) {
      M <- (D >= lo[k] & D < bornes[k]) * matrix(w$pop_ae * ok, nrow(w), nrow(w), byrow = TRUE)
      s <- rowSums(M)
      out[[paste0("lp_r", k)]] <- if_else(s > 0, as.vector(M %*% coalesce(w$lp, 0)) / s, NA_real_)
    }
    out
  })
}
DIX <- log(1.1)   # variables en « +10 % de prix »
ix <- list(mixte = indice("unite"), canton = indice("canton"), aire = indice("aire"))
prix <- list(
  mixte  = ix$mixte  %>% left_join(couronnes_prix(u_mixte, ix$mixte, c(20, 50)), by = c("u", "annee")),
  canton = ix$canton %>% left_join(couronnes_prix(u_canton, ix$canton, c(20, 50)), by = c("u", "annee")),
  aire   = ix$aire   %>% left_join(couronnes_prix(u_aire, ix$aire, 50), by = c("u", "annee"))) %>%
  map(~ .x %>% mutate(across(starts_with("lp"), ~ .x / DIX)))
for (nm in names(prix)) {
  p <- prix[[nm]] %>% select(u, annee, lp) %>% pivot_wider(names_from = annee, values_from = lp, names_prefix = "p")
  cat(sprintf("%s : %d unités avec un indice aux trois périodes ; écart-type de l'évolution relative 2014-2023 : %.1f %%\n",
              nm, sum(complete.cases(p)), 100 * sd((p$p2023 - p$p2014) * DIX, na.rm = TRUE)))
}

#  Hausse nationale des prix réels : indice hédonique à commune donnée, déflaté par l'IPC
mn <- feols(lpm2 ~ i(annee, ref = "2014") | code22 + type^pieces, data = v)
ipc <- charger_ipc(); lipc <- function(a) log(mean(ipc$ipc[ipc$annee %in% a]))
hausse <- tibble(annee = c("2019", "2023"), nominale = coef(mn)[paste0("annee::", c("2019", "2023"))],
                 inflation = c(lipc(2018:2020), lipc(2022:2024)) - lipc(2014:2016)) %>% mutate(reelle = nominale - inflation)
cat("\nHausse nationale des prix au m² depuis 2014-2016 (log x 100) :\n")
print(as.data.frame(hausse %>% mutate(across(where(is.double), ~ round(100 * .x, 1)))), row.names = FALSE)

# ── §2 Données ─────────────────────────────────────────────────────────────
joindre <- function(d) d %>%
  left_join(prix$mixte %>% select(unite = u, annee, m_p = lp, m_p1 = lp_r1, m_p2 = lp_r2), by = c("unite", "annee")) %>%
  left_join(prix$canton %>% select(canton = u, annee, c_p = lp, c_p1 = lp_r1, c_p2 = lp_r2), by = c("canton", "annee")) %>%
  left_join(prix$aire %>% select(aire = u, annee, a_p = lp, a_p1 = lp_r1), by = c("aire", "annee"))
pers <- joindre(pers); naiss <- joindre(naiss)
for (nm in c("pers", "naiss")) { d <- get(nm)
  cat(sprintf("%s : poids avec prix — mixte %.1f %%, canton %.1f %%, aire %.1f %%\n", nm,
              100 * sum(d$n[!is.na(d$m_p) & !is.na(d$m_p1)]) / sum(d$n), 100 * sum(d$n[!is.na(d$c_p) & !is.na(d$c_p1)]) / sum(d$n),
              100 * sum(d$n[!is.na(d$a_p) & !is.na(d$a_p1)]) / sum(d$n))) }

# ── §3 Modèles SLX ─────────────────────────────────────────────────────────
#  Issues, spécifications et noms des termes : comme au §4 du script 70
SPECS <- c("Unite + age x millesime" = "", "+ departement x millesime" = " + dept^annee")
ISSUES <- list("Femmes 25-29 en couple" = list(d = "pers", f = quote(sexe == "Femmes"), y = "couple"),
               "Hommes 25-29 en couple" = list(d = "pers", f = quote(sexe == "Hommes"), y = "couple"),
               "Femmes 25-29 seules" = list(d = "pers", f = quote(sexe == "Femmes"), y = "seul"),
               "Premiere naissance (couples sans enfant)" = list(d = "naiss", f = quote(TRUE), y = "naiss01"),
               "Premiere naissance, couples installes" = list(d = "naiss", f = quote(installe), y = "naiss01"))
NOMS <- c("Unite", "Couronne 0-20 km", "Couronne 20-50 km")
ECH_P <- list("Mixte" = list(u = "unite", t = c("m_p", "m_p1", "m_p2")),
              "Canton-ou-ville" = list(u = "canton", t = c("c_p", "c_p1", "c_p2")),
              "Aire d'attraction" = list(u = "aire", t = c("a_p", "a_p1")))
slx_prix <- function(x, y, ech, fe) {
  m <- feols(as.formula(sprintf("%s ~ %s | %s + age^annee%s", y, paste(ech$t, collapse = " + "), ech$u, fe)),
             data = x, weights = ~n, cluster = ~dept)
  b <- coef(m)[ech$t]; V <- vcov(m)[ech$t, ech$t]
  noms <- if (length(ech$t) == 2) c("Unite", "Couronne 0-50 km") else NOMS
  moy <- x %>% group_by(annee) %>% summarise(v = 100 * sum(n * .data[[y]]) / sum(n), .groups = "drop")
  recul <- moy$v[moy$annee == "2023"] - moy$v[moy$annee == "2014"]
  # extrapolation : effet uniforme (pour +10 %) x hausse nationale réelle 2014-2023 (en dizaines de pour cent, log)
  ext <- 100 * sum(b) * hausse$reelle[hausse$annee == "2023"] / DIX
  bind_rows(tibble(terme = noms, effet_pts = 100 * b, se = 100 * sqrt(diag(V))),
            tibble(terme = "Effet uniforme (somme)", effet_pts = 100 * sum(b), se = 100 * sqrt(sum(V)))) %>%
    mutate(p = 2 * pnorm(-abs(effet_pts / se)), observations = nobs(m), unites = n_distinct(x[[ech$u]]),
           hausse_reelle_pct = 100 * hausse$reelle[hausse$annee == "2023"], extrapolation_pts = ext, recul_pts = recul,
           part_du_recul_pct = 100 * ext / recul)
}
modeles <- imap_dfr(ISSUES, function(is, il) {
  base <- get(is$d) %>% filter(!!is$f)
  imap_dfr(ECH_P, function(ech, el) imap_dfr(SPECS, function(fe, sl) {
    x <- base %>% filter(if_all(all_of(ech$t), ~ !is.na(.x)))
    slx_prix(x, is$y, ech, fe) %>% mutate(issue = il, echelle = el, specification = sl, .before = 1)
  }))
})
cat("\nModèles SLX, +10 % de prix (relatif), en points :\n")
print(as.data.frame(modeles %>% select(issue, echelle, specification, terme, effet_pts, se, p, part_du_recul_pct) %>%
                      mutate(across(where(is.double), ~ round(.x, 3)))), row.names = FALSE)

# ── Sortie ─────────────────────────────────────────────────────────────────
ecrire(bind_rows(hausse %>% mutate(bloc = "Hausse nationale des prix", .before = 1),
                 modeles %>% mutate(bloc = "Modeles SLX, prix", .before = 1)),
       "Output/prix_voisinage.csv")
