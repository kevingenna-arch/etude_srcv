# ============================================================================
#  71. DVF+ : ventes de logements anciens par commune, 2014-2024
# ----------------------------------------------------------------------------
#  Extrait des 97 fichiers départementaux DVF+ (Cerema) les ventes d'un seul
#  logement (une maison ou un appartement, un lot, une commune), hors ventes
#  en l'état futur d'achèvement, avec leur prix au m², leur type et leur
#  nombre de pièces, comme le script 26 mais à la commune. Sert au script 72
#  (indice de prix par territoire, confronté à la vie en couple et aux
#  naissances du recensement).
#  Communes : le code est celui de l'année de la vente. Les codes absents de la
#  table d'appartenance de l'Insee (2022) — communes fusionnées avant 2022 ou
#  créées après — sont rattachés à la commune de la table dont le chef-lieu
#  (Admin Express 2025) est le plus proche du centre de leurs parcelles.
#  Filtres : 9 à 300 m², 100 à 30 000 € le m².
#  Entrées : DIR_DVF (hors dépôt, 8,3 Go), table d'appartenance Insee 2022,
#            Admin Express COG CARTO 2025 (F_ADMIN_EXPRESS)
#  Sortie  : Data/dvf/ventes_logements_2014_2024.rds (cache, hors dépôt)
# ============================================================================

source("R/00_utils.R")
source("R/00_config.R")
suppressPackageStartupMessages({library(data.table); library(readxl); library(sf)})

F_VENTES <- file.path("Data", "dvf", "ventes_logements_2014_2024.rds")
dir.create(dirname(F_VENTES), showWarnings = FALSE)
COLS <- c("anneemut", "coddep", "libnatmut", "vefa", "valeurfonc", "nblot", "nbcomm", "l_codinsee", "codtypbien",
          paste0("nbapt", 1:5, "pp"), paste0("sapt", 1:5, "pp"), paste0("nbmai", 1:5, "pp"), paste0("smai", 1:5, "pp"),
          "geompar_x", "geompar_y")

lire_dept <- function(f) {
  d <- fread(f, sep = "|", select = COLS, colClasses = list(character = c("coddep", "l_codinsee", "codtypbien", "vefa")),
             showProgress = FALSE)
  d <- d[libnatmut == "Vente" & vefa %in% c("False", "FALSE", "f", "0") & nbcomm == 1 &
           codtypbien %in% c("111", "121") & anneemut >= 2014 & anneemut <= 2024]
  # Pas de filtre sur les lots : une maison seule n'en a en général aucun (nblot = 0), un appartement
  # vendu avec sa cave ou son parking en a deux ou plus ; le type de bien (111, 121) garantit un seul logement.
  lots <- d[, .N, by = .(codtypbien, nblot = pmin(nblot, 2))]
  apt <- d$codtypbien == "121"   # pièces : la catégorie à 1 dans nbapt*pp (appartements) ou nbmai*pp (maisons)
  pieces <- rep(NA_integer_, nrow(d)); surface <- rep(NA_real_, nrow(d))
  for (k in 1:5) {
    sel <- is.na(pieces) & ((apt & d[[paste0("nbapt", k, "pp")]] == 1) | (!apt & d[[paste0("nbmai", k, "pp")]] == 1))
    pieces[sel] <- k
    surface[sel] <- if_else(apt[sel], d[[paste0("sapt", k, "pp")]][sel], d[[paste0("smai", k, "pp")]][sel])
  }
  d[, `:=`(pieces = pieces, surface = surface)]
  d <- d[!is.na(pieces) & surface >= 9 & surface <= 300 & valeurfonc > 1000]
  d[, pm2 := valeurfonc / surface]
  list(ventes = d[pm2 >= 100 & pm2 <= 30000, .(code = gsub("[{}\"]", "", l_codinsee), annee = as.integer(anneemut),
                                               type = fifelse(codtypbien == "121", "Appartement", "Maison"), pieces,
                                               lpm2 = log(pm2), x = geompar_x, y = geompar_y)],
       lots = lots)
}

fichiers <- list.files(DIR_DVF, pattern = "^dvf_plus_d.*\\.csv$", full.names = TRUE)
cat("Fichiers départementaux DVF+ :", length(fichiers), "\n")
t0 <- Sys.time()
lus <- lapply(fichiers, function(f) { r <- lire_dept(f); cat("."); r })
v <- rbindlist(lapply(lus, `[[`, "ventes"))
cat("\nLecture :", round(as.numeric(difftime(Sys.time(), t0, units = "mins")), 1), "min ;", nrow(v), "ventes\n")
cat("Ventes d'un seul logement selon le nombre de lots (111 maison, 121 appartement ; 2 = deux ou plus) :\n")
print(rbindlist(lapply(lus, `[[`, "lots"))[, .(N = sum(N)), keyby = .(codtypbien, nblot)])
v <- v[!startsWith(code, "97")]                              # métropole
cat("Ventes par année :\n"); print(v[, .N, keyby = annee])

# ── Codes absents de la table 2022 : chef-lieu le plus proche du centre des parcelles ──
tab <- read_excel(file.path("Data", "zonage", "table-appartenance-geo-communes-2022.xlsx"), sheet = "COM", skip = 5)$CODGEO
v[, code22 := fifelse(code %in% tab, code, NA_character_)]
# Paris, Lyon, Marseille : DVF+ peut porter les arrondissements
v[is.na(code22) & startsWith(code, "751"), code22 := "75056"]
v[is.na(code22) & startsWith(code, "6938"), code22 := "69123"]
v[is.na(code22) & startsWith(code, "132"), code22 := "13055"]
manque <- v[is.na(code22) & !is.na(x) & !is.na(y), .(x = median(x), y = median(y), n = .N), by = code]
cat("Codes hors table 2022 :", nrow(manque), "(", sum(manque$n), "ventes ) ; sans coordonnées :",
    v[is.na(code22) & (is.na(x) | is.na(y)), .N], "ventes\n")
if (nrow(manque) > 0) {
  pts <- st_read(F_ADMIN_EXPRESS, layer = "chef_lieu_de_commune", quiet = TRUE)
  pts <- pts[pts$code_insee_de_la_commune %in% tab, ]
  p <- st_as_sf(manque, coords = c("x", "y"), crs = 2154)
  manque[, vers := pts$code_insee_de_la_commune[st_nearest_feature(p, st_transform(pts, 2154))]]
  v[manque, on = "code", code22 := fifelse(is.na(code22), i.vers, code22)]
}
v <- v[!is.na(code22), .(code22, annee, type, pieces, lpm2)]
cat("Ventes retenues :", nrow(v), "; communes :", uniqueN(v$code22), "\n")
saveRDS(v, F_VENTES)
cat("Écrit :", F_VENTES, "\n")
