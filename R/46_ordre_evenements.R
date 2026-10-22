# ============================================================================
#  SRCV — 46. Qui vient en premier : le déménagement ou la naissance ?
# ----------------------------------------------------------------------------
#  Si le logement est choisi en vue d'un enfant, les couples s'agrandissent
#  AVANT la naissance ; s'il la contraint, la naissance n'arrive qu'une fois
#  la place trouvée, et les couples à l'étroit qui ont un enfant quand même
#  déménagent APRÈS. Deux mesures :
#
#  A. En coupe (toutes les vagues de la période) : pour les couples qui ont eu
#     un enfant, année d'emménagement dans le logement actuel comparée à
#     l'année de naissance ; pour les autres, année d'emménagement comparée à
#     l'année d'enquête. Rythme de déménagement l'année de la naissance, un
#     an et deux ans avant, rapporté à celui des couples sans naissance.
#  B. En panel (vagues 2022 à 2025, trois vagues successives t-1, t, t+1) :
#     - naissance entre t et t+1 après un agrandissement entre t-1 et t ;
#     - agrandissement entre t et t+1 après une naissance entre t-1 et t ;
#     comparés aux couples qui n'ont pas déménagé (resp. pas eu d'enfant).
#     Agrandissement = déménagement vers au moins 10 m² de plus ou une pièce
#     de plus. Naissance = nouvel enfant, né depuis la vague précédente.
#  Statistiques non pondérées (séquences d'événements), avec effectifs et
#  intervalles de Wilson.
#  Sortie : Output/ordre_evenements.csv
# ============================================================================

source("R/00_prepa_fecondite.R")
source("R/00_norme_peuplement.R")
activer_cache_prepa()

wilson <- function(x, n) {
  p <- x / n; z <- 1.96; d <- 1 + z^2 / n
  c <- p + z^2 / (2 * n); e <- z * sqrt(p * (1 - p) / n + z^2 / (4 * n^2))
  tibble(pct = 100 * p, ic_bas = 100 * (c - e) / d, ic_haut = 100 * (c + e) / d)
}

df <- bind_rows(millesimes_prepares(restreindre = TRUE)) %>%
  select(any_of(c(VARS_INTERET, "datent"))) %>%
  mutate(annee_num = as.integer(annee), id_n = normaliser_id(id),
         naiss01 = as.integer(naissance == "Oui"),
         rang = if_else(nb_enf_avant == 0, "Premier enfant", "Rang 2 ou plus"),
         petit = if_else(surface < 60, "Moins de 60 m2", "60 m2 ou plus")) %>%
  filter(!is.na(datent), datent <= annee_num, !is.na(surface))

# Individus : année de naissance et identifiant personnel
individus <- imap_dfr(VAGUES[unique(df$annee)], function(cfg, an) {
  i <- lire_srcv(cfg$ind); names(i) <- toupper(names(i))
  anais <- if ("ANAIS" %in% names(i)) num(i$ANAIS) else as.integer(an) - ages_individus(i, an)
  tibble(annee = an, id_n = cle_menage_ind(i), pid = normaliser_id(i$RB030), anais = anais,
         lien = trimws(as.character(i$LIENPREF)))
})

# ── A. En coupe : déménagement l'année de la naissance, un et deux ans avant
naissances <- individus %>% filter(lien %in% c("02", "2")) %>%
  group_by(annee, id_n) %>% summarise(an_naiss = max(anais), .groups = "drop")
a <- df %>% left_join(naissances, by = c("annee", "id_n")) %>%
  mutate(reference = if_else(naiss01 == 1 & !is.na(an_naiss), an_naiss, annee_num),
         ecart = datent - reference,
         moment = case_when(ecart >= 1 ~ "Apres la naissance",
                            ecart == 0 ~ "L'annee meme",
                            ecart == -1 ~ "Un an avant",
                            ecart == -2 ~ "Deux ans avant",
                            TRUE ~ "Trois ans avant ou plus"),
         groupe = if_else(naiss01 == 1, "Couples avec naissance", "Couples sans naissance"))
#  Pour les couples sans naissance, « l'année même » est l'année d'enquête : on
#  ne garde que les femmes de 25 à 39 ans, l'âge des naissances.
a <- a %>% filter(age_femme >= 25, age_femme <= 39)
coupe <- a %>% group_by(groupe, rang, moment) %>% summarise(n = n(), .groups = "drop") %>%
  group_by(groupe, rang) %>% mutate(total = sum(n)) %>% ungroup() %>%
  bind_cols(wilson(.$n, .$total)) %>%
  mutate(moment = factor(moment, c("Trois ans avant ou plus", "Deux ans avant", "Un an avant",
                                   "L'annee meme", "Apres la naissance"))) %>% arrange(groupe, rang, moment)
cat("\nA. Année d'emménagement dans le logement actuel (femmes de 25 à 39 ans) :\n")
print(as.data.frame(coupe %>% mutate(across(where(is.double), ~ round(.x, 1)))), row.names = FALSE)

# ── B. En panel : trois vagues successives des fichiers 2022-2025 ──────────
FPR <- intersect(c("2022", "2023", "2024", "2025"), unique(df$annee))
vague <- df %>% filter(annee %in% FPR) %>%
  select(annee, annee_num, id_n, surface, pieces, datent, nb_enf, rang, petit, age_femme)
ind_fpr <- individus %>% filter(annee %in% FPR)
nouveaux <- function(t0, t1) {
  avant <- ind_fpr %>% filter(annee == t0) %>% select(id_n, pid)
  ind_fpr %>% filter(annee == t1, anais >= as.integer(t0)) %>%
    anti_join(avant, by = c("id_n", "pid")) %>% distinct(id_n) %>% mutate(naiss = 1L)
}
etape <- function(t0, t1) {
  x0 <- vague %>% filter(annee == t0); x1 <- vague %>% filter(annee == t1)
  x0 %>% inner_join(x1 %>% select(id_n, surface1 = surface, pieces1 = pieces, datent1 = datent),
                    by = "id_n") %>%
    left_join(nouveaux(t0, t1), by = "id_n") %>%
    transmute(id_n, t0 = t0, naiss = coalesce(naiss, 0L),
              demenage = datent1 > datent,
              agrandi = demenage & (surface1 >= surface + 10 | pieces1 > pieces),
              rang, petit)
}
triples <- map_dfr(seq_len(length(FPR) - 2), function(k) {
  e1 <- etape(FPR[k], FPR[k + 1]); e2 <- etape(FPR[k + 1], FPR[k + 2])
  inner_join(e1 %>% select(id_n, naiss1 = naiss, agrandi1 = agrandi, demenage1 = demenage, rang, petit),
             e2 %>% select(id_n, naiss2 = naiss, agrandi2 = agrandi),
             by = "id_n") %>% mutate(triple = paste(FPR[k], FPR[k + 2], sep = "-"))
})
cat("\nB. Couples présents trois vagues de suite :", nrow(triples), "\n")
proba <- function(d, cond, y, lib) d %>% group_by(rang, condition = {{ cond }}) %>%
  summarise(x = sum({{ y }}), n = n(), .groups = "drop") %>%
  bind_cols(wilson(.$x, .$n)) %>% mutate(mesure = lib, .before = 1)
panel <- bind_rows(
  proba(triples %>% mutate(c = if_else(agrandi1, "Agrandi entre t-1 et t",
                                       if_else(demenage1, "Autre demenagement", "Pas de demenagement"))),
        c, naiss2, "Naissance entre t et t+1, selon le logement entre t-1 et t"),
  proba(triples %>% mutate(c = if_else(naiss1 == 1, "Naissance entre t-1 et t", "Pas de naissance")),
        c, agrandi2, "Agrandissement entre t et t+1, selon la naissance entre t-1 et t"),
  proba(triples %>% mutate(c = if_else(naiss1 == 1, "Naissance entre t-1 et t", "Pas de naissance")),
        c, agrandi1, "Agrandissement entre t-1 et t, selon la naissance entre t-1 et t"))
cat("\nB. Séquences dans le panel (%, non pondéré) :\n")
print(as.data.frame(panel %>% mutate(across(where(is.double), ~ round(.x, 1)))), row.names = FALSE)
cat("\nB bis. Couples logés sous 60 m2 à la première vague :\n")
panel_petits <- bind_rows(
  proba(filter(triples, petit == "Moins de 60 m2") %>%
          mutate(c = if_else(naiss1 == 1, "Naissance entre t-1 et t", "Pas de naissance")),
        c, agrandi2, "Agrandissement entre t et t+1, selon la naissance entre t-1 et t (sous 60 m2)"),
  proba(filter(triples, petit == "Moins de 60 m2") %>%
          mutate(c = if_else(agrandi1, "Agrandi entre t-1 et t", "Pas agrandi")),
        c, naiss2, "Naissance entre t et t+1, selon l'agrandissement entre t-1 et t (sous 60 m2)"))
print(as.data.frame(panel_petits %>% mutate(across(where(is.double), ~ round(.x, 1)))), row.names = FALSE)

ecrire(bind_rows(coupe %>% mutate(bloc = "A. Coupe : annee d'emmenagement", moment = as.character(moment),
                                  .before = 1),
                 panel %>% mutate(bloc = "B. Panel : sequences", .before = 1),
                 panel_petits %>% mutate(bloc = "B. Panel : sous 60 m2", .before = 1)),
       "Output/ordre_evenements.csv")
