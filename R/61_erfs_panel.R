# ============================================================================
#  ERFS — 61. Panel d'un an : couples suivis de N à N+1
# ----------------------------------------------------------------------------
#  Partagé par les scripts 61 et 63 ; suppose `df` (R/57_erfs_preparation.R).
#  L'enquête Emploi interroge chaque logement six trimestres de suite : les
#  ménages en 1re ou 2e interrogation au 4e trimestre de N le sont encore au
#  4e trimestre de N+1, en 5e ou 6e. Les identifiants changent d'un millésime
#  à l'autre : les couples sont appariés sur la région et les années et mois
#  de naissance des deux conjoints, et le rang d'interrogation doit avancer de
#  quatre.
#  Crée `appariement` (bilan par année) et `panel` : une ligne par couple
#  suivi, avec les variables de N, et de N+1 la clé (`cle_b`), la composition
#  avant la naissance (`nb_enf_b`, `benjamin_b`, `ages_b`) et la naissance :
#  `naiss` (enfant né après la semaine de référence de N) et `naiss_concue`
#  (né au moins neuf mois après, donc conçu après l'enquête de N).
# ============================================================================

CLE <- c("reg_code", "naia_femme", "naim_femme", "naia_homme", "naim_homme")

uniques <- function(d) d %>% filter(if_all(all_of(CLE), ~ !is.na(.x))) %>%
  group_by(across(all_of(CLE))) %>% filter(n() == 1) %>% ungroup()
annees <- sort(unique(df$annee))
appariement <- map_dfr(head(annees, -1), function(a) {
  A <- uniques(filter(df, annee == a, rga %in% c("1", "2")))
  B <- uniques(filter(df, annee == a + 1, rga %in% c("5", "6"))) %>%
    transmute(across(all_of(CLE)), cle_b = cle, rga_b = rga, naiss01_b = naiss01, naiss_am_b = naiss_am,
              nb_enf_b = nb_enf_avant, benjamin_b = benjamin, ages_b = ages)
  m <- inner_join(A, B, by = CLE)
  tibble(annee = a, attendus = nrow(A), apparies = nrow(m),
         rga_coherent = sum(as.integer(m$rga_b) == as.integer(m$rga) + 4),
         donnees = list(filter(m, as.integer(rga_b) == as.integer(rga) + 4)))
})
cat("\nAppariement de N à N+1 (couples en 1re ou 2e interrogation en N) :\n")
print(as.data.frame(appariement %>% select(-donnees) %>% mutate(taux_pct = round(100 * rga_coherent / attendus, 1))),
      row.names = FALSE)

panel <- bind_rows(appariement$donnees) %>%
  mutate(naiss = as.integer(naiss01_b == 1 & !is.na(naiss_am_b) & naiss_am_b > datref),
         naiss_concue = as.integer(naiss == 1 & naiss_am_b >= datref + 9))
