# ============================================================================
#  SRCV — 52. La ZEAT « non renseignée » de 2014 biaise-t-elle les résultats ?
# ----------------------------------------------------------------------------
#  En 2010 et 2014, la ZEAT manque pour 21 à 23 % des couples, et surtout pour
#  ceux qui viennent d'emménager (42 % de mobiles contre 4 % ; script 51). Les
#  modèles de naissance l'utilisent en effets fixes, avec une modalité
#  « Non renseigne » : comme les naissances suivent souvent un déménagement,
#  cette modalité pourrait absorber une partie de l'association.
#  On reprend la mécanique du script 28 (mêmes échantillons, mêmes modèles,
#  mêmes chiffres que l'article) sous trois variantes :
#   - référence (doit redonner les chiffres publiés) ;
#   - sans ZEAT dans les modèles ;
#   - ZEAT de 2014 imputée par celle du même ménage en 2018 (panel 2014-2018,
#     fichier ménages complet de 2018), la modalité restant pour les autres.
#  À lancer avec SRCV_DEBUT=2014 (période de l'article).
#  Sortie : Output/robustesse_zeat.csv (suffixée selon SRCV_DEBUT)
# ============================================================================

lignes <- readLines("R/28_robustesse_naissance_fpr.R", encoding = "UTF-8")
eval(parse(text = lignes[1:(grep("^resultats <- imap_dfr", lignes) - 1)], encoding = "UTF-8"))

NR <- "Non renseigne"
REF <- VARIANTES[["Reference (ne en N ou N-1)"]]
cat("\nZEAT non renseignée par vague (champ fécond) :\n")
print(base %>% group_by(annee) %>% summarise(n = n(), non_renseignee_pct = round(100 * mean(zeat == NR), 1)))

# ── ZEAT de 2014 imputée par le panel ──────────────────────────────────────
z18 <- charger_menages("2018", VAGUES[["2018"]]) %>%
  transmute(id_n = normaliser_id(id), zeat18 = as.character(zeat)) %>%
  filter(zeat18 != NR) %>% distinct(id_n, .keep_all = TRUE)
base_imputee <- base %>% mutate(id_n = normaliser_id(id)) %>%
  left_join(z18, by = "id_n") %>%
  mutate(impute = annee == "2014" & zeat == NR & !is.na(zeat18),
         zeat = factor(if_else(impute, zeat18, as.character(zeat)), levels = levels(base$zeat))) %>%
  select(-id_n, -zeat18)
imputation <- base_imputee %>% filter(annee == "2014") %>%
  summarise(non_renseignees = sum(base$annee == "2014" & base$zeat == NR), imputees = sum(impute),
            restantes = sum(zeat == NR))
cat("\nImputation de la ZEAT 2014 par le panel 2018 :\n"); print(imputation)

# ── Les trois variantes ────────────────────────────────────────────────────
FORMULES <- c("F_BASE", "F_POOLE", "F_RANG1", "F_TR", "F_TR_1", "F_M5")
originales <- mget(FORMULES)
base_origine <- base
resultats <- bind_rows(
  estimer_variante(REF, "Reference (article)"),
  { for (f in FORMULES) assign(f, update(originales[[f]], . ~ . - zeat))
    r <- estimer_variante(REF, "Sans ZEAT")
    for (f in FORMULES) assign(f, originales[[f]])
    r },
  { base <- base_imputee
    r <- estimer_variante(REF, "ZEAT 2014 imputee par le panel 2018")
    base <- base_origine
    r })
ecrire(resultats, "Output/robustesse_zeat.csv")

# ── Comparaison avec les chiffres publiés ──────────────────────────────────
cle <- function(r) case_when(
  r$bloc == "Surface (+10 m2)" ~ paste("Surface +10 m2,", r$echantillon),
  r$bloc == "Tranches de surface (vs 70-89 m2)" & grepl("Moins de 50", r$contraste) ~
    paste("Moins de 50 m2,", r$echantillon),
  r$bloc == "Statut, M5 (vs locataire)" & grepl("^Proprietaire_accedant -", r$contraste) ~
    "Accedant vs locataire, 1er enfant",
  grepl("Decomposition", r$bloc) & r$contraste == "Surface de 2010" ~
    paste("Part de la baisse, surface", if_else(grepl("lineaire", r$bloc), "lineaire", "en tranches")),
  TRUE ~ NA_character_)
comparaison <- resultats %>% mutate(indicateur = cle(.)) %>% filter(!is.na(indicateur)) %>%
  select(variante, indicateur, valeur = ame_pts, p.value) %>%
  mutate(valeur = round(valeur, 2), p.value = round(p.value, 3))
cat("\nComparaison (article : surface 0,34 / 0,59 / installés 0,35 ; < 50 m2 installés -9,13 ;",
    "accédant 5,15 ; part 3,8 / 19,0 %) :\n")
print(as.data.frame(comparaison %>% select(-p.value) %>% pivot_wider(names_from = variante, values_from = valeur)),
      row.names = FALSE)
