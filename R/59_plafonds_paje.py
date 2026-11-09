"""ERFS — 59. Plafonds de ressources de l'allocation de base de la PAJE, 2005-2023.

Appelé par R/59_erfs_plafonds_paje.R. Lit les PARAMÈTRES d'OpenFisca (aucune
micro-donnée) au 1er juin de chaque année et écrit, pour les trois régimes :
  - avant : enfants nés avant avril 2014 (un seul plafond) ;
  - r2014 : enfants nés d'avril 2014 à mars 2018 (taux plein / taux partiel) ;
  - r2018 : enfants nés depuis avril 2018 (taux plein / taux partiel),
le plafond sans enfant, les majorations par enfant, la majoration biactifs /
parent isolé, les montants mensuels à taux plein, la prime de naissance et le
seuil de biactivité (12 BMAF de l'année N-2). Un paramètre absent à une date
(régime pas encore créé ou éteint) est laissé vide.

Usage : python 59_plafonds_paje.py sortie.csv
"""
import csv
import sys

from openfisca_france import FranceTaxBenefitSystem

ANNEES = range(2005, 2024)


def lire(p, chemin):
    try:
        for c in chemin.split("."):
            p = p[c] if c.isdigit() else getattr(p, c)
        return float(p)
    except Exception:
        return None


def main(sortie):
    tbs = FranceTaxBenefitSystem()
    lignes = []
    for an in ANNEES:
        pf = tbs.parameters(f"{an}-06-01").prestations_sociales.prestations_familiales
        bmaf_gel = tbs.parameters("2013-04-01").prestations_sociales.prestations_familiales.bmaf.bmaf
        bmaf = pf.bmaf.bmaf
        bmaf_n2 = tbs.parameters(f"{an - 2}-06-01").prestations_sociales.prestations_familiales.bmaf.bmaf
        paje = pf.petite_enfance.paje
        pl = "paje_plaf."
        r = {"annee": an, "bmaf": float(bmaf), "seuil_biactif": 12 * float(bmaf_n2)}
        r["avant_p0"] = lire(paje, pl + "ne_adopte_avant_04_2014.plafond_ressources_0_enfant")
        r["avant_maj12"] = lire(paje, pl + "ne_adopte_avant_04_2014.majorations_enfants.premier_2eme_enfant")
        r["avant_maj3"] = lire(paje, pl + "ne_adopte_avant_04_2014.majorations_enfants.troisieme_plus_enfant")
        r["avant_biact"] = lire(paje, pl + "ne_adopte_avant_04_2014.biactifs_parents_isoles")
        for t in ("taux_plein", "taux_partiel"):
            r[f"r2014_{t}_p0"] = lire(paje, pl + f"ne_adopte_04_2014_et_03_2018.{t}.plafond_ressources_0_enfant")
            r[f"r2014_{t}_biact"] = lire(paje, pl + f"ne_adopte_04_2014_et_03_2018.{t}.biactifs_parents_isoles")
            r[f"r2018_{t}_p0"] = lire(paje, pl + f"ne_adopte_apres_04_2018.{t}.plafond_ressources_0_enfant")
            r[f"r2018_{t}_biact"] = lire(paje, pl + f"ne_adopte_apres_04_2018.{t}.biactifs_parents_isoles")
        r["r2014_maj"] = lire(paje, pl + "ne_adopte_04_2014_et_03_2018.majorations_enfants.majoration_enfant_supp")
        r["r2018_maj12"] = lire(paje, pl + "ne_adopte_apres_04_2018.majorations_enfants.premier_2eme_enfant")
        r["r2018_maj3"] = lire(paje, pl + "ne_adopte_apres_04_2018.majorations_enfants.troisieme_plus_enfant")
        # Montants mensuels à taux plein : gelés d'avril 2013 à mars 2018 (comme dans paje_base)
        indice = bmaf_gel if (an, 6) > (2013, 4) and (an, 6) < (2018, 4) else bmaf
        for reg, ch in (("avant", "avant_2014"), ("r2014", "apres_2014"), ("r2018", "apres_2018")):
            taux = lire(paje, f"paje_cm.montant.allocation_base_taux_plein.{ch}.taux")
            r[f"{reg}_ab_mois"] = None if taux is None else float(indice) * taux
        prime = lire(paje, "paje_cm2.montant.prime_naissance")
        r["prime"] = None if prime is None else float(bmaf) * prime
        lignes.append(r)
    with open(sortie, "w", encoding="utf-8", newline="") as f:
        w = csv.DictWriter(f, fieldnames=list(lignes[0]))
        w.writeheader()
        w.writerows(lignes)
    print(f"{len(lignes)} années écrites dans {sortie}")


if __name__ == "__main__":
    main(sys.argv[1])
