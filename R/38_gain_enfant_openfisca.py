"""SRCV — 38. Gain d'un enfant supplémentaire selon le barème (OpenFisca local).

Appelé par R/38_politique_familiale.R, qui prépare l'entrée et lit la sortie.
Pour chaque couple, on simule deux situations la même année de législation :
sans puis avec un nouveau-né (né le 1er janvier), et on retient l'écart.

  - prestations : allocations familiales (af), complément familial (cf) et
    allocation de base de la PAJE (paje_base), cumulées sur l'année ;
  - prime à la naissance (paje_naissance), versée deux mois avant la
    naissance dans OpenFisca, donc cumulée sur l'année précédente et l'année ;
  - impôt sur le revenu (impot_revenu_restant_a_payer, négatif quand on
    paie) : l'écart est le gain de quotient familial.

Hypothèses : couple marié (imposition commune), revenu imposable identique
sur les quatre années que lisent les bases ressources (N-3 à N), réparti
entre les deux parents selon `part1`. Prestations soumises à un choix
(congé parental, complément de mode de garde) et services exclus.

Usage : python 38_gain_enfant_openfisca.py entree.csv sortie.csv
Les micro-données restent sur ce poste : OpenFisca tourne en local.
"""
import csv
import sys
from collections import defaultdict

from openfisca_core.simulation_builder import SimulationBuilder
from openfisca_france import FranceTaxBenefitSystem

TBS = FranceTaxBenefitSystem()
FLUX = ["af", "cf", "paje_base"]
LOT = 5000


def entites(lignes, annee, avec_bebe):
    """Une famille, un foyer fiscal et un ménage par couple."""
    ans = [str(annee - k) for k in range(4)]
    individus, familles, foyers, menages = {}, {}, {}, {}
    for r in lignes:
        c = r["cle"]
        revenu, part1 = float(r["revenu"]), float(r["part1"])
        for p, part in (("p1", part1), ("p2", 1 - part1)):
            individus[f"{c}_{p}"] = {
                "date_naissance": {"ETERNITY": "1985-01-01"},
                "salaire_imposable": {a: revenu * part for a in ans},
                "statut_marital": {str(annee - 1): "marie", str(annee): "marie"},
            }
        enfants = []
        ages = [int(a) for a in r["ages"].split(";") if a != ""]
        for k, age in enumerate(ages):
            nom = f"{c}_e{k}"
            individus[nom] = {"date_naissance": {"ETERNITY": f"{annee - age}-01-01"}}
            enfants.append(nom)
        if avec_bebe:
            nom = f"{c}_bebe"
            individus[nom] = {"date_naissance": {"ETERNITY": f"{annee}-01-01"}}
            enfants.append(nom)
        parents = [f"{c}_p1", f"{c}_p2"]
        familles[c] = {"parents": parents, "enfants": enfants}
        foyers[c] = {"declarants": parents, "personnes_a_charge": enfants}
        menages[c] = {"personne_de_reference": [parents[0]], "conjoint": [parents[1]],
                      "enfants": enfants}
    return {"individus": individus, "familles": familles,
            "foyers_fiscaux": foyers, "menages": menages}


def montants(lignes, annee, avec_bebe):
    sim = SimulationBuilder().build_from_entities(TBS, entites(lignes, annee, avec_bebe))
    an, avant = str(annee), str(annee - 1)
    res = {v: sim.calculate_add(v, an) for v in FLUX}
    res["paje_naissance"] = (sim.calculate_add("paje_naissance", avant)
                             + sim.calculate_add("paje_naissance", an))
    res["ir"] = sim.calculate("impot_revenu_restant_a_payer", an)
    return res


def main(entree, sortie):
    groupes = defaultdict(list)
    with open(entree, encoding="utf-8") as f:
        for r in csv.DictReader(f):
            groupes[int(r["legis"])].append(r)
    with open(sortie, "w", encoding="utf-8", newline="") as f:
        w = csv.writer(f)
        composantes = FLUX + ["paje_naissance", "ir"]
        w.writerow(["cle", "legis"] + composantes + ["gain"])
        for annee in sorted(groupes):
            tous = groupes[annee]
            # Par lots, pour borner la mémoire d'une simulation
            for debut in range(0, len(tous), LOT):
                lignes = tous[debut:debut + LOT]
                sans = montants(lignes, annee, False)
                avec = montants(lignes, annee, True)
                for i, r in enumerate(lignes):
                    ecarts = [float(avec[v][i] - sans[v][i]) for v in composantes]
                    w.writerow([r["cle"], annee] + [round(e, 2) for e in ecarts]
                               + [round(sum(ecarts), 2)])
            print(f"Législation {annee} : {len(tous)} couples simulés", flush=True)


if __name__ == "__main__":
    main(sys.argv[1], sys.argv[2])
