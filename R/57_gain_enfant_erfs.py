"""ERFS — 57. Gain d'un enfant supplémentaire pour les couples de l'ERFS (OpenFisca local).

Appelé par R/57_erfs_politique_familiale.R. Même calcul que le script 50 pour
k = 0 (écart entre la situation avec et sans nouveau-né, même législation ;
allocations familiales, complément familial, allocation de base et prime de
naissance de la PAJE, impôt sur le revenu), avec en plus les MONTANTS de la
situation sans nouveau-né, qui servent à confronter la simulation aux
prestations et à l'impôt observés dans l'ERFS.

Entrée : cle, legis, rev1, rev2, conj, ages (âges des enfants déjà nés,
séparés par « ; »). rev1 >= rev2 : revenus imposables des deux conjoints.
Mariés ou pacsés : une déclaration commune ; sinon deux foyers, les enfants à
la charge de celui qui gagne le plus.

Usage : python 57_gain_enfant_erfs.py entree.csv sortie.csv [processus]
Les micro-données restent sur ce poste : OpenFisca tourne en local.
"""
import csv
import sys
from collections import defaultdict
from multiprocessing import Pool

FLUX = ["af", "cf", "paje_base"]
COMPOSANTES = FLUX + ["paje_naissance", "ir"]
LOT = 4000
TBS = None


def init():
    global TBS
    from openfisca_france import FranceTaxBenefitSystem
    TBS = FranceTaxBenefitSystem()


def entites(lignes, annee, avec_bebe):
    ans = [str(annee - j) for j in range(4)]
    individus, familles, foyers, menages = {}, {}, {}, {}
    for r in lignes:
        c, conj = r["cle"], r["conj"]
        parents = [f"{c}_p1", f"{c}_p2"]
        for p, rev in zip(parents, (float(r["rev1"]), float(r["rev2"]))):
            individus[p] = {"date_naissance": {"ETERNITY": "1985-01-01"},
                            "salaire_imposable": {a: rev for a in ans},
                            "statut_marital": {str(annee - 1): conj, str(annee): conj}}
        enfants = []
        for j, age in enumerate(int(a) for a in r["ages"].split(";") if a != ""):
            nom = f"{c}_e{j}"
            individus[nom] = {"date_naissance": {"ETERNITY": f"{annee - age}-01-01"}}
            enfants.append(nom)
        if avec_bebe:
            individus[f"{c}_bebe"] = {"date_naissance": {"ETERNITY": f"{annee}-01-01"}}
            enfants.append(f"{c}_bebe")
        familles[c] = {"parents": parents, "enfants": enfants}
        if conj in ("marie", "pacse"):
            foyers[f"{c}#A"] = {"declarants": parents, "personnes_a_charge": enfants}
        else:
            foyers[f"{c}#A"] = {"declarants": [parents[0]], "personnes_a_charge": enfants}
            foyers[f"{c}#B"] = {"declarants": [parents[1]]}
        menages[c] = {"personne_de_reference": [parents[0]], "conjoint": [parents[1]],
                      "enfants": enfants}
    return {"individus": individus, "familles": familles,
            "foyers_fiscaux": foyers, "menages": menages}


def montants(lignes, annee, avec_bebe):
    from openfisca_core.simulation_builder import SimulationBuilder
    sim = SimulationBuilder().build_from_entities(TBS, entites(lignes, annee, avec_bebe))
    an, avant = str(annee), str(annee - 1)
    res = {v: sim.calculate_add(v, an) for v in FLUX}     # familles, dans l'ordre des lignes
    res["paje_naissance"] = (sim.calculate_add("paje_naissance", avant)
                             + sim.calculate_add("paje_naissance", an))
    impot = sim.calculate("impot_revenu_restant_a_payer", an)
    par_couple = defaultdict(float)
    for fid, v in zip(sim.populations["foyer_fiscal"].ids, impot):
        par_couple[fid.split("#")[0]] += float(v)
    res["ir"] = [par_couple[r["cle"]] for r in lignes]
    return res


def lot(args):
    annee, lignes = args
    sans = montants(lignes, annee, False)
    avec = montants(lignes, annee, True)
    sortie = []
    for i, r in enumerate(lignes):
        niveaux = [float(sans[v][i]) for v in COMPOSANTES]
        ecarts = [float(avec[v][i] - sans[v][i]) for v in COMPOSANTES]
        sortie.append([r["cle"], annee] + [round(e, 2) for e in niveaux + ecarts] + [round(sum(ecarts), 2)])
    return sortie


def main(entree, sortie, processus=8):
    groupes = defaultdict(list)
    with open(entree, encoding="utf-8") as f:
        for r in csv.DictReader(f):
            groupes[int(r["legis"])].append(r)
    taches = [(an, g[i:i + LOT]) for an, g in sorted(groupes.items()) for i in range(0, len(g), LOT)]
    print(f"{sum(len(t[1]) for t in taches)} situations, {len(taches)} lots, {processus} processus",
          flush=True)
    with Pool(processus, initializer=init) as pool, open(sortie, "w", encoding="utf-8", newline="") as f:
        w = csv.writer(f)
        w.writerow(["cle", "legis"] + [f"{v}_sans" for v in COMPOSANTES] + COMPOSANTES + ["gain"])
        for n, res in enumerate(pool.imap_unordered(lot, taches), 1):
            w.writerows(res)
            f.flush()
            if n % 10 == 0 or n == len(taches):
                print(f"  {n}/{len(taches)} lots", flush=True)


if __name__ == "__main__":
    main(sys.argv[1], sys.argv[2], int(sys.argv[3]) if len(sys.argv) > 3 else 8)
