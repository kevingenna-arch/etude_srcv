"""SRCV — 50. Gain d'un enfant supplémentaire, mesure affinée (OpenFisca local).

Appelé par R/50_politique_familiale_precision.R. Même principe que le script
38 (écart entre la situation avec et sans nouveau-né, même législation), avec
trois différences :

  - revenu imposable de chaque parent (`rev1` >= `rev2`), réparti selon leurs
    revenus d'activité observés, au lieu d'un partage forfaitaire ;
  - statut conjugal réel (`conj`) : marié ou pacsé, une déclaration commune ;
    sinon deux foyers fiscaux, les enfants à la charge du parent qui gagne le
    plus (celui pour qui le quotient familial rapporte le plus) ;
  - `decalage` k = 0, 1 ou 2 : année k après la naissance, au même barème
    (le nouveau-né a k ans, ses aînés k ans de plus). La prime de naissance
    n'est comptée que pour k = 0.

Les lots sont simulés en parallèle (processus distincts, un système
socio-fiscal chargé par processus).

Usage : python 50_gain_enfant_openfisca.py entree.csv sortie.csv [processus]
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


def entites(lignes, annee, k, avec_bebe):
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
            individus[nom] = {"date_naissance": {"ETERNITY": f"{annee - age - k}-01-01"}}
            enfants.append(nom)
        if avec_bebe:
            individus[f"{c}_bebe"] = {"date_naissance": {"ETERNITY": f"{annee - k}-01-01"}}
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


def montants(lignes, annee, k, avec_bebe):
    from openfisca_core.simulation_builder import SimulationBuilder
    sim = SimulationBuilder().build_from_entities(TBS, entites(lignes, annee, k, avec_bebe))
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
    annee, k, lignes = args
    sans = montants(lignes, annee, k, False)
    avec = montants(lignes, annee, k, True)
    sortie = []
    for i, r in enumerate(lignes):
        ecarts = [float(avec[v][i] - sans[v][i]) for v in COMPOSANTES]
        if k > 0:
            ecarts[COMPOSANTES.index("paje_naissance")] = 0.0
        sortie.append([r["cle"], annee, k] + [round(e, 2) for e in ecarts] + [round(sum(ecarts), 2)])
    return sortie


def main(entree, sortie, processus=8):
    groupes = defaultdict(list)
    with open(entree, encoding="utf-8") as f:
        for r in csv.DictReader(f):
            groupes[(int(r["legis"]), int(r["decalage"]))].append(r)
    taches = [(an, k, g[i:i + LOT]) for (an, k), g in sorted(groupes.items())
              for i in range(0, len(g), LOT)]
    print(f"{sum(len(t[2]) for t in taches)} situations, {len(taches)} lots, {processus} processus",
          flush=True)
    with Pool(processus, initializer=init) as pool, open(sortie, "w", encoding="utf-8", newline="") as f:
        w = csv.writer(f)
        w.writerow(["cle", "legis", "decalage"] + COMPOSANTES + ["gain"])
        for n, res in enumerate(pool.imap_unordered(lot, taches), 1):
            w.writerows(res)
            if n % 10 == 0 or n == len(taches):
                print(f"  {n}/{len(taches)} lots", flush=True)


if __name__ == "__main__":
    main(sys.argv[1], sys.argv[2], int(sys.argv[3]) if len(sys.argv) > 3 else 8)
