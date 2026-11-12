"""ERFS — 64. Gain complet d'un enfant supplémentaire : avec RSA, prime d'activité et aides au logement.

Appelé par R/64_erfs_gain_complet.R. Même démarche que le script 57 (écart
entre la situation avec et sans nouveau-né, même législation), avec en plus le
RSA (socle et activité), la prime d'activité (depuis 2016) et les aides au
logement (APL, ALF, ALS), tous calculés par OpenFisca pour la famille. Le RMI
n'a pas ses paramètres dans OpenFisca-France avant juin 2009 : les barèmes
simulés commencent en 2010, première année entière du RSA. Le RSA et la prime d'activité comptent les
allocations familiales et la PAJE dans leurs ressources : ils baissent quand
celles-ci montent.

Entrée : cle, legis, rev1, rev2 (revenus imposables des deux conjoints,
rev1 >= rev2), act1, act2 (dont revenus d'activité ; le reste est traité en
allocations chômage), conj, ages (âges des enfants déjà nés, séparés par
« ; »), statut (statut d'occupation, nom OpenFisca), loyer (mensuel, euros de
l'année de législation), zone (zone_1, zone_2, zone_3).
Mariés ou pacsés : une déclaration commune ; sinon deux foyers, les enfants à
la charge de celui qui gagne le plus. Une seule famille dans tous les cas.

Usage : python 64_gain_complet_erfs.py entree.csv sortie.csv [processus]
Les micro-données restent sur ce poste : OpenFisca tourne en local.
"""
import csv
import sys
from collections import defaultdict
from multiprocessing import Pool

FLUX = ["af", "cf", "paje_base", "rsa", "ppa", "aide_logement"]
COMPOSANTES = FLUX + ["paje_naissance", "ir"]
LOT = 2000
TBS = None


def init():
    global TBS
    from openfisca_france import FranceTaxBenefitSystem
    TBS = FranceTaxBenefitSystem()


def entites(lignes, annee, avec_bebe):
    ans = [str(annee - j) for j in range(4)]
    mois = [f"{annee - j}-{m:02d}" for j in (0, 1) for m in range(1, 13)]
    individus, familles, foyers, menages = {}, {}, {}, {}
    for r in lignes:
        c, conj = r["cle"], r["conj"]
        parents = [f"{c}_p1", f"{c}_p2"]
        for p, rev, act in zip(parents, (float(r["rev1"]), float(r["rev2"])), (float(r["act1"]), float(r["act2"]))):
            act = min(max(act, 0.0), rev)
            individus[p] = {"date_naissance": {"ETERNITY": "1985-01-01"},
                            "salaire_imposable": {a: act for a in ans},
                            "chomage_imposable": {a: rev - act for a in ans},
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
        menages[c] = {"personne_de_reference": [parents[0]], "conjoint": [parents[1]], "enfants": enfants,
                      "statut_occupation_logement": {m: r["statut"] for m in mois},
                      "loyer": {m: float(r["loyer"] or 0) for m in mois},
                      "zone_apl": {m: r["zone"] for m in mois}}
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
