"""ERFS — 60. Gain d'un enfant à chaque âge de son enfance, et réforme du quotient familial.

Appelé par R/60_erfs_qf_prestations.R. Pour un couple de l'ERFS, une législation
(`legis`) et un âge `k` de l'enfant à naître : écart entre la situation avec et
sans cet enfant pour les allocations familiales, le complément familial,
l'allocation de base et la prime de naissance de la PAJE (calculées seulement si
k < 3) et l'impôt sur le revenu, plus le niveau de l'impôt sans l'enfant. Les
enfants déjà nés ont vieilli de k ans (ceux qui dépassent 20 ans sortent du
foyer, comme au script 57) ; revenus et statut conjugal restent ceux de l'enquête, en euros de la
législation. Option `plafond` : plafond de l'avantage du quotient familial par
demi-part pour personne à charge (vide = la loi).

Entrée : cle, legis, rev1, rev2, conj, ages, k, plafond
Sortie : cle, legis, k, plafond, af, cf, paje_base, paje_naissance, ir, ir_sans
Usage : python 60_gain_vie_enfant.py entree.csv sortie.csv [processus]
Les micro-données restent sur ce poste : OpenFisca tourne en local.
"""
import csv
import sys
from collections import defaultdict
from multiprocessing import Pool

COMPOSANTES = ["af", "cf", "paje_base", "paje_naissance", "ir"]
LOT = 4000
TBS = {}


def init():
    from openfisca_france import FranceTaxBenefitSystem
    TBS[""] = FranceTaxBenefitSystem()


def systeme(plafond):
    """Législation, ou législation dont le plafond du quotient familial est fixé à `plafond`."""
    if plafond not in TBS:
        from openfisca_core import periods
        from openfisca_core.reforms import Reform
        valeur = float(plafond)

        class Plafond(Reform):
            def apply(self):
                def modifier(p):
                    p.impot_revenu.calcul_impot_revenu.plaf_qf.plafond_avantages_procures_par_demi_part.general.update(
                        period=periods.period("year:2000:40"), value=valeur)
                    return p
                self.modify_parameters(modifier_function=modifier)
        TBS[plafond] = Plafond(TBS[""])
    return TBS[plafond]


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
        for j, age in enumerate(int(a) + k for a in r["ages"].split(";") if a != ""):
            if age <= 20:
                nom = f"{c}_e{j}"
                individus[nom] = {"date_naissance": {"ETERNITY": f"{annee - age}-01-01"}}
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


def montants(lignes, annee, k, plafond, avec_bebe):
    from openfisca_core.simulation_builder import SimulationBuilder
    sim = SimulationBuilder().build_from_entities(systeme(plafond), entites(lignes, annee, k, avec_bebe))
    an, avant = str(annee), str(annee - 1)
    zero = [0.0] * len(lignes)
    res = {v: sim.calculate_add(v, an) for v in ("af", "cf")}
    if k < 3:
        res["paje_base"] = sim.calculate_add("paje_base", an)
        res["paje_naissance"] = (sim.calculate_add("paje_naissance", avant)
                                 + sim.calculate_add("paje_naissance", an))
    else:
        res["paje_base"], res["paje_naissance"] = zero, zero
    impot = sim.calculate("impot_revenu_restant_a_payer", an)
    par_couple = defaultdict(float)
    for fid, v in zip(sim.populations["foyer_fiscal"].ids, impot):
        par_couple[fid.split("#")[0]] += float(v)
    res["ir"] = [par_couple[r["cle"]] for r in lignes]
    return res


def lot(args):
    (annee, k, plafond), lignes = args
    sans = montants(lignes, annee, k, plafond, False)
    avec = montants(lignes, annee, k, plafond, True)
    return [[r["cle"], annee, k, plafond]
            + [round(float(avec[v][i] - sans[v][i]), 2) for v in COMPOSANTES]
            + [round(float(sans["ir"][i]), 2)] for i, r in enumerate(lignes)]


def main(entree, sortie, processus=8):
    groupes = defaultdict(list)
    with open(entree, encoding="utf-8") as f:
        for r in csv.DictReader(f):
            groupes[(int(r["legis"]), int(r["k"]), r["plafond"])].append(r)
    taches = [(cle, g[i:i + LOT]) for cle, g in sorted(groupes.items()) for i in range(0, len(g), LOT)]
    print(f"{sum(len(t[1]) for t in taches)} situations, {len(taches)} lots, {processus} processus", flush=True)
    with Pool(processus, initializer=init) as pool, open(sortie, "w", encoding="utf-8", newline="") as f:
        w = csv.writer(f)
        w.writerow(["cle", "legis", "k", "plafond"] + COMPOSANTES + ["ir_sans"])
        for n, res in enumerate(pool.imap_unordered(lot, taches), 1):
            w.writerows(res)
            f.flush()
            if n % 10 == 0 or n == len(taches):
                print(f"  {n}/{len(taches)} lots", flush=True)


if __name__ == "__main__":
    main(sys.argv[1], sys.argv[2], int(sys.argv[3]) if len(sys.argv) > 3 else 8)
