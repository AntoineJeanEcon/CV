# -*- coding: utf-8 -*-
"""
000_download_cmip6.py
=====================
Téléchargement des données CMIP6 brutes depuis ESGF (nœud principal CEDA).
10 modèles retenus sur critère de disponibilité hist-nat confirmée (DAMIP)
avec pr et tas en fréquence mensuelle.

Modèles retenus (diversité des centres de modélisation) :
  MIROC6 (Japon), CanESM5 (Canada), IPSL-CM6A-LR (France),
  CNRM-CM6-1 (France), HadGEM3-GC31-LL (UK), MRI-ESM2-0 (Japon),
  BCC-CSM2-MR (Chine), GFDL-ESM4 (USA), ACCESS-ESM1-5 (Australie),
  NorESM2-LM (Norvège)

Expériences : historical, hist-nat
Variables   : pr (précipitations), tas (température de surface)
Fréquence   : mensuelle (Amon)

Sources
-------
Nœud principal  : https://esgf.ceda.ac.uk/esg-search/search
Nœud fallback   : https://esgf-node.llnl.gov/esg-search/search

Sortie
------
data/cmip6/raw/*.nc   (fichiers bruts par modèle/expérience/variable)
data/cmip6/download_log.txt
"""

import requests
import time
from pathlib import Path
from datetime import datetime

# ── CONFIG — À ADAPTER À VOTRE ARBORESCENCE ──────────────────────────────────
ROOT     = Path(".")          # remplacez par le chemin absolu de votre projet
OUT_DIR  = ROOT / "data/cmip6/raw"
LOG_PATH = ROOT / "data/cmip6/download_log.txt"
OUT_DIR.mkdir(parents=True, exist_ok=True)

MODELS = [
    "MIROC6",
    "CanESM5",
    "IPSL-CM6A-LR",
    "CNRM-CM6-1",
    "HadGEM3-GC31-LL",
    "MRI-ESM2-0",
    "BCC-CSM2-MR",
    "GFDL-ESM4",
    "ACCESS-ESM1-5",
    "NorESM2-LM",
]
EXPERIMENTS = ["historical", "hist-nat"]
VARIABLES   = ["pr", "tas"]

# Variants spécifiques par modèle (r1i1p1f1 par défaut)
VARIANTS = {
    "HadGEM3-GC31-LL": "r1i1p1f3",
    "CNRM-CM6-1":      "r1i1p1f2",
}
DEFAULT_VARIANT = "r1i1p1f1"

NODES = [
    "https://esgf.ceda.ac.uk/esg-search/search",
    "https://esgf-node.llnl.gov/esg-search/search",
]

# ── HELPERS ───────────────────────────────────────────────────────────────────
LOG_LINES = []

def log(msg=""):
    ts   = datetime.now().strftime("[%Y-%m-%d %H:%M:%S]")
    line = f"{ts} {msg}"
    print(line)
    LOG_LINES.append(line)

def search_files(node, model, experiment, variable, variant):
    params = {
        "type":           "File",
        "project":        "CMIP6",
        "source_id":      model,
        "experiment_id":  experiment,
        "variable_id":    variable,
        "frequency":      "mon",
        "variant_label":  variant,
        "format":         "application/solr+json",
        "limit":          100,
        "latest":         "true",
        "fields":         "title,size,url,checksum,checksum_type",
    }
    r = requests.get(node, params=params, timeout=30)
    r.raise_for_status()
    docs = r.json()["response"]["docs"]
    results = []
    for doc in docs:
        for url_entry in doc.get("url", []):
            if "|HTTPServer" in url_entry:
                url = url_entry.split("|")[0]
                results.append({"title": doc["title"], "url": url,
                                 "size": doc.get("size", 0)})
                break
    return results

def download_file(url, dest_path):
    r = requests.get(url, stream=True, timeout=120)
    r.raise_for_status()
    with open(dest_path, "wb") as f:
        for chunk in r.iter_content(chunk_size=8192):
            f.write(chunk)

# ── MAIN ─────────────────────────────────────────────────────────────────────
log("=" * 70)
log(f"000_download_cmip6.py — Téléchargement CMIP6")
log(f"Modèles      : {MODELS}")
log(f"Expériences  : {EXPERIMENTS}")
log(f"Variables    : {VARIABLES}")
log("=" * 70)

n_ok, n_skip, n_err = 0, 0, 0

for model in MODELS:
    variant = VARIANTS.get(model, DEFAULT_VARIANT)
    for experiment in EXPERIMENTS:
        for variable in VARIABLES:

            tag   = f"{model} / {experiment} / {variable} / {variant}"
            files = []

            for node in NODES:
                try:
                    files = search_files(node, model, experiment, variable, variant)
                    if files:
                        log(f"\n{tag} — {len(files)} fichier(s) trouvé(s) [{node}]")
                        break
                except Exception as e:
                    log(f"  Nœud {node} inaccessible : {e}")

            if not files:
                log(f"\nATTENTION — Aucun fichier trouvé : {tag}")
                n_err += 1
                continue

            for f in files:
                dest     = OUT_DIR / f["title"]
                size_mo  = f["size"] / 1e6

                if dest.exists():
                    log(f"  SKIP (déjà présent) : {f['title']}")
                    n_skip += 1
                    continue

                log(f"  Téléchargement : {f['title']} ({size_mo:.1f} Mo)")
                try:
                    download_file(f["url"], dest)
                    log(f"  OK : {f['title']} — {dest.stat().st_size/1e6:.1f} Mo sur disque")
                    n_ok += 1
                except Exception as e:
                    log(f"  ERREUR téléchargement {f['title']} : {e}")
                    if dest.exists():
                        dest.unlink()
                    n_err += 1

            time.sleep(1)

log("")
log("=" * 70)
log(f"Fin — OK : {n_ok} | Skip : {n_skip} | Erreurs : {n_err}")
log("=" * 70)

LOG_PATH.parent.mkdir(parents=True, exist_ok=True)
LOG_PATH.write_text("\n".join(LOG_LINES), encoding="utf-8")
