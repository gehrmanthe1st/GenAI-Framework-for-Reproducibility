# Environment (Author Setup)

This project runs from Jupyter notebooks. Recreate the Python environment exactly
using the pinned lockfile requirements-lock.txt. The steps below work on Windows, macOS, Linux, and WSL.

----------------------------------------------------------------

## What you need
- Python (a 3.x version compatible with the lockfile; see env/python-version.txt if included)
- pip (bundled with recent Python)
- Optional: JupyterLab/Notebook; otherwise you can launch Jupyter from the venv

Runnable, self-contained notebook folders for the GenAI framework:
- framework_sample_run/
- framework_code_only/

Each folder contains a notebook and its helper files side-by-side (e.g., data_snippet.py, Metrics_checker.txt, data files, and a local .env).

----------------------------------------------------------------

## Create and activate the virtual environment (at the repo root)

Create one venv in the repo root: ./.venv. Use this same venv for all notebooks.

Windows (PowerShell):
    cd path\to\genai_reproducibility_framework
    python -m venv .venv
    .venv\Scripts\Activate.ps1

macOS / Linux / WSL (bash/zsh):
    cd /path/to/genai_reproducibility_framework
    python -m venv .venv
    source .venv/bin/activate

----------------------------------------------------------------

## Install the exact package set from the lockfile

    python -m pip install --upgrade pip setuptools wheel
    pip install -r requirements-lock.txt

This installs the precise versions used by the author for bit-for-bit reproducibility.

----------------------------------------------------------------

## Use the correct Jupyter kernel (two options)

You must run the notebooks with the interpreter from the created venv in previous step.

Option A - Quick run (recommended)
Start Jupyter from the activated venv so it uses the right interpreter automatically:
    python -m jupyter lab
    (or: python -m notebook)

Then open the notebook you want (e.g., framework_sample_run/GenAI_framework.ipynb) and run cells.

Option B - Register a named kernel once, then pick it in the UI
If you prefer starting Jupyter any way you like, register this venv as a kernel:
    pip install ipykernel
    python -m ipykernel install --user --name genai-repro --display-name "Python (genai-repro)"

Then in Jupyter:
- Classic Notebook: Kernel -> Change kernel -> "Python (genai-repro)"

----------------------------------------------------------------

## Secrets (.env files)

Each runnable folder expects a local .env in the same folder as its notebook.
Do not commit real secrets. Use the provided .env.example in each folder as a template:
1) Open .env.example and paste your own keys
2) Rename .env.example to .env

----------------------------------------------------------------

## Running the notebooks (summary)

1) Activate the repo venv (.venv)
2) Ensure Jupyter uses this venv (Option A or B above)
3) Open the notebook you want (e.g., sample_run/GenAI_framework.ipynb)
4) Run all cells - notebooks read sibling files (helpers, articles, .env) with no special paths. Loading in dataset will require special pathing and user defined functions

----------------------------------------------------------------

## Quick verification cell (optional)

Put this in the first cell and run to confirm the environment:
    import sys, platform, pandas as pd
    print("Python:", sys.version.split()[0], "| OS:", platform.system(), platform.release())
    print("pandas:", pd.__version__)

----------------------------------------------------------------

## Common fixes

Notebook opened with the wrong Python:
- Select the correct kernel (see "Use the correct Jupyter kernel"), or re-launch Jupyter from the activated venv.

Kernel not visible:
- Register it once:
    pip install ipykernel
    python -m ipykernel install --user --name genai-repro --display-name "Python (genai-repro)"

"import data_snippet" fails:
- The helper lives next to the notebook. Add at the top of the notebook:
    import sys, pathlib
    sys.path.insert(0, str(pathlib.Path.cwd()))

".env" not being read:
- Ensure the .env file sits beside the notebook and is named exactly .env.
- To load it explicitly:
    from pathlib import Path
    from dotenv import load_dotenv
    load_dotenv(Path.cwd() / ".env")
