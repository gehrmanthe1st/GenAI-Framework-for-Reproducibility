# GenAI Reproducibility Framework

## Thesis context & purpose

This repository accompanies my **master’s thesis** on using a structured GenAI workflow to reproduce statistical analyses reported in research articles. The framework turns article Methods/Results into an **auditable analysis pipeline**, generates **vetted R code**, and evaluates outputs against **target outcomes**.

This repository contains:
- **framework_sample_run/** — self-contained demo (notebook + helpers + 1 article/graph/dataset/targets).
- **framework_code_only/** — framework notebook + helpers (no data; plug in your own).
- **framework_testing_records/** — 16 study folders (supplementary material).
- **Results_Analysis/** — analysis notebook and result sheets.
- **env/** — environment instructions (`ENVIRONMENT.md`, `python-version.txt`).
- **requirements-lock.txt** — exact package versions for reproducibility.

---

## Quick start

1. Create and activate a virtual environment **at the repo root** and install the exact package set (see `env/ENVIRONMENT.md`).
2. Choose a folder to run (e.g., `framework_sample_run/` or `framework_code_only/`).
3. In that folder, copy `.env.example` → `.env` and add your API keys.
4. Start Jupyter from the activated venv *or* select the registered kernel, then open the notebook in that folder and run all cells.
   - The notebook reads files that sit next to it (e.g., `config.py`, `data_snippet.py`, `Metrics_checker.txt`, data files, `.env`).

## Note

`.env` files contain your API KEYS and are not related to the virtual enviroment or `env/ENVIRONMENT.md`. These files are treated as confidential information, please handle your `.env` with care.
In this git a placeholder `.env.example` has been added in place of `.env`, do consider for this while running.

---

## Folder guide

- **framework_sample_run/**  
  End-to-end example. Open `GenAI_framework.ipynb`. Uses the included demo article/data/targets.

- **framework_code_only/**  
  Same notebook + helpers, but no data. Provide your own inputs and run.

- **framework_testing_records/**  
  Supplementary archive of 16 tests (R scripts, data, notes). Not needed to run the demos.

- **Results_Analysis/**  
  Notebook and spreadsheets summarizing results.

---

## Environment & secrets

- Recreate the environment **exactly** with `requirements-lock.txt` (instructions in `env/ENVIRONMENT.md`).
- Each runnable folder needs a local `.env` **in the same folder as the notebook**.  
  Use `.env.example` as a template. Real `.env` files are intentionally ignored by Git.

---

## Troubleshooting (short)

- **Wrong Python/kernel:** launch Jupyter *from the activated venv* or select the registered kernel shown in `env/ENVIRONMENT.md`.
- **Cannot import helper modules:** make sure you opened the notebook **in its own folder** (helpers live next to it).
- **`.env` not loaded:** confirm the `.env.example` file is renamed to`.env` and that the `.env` file is in the same folder as the notebook and the keys are set.

