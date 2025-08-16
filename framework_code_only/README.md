# Framework Code Only — Quick Use

Use this folder to run the **GenAI framework notebook** with your **own** article/data. All outputs appear **inside the notebook**.

## Prerequisites
- Create & activate the repo venv and install from `requirements-lock.txt` (see `env/ENVIRONMENT.md`).
- In this folder, copy `.env.example` → `.env` and add your API keys.

## What you need to provide
- **Article** (PDF or text excerpt of Methods & Results)
- **Dataset** (CSV/SPSS/etc.; include a codebook if available)
- *(Optional)* **Tables & Figures** (screenshot in .png format of table or figure required)
- **Target outcomes** (part of results section you aim to reproduce)

> Edit file paths in the necessary (marked) cell of `GenAI_framework.ipynb`.

## Workflow (aligns with thesis appendices)
1. **Load data & context**
   - Use the PDF extractor to pull Methods/Results.
   - Load & tidy the dataset (and codebook if present).
   - Run the data summarizer for preview + variable summaries.
   - *(Optional)* Run table/figure describers.

2. **Prompt 1 — Analysis Pipeline Creation** (Reproducibility AI)
   - **Input:** article excerpt, dataset context, explicit goals.
   - **Output:** 9-step analysis pipeline.

3. **Prompt 2 — Pipeline Review & Revision** (Reviewer AI)
   - **Input:** prior inputs + pipeline.
   - **Output:** metric-based evaluation + **revised pipeline**.

4. **Prompt 3 — Data & Code Diagnostics** (Reproducibility AI)
   - **Input:** revised pipeline (+ any edits).
   - **Output:** library list, dataset understanding, cleaning plan, edit log.

5. **Prompt 4 — Model Clarification (optional)** (Reproducibility AI)
   - **Input:** add/remove libraries, extra dataset notes (only if needed).
   - **Output:** edit log; questions if assumptions need confirmation; “ready to code” signal.

6. **Prompt 5 — Final Code Generation** (Reproducibility AI)
   - **Output:** **R code** consistent with the approved pipeline.

7. **Prompt 6 — Review & Revise Code** (Reviewer AI)
   - **Input:** generated R code (+ errors if you tested).
   - **Output:** **final revised R code**.

**Optional:** *Code Error Fix* — provide the last code + specific error to receive a corrected version (Reviewer AI).

Refer the provided study for further clarity on the method to run under **Methodology: Testing phase**, and **Appendix D: Framework code** to refer to examples.

## Notes
- You may adjust **developer/system and user roles** for **prompts** inside the notebook as per your necessities.
- Keep `.env` **in this folder**; it is intentionally ignored by Git.
- All results/logs remain **in-notebook** unless you export them explicitly.
