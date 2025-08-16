# Sample Run

Self-contained demo: notebook + helpers + article/dataset/targets.

## Contents
- GenAI_framework.ipynb
- config.py, data_snippet.py, Metrics_checker.txt
- article.pdf
- data_original/ (e.g., BogusVisualFeedbackData.sav)
- targetOutcomes.md
- .env.example → copy to .env (local secrets, not committed)

## How to run
1) See env/ENVIRONMENT.md to create/activate .venv and install from requirements-lock.txt.  
2) In this folder, copy .env.example → .env and add your keys.  
3) Start Jupyter **from the activated venv** (`python -m jupyter lab`) or select the registered kernel.  
4) Open GenAI_framework.ipynb and run all cells.
