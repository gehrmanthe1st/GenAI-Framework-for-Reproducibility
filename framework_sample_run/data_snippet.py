# dataset_snippets.py
import pandas as pd
from pathlib import Path
from typing import List, Tuple, Optional



def _summarise_column(col: pd.Series) -> dict:
    n = len(col)
    na = col.isna().sum()
    pct_na = round(100 * na / n, 2)
    n_unique = col.nunique(dropna=True)

    summary = dict(
        dtype=str(col.dtype),
        example=col.dropna().iloc[0] if n_unique else None,
        **{"%missing": pct_na, "n_unique": n_unique},
        min=None,
        max=None,
    )

    if pd.api.types.is_numeric_dtype(col):
        summary["min"] = col.min()
        summary["max"] = col.max()
    return summary


def _md(df: pd.DataFrame) -> str:
    return df.to_markdown(index=False, tablefmt="github")


def make_llm_snippets(
    file_path: str,
    id_cols: Optional[List[str]] = None,
    extra_strata: Optional[List[str]] = None,
    n_per_stratum: int = 5,
) -> Tuple[str, str, str]:
    """
    Parameters
    ----------
    file_path : str
        Path to full CSV.
    id_cols : list[str] or None
        Primary grouping columns (e.g. ["subj"]). If None, no grouping.
    extra_strata : list[str] or None
        Additional grouping columns (can be multiple). Ignored if None.
    n_per_stratum : int
        How many rows to sample per stratum (or overall if no grouping).

    Returns
    -------
    preview_md, summary_md, counts_md : str
        Markdown strings for LLM prompts.
    """
    df = pd.read_csv(file_path)

    # ---------- 1. sampling logic ----------
    if id_cols is None:
        strata_cols: List[str] = []
    else:
        strata_cols = id_cols + (extra_strata or [])
        
    if strata_cols:  # grouped sampling
        sampled = (
            df.groupby(strata_cols, group_keys=False)
            # A random seed can be added to the function here as an extension if deterministic sampling is needed in groups
            # Use <,random_state=476> after the <len(g)> argument, 
              .apply(lambda g: g.sample(min(n_per_stratum, len(g)))) 
              .reset_index(drop=True)
        )
        sample_title = f"{n_per_stratum} rows per stratum ({' × '.join(strata_cols)})"
    else:  # no grouping – simple random sample
        # seed can be added if needed for reproducibility add <, random_state=42> if needed after the <min()> argument
        sampled = df.sample(min(n_per_stratum, len(df)))
        sample_title = f"Random sample of {len(sampled)} rows (no grouping)"

    preview_md = f"### Dataset sample – {sample_title}\n\n" + _md(sampled)

    # ---------- 2. column-wise summary ----------
    summary_rows = [
        dict(column=col, **_summarise_column(df[col])) for col in df.columns
    ]
    summary_df = pd.DataFrame(summary_rows)[
        ["column", "dtype", "min", "max", "example", "%missing", "n_unique"]
    ]
    summary_md = "### Column-wise summary\n\n" + _md(summary_df)

    # ---------- 3. factor counts ----------
    cat_cols = df.select_dtypes(include="object").columns
    counts_parts = []
    for col in cat_cols:
        counts = df[col].value_counts(dropna=False).reset_index()
        counts.columns = [col, "n"]
        counts_parts.append(f"#### Value counts for `{col}`\n\n" + _md(counts))
    counts_md = "\n\n".join(counts_parts) if counts_parts else "### No categorical columns\n"

    return preview_md, summary_md, counts_md


# -------------------- demo --------------------
if __name__ == "__main__":
    prev, summ, cnt = make_llm_snippets(
        file_path="dataset.csv",
        id_cols=None,             # None => no grouping
        extra_strata=[],  # can only be [] or multiple columns
        n_per_stratum=5,
    )
