# ML_RV

Machine-learning forecasts of factor realized volatility and their use in
volatility-managed portfolios, including a replication of DeMiguel et al. (2024)
"A Multifactor Perspective on Volatility-Managed Portfolios" with ML signals.

- Pipeline code: `R/`; tests: `tests/testthat/`.
- Manuscript and rendered results: `latex_paper/`.
- Method notes: `docs/` (start with `docs/DEMIGUEL_REPLICATION.md` for the
  replication and `docs/PORTFOLIO_METHODS.md` for the portfolio backtests).
- **Data and models:** several inputs and all fitted models are deliberately
  not in git. `docs/DATA_AND_MODELS.md` lists what to download, what to
  rebuild and in which order, and how each file is pinned by SHA-256.
