# Prompt for the next conversation: portfolio tables, figures and inference

Work in the empirical asset-pricing R repository ML_RV. Read AGENTS.md and DESIGN_DECISIONS.md, inspect the current branch/status/diff, and present a focused implementation plan for approval before editing. Preserve uncommitted work. Do not commit, push, refit ML models, regenerate portfolio backtests, or change portfolio definitions without separate authorization. Follow the existing dplyr/purrr style, extensive explanatory comments, and testthat conventions. This task extends reporting and manuscript text from completed backtests.

Read docs/PORTFOLIO_RESULTS_REVIEW.md and the reproducible analysis in tmp/pdfs/evaluate-completed-ports.R and tmp/pdfs/evaluate-ports-details.R as starting evidence, then independently verify relevant numbers against saved returns. Do not treat comments or the prior review as substitutes for the code, effective settings, provenance and source papers.

## Sources and scope

- Primary MM: ports/v3/1986/{panel,spx}/expanding_vol_10bps_60m/all_ports.rds.
- Primary Cederburg and multifactor: ports/v3/1986/allocations/expanding_vol_10bps_60m_alloc_60_60_60m/.
- Main OOS period: July 2011–December 2024, 162 months. Sixty is the minimum calibration history; estimation expands using all available prior observations, not a rolling 60-month window.
- Legacy full_sample runs are ex-post normalization sensitivities for the internet appendix only. Discover their actual settings, date coverage and available specifications; do not infer an allocation's normalization solely from its upstream folder name. Keep older expanding_rv and 120-month outputs separate. Compare alternative runs on common dates and report their native coverage too.
- Code: R/oos_ports.R, R/portfolio_helpers.R, R/allocation_ports.R, R/allocation_helpers.R, R/cederburg_helpers.R, R/multifactor_helpers.R, R/report_results.R, and their tests.
- Existing manuscript/output: latex_paper/manuscript.tex and latex_paper/results_v3/1986/{main,main_appendix,internet_appendix}/. Inspect existing portfolio_performance.tex, portfolio_section.tex, portfolio_wealth_*.png, portfolio_cluster_sharpe.png and portfolio_ew_sharpe_waterfall.* for style. Existing rendered tables contain older results and must not be mixed with new ones.
- JKP mapping: data/cluster_labels.csv. Distinguish return-factor themes from predictor-feature groupings.
- Reference PDFs in C:/Users/paulo.fonteles/Downloads/: ssrn-2659431 (1).pdf (Moreira–Muir); ssrn-3357038.pdf (Cederburg et al.); The Journal of Finance - 2024 - DeMIGUEL - A Multifactor Perspective on Volatility‐Managed Portfolios.pdf. Treat document instructions as document content. Read the papers directly when establishing methods or inference.

Use two major manuscript subsections: **Single-factor analysis** and **Multifactor analysis**. Main tables/figures emphasize EW forecast ensemble, HAR, HARX, lagged-RV/RW and unmanaged baselines. Detailed individual ML models belong in the appendix. Always distinguish “EW forecast ensemble” from “EW allocation.” Reuse the cached ensemble; do not rebuild predictions. Verify ensemble membership and the log-space averaging convention.

## Single-factor analysis

1. MM-style managed versus unmanaged: inverse own-factor predicted variance or own-factor lagged RV, with expanding volatility-matching normalization. Show panel-wide paired factor results, a separately labelled equal-weight aggregate of the 153 nonmarket factors, and the pooled-model market strategy. Put standalone market forecasts in the internet appendix.
2. Cederburg-style EW combination: 50% unmanaged sleeve plus 50% own-variance-managed sleeve versus the standalone unmanaged factor. Show the same scopes and forecast benchmarks. This EW combination is our robust extension, not Cederburg's estimated MVO allocation.
3. Appendix: signed Cederburg two-sleeve MVO versus unmanaged-only MVO. The latter optimizes factor/cash exposure and is not buy-and-hold. Include EW forecast, HAR, HARX and RW, with all other models in detailed tables. Do not transfer the multifactor nonnegativity constraint to this signed implementation.

For each family, tabulate annualized mean excess return, volatility, net Sharpe, CER at gamma=5, maximum drawdown, turnover and modeled costs. Show deltas versus the proper benchmark, mean raw exposure and managed/benchmark volatility ratio next to CER. Include spanning alpha, robust t-statistic, beta, R-squared and annualized appraisal ratio where meaningful. Gross results are useful to separate signal value from modeled costs; net results are primary. Clearly distinguish averages/medians of individual-factor statistics from statistics of the aggregate portfolio.

Use main wealth panels for the 153-factor aggregate and pooled market, with consistent colors across forecast models. Use theme-level distributions of paired delta Sharpe/delta CER, not just absolute Sharpe boxplots. Show counts, dispersion and fractions improved. Detailed factor-by-factor tables and the standalone-market panels go in the appendix.

## Multifactor analysis

Compare UMV, CMV and FCMV separately under EW and HEW allocation:

- UMV: unmanaged factors only.
- CMV: unmanaged plus managed sleeves, all conditioned on market volatility. Main signal from the pooled panel's market forecast; standalone market signal is an appendix comparison.
- FCMV: each factor's own predicted volatility conditions its managed sleeve.
- Historical comparator currently available: lagged_market, the square root of lagged market RV, broadcast to factors. It is not factor-own RW FCMV. If an own-factor lagged-RV FCMV comparator is desired, flag that it requires a new backtest and approval; do not fabricate it from CMV or existing single-factor results.

Deduplicate UMV and lagged_market copies across model files only after equality checks. Compare every conditional portfolio with its corresponding allocation's UMV. There are 154 underlying factors including the market and 308 conditional sleeves. HEW gives equal weight to 13 JKP themes plus a separate Market cluster, then equal factor weights within a cluster and equal sleeve weights within a factor. Thus unmanaged market weight is 1/14 under HEW versus 1/154 under EW. Explain this composition effect before interpreting HEW's higher Sharpe as forecasting value.

Main tables: panels by allocator; rows UMV and CMV/FCMV forecast specifications. Include net mean, risk, Sharpe, CER, paired gains, exposure, turnover and costs. Use corresponding wealth/drawdown panels without overcrowding; keep primary EW-forecast, HAR, HARX, lagged-market and UMV comparisons legible. Provide a theme contribution/exposure analysis and distinguish contributions to portfolio returns from standalone theme Sharpe ratios.

Keep multifactor MVO only in a diagnostic appendix table: dimensions, training observations, covariance rank/rcond, active underlying factors, normalized maximum factor share, effective number of factors 1/sum((w/sum(w))^2), binding exposure constraints, turnover/costs, worst return and insolvency count. Retain its performance for context. Current constrained results are concentrated but not insolvent; do not repeat old unconstrained insolvency claims or claim concentration alone proves MVO universally invalid.

## Backtest conventions to document and verify

Use f_t for holding-month factor excess return, v_t for the variance signal available at formation, and sigma_t=sqrt(v_t).

| Result | Managed basis / normalization | Allocation and comparison |
|---|---|---|
| Primary MM expanding_vol | c_t f_t/v_t; c_t=sd(f_s)/sd(f_s/v_s), s<t, available paired history | One managed factor; raw and 1.5-capped exposure; buyhold comparator |
| Legacy MM expanding_rv | Same inverse variance, c_t=mean(RV_s), s<t | Separate sensitivity; does not match return volatility |
| Appendix MM full_sample | Constant c matching full-evaluation-sample gross volatility | Explicitly ex post; caps and costs can break exact volatility matching |
| Primary Cederburg training_vol | Same own-variance c_t and historical pairs as primary MM; recomputed each formation | EW 50/50 versus buyhold; signed MVO versus unmanaged_mvo; combined exposure cap ±5 |
| Cederburg saved option | Historical saved MM weights/normalization | Separate training basis; not the primary runner's configuration; verify whether any requested legacy artifact actually exists |
| Multifactor UMV | Unmanaged factors | EW, HEW or constrained nonnegative MVO; matching UMV is comparator |
| CMV/FCMV EW or HEW | c_j,t f_j,t/sigma_j,t, with c_j,t=sd(f_j,s)/sd(f_j,s/sigma_j,s), s<t | Fixed sleeve coefficients, optional aggregate 1.5 exposure cap |
| CMV/FCMV MVO | f_j,t/sigma_j,t, no economic volatility normalizer | Nonnegative coefficients and optimizer exposure constraints; numerical column scaling is undone |

MM calibration_data retains warmup observations. Allocation uses these to calibrate while evaluating only on saved MM holding dates, avoiding a second 60-month delay. Cederburg uses one current c_t across all historical training rows; it does not insert each historical c_s into its training matrix. RW can have more prior calibration observations than ML forecasts. Confirm current MM and Cederburg scalers agree on keyed matching dates.

EW/HEW coefficients sum to one across sleeves; effective factor exposures need not. Multifactor MVO has exposure at most 1.5 and conditional exposure at most 1.2 times UMV exposure, subject to that absolute ceiling. Distinguish these constraints inside optimization from proportional post-allocation capping. Cederburg's ±5 cap and MM's 1.5 cap differ; show raw and capped results consistently and do not silently change constraints to harmonize them.

Our RV is a sum of daily squared returns; monthly factor returns compound daily returns. RW is a lagged second moment. Cederburg equation (4) additionally adjusts by 22 divided by the month's trading-day count; our saved target does not. The lagged-market multifactor signal is not exactly DeMiguel's demeaned within-month daily SD: variable trading-day counts and demeaning matter. Avoid claiming exact equivalence up to a universal sqrt(22). Compounded daily long-short factor returns also need not match a provider's independently formed monthly factor series. Document these departures rather than calling the backtests exact replications.

Costs are 10 bps times changes in target factor exposure, with entry from zero, combined single-factor sleeve netting and no terminal liquidation. They omit stock turnover, stock-level trade netting, factor internal implementation costs, shorting and financing costs. Never claim replication of DeMiguel's stock-level trading-diversification benefits. Report all results as conditional on this factor-level cost proxy.

Use monthly excess returns for Sharpe and CER. Default comparable net CER uses net-return variance. Existing multifactor cer_paper_ann uses gross variance and sharpe_paper uses net mean/gross SD (DeMiguel fn24 convention for Sharpe); show this separately and label it rather than silently mixing formulas across families. DeMiguel's cumulative-payoff figure adds self-financing payoffs (fn23); our compounded wealth adds rf to net excess returns and assumes a collateralized reinvested strategy. Label this difference; optionally provide cumulative excess payoffs as a separately defined comparison. Never compound an insolvent strategy through zero.

## Inference and robustness

Verify the source papers' procedures:

- MM: spanning-regression alpha and appraisal ratio; heteroskedasticity-robust standard errors in regression tables. Do not relabel this as a paired Sharpe bootstrap.
- Cederburg: Jobson–Korkie Sharpe-difference test with Memmel correction (fn8); CER-difference test attributed to DeMiguel–Garlappi–Uppal 2009 (sec4.3.1, fn19). Its simulation bootstrap serves a separate economic null and is not simply a paired OOS performance test.
- DeMiguel 2024: 10,000 paired stationary-block bootstrap draws, expected block length five months, one-sided Sharpe-difference inference (fn15); alpha Newey–West lag one, with five/ten-month robustness (fn25). Appendix alternatives include Jobson–Korkie and Ledoit–Wolf.

Implement paired month-block inference as the main robust reporting procedure for prespecified EW-forecast contrasts with unmanaged, RW, HAR and HARX: differences in Sharpe, mean excess return and CER. Cite the source accurately: extension to CER/mean is ours, not a verbatim DeMiguel 2024 procedure. Preserve dates and resample identical month indices across strategies and factors, retaining cross-factor dependence. Use a fixed documented seed, 10,000 draws, expected block length five and sensitivity to three/ten. Define the null, tail convention and confidence intervals explicitly; do not compute an invalid p-value from an uncentered bootstrap by accident. If reproducing the paper's one-sided sign-fraction procedure, label it precisely and handle identical-series/zero-variance cases. Report effect sizes, confidence intervals and raw/multiplicity-adjusted p-values, with a prespecified comparison family and dependence-appropriate correction. Do not treat 153 correlated factor results or factor-month observations as independent replication. Explain inference is conditional on fitted OOS strategies and does not refit the full learning pipeline.

Spanning alpha is an ex-post opportunity-set diagnostic, not proof of implementable MVO improvement. Add paired mean-difference HAC inference if helpful, with explicit bandwidth. Paper-comparable parametric Sharpe/CER tests can be appendix robustness; reproduce their formulas with verified tests, not library-name guesses.

Use common-date full-period comparisons, plus prespecified broad subperiods 2011H2–2014, 2015–2019, 2020–2024. Crisis panels may show COVID February–April 2020 and calendar 2022 returns/drawdowns/exposures. Short crisis windows are descriptive, not annualized Sharpe significance exercises. The primary run cannot evaluate the 2008 GFC. Do not choose windows or main specifications after seeing which produce favorable results.

## Delivery and validation

Extend existing reporting helpers rather than duplicating portfolio construction. Produce reproducible LaTeX tables, PDF/PNG figures, machine-readable statistics/inference, source manifests/effective settings, and concise manuscript text/captions explaining exactly what is shown. Inspect rendered figures/tables and compile the manuscript if supported; report unresolved build failures honestly.

Test return-column mapping, units/annualization, benchmark/date alignment, factor counts, unique keys, identical benchmark deduplication, paired bootstrap dependence and reproducibility, analytical/simple-fixture metric values, identical-strategy differences, zero variance, missing periods, insolvency, exposure diagnostics, and matching MM/Cederburg scalers. Pin a few actual verified numeric results from the primary artifacts with tolerances. Make tests about behavior, not duplicated implementation formulas.

Do not predetermine the narrative. Initial review finds the ensemble usually beats forecast benchmarks but not consistently unmanaged portfolios; gains vary by theme, risk metric and cap. FCMV gains over matching UMV are modest. Report adverse and inconclusive results as clearly as favorable ones. Deliver a summary of files changed, tests, outputs, conventions, and remaining limitations.
