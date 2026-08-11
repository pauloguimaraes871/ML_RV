# V2 data and model audit

Date: 2026-08-11

## Executive conclusion

The VIX/VXO correction in `load_data.R` is correct and economically material for
the 1986 specification.  It removes use of the penultimate daily VIX/VXO
observation from the forecast month and instead uses the previous month's final
available observation.  This is the appropriate timing convention when a row
labelled month *t* is used to forecast realized variance during month *t*.

The correction does **not** change the 1927 or 1962 datasets: the v1 and v2
RDS files have identical SHA-256 hashes.  They exclude the implied-volatility
series because it begins later.  The 1986 raw, z-score, and range-preprocessed
datasets all differ between v1 and v2.  Thus, only the 1986 model family is
fundamentally changed by this correction.

The current repository is not yet ready to rerun `oos_ports.R` on v2 models.
That script still reads legacy `models/.../list_backtest_res_*.rds` files, and
`fit_models.R` still builds those legacy lists from legacy model files.  It does
not construct an equivalent v2 list from `models_v2`.

## Evidence for the VIX/VXO correction

The old code calculated `lag(vix, 1)` before selecting month end.  After the
monthly `filter(dates == max(dates))`, that produced the prior trading day's
VIX in the same forecast month.  The revised code selects month end, ungroups,
then calculates the lag, giving the previous month's month-end VIX.

For the local VIX file, 427 of 429 months with at least two observations have a
different old-versus-new raw VIX input.  Before the variance transformation,
the median absolute difference is 3.3% and the maximum is 27.2%.  Since the
feature is squared and divided by 12, the downstream difference can be larger.

Hash checks:

| Dataset pair | Result |
|---|---|
| `covariates_1927.rds` / `covariates_1927v2.rds` | identical |
| `covariates_1962.rds` / `covariates_1962v2.rds` | identical |
| `covariates_1986.rds` / `covariates_1986v2.rds` | different |
| 1986 z-score and range preprocessed pairs | different |

## `models_v2` provenance and tuning inventory

The serialized objects were inspected without loading them into R.  Every
artifact except the three noted below contains an embedded v2 dataset label in
the serialized `backtest_meta` data.  The labels match the intended raw,
z-score, or range-preprocessed input.  The source code also explicitly writes
those labels after fitting.

Artifacts that cannot be certified as v2:

- `models_v2/1986/nn1_range_1986.rds`
- `models_v2/1986/rf_range_1986.rds`
- `models_v2/1986/rf_zscore_1986_grid.rds`

The first two embed the old-style covariate object name without a v2 provenance
label; the third does not contain a `wf_backtest_results` metadata record at
all.  Do not use these three in a v2 comparison until they are re-created and
read back in R to verify `@backtest_meta$dataset` and `@backtest_meta$tuning_method`.

The 1927 artifacts are HAR-X plus ENET, RF, XGBoost, and NN1--NN5.  HAR-X has
no tunable hyperparameters; each non-HAR model is marked as Bayesian
optimization.  There is **no 1927 grid-search artifact**, so the claim that all
1927 models were trained under both methods is false.

For 1986, ENET and the five NN architectures have both Bayesian and
grid-named range artifacts, as well as Bayesian z-score artifacts.  RF and
XGBoost have candidate range/grid pairs, but method provenance should be
re-read in R before relying on their filenames.  Several `*_grid.rds` objects
are actually marked `bayesian_optimization`; a filename is not a valid method
record.

The current `fit_models.R` is itself insufficient to reproduce all grid
artifacts: its v2 fit calls are Bayesian optimization calls, while the result
compiler infers the search method from the *length* of a domain rather than the
stored `tuning_method`.  A genuine two-point grid is therefore misclassified
as Bayesian optimization.

## Assessment of tuning choices

The rolling time-ordered train/validation/test arrangement is sound in
principle.  It follows the essential discipline in Gu, Kelly, and Xiu (2020):
choose complexity on a disjoint validation sample, then refit before testing.
Carr, Wu, and Zhang (2020), which is directly about realized variance, is an
especially useful reference: it emphasizes time-series validation and warns
that extensive hyperparameter search is prone to overfit noisy financial data.

The model classes and broad parameter ranges are defensible for a small monthly
S&P 500 RV time series:

- ENET: `alpha` from 0 to 1 and `lambda.min.ratio` from `1e-4` to `1e-2` are
  reasonable.
- RF: 300--1500 trees, shallow-to-moderate depth (3--12), and terminal nodes
  of 1--6 are plausible, although a terminal node of 1 is aggressive for this
  sample size.
- XGBoost: depth 2--5, learning rate 0.05--0.20, and sub/column sampling
  0.60--0.95 are conservative enough to be defensible.
- Dense NN: compact pre-specified architectures and early stopping are more
  defensible than an architecture search.  The batch-size upper bound of 256 is
  larger than some training windows after sequence construction and should be
  capped explicitly and recorded.

The main concern is search dimensionality and parameterization, not the
individual endpoints.  Each NN tunes six parameters and XGBoost tunes eight,
but only about 32 and 28 Bayesian objective evaluations respectively per
rebalancing date (initial points plus iterations).  That is a thin budget in a
noisy, high-dimensional, dependent time series.  More importantly,
regularization strengths and learning rates are optimized on linear scales
despite spanning orders of magnitude.  Search `log10(lambda)`, `log10(lr)`,
and related positive penalties; round/cast integer parameters inside the
objective; then store the transformed and realised values.

For the first cleaned rerun, reduce the flexible-model search to a small,
predeclared set of influential parameters.  This is more credible than
expanding the budget or porting grids from the much larger cross-sectional
return settings of Gu et al. (2020) and Hanauer and Kalsbach (2023).

## Important implementation findings

1. **Blocking -- v2 portfolios are impossible from the current scripts.**
   `oos_ports.R` reads `models/`, not `models_v2/`; the list-building section in
   `fit_models.R` also reads and writes legacy objects.  Running it would
   silently backtest the old models.

2. **Blocking -- LSTM is not saved as a v2 artifact.**  The LSTM call writes
   `lstm_backtest_res_1986_rescaled_5.rds` in the working directory, not under
   `models_v2/1986`, and records the wrong dataset name
   (`covariates_1986_df_gold2`) despite receiving the z-score data frame.

3. **Important -- the LSTM validation sequences omit training-tail context.**
   `set_eval_function()` creates sequences independently for train and
   validation.  With `padding = FALSE`, the first 11 validation months are
   discarded for a 12-month sequence, and the remaining validation sequences
   lack the final training observations that would be available in deployment.
   This does not create look-ahead bias, but it tunes a different forecasting
   problem from the refit/OOS problem.  Build validation sequences by prepending
   the last `T - 1` training feature rows, then score only validation targets.

4. **Important -- output paths are inconsistent.**  The 1927 XGBoost call saves
   to `models_v2/1986/xgb_range_1927.rds`, and the saved filename therefore
   contradicts its training sample.  It must be written under `models_v2/1927`.

5. **Important -- model-result labels are unreliable.**  `compile_results()`
   determines `search_type` from domain vector lengths rather than
   `@backtest_meta$tuning_method`.  This misreports two-point grids and explains
   misleading `grid` names.

6. **Important -- validation and test provenance lack a data fingerprint.**
   A manually assigned dataset-name string is useful but not sufficient.  Store
   the input file SHA-256, date range, feature names, target transform, code
   commit, package lock hash, and tuning seed in every backtest object.

7. **Robustness -- reproducibility is fragile.**  Analysis scripts call
   `renv::install()` at runtime and modify the global `future` plan.  These
   side effects make a rerun environment-dependent.  Tests cover only
   preprocessing helpers; there are no tests for VIX timing, split integrity,
   model provenance, LSTM alignment, or the portfolio return join.

## Recommended execution order

1. Preserve this commit as the audit checkpoint; do not interpret any current
   v2 portfolio output.
2. Correct the v2 list-builder and `oos_ports.R` paths, XGBoost path,
   `compile_results()` provenance logic, and LSTM validation/save metadata.
3. Add targeted tests, including a regression test proving VIX at month *t*
   equals the previous month-end input and a test that `oos_ports` rejects
   non-v2 model lists.
4. Re-run the 1986 non-LSTM models whose v2 provenance is not certified, then
   re-run LSTM.  Only after a v2 list is constructed should portfolio backtests
   be rerun.
5. Treat the 1927 models as unchanged for this issue; they need not be retrained
   solely because files were renamed v2.

Adding JKP factor returns to the **portfolio application only** does not by
itself require retraining S&P 500 RV models: the same corrected variance
forecast can scale a new return series.  Retraining is necessary if factors are
new predictors, if each factor receives its own RV target, or if the learning
task is otherwise changed.  Define that economic mapping before launching a
large rerun.

## Sources consulted

- Carr, Wu, and Zhang (2020), `papers/CarrWuZhang2020JOI.pdf`.
- Gu, Kelly, and Xiu (2020), `papers/Empirical Asset Pricing via Machine_rfs2020.pdf`.
- Hanauer and Kalsbach (2023), *Machine learning and the cross-section of
  emerging market stock returns*, Emerging Markets Review 55, 101022.
- Additional local volatility references: `papers/Predictive regressions for
  aggregate stock market volatility_JFE2012.pdf` and Schwert (1989).

## Verification limitation

This workstation has no R or Rscript executable, so full R-level deserialization
and the `testthat` suite could not be executed.  Serialized RDS contents,
source code, file hashes, and the underlying VIX CSV were inspected directly.
Before relying on results, run the tests and a small end-to-end v2 smoke test in
the project's R/renv environment.
