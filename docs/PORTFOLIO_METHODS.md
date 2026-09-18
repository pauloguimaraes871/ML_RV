# V3 factor-specific volatility management

Run `R/oos_ports.R` from the project. It reads saved V3 forecasts and writes
`ports/v3/<sample>/<universe>/<normalization>_<cost>bps/`. It does not fit models.
The 1986 and 1927 directories can contain different sets of completed models;
every matching saved model is listed in the output manifest. Within a sample
and universe, all models must have the same factor-month forecast coverage.

## Formation dates and units

`month_id` is the month whose RV is predicted and whose return is earned.
Forecasts for that month are available at formation, before the return is earned.
The previous-calendar-month RV and the expanding normalizer use only earlier
months. A calendar join prevents a missing month from being treated as a lag.

V3 forecasts are in `log(RV + 1e-8)` units, not standardized target units. Inversion
is `exp(pred) - 1e-8`, without an invented bias correction. It is the inverse log
forecast, not automatically an unbiased conditional arithmetic expectation of RV.
Nonpositive or nonfinite inversions stop the calculation rather than being floored.
`rv_level_lag1` is an expanding mean of logged, lagged RV used as a model feature;
it is not the arithmetic mean of level RV used to anchor portfolio exposure.

The retained RV definition is the monthly sum of squared daily returns. It is
not demeaned, matching the existing forecast targets. Cederburg et al. equation
(4) additionally multiplies the sum by `22 / number_of_days`. We do not silently
apply that adjustment to models trained on a different target. The market target
also retains the original CRSP total-return definition used in model training.

## Normalization

For each factor independently, the managed excess return is `c_t / v_t * f_t`.
The ML strategy uses that factor's predicted RV; `rw` uses its previous month's
RV; `buyhold` is an exposure of one to the same factor, not always the market.

- `expanding_rv` (default): `c_t` is the arithmetic mean of the factor's observed
  RV through the preceding month. This is a past-only, factor-specific exposure
  anchor. It does **not** promise equality of managed and unmanaged volatility.
- `expanding_vol`: `c_t = sd(f_s) / sd(f_s / v_s)` on earlier usable observations.
  This implements the historical volatility-matching calibration described in
  Cederburg section 4.3.1. For ML it uses only saved prior OOS forecasts, not final
  in-sample predictions. With the default 120-observation requirement, the first
  ten years of ML forecasts are calibration observations and are reported as
  warm-up. RW uses historical lagged RV; all three strategies are evaluated on
  the same remaining dates within each factor.
- `full_sample`: constant risk matching over the evaluation sample, an explicitly
  **ex post** diagnostic (`settings$implementable = FALSE`). It is not the default
  and is not evidence of a feasible real-time allocation. Equality is for raw
  gross volatility; caps and costs change it.

Historical missing months are not imputed. Expanding means count only observed
RV months; risk matching counts only available return/signal pairs. Forecast
months must be consecutive. `coverage.csv` reports history gaps, calibration
warm-up and actual evaluation dates. Different factors can have different
histories, so the summaries do not pool their returns into a synthetic portfolio.

## Return construction and cash

JKP's `ret` is already signed and is a decimal long-short factor return. Do not
multiply by `direction`, and do not subtract RF again. Daily factor returns are
compounded within each month as `prod(1 + ret) - 1`. The resulting series need
not equal a provider monthly factor formed independently at a monthly frequency;
exact replication would require that native monthly series.

CRSP `vwretd` total returns are compounded within month and French RF is then
subtracted. The existing key `spx_vw` is retained; it labels the CRSP series used
by this project and does not establish that it is the S&P 500 index.
For both cases, collateralized total return is `RF + exposure * factor_excess`.

RF comes from the saved official `F-F_Research_Data_Factors_CSV.zip`, downloaded
September 14, 2026, with a July 2026 CRSP vintage. Percentage-point RF values are
divided by 100 once. The source starts in July 1926; no earlier cash returns are
invented. Current revised historical data are not a point-in-time-vintage dataset.

## Transaction costs and leverage

Raw and capped portfolios each pay `cost_per_unit * abs(w_t - w_(t-1))` based on
their own weights. This is MM target-exposure turnover, not drift-adjusted stock
turnover. Default exposure before entry is zero, so entry is charged. Set
`initial_weight = 1` to describe a pre-existing unmanaged exposure instead.
There is no forced liquidation charge at the end of the sample.

The default is 10 bp per exposure unit (`0.001`); MM sensitivities are 1 and 14 bp.
For long-short factors this is an assumed price per unit of factor exposure,
**not** an estimated bid-ask spread or stock-trading charge. Net results exclude
internal factor turnover, stock trade netting, borrow fees, financing spreads and
market impact. Unmanaged ongoing factor costs are also excluded. No claim of
DeMiguel stock-level cost replication is made with these data.

The cap is 1.5 factor units. For a dollar-long/dollar-short factor this can imply
3.0 units of gross stock exposure; it is not a 150% gross-stock leverage limit.
Raw exposures are not silently truncated. Months with a raw net total return at
or below -100% are counted; the workflow does not compound a wealth curve through
insolvency or invent a margin-call recovery rule.

## Evaluation and Cederburg's critique

Report direct OOS Sharpe and certainty-equivalent differences against each
factor's unmanaged version on identical dates. Sharpe uses excess-return standard
deviation, annualized by `sqrt(12)`. CER is `12 * (mean_excess - gamma/2 * variance)`;
gamma changes this metric only. Zero-variance Sharpe is explicitly undefined.
Five-year calendar blocks, maximum exposure and cap-binding frequency provide
descriptive stability diagnostics. Partial blocks retain their observation count.

A positive in-sample spanning alpha does not demonstrate that the managed
strategy beats the original factor, or that an optimal combination could have
been chosen in real time. No ex post combination is optimized here. No p-values,
multiple-testing claims or complete replication of Cederburg's combination tests
are implied. MVO, oracle, predicted-mean scaling and the market-only 60/40 strategy
have been removed from the individual-factor analysis. The separate multifactor
analysis below uses DeMiguel's constrained mean-variance specification.

## DeMiguel multifactor specification

`R/multifactor_helpers.R` implements equations (3)-(6) of DeMiguel, Martin-Utrera
and Uppal (2024), DOI 10.1111/jofi.13395. It jointly fits the unmanaged excess
returns and their market-volatility-scaled counterparts, `[f, f / sigma]`.
The conditional portfolio (CMV) has weights `a_k + b_k / sigma`, with both
coefficients nonnegative. The unconditional benchmark (UMV) sets `b_k = 0`.
There is no sum-to-one budget on these zero-cost factor exposures. The production
optimizer now imposes an explicit factor-exposure limit (see below). Gamma is 5
by default and determines the allocations, unlike the MM evaluation-only gamma.

`R/allocation_ports.R` now runs this analysis for each pooled-panel forecast model and the
shared EW forecast. The standalone market estimation universe is not pooled
with these factors. Samples remain separate (1986 and, when complete, 1927).
The current sample switch is preserved. The factor universe is fixed to the
saved panel forecast universe, including the market; it is not selected by
multifactor performance. This differs from the paper's nine-factor universe.

The production lagged-market comparator uses the square root of the previous
month's saved market RV. This sum-of-squares signal is not the paper's demeaned
daily sample SD: trading-day counts and daily means prevent an exact constant
conversion. `prepare_market_sigma()` remains available and tested for exact
daily-SD research, but the allocation script requires only saved MM inputs.
The ML alternatives use market forecasts or own-factor forecasts as documented
in ALLOCATION_METHODS.md.

Every allocation is fitted on an expanding window ending before its holding
month. All signals, including the historical benchmark, use the same saved OOS
forecast calendar so the benchmark does not receive extra estimation history.
At least 120 prior common months are required, giving July 2016 as the first
portfolio month for forecasts beginning July 2006. No in-sample ML forecasts are
substituted. Earlier incomplete history is excluded up to the latest series start
and recorded in settings; subsequent gaps or truncated evaluation returns fail.

The cost model is an explicit factor-level adaptation. With fixed nonnegative
coefficients, target-exposure turnover is `sum(b) * abs(diff(1 / sigma))`.
Its historical mean times `cost_per_unit` enters the optimization as a linear
penalty. Actual OOS costs charge entry from cash and all subsequent changes in
factor exposure, including changes caused by re-estimating `a` and `b`. Those
future coefficient-refit costs are not anticipated by the training proxy.
Internal factor trading, stock weight drift, trade netting, borrow and financing
remain unavailable. These outputs do not replicate the paper's stock-level
transaction-cost objective or its trading-diversification benefits.

Sample covariance is used without a ridge or inverse. The nonnegative convex
problem is solved after rescaling the columns for numerical accuracy, then
coefficients are returned in their original units. Singular covariance is allowed
(the full panel has 308 extended columns versus 120 initial observations); failure
of convergence or first-order optimality checks aborts. Singular problems can
have nonunique weights even with a bounded feasible set. Fit diagnostics record
training endpoints, dimensions, raw covariance rank/rcond, exposure limits and
optimality residuals. KKT convergence alone does not establish identification.

The default MVO solution now constrains current factor exposure inside the
optimizer: UMV at 1.5 and CMV/FCMV at min(1.5, 1.2 * UMV exposure). This is a
factor-level adaptation of the paper's relative-leverage check, not stock-level
leverage. Setting `mvo_exposure_limit = NULL` explicitly retains the old unbounded
experiment. The `capped` version remains a proportional-rescaling diagnostic for
compatibility and robust rules; `implementation` identifies it in summaries.
For default constrained MVO it is redundant. Both mean exposure levels are
reported and raw/capped costs are still calculated separately.

Both portfolios use identical evaluation dates. `sharpe_paper` follows footnote 24:
annualized mean net excess return divided by gross-return volatility. `sharpe_net`
uses net-return volatility instead. `cer_paper_ann` likewise penalizes gross
variance. RF is retained separately and insolvency counts use `1 + RF + net`;
Sharpe and CER are NA when any insolvency occurs, and no wealth curve is
compounded through insolvency. No inference or significance
claims are produced. The existing compounded monthly factor returns are retained;
this is another data convention difference from an exact paper replication.

Current outputs live in `ports/v3/<sample>/allocations/<mm_run>/`, with individual
RDS results, `summary.csv`, `settings.rds`, an executed-script snapshot and a
manifest hashing the effective settings and snapshot as well as input files.
Historical checkpoints are not consumed by this workflow. See
`docs/ALLOCATION_METHODS.md` for all baseline and allocation identifiers.

MM summaries and reporting tables expose average raw weight and volatility
relative to the gross unmanaged return on identical dates. Each return version
has its own risk ratio, displayed beside CER. The factor-panel ratio is computed
after aggregating factor returns. Expanding mean RV does not ensure unit average
exposure or matched return volatility; no forecast bias adjustment is added.
Sharpe is invariant only to constant scaling of a return series, not generally
to changing time-varying normalizers, caps, costs, or evaluation windows.

## References

- Moreira and Muir (2017), equations (1)-(2), Table IV:
  https://doi.org/10.1111/jofi.12513
- Cederburg et al., attached March 2019 manuscript, equations (3)-(4), section
  4.3.1 and direct OOS comparisons; published article:
  https://doi.org/10.1016/j.jfineco.2020.04.015
- French RF definition and the June 2024 source change:
  https://mba.tuck.dartmouth.edu/pages/faculty/ken.french/Data_Library/f-f_factors.html

## Verification

`tests/testthat/test-oos-ports.R` tests factor isolation, temporal invariance,
calendar alignment, units, normalization, entry and separate turnover costs,
paired metrics, historical gaps and invalid inputs. Output manifests record
input/model/code hashes and each run saves its R session information. Neither
load_data.R's target construction nor fit_models.R's estimation was changed.
