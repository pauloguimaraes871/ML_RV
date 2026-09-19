# Inference for completed portfolio strategies

## Scope and entry point

This reporting layer reads completed backtests. It never refits ML models,
rebuilds the forecast ensemble, estimates new allocation weights, or changes
portfolio definitions. Run from the project R session:

```r
source("R/report_portfolio_inference.R")
report_portfolio_inference()
```

It reads the primary `expanding_vol_10bps_60m` MM portfolios and the allocation
run `expanding_vol_10bps_60m_alloc_60_60_60m`. Evaluation dates are July 2011
through December 2024, 162 months. Sixty is a minimum calibration history.
Subsequent estimation expands. Legacy normalization sensitivities do not enter
these tests.

The implementation uses two procedures: paired stationary-block inference for
performance differences, and Newey-West inference for spanning alpha. It does
not add a collection of alternative parametric tests.

## Relation to the papers

The directly inspected reference PDFs are Moreira and Muir, SSRN 2659431,
Cederburg et al., SSRN 3357038, and DeMiguel, Martín-Utrera and Uppal (2024),
*A Multifactor Perspective on Volatility-Managed Portfolios*.

- Cederburg et al. use the Jobson-Korkie Sharpe comparison with Memmel's
  correction (footnote 8). Their CER-difference test is attributed to DeMiguel,
  Garlappi and Uppal (2009), Section 4.3.1 and footnote 19. Their simulation
  bootstrap examines a separate economic null. We preserve the distinction
  between spanning opportunities and implementable out-of-sample allocations.
  We do not implement those additional parametric tests here.
- DeMiguel et al. (2024), footnote 15, use 10,000 paired stationary-bootstrap
  samples with expected block length five and a one-sided sign-fraction
  procedure for Sharpe differences. Footnote 25 uses Newey-West lag one for
  alpha, with five- and ten-month robustness. These choices motivate our
  resampling and HAC specifications. Our centered test below differs from
  their sign-fraction calculation. Applying the same bootstrap to mean returns
  and CER is our extension.
- Moreira and Muir report spanning alpha and appraisal ratios with
  heteroskedasticity-robust regression inference. Their regression inference
  is not a paired Sharpe bootstrap. We use the serial-correlation-robust HAC
  specification consistently instead of adding another covariance estimator.

## Performance metrics and resampling

All inputs are monthly decimal **net excess returns**. For a sample of monthly
returns `r`, annualized mean is `12 * mean(r)`, Sharpe is
`sqrt(12) * mean(r) / sd(r)`, and CER is
`12 * (mean(r) - 5 / 2 * var(r))`. The variance is the sample variance with
denominator `n - 1`. Net-return variance is used throughout. The separate
paper-style net-mean/gross-risk statistics are not mixed into these tests.

Let the observed statistic difference be `d = statistic(strategy) -
statistic(benchmark)`, and let `d_star` be its paired bootstrap analogue.
The primary null is `d_population <= 0` against `d_population > 0`.
The one-sided centered-error p-value is:

```r
(1 + sum(d_star - d >= d)) / (draws + 1)
```

Centering imposes the boundary null for the bootstrap error distribution.
This is not the uncentered frequency `mean(d_star <= 0)`. We also report
two-sided pointwise 95% percentile intervals. These intervals are not
multiplicity-adjusted and are not obtained by inverting the one-sided tests.
Bootstrap validity relies on a sufficiently stable weakly dependent return
process. It does not remove structural-break concerns in a 162-month sample.

There are 10,000 draws, seed 20260918, and an expected geometric block length
of five months. Three- and ten-month blocks are sensitivity analyses. The
generator uses circular blocks, preserves the caller's random-number state,
and fixes the RNG and discrete-sampling algorithms. Exact index matrices are
saved so that the experiment is reproducible.

Every strategy and factor uses identical month indices. The single-factor
aggregate first equal-weights the 153 nonmarket factor returns in each month.
Resampling this series is equivalent to applying the same month draw to all
its constituent factors. The multifactor portfolios retain their saved 154-factor
opportunity set, including the market. Factor-level means, medians, interquartile
ranges, and fractions improved are descriptive. Neither the 153 factors nor
factor-months are treated as independent observations for significance tests.

## Prespecified contrasts and multiplicity

The EW forecast ensemble is compared with unmanaged, RW, HAR, and HARX in
MM and the fixed 50/50 single-factor combination. Signed two-sleeve MVO is
compared with its own unmanaged-only MVO and with the RW/HAR/HARX two-sleeve
strategies. The unmanaged MVO comparator is selected from the same saved file
as the EW-forecast strategy, retaining its calibration sample.

CMV and FCMV are compared with the corresponding allocator's UMV, HAR, and
HARX. CMV additionally uses the available lagged-market-volatility comparator.
There is no saved own-factor RW FCMV comparator. This report does not create
one or relabel lagged-market CMV as FCMV.

Holm adjustment is applied separately to four families, jointly across all
three endpoints, raw/capped versions, scopes and benchmarks in that family:

| Family | Comparisons | Endpoint tests |
|---|---:|---:|
| Primary single factor: MM and 50/50, aggregate and pooled market | 32 | 96 |
| Primary multifactor: CMV/FCMV under EW and HEW | 28 | 84 |
| Signed Cederburg: aggregate and pooled market | 16 | 48 |
| Standalone-market appendix, including standalone-market CMV | 40 | 120 |

Holm controls familywise error under arbitrary dependence when the marginal
p-values are valid. It does not assume independent strategies. Undefined
endpoints remain in the adjustment's family size. Raw p-values, adjusted
p-values and effect sizes are all retained. Adjustment is repeated within each
block-length sensitivity, which must not be searched for favorable results.
The three broad subperiods are descriptive, not additional inference families.

## Spanning alpha

Regress strategy excess return on an intercept and benchmark excess return.
Report annualized alpha, its HAC standard error, t-statistic, normal-reference
two-sided p-value, beta, R-squared and annualized appraisal ratio. The latter
uses monthly alpha divided by residual RMSE and multiplied by `sqrt(12)`.
RMSE has denominator `n`. The Bartlett HAC estimate uses lag one, no
prewhitening, and the `n / (n - 2)` finite-sample adjustment. Lags five and ten
are reported as robustness. Alpha describes an ex-post opportunity set and
does not establish an implementable MVO improvement.

## Validation and outputs

Inputs must have finite returns, unique series-month keys, complete common
dates, and total returns above -100%. Total return is net excess return plus
the saved risk-free collateral return. Net returns must match gross returns
less saved costs. Benchmark copies are checked by economic keys before
deduplication. MM and Cederburg current scalers are compared by factor and
holding month. Upstream hashes and the cached seven-member log-forecast
averaging identity are checked without modifying the cache.

Identical finite-metric strategies have zero differences, zero-width
intervals and p-value one. Zero-variance Sharpe is undefined. If any sampled
Sharpe is undefined, the helper reports that status rather than discarding
draws. Relative machine-precision tolerance identifies numerically constant
series. Perfect spanning fits have undefined t-statistics and appraisal
ratios. Missing periods, insolvency, and invalid dates fail explicitly.

The report writes statistics, contrasts, bootstrap inference, spanning
regressions, factor/theme descriptions, source hashes, effective settings,
calibration checks, index matrices and session information to
`latex_paper/results_v3/1986/data/portfolio_inference_*`. LaTeX tables use
the existing paper formatting helpers. The independent
`portfolio_inference_preview.tex` includes only the new inference tables,
avoiding accidental mixing with older descriptive portfolio outputs.

Costs remain conditional on the saved 10-bp factor-exposure proxy, including
entry from zero and no terminal liquidation. They exclude underlying-stock
turnover, stock-level netting, financing and shorting costs. Inference is
conditional on the fitted out-of-sample strategies and does not resample or
refit the full learning and allocation pipeline.
