# Allocation experiments: Cederburg, UMV, CMV and FCMV

## Workflow

1. Run `R/oos_ports.R` to produce the individual-factor MM portfolios.
2. Run `R/allocation_ports.R` to read those saved paths and allocate across sleeves.

The allocation script never refits a predictive model or reconstructs a different
EW forecast. It verifies the model/data hashes in the saved MM manifests, checks
common dates and factor returns, and records the source RDS hashes. Each sample
is separate. The defaults run 1986; adding 1927 requires completed panel and
standalone-market MM artifacts for that sample. No substitution across samples
or estimation universes is allowed.

Set `models_to_run <- "ew"` for a smaller complete comparison; the default lists
all nine forecast methods. Results are saved separately for every specification
in `ports/v3/<sample>/allocations/<mm_run>/`, alongside a combined `summary.csv`,
settings, input manifest and session information. The old individual-factor
reporting schema is unchanged. Existing paper tables do not yet consume these
new allocation results.

## Cederburg

`run_cederburg_ports()` reads the factor's own `vol_scaled` path (or `rw` when
explicitly selected). It builds two excess-return sleeves: the original factor
and its own-variance-managed version. It estimates signed MVO weights using the
full sample covariance and mean on strictly prior months (equation 15 of the
attached March 2019 manuscript). Risk aversion is five. The final exposure is
`a + b * scaler`, with a separate capped version clipped to [-5, 5]. This is the
paper's equation (16), the real-time counterpart of equation (11).

Equal variances are **not** required by equations (7), (11), (15) or (16).
They enter the simplification in equations (9)-(10). Thus the general covariance
implementation works with `expanding_rv`. Two explicit versions are saved:

- `training_vol`: at each formation month calculate one constant
  `c_t = sd(f_past) / sd(f_past / own_variance_signal_past)` and apply it to all
  managed training returns and the current exposure. This reproduces the
  calibration procedure in section 4.3.1, applied to the project's signals.
- `saved`: reuse the saved historical MM `w_raw` series, including its original
  time-varying normalization. This is the requested existing-MM-path extension,
  not an exact reproduction of the paper's window-specific rescaling.

Each version contains signed MVO, 50/50 EW, and an original-factor-only MVO
benchmark on identical dates. The latter estimates its exposure from the same
past sample and receives the same cap. As requested, no second robust rule is
invented for two sleeves: hierarchical EW would equal ordinary EW here.

MVO estimates gross-return moments, as in the paper. Costs are subsequently
charged on changes in the **combined** factor exposure; opposing trades in two
sleeves of the same factor net before costs. Entry from zero is charged. There
is no forced terminal liquidation. The general covariance solver rejects
singular or poorly conditioned two-sleeve problems rather than silently adding
a ridge or taking a pseudoinverse. Gross and net returns and raw/capped costs
are retained. Reported Sharpe and CER use net excess returns.

Using forecast signals, the existing V3 variance definition, compounded monthly
factor returns and assumed factor-exposure costs remains an adaptation of
Cederburg, not a full data/transaction-cost replication. The original variance
target is not silently changed to a demeaned or daily-count-adjusted measure.

## Multifactor specifications

`run_multifactor_ports()` reuses the same optimizer and extends its scaler from
one monthly market signal to an explicitly keyed factor-by-month signal matrix:

| Specification | Return basis | Signal source |
| --- | --- | --- |
| UMV | Unmanaged factors only | None |
| CMV-panel | Unmanaged and market-volatility-managed factors | Market forecast from the pooled-panel model |
| CMV-spx | Unmanaged and market-volatility-managed factors | Standalone market model |
| FCMV | Unmanaged and own-volatility-managed factors | Each factor's own pooled-panel forecast |

All three conditional versions use inverse **volatility**, as in DeMiguel;
Cederburg instead combines the saved inverse-**variance** MM sleeves. The market
signal broadcasts across all factors in CMV. Each FCMV signal joins on both
factor name and date. A missing, duplicated or misaligned signal fails explicitly.
UMV is repeated in each saved specification for convenient paired comparisons;
for a given allocation rule its returns must be identical across signal sources.

MVO uses the nonnegative coefficients and gross variance objective from DeMiguel
equation (6). Expected incremental factor-exposure costs enter through the
historical average absolute change in **each factor's** scaler. Actual costs
include subsequent coefficient-refit trades as well as scaler changes. The
training proxy does not anticipate those future refit costs. Raw and
proportionally capped exposures retain separate costs. The cap is on the sum of
factor exposures, not stock-level gross leverage.

The two robust rules require no estimated expected-return vector:

- **EW:** `1/K` in each unmanaged sleeve for UMV; `1/(2K)` in every unmanaged
  and managed sleeve for CMV and FCMV.
- **Hierarchical EW:** equal capital per cluster, equal capital per factor within
  each cluster, then equal capital per sleeve of that factor. Cluster labels come
  from `data/cluster_labels.csv`; the market is an explicit separate cluster.
  A larger cluster therefore does not automatically receive more capital.

For robust allocations, the managed sleeve is first matched to its own unmanaged
factor's volatility **within the strictly prior training window**, using a single
constant for that window and the current month. Without this convention,
equal-weighting `f` and `f / sigma` would mostly be a choice of volatility units.
This normalization is not applied across different unmanaged factors: EW remains
equal capital, not inverse-volatility across factors. `managed_normalization`
records the choice, and weights include both the normalizer and final scaler.
MVO retains the prior unnormalized basis by default; a constant positive basis
rescaling does not change its attainable portfolios or objective when costs are
rescaled consistently. No future or full-sample volatility estimate is used.

## Timing, costs and practical limits

All allocation methods start after at least 120 prior common saved OOS months.
The default 1986 forecasts therefore produce allocation returns from July 2016.
EW and HEW use the same evaluation dates as MVO even though they need fewer
estimated parameters. Every monthly coefficient is fitted before that month's
return is observed. Both the pooled and standalone forecasts must cover the
same calendar. Changes to future returns/signals cannot alter earlier weights.

All costs remain incremental factor-exposure proxies. We lack the underlying
stock holdings, returns, trading costs and financing terms needed to reproduce
DeMiguel's stock-trade netting benefits. Raw unconstrained MVO can imply very
large factor exposures; insolvency months are reported and no wealth curve is
compounded through them. Caps are separately labelled implementation diagnostics.
No p-values or superiority claims follow from these descriptive comparisons.

Tests cover covariance-based allocation with unequal variances, invariance to
basis units, hierarchical budgets, equivalence of common and own signals when
they coincide, exact saved-scaler reuse, signed exposure caps, missing keys,
normalization and temporal integrity. Existing MM and multifactor regression
tests continue to run.
