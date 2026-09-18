# Allocation experiments: Cederburg, UMV, CMV and FCMV

## Workflow

1. Run `R/oos_ports.R` to produce the individual-factor MM portfolios.
2. Run `R/allocation_ports.R` to read those saved paths and allocate across sleeves.

The allocation script never refits a predictive model or reconstructs a different
EW forecast. It verifies the model/data hashes in the saved MM manifests, checks
common dates and factor returns, warns on upstream code mismatches, and records
the source RDS hashes. Each sample
is separate. The defaults run 1986; adding 1927 requires completed panel and
standalone-market MM artifacts for that sample. No substitution across samples
or estimation universes is allowed.

Set `models_to_run <- "ew"` for a smaller complete comparison; the default lists
all nine forecast methods. Results are saved separately for every specification
in `ports/v3/<sample>/allocations/<mm_run>/`, alongside a combined `summary.csv`,
settings, input manifest and session information. Both entry scripts save an
`*_executed.R` snapshot with effective configuration replacing file defaults;
the manifest hashes this snapshot and settings.rds. Existing paper tables do not yet consume these
new allocation results.

## Cederburg

`run_cederburg_ports()` reads the factor's own `vol_scaled` or `rw` path. The
entry script runs BOTH methods, records `managed_method` in summaries, and
includes it in every filename. RW is the lagged-own-variance comparator; it
is distinct from the common lagged-market-volatility multifactor signal.
It builds two excess-return sleeves: the original factor
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
| Lagged-market CMV | Unmanaged and market-volatility-managed factors | Square root of saved lagged market RV |

All four conditional versions use inverse **volatility**, as in DeMiguel;
Cederburg instead combines the saved inverse-**variance** MM sleeves. The market
signal broadcasts across all factors in CMV. Each FCMV signal joins on both
factor name and date. A missing, duplicated or misaligned signal fails explicitly.
UMV is repeated in each saved specification for convenient paired comparisons;
for a given allocation rule its returns must be identical across signal sources.

The saved-RV baseline is explicitly an adaptation, not the paper's exact daily
SD benchmark: `sum(r_d^2) = (n - 1) * sd(r_d)^2 + n * mean(r_d)^2`. Thus sqrt(22)
is not a general conversion. The exact daily-SD helper and test are retained,
without introducing raw CRSP inputs into this second-stage script.

MVO uses the nonnegative coefficients and gross variance objective from DeMiguel
equation (6). Expected incremental factor-exposure costs enter through the
historical average absolute change in **each factor's** scaler. Actual costs
include subsequent coefficient-refit trades as well as scaler changes. The
training proxy does not anticipate those future refit costs. Raw and
proportionally capped exposures retain separate costs. The cap is on the sum of
factor exposures, not stock-level gross leverage.

MVO now imposes the bound INSIDE optimization. At formation, UMV satisfies
`sum(a) <= 1.5`; CMV/FCMV satisfies `sum(a + b * current_scaler) <=
min(1.5, 1.2 * sum(a_UMV))`. The current signal is known before the payoff.
If UMV chooses cash, this relative-exposure rule also forces CMV/FCMV to cash.
The relative rule follows the motivation of DeMiguel's IA XIII sensitivity
check, but factor-exposure sums cannot reproduce stock leverage without holdings.
The absolute bound is an explicit implementation choice for this 154-factor panel.
SLSQP solves the bounded convex problem without a covariance inverse or ridge.
The feasible set is compact even for singular covariance; coefficients may still
be nonunique. Rank and rcond describe the RAW sample covariance and are separate
from numerical KKT convergence. Feasibility and KKT are checked after documented
round-off cleanup; other solver failures abort.

`mvo_exposure_limit = NULL` explicitly requests the legacy unbounded comparison.
The `raw` version denotes the solver output, now constrained by default.
`capped` retains proportional rescaling for compatibility and robust portfolios;
it is redundant for default MVO. Summary `implementation`, `exposure_raw`, and
`exposure_capped` distinguish the implementation and mean exposure levels.

EW and HEW are the primary robust comparisons; MVO is retained for comparison.
DeMiguel, Garlappi and Uppal (2009), *Optimal Versus Naive Diversification*,
https://doi.org/10.1093/rfs/hhm075, motivate using 1/N as an OOS benchmark against
estimation-sensitive optimized portfolios. This is not evidence that our HEW
extension must outperform. Neither robust rule requires an expected-return vector:

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

Multifactor EW and HEW start after 60 prior common saved OOS months. This gives
the expanding managed/unmanaged volatility normalizer five years of history
without requiring MVO's ten-year moment-estimation window. Multifactor MVO and
all Cederburg allocations retain 120 prior months. Comparisons with MVO must
use their common evaluation dates. Every monthly coefficient is fitted before
that month's return is observed. Both the pooled and standalone forecasts must
cover the same calendar. Changes to future returns/signals cannot alter earlier
weights.

The saved expanding_vol run currently has only 102 months, fewer than the 120
needed for MVO and Cederburg training. Keep expanding_rv as the source for this
stage. On IDENTICAL dates/history, multifactor allocations and Cederburg training_vol
depend only on signal_rv, factor_exc and rf, not saved MM w_raw. Only Cederburg
`saved` inherits that normalization. Matching overlapping inputs does not justify
changing the training window or silently reducing the selected min_history.

All costs remain incremental factor-exposure proxies. We lack the underlying
stock holdings, returns, trading costs and financing terms needed to reproduce
DeMiguel's stock-trade netting benefits. Raw unconstrained MVO can imply very
large factor exposures; insolvency months are reported and multifactor Sharpe
and CER are suppressed if any occur. No wealth curve is compounded through them.
Exposure bounds do not guarantee solvency against arbitrary factor losses.
No p-values or superiority claims follow from these descriptive comparisons.

Tests cover covariance-based allocation with unequal variances, invariance to
basis units, hierarchical budgets, equivalence of common and own signals when
they coincide, exact saved-scaler reuse, signed exposure caps, missing keys,
normalization and temporal integrity. Existing MM and multifactor regression
tests continue to run.
