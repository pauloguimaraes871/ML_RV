# DeMiguel, Martin-Utrera and Uppal (2024) replication and ML extension

Reference: V. DeMiguel, A. Martin-Utrera and R. Uppal, "A Multifactor Perspective
on Volatility-Managed Portfolios", *Journal of Finance* 79(6), 2024,
https://doi.org/10.1111/jofi.13395.

The JKP factor panel is replaced by the paper's nine factors and the existing V3
pipeline (forecasts, MM portfolios, Cederburg, multifactor allocations) is rerun
on them. A separate replication path, without ML, will reproduce Tables I and
III (not yet implemented; see Section 8).

## 1. Data (`R/load_demiguel_data.R`, `R/demiguel_helpers.R`)

| Key | Paper factor | Source file (in `data/demiguel/raw/`) |
| --- | --- | --- |
| `mkt` | MKT | French `F-F_Research_Data_Factors` (Mkt-RF) |
| `smb` | SMB | French three-factor file |
| `hml` | HML | French three-factor file |
| `rmw`, `cma` | RMW, CMA | French `F-F_Research_Data_5_Factors_2x3` |
| `umd` | UMD | French `F-F_Momentum_Factor` |
| `roe`, `ia` | ROE, IA | Hou-Xue-Zhang `q5_factors_*_2025.csv` |
| `bab` | BAB | AQR *Betting Against Beta Equity Factors*, USA column |

Downloaded 2026-10-06 (French CRSP vintage 202608, q5 vintage 2025, AQR as of
July 2026). SHA-256 hashes are in `data/manifest_demiguel_v3.csv`; the daily AQR
workbook (31 MB) is untracked, like the raw JKP file.

**Source choices follow the paper, checked against Table I (Panel A, SR(r_k)).**
The Appendix defines SMB as three small minus three big book-to-market
portfolios, i.e. the three-factor SMB, which is also the closer match
(.221 vs .235 for the 2x3 SMB; paper .208). HML is identical in both French
files; MKT has the same 1977-2020 Sharpe in both. All nine factors match within
the pre-set tolerance of 0.03 (`data/demiguel/table1_sharpe_check.csv`):

| | MKT | SMB | HML | RMW | CMA | UMD | ROE | IA | BAB |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| Paper | .530 | .208 | .170 | .506 | .399 | .474 | .722 | .508 | .880 |
| Ours | .527 | .221 | .193 | .489 | .411 | .475 | .709 | .516 | .886 |

Remaining gaps reflect provider revisions since the paper's vintage.

### Monthly returns versus daily variance (decision 3)

Portfolios earn the providers' **native monthly** returns. **Daily** provider
files are used only to measure variance: the ML targets and HAR features (monthly
sum of squared daily returns, as for JKP) and the replication signals (sample
variance/SD of daily returns, as in the paper). This mirrors the paper, which
downloads monthly factors and estimates sigma from daily returns. Compounded
daily returns are not substituted for monthly returns: they correlate above
0.995 with the monthly series for eight factors, but only 0.93 for UMD because
French's daily momentum factor is rebalanced daily.

All nine factors, MKT included, are excess returns; RF (French, same vintage) is
kept only as collateral and is never subtracted again. `mkt` is French Mkt-RF,
not the CRSP `vwretd` series stored as `spx_vw` in the JKP panel; `spx_vw` is not
pooled into this panel.

### Samples and entry rule (decision 2)

Both samples use the JKP inclusion rule: each key drops its first two months and
then needs a complete HAR block, so factors enter with their own provider
history. Long sample = 1927 covariate tier. Keys enter the covariate panel in
1927-01 (MKT, SMB, HML, UMD), 1931-02 (BAB), 1963-09 (RMW, CMA) and 1967-03
(ROE, IA); these are covariate-panel dates, not OOS forecast dates (Section 6).
Short sample = 1986 tier (all keys from February 1986). Sample ids remain `1927`/`1986` so the runners' switches are
unchanged. The shared macro, implied-volatility and calendar covariates are the
saved `covariates_<tier>v2.rds` artifacts of `load_data.R`, verified equal to the
values in the JKP panel, so the two universes differ only in their factors.

## 2. Replication choices

- Optimization mirrors the paper: nonnegative eta, gamma = 5, a 120-month
  expanding window from January 1967, OOS January 1977 - December 2020, signal =
  lagged sample SD of daily market returns, **no exposure cap** (the paper's 20%
  leverage limit is only a robustness check, IA XIII).
- **JKP production differs on purpose**: it caps exposure at 1.5 and at 1.2x UMV
  inside the optimizer, uses 60 months of history and the square root of lagged
  sum-of-squares RV. Do not compare the two without stating this.
- Table III Panels A and B (no costs) are exact targets. Panel C (costs without
  trading diversification) is approximated at the factor level (below). Panel D
  needs stock-level netting and cannot be replicated. CFW is deferred.

### Factor-level transaction costs (decision 4)

Equation (11) prices each factor's trade stock by stock. Splitting the trade,

    theta_{t+1} x_{t+1} - theta_t x_t^+ = theta_{t+1}(x_{t+1} - x_t^+) + (theta_{t+1} - theta_t) x_t^+,

gives per factor `kappa_reb * |theta_{t+1}| + kappa_unit * |theta_{t+1} - theta_t|`,
an upper bound by the triangle inequality. `kappa_reb` is the factor's own
rebalancing cost published by the paper (Figure 2B and p. 3870). `kappa_unit`,
the cost of changing exposure by one unit, is not published and needs stock data
(no WRDS access); it is calibrated from Figure 5 (managed-factor gross versus net
returns ignoring trading diversification). With eta >= 0 the in-sample cost is
linear in eta, so "optimized accounting for TC" (equation 6) uses the existing
linear cost vector of the optimizer.

## 3. Naming (decision 6)

Existing keys (`umv`, `cmv`, `fcmv`, `cmv_panel`, ...) are kept so saved JKP
artifacts stay valid. DeMiguel portfolios get their own keys. At the final table
step labels change: old EW/HEW "CMV/UMV/FCMV" become CEW/UEW/FEW (CHEW/UHEW/FHEW)
and the DeMiguel mean-variance portfolios become CMV/UMV/FCMV.

## 4. Open items

- HEW is not used for the nine factors: each factor is its own cluster, so HEW
  equals EW (decision 7).
- FCMV with own-factor lagged RV is added for the DeMiguel dataset only. **JKP
  allocations still lack it and must be rerun** to include it (decision 9).

## 5. Step 2 review decisions (owner instruction, 2026-10-11)

Step 2 means the dataset switch and ML model fits. It does not mean the later
Table I replication (Step 3), portfolio/cost changes, or Table III replication.
The owner supplied Claude's independent review in the conversation and asked
Codex to record the following decisions and request Step 2 closeout.

- Accept the stored OOS forecasts and comparisons as **trusted with
  qualifications**, following the successful artifact audits and Claude's
  independent review. This does not establish bit-exact replay or successful
  reloading of serialized neural networks.
- Keep the current DeMiguel runner defaults intentionally for this phase:
  `dataset = "demiguel"`, `run_1986 = TRUE`, `run_1927 = TRUE`,
  `run_panel = TRUE`, `run_spx_only = FALSE`, eight models, `range`
  preprocessing, and `grid_search` (HAR/HARX remain untuned).
- Preserve the existing estimation engine, split/tuning/seed logic and JKP
  signatures. No refitting is indicated by the review.
- Record JKP reproduction settings below. Do not silently change the
  production runner's defaults while checking JKP.
- Keep provenance hardening in a separately scoped change. Unsigned descriptive
  code/environment metadata improves traceability but does **not** prevent
  false cache hits. Prevention requires an explicitly designed cache-validation
  policy using relevant code/environment fingerprints, with compatibility and
  migration considered before implementation.
- Distinguish provider/covariate entry dates from actual OOS forecast entry
  dates. Do not use provider history to describe the OOS panel composition.
- Interpret pooled JKP-versus-DeMiguel scores as different prediction panels.
  Lower RMSE across panels is not evidence of universally better forecasting;
  comparisons against HAR within each panel are more informative.

### Safe JKP reproduction check

For the complete JKP regression check, override the following top-level
settings **in memory**, without editing the production script:

    dataset <- "jkp"
    run_1986 <- TRUE
    run_1927 <- TRUE
    run_panel <- TRUE
    run_spx_only <- TRUE
    models_to_run <- c("har", "harx", "glmnet", "rf", "xgb", "nn1", "nn2", "nn3")
    preprocessing_to_run <- "range"
    tuning_methods_to_run <- "grid_search"
    dry_run <- TRUE

Claude reported **32/32 cache hits** with this configuration. Recheck it before
closeout. The DeMiguel dry run should give **16/16 cache hits** with standalone
market disabled. A missing dataset, unexpected cache miss, hash mismatch, or
coverage mismatch stops closeout; do not launch a fit or relax a check.

## 6. Implemented Step 2 scope and verified fit state

At registration, branch is `feat/demiguel-replication`, HEAD `775fb81`.
The production-code diff remains the original three uncommitted Step 2 files:

- `R/helpers.R`: `build_run_matrix(..., panel_universe = "panel")` supports
  `"panel"` and `"demiguel"` while keeping the existing default.
- `R/fit_models.R`: dataset specifications route the target, covariates, model
  outputs and inclusion-policy text; pooled panels retain `key_col = "name"`;
  standalone market is restricted to JKP. The JKP inclusion-policy string
  participates in run signatures and must remain unchanged.
- `tests/testthat/test-dados_gold.R`: default-equality/universe-renaming and
  invalid-universe tests for the run matrix.

The core estimation engine was not modified by Codex. Codex created temporary
execution harnesses under `tmp/`, fitted missing artifacts, and audited saved
results. This documentation update is an additional closeout change; it is
not part of Claude's earlier three-file code review.

All eight models (HAR, HARX, glmnet, RF, XGB, NN1, NN2, NN3) are saved for both
samples under `models_v3/demiguel/<sample>/`.

- The 8 October run saved NN2 for 1927, then terminated during NN3 refitting.
  Its `RUNNING` marker is stale; no completed NN3 artifact was saved by that run.
  The logs do not establish the termination cause.
- The 10 October run reused 15 cached models, fitted only NN3 for 1927, and
  completed its final audit on **2026-10-10 at 20:40 Sao Paulo time**.
- Its audit called `read_model_result()` on all 16 saved artifacts and verified
  identical observation keys and targets across models within each sample.
- Recorded inputs and the 15 existing model files were unchanged during that
  final run. Codex independently rehashed all 16 current artifacts and all
  recorded final-run inputs on 2026-10-11; they still match the final manifests.

| Sample | Observations per model | Distinct OOS months | First OOS month | Last OOS month |
| --- | ---: | ---: | --- | --- |
| 1927 | 7,371 | 931 | 1947-06 | 2024-12 |
| 1986 | 1,998 | 222 | 2006-07 | 2024-12 |

The long panel is unbalanced. Codex's per-factor audit established these OOS
entry dates (different from provider/covariate history in Section 1):

| Keys | First OOS month | Observations per key |
| --- | --- | ---: |
| `mkt`, `smb`, `hml`, `umd`, `bab` | 1947-06 | 931 |
| `rmw`, `cma` | 1966-09 | 700 |
| `roe`, `ia` | 1970-03 | 658 |

All nine short-sample keys have 222 observations from 2006-07 to 2024-12.

Local execution evidence, currently gitignored:

- `tmp/demiguel_resume_20261008/`: first harness, logs, launch record, inputs and
  14-existing-model hash manifest. Preserve this evidence.
- `tmp/demiguel_resume_20261010/`: successful NN3-only harness, preflight logs,
  run logs, launch record, inputs, 15-existing-model hash manifest, final
  `status.txt`, and `audit_models.csv` for all 16 artifacts.

## 7. Claude review evidence and remaining qualifications

The following are findings **reported by Claude in the owner-supplied review**,
not additional tests run by Codex during this documentation update:

- Independent audit of 32 panel artifacts (16 JKP, 16 DeMiguel); method metadata,
  feature vectors/order and forecast/rebalance calendars match. Seven
  provenance fields differ as expected for the dataset substitution.
- No detected look-ahead in feature lags, truncated preprocessing checks or
  daily-return target construction. Equivalence alone does not rule out a
  shared upstream data-vintage or economic-availability defect.
- Every DeMiguel NN artifact contains 10 serialized ensemble members. The three
  JKP 1986 NN artifacts contain zero serialized members; Claude traced this
  to fits predating serialization fixes. Their stored forecasts/metrics remain
  auditable, but reloading those historical fitted ensembles is not established.
- JKP regression dry run: 32/32 hits. Data tests: 17 tests, 54 expectations,
  zero failures. Run-matrix tests: 23 expectations, zero failures.
- JKP published report metrics agree with the artifacts to about `4e-16`.
  EW ensembles were excluded from this comparison.
- Native and common-calendar metric comparisons coincide: the two panels use
  the same OOS month calendars, but different cross-sections and entry dates.
- RF has the lowest long-sample DeMiguel RMSE; NN1 is worse than HAR in both
  DeMiguel samples. Most DeMiguel pooled RMSEs are lower than their JKP
  counterparts, without establishing greater forecasting skill across panels.
- The `summary.lm()` near-perfect-fit warning was traced to the auxiliary
  surrogate of linear-model predictions, rather than the forecasts themselves.

Unverified: bit-exact fit reproduction, actual reloading/prediction from the
serialized networks, and historical working-tree/package environments.
The current provenance does not establish historical code or installed package
versions. Do not claim that a descriptive metadata addition retroactively
recovers those facts.

Resolved at closeout: the JKP artifacts and their signed covariate files contain
154 keys = 153 JKP factors plus `spx_vw`; the runner comment was corrected.
Claude's review scripts, outputs (including `metrics_comparison.csv`) and
dry-run logs are preserved, hash-verified, in `tmp/demiguel_review_20261011/`
(gitignored), alongside the fit evidence.

## 8. Step 2 closeout still required

The implementation and model fitting are complete. Claude should now:

1. Re-read the original handoff, this decision record and its own review;
   inspect the current branch, status, commits and exact diff.
2. Review this newly added documentation, including the intentional defaults,
   JKP dry-run settings, corrected OOS entry dates and evidence attribution.
3. Recheck final artifact/input hashes and audit status, 16/16 DeMiguel and
   32/32 JKP dry-run hits, targeted data/run-matrix tests, and the important
   metadata/coverage equivalence claims. Stop on discrepancies.
4. Confirm the recorded serialization limitation and warning attribution against
   the actual artifacts; leave network reloading unverified unless separately
   tested with approval. Do not refit the historical JKP NNs.
5. Ensure the review/metric comparison evidence is available and correct any
   report wording that confuses provider history with OOS coverage.
6. Present the final diff, validation summary and proposed commit for the
   owner's explicit approval. Proposed title:
   `feat: add dataset switch to fit the DeMiguel factor panel`.

The anticipated source/documentation scope is now **four** files:
`R/fit_models.R`, `R/helpers.R`, `tests/testthat/test-dados_gold.R`, and
`docs/DEMIGUEL_REPLICATION.md`. Do not stage `data/cluster_labels.csv`,
gitignored models, `tmp/`, or unrelated files. No commit, push, merge or PR action
has been authorized by the decision-registration request. Obtain the exact
owner-required co-author line before committing; do not invent it.

Provenance/cache redesign and a new inclusion-policy-string test are not
implemented by this closeout and require their own approved scope. Table I/III,
transaction-cost calibration, portfolio dataset switches, inference/reporting
changes, and the missing JKP own-factor lagged-RV FCMV remain later steps.

### Closeout result (2026-10-11)

Rechecked by Claude: 32/32 artifacts unchanged since the review; input manifests
8/8, 14/14, 15/15; DeMiguel dry run 16/16 and JKP dry run (Section 5 settings,
in memory) 32/32 cache hits; data tests 17/54/0 failures; run-matrix 23/0
failures; OOS entry dates in Section 6 confirmed; serialization counts and the
surrogate-warning attribution confirmed against the artifacts. Still unverified:
bit-exact refits, reloading serialized networks, historical working tree and
package versions.

## 9. Step 3: Table I, Panels A and C (`R/demiguel_replication.R`)

### Specification, taken from the authors' code

The authors' replication package (Mendeley Data, DOI 10.17632/dmjfztbc4y.3,
CC BY 4.0; `ReplicationPackage.zip`, SHA-256
`e2dc5e677004141a25cdce575c853bb2ce415c00cfc3e989a0f9a25680dbb5a7`, 457 MB) was
downloaded on 2026-10-11 to `C:\Users\paulo.fonteles\Downloads\DMU_replication\`,
outside the repository. Table I comes from `JF_Table1.m`, `VMPnew_TC_FFHXZBAB.m`
(in-sample) and `VMP_OOS_noshortconst_FFHXZBAB.m` (OOS). Their code settles the
choices the paper leaves open:

- **Weights are signed and unconstrained**: b = (1/gamma) Sigma^-1 mu, gamma = 5.
  Nonnegativity applies only to the multifactor portfolios.
- **The window is February 1977 - December 2020 (527 months)**, both for the
  in-sample estimation/evaluation and the OOS period. The data start in
  February 1967 because the first month is consumed by the variance lag; with
  M = 120 the evaluation starts at row M + 1. The paper rounds this to
  "January 1977".
- In-sample weights are rescaled to b/|b2|, falling back to the unmanaged factor
  if |b2| <= 0.001. OOS weights use an expanding window from the first sample
  month and are neither rescaled nor capped.
- The managed factor is r_{t+1} c / sigma2_t, where sigma2_t is the **sample
  variance (N - 1) of daily returns in month t** (their `VolatilityF`; our
  formula reproduces it to 7e-16 on their daily data). c = sd(r)/sd(r/sigma2)
  over the sample; returns and Sharpe ratios do not depend on it.
- p-values: 10,000 stationary-bootstrap draws of the realized return pair,
  average block 5, no re-estimation; p = fraction of draws in which the managed
  Sharpe ratio does not exceed the unmanaged one (uncentred). Their MATLAB random
  stream cannot be reproduced, so our p-values carry Monte Carlo error.

### Implementation check on the authors' data

Run on the package's own factor returns and daily returns (scratch check, not
part of the pipeline), our helpers reproduce **all 27 Panel A/C Sharpe ratios to
the third decimal (maximum difference 0.000)**; p-values differ by at most 0.017
(bootstrap noise). Scripts and the CSV export are kept in
`tmp/demiguel_table1_validation_20261011/` (gitignored).

### Result on our provider data (2025 / 202608 vintage)

`data/demiguel/table1_panels_ac.csv` and `table1_panels_ac_vs_paper.csv`.
Sharpe ratios are within 0.015 of the paper; p-values within 0.081. The same
code matches the paper exactly on the authors' data, so these gaps are data
revisions, not implementation differences. One inference changes at 5%: the OOS
p-value of IA falls from .062 to .038.

| | MKT | SMB | HML | RMW | CMA | UMD | ROE | IA | BAB |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| A: SR(r_k, r^sigma) paper | .585 | .246 | .215 | .739 | .419 | 1.088 | 1.153 | .621 | 1.397 |
| A: ours | .589 | .240 | .205 | .751 | .422 | 1.078 | 1.155 | .633 | 1.393 |
| C: SR(r_k, r^sigma) paper | .408 | .068 | .194 | .527 | .355 | 1.035 | 1.094 | .605 | 1.321 |
| C: ours | .412 | .068 | .190 | .539 | .362 | 1.022 | 1.098 | .619 | 1.318 |

Note: the Step 1 check in Section 1 uses January 1977 - December 2020 (528
months); Table I itself uses February 1977 (527 months), hence small differences
in SR(r_k) between the two (e.g. MKT .527 vs .534).

### Not yet done (Panels B, D, E)

The package also contains the stock-level inputs behind the costs
(`MainDataRep.mat`: stock weights `RPS`, half-spreads `BidAskSpreads`, stock
returns `rRisky`), with noise added by the authors to protect proprietary data.
Their Panel D cost is |a| times the unmanaged sleeve's stock-level rebalancing
cost plus |b| times the managed sleeve's, averaged over time. Whether to derive
per-factor cost series from these data, instead of the Figure 5 calibration
proposed in Section 2, is an open owner decision. Panel E (trading
diversification) needs the same stock-level netting.

## 10. Stock-level transaction costs (`R/load_demiguel_costs.R`, `R/demiguel_cost_helpers.R`)

Owner decision (2026-10-11): use the authors' stock-level cost data instead of
the Figure 5 calibration of Section 2. Kept inside OneDrive, gitignored, with
hashes in `data/manifest_demiguel_costs_v3.csv`. New dependency: `hdf5r` 1.3.12
(reads the MATLAB 7.3 file; recorded alone in `renv.lock`).

**What the data are.** `MainDataRep.mat` holds, for February 1967 - December
2020 (647 months, 24,991 stock slots): the stock weights of the authors' own
CRSP rebuild of each of the nine factors (`RPS`; long legs sum to +1, short legs
to -1, BAB legs scaled by 1/beta), each stock's cost per dollar traded kappa
(`BidAskSpreads`, Abdi-Ranaldo), and stock returns (`rRisky`). The rebuilt
factor returns correlate 0.91-1.00 with the provider returns, as in the paper's
footnote 5. The authors added noise to protect proprietary data; at factor level
it is small (Figure 2B within 0.4 bp, below).

**How it is used.** As in the paper: returns come from the providers; costs come
from pricing the stock trades implied by a path of factor exposures. Any of our
portfolios, including the ML-managed ones, is a path w_{j,t} on factor sleeves
(theta for unmanaged sleeves, theta x signal for managed ones), so the same
engine prices them, with three netting levels: `sleeve` (no trading
diversification), `factor` (netting within each factor) and `portfolio` (full
trading diversification). Only the nine DeMiguel factors and only February 1967 -
December 2020 are covered; outside that window the existing flat factor-level
proxy is kept and labelled (owner decision (i)).

**Exact conventions mirrored from their MATLAB.** trade = x_t w_t - x_{t-1}
w_{t-1} (1 + r_{t-1}); NA terms are dropped (`nansum`), so stocks outside CRSP in
either month (NaN weight or return) are not charged, while listed stocks entering
or leaving a factor (weight 0) are. The first month of an OOS path is charged as
entry from cash. Their Table III OOS weights are re-estimated every 12 months.

**Check.** Average monthly cost of each unmanaged factor, trades March 1977 -
December 2020 (`data/demiguel/fig2b_factor_costs.csv`); the engine matches an
independent replica of their figure code to 1e-8:

| bp/month | MKT | SMB | HML | RMW | CMA | UMD | ROE | IA | BAB |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| Engine | 1.54 | 7.17 | 9.95 | 9.86 | 13.44 | 45.84 | 30.74 | 21.66 | 28.17 |
| Paper (p. 3870, Fig. 2B) | ~1.5 | 7.5 | 10 | 10 | 13 | 46 | 31 | 22 | 28 |

## 11. Step 3b: Table I, Panels B, D and E (`R/demiguel_replication.R`)

Costs come from the stock-level engine of Section 10 with our provider returns
and our own-variance signal. The formulas mirror `VMPnew_TC_FFHXZBAB.m`
(in-sample) and `VMP_OOS_noshortconst_FFHXZBAB.m` (OOS) line by line:

- **B (in-sample, no trading diversification).** Weights solve
  min gamma/2 b'Sigma b - mu'b + sum_j C_j |b_j|, C_j = each sleeve's average
  own stock-level cost at unit weight (their `pppquadprog_sc.m`), then b/|b2|.
  Net return = gross - mean_t |sum_j c_{j,t} b_j|, their exact expression.
- **D (OOS, no trading diversification).** Panel C weights; cost
  sum_j |b_{j,t}| c_{j,t} at the holding month, averaged and subtracted.
- **E (OOS, with trading diversification).** Weights solve
  min gamma/2 b'Sigma b - mu'b + ||Phi b||_1 / D, Phi = stock-level trades of
  both sleeves (netted within the factor) over the estimation window (their
  `pppfminunc.m`). As in their code, the window's trades run up to the trade
  INTO the holding month (a small look-ahead in kappa and weights that we
  mirror, not endorse). Cost = netted stock-level cost of the current weights;
  the first OOS month is charged as entry from cash.
- Sharpe ratios divide the net mean by the gross SD (footnote 24). Benchmarks:
  the in-sample unmanaged factor net of its average own cost (B); the OOS
  unmanaged factor net of its own costs, entry included (D, E).
- Their optimizers are a smoothed quasi-Newton (E) and quadprog (B). We solve
  the same convex problems exactly: sign-pattern enumeration with KKT checks
  (B) and an angular sweep over the cones where the L1 cost is linear (E). On
  300 random problems neither is ever worse than a brute-force search.

**Implementation check on the authors' data** (their returns, daily data and
stock-level data): 26 of 27 cost-panel Sharpe ratios match the paper within
0.001; p-values within 0.015 (bootstrap noise). The exception is **ROE, Panel
B: .331 vs .311**. In the paper ROE's Panel B Sharpe equals the unmanaged one,
i.e. the managed sleeve is (nearly) priced out; on the noise-added package data
it enters, but a 10.5% higher managed-sleeve cost would price it out (MKT, UMD,
BAB need 23-39%). We attribute it to the authors' deliberate data noise; it
cannot be verified without their clean data.

**Result on our provider data** (`data/demiguel/table1_panels_bde.csv`,
`table1_panels_bde_vs_paper.csv`): Sharpe ratios within 0.04 of the paper
(largest: IA Panel D, -.087 vs -.127); every Panel B/D/E conclusion keeps its
sign. Distance to the paper is reported, not asserted in tests.

| | MKT | SMB | HML | RMW | CMA | UMD | ROE | IA | BAB |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| B: SR(r_k, r^sigma) paper | .521 | .125 | .053 | .356 | .159 | .251 | .311 | .107 | .703 |
| B: ours | .525 | .127 | .066 | .343 | .161 | .247 | .326 | .112 | .696 |
| D: paper | .324 | -.295 | -.041 | -.453 | -.047 | .194 | .269 | -.127 | .690 |
| D: ours | .329 | -.298 | -.039 | -.450 | -.024 | .196 | .252 | -.087 | .681 |
| E: paper | .433 | .035 | .089 | .226 | .153 | .209 | .324 | .193 | .746 |
| E: ours | .437 | .033 | .096 | .210 | .152 | .206 | .320 | .206 | .743 |

Runtime: about one hour (`run_cost_panels <- TRUE`), dominated by Panel E's
9 x 527 OOS re-solves. Validation scripts: `tmp/demiguel_table1_validation_20261011/`.

## 12. Step 6: Table III, columns (1), (3), (4), (6) (`R/demiguel_table3.R`)

UMV and CMV multifactor portfolios of the nine factors, nonnegative weights,
gamma = 5, CMV sleeves f / sigma_t with sigma_t the previous month's daily
market SD (our French Mkt-RF daily; their `VIXmonth`). CFW (columns 2, 5)
remains deferred. Mirrors `OOS_nonnegative.m` and `JF_performancematrixTable4.m`
(`R/demiguel_table3_helpers.R`):

- OOS weights are re-estimated every 12 months, expanding from February 1967,
  and held fixed in between. The cost-aware window includes the trade into
  the holding month, divided by D = number of return months (their convention).
- Costs charge the actual stock trades between consecutive months, entry from
  cash in the first OOS month: netted across all 18 sleeves (Panel D) or
  sleeve by sleeve (Panel C). BAB's zero stock weights are treated as missing,
  as in their Table III script (unlike Figure 2B).
- Columns (1), (3): nonnegative MV (`quadprog`). Columns (4), (6): nonnegative
  MV with the stock-level L1 cost; they use SQP, we use a smoothed L-BFGS-B
  certified by a duality gap below 1e-7 of the no-cost utility. cov1para and
  nwest are exact ports; cov1para applies when cond(Sigma) > 1e9.
- Statistics: annualized net mean, gross SD, Sharpe, bootstrap p-value of
  SR(CMV) - SR(UMV) on net returns, Newey-West(1) alpha of CMV net on UMV net,
  TC = annualized gross minus net mean.

**Implementation check on the authors' data** (their returns, `VIXmonth`,
stock data; `tmp/demiguel_table3_validation_20261012/`): columns (1) and (3)
reproduce the paper EXACTLY in all panels and statistics (p-values within
bootstrap noise); columns (4) and (6) within 0.006 in means, 0.007 in Sharpe
ratios and 0.06 in t-statistics (noise in their stock data and their
approximate SQP versus our certified optimum).

**Result on our data** (`data/demiguel/table3.csv`, `table3_vs_paper.csv`): all
Sharpe ratios within 0.015 of the paper. Panel D, the headline:

| | UMV (1) | CMV (3) | UMV (4) | CMV (6) |
| --- | --- | --- | --- | --- |
| SR paper | .891 | 1.010 | .940 | 1.062 |
| SR ours | .884 | 1.006 | .938 | 1.062 |
| p(SR_CMV - SR_UMV) paper / ours | | .072 / .061 | | .006 / .005 |
| t(alpha) paper / ours | | 2.759 / 2.828 | | 3.637 / 3.605 |

Runtime about 15 minutes.

## 13. Step 7: Table I, lagged RV versus ML (`R/demiguel_table1_ml.R`)

Same individual-factor portfolios as Sections 9 and 11 (Panels A-E, authors'
specification and stock-level costs); only the holding-month variance signal
changes. `run_table1_cost_free()` / `run_table1_costs()` gained an optional
`variance` argument and per-factor start/end/M (`factor_setting()`), with
bootstrap calendars shared when windows coincide; with the defaults they
reproduce the committed Sections 9 and 11 results bit for bit.

Signals: `lagvar` (paper: previous month's daily sample variance), `lagss`
(previous month's sum of squared daily returns, our target lagged), `har`,
`harx`, `ew` (pooled long-sample forecasts, `models_v3/demiguel/1927`,
exp(pred) - 1e-8; `ew` = EW of harx, glmnet, rf, xgb, nn1-nn3). All signals
are evaluated on identical months within each factor (checked).

- `paper_window` (`data/demiguel/table1_ml_paper_window.csv`): evaluation
  February 1977 - December 2020 for all factors. ROE and IA forecasts start in
  March 1970, so their first estimation window is 83 months, not 120; their
  `lagvar` rows therefore differ slightly from Section 11. The other seven
  `lagvar` rows equal the committed Panels A-E exactly.
- `full_oos` (`data/demiguel/table1_ml_full_oos.csv`): each factor's whole
  forecast history through December 2024 (MKT, SMB, HML, UMD, BAB from June
  1957; RMW, CMA from September 1976; ROE, IA from March 1980), M = 120.
  Panels A and C only (stock-level costs end in December 2020).

Paper window, selected Sharpe ratios (C = OOS gross, D = OOS net of costs with
cost-unaware weights, E = OOS cost-aware):

| | C lagvar | C ew | D lagvar | D ew | E lagvar | E ew |
| --- | --- | --- | --- | --- | --- | --- |
| MKT | .412 | .414 | .329 | .360 | .437 | .450 |
| UMD | 1.022 | 1.070 | .196 | -.285 | .206 | .269 |
| ROE | 1.055 | 1.149 | .216 | .157 | .277 | .293 |
| RMW | .539 | .529 | -.450 | -.606 | .210 | .209 |
| BAB | 1.318 | 1.264 | .681 | .595 | .743 | .746 |

SMB, HML, CMA and IA are within about 0.03 across signals in C and E. `lagss`
tracks `lagvar` closely (definition is not the driver). ML signals cut the
managed sleeve's trading cost (`cost_managed_bp`: in-sample mean monthly cost
at unit exposure) by 15-45% (e.g. BAB 52 vs 72 bp/month), and
in Panel E match or beat `lagvar` for every factor up to +/-0.003. Open item:
UMD in Panel D turns negative under ML despite lower costs (diagnosis pending).

Runtime about 5 hours (Panel E's exact cone sweep for 5 signals).