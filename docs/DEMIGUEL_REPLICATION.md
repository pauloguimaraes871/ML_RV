# DeMiguel, Martin-Utrera and Uppal (2024) replication and ML extension

Reference: V. DeMiguel, A. Martin-Utrera and R. Uppal, "A Multifactor Perspective
on Volatility-Managed Portfolios", *Journal of Finance* 79(6), 2024,
https://doi.org/10.1111/jofi.13395.

The JKP factor panel is replaced by the paper's nine factors and the existing V3
pipeline (forecasts, MM portfolios, Cederburg, multifactor allocations) is rerun
on them. A separate replication path, without ML, reproduces Tables I and III.

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
history. Long sample = 1927 covariate tier (MKT, SMB, HML, UMD from 1927; BAB from
1931; RMW, CMA from 1963; ROE, IA from 1967). Short sample = 1986 tier (all keys
from February 1986). Sample ids remain `1927`/`1986` so the runners' switches are
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
