# Completed portfolio backtests: descriptive review

Reviewed 18 September 2026. Primary artifacts: `ports/v3/1986/{panel,spx}/expanding_vol_10bps_60m/` and `ports/v3/1986/allocations/expanding_vol_10bps_60m_alloc_60_60_60m/`. This review does not change backtests, models, reporting production code or the manuscript. Its statistics are descriptive; new significance tests have not yet been run.

## Evaluation and checks

The primary evaluation contains 162 months, July 2011–December 2024. Calibration uses at least 60 observations and subsequently expands; this is not a rolling five-year window. The panel has 153 JKP factors plus the market. The single-factor panel aggregate below excludes the market; the multifactor allocation includes it. These are different opportunity sets.

The allocation folder contains all 144 expected strategy RDS files, plus settings. Every entry in its input manifest matched the current file hash. UMV monthly returns were identical across all 36 model/signal files for each allocator. Nine targeted assertions passed: common evaluation coverage, 153-factor panel count, agreement of independently recomputed MM Sharpe/CER with saved metrics, and matching MM/Cederburg current scalers for EW-forecast and RW signals on all 24,948 factor-months each. These checks are not a complete new audit of upstream forecast provenance.

All nine forecast models are included in the supporting aggregate/individual analysis files. Main tables here focus on EW ensemble, HAR, HARX and RW. “EW ensemble” describes forecast averaging; “EW allocation” describes equal capital coefficients. They are distinct.

Sharpe uses monthly net excess returns, annualized by sqrt(12). CER is 12 times mean(net) minus gamma/2 times variance(net), gamma=5. Volatility is annualized. Drawdown compounds 1+rf+net, starting at wealth one. These are the existing collateralized-wealth conventions, not DeMiguel's cumulative dollar-payoff convention. Costs use the saved 10-bp factor-exposure proxy.

## Single-factor evidence

Net Sharpe, raw exposure:

| Forecast/baseline | MM: 153-factor aggregate | MM: pooled market | Cederburg EW: 153-factor aggregate | Cederburg EW: pooled market |
|---|---:|---:|---:|---:|
| Unmanaged comparator | 0.480 | 0.769 | 0.480 | 0.769 |
| RW | 0.330 | 0.498 | 0.462 | 0.702 |
| HAR | 0.428 | 0.691 | 0.476 | 0.768 |
| HARX | 0.403 | 0.721 | 0.466 | 0.785 |
| EW forecast ensemble | 0.468 | 0.731 | 0.497 | 0.786 |

The ensemble improves on these managed forecasting benchmarks, but pure raw MM management does not beat unmanaged Sharpe in either aggregate scope. The Cederburg 50/50 combination slightly beats unmanaged Sharpe in both scopes. This is not evidence that every factor benefits, and these small differences require paired inference.

Risk and utility for the ensemble, raw exposure (annual percentages; volatility ratio uses the corresponding net unmanaged aggregate):

| Scope / strategy | Mean | Volatility | Volatility ratio | CER | Maximum drawdown |
|---|---:|---:|---:|---:|---:|
| 153-factor unmanaged | 1.29 | 2.69 | 1.00 | 1.11 | -8.84 |
| 153-factor MM ensemble | 1.06 | 2.27 | 0.84 | 0.93 | -4.61 |
| 153-factor Cederburg EW ensemble | 1.17 | 2.36 | 0.88 | 1.03 | -6.75 |
| Market unmanaged | 11.53 | 14.99 | 1.00 | 5.91 | -24.98 |
| Market MM ensemble | 12.87 | 17.62 | 1.18 | 5.11 | -27.43 |
| Market Cederburg EW ensemble | 12.20 | 15.53 | 1.04 | 6.17 | -20.30 |

The aggregate Cederburg Sharpe gain does not imply a CER gain for the factor panel: its lower mean leaves CER below unmanaged. The pooled-market combination has both modestly higher Sharpe and CER, and a smaller drawdown. Expanding training-volatility matching does not guarantee equal realized evaluation-period risk.

Caps materially change MM interpretation. At the 1.5 cap, ensemble panel Sharpe rises to 0.523 versus unmanaged 0.480, while pooled-market Sharpe becomes 0.760 versus 0.769. Ensemble panel CER remains lower (0.96% versus 1.11%; annual volatility 2.03% versus 2.69%). Pooled-market capped CER is 5.76% versus 5.91% (volatility 14.54% versus 14.99%). Report raw and capped explicitly rather than selecting the favorable version after seeing outcomes.

Across the 153 individual factors, raw MM ensemble Sharpe beats unmanaged in only 44.4%, RW in 66.0%, HAR in 58.8%, and HARX in 68.6%. Median respective differences are -0.027, +0.063, +0.012 and +0.021. For Cederburg EW the corresponding fractions are 49.0%, 61.4%, 56.2% and 68.6%; median differences are -0.005, +0.013, +0.007 and +0.010. These cross-sectional summaries are not independent-binomial significance tests.

Theme heterogeneity is substantial. Median raw MM ensemble minus unmanaged Sharpe is positive for Momentum (+0.162), Profit Growth (+0.127), Low Leverage (+0.101) and Low Risk (+0.080), but negative for Accruals (-0.321), Value (-0.195), Seasonality (-0.139) and Investment (-0.104). Low Risk improves in 17/18 factors, whereas Value improves in 1/18 and Size in 0/5. Use paired theme plots rather than a single “ML works” statement.

The pooled-market ensemble is stronger than its standalone-market counterpart: MM Sharpe 0.731 versus 0.665; Cederburg EW 0.786 versus 0.749. This supports the planned appendix separation but is not yet a significance conclusion.

Performance varies over time. Pooled-market raw MM ensemble Sharpe is 1.29 in 2011H2–2014, 0.644 in 2015–2019 and 0.369 in 2020–2024; unmanaged is respectively 1.12, 0.796 and 0.619. The analogous Cederburg ensemble values are 1.26, 0.721 and 0.530. Thus full-period results do not establish a stable market timing improvement. The primary run cannot speak to the 2008 crisis.

The ensemble is a representative, not the ex-post best model. For the panel raw MM aggregate, RF and XGB Sharpe are 0.521 and 0.506 versus ensemble 0.468; Cederburg EW gives 0.523 and 0.519 versus 0.497. Preserve the ensemble's prespecified main role and disclose other models in the appendix.

## Multifactor evidence

Net Sharpe, raw exposure, common dates:

| Strategy/signal | EW allocation | HEW allocation |
|---|---:|---:|
| UMV | 0.517 | 1.31 |
| CMV, lagged market RV | 0.496 | 1.22 |
| CMV, HAR pooled market | 0.497 | 1.25 |
| CMV, HARX pooled market | 0.505 | 1.27 |
| CMV, EW-ensemble pooled market | 0.517 | 1.28 |
| FCMV, HAR own-factor signals | 0.527 | 1.32 |
| FCMV, HARX own-factor signals | 0.530 | 1.33 |
| FCMV, EW-ensemble own-factor signals | 0.543 | 1.35 |

FCMV is the strongest of the three highlighted forecasts under each allocator. Its incremental Sharpe gain over UMV is modest, around 0.026 under EW and 0.04 under HEW. Ensemble CMV is essentially tied with EW UMV and below HEW UMV. This favors distinguishing market timing from own-factor conditioning.

HEW is a different portfolio composition: equal weights across 13 JKP themes and a separate Market cluster. Its unmanaged market exposure is 7.14%, versus 0.65% under EW. The large Sharpe level difference between HEW and EW is therefore not a measure of the forecasting benefit. Compare conditional versus unmanaged within each allocator, then explain composition separately.

Costs consume part of the gross gains. Under EW allocation, annual incremental costs are about 0.74 bps for UMV (initial entry only), 27.8 bps for lagged-market CMV, 13.8 bps for ensemble CMV and 11.0 bps for ensemble FCMV. Ensemble CMV/FCMV robust raw and capped returns coincide in this sample; lagged-market CMV does hit the cap, so do not universally deduplicate raw/capped strategies.

No own-factor RW FCMV comparator is saved in the current runner. Lagged-market CMV cannot substitute for it. Adding that comparator is a separate backtest extension.

### MVO appendix interpretation

For the ensemble's constrained multifactor MVO, mean maximum underlying-factor share is about 39–40%; mean effective factor count is 3.5–3.8. Only about five factors have economically nonzero exposure (threshold 1e-6), despite 154 available factors. Aggregate exposure is at the 1.5 limit. Conditional covariance systems contain 308 columns and exhibit rcond around 1e-23 or smaller on average, with mean recorded rank about 140. Bounded optimization can produce a feasible solution without a full-rank covariance; convergence does not establish statistical identification or stability.

These current constrained portfolios are not insolvent. Ensemble UMV, pooled CMV and FCMV net Sharpe are 0.637, 0.619 and 0.606; their worst monthly total returns are approximately -5.94%, -5.36% and -5.42%. Lagged-market constrained CMV Sharpe is 0.702. A fair diagnostic table should include that favorable comparator too. The evidence supports concerns about concentration, estimation and representativeness of the broad panel, not a blanket claim that every MVO result fails.

Cederburg MVO is also mixed. With the ±5 cap, ensemble pooled-market MVO Sharpe is 0.538 versus 0.628 for unmanaged-only MVO. For the equal-weight aggregate of individual factor MVO strategies, it is 0.641 versus 0.469. No Cederburg summary in this primary folder records insolvency. Keep paired factor distributions and exposure diagnostics alongside aggregate appendix performance.

## Methods and inference that reporting must preserve

The complete implementation table and new-conversation instructions are in `docs/PORTFOLIO_REPORTING_PROMPT.md`. Most important distinctions:

- MM and Cederburg use inverse own variance; multifactor conditional portfolios use inverse volatility. DeMiguel also uses inverse variance for the individual-factor analysis (fn14), so the difference is not unique to this project.
- Primary MM expanding_vol and Cederburg training_vol share the same current c_t on matching history. Neither is expanding_rv. Full-sample c is ex post and belongs in the appendix.
- Multifactor robust normalizers match each managed sleeve's training volatility; MVO's numerical column standardization is undone and is not this economic normalization.
- Allocation consumes saved calibration signals/returns. Multifactor results are independent of upstream MM weight normalization conditional on identical input signals, return history and evaluation dates; they are not independent of upstream artifacts or their coverage.
- Factor-level costs cannot reproduce the papers' full stock-level costs or DeMiguel's trading diversification. Simple-return compounding, sum-of-squares RV, caps and expanding normalization are explicit implementation choices.

Paper-specific testing verified from the attached texts:

| Paper | Relevant tests/statistics |
|---|---|
| Moreira–Muir | Managed-on-unmanaged spanning alpha, appraisal ratio and regression inference; table captions specify heteroskedasticity-adjusted standard errors |
| Cederburg | Jobson–Korkie Sharpe differences with Memmel correction (fn8); CER-difference inference following DeMiguel–Garlappi–Uppal 2009 (sec4.3.1, fn19) |
| DeMiguel 2024 | 10,000 paired stationary-block bootstrap samples, average block length five months, one-sided Sharpe comparison (fn15); alpha Newey–West lag one with five/ten-month sensitivity (fn25) |

For this panel, use paired synchronized month-block resampling that preserves cross-factor dependence, with explicitly defined mean/Sharpe/CER contrasts and multiplicity handling. Report confidence intervals and effects rather than significance counts alone. These tests remain to be implemented and run; no inferential claims follow from this descriptive review.

Source locations: the three user-supplied PDFs in Downloads; extracted review texts in tmp/pdfs/{mm-review,cederburg,demiguel}.txt. DeMiguel fn24 uses net mean with gross SD; existing multifactor paper CER also uses gross variance. Keep those separate from consistently defined net-risk metrics. DeMiguel fn23 sums self-financing dollar payoffs, unlike the collateralized compounded wealth curves proposed here.

## Reproduction and outputs

Run the scratch review scripts with R 4.3 and the project renv library:

1. `tmp/pdfs/evaluate-completed-ports.R`: all-model individual and aggregate statistics, paired panel summaries, MVO diagnostics.
2. `tmp/pdfs/evaluate-ports-details.R`: highlighted multifactor metrics, themes, subperiods, manifest and UMV equality checks.
3. `tmp/pdfs/validate-portfolio-review.R`: nine targeted assertions and all-model rankings.

Supporting CSVs are `tmp/pdfs/portfolio-review-{aggregate,individual,comparisons,multifactor,mvo,themes,subperiods}.csv`; the combined RDS is `tmp/pdfs/portfolio-review.rds`. No statistical bootstrap, new regression inference, manuscript rendering, model refit or portfolio rerun was performed for this review. Legacy appendix runs were not re-evaluated numerically here; their effective normalization and coverage must be verified when integrated.
