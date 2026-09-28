# Long- and short-sample reporting

The main analysis uses the long sample. Daily returns begin in 1926, but the
saved model and portfolio directories are named `1927`, the first predictor
year. The short sample uses predictors beginning in 1986.

## One reporting body

`R/report_results.R` defines `report_results(sample_id, output_dir,
include_inference)`. Both samples execute the same body and metric helpers.
It reads the checked, cached seven-member log-forecast ensemble and completed
backtests. It neither fits models nor constructs portfolios.

```r
source(here::here("R", "report_results.R"))
report_results("1927")
```

`source(here::here("R", "report_1927_panels.R"))` runs that application and
composes the manuscript exhibits. It renders BH columns for the short sample
from the existing bootstrap without changing its original estimates or Holm
values. `R/compose_sample_reports.R` controls only sample ordering, layout and
comparison plots. It extracts original table bodies, preserving numeric cells.
The manuscript reads `latex_paper/results_v3/combined/`.

For layout-only revisions, after generating both samples:

```r
source(here::here("R", "compose_sample_reports.R"))
compose_sample_reports()
```

## Coverage and inference

Long-sample forecasts span June 1947–December 2024. Market portfolios retain
June 1952–December 2024 (871 months). Balanced factor aggregates and multifactor
portfolios retain January 1980–December 2024 (540 months). Short-sample forecasts
span July 2006–December 2024 and portfolios July 2011–December 2024 (162 months).

Paired bootstrap draws are shared by all strategies on the same calendar.
Different native histories are not truncated to the shorter calendar. Inference
uses the existing 10,000 draws, seed 20260918 and block lengths 5, 3 and 10.
Holm and BH use all Sharpe, mean and CER comparisons in the same prespecified
family, separately by sample and block length. BH is supplementary and depends
on its usual dependence assumptions. No favorable benchmark is dropped.

There is no saved long-sample full-sample-normalizer sensitivity. Its appendix
table is therefore short-sample only. Creating the long counterpart requires
separate authorization to run a new backtest.

## Regression checks

The complete shared short-sample report was generated in
`tmp/report_short_check/`. All 30 comparable data CSVs matched the existing
1986 outputs exactly. The reporting-code manifest appropriately differs.
`tests/testthat/test-two-sample-reporting.R` checks this result, native calendars,
selected metric values, matched MM/Cederburg scalers, adjustment families,
unchanged balanced-calendar bootstrap estimates, and preservation of original
short-sample table rows. Portable metric/bootstrap tests remain in the existing
reporting and inference test files.

Combined source hashes and both samples' bootstrap statistics, including BH,
are saved under `latex_paper/results_v3/combined/data/`.

Table 8 contains four capped panels, two allocators per sample. These are the
existing specifications, presented together without adding raw specifications.
The factor Sharpe figure uses a continuation because twelve construction/sample
panels become unreadable on one portrait page. Removed appendix exhibits retain
their underlying machine-readable statistics.
