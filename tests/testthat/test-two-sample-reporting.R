base::source(here::here("R", "reporting_helpers.R"))

# These integration checks use saved reporting artifacts. Portable analytical
# tests for metrics and bootstrap sampling remain in the existing helper tests.
testthat::test_that("shared reporting preserves the reviewed short-sample statistics", {
  old <- here::here("latex_paper", "results_v3", "1986", "data")
  checked <- here::here("tmp", "report_short_check", "data")
  testthat::skip_if_not(base::dir.exists(checked), "Run the shared short-sample audit first.")
  files <- base::intersect(base::list.files(old, pattern = "\\.csv$"),
    base::list.files(checked, pattern = "\\.csv$"))
  files <- base::setdiff(files, "report_code_manifest.csv")
  testthat::expect_gte(base::length(files), 30L)
  purrr::walk(files, function(file) {
    testthat::expect_identical(utils::read.csv(base::file.path(old, file)),
      utils::read.csv(base::file.path(checked, file)), info = file)
  })
})

testthat::test_that("long inference retains native calendars and verified Sharpe values", {
  root <- here::here("latex_paper", "results_v3", "1927", "data")
  path <- base::file.path(root, "portfolio_inference_statistics.csv")
  testthat::skip_if_not(base::file.exists(path), "Generate the long-sample report first.")
  returns <- utils::read.csv(base::file.path(root, "portfolio_inference_returns.csv"))
  returns$month_id <- base::as.Date(returns$month_id)
  testthat::expect_equal(base::anyDuplicated(returns[c("series_id", "month_id")]), 0L)
  coverage <- returns %>% dplyr::group_by(scope, series_id) %>%
    dplyr::summarise(n = dplyr::n(), first = base::min(month_id), .groups = "drop")
  market <- dplyr::filter(coverage, scope %in% c("market_pooled", "market_separate"))
  balanced <- dplyr::filter(coverage, !scope %in% c("market_pooled", "market_separate"))
  testthat::expect_true(base::all(market$n == 871L & market$first == base::as.Date("1952-06-01")))
  testthat::expect_true(base::all(balanced$n == 540L & balanced$first == base::as.Date("1980-01-01")))
  statistics <- summarise_portfolio_returns(returns, "series_id")
  get <- function(id) dplyr::filter(statistics, series_id == id)$sharpe_ratio
  testthat::expect_equal(get("mm/aggregate_153/capped/ew"), .9204959659, tolerance = 1e-9)
  testthat::expect_equal(get("mm/market_pooled/capped/ew"), .5196907969, tolerance = 1e-9)
  scalers <- utils::read.csv(base::file.path(root, "portfolio_inference_scaler_checks.csv"))
  testthat::expect_true(base::all(scalers$max_absolute_difference < 1e-12))
  testthat::expect_true(base::all(scalers$n_pairs[scalers$universe == "panel"] == 153L * 540L + 871L))
})

testthat::test_that("BH uses the complete prespecified family and leaves inference estimates intact", {
  path <- here::here("latex_paper", "results_v3", "1927", "data", "portfolio_inference_bootstrap.csv")
  testthat::skip_if_not(base::file.exists(path), "Generate the long-sample inference first.")
  data <- utils::read.csv(path)
  purrr::walk(base::split(data, base::interaction(data$family, data$block_length, drop = TRUE)), function(rows) {
    testthat::expect_equal(rows$p_bh, stats::p.adjust(rows$p_value, "BH"), tolerance = 1e-12)
    testthat::expect_equal(rows$p_holm, stats::p.adjust(rows$p_value, "holm"), tolerance = 1e-12)
    testthat::expect_setequal(base::unique(rows$metric), c("sharpe", "mean_ann", "cer_ann"))
  })
  baseline <- here::here("tmp", "long_inference_before_native_market.csv")
  if (base::file.exists(baseline)) {
    old <- utils::read.csv(baseline) %>% dplyr::filter(scope %in% c("aggregate_153", "all_154"))
    comparison <- dplyr::inner_join(old, data,
      by = c("comparison_id", "metric", "block_length"), suffix = c("_old", "_new"),
      relationship = "one-to-one")
    testthat::expect_equal(base::nrow(comparison), base::nrow(old))
    for (column in c("estimate", "ci_lower", "ci_upper", "p_value")) {
      testthat::expect_equal(comparison[[base::paste0(column, "_old")]],
        comparison[[base::paste0(column, "_new")]], tolerance = 1e-12)
    }
  }
})

testthat::test_that("combined tables preserve every original short-sample numeric row", {
  root <- here::here("latex_paper", "results_v3")
  testthat::skip_if_not(base::dir.exists(base::file.path(root, "combined")))
  for (name in c("factor_gain_summary", "portfolio_performance",
                 "portfolio_performance_cederburg_ew", "portfolio_performance_multifactor")) {
    old <- base::readLines(base::file.path(root, "1986", "main", base::paste0(name, ".tex")))
    new <- base::readLines(base::file.path(root, "combined", "main", base::paste0(name, ".tex")))
    numeric_rows <- old[base::grepl(" & [-0-9]", old)]
    testthat::expect_true(base::all(numeric_rows %in% new), info = name)
    testthat::expect_lt(base::grep("Panel A. Long sample", new, fixed = TRUE)[1],
      base::grep("Panel [A-Z]\\. Short sample", new)[1])
  }
  performance <- base::readLines(base::file.path(root, "combined", "main", "performance.tex"))
  testthat::expect_true(base::any(base::grepl(
    "Model & RMSE & MAE & MPHE & MAPE & R-squared & Bias", performance, fixed = TRUE)))
  testthat::expect_false(base::any(base::grepl("tabular*", performance, fixed = TRUE)))
})
