base::source(base::file.path(here::here(), "R", "portfolio_inference_helpers.R"))
base::source(base::file.path(here::here(), "R", "portfolio_inference_inputs.R"))
base::source(base::file.path(here::here(), "R", "reporting_helpers.R"))
base::source(base::file.path(here::here(), "R", "report_portfolio_inference.R"))

testthat::test_that("primary artifacts preserve the verified scopes, benchmarks and numeric results", {
  # Artifacts are research inputs, not portable test fixtures. The analytical
  # tests above run everywhere. This regression test runs when inputs exist.
  path <- base::file.path(here::here(), "ports/v3/1986/panel/expanding_vol_10bps_60m/all_ports.rds")
  testthat::skip_if_not(base::file.exists(path), "Completed primary backtests are not installed.")
  data <- base::suppressWarnings(read_primary_portfolio_inference())
  testthat::expect_equal(base::nrow(data$contrasts), 116L)
  testthat::expect_true(base::all(base::c(data$contrasts$strategy, data$contrasts$benchmark) %in% data$returns$series_id))
  testthat::expect_equal(base::length(base::unique(data$returns$month_id)), 162L)
  testthat::expect_equal(base::min(data$returns$month_id), base::as.Date("2011-07-01"))
  testthat::expect_equal(base::max(data$returns$month_id), base::as.Date("2024-12-01"))
  testthat::expect_true(base::all(data$scaler_checks$max_absolute_difference < 1e-12))
  testthat::expect_equal(data$scaler_checks$n_pairs, base::rep(base::c(24948L, 162L), each = 4L))
  testthat::expect_true(base::all(data$provenance_checks$matches[
    data$provenance_checks$match_required]))
  testthat::expect_true(base::all(data$provenance_checks$input_type %in%
    base::c("saved_input", "live_source")))
  testthat::expect_true(base::all(data$ensemble_checks$max_absolute_difference < 1e-12))
  testthat::expect_equal(base::unique(data$ensemble_checks$n_members), 7L)
  testthat::expect_false(base::any(base::grepl("fcmv", data$contrasts$forecast) &
    data$contrasts$comparator == "lagged_market"))
  testthat::expect_equal(base::nrow(data$implementation), 9720L)
  testthat::expect_true(base::all(base::abs(data$implementation$gross_return -
    data$implementation$cost - data$implementation$excess_return) < 1e-12))
  mvo <- dplyr::filter(data$contrasts, portfolio_family == "ced_mvo", comparator == "unmanaged_mvo_ew")
  testthat::expect_equal(base::nrow(mvo), 6L)
  stats <- summarise_portfolio_returns(data$returns, "series_id")
  get <- function(id, column) dplyr::filter(stats, series_id == id)[[column]]
  # These values were separately recomputed from holding-month returns before
  # implementing this adapter, rather than copied from its generated output.
  testthat::expect_equal(get("mm/aggregate_153/raw/ew", "sharpe_ratio"), .4676675, tolerance = 1e-6)
  testthat::expect_equal(get("mm/aggregate_153/raw/ew", "cer_ann"), .009319298, tolerance = 1e-8)
  testthat::expect_equal(get("mm/market_pooled/raw/buyhold", "sharpe_ratio"), .769244, tolerance = 1e-6)
  testthat::expect_equal(get("ced_ew/market_pooled/raw/ew", "sharpe_ratio"), .785550, tolerance = 1e-6)
  testthat::expect_equal(get("multi_ew/all_154/raw/fcmv_ew", "sharpe_ratio"), .542831, tolerance = 1e-6)
  testthat::expect_equal(get("multi_hew/all_154/raw/umv", "sharpe_ratio"), 1.305258, tolerance = 1e-6)

  descriptive <- summarise_single_factor_implementation(data$implementation)
  testthat::expect_equal(base::nrow(descriptive), 60L)
  get_descriptive <- function(family_id, scope_id, version_id, model_id, column) {
    dplyr::filter(descriptive, family == family_id, scope == scope_id,
                  version == version_id, model == model_id)[[column]]
  }
  testthat::expect_equal(get_descriptive("ced_ew", "aggregate_153", "capped", "ew",
    "net_sharpe"), .4972129, tolerance = 1e-6)
  testthat::expect_equal(get_descriptive("ced_mvo", "market_pooled", "capped", "ew",
    "net_sharpe"), .5379308, tolerance = 1e-6)

  output_dir <- base::tempfile("cederburg-tables-")
  base::dir.create(base::file.path(output_dir, "main"), recursive = TRUE)
  base::dir.create(base::file.path(output_dir, "internet_appendix"), recursive = TRUE)
  base::on.exit(base::unlink(output_dir, recursive = TRUE), add = TRUE)
  outputs <- write_cederburg_descriptive_tables(descriptive, output_dir)
  testthat::expect_true(base::file.exists(base::file.path(output_dir, "main", outputs$main)))
  testthat::expect_true(base::file.exists(base::file.path(
    output_dir, "internet_appendix", outputs$internet_appendix)))

  spanning <- purrr::pmap_dfr(data$contrasts, function(comparison_id, family,
    portfolio_family, scope, version, forecast, comparator, strategy, benchmark) {
    returns <- data$returns %>%
      dplyr::filter(series_id %in% c(strategy, benchmark)) %>%
      dplyr::select(month_id, series_id, excess_return) %>%
      tidyr::pivot_wider(names_from = series_id, values_from = excess_return) %>%
      dplyr::transmute(month_id, excess_return = .data[[strategy]],
                        benchmark_return = .data[[benchmark]])
    span_portfolio_returns(returns, lag = 1L) %>%
      dplyr::mutate(comparison_id, family, portfolio_family, scope, version,
                    forecast, comparator)
  })
  spanning_outputs <- write_single_factor_spanning_table(spanning, output_dir)
  testthat::expect_true(base::file.exists(base::file.path(
    output_dir, "internet_appendix", spanning_outputs$internet_appendix)))
})
