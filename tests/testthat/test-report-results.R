base::source(base::file.path(here::here(), "R", "helpers.R"))
base::source(base::file.path(here::here(), "R", "reporting_helpers.R"))

make_reporting_fixture <- function() {
  observations <- tibble::tibble(
    month_id = rep(as.Date(c("2020-01-01", "2020-02-01")), each = 2),
    name = rep(c("a", "b"), 2),
    target = c(-4, -2, -3, -1), pred = c(-5, -1, -2, -2)
  )
  meta <- list(key_col = "name", eval_metric = "rmse", huber_delta = 1,
               quantile_tau = 0.5,
               provenance = list(universe = "panel_1986",
                                 target_transform = "log", preprocessing = "range"))
  list(harx = list(oos_outputs = observations, backtest_meta = meta),
       glmnet = list(oos_outputs = dplyr::mutate(observations, pred = target),
                     backtest_meta = meta))
}

testthat::test_that("EW averages within target-month and is invariant to row order", {
  models <- make_reporting_fixture()
  models$glmnet$oos_outputs <- models$glmnet$oos_outputs[c(4, 2, 1, 3), ]
  ensemble <- build_ew_ensemble(models, members = c("harx", "glmnet"))
  testthat::expect_equal(ensemble$oos_outputs$pred, c(-4.5, -1.5, -2.5, -1.5))
  testthat::expect_equal(ensemble$oos_outputs$error, c(0.5, -0.5, -0.5, 0.5))
  testthat::expect_equal(ensemble$eval_metrics$cons_oos[
    ensemble$eval_metrics$metric == "rmse"], 0.5)
  testthat::expect_equal(ensemble$backtest_meta$weights$weight, c(0.5, 0.5))
  models$har <- models$harx
  models$har$oos_outputs$pred <- 999
  testthat::expect_equal(build_ew_ensemble(models, c("harx", "glmnet"))$oos_outputs,
                         ensemble$oos_outputs)
})

testthat::test_that("EW rejects incomplete, duplicated or inconsistent members", {
  original <- make_reporting_fixture()
  testthat::expect_error(build_ew_ensemble(original, c("harx", "rf")), "Missing")
  testthat::expect_error(build_ew_ensemble(original, c("harx", "harx")), "unique")
  testthat::expect_error(build_ew_ensemble(original, c("har", "harx")), "HAR")
  models <- original
  models$glmnet$oos_outputs <- models$glmnet$oos_outputs[-1, ]
  testthat::expect_error(build_ew_ensemble(models, c("harx", "glmnet")), "coverage")
  models <- original
  models$glmnet$oos_outputs$target[1] <- 0
  testthat::expect_error(build_ew_ensemble(models, c("harx", "glmnet")), "target")
  models <- original
  models$glmnet$oos_outputs <- dplyr::bind_rows(models$glmnet$oos_outputs,
                                               models$glmnet$oos_outputs[1, ])
  testthat::expect_error(build_ew_ensemble(models, c("harx", "glmnet")), "Duplicate")
  models <- original
  models$glmnet$backtest_meta$provenance$target_transform <- "level"
  testthat::expect_error(build_ew_ensemble(models, c("harx", "glmnet")), "incompatible")
})

testthat::test_that("reporting and portfolios reuse one source-checked EW artifact", {
  models <- make_reporting_fixture()
  models$har <- models$harx
  models <- purrr::imap(models, function(model, name) {
    model$source <- tibble::tibble(
      file = paste0(name, ".rds"),
      sha256 = paste0("hash-", name)
    )
    model
  })
  path <- tempfile(fileext = ".rds")

  first <- testthat::expect_message(
    load_or_build_ew_ensemble(models, path, c("harx", "glmnet")),
    "Building EW ensemble"
  )
  testthat::expect_true(file.exists(path))

  cached <- readRDS(path)
  cached$cache_marker <- TRUE
  saveRDS(cached, path)
  reused <- testthat::expect_message(
    load_or_build_ew_ensemble(models, path, c("harx", "glmnet")),
    "Reusing EW ensemble"
  )
  testthat::expect_true(reused$cache_marker)
  testthat::expect_equal(reused$oos_outputs, first$oos_outputs)

  models$glmnet$source$sha256 <- "replacement-hash"
  rebuilt <- testthat::expect_message(
    load_or_build_ew_ensemble(models, path, c("harx", "glmnet")),
    "Rebuilding stale EW ensemble"
  )
  testthat::expect_null(rebuilt$cache_marker)
  testthat::expect_equal(
    dplyr::filter(rebuilt$backtest_meta$sources, model == "glmnet")$sha256,
    "replacement-hash"
  )
})

testthat::test_that("forecast validation fails early on unusable inputs", {
  observations <- make_reporting_fixture()$harx$oos_outputs
  testthat::expect_error(validate_forecasts(observations[0, ], c("month_id", "name")), "empty")
  observations$pred[1] <- Inf
  testthat::expect_error(validate_forecasts(observations, c("month_id", "name")), "finite")
  observations$pred <- as.character(observations$pred)
  testthat::expect_error(validate_forecasts(observations, c("month_id", "name")), "numeric")
})

testthat::test_that("grouped metrics reuse the consolidated formula", {
  observations <- make_reporting_fixture()$harx$oos_outputs
  actual <- summarise_forecasts(observations, "name")
  expected <- calc_eval_metrics(observations$pred[observations$name == "a"],
                                observations$target[observations$name == "a"])
  testthat::expect_equal(actual$rmse[actual$name == "a"], expected$rmse)
  testthat::expect_equal(actual$rss[actual$name == "a"], expected$rss)
  one <- summarise_forecasts(observations[1, ], "name")
  testthat::expect_true(is.na(one$rss))
  testthat::expect_equal(one$rmse, 1)
  # Pool squared errors, never average subgroup RMSEs.
  observations$pred <- observations$target + c(0, 4, 0, 4)
  pooled <- summarise_forecasts(observations)
  testthat::expect_equal(pooled$rmse, sqrt(8))
})

testthat::test_that("refit blocks are forward-looking and include partial last blocks", {
  observations <- make_reporting_fixture()$harx$oos_outputs
  dates <- as.Date(c("2020-01-01", "2020-02-01"))
  result <- assign_refit_dates(observations, dates)
  testthat::expect_equal(result$refit_date, observations$month_id)
  testthat::expect_error(assign_refit_dates(observations, dates[2]), "before")
  testthat::expect_error(assign_refit_dates(observations, rev(dates)), "sorted")
})

testthat::test_that("rolling RMSE uses complete calendar windows without future errors", {
  observations <- tibble::tibble(month_id = as.Date(c("2020-01-01", "2020-02-01", "2020-03-01")),
                                  target = c(0, 0, 0), pred = c(1, 3, 100))
  actual <- rolling_forecast_rmse(observations, character(), 2L)
  testthat::expect_true(is.na(actual$rmse[1]))
  testthat::expect_equal(actual$rmse[2], sqrt(5))
  observations <- observations[-2, ]
  testthat::expect_true(all(is.na(rolling_forecast_rmse(observations, character(), 2L)$rmse)))
})

testthat::test_that("theme loadings exclude intercepts and preserve signed cancellation", {
  importance <- tibble::tibble(model = "harx", universe = "panel",
    dates = as.Date("2020-01-01"), feat = c("(Intercept)", "rv_month_lag1", "rv_tw_lag1", "vix_lag1"),
    imp = c(100, 2, -1, 1))
  actual <- summarise_theme_loadings(importance, report_theme_map())
  testthat::expect_equal(sum(actual$gross_share), 1)
  testthat::expect_equal(actual$net_share[actual$theme == "Own-series RV terms"], 0.25)
  testthat::expect_equal(actual$gross_share[actual$theme == "Own-series RV terms"], 0.75)
  importance$feat[2] <- "unmapped_predictor"
  testthat::expect_error(summarise_theme_loadings(importance, report_theme_map()), "Unmapped")
})

testthat::test_that("paper captions bold the title sentence", {
  actual <- format_report_caption("Forecast performance. Lower values are better.")
  testthat::expect_match(actual$full, "\\\\textbf\\{: Forecast performance\\.\\}")
  testthat::expect_match(actual$full, "Lower values are better\\.")
  testthat::expect_error(format_report_caption(character()), "one nonempty")
})

testthat::test_that("panel tables preserve aligned columns and panel labels", {
  path <- tempfile(fileext = ".tex")
  panels <- list("Panel A. First sample" = tibble::tibble(Model = "HAR", RMSE = 1),
                 "Panel B. Second sample" = tibble::tibble(Model = "EW", RMSE = 0.9))
  write_report_panel_table(panels, path, "Forecast performance. Details.", "tab:test")
  output <- paste(readLines(path), collapse = "\n")
  testthat::expect_match(output, "Panel A\\. First sample")
  testthat::expect_match(output, "Panel B\\. Second sample")
  testthat::expect_match(output, "\\\\textbf\\{: Forecast performance\\.\\}")
  panels[[2]] <- tibble::tibble(Model = "EW", MAE = 0.8)
  testthat::expect_error(write_report_panel_table(panels, path, "Caption.", "tab:test"), "same columns")
})

testthat::test_that("cluster tables identify HAR improvements and row winners", {
  path <- tempfile(fileext = ".tex")
  data <- tibble::tibble(cluster = "Value", Factor = c("Cluster aggregate", "factor_a"),
    row_type = c("aggregate", "factor"), HAR = c(1, 1), RF = c(0.8, 1.1), EW = c(0.9, 0.7))
  write_cluster_metric_table(data, path, "Cluster forecasts. Details.", "tab:test", "min")
  output <- paste(readLines(path), collapse = "\n")
  testthat::expect_match(output, "Panel A\\. Value")
  testthat::expect_match(output, "\\\\cellcolor\\{blue!10\\}0\\.8000")
  testthat::expect_match(output, "\\\\textbf\\{\\\\cellcolor\\{blue!10\\}0\\.7000\\}")
  testthat::expect_error(
    write_cluster_metric_table(dplyr::select(data, -HAR), path, "Caption.", "tab:test", "min"),
    "HAR")
})

testthat::test_that("portfolio summaries use excess returns and total-return drawdowns", {
  data <- tibble::tibble(
    universe = "Panel",
    strategy = "EW",
    month_id = as.Date(c("2020-01-01", "2020-02-01", "2020-03-01")),
    excess_return = c(.01, -.02, .03),
    total_return = c(.011, -.019, .031)
  )
  actual <- summarise_portfolio_returns(data, c("universe", "strategy"))
  expected_wealth <- cumprod(1 + data$total_return)
  expected_drawdown <- min(expected_wealth / cummax(c(1, expected_wealth))[-1] - 1)
  testthat::expect_equal(actual$annualized_excess_return, 12 * mean(data$excess_return))
  testthat::expect_equal(actual$annualized_volatility, sqrt(12) * stats::sd(data$excess_return))
  testthat::expect_equal(actual$sharpe_ratio,
    sqrt(12) * mean(data$excess_return) / stats::sd(data$excess_return))
  testthat::expect_equal(actual$max_drawdown, expected_drawdown)
  data$total_return[2] <- -1
  testthat::expect_error(summarise_portfolio_returns(data, c("universe", "strategy")), "above -100")
})

testthat::test_that("portfolio aggregation equally weights factors within each month", {
  data <- tidyr::expand_grid(
    universe = "Panel", strategy = "EW", name = c("a", "b"),
    month_id = as.Date(c("2020-01-01", "2020-02-01"))
  ) %>%
    dplyr::mutate(excess_return = c(.01, .03, .02, .04),
      total_return = excess_return + .001)
  actual <- aggregate_portfolio_returns(
    data, c("universe", "strategy"), c("excess_return", "total_return"))
  expected <- data %>% dplyr::group_by(universe, strategy, month_id) %>%
    dplyr::summarise(dplyr::across(c(excess_return, total_return), mean), .groups = "drop")
  testthat::expect_equal(actual, expected)
  testthat::expect_error(
    aggregate_portfolio_returns(data[-1, ], c("universe", "strategy"),
      c("excess_return", "total_return")),
    "balanced")
})

testthat::test_that("cluster tables can compare learning models with several benchmarks", {
  path <- tempfile(fileext = ".tex")
  data <- tibble::tibble(
    cluster = "Value", Factor = "Cluster aggregate", row_type = "aggregate",
    Unmanaged = .4, RW = .5, HAR = .45, HARX = .55, ENET = .54, EW = .6
  )
  write_cluster_metric_table(data, path, "Portfolio Sharpe ratios. Details.", "tab:test",
    direction = "max", benchmark_columns = c("Unmanaged", "RW", "HAR", "HARX"),
    shade_columns = c("ENET", "EW"))
  output <- paste(readLines(path), collapse = "\n")
  testthat::expect_false(grepl("cellcolor\\{blue!10\\}0.5400", output))
  testthat::expect_match(output, "\\\\textbf\\{\\\\cellcolor\\{blue!10\\}0.6000\\}")
})
