base::source(base::file.path(here::here(), "R", "portfolio_helpers.R"))

make_port_fixture <- function() {
  dates <- base::seq(base::as.Date("2000-01-01"), by = "month", length.out = 18)
  target <- tidyr::expand_grid(name = c("a", "b"), month_id = dates) |>
    dplyr::mutate(rv_month = base::rep(c(.01, .04, .02), 12) *
                    dplyr::if_else(name == "a", 1, 10))
  forecasts <- target |>
    dplyr::filter(month_id >= dates[7]) |>
    dplyr::mutate(target = base::log(rv_month + 1e-8),
                  pred = base::log(rv_month / 2 + 1e-8), error = target - pred) |>
    dplyr::select(name, month_id, target, pred, error)
  returns <- target |>
    dplyr::mutate(factor_exc = base::rep(c(.02, -.01, .04, -.03, .01, .03), 6) *
                    dplyr::if_else(name == "a", 1, 2), rf = .002) |>
    dplyr::select(name, month_id, factor_exc, rf)
  base::list(oos_outputs = forecasts, factor_returns = returns, target_gold = target,
             min_history = 3L)
}

testthat::test_that("each factor uses its own past RV, prediction and return", {
  f <- make_port_fixture()
  p <- base::do.call(run_oos_ports, f)$w_compiled_df
  first <- p |> dplyr::filter(method == "vol_scaled", month_id == base::as.Date("2000-07-01"))
  testthat::expect_equal(first$normalizer, c(.07 / 3, .7 / 3))
  testthat::expect_equal(first$w_raw, base::rep((.07 / 3) / .005, 2))
  testthat::expect_equal(first$r_exc_fwd1_raw, first$w_raw * c(.02, .04))
  testthat::expect_equal(first$r_strat_fwd1_raw, .002 + first$r_exc_fwd1_raw)
  testthat::expect_setequal(base::unique(p$method), c("vol_scaled", "rw", "buyhold"))
  testthat::expect_false(base::anyNA(p$w_raw))
  f$oos_outputs <- f$oos_outputs[base::rev(base::seq_len(base::nrow(f$oos_outputs))), ]
  f$factor_returns <- f$factor_returns[base::rev(base::seq_len(base::nrow(f$factor_returns))), ]
  testthat::expect_equal(base::do.call(run_oos_ports, f)$w_compiled_df, p)
})

testthat::test_that("future targets, returns and predictions cannot change past OOS decisions", {
  for (normalization in c("expanding_rv", "expanding_vol")) {
    f <- make_port_fixture()
    f$normalization <- normalization
    original <- base::do.call(run_oos_ports, f)$w_compiled_df
    cutoff <- base::as.Date("2001-03-01")
    late <- f$oos_outputs$month_id >= cutoff
    f$oos_outputs$pred[late] <- f$oos_outputs$pred[late] + 1
    f$oos_outputs$target[late] <- f$oos_outputs$target[late] + 1
    f$target_gold$rv_month[f$target_gold$month_id >= cutoff] <-
      base::exp(f$oos_outputs$target[late]) - 1e-8
    f$factor_returns$factor_exc[f$factor_returns$month_id >= cutoff] <- .5
    changed <- base::do.call(run_oos_ports, f)$w_compiled_df
    testthat::expect_equal(dplyr::filter(changed, month_id < cutoff),
                           dplyr::filter(original, month_id < cutoff))
  }
})

testthat::test_that("raw and capped trading costs are independent and inception is explicit", {
  f <- make_port_fixture()
  p <- base::do.call(run_oos_ports, f)$w_compiled_df |>
    dplyr::filter(name == "a", method == "vol_scaled")
  testthat::expect_equal(p$turnover_raw, base::abs(p$w_raw - c(0, utils::head(p$w_raw, -1))))
  testthat::expect_equal(p$turnover, base::abs(p$w_capped - c(0, utils::head(p$w_capped, -1))))
  testthat::expect_equal(p$r_exc_fwd1_net_raw, p$r_exc_fwd1_raw - .001 * p$turnover_raw)
  testthat::expect_equal(p$r_exc_fwd1_net_capped, p$r_exc_fwd1_capped - .001 * p$turnover)
  testthat::expect_true(base::any(p$tc_raw != p$tc))
  f$initial_weight <- 1
  b <- base::do.call(run_oos_ports, f)$w_compiled_df |> dplyr::filter(method == "buyhold")
  testthat::expect_equal(b$tc, base::rep(0, base::nrow(b)))
  f$cost_per_unit <- 0
  p <- base::do.call(run_oos_ports, f)$w_compiled_df
  testthat::expect_equal(p$r_exc_fwd1_net_raw, p$r_exc_fwd1_raw)
})

testthat::test_that("risk matching is past-only and full-sample matching is labeled ex post", {
  f <- make_port_fixture()
  f$normalization <- "expanding_vol"
  result <- base::do.call(run_oos_ports, f)
  testthat::expect_equal(result$coverage$n_warmup, c(3L, 3L))
  p <- result$w_compiled_df |> dplyr::filter(name == "a", method == "vol_scaled")
  r <- f$factor_returns |> dplyr::filter(name == "a", month_id %in% f$oos_outputs$month_id)
  v <- f$oos_outputs |> dplyr::filter(name == "a")
  testthat::expect_equal(p$normalizer[1], stats::sd(r$factor_exc[1:3]) /
    stats::sd(r$factor_exc[1:3] / (base::exp(v$pred[1:3]) - 1e-8)))
  f$normalization <- "full_sample"
  result <- base::do.call(run_oos_ports, f)
  testthat::expect_false(result$settings$implementable)
  testthat::expect_equal(result$strategy_summary$sd_return_raw,
                         base::rep(result$strategy_summary$sd_return_raw[c(1, 4)], each = 3))
})

testthat::test_that("summaries use excess volatility and paired benchmark dates", {
  f <- make_port_fixture()
  f$factor_returns$rf <- base::rep(c(.001, .02, .04), 12)
  result <- base::do.call(run_oos_ports, f)
  p <- result$w_compiled_df |> dplyr::filter(name == "a", method == "vol_scaled")
  s <- result$strategy_summary |> dplyr::filter(name == "a", method == "vol_scaled")
  testthat::expect_equal(s$sharpe_ratio_raw, base::mean(p$r_exc_fwd1_raw) /
                          stats::sd(p$r_exc_fwd1_raw) * base::sqrt(12))
  testthat::expect_equal(s$cer_ann_raw, 12 * (base::mean(p$r_exc_fwd1_raw) -
                          5 / 2 * stats::var(p$r_exc_fwd1_raw)))
  testthat::expect_equal(s$n, base::nrow(p))
})

testthat::test_that("invalid keys, scales, gaps, history and parameters fail explicitly", {
  original <- make_port_fixture()
  f <- original; f$oos_outputs <- f$oos_outputs[0, ]
  testthat::expect_error(base::do.call(run_oos_ports, f), "empty")
  f <- original; f$oos_outputs <- dplyr::bind_rows(f$oos_outputs, f$oos_outputs[1, ])
  testthat::expect_error(base::do.call(run_oos_ports, f), "Duplicate")
  f <- original; f$oos_outputs$pred[1] <- Inf
  testthat::expect_error(base::do.call(run_oos_ports, f), "finite")
  f <- original; f$oos_outputs$pred[1] <- -100
  testthat::expect_error(base::do.call(run_oos_ports, f), "positive")
  f <- original; f$target_gold$rv_month[1] <- 0
  testthat::expect_error(base::do.call(run_oos_ports, f), "positive")
  f <- original; f$target_gold$rv_month[7] <- .5
  testthat::expect_error(base::do.call(run_oos_ports, f), "targets disagree")
  f <- original; f$factor_returns <- f$factor_returns[-7, ]
  testthat::expect_error(base::do.call(run_oos_ports, f), "coverage|consecutive")
  f <- original; f$oos_outputs <- f$oos_outputs[-2, ]
  testthat::expect_error(base::do.call(run_oos_ports, f), "consecutive")
  f <- original; f$min_history <- 100L
  testthat::expect_error(base::do.call(run_oos_ports, f), "history")
  for (bad in c(-1, NA_real_, Inf)) {
    f <- original; f$cost_per_unit <- bad
    testthat::expect_error(base::do.call(run_oos_ports, f), "cost_per_unit")
  }
  f <- original; f$normalization <- "expanding_vol"; f$factor_returns$factor_exc <- 0
  testthat::expect_error(base::do.call(run_oos_ports, f), "variance|history")
})

testthat::test_that("daily factor and market returns are compounded within month", {
  dates <- base::as.Date(c("2000-01-03", "2000-01-04", "2000-02-01", "2000-02-02"))
  daily <- tibble::tibble(name = "a", date = dates, ret = c(.1, -.1, .2, -.1))
  market <- tibble::tibble(date = dates, ret = c(.1, -.1, .2, -.1))
  rf <- tibble::tibble(month_id = base::as.Date(c("2000-01-01", "2000-02-01")), rf = .001)
  p <- prepare_factor_returns(daily, market, rf)
  testthat::expect_equal(p$factor_exc[p$name == "a"], c(-.01, .08))
  testthat::expect_equal(p$factor_exc[p$name == "spx_vw"], c(-.011, .079))
  testthat::expect_error(prepare_factor_returns(dplyr::bind_rows(daily, daily[1, ]), market, rf), "Duplicate")
  testthat::expect_error(prepare_factor_returns(daily, market, rf[-1, ]), "risk-free")
})

testthat::test_that("historical gaps are counted and a lag never bridges a missing month", {
  f <- make_port_fixture()
  f$target_gold <- f$target_gold[-6, ]
  f$factor_returns <- f$factor_returns[-6, ]
  result <- base::do.call(run_oos_ports, f)
  testthat::expect_equal(result$coverage$historical_missing_months, c(1L, 0L))
  testthat::expect_equal(result$coverage$n_warmup, c(1L, 0L))
  a <- result$w_compiled_df |> dplyr::filter(name == "a", method == "rw")
  testthat::expect_equal(a$month_id[1], base::as.Date("2000-08-01"))
  testthat::expect_equal(a$signal_rv[1], .01)
})

testthat::test_that("one factor cannot change the calibration of another", {
  f <- make_port_fixture()
  original <- base::do.call(run_oos_ports, f)$w_compiled_df |> dplyr::filter(name == "b")
  f$target_gold$rv_month[f$target_gold$name == "a"] <- 20
  f$oos_outputs$target[f$oos_outputs$name == "a"] <- base::log(20 + 1e-8)
  f$oos_outputs$pred[f$oos_outputs$name == "a"] <- base::log(2 + 1e-8)
  changed <- base::do.call(run_oos_ports, f)$w_compiled_df |> dplyr::filter(name == "b")
  testthat::expect_equal(changed, original)
})

testthat::test_that("zero volatility is reported as undefined Sharpe without dropping returns", {
  f <- make_port_fixture(); f$factor_returns$factor_exc <- 0
  result <- base::do.call(run_oos_ports, f)
  testthat::expect_true(base::all(base::is.na(result$strategy_summary$sharpe_ratio_raw)))
  testthat::expect_true(base::all(result$strategy_summary$n == 12L))
})

testthat::test_that("invalid dates, classes, single rows, cap and return inputs fail", {
  original <- make_port_fixture()
  f <- original; f$oos_outputs$month_id <- base::as.character(f$oos_outputs$month_id)
  testthat::expect_error(base::do.call(run_oos_ports, f), "Date")
  f <- original; f$oos_outputs$month_id[1] <- f$oos_outputs$month_id[1] + 1
  testthat::expect_error(base::do.call(run_oos_ports, f), "first-of-month")
  f <- original; f$oos_outputs$pred <- base::as.character(f$oos_outputs$pred)
  testthat::expect_error(base::do.call(run_oos_ports, f), "numeric")
  f <- original; f$oos_outputs$pred[1] <- 1000
  testthat::expect_error(base::do.call(run_oos_ports, f), "finite and positive")
  f <- original; f$oos_outputs <- f$oos_outputs[1, ]
  testthat::expect_error(base::do.call(run_oos_ports, f), "history")
  f <- original; f$factor_returns$rf[1] <- .03
  testthat::expect_error(base::do.call(run_oos_ports, f), "agree")
  f <- original; f$factor_returns$factor_exc[1] <- NA_real_
  testthat::expect_error(base::do.call(run_oos_ports, f), "finite")
  f <- original; f$weight_cap <- .5
  testthat::expect_error(base::do.call(run_oos_ports, f), "weight_cap")
  f <- original; f$min_history <- 3.5
  testthat::expect_error(base::do.call(run_oos_ports, f), "integer")
  f <- original; f$initial_weight <- 2
  testthat::expect_error(base::do.call(run_oos_ports, f), "initial_weight")
})

testthat::test_that("French snapshot parses monthly RF and converts percentage units once", {
  rf <- read_french_rf(base::file.path(here::here(), "data", "F-F_Research_Data_Factors_CSV.zip"))
  testthat::expect_equal(rf$month_id[1], base::as.Date("1926-07-01"))
  testthat::expect_equal(rf$rf[1], .0022)
  testthat::expect_false(base::anyDuplicated(rf$month_id) > 0)
  testthat::expect_error(read_french_rf("missing-rf-snapshot.zip"), "Missing")
})
