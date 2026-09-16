base::source(base::file.path(here::here(), "R", "portfolio_helpers.R"))
base::source(base::file.path(here::here(), "R", "multifactor_helpers.R"))

make_multifactor_fixture <- function() {
  dates <- base::seq(base::as.Date("2000-01-01"), by = "month", length.out = 40)
  i <- base::seq_along(dates)
  returns <- dplyr::bind_rows(
    tibble::tibble(name = "a", month_id = dates, factor_exc = .005 + .03 * base::sin(i)),
    tibble::tibble(name = "b", month_id = dates, factor_exc = .003 + .02 * base::cos(i * 1.3))
  ) %>% dplyr::mutate(rf = .001)
  base::list(factor_returns = returns,
    signal = tibble::tibble(month_id = dates, sigma = .01 + .004 * base::sin(i / 2)),
    factor_names = c("a", "b"), evaluation_start = dates[21], min_history = 20L)
}

testthat::test_that("equation 6 agrees with the analytic single-basis solution", {
  x <- base::matrix(c(-.02, .01, .03, .04), ncol = 1)
  fit <- fit_multifactor(x, .001, gamma = 5)
  testthat::expect_equal(fit$coef, (base::mean(x) - .001) / (5 * stats::var(x[, 1])), tolerance = 1e-7)
  testthat::expect_equal(fit_multifactor(x, .1)$coef, 0)
  duplicate <- fit_multifactor(base::cbind(x, x), c(.001, .001))
  testthat::expect_equal(base::sum(duplicate$coef), fit$coef, tolerance = 1e-7)
  testthat::expect_error(fit_multifactor(base::matrix(1, 4, 2), c(0, 0)), "constant")
  testthat::expect_error(fit_multifactor(x, -1), "cost")
})

testthat::test_that("CMV nests UMV in the training objective including timing costs", {
  f <- make_multifactor_fixture()
  x <- base::matrix(dplyr::filter(f$factor_returns, name == "a")$factor_exc, ncol = 1)
  z <- 1 / f$signal$sigma
  umv <- fit_multifactor(x, 0)
  cmv <- fit_multifactor(base::cbind(x, x * z),
    c(0, .001 * base::mean(base::abs(base::diff(z)))))
  testthat::expect_gte(cmv$utility + 1e-8, umv$utility)
  costly <- fit_multifactor(base::cbind(x, x * z), c(0, 1e6))
  testthat::expect_equal(costly$coef[2], 0)
  testthat::expect_equal(costly$coef[1], umv$coef[1], tolerance = 1e-6)
})

testthat::test_that("market timing uses the previous calendar month's sample SD", {
  daily <- tibble::tibble(date = base::as.Date(c("2000-01-03", "2000-01-04", "2000-02-01", "2000-02-02")),
                          ret = c(.1, -.1, .2, -.2))
  signal <- prepare_market_sigma(daily)
  testthat::expect_equal(signal$month_id, base::as.Date(c("2000-02-01", "2000-03-01")))
  testthat::expect_equal(signal$sigma, c(stats::sd(c(.1, -.1)), stats::sd(c(.2, -.2))))
})

testthat::test_that("multifactor exposures, costs and evaluation are paired", {
  f <- make_multifactor_fixture()
  p <- base::do.call(run_multifactor_ports, f)
  w <- p$weights
  testthat::expect_true(base::all(w$a >= 0 & w$b >= 0))
  testthat::expect_equal(w$w_raw, w$a + w$b / w$sigma)
  testthat::expect_true(base::all(w$b[w$method == "umv"] == 0))
  testthat::expect_true(base::all(p$monthly_returns$exposure_capped <= 1.5 + 1e-10))
  first <- dplyr::filter(p$monthly_returns, month_id == base::min(month_id))
  testthat::expect_equal(first$turnover_raw, first$exposure_raw)
  testthat::expect_equal(first$turnover_capped, first$exposure_capped)
  testthat::expect_equal(p$monthly_returns$net_raw,
                         p$monthly_returns$gross_raw - .001 * p$monthly_returns$turnover_raw)
  testthat::expect_equal(p$strategy_summary$n, base::rep(20L, 4))
  testthat::expect_true(base::all(p$diagnostics$train_end < p$diagnostics$month_id))
  cash <- f
  cash$factor_returns$factor_exc <- -base::abs(cash$factor_returns$factor_exc)
  cash_result <- base::do.call(run_multifactor_ports, cash)
  testthat::expect_true(base::all(cash_result$weights$w_raw == 0))
  testthat::expect_true(base::all(cash_result$monthly_returns$net_capped == 0))
  testthat::expect_true(base::all(base::is.na(cash_result$strategy_summary$sharpe_net)))
  f$factor_returns <- f$factor_returns[base::rev(base::seq_len(base::nrow(f$factor_returns))), ]
  testthat::expect_equal(base::do.call(run_multifactor_ports, f)$weights, w)

  ## The held-out return can affect the payoff, never its formation weight.
  f$factor_returns$factor_exc[f$factor_returns$month_id >= base::as.Date("2003-01-01")] <- .5
  changed <- base::do.call(run_multifactor_ports, f)$weights
  testthat::expect_equal(
    dplyr::filter(changed, month_id <= base::as.Date("2003-01-01"))$w_raw,
    dplyr::filter(w, month_id <= base::as.Date("2003-01-01"))$w_raw
  )

  f <- make_multifactor_fixture()
  f$signal$sigma[f$signal$month_id > base::as.Date("2003-01-01")] <- .5
  changed <- base::do.call(run_multifactor_ports, f)$weights
  testthat::expect_equal(
    dplyr::filter(changed, month_id <= base::as.Date("2003-01-01"))$w_raw,
    dplyr::filter(w, month_id <= base::as.Date("2003-01-01"))$w_raw
  )
})

testthat::test_that("multifactor inputs reject missing keys, gaps and invalid signals", {
  f <- make_multifactor_fixture()
  f$signal$sigma[1] <- 0
  testthat::expect_error(base::do.call(run_multifactor_ports, f), "volatility")
  f <- make_multifactor_fixture()
  f$factor_returns <- f$factor_returns[-10, ]
  testthat::expect_error(base::do.call(run_multifactor_ports, f), "finite")
  f <- make_multifactor_fixture()
  f$min_history <- 39L
  testthat::expect_error(base::do.call(run_multifactor_ports, f), "Insufficient")
  f <- make_multifactor_fixture()
  f$factor_returns$rf[1] <- .002
  testthat::expect_error(base::do.call(run_multifactor_ports, f), "RF")
  f <- make_multifactor_fixture()
  f$factor_returns <- f$factor_returns[-40, ]
  testthat::expect_error(base::do.call(run_multifactor_ports, f), "end")
  f <- make_multifactor_fixture()
  f$signal <- dplyr::bind_rows(f$signal, f$signal[1, ])
  testthat::expect_error(base::do.call(run_multifactor_ports, f), "Duplicate")
  f <- make_multifactor_fixture()
  f$signal <- f$signal[-10, ]
  testthat::expect_error(base::do.call(run_multifactor_ports, f), "consecutive")
})
