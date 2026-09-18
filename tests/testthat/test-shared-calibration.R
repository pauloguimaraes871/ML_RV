base::source(base::file.path(here::here(), "R", "portfolio_helpers.R"))
base::source(base::file.path(here::here(), "R", "multifactor_helpers.R"))
base::source(base::file.path(here::here(), "R", "cederburg_helpers.R"))

shared_calibration_fixture <- function() {
  dates <- base::seq(base::as.Date("2000-01-01"), by = "month", length.out = 30)
  i <- base::seq_along(dates)
  target <- tibble::tibble(name = "a", month_id = dates,
    rv_month = .002 * (2 + base::sin(i / 2)))
  returns <- dplyr::mutate(target, factor_exc = .005 + .04 * base::cos(i), rf = .001)
  forecasts <- target %>% dplyr::slice(7:30) %>%
    dplyr::mutate(target = base::log(rv_month + 1e-8),
      pred = base::log(rv_month * 1.2 + 1e-8))
  base::list(oos_outputs = forecasts, factor_returns = returns, target_gold = target,
    normalization = "expanding_vol", min_history = 6L)
}

testthat::test_that("MM and Cederburg share calibration without a second warm-up", {
  fixture <- shared_calibration_fixture()
  mm <- base::do.call(run_oos_ports, fixture)
  testthat::expect_lt(base::min(mm$calibration_data$month_id),
    base::min(mm$w_compiled_df$month_id))
  for (signal in c("vol_scaled", "rw")) {
    cb <- run_cederburg_ports(mm, managed_method = signal, min_history = 6L)
    comparison <- cb$weights %>% dplyr::filter(method == "ew") %>%
      dplyr::inner_join(mm$w_compiled_df %>% dplyr::filter(method == signal) %>%
        dplyr::select(name, month_id, mm_c = normalizer, mm_w = w_raw),
        by = c("name", "month_id"), relationship = "one-to-one")
    testthat::expect_equal(base::nrow(comparison), base::nrow(cb$weights) / 3)
    testthat::expect_equal(base::min(comparison$month_id), base::min(mm$w_compiled_df$month_id))
    testthat::expect_equal(comparison$normalizer, comparison$mm_c)
    testthat::expect_equal(comparison$scaler, comparison$mm_w)
    testthat::expect_equal(comparison$w_raw, .5 + .5 * comparison$mm_w)
  }
})

testthat::test_that("preserved warm-up signals support multifactor formation and remain causal", {
  fixture <- shared_calibration_fixture()
  mm <- base::do.call(run_oos_ports, fixture)
  d <- mm$calibration_data %>% dplyr::filter(method == "vol_scaled")
  p <- run_multifactor_ports(dplyr::select(d, name, month_id, factor_exc, rf),
    dplyr::transmute(d, month_id, sigma = base::sqrt(signal_rv)), "a",
    base::min(mm$w_compiled_df$month_id), allocation = "ew", min_history = 6L)
  testthat::expect_equal(base::min(p$monthly_returns$month_id), base::min(mm$w_compiled_df$month_id))
  cutoff <- base::as.Date("2001-07-01")
  fixture$factor_returns <- fixture$factor_returns %>%
    dplyr::mutate(factor_exc = dplyr::if_else(month_id >= cutoff, .2, factor_exc))
  changed <- base::do.call(run_oos_ports, fixture)
  for (signal in c("rw", "vol_scaled")) {
    before <- run_cederburg_ports(mm, managed_method = signal, min_history = 6L)$weights
    after <- run_cederburg_ports(changed, managed_method = signal, min_history = 6L)$weights
    testthat::expect_equal(dplyr::filter(before, month_id <= cutoff)$w_raw,
      dplyr::filter(after, month_id <= cutoff)$w_raw)
  }
})
