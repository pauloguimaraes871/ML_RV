base::source(base::file.path(here::here(), "R", "portfolio_inference_helpers.R"))
base::source(base::file.path(here::here(), "R", "portfolio_inference_inputs.R"))

testthat::test_that("saved net return adapters preserve raw, capped and collateral units", {
  data <- tibble::tibble(month_id = base::as.Date("2011-07-01"), rf = .001,
    r_exc_fwd1_raw = .03, r_exc_fwd1_capped = .02,
    tc_raw = .004, tc = .002, r_exc_fwd1_net_raw = .026,
    r_exc_fwd1_net_capped = .018, r_strat_fwd1_net_raw = .027,
    r_strat_fwd1_net_capped = .019)
  raw <- extract_portfolio_inference_returns(data, "mm", "raw")
  capped <- extract_portfolio_inference_returns(data, "mm", "capped")
  testthat::expect_equal(raw$excess_return, .026)
  testthat::expect_equal(raw$total_return, .027)
  testthat::expect_equal(capped$excess_return, .018)
  testthat::expect_equal(capped$total_return, .019)
  allocation <- tibble::tibble(month_id = data$month_id, rf = .001,
    gross_raw = .03, net_raw = .026, tc_raw = .004,
    gross_capped = .02, net_capped = .018, tc_capped = .002)
  testthat::expect_equal(extract_portfolio_inference_returns(allocation, "allocation", "raw")$total_return, .027)
  testthat::expect_equal(extract_portfolio_inference_returns(allocation, "allocation", "capped")$total_return, .019)
  data$r_exc_fwd1_net_raw <- .03
  testthat::expect_error(extract_portfolio_inference_returns(data, "mm", "raw"), "less nonnegative costs")
  allocation$net_raw <- -1.002
  allocation$gross_raw <- -1.002
  allocation$tc_raw <- 0
  testthat::expect_error(extract_portfolio_inference_returns(allocation, "allocation", "raw"), "Insolvent")
})

testthat::test_that("benchmark copies are aligned by keys and must agree before deduplication", {
  data <- tibble::tibble(name = base::c("b", "a"), month_id = base::as.Date("2011-07-01"),
    value = base::c(.01, -.02))
  testthat::expect_true(check_portfolio_benchmark_copy(data, data[2:1, ], base::c("name", "month_id")))
  bad <- data
  bad$value[1] <- .011
  testthat::expect_error(check_portfolio_benchmark_copy(data, bad, base::c("name", "month_id")), "cannot be deduplicated")
  testthat::expect_error(check_portfolio_benchmark_copy(data, data[1, ], base::c("name", "month_id")), "cannot be deduplicated")
  testthat::expect_error(check_portfolio_benchmark_copy(data, dplyr::bind_rows(data, data[1, ]),
    base::c("name", "month_id")), "unique complete keys")
})
