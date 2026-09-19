base::source(base::file.path(here::here(), "R", "portfolio_inference_helpers.R"))

inference_fixture <- function(n = 60L) {
  dates <- base::seq(base::as.Date("2011-07-01"), by = "month", length.out = n)
  r <- .006 + .025 * base::sin(base::seq_len(n))
  dplyr::bind_rows(
    tibble::tibble(month_id = dates, series_id = "benchmark", excess_return = r, total_return = r),
    tibble::tibble(month_id = dates, series_id = "strategy", excess_return = r + .001, total_return = r + .001))
}

inference_contrasts <- function() {
  tibble::tibble(comparison_id = "constant_increment", family = "primary",
    strategy = "strategy", benchmark = "benchmark")
}

testthat::test_that("stationary calendars are reproducible and preserve caller RNG", {
  dates <- base::sort(base::unique(inference_fixture()$month_id))
  base::set.seed(41)
  old <- .Random.seed
  first <- draw_portfolio_months(dates, draws = 1000L)
  testthat::expect_identical(.Random.seed, old)
  testthat::expect_identical(first, draw_portfolio_months(dates, draws = 1000L))
  testthat::expect_false(base::identical(first$indices, draw_portfolio_months(dates, draws = 1000L, seed = 2L)$indices))
  testthat::expect_true(base::all(first$indices >= 1L & first$indices <= 60L))
  # A geometric restart occurs with probability 1/5, but may coincidentally
  # choose the next observation. Thus continuation probability is .8 + .2/60.
  continued <- first$indices[, -1] == first$indices[, -60] %% 60L + 1L
  testthat::expect_equal(base::mean(continued), .8 + .2 / 60, tolerance = .01)
  testthat::expect_error(draw_portfolio_months(dates[-2]), "consecutive")
  testthat::expect_error(draw_portfolio_months(dates, seed = NA_integer_), "seed")
  testthat::expect_error(draw_portfolio_months(dates, block_length = 0), "block_length")
  testthat::expect_error(draw_portfolio_months(dates, draws = 1), "draws")
})

testthat::test_that("paired bootstrap preserves constant paired increments and annual units", {
  data <- inference_fixture()
  calendar <- draw_portfolio_months(base::sort(base::unique(data$month_id)), draws = 499L)
  actual <- infer_portfolio_pairs(data, inference_contrasts(), calendar)
  mean <- dplyr::filter(actual, metric == "mean_ann")
  cer <- dplyr::filter(actual, metric == "cer_ann")
  testthat::expect_equal(mean$estimate, .012, tolerance = 1e-12)
  testthat::expect_equal(base::c(mean$ci_lower, mean$ci_upper), base::rep(.012, 2), tolerance = 1e-12)
  testthat::expect_equal(cer$estimate, .012, tolerance = 1e-12)
  testthat::expect_equal(mean$p_value, 1 / 500)
  testthat::expect_equal(actual$p_holm, stats::p.adjust(actual$p_value, "holm"))
  testthat::expect_error(infer_portfolio_pairs(data, inference_contrasts(), calendar, gamma = 0), "gamma")
  testthat::expect_error(infer_portfolio_pairs(data, inference_contrasts(), 1), "calendar")
  shuffled <- data[base::rev(base::seq_len(base::nrow(data))), ]
  testthat::expect_equal(infer_portfolio_pairs(shuffled, inference_contrasts(), calendar), actual)
  reverse <- dplyr::mutate(inference_contrasts(), strategy = "benchmark", benchmark = "strategy")
  reversed <- infer_portfolio_pairs(data, reverse, calendar)
  testthat::expect_equal(dplyr::filter(reversed, metric == "mean_ann")$p_value, 1)
})

testthat::test_that("identical strategies give zero differences and no rejection", {
  data <- inference_fixture()
  data$excess_return[data$series_id == "strategy"] <- data$excess_return[data$series_id == "benchmark"]
  calendar <- draw_portfolio_months(base::sort(base::unique(data$month_id)), draws = 199L)
  actual <- infer_portfolio_pairs(data, inference_contrasts(), calendar)
  testthat::expect_equal(actual$estimate, base::rep(0, 3))
  testthat::expect_equal(actual$ci_lower, base::rep(0, 3))
  testthat::expect_equal(actual$ci_upper, base::rep(0, 3))
  testthat::expect_equal(actual$p_value, base::rep(1, 3))
  testthat::expect_true(base::all(actual$status == "identical_strategies"))
  data$excess_return <- 0
  zero <- infer_portfolio_pairs(data, inference_contrasts(), calendar)
  testthat::expect_true(base::is.na(dplyr::filter(zero, metric == "sharpe")$p_value))
  testthat::expect_true(base::all(dplyr::filter(zero, metric != "sharpe")$p_value == 1))
})

testthat::test_that("inference rejects gaps, duplicate keys, invalid calendars and insolvency", {
  data <- inference_fixture()
  calendar <- draw_portfolio_months(base::sort(base::unique(data$month_id)), draws = 99L)
  testthat::expect_error(align_portfolio_inference(data[-1, ]), "coverage")
  testthat::expect_error(align_portfolio_inference(dplyr::bind_rows(data, data[1, ])), "unique")
  testthat::expect_error(align_portfolio_inference(data[0, ]), "nonempty")
  gap <- dplyr::filter(data, month_id != base::as.Date("2012-01-01"))
  testthat::expect_error(align_portfolio_inference(gap), "consecutive")
  bad <- data
  bad$total_return[1] <- -1
  testthat::expect_error(align_portfolio_inference(bad), "Insolvent")
  bad$total_return[1] <- NA_real_
  testthat::expect_error(align_portfolio_inference(bad), "finite")
  calendar$indices[1, 1] <- 0L
  testthat::expect_error(infer_portfolio_pairs(data, inference_contrasts(), calendar), "calendar")
})

testthat::test_that("spanning regression recovers analytical orthogonal-fixture coefficients", {
  # Repeating balanced orthogonal shocks identify alpha=.001, beta=2,
  # residual RMSE=.01 and R-squared=.8 exactly.
  x <- base::rep(base::c(-.01, -.01, .01, .01), 15)
  error <- base::rep(base::c(-.01, .01, -.01, .01), 15)
  data <- tibble::tibble(month_id = base::seq(base::as.Date("2011-07-01"), by = "month", length.out = 60),
    benchmark_return = x, excess_return = .001 + 2 * x + error)
  actual <- span_portfolio_returns(data, lag = 0L)
  testthat::expect_equal(actual$alpha_ann, .012, tolerance = 1e-12)
  testthat::expect_equal(actual$beta, 2, tolerance = 1e-12)
  testthat::expect_equal(actual$r_squared, .8, tolerance = 1e-12)
  testthat::expect_equal(actual$appraisal_ratio, base::sqrt(12) * .1, tolerance = 1e-12)
  testthat::expect_equal(actual$alpha_se_ann, 12 * .01 / base::sqrt(58), tolerance = 1e-12)
  # For lag one, adjacent residual products sum to -59*.0001.
  testthat::expect_equal(span_portfolio_returns(data, lag = 1L)$alpha_se_ann,
    12 * .01 / base::sqrt(60 * 58), tolerance = 1e-12)
  data$excess_return <- data$benchmark_return
  testthat::expect_equal(span_portfolio_returns(data)$status, "perfect_fit")
  testthat::expect_true(base::is.na(span_portfolio_returns(data)$t_alpha))
  data$benchmark_return <- 0
  testthat::expect_error(span_portfolio_returns(data), "constant")
})
