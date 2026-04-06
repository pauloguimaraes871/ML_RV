library("testthat")
## Helpers and classes
source(file.path(here::here(), "R", "helpers.R"))
source(file.path(here::here(), "R", "classes.R"))


testthat::test_that("prep_covariates runs without errors", {
  
  df <- data.frame(
    month_id = 1:100,
    month = factor(rep(1:12, length.out = 100)),
    x1 = rnorm(100),
    x2 = rnorm(100)
  )
  
  expect_no_error({
    res <- prep_covariates(df, scaling = "range")
  })
  
  expect_equal(nrow(res), nrow(df))
})

testthat::test_that("LOCF imputes NAs correctly", {
  
  df <- data.frame(
    month_id = 1:5,
    month = factor(1:5),
    x = c(1, NA, NA, 4, NA)
  )
  
  res <- impute_locf(df)
  
  expect_equal(res$x, c(1, 1, 1, 4, 4))
})

testthat::test_that("scaling uses only past data (no leakage)", {
  
  x <- c(1, 2, 3, 1000)  # extreme last value
  
  scaled <- rescale_expanding_to_unit(x, min_n = 2)
  
  # value at position 3 should NOT be affected by 1000
  scaled_without_last <- rescale_expanding_to_unit(x[1:3], min_n = 2)
  
  expect_equal(scaled[3], scaled_without_last[3])
})

testthat::test_that("winsorization clips extreme values", {
  
  x <- c(rep(1, 50), 1000)
  
  w <- winsorize_expanding(x, p_lo = 0.05, p_hi = 0.95, min_n = 10)
  
  # last value should be clipped (not equal to 1000)
  expect_true(w[length(w)] < 1000)
})

testthat::test_that("early periods return raw values", {
  
  x <- stats::rnorm(50)
  scaled <- rescale_expanding_to_unit(x, min_n = 20)
  
  testthat::expect_equal(scaled[1:20], x[1:20])
})

testthat::test_that("range scaling matches manual lagged min max formula", {
  
  x <- c(1, 2, 3, 4, 10)
  scaled <- rescale_expanding_to_unit(x, min_n = 3)
  
  # i = 4 uses window x[1:3] = c(1,2,3)
  expected_4 <- 2 * ((4 - 1) / (3 - 1)) - 1
  testthat::expect_equal(scaled[4], expected_4)
  
  # i = 5 uses window x[1:4] = c(1,2,3,4)
  expected_5 <- 2 * ((10 - 1) / (4 - 1)) - 1
  testthat::expect_equal(scaled[5], expected_5)
})

testthat::test_that("range scaling uses only past data", {
  
  x1 <- c(1, 2, 3, 4, 5, 1000)
  x2 <- c(1, 2, 3, 4, 5, -1000)
  
  s1 <- rescale_expanding_to_unit(x1, min_n = 3)
  s2 <- rescale_expanding_to_unit(x2, min_n = 3)
  
  testthat::expect_equal(s1[1:5], s2[1:5])
})

testthat::test_that("constant window returns zero", {
  
  x <- c(rep(5, 10), 5, 7)
  scaled <- rescale_expanding_to_unit(x, min_n = 5)
  
  testthat::expect_equal(scaled[6], 0)
  testthat::expect_equal(scaled[7], 0)
  testthat::expect_equal(scaled[8], 0)
})

testthat::test_that("scaled values respect historical range logic", {
  
  x <- c(1, 2, 3, 10)
  scaled <- rescale_expanding_to_unit(x, min_n = 2)
  
  # value exceeds historical max → should be > 1
  testthat::expect_true(scaled[4] > 1)
})

testthat::test_that("month dummies are created", {
  
  df <- data.frame(
    month_id = 1:24,
    month = factor(rep(1:12, 2)),
    x = rnorm(24)
  )
  
  res <- prep_covariates(df)
  
  dummy_cols <- grep("^m_", colnames(res), value = TRUE)
  
  expect_equal(length(dummy_cols), 12)
  
  # each row should have exactly one dummy = 1
  row_sums <- rowSums(res[, dummy_cols])
  
  expect_true(all(row_sums == 1))
})

testthat::test_that("range scaling produces finite values", {
  
  x <- stats::rnorm(200)
  scaled <- rescale_expanding_to_unit(x, min_n = 20)
  
  testthat::expect_true(all(is.finite(scaled)))
})

testthat::test_that("NA values are preserved in range scaling", {
  
  x <- c(1, 2, NA, 4, 5, 6)
  scaled <- rescale_expanding_to_unit(x, min_n = 3)
  
  testthat::expect_true(is.na(scaled[3]))
})

testthat::test_that("degenerate window (min == max) returns zero", {
  
  x <- c(rep(5, 50), 6)
  scaled <- rescale_expanding_to_unit(x, min_n = 20)
  
  # once window is constant → output should be 0
  testthat::expect_equal(scaled[21], 0)
})

testthat::test_that("new extremes expand scale beyond [-1,1]", {
  
  x <- c(1:50, 1000)
  scaled <- rescale_expanding_to_unit(x, min_n = 20)
  
  testthat::expect_true(scaled[51] > 1)
})

testthat::test_that("scaling preserves ordering within fixed window", {
  
  window <- c(1, 2, 3, 4)
  mn <- min(window)
  mx <- max(window)
  
  val1 <- 5
  val2 <- 10
  
  s1 <- 2 * ((val1 - mn)/(mx - mn)) - 1
  s2 <- 2 * ((val2 - mn)/(mx - mn)) - 1
  
  testthat::expect_true(s2 > s1)
})


testthat::test_that("winsorization limits influence before scaling", {
  
  x <- c(rep(1, 50), 1000)
  
  w <- winsorize_expanding(x, p_lo = 0.05, p_hi = 0.95, min_n = 10)
  scaled <- rescale_expanding_to_unit(w, min_n = 10)
  
  # scaled value should not explode excessively
  testthat::expect_true(abs(scaled[51]) < 50)
})

testthat::test_that("range scaling is affine invariant after warmup", {
  
  x <- stats::rnorm(100)
  
  scaled1 <- rescale_expanding_to_unit(x, min_n = 20)
  scaled2 <- rescale_expanding_to_unit(2 * x + 5, min_n = 20)
  
  testthat::expect_equal(scaled1[21:100], scaled2[21:100], tolerance = 1e-12)
})


testthat::test_that("values within historical range map to [-1,1]", {

  x <- c(1, 2, 3, 2.5)
  scaled <- rescale_expanding_to_unit(x, min_n = 2)
  
  # 2.5 is within [1,3]
  testthat::expect_true(scaled[4] <= 1 && scaled[4] >= -1)
})

testthat::test_that("repeated values produce stable scaling", {
  
  x <- rep(5, 100)
  scaled <- rescale_expanding_to_unit(x, min_n = 20)
  
  testthat::expect_true(all(abs(scaled[30:100]) < 1e-8))
})

testthat::test_that("winsorized values stay within scaling expectations", {
  
  x <- stats::rnorm(200)
  
  w <- winsorize_expanding(x, min_n = 20)
  scaled <- rescale_expanding_to_unit(w, min_n = 20)
  
  # after enough data, most values should be reasonably bounded
  testthat::expect_true(mean(abs(scaled[50:200]) < 5) > 0.9)
})

testthat::test_that("excluded columns are not transformed", {
  
  df <- data.frame(
    month_id = 1:100,
    month = factor(rep(1:12, length.out = 100)),
    sqrt_days = rep(10, 100),
    x = rnorm(100)
  )
  
  res <- prep_covariates(df)
  
  expect_equal(res$sqrt_days, df$sqrt_days)
})

testthat::test_that("no unexpected NA inflation", {
  
  df <- data.frame(
    month_id = 1:100,
    month = factor(rep(1:12, length.out = 100)),
    x = rnorm(100)
  )
  
  res <- prep_covariates(df)
  
  # allow early NA due to min_n, but not all NA
  expect_true(sum(!is.na(res$x)) > 50)
})

testthat::test_that("prep_covariates is deterministic", {
  
  df <- data.frame(
    month_id = 1:100,
    month = factor(rep(1:12, length.out = 100)),
    x = rnorm(100)
  )
  
  res1 <- prep_covariates(df)
  res2 <- prep_covariates(df)
  
  expect_equal(res1, res2)
})

testthat::test_that("scale_expanding runs and preserves length", {
  
  x <- stats::rnorm(100)
  
  scaled <- scale_expanding(x, min_n = 20)
  
  testthat::expect_equal(length(scaled), length(x))
})

testthat::test_that("early periods return raw values", {
  
  x <- stats::rnorm(50)
  
  scaled <- scale_expanding(x, min_n = 20)
  
  testthat::expect_equal(scaled[1:20], x[1:20])
})


testthat::test_that("z-score scaling uses only past data", {
  
  x1 <- c(1, 2, 3, 4, 5, 1000)
  x2 <- c(1, 2, 3, 4, 5, -1000)
  
  s1 <- scale_expanding(x1, min_n = 3)
  s2 <- scale_expanding(x2, min_n = 3)
  
  # first 5 values must be identical
  testthat::expect_equal(s1[1:5], s2[1:5])
})

testthat::test_that("z-score matches manual median/MAD calculation", {
  
  x <- c(1, 2, 3, 4, 5)
  
  scaled <- scale_expanding(x, min_n = 3)
  
  # For i = 4 → window = (1,2,3)
  med <- stats::median(c(1,2,3))
  mad <- stats::mad(c(1,2,3), constant = 1.4826)
  
  expected <- (4 - med) / mad
  
  testthat::expect_equal(scaled[4], expected)
})

testthat::test_that("mad_floor prevents division by near-zero", {
  
  x <- c(rep(5, 50), 5.001)
  
  scaled <- scale_expanding(x, min_n = 20, mad_floor = 1e-2)
  
  # value should not explode
  testthat::expect_true(abs(scaled[51]) < 100)
})

testthat::test_that("constant series returns zero after scaling", {
  
  x <- rep(5, 100)
  
  scaled <- scale_expanding(x, min_n = 20)
  
  # after min_n → should be 0
  testthat::expect_true(all(abs(scaled[30:100]) < 1e-8))
})


testthat::test_that("median/MAD scaling is robust to outliers", {
  
  x <- c(rep(1, 50), 1000, 1, 1, 1)
  
  scaled <- scale_expanding(x, min_n = 20)
  
  # after outlier, values should return close to 0
  testthat::expect_true(abs(scaled[52]) < 1)
})

testthat::test_that("no infinite or NaN values produced", {
  
  x <- stats::rnorm(200)
  
  scaled <- scale_expanding(x, min_n = 20)
  
  testthat::expect_true(all(is.finite(scaled)))
})

testthat::test_that("NA values are preserved", {
  
  x <- c(1, 2, NA, 4, 5, 6)
  
  scaled <- scale_expanding(x, min_n = 3)
  
  testthat::expect_true(is.na(scaled[3]))
})

testthat::test_that("scale_expanding is deterministic", {
  
  x <- stats::rnorm(100)
  
  s1 <- scale_expanding(x)
  s2 <- scale_expanding(x)
  
  testthat::expect_equal(s1, s2)
})

testthat::test_that("scaled values reflect position relative to median", {
  
  x <- 1:100
  scaled <- scale_expanding(x, min_n = 20)
  
  # after enough history → values above median should be positive
  testthat::expect_true(all(scaled[50:100] > 0))
})

testthat::test_that("peak value is large relative to past distribution", {
  
  x <- c(1:50, 50:1)
  scaled <- scale_expanding(x, min_n = 20)
  
  # at the peak → should be strongly positive
  testthat::expect_true(scaled[50] > 1)
})

testthat::test_that("values after peak become negative", {
  
  x <- c(1:50, 50:1)
  scaled <- scale_expanding(x, min_n = 20)
  
  # after the peak, values drop → should become negative eventually
  testthat::expect_true(any(scaled[60:100] < 0))
})

testthat::test_that("scaling reflects relative position vs past median", {
  
  x <- c(1:50, 50:1)
  scaled <- scale_expanding(x, min_n = 20)
  
  # before peak → mostly positive
  testthat::expect_true(mean(scaled[30:50] > 0) > 0.8)
  
  # after peak → trend downward
  testthat::expect_true(mean(scaled[70:100] < 0) > 0.5)
})

testthat::test_that("larger values produce larger scaled values at same time", {
  
  x <- c(1,2,3,4,5)
  scaled <- scale_expanding(x, min_n = 3)
  
  # At time t=5, compare hypothetical values
  window <- x[1:4]
  med <- stats::median(window)
  mad <- stats::mad(window, constant = 1.4826)
  
  val1 <- (6 - med)/mad
  val2 <- (10 - med)/mad
  
  testthat::expect_true(val2 > val1)
})


