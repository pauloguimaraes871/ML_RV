## Tests for the gold datasets: prep_covariates and the artifacts it produces.
##
## Three sections, in the order a build passes through them:
##   1. prep_covariates: LOCF, winsorizing, the expanding scalers, key
##      isolation, and the walk-forward split and run-matrix machinery
##   2. the transforms it applies: the scaler warm-up is discarded rather than
##      passed through raw, the variance-family predictors are logged before
##      winsorizing and scaling, and each series carries its own volatility
##      level, which the within-key scaling would otherwise erase
##   3. the artifacts on disk: integration checks that read what load_data.R
##      writes and assert the properties the fitting stage relies on
##
## R/load_data.R runs this file after writing the artifacts and refuses to
## finish if it fails. A failure in section 3 usually means the artifacts on
## disk predate a preprocessing change and load_data.R needs to be re-run.

library("testthat")
`%>%` <- dplyr::`%>%`
source(file.path(here::here(), "R", "helpers.R"))
source(file.path(here::here(), "R", "classes.R"))


# 1. prep_covariates =========================================================



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

  # The scaler warm-up is discarded, so the frame is shorter by min_n.
  expect_equal(nrow(res), nrow(df) - 36L)
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

  # Compared on the rows that survive the warm-up drop.
  expect_equal(res$sqrt_days, df$sqrt_days[-seq_len(36L)])
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


# Panel dataset tests------------------------------------------------------

testthat::test_that("implied vol is lagged AFTER the month-end observation is picked", {

  # Mirrors the VIX pipeline in load_data.R. The regression this guards against
  # is lagging the DAILY series and only then reducing to month level, which
  # silently turns the covariate into a within-month lag.
  daily <- tibble::tibble(
    dates   = as.Date(c("1990-01-15", "1990-01-30", "1990-01-31",
                        "1990-02-14", "1990-02-28")),
    vix_raw = c(10, 20, 30, 40, 50)
  )

  correct <- daily %>%
    dplyr::mutate(
      month_id = as.Date(paste(lubridate::year(dates),
                               lubridate::month(dates), "01", sep = "-"))
    ) %>%
    dplyr::group_by(month_id) %>%
    dplyr::filter(dates == max(dates)) %>%
    dplyr::arrange(month_id) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(vix_lag1 = dplyr::lag(vix_raw, 1))

  # February must see January's month-END value (30), not January's second-to-
  # last day (20) and not a within-February value.
  testthat::expect_equal(correct$vix_lag1[2], 30)

  # The wrong ordering is a different series, so this test genuinely bites.
  wrong <- daily %>%
    dplyr::mutate(
      month_id = as.Date(paste(lubridate::year(dates),
                               lubridate::month(dates), "01", sep = "-"))
    ) %>%
    dplyr::arrange(dates) %>%
    dplyr::mutate(vix_lag1 = dplyr::lag(vix_raw, 1)) %>%
    dplyr::group_by(month_id) %>%
    dplyr::filter(dates == max(dates)) %>%
    dplyr::ungroup()

  testthat::expect_equal(wrong$vix_lag1[2], 40)
  testthat::expect_false(identical(correct$vix_lag1, wrong$vix_lag1))
})


testthat::test_that("HAR blocks cannot cross a factor boundary", {

  # Two keys over the same calendar, with deliberately different return paths.
  dts <- seq(as.Date("2000-01-01"), by = "day", length.out = 400)
  panel <- dplyr::bind_rows(
    tibble::tibble(name = "A", date = dts, ret =  seq_along(dts) / 1000),
    tibble::tibble(name = "B", date = dts, ret = -seq_along(dts) / 1000)
  )

  build_blocks <- function(df) {
    purrr::reduce(
      generate_rv_feature_block(df, date_col = "date", ret_col = "ret"),
      dplyr::full_join,
      by = "month_id"
    )
  }

  grouped <- panel %>%
    dplyr::group_by(name) %>%
    dplyr::group_modify(~ build_blocks(.x)) %>%
    dplyr::ungroup()

  solo_b <- build_blocks(panel %>% dplyr::filter(name == "B"))

  # Computing B inside the panel must equal computing B on its own.
  testthat::expect_equal(
    as.data.frame(grouped %>%
                    dplyr::filter(name == "B") %>%
                    dplyr::select(-name)),
    as.data.frame(solo_b)
  )

  # And the ungrouped calculation must NOT agree, otherwise the check above
  # would pass even if the grouping were dropped.
  naive <- build_blocks(panel)
  testthat::expect_false(
    isTRUE(all.equal(as.data.frame(naive), as.data.frame(solo_b)))
  )
})


testthat::test_that("preprocessing at t is invariant to future observations", {

  df <- data.frame(
    month_id = seq(as.Date("1990-01-01"), by = "month", length.out = 80),
    month    = factor(rep(1:12, length.out = 80), levels = 1:12),
    x        = as.numeric(1:80)
  )

  res_full <- prep_covariates(df, scaling = "zscore", min_n = 6)

  # Replace the tail with extreme values; everything strictly before the first
  # changed row must be untouched, because every statistic is lagged expanding.
  df_perturbed      <- df
  df_perturbed$x[70:80] <- 1e6
  res_perturbed     <- prep_covariates(df_perturbed, scaling = "zscore",
                                       min_n = 6)

  # Aligned on month_id rather than position, because the warm-up drop shifts
  # the row indices and the invariant is about calendar time, not offsets.
  untouched <- df$month_id[1:69]
  testthat::expect_equal(
    res_full$x[res_full$month_id %in% untouched],
    res_perturbed$x[res_perturbed$month_id %in% untouched]
  )

  # The comparison must not be vacuous: some pre-perturbation rows survive.
  testthat::expect_gt(sum(res_full$month_id %in% untouched), 0L)
})


testthat::test_that("prep_covariates isolates keys when key_col is supplied", {

  months <- seq(as.Date("1990-01-01"), by = "month", length.out = 24)

  df <- data.frame(
    name     = rep(c("A", "B"), each = 24),
    month_id = rep(months, 2),
    month    = factor(rep(rep(1:12, 2), 2), levels = 1:12),
    # B opens with an NA so that a panel-wide LOCF would reach back into A
    x        = c(as.numeric(1:24), c(NA_real_, as.numeric(102:124)))
  )

  res <- prep_covariates(df, scaling = "zscore", min_n = 6, key_col = "name")

  # B's opening NA is inside the scaler warm-up and no longer survives to the
  # output, so it cannot be asserted on directly here. The isolation it was
  # written to catch is checked on impute_locf itself, where the logic lives,
  # in "leading NAs are not filled across keys" below.
  testthat::expect_true(nrow(res) > 0L)

  # Stronger invariant: B inside the panel is identical to B on its own. This
  # is what actually rules out A's history reaching B, through the fill or
  # through the scaling.
  res_b_alone <- prep_covariates(
    df %>% dplyr::filter(name == "B") %>% dplyr::select(-name),
    scaling = "zscore", min_n = 6
  )

  res_b_panel <- as.data.frame(
    res %>% dplyr::filter(name == "B") %>% dplyr::select(-name)
  )
  res_b_alone <- as.data.frame(res_b_alone)
  row.names(res_b_panel) <- NULL
  row.names(res_b_alone) <- NULL

  testthat::expect_equal(res_b_panel, res_b_alone)
})


testthat::test_that("a month is never split across temporal blocks", {

  months <- seq(as.Date("1990-01-01"), by = "month", length.out = 40)
  keys   <- c("A", "B", "C")

  panel <- tibble::tibble(
    month_id = rep(months, each = length(keys)),
    name     = rep(keys, times = length(months))
  ) %>%
    dplyr::mutate(x = seq_len(dplyr::n()) / 100)

  tgt <- panel %>% dplyr::transmute(month_id, name, rv_month = x * 2)

  dates <- sort(unique(panel$month_id))

  res <- split_ts(covariates = panel, target = tgt, dates = dates,
                  current_date = dates[30], train_n = 20, val_n = 5,
                  key_col = "name")

  # Every month that made it into a block brought all three keys with it.
  # Row-position slicing would have cut a month in half here.
  testthat::expect_true(
    all(dplyr::count(res$covariates_train, month_id)$n == 3)
  )
  testthat::expect_true(
    all(dplyr::count(res$covariates_val, month_id)$n == 3)
  )

  # Train and validation share no month at all.
  testthat::expect_length(
    intersect(res$covariates_train$month_id, res$covariates_val$month_id), 0
  )

  # The month being predicted is in none of the fitting blocks.
  testthat::expect_false(dates[30] %in% res$covariates_train$month_id)
  testthat::expect_false(dates[30] %in% res$covariates_refit$month_id)
})


testthat::test_that("split_ts joins the target on the key and drops ids", {

  months <- seq(as.Date("1990-01-01"), by = "month", length.out = 40)
  keys   <- c("A", "B", "C")

  panel <- tibble::tibble(
    month_id = rep(months, each = length(keys)),
    name     = rep(keys, times = length(months))
  ) %>%
    dplyr::mutate(x = seq_len(dplyr::n()) / 100)

  tgt <- panel %>% dplyr::transmute(month_id, name, rv_month = x * 2)
  dates <- sort(unique(panel$month_id))

  res <- split_ts(covariates = panel, target = tgt, dates = dates,
                  current_date = dates[30], train_n = 20, val_n = 5,
                  key_col = "name")

  # Joining on month_id alone would fan each covariate row out to three target
  # rows; the row count and the exact rv_month values both pin the key join.
  testthat::expect_equal(nrow(res$full_data_train_clean),
                         nrow(res$covariates_train))
  testthat::expect_equal(res$full_data_train_clean$rv_month,
                         res$full_data_train_clean$x * 2)

  # Neither the key nor month_id may reach the design matrix.
  testthat::expect_false("name"     %in% names(res$full_data_train_clean))
  testthat::expect_false("month_id" %in% names(res$full_data_train_clean))

  # The key is also stripped from the covariate frames handed to the fitters,
  # which is what lets fit_rv_model()'s covariates[, -1] convention stand.
  testthat::expect_false("name" %in% names(res$covariates_train))
  testthat::expect_false("name" %in% names(res$covariates_val))
  testthat::expect_false("name" %in% names(res$covariates_refit))
  testthat::expect_true("month_id" %in% names(res$covariates_train))
})


testthat::test_that("train_n and val_n count months, not rows", {

  months <- seq(as.Date("1990-01-01"), by = "month", length.out = 40)
  keys   <- c("A", "B", "C")

  panel <- tibble::tibble(
    month_id = rep(months, each = length(keys)),
    name     = rep(keys, times = length(months))
  ) %>%
    dplyr::mutate(x = seq_len(dplyr::n()) / 100)

  tgt   <- panel %>% dplyr::transmute(month_id, name, rv_month = x * 2)
  dates <- sort(unique(panel$month_id))

  # At the very first split, training runs to dates[train_n - 1] = 19 months.
  # With three keys that is 57 rows: the month count must not follow the panel
  # width.
  res <- split_ts(covariates = panel, target = tgt, dates = dates,
                  current_date = dates[25], train_n = 20, val_n = 5,
                  key_col = "name")

  testthat::expect_equal(dplyr::n_distinct(res$covariates_train$month_id), 19)
  testthat::expect_equal(nrow(res$covariates_train), 19 * length(keys))
})


testthat::test_that("a one-key panel reproduces the single-series split exactly", {

  months <- seq(as.Date("1990-01-01"), by = "month", length.out = 40)

  one_key <- tibble::tibble(
    month_id = months,
    name     = "A",
    x        = seq_along(months) / 100
  )
  tgt <- one_key %>% dplyr::transmute(month_id, name, rv_month = x * 2)
  dates <- sort(unique(one_key$month_id))

  with_key <- split_ts(covariates = one_key, target = tgt, dates = dates,
                       current_date = dates[30], train_n = 20, val_n = 5,
                       key_col = "name")

  without_key <- split_ts(
    covariates   = one_key %>% dplyr::select(-name),
    target       = tgt %>% dplyr::select(-name),
    dates        = dates,
    current_date = dates[30],
    train_n      = 20, val_n = 5,
    key_col      = NULL
  )

  testthat::expect_equal(as.data.frame(with_key$full_data_train_clean),
                         as.data.frame(without_key$full_data_train_clean))
  testthat::expect_equal(as.data.frame(with_key$covariates_refit),
                         as.data.frame(without_key$covariates_refit))
  testthat::expect_equal(with_key$target_val, without_key$target_val)
})


testthat::test_that("validator accepts a declared panel and rejects an undeclared one", {

  months <- seq(as.Date("1990-01-01"), by = "month", length.out = 40)
  keys   <- c("A", "B", "C")

  panel <- tibble::tibble(
    month_id = rep(months, each = length(keys)),
    name     = rep(keys, times = length(months))
  ) %>%
    dplyr::mutate(x = seq_len(dplyr::n()) / 100)

  tgt <- panel %>% dplyr::transmute(month_id, name, rv_month = x * 2)

  call_validator <- function(key_col) {
    validator(
      target = tgt, covariates = panel, model = "har",
      hyper_grid_domain_list = list(), tuning_method = "grid_search",
      obj_fun = "squared_error", eval_metric = "rmse",
      train_n = 20, val_n = 0, rebal_months = c(6),
      early_stop = NULL, gsm_algo = "ols",
      keras_architecture_pars = NULL, n_ensembles = 1L, parallel = FALSE,
      key_col = key_col
    )
  }

  # Declared: the repeated month_id values are legitimate panel rows.
  testthat::expect_no_error(call_validator("name"))

  # Undeclared: the same frame must be rejected rather than silently treated as
  # a single series with duplicated months.
  testthat::expect_error(call_validator(NULL), "duplicate")
})


testthat::test_that("fixed and tunable hyperparameter names cannot overlap", {

  testthat::expect_error(
    partition_hyper_domain(
      hyper_grid_domain_list = list(eta = c(0.01, 0.2), max_depth = c(2, 5)),
      fixed_params           = list(max_depth = 3L, subsample = 0.8),
      tuning_method          = "bayesian_optimization"
    ),
    "disjoint"
  )

  # Disjoint names are accepted.
  testthat::expect_no_error(
    partition_hyper_domain(
      hyper_grid_domain_list = list(eta = c(0.01, 0.2), max_depth = c(2, 5)),
      fixed_params           = list(subsample = 0.8),
      tuning_method          = "bayesian_optimization"
    )
  )
})


testthat::test_that("a constant cannot be smuggled in as a degenerate bound", {

  testthat::expect_error(
    partition_hyper_domain(
      hyper_grid_domain_list = list(eta = c(0.01, 0.2), subsample = c(0.8, 0.8)),
      tuning_method          = "bayesian_optimization"
    ),
    "degenerate"
  )

  # Unnamed entries and non-scalar constants are rejected too.
  testthat::expect_error(
    partition_hyper_domain(
      hyper_grid_domain_list = list(c(0.01, 0.2)),
      tuning_method          = "bayesian_optimization"
    ),
    "named"
  )
  testthat::expect_error(
    partition_hyper_domain(
      hyper_grid_domain_list = list(eta = c(0.01, 0.2)),
      fixed_params           = list(subsample = c(0.7, 0.8)),
      tuning_method          = "bayesian_optimization"
    ),
    "single values"
  )
  testthat::expect_error(
    partition_hyper_domain(
      hyper_grid_domain_list = list(eta = c(0.01, 0.2)),
      log10_params           = "nrounds",
      tuning_method          = "bayesian_optimization"
    ),
    "unknown"
  )
})


testthat::test_that("search-unit values are resolved to engine units", {

  res <- resolve_hyper_values(
    searched     = list(eta = -1, max_depth = 3.4),
    fixed        = list(nrounds = 1000L, subsample = 0.8),
    int_params   = c("max_depth", "nrounds"),
    log10_params = "eta"
  )

  # log10 bound of -1 becomes an eta of 0.1
  testthat::expect_equal(res$eta, 0.1)
  # non-integer search values are rounded
  testthat::expect_identical(res$max_depth, 3L)
  # constants come through untouched, and everything the engine needs is present
  testthat::expect_equal(res$subsample, 0.8)
  testthat::expect_setequal(names(res),
                            c("eta", "max_depth", "nrounds", "subsample"))
})


testthat::test_that("the Bayesian objective exposes only the searched names", {

  # Stand-in for the inner fit(): hard-coded formals covering every engine
  # argument, exactly like the closures set_eval_function() returns.
  inner <- function(eta, max_depth, nrounds, subsample) {
    list(Score = -eta, seen = list(eta = eta, max_depth = max_depth,
                                   nrounds = nrounds, subsample = subsample))
  }

  parts <- partition_hyper_domain(
    hyper_grid_domain_list = list(eta = c(-2, -0.8), max_depth = c(2, 5)),
    fixed_params           = list(nrounds = 1000L, subsample = 0.8),
    int_params             = c("max_depth", "nrounds"),
    log10_params           = "eta",
    tuning_method          = "bayesian_optimization"
  )

  objective <- make_hyper_objective(
    inner         = inner,
    tunable_names = names(parts$tunable),
    fixed         = parts$fixed,
    int_params    = parts$int_params,
    log10_params  = parts$log10_params
  )

  # bayesOpt() matches `bounds` against formals(FUN); the constants must not
  # appear there or they would be treated as search dimensions.
  testthat::expect_equal(names(formals(objective)), c("eta", "max_depth"))
  testthat::expect_setequal(names(formals(objective)), names(parts$tunable))

  # ...yet the engine still receives the complete argument set.
  out <- objective(eta = -1, max_depth = 4)
  testthat::expect_equal(out$seen$eta, 0.1)
  testthat::expect_identical(out$seen$max_depth, 4L)
  testthat::expect_identical(out$seen$nrounds, 1000L)
  testthat::expect_equal(out$seen$subsample, 0.8)
})


testthat::test_that("grid values stay in engine units and are not log-scaled", {

  parts <- partition_hyper_domain(
    hyper_grid_domain_list = list(eta = c(0.03, 0.06, 0.12)),
    fixed_params           = list(nrounds = 1000L),
    log10_params           = "eta",
    tuning_method          = "grid_search"
  )

  # A log10 declaration applies to continuous Bayesian bounds only. Applying it
  # to a grid would silently turn 0.03 into 10^0.03.
  testthat::expect_equal(parts$log10_params, character())
  testthat::expect_equal(parts$tunable$eta, c(0.03, 0.06, 0.12))
})


testthat::test_that("validator accepts constants supplied via fixed_params", {

  months <- seq(as.Date("1990-01-01"), by = "month", length.out = 60)
  cov_df <- tibble::tibble(month_id = months, x = seq_along(months) / 100)
  tgt_df <- tibble::tibble(month_id = months, rv_month = seq_along(months) / 50)

  call_validator <- function(domain, fixed) {
    validator(
      target = tgt_df, covariates = cov_df, model = "rf",
      hyper_grid_domain_list = domain, tuning_method = "bayesian_optimization",
      obj_fun = "squared_error", eval_metric = "rmse",
      train_n = 20, val_n = 10, rebal_months = c(6),
      early_stop = NULL, gsm_algo = "ols",
      keras_architecture_pars = NULL, n_ensembles = 1L, parallel = FALSE,
      fixed_params = fixed
    )
  }

  # ranger needs all four arguments, but only two of them are searched here.
  # The union is what has to be complete.
  testthat::expect_no_error(
    call_validator(
      domain = list(mtry = c(0.15, 0.6), max.depth = c(2L, 8L)),
      fixed  = list(num.trees = 1000L, min.bucket = 5L)
    )
  )

  # Dropping a required argument from both lists is still an error.
  testthat::expect_error(
    call_validator(
      domain = list(mtry = c(0.15, 0.6), max.depth = c(2L, 8L)),
      fixed  = list(num.trees = 1000L)
    ),
    "do not match"
  )

  # And a name present in both lists is rejected before any domain check.
  testthat::expect_error(
    call_validator(
      domain = list(mtry = c(0.15, 0.6), max.depth = c(2L, 8L)),
      fixed  = list(num.trees = 1000L, min.bucket = 5L, mtry = 0.3)
    ),
    "disjoint"
  )
})


testthat::test_that("cache lookup rejects an object when a material field changes", {

  base_prov <- list(
    dataset_version = "v3",
    universe        = "panel_1986",
    preprocessing   = "range",
    model           = "xgb",
    model_id        = "xgb",
    feature_set     = "harx",
    tuning_method   = "grid_search",
    tunable_domain  = list(eta = c(0.03, 0.06, 0.12)),
    fixed_params    = list(nrounds = 1000L, subsample = 0.8),
    train_n         = 120,
    val_n           = 90,
    seed            = 123
  )

  testthat::expect_true(provenance_matches(base_prov, base_prov))

  # Every material field must break the match on its own.
  material <- c("dataset_version", "universe", "preprocessing", "model",
                "model_id", "feature_set", "tuning_method", "train_n",
                "val_n", "seed")

  for (field in material) {
    changed <- base_prov
    changed[[field]] <- "SOMETHING-ELSE"
    testthat::expect_false(provenance_matches(base_prov, changed),
                           info = field)
    testthat::expect_true(field %in% provenance_diff(base_prov, changed),
                          info = field)
  }

  # Narrowing the search space is a different run.
  narrowed <- base_prov
  narrowed$tunable_domain <- list(eta = c(0.03, 0.06))
  testthat::expect_false(provenance_matches(base_prov, narrowed))

  # Moving a parameter between fixed and tunable is a different run, even
  # though the union of names is unchanged.
  moved <- base_prov
  moved$fixed_params   <- list(subsample = 0.8)
  moved$tunable_domain <- list(eta = c(0.03, 0.06, 0.12),
                               nrounds = c(1000L, 1000L))
  testthat::expect_false(provenance_matches(base_prov, moved))
})


testthat::test_that("descriptive fields never affect the cache decision", {

  base_prov <- list(
    dataset_version = "v3", universe = "panel_1986",
    model = "xgb", tuning_method = "grid_search", seed = 123
  )

  # Two identical runs differ in these; treating them as material would make
  # the cache useless.
  rerun <- base_prov
  rerun$timestamp     <- Sys.time()
  rerun$elapsed_time  <- 987.6
  rerun$backtest_id   <- "xgb_2026-05-19_07-32-37_rmse_squared_error"
  rerun$note          <- "re-run on a different machine"

  testthat::expect_true(provenance_matches(base_prov, rerun))
  testthat::expect_equal(provenance_diff(base_prov, rerun), character(0))
})


testthat::test_that("provenance, not the output path, identifies a run", {

  # The failure this guards against: two runs whose intended output paths
  # collide, distinguished only by a hand-assigned label. A filename such as
  # xgb_range_1927.rds proves nothing about which sample was fitted.
  shared_path <- "models_v3/1927/xgb_range_1927.rds"

  prov_1927 <- list(
    dataset_version = "v3", universe = "panel_1927", preprocessing = "range",
    model = "xgb", tuning_method = "grid_search", seed = 123,
    output_path = shared_path
  )
  prov_1986 <- prov_1927
  prov_1986$universe <- "panel_1986"

  # Same path, same filename, same everything a human would eyeball.
  testthat::expect_equal(prov_1927$output_path, prov_1986$output_path)

  # Still not the same run.
  testthat::expect_false(provenance_matches(prov_1927, prov_1986))
  testthat::expect_true("universe" %in% provenance_diff(prov_1927, prov_1986))

  # And the path itself carries no weight: changing only the path is a match.
  relocated <- prov_1927
  relocated$output_path <- "models_v3/1986/xgb_range_1927.rds"
  testthat::expect_true(provenance_matches(prov_1927, relocated))
})


testthat::test_that("the run signature is order-independent and deterministic", {

  a <- list(universe = "panel_1986", model = "rf", seed = 123)
  b <- list(seed = 123, model = "rf", universe = "panel_1986")

  testthat::expect_equal(compute_run_signature(a), compute_run_signature(b))
  testthat::expect_equal(compute_run_signature(a), compute_run_signature(a))

  # An absent field signs the same as an explicitly NULL one, so adding a field
  # nobody set cannot invalidate existing entries.
  c_prov <- a
  c_prov$acq <- NULL
  testthat::expect_equal(compute_run_signature(a), compute_run_signature(c_prov))
})


testthat::test_that("the run matrix holds exactly the approved runs", {

  rm_default <- build_run_matrix()

  # The approved model set, and nothing else. LSTM, NN4 and NN5 are out of the
  # current run plan even though their helpers still exist.
  testthat::expect_setequal(
    unique(rm_default$model_id),
    c("har", "harx", "glmnet", "rf", "xgb", "nn1", "nn2", "nn3")
  )
  testthat::expect_false(any(c("lstm", "nn4", "nn5") %in% rm_default$model_id))

  # Both preprocessing specifications, both tuning methods.
  testthat::expect_setequal(unique(rm_default$preprocessing),
                            c("range", "zscore"))
  testthat::expect_setequal(
    setdiff(unique(rm_default$tuning_method), NA_character_),
    c("grid_search", "bayesian_optimization")
  )

  # HAR and HARX are fitted once per preprocessing specification, with no
  # tuning method, rather than duplicated into fake grid and Bayesian copies.
  untuned <- rm_default %>% dplyr::filter(model_id %in% c("har", "harx"))
  testthat::expect_true(all(is.na(untuned$tuning_method)))
  testthat::expect_equal(nrow(untuned), 2 * 2 * 2)  # 2 models x 2 prep x 2 samples

  tuned <- rm_default %>% dplyr::filter(!model_id %in% c("har", "harx"))
  testthat::expect_false(any(is.na(tuned$tuning_method)))

  # 28 runs per (universe, sample): 2 untuned x 2 prep, plus 6 tuned x 2 prep
  # x 2 tuning methods. Defaults enable one universe and two samples.
  testthat::expect_equal(nrow(rm_default), 28 * 2)
  testthat::expect_setequal(unique(rm_default$universe), "panel")
  testthat::expect_setequal(unique(rm_default$sample), c("1986", "1927"))

  # No duplicated runs.
  testthat::expect_equal(nrow(dplyr::distinct(rm_default)), nrow(rm_default))
})


testthat::test_that("the run flags control the matrix without editing anything else", {

  # 1986 only
  only_1986 <- build_run_matrix(run_1927 = FALSE)
  testthat::expect_setequal(unique(only_1986$sample), "1986")
  testthat::expect_equal(nrow(only_1986), 28)

  # S&P-exclusive runs are off by default and appear when enabled
  testthat::expect_false("spx" %in% build_run_matrix()$universe)
  with_spx <- build_run_matrix(run_spx_only = TRUE)
  testthat::expect_setequal(unique(with_spx$universe), c("panel", "spx"))
  testthat::expect_equal(nrow(with_spx), 28 * 4)

  # Turning everything off yields no runs rather than an error
  testthat::expect_equal(
    nrow(build_run_matrix(run_1986 = FALSE, run_1927 = FALSE)), 0
  )
  testthat::expect_equal(
    nrow(build_run_matrix(run_panel = FALSE, run_spx_only = FALSE)), 0
  )
})


testthat::test_that("panel preprocessing rejects a month factor with dropped levels", {

  # model.matrix() codes `month` from its declared levels, so a key carrying a
  # short factor would silently produce fewer dummy columns and group_modify()
  # would pad the rest with NA. That must fail loudly instead.
  df <- data.frame(
    name     = rep(c("A", "B"), each = 6),
    month_id = rep(seq(as.Date("1990-01-01"), by = "month", length.out = 6), 2),
    month    = factor(rep(1:6, 2)),
    x        = as.numeric(1:12)
  )

  testthat::expect_error(prep_covariates(df, key_col = "name"))
})




# 2. The transforms it applies ===============================================


## A frame shaped like the real thing: a positive, right-skewed monthly term,
## a column carrying exact zeros, and a column that must not be touched.
make_df <- function(n = 120, seed = 1, key = NULL) {
  set.seed(seed)
  out <- data.frame(
    month_id  = seq(as.Date("1990-01-01"), by = "month", length.out = n),
    month     = factor(rep(1:12, length.out = n), levels = 1:12),
    rv_month_lag1 = exp(stats::rnorm(n, -6.5, 0.9)),
    rv_tw_lag1    = exp(stats::rnorm(n, -6.6, 1.0)),
    jump_td_lag1  = ifelse(stats::runif(n) < 0.6, 0, exp(stats::rnorm(n, -9, 1))),
    dfy_lag1      = stats::rnorm(n),
    sqrt_days     = rep(4.5, n)
  )
  if (!is.null(key)) out$name <- key
  out
}


# Warm-up ---------------------------------------------------------------

testthat::test_that("the warm-up rows are dropped, not passed through raw", {

  n <- 120L; m <- 36L
  df  <- make_df(n)
  res <- prep_covariates(df, scaling = "range", min_n = m)

  testthat::expect_equal(nrow(res), n - m)

  ## The rows that survive are exactly the ones the scalers actually scaled.
  testthat::expect_equal(res$month_id, df$month_id[-seq_len(m)])

  ## And none of them is still on the natural scale. Before this fix the first
  ## min_n values of rv_month_lag1 sat near exp(-6.5), orders of magnitude away
  ## from the scaled range the rest of the column occupies.
  testthat::expect_true(all(abs(res$rv_month_lag1) < 50))
})

testthat::test_that("the warm-up drop runs within key, not across the panel", {

  m <- 6L
  df <- rbind(make_df(24, seed = 1, key = "A"),
              make_df(24, seed = 2, key = "B"))
  res <- prep_covariates(df, scaling = "range", min_n = m, key_col = "name")

  testthat::expect_equal(as.integer(table(res$name)), c(24L - m, 24L - m))
})

testthat::test_that("a key no longer than the warm-up is emptied, not kept raw", {

  df <- rbind(make_df(24, seed = 1, key = "A"),
              make_df(5,  seed = 2, key = "B"))
  res <- prep_covariates(df, scaling = "range", min_n = 6L, key_col = "name")

  testthat::expect_equal(sum(res$name == "B"), 0L)
  testthat::expect_equal(sum(res$name == "A"), 24L - 6L)
})


# Log transform ---------------------------------------------------------

testthat::test_that("log_with_floor logs outright when there are no zeros", {

  x <- exp(stats::rnorm(50))
  testthat::expect_equal(log_with_floor(x, min_n = 10L), log(x))
})

testthat::test_that("a zero takes the smallest positive value seen so far", {

  x   <- c(4, 2, 8, 0, 1, 0)
  out <- log_with_floor(x)

  ## Position 4 sees 4, 2, 8 behind it, so it floors at 2. Position 6 has also
  ## seen the 1, so it floors lower.
  testthat::expect_equal(out, log(c(4, 2, 8, 2, 1, 1)))
  testthat::expect_true(all(is.finite(out)))
})

testthat::test_that("positive values are never shifted", {

  x <- c(exp(stats::rnorm(20)), 0, exp(stats::rnorm(19)))
  out <- log_with_floor(x)

  ## The floor applies to the zero alone. Everything else is a plain log, so a
  ## column that happens to contain a zero is not distorted elsewhere.
  keep <- x > 0
  testthat::expect_equal(out[keep], log(x[keep]))
  testthat::expect_true(all(is.finite(out)))
})

testthat::test_that("the floor is decided from history, never from the future", {

  ## The old version asked any(x == 0) over the whole column, so a zero
  ## appearing in the test period changed the transform for every earlier row.
  x  <- c(5, 3, 9, 4, 7)
  x2 <- c(5, 3, 9, 4, 0)          # a zero arrives at the very end

  testthat::expect_equal(log_with_floor(x)[1:4], log_with_floor(x2)[1:4])
})

testthat::test_that("a leading zero with no positive value behind it is NA, not -Inf", {

  out <- log_with_floor(c(0, 0, 4, 2))

  testthat::expect_true(all(is.na(out[1:2])))
  testthat::expect_false(any(is.infinite(out)))
  testthat::expect_equal(out[3:4], log(c(4, 2)))
})

testthat::test_that("the log transform is weakly monotone, so ordering survives", {

  ## A zero cannot map below the smallest positive value seen so far, so it
  ## ties with it rather than ordering beneath it. Everything else is strictly
  ## ordered.
  x   <- c(1e-9, 1e-7, 0, 1e-5, 1e-3)
  out <- log_with_floor(x)

  testthat::expect_equal(out[3], log(1e-9))
  testthat::expect_false(is.unsorted(out[c(1, 2, 4, 5)]))
})

testthat::test_that("the generated macro series are lagged to their release", {

  ## ppi_vol, ip_vol and mbas_vol come from schwert_vol_oos(), which builds
  ## sigma_t from residuals through t-1. That is NOT enough on its own: the
  ## predictor row for month t forecasts month t's realized variance, so its
  ## origin is the end of t-1, and the t-1 macro observation is not published
  ## until the middle of t. Each value must therefore be stamped one month
  ## later than the month it summarises.
  ##
  ## Checked against dfy_lag1, a market-observed spread that needs no release
  ## lag because it is a price: if the generated series were aligned like a
  ## price, this test would fail.
  ## Checked against the source rather than the artifact, so this runs in the
  ## unit section before the artifact helpers are defined, and still fails if
  ## someone reverts the alignment without rebuilding.
  src <- paste(readLines(file.path(here::here(), "R", "load_data.R"),
                         warn = FALSE), collapse = "\n")
  testthat::expect_equal(
    length(gregexpr("month_id = lubridate::`%m\\+%`\\(month_id, months\\(1\\)\\)",
                    src)[[1]]), 3L)
  testthat::expect_false(grepl("No lag needed", src, fixed = TRUE))
})

testthat::test_that("a forced split signs the run", {

  ## always.split.variables changes what ranger estimates, but it was set
  ## inside helpers.R and recorded nowhere, so changing it alone would have
  ## been reported as a cache HIT and the stale object reused.
  testthat::expect_true("forced_split_cols" %in% .provenance_signature_fields)

  base <- list(dataset_version = "v3", model = "rf", model_id = "rf",
               key_col = "name", target_name = "rv_month")
  a <- compute_run_signature(c(base, list(forced_split_cols = NULL)))
  b <- compute_run_signature(c(base, list(forced_split_cols = "rv_level_lag1")))
  testthat::expect_false(identical(a, b))
})

testthat::test_that("a fitted network survives saveRDS", {

  ## A Keras model is a Python external pointer: saveRDS() keeps the R wrapper
  ## and silently drops the network, so a reloaded object holds null pointers
  ## and predicts nothing. The numeric backtest survives that because it is
  ## already computed, which is why this went unnoticed -- the failure is only
  ## visible when you try to replay a fitted model in a fresh session.
  testthat::skip_if_not(nzchar(Sys.getenv("RETICULATE_PYTHON")),
                        "no python pinned")
  testthat::skip_if_not(requireNamespace("keras", quietly = TRUE))

  ok <- tryCatch({ suppressMessages(library(keras)); TRUE },
                 error = function(e) FALSE)
  testthat::skip_if_not(ok, "keras unavailable")

  set.seed(1)
  X <- matrix(stats::rnorm(120 * 4), 120, 4); colnames(X) <- paste0("v", 1:4)
  y <- X[, 1] + stats::rnorm(120, sd = 0.1)
  m <- keras::keras_model_sequential()
  m <- keras::layer_dense(m, units = 4, activation = "relu", input_shape = 4)
  m <- keras::layer_dense(m, units = 1)
  keras::compile(m, optimizer = "adam", loss = "mse")
  keras::fit(m, X, y, epochs = 2, batch_size = 32, verbose = 0)

  ## The construction fit_rv_model() performs.
  fit_obj <- list(m)
  attr(fit_obj, "serialized") <- list(keras::serialize_model(m))
  class(fit_obj) <- unique(c("keras_ensemble", class(fit_obj)))

  ## The bytes are what survive; assert they are there and are raw.
  testthat::expect_true(is.raw(attr(fit_obj, "serialized")[[1]]))

  ## And that a round trip through serialize/unserialize reproduces the model.
  restored <- keras::unserialize_model(attr(fit_obj, "serialized")[[1]])
  testthat::expect_equal(as.numeric(stats::predict(restored, X[1:5, ])),
                         as.numeric(stats::predict(m, X[1:5, ])),
                         tolerance = 1e-8)
})

testthat::test_that("the signed forced split is derived, not restated", {

  ## The earlier version hardcoded "rv_level_lag1" in run_spec under a
  ## condition that merely mirrored select_features(). Editing
  ## rf_always_split() to return NULL then changed the estimator while the
  ## signature stayed put, which is a false cache hit. Both the run record and
  ## the two ranger call sites must read the rule from one function.
  src <- paste(readLines(file.path(here::here(), "R", "fit_models.R"),
                         warn = FALSE), collapse = "\n")

  ## run_spec derives the value by calling the helper ...
  testthat::expect_true(grepl("forced_split_cols\\s*=\\s*if\\s*\\(identical\\(spec\\$model, \"rf\"\\)\\)\\{\\s*rf_always_split\\(",
                              src))
  ## ... and does not restate the column name as a literal beside it.
  testthat::expect_false(grepl('forced_split_cols\\s*=\\s*if[^}]*"rv_level_lag1"\\s*\\}', src))

  ## Both engine call sites read from the same helper.
  eng <- paste(readLines(file.path(here::here(), "R", "helpers.R"),
                         warn = FALSE), collapse = "\n")
  testthat::expect_equal(
    length(gregexpr("always[.]split[.]variables", eng)[[1]]), 2L)
  ## Two CALL sites. The definition reads "rf_always_split <- function", with no
  ## open paren after the name, so it is not counted here.
  testthat::expect_equal(
    length(gregexpr("rf_always_split\\(", eng)[[1]]), 2L)
  testthat::expect_true(grepl("rf_always_split <- function", eng, fixed = TRUE))
})

testthat::test_that("the resolved feature vector signs the run", {

  ## feature_set is only the label "har"/"harx". select_features() decides what
  ## the label means, so without the resolved vector a change there would let a
  ## cached fit be reused for a different design matrix.
  testthat::expect_true("feature_names_requested" %in% .provenance_signature_fields)

  base <- list(dataset_version = "v3", model = "har", model_id = "har",
               feature_set = "har", key_col = "name")
  a <- compute_run_signature(c(base, list(
    feature_names_requested = c("rv_month_lag1", "rv_tw_lag1", "rv_td_lag1"))))
  b <- compute_run_signature(c(base, list(
    feature_names_requested = c("rv_month_lag1", "rv_tw_lag1", "rv_td_lag1",
                                "rv_level_lag1"))))
  testthat::expect_false(identical(a, b))
})

testthat::test_that("rf_always_split follows the data, not the model name", {

  ## Pooled runs carry the level column and must protect it.
  testthat::expect_equal(
    rf_always_split(c("month_id", "name", "rv_month_lag1", "rv_level_lag1")),
    "rv_level_lag1")

  ## Single-series runs have it withheld by select_features(), so there is
  ## nothing to force and ranger must be passed NULL rather than a missing name.
  testthat::expect_null(rf_always_split(c("month_id", "rv_month_lag1")))
})

testthat::test_that("the zero-inflated jump family is not logged", {

  ## jump = max(0, RV - BPV), so it is a point mass at zero: exactly zero in
  ## 3.7% of panel rows weekly and 66% daily. Any floor then becomes most of
  ## the distribution and its distance from the rest of the mass is arbitrary.
  ## Worse, a floor that moves at all re-bases two thirds of the column: an
  ## expanding floor drifting 1.5 log units drove the scaled daily term to
  ## -17.5 against a bound of 5. The jump terms stay on the variance scale.
  testthat::expect_false(any(grepl("^jump_", prep_covariates_log_cols)))

  ## The strictly positive families are logged.
  for (fam in c("rv_", "rq_", "cont_", "oil_rv_"))
    testthat::expect_true(any(grepl(paste0("^", fam), prep_covariates_log_cols)))
})

testthat::test_that("a fixed offset would flatten quarticity but the floor does not", {

  ## Realized quarticity lives near 1e-08, which is exactly the offset the
  ## target transform uses. Applying it here would collapse the column.
  rq <- exp(stats::rnorm(200, log(1e-8), 2))

  flattened <- log(rq + 1e-08)
  honest    <- log_with_floor(rq, min_n = 36L)

  testthat::expect_gt(stats::sd(honest), stats::sd(flattened))
})

testthat::test_that("logging happens before scaling, so the column is centred", {

  df  <- make_df(400)
  res <- prep_covariates(df, scaling = "range",
                         winsorize_range = c(0.025, 0.975), min_n = 36L)

  ## Range scaling a raw variance piles the mass against the minimum, because
  ## the expanding max is set by a handful of extreme months. Logging first
  ## removes the skew, so the median should sit away from the floor.
  at_floor <- mean(res$rv_month_lag1 <= min(res$rv_month_lag1) + 0.02)
  testthat::expect_lt(at_floor, 0.05)
  testthat::expect_gt(stats::median(res$rv_month_lag1), -0.75)
})

testthat::test_that("a negative value in a logged column is rejected loudly", {

  df <- make_df(60)
  df$rv_month_lag1[10] <- -1

  testthat::expect_error(
    prep_covariates(df, scaling = "range", min_n = 12L),
    "must be non-negative"
  )
})

testthat::test_that("columns outside log_cols are left on their own scale", {

  df  <- make_df(120)
  res <- prep_covariates(df, scaling = "range", min_n = 36L)

  ## sqrt_days is excluded from winsorizing and scaling entirely.
  testthat::expect_equal(res$sqrt_days, df$sqrt_days[-seq_len(36L)])
})


# Series level ----------------------------------------------------------

testthat::test_that("rv_level_lag1 is the expanding mean of the logged term", {

  n <- 120L; m <- 36L
  df  <- make_df(n)
  res <- prep_covariates(df, scaling = "range", min_n = m)

  expected <- cumsum(log(df$rv_month_lag1)) / seq_len(n)
  testthat::expect_equal(res$rv_level_lag1, expected[-seq_len(m)])
})

testthat::test_that("rv_level_lag1 uses nothing beyond the row it sits on", {

  ## rv_month_lag1 is already a lag, so an expanding mean through row t carries
  ## information up to t-1 only. Perturbing the tail must leave earlier rows
  ## untouched.
  df <- make_df(120)
  df2 <- df; df2$rv_month_lag1[100:120] <- 1e3

  a <- prep_covariates(df,  scaling = "range", min_n = 36L)
  b <- prep_covariates(df2, scaling = "range", min_n = 36L)

  keep <- df$month_id[1:99]
  testthat::expect_equal(a$rv_level_lag1[a$month_id %in% keep],
                         b$rv_level_lag1[b$month_id %in% keep])
})

testthat::test_that("rv_level_lag1 is never scaled away", {

  ## The whole point of the column is the cross-key level. Scaling it within
  ## key would map every series onto the same range and delete it again.
  df <- rbind(make_df(120, seed = 1, key = "loud"),
              make_df(120, seed = 2, key = "quiet"))
  df$rv_month_lag1[df$name == "quiet"] <-
    df$rv_month_lag1[df$name == "quiet"] / 30

  res <- prep_covariates(df, scaling = "range", min_n = 36L, key_col = "name")
  lv  <- tapply(res$rv_level_lag1, res$name, mean)

  ## log(30) is about 3.4, and the level column must still show that gap.
  testthat::expect_gt(lv[["loud"]] - lv[["quiet"]], 3)

  ## While a scaled column shows almost none of it.
  sc <- tapply(res$rv_month_lag1, res$name, mean)
  testthat::expect_lt(abs(sc[["loud"]] - sc[["quiet"]]), 0.5)
})

testthat::test_that("rv_level_lag1 orders series by how volatile they are", {

  df <- rbind(make_df(120, seed = 3, key = "a"),
              make_df(120, seed = 4, key = "b"),
              make_df(120, seed = 5, key = "c"))
  df$rv_month_lag1[df$name == "b"] <- df$rv_month_lag1[df$name == "b"] / 10
  df$rv_month_lag1[df$name == "c"] <- df$rv_month_lag1[df$name == "c"] / 100

  res <- prep_covariates(df, scaling = "range", min_n = 36L, key_col = "name")
  lv  <- tapply(res$rv_level_lag1, res$name, mean)

  testthat::expect_true(lv[["a"]] > lv[["b"]] && lv[["b"]] > lv[["c"]])
})

testthat::test_that("a frame without rv_month_lag1 gains no level column", {

  df <- data.frame(
    month_id = seq(as.Date("1990-01-01"), by = "month", length.out = 60),
    month    = factor(rep(1:12, length.out = 60), levels = 1:12),
    x        = stats::rnorm(60)
  )
  res <- prep_covariates(df, scaling = "range", min_n = 12L)

  testthat::expect_false("rv_level_lag1" %in% names(res))
})


# Key isolation, which every transform must preserve -----------------------

testthat::test_that("leading NAs are not filled across keys", {

  ## This is the isolation the panel test used to assert on prep_covariates
  ## output, before the warm-up drop removed the row it looked at. impute_locf
  ## is where the behaviour lives.
  df <- data.frame(
    month_id = rep(seq(as.Date("1990-01-01"), by = "month", length.out = 3), 2),
    name     = rep(c("A", "B"), each = 3),
    x        = c(1, 2, 3, NA, 5, 6)
  )
  out <- df %>%
    dplyr::group_by(name) %>%
    dplyr::group_modify(~ impute_locf(.x)) %>%
    dplyr::ungroup()

  ## B opens with an NA and has nothing above it, so the NA survives. A
  ## panel-wide fill would have written A's last value, 3.
  testthat::expect_true(is.na(out$x[out$name == "B"][1]))
})

testthat::test_that("a key inside the panel matches the same key on its own", {

  df <- rbind(make_df(80, seed = 7, key = "A"),
              make_df(80, seed = 8, key = "B"))

  panel <- prep_covariates(df, scaling = "range", min_n = 24L, key_col = "name")
  alone <- prep_covariates(df[df$name == "B", setdiff(names(df), "name")],
                           scaling = "range", min_n = 24L)

  b <- as.data.frame(panel[panel$name == "B", names(alone)])
  row.names(b) <- NULL
  row.names(alone) <- NULL
  testthat::expect_equal(b, as.data.frame(alone))
})


# 3. The artifacts on disk ===================================================


DATA  <- file.path(here::here(), "data")
MIN_N <- 36L

art <- function(f) file.path(DATA, f)
have <- function(f) file.exists(art(f))
read_art <- function(f) readRDS(art(f))

## Artifacts written before a preprocessing change cannot satisfy the tests for
## it. Exactly one test asserts the column is there, so it fails and says so;
## everything that depends on it skips rather than raising a subscript error.
has_level <- function(f) have(f) && "rv_level_lag1" %in% names(read_art(f))
skip_unless_rebuilt <- function(f) {
  testthat::skip_if_not(
    has_level(f),
    paste0(f, " predates the preprocessing fixes; re-run R/load_data.R")
  )
}

PANELS <- list(
  list(tag = "1986", raw = "covariates_panel_1986_v3.rds",
       range = "covariates_panel_1986_range_v3.rds",
       zscore = "covariates_panel_1986_zscore_v3.rds",
       spx_range = "covariates_spx_1986_range_v3.rds"),
  list(tag = "1927", raw = "covariates_panel_1927_v3.rds",
       range = "covariates_panel_1927_range_v3.rds",
       zscore = "covariates_panel_1927_zscore_v3.rds",
       spx_range = "covariates_spx_1927_range_v3.rds")
)

ALL_ARTIFACTS <- c(
  "covariates_panel_1986_v3.rds", "covariates_panel_1986_range_v3.rds",
  "covariates_panel_1986_zscore_v3.rds", "covariates_panel_1927_v3.rds",
  "covariates_panel_1927_range_v3.rds", "covariates_panel_1927_zscore_v3.rds",
  "covariates_spx_1986_range_v3.rds", "covariates_spx_1986_zscore_v3.rds",
  "covariates_spx_1927_range_v3.rds", "covariates_spx_1927_zscore_v3.rds",
  "target_panel_v3.rds", "target_spx_v3.rds"
)


# The artifacts exist ------------------------------------------------------

testthat::test_that("every artifact load_data.R promises is on disk", {
  missing <- ALL_ARTIFACTS[!vapply(ALL_ARTIFACTS, have, logical(1))]
  testthat::expect_equal(missing, character(0))
})


# The warm-up is gone ------------------------------------------------------

for (p in PANELS) local({
  pp <- p

  testthat::test_that(paste0("panel ", pp$tag,
                             ": each key is shorter by exactly the warm-up"), {
    testthat::skip_if_not(have(pp$raw))
    skip_unless_rebuilt(pp$range)

    raw <- read_art(pp$raw)
    prp <- read_art(pp$range)

    n_raw <- table(raw$name)
    n_prp <- table(prp$name)

    ## Keys that had no more than the warm-up are dropped entirely.
    survivors <- names(n_raw)[n_raw > MIN_N]
    testthat::expect_setequal(names(n_prp), survivors)
    testthat::expect_equal(as.integer(n_prp[survivors]),
                           as.integer(n_raw[survivors]) - MIN_N)
  })

  testthat::test_that(paste0("panel ", pp$tag,
                             ": the first retained month is the one after the warm-up"), {
    testthat::skip_if_not(have(pp$raw))
    skip_unless_rebuilt(pp$range)

    raw <- read_art(pp$raw); prp <- read_art(pp$range)
    first_raw <- tapply(raw$month_id, raw$name, function(d) sort(d)[MIN_N + 1L])
    first_prp <- tapply(prp$month_id, prp$name, min)
    k <- intersect(names(first_raw), names(first_prp))

    testthat::expect_equal(unname(first_prp[k]), unname(first_raw[k]))
  })

  testthat::test_that(paste0("panel ", pp$tag,
                             ": the scaling exclusion list is honoured exactly"), {
    testthat::skip_if_not(have(pp$raw))
    skip_unless_rebuilt(pp$range)

    raw <- read_art(pp$raw); prp <- read_art(pp$range)
    both <- merge(raw[, c("month_id", "name", "sqrt_days")],
                  prp[, c("month_id", "name", "sqrt_days")],
                  by = c("month_id", "name"), suffixes = c("_raw", "_prp"))

    ## sqrt_days is excluded from winsorizing and scaling, so it must come out
    ## bit-identical. This is the exact form of the check a magnitude bound
    ## only gestures at: an exclusion-list regression shows up here as an
    ## inequality, not as a number that happens to be large.
    testthat::expect_equal(both$sqrt_days_prp, both$sqrt_days_raw)
    testthat::expect_gt(nrow(both), 0L)
  })

  testthat::test_that(paste0("panel ", pp$tag,
                             ": scaled columns sit in the unit band apart from real extremes"), {
    skip_unless_rebuilt(pp$range)

    prp <- read_art(pp$range)
    num <- setdiff(names(prp)[vapply(prp, is.numeric, logical(1))],
                   c("sqrt_days", "rv_level_lag1"))
    num <- num[!grepl("^m_[0-9]+$", num)]

    ## A distribution check rather than a maximum, because a maximum cannot
    ## tell an unscaled column from a genuine tail: unscaled tbill_lag1 runs 5
    ## to 8, and the legitimately-scaled jump daily term reaches 7.5 in the
    ## 1927 panel once an out-of-sample month exceeds its whole history. A
    ## column that was never scaled fails this on nearly every row; a column
    ## with a fat tail does not.
    ##
    ## Note this is NOT the warm-up detector either. The un-scaled warm-up
    ## values were SMALL, so they would sit inside any band. The row-count and
    ## first-retained-month tests above are the exact checks for that.
    inside <- vapply(prp[num], function(x) mean(abs(x) <= 1.5), numeric(1))
    testthat::expect_true(all(inside >= 0.95),
                          info = paste("least contained:",
                                       paste(names(sort(inside))[1:3], collapse = ", ")))
  })
})


# The level column ---------------------------------------------------------

for (p in PANELS) local({
  pp <- p

  testthat::test_that(paste0("panel ", pp$tag,
                             ": rv_level_lag1 is present and unscaled"), {
    testthat::skip_if_not(have(pp$range))

    prp <- read_art(pp$range)
    testthat::expect_true("rv_level_lag1" %in% names(prp))
    skip_unless_rebuilt(pp$range)

    ## It carries log realized variance, so it must sit well below the scaled
    ## band. If it were inside [-1, 1] it would have been scaled by mistake.
    testthat::expect_lt(max(prp$rv_level_lag1), -1)
    testthat::expect_true(all(is.finite(prp$rv_level_lag1)))
  })

  testthat::test_that(paste0("panel ", pp$tag,
                             ": rv_level_lag1 equals the expanding mean of the logged term"), {
    testthat::skip_if_not(have(pp$raw))
    skip_unless_rebuilt(pp$range)

    raw <- read_art(pp$raw)[, c("month_id", "name", "rv_month_lag1")]
    raw <- raw[order(raw$name, raw$month_id), ]
    raw$expected <- ave(log(raw$rv_month_lag1), raw$name,
                        FUN = function(v) cumsum(v) / seq_along(v))
    raw <- raw[ave(seq_len(nrow(raw)), raw$name, FUN = seq_along) > MIN_N, ]

    prp <- read_art(pp$range)[, c("month_id", "name", "rv_level_lag1")]
    both <- merge(raw, prp, by = c("month_id", "name"))

    testthat::expect_equal(nrow(both), nrow(prp))
    testthat::expect_equal(both$rv_level_lag1, both$expected)
  })

  testthat::test_that(paste0("panel ", pp$tag,
                             ": rv_level_lag1 tracks how volatile each series really is"), {
    testthat::skip_if_not(have("target_panel_v3.rds"))
    skip_unless_rebuilt(pp$range)

    prp <- read_art(pp$range)
    tgt <- read_art("target_panel_v3.rds")
    tgt <- tgt[tgt$month_id >= min(prp$month_id), ]

    truth <- tapply(log(tgt$rv_month + 1e-08), tgt$name, mean)
    level <- tapply(prp$rv_level_lag1, prp$name, mean)
    k <- intersect(names(truth), names(level))

    ## Before the level column existed the pooled forecast correlated -0.244
    ## with this. The column itself must do far better than that.
    testthat::expect_gt(stats::cor(level[k], truth[k]), 0.85)

    ## And it must spread the series out rather than flatten them.
    testthat::expect_gt(stats::sd(level[k]), 0.5 * stats::sd(truth[k]))
  })

  testthat::test_that(paste0("panel ", pp$tag,
                             ": the market key reads as more volatile than a typical factor"), {
    skip_unless_rebuilt(pp$range)

    prp <- read_art(pp$range)
    testthat::skip_if_not("spx_vw" %in% prp$name)

    level <- tapply(prp$rv_level_lag1, prp$name, mean)
    testthat::expect_gt(level[["spx_vw"]], stats::median(level))
  })
})


# The log transform --------------------------------------------------------

testthat::test_that("the variance-family columns are logged, not left skewed", {
  skip_unless_rebuilt("covariates_panel_1986_range_v3.rds")

  prp <- read_art("covariates_panel_1986_range_v3.rds")
  check <- intersect(c("rv_month_lag1", "rv_tw_lag1", "rq_month_lag1",
                       "cont_month_lag1", "vix_lag1", "oil_rv_month_lag1"),
                     names(prp))

  for (v in check) {
    at_floor <- mean(prp[[v]] <= min(prp[[v]]) + 0.02)
    testthat::expect_lt(at_floor, 0.10)
    testthat::expect_gt(stats::median(prp[[v]]), -0.80)
  }
})

testthat::test_that("the jump daily term keeps its zeros rather than gaining -Inf", {
  skip_unless_rebuilt("covariates_panel_1986_range_v3.rds")

  prp <- read_art("covariates_panel_1986_range_v3.rds")
  testthat::skip_if_not("jump_td_lag1" %in% names(prp))

  ## It is exactly zero most months, so a large floor share is the data and not
  ## a defect. What matters is that nothing became non-finite.
  testthat::expect_true(all(is.finite(prp$jump_td_lag1)))
})


# Nothing broke ------------------------------------------------------------

testthat::test_that("no PREPROCESSED artifact carries an NA or a non-finite value", {
  ## The raw panel artifacts are excluded on purpose: they are the input to
  ## prep_covariates and still hold the gaps that impute_locf fills.
  prepped <- grep("_(range|zscore)_v3[.]rds$", ALL_ARTIFACTS, value = TRUE)
  for (f in prepped) {
    if (!have(f)) next
    d <- read_art(f)
    num <- d[vapply(d, is.numeric, logical(1))]
    testthat::expect_true(all(vapply(num, function(x) all(is.finite(x)), logical(1))),
                          info = f)
  }
})

testthat::test_that("the range and z-score builds cover the same rows", {
  for (p in PANELS) {
    if (!have(p$range) || !have(p$zscore)) next
    a <- read_art(p$range); b <- read_art(p$zscore)
    testthat::expect_equal(nrow(a), nrow(b), info = p$tag)
    testthat::expect_setequal(names(a), names(b))
    testthat::expect_equal(a$month_id, b$month_id, info = p$tag)
    testthat::expect_equal(a$name, b$name, info = p$tag)
  }
})

testthat::test_that("the market key inside the panel is untouched by the factors", {
  for (p in PANELS) {
    if (!have(p$range) || !have(p$spx_range)) next
    panel <- read_art(p$range); spx <- read_art(p$spx_range)

    shared <- intersect(names(panel), names(spx))
    lhs <- as.data.frame(panel %>% dplyr::filter(name == "spx_vw") %>%
                           dplyr::arrange(month_id) %>%
                           dplyr::select(dplyr::all_of(shared)))
    rhs <- as.data.frame(spx %>% dplyr::arrange(month_id) %>%
                           dplyr::select(dplyr::all_of(shared)))
    row.names(lhs) <- NULL; row.names(rhs) <- NULL

    testthat::expect_equal(lhs, rhs, info = p$tag)
  }
})

testthat::test_that("`name` is a key, never a feature", {
  for (f in ALL_ARTIFACTS) {
    if (!have(f) || !grepl("^covariates_", f)) next
    d <- read_art(f)
    ## The S&P frames carry no key at all, which satisfies this trivially.
    if (!"name" %in% names(d)) next
    testthat::expect_false(is.numeric(d$name), info = f)
  }
})

testthat::test_that("every covariate row has a target to match", {
  for (p in PANELS) {
    if (!have(p$range) || !have("target_panel_v3.rds")) next
    cov <- read_art(p$range); tgt <- read_art("target_panel_v3.rds")
    joined <- merge(cov[, c("month_id", "name")], tgt, by = c("month_id", "name"))
    testthat::expect_equal(nrow(joined), nrow(cov), info = p$tag)
  }
})
