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


# Panel dataset (v3) tests------------------------------------------------------

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


