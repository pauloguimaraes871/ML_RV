## Unit tests for the transforms prep_covariates applies:
##   1. the scaler warm-up is discarded rather than passed through raw
##   2. the variance-family predictors are logged before winsorizing and scaling
##   3. each series carries its own volatility level, which the within-key
##      scaling would otherwise erase
library("testthat")
`%>%` <- dplyr::`%>%`
source(file.path(here::here(), "R", "helpers.R"))

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


# 1. Warm-up ---------------------------------------------------------------

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


# 2. Log transform ---------------------------------------------------------

testthat::test_that("log_with_floor logs outright when there are no zeros", {

  x <- exp(stats::rnorm(50))
  testthat::expect_equal(log_with_floor(x, min_n = 10L), log(x))
})

testthat::test_that("log_with_floor shifts only columns holding exact zeros", {

  x <- c(exp(stats::rnorm(20)), 0, exp(stats::rnorm(19)))
  out <- log_with_floor(x, min_n = 20L)

  testthat::expect_true(all(is.finite(out)))
  testthat::expect_false(any(is.nan(out)))

  ## The shift comes from the warm-up window only, so it is knowable at the
  ## time each retained row is formed.
  shift <- min(x[seq_len(20L)][x[seq_len(20L)] > 0])
  testthat::expect_equal(out, log(x + shift))
})

testthat::test_that("the log transform is monotone, so ordering survives", {

  x <- c(1e-9, 1e-7, 0, 1e-5, 1e-3)
  out <- log_with_floor(x, min_n = 5L)
  testthat::expect_equal(order(out), order(x))
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


# 3. Series level ----------------------------------------------------------

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


# Key isolation, which all three fixes must preserve -----------------------

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
