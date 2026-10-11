## Tests for the Table I replication helpers (R/demiguel_replication_helpers.R)
##
##   1. variance stamping: definition, holding-month timing, no look-ahead
##   2. weights: signed closed form, in-sample rescaling and fallback,
##      expanding OOS windows that never see the holding month
##   3. the DeMiguel bootstrap p-value on known cases
##   4. the saved result, when R/demiguel_replication.R has been run

library("testthat")
`%>%` <- dplyr::`%>%`
source(file.path(here::here(), "R", "portfolio_helpers.R"))
source(file.path(here::here(), "R", "allocation_helpers.R"))
source(file.path(here::here(), "R", "portfolio_inference_helpers.R"))
source(file.path(here::here(), "R", "demiguel_replication_helpers.R"))

make_daily <- function(months = 30L, names = c("a", "b")) {
  days <- seq(as.Date("2000-01-03"), by = "day", length.out = months * 31L)
  days <- days[lubridate::wday(days, week_start = 1) <= 5 &
                 days < lubridate::add_with_rollback(as.Date("2000-01-01"), months(months))]
  tidyr::expand_grid(name = names, date = days) %>%
    dplyr::mutate(ret = .01 * sin(as.numeric(date) / 3 + nchar(name)) *
                    (1 + .5 * cos(as.numeric(date) / 40)))
}

make_sample <- function(n = 160L) {
  months <- seq(as.Date("1990-01-01"), by = "month", length.out = n)
  i <- seq_len(n)
  variance <- 1e-4 * (1.5 + sin(i / 7))
  monthly <- tibble::tibble(name = "a", month_id = months,
                            factor_exc = .004 + .5 * sqrt(variance) * sin(i * 1.7) + .002 * cos(i / 3))
  list(monthly = monthly,
       variance = tibble::tibble(name = "a", month_id = months, variance = variance))
}


# 1. Variance stamping =========================================================

testthat::test_that("variance is the previous month's sample variance of daily returns", {
  daily <- make_daily()
  v <- prepare_factor_variance(daily)
  jan <- dplyr::filter(daily, name == "a", date < as.Date("2000-02-01"))$ret
  testthat::expect_equal(v$variance[v$name == "a" & v$month_id == as.Date("2000-02-01")],
                         stats::var(jan))
  testthat::expect_false(any(v$month_id == as.Date("2000-01-01")))
})

testthat::test_that("changing a later month's daily returns never changes earlier signals", {
  daily <- make_daily()
  shocked <- dplyr::mutate(daily, ret = dplyr::if_else(date >= as.Date("2001-06-01"), ret * 5, ret))
  base <- prepare_factor_variance(daily)
  alt <- prepare_factor_variance(shocked)
  early <- base$month_id <= as.Date("2001-06-01")
  testthat::expect_equal(alt$variance[early], base$variance[early])
  testthat::expect_false(isTRUE(all.equal(alt$variance[!early], base$variance[!early])))
})

testthat::test_that("degenerate months abort", {
  one_day <- tibble::tibble(name = "a", date = as.Date(c("2000-01-03", "2000-02-01", "2000-02-02")),
                            ret = c(.01, .02, -.01))
  testthat::expect_error(prepare_factor_variance(one_day), "at least two")
  flat <- tibble::tibble(name = "a", date = as.Date(c("2000-01-03", "2000-01-04")), ret = .01)
  testthat::expect_error(prepare_factor_variance(flat), "positive variance")
})


# 2. Weights and windows =======================================================

testthat::test_that("the sample needs full coverage and scales the managed sleeve", {
  f <- make_sample()
  s <- build_table1_sample(f$monthly, f$variance, "a", min(f$monthly$month_id),
                           max(f$monthly$month_id))
  c_scale <- stats::sd(s$factor_exc) / stats::sd(s$factor_exc / s$variance)
  testthat::expect_equal(s$managed, s$factor_exc * c_scale / s$variance)
  testthat::expect_equal(stats::sd(s$managed), stats::sd(s$factor_exc))
  testthat::expect_error(build_table1_sample(f$monthly, f$variance, "a",
    as.Date("1980-01-01"), max(f$monthly$month_id)), "span the window")
})

testthat::test_that("signed weights equal (1 / gamma) Sigma^-1 mu and may be negative", {
  f <- make_sample()
  x <- cbind(unmanaged = f$monthly$factor_exc,
             managed = f$monthly$factor_exc * 1e-4 / f$variance$variance)
  testthat::expect_equal(mv_signed_weights(x, 5),
                         as.vector(solve(stats::cov(x), colMeans(x))) / 5, tolerance = 1e-10)
  flipped <- x; flipped[, 2] <- -flipped[, 2]
  testthat::expect_equal(mv_signed_weights(flipped, 5)[2], -mv_signed_weights(x, 5)[2],
                         tolerance = 1e-10)
})

testthat::test_that("in-sample uses rows M + 1 to T and rescales by |b2|", {
  f <- make_sample()
  s <- build_table1_sample(f$monthly, f$variance, "a", min(f$monthly$month_id),
                           max(f$monthly$month_id))
  r <- table1_in_sample(s, M = 120L)
  testthat::expect_equal(r$month_id, s$month_id[121:160])
  testthat::expect_equal(abs(r$b[1]), 1)
  w <- s[121:160, ]
  b <- as.vector(solve(stats::cov(cbind(w$factor_exc, w$managed)),
                       colMeans(cbind(w$factor_exc, w$managed))))
  testthat::expect_equal(r$managed_combo, as.vector(cbind(w$factor_exc, w$managed) %*% (b / abs(b[2]))),
                         tolerance = 1e-10)
  ## Rows before M + 1 do not affect the in-sample weights
  s2 <- s; s2$factor_exc[1:120] <- s2$factor_exc[1:120] * 3
  testthat::expect_equal(table1_in_sample(s2, M = 120L)$managed_combo, r$managed_combo)
})

testthat::test_that("OOS weights use only earlier months, from an expanding window", {
  f <- make_sample()
  s <- build_table1_sample(f$monthly, f$variance, "a", min(f$monthly$month_id),
                           max(f$monthly$month_id))
  r <- table1_out_of_sample(s, M = 120L)
  testthat::expect_equal(nrow(r), 40L)
  testthat::expect_true(all(r$train_end < r$month_id))
  x <- cbind(s$factor_exc, s$managed)
  b <- as.vector(solve(stats::cov(x[1:129, ]), colMeans(x[1:129, ]))) / 5
  testthat::expect_equal(r$managed_combo[10], sum(x[130, ] * b), tolerance = 1e-10)
  ## A shock to a future return leaves every earlier OOS return unchanged
  s2 <- s; s2$factor_exc[150] <- s2$factor_exc[150] + .2
  s2$managed[150] <- s2$managed[150] + .2
  r2 <- table1_out_of_sample(s2, M = 120L)
  testthat::expect_equal(r2$managed_combo[1:29], r$managed_combo[1:29])
})


# 3. Bootstrap p-value ==========================================================

testthat::test_that("the p-value follows the uncentred DeMiguel convention", {
  dates <- seq(as.Date("2000-01-01"), by = "month", length.out = 120)
  cal <- draw_portfolio_months(dates, draws = 2000L, block_length = 5, seed = 1L)
  base <- .005 + .04 * sin(seq_len(120) * 1.3)
  testthat::expect_equal(demiguel_sr_pvalue(base, base, cal), .5)
  testthat::expect_lt(demiguel_sr_pvalue(base, base + .03, cal), .01)
  testthat::expect_gt(demiguel_sr_pvalue(base, base - .03, cal), .99)
  ## Matches a direct computation on the same draws
  pol <- base + .002
  sr <- function(m) rowMeans(m) / apply(m, 1, stats::sd)
  delta <- sr(matrix(base[cal$indices], nrow(cal$indices))) - sr(matrix(pol[cal$indices], nrow(cal$indices)))
  x_j <- sort(unique(delta))[which.min(abs(sort(unique(delta))))]
  testthat::expect_equal(demiguel_sr_pvalue(base, pol, cal), mean(delta > x_j))
  testthat::expect_error(demiguel_sr_pvalue(base, base[-1], cal), "equal length")
})

testthat::test_that("annual Sharpe uses sqrt(12) and the sample sd", {
  x <- c(.01, -.02, .03, .005)
  testthat::expect_equal(annual_sharpe(x), sqrt(12) * mean(x) / stats::sd(x))
  testthat::expect_error(annual_sharpe(c(.01, .01)), "constant")
})


# 4. Saved result ==============================================================

saved_path <- file.path(here::here(), "data", "demiguel_table1_ac_v3.rds")
if (file.exists(saved_path)) {
  saved <- readRDS(saved_path)

  testthat::test_that("saved Table I uses the paper's window and settings", {
    testthat::expect_true(all(saved$table$start == as.Date("1977-02-01")))
    testthat::expect_true(all(saved$table$end == as.Date("2020-12-01")))
    testthat::expect_true(all(saved$table$n == 527L))
    testthat::expect_equal(saved$settings[c("M", "gamma", "draws", "block_length")],
                           list(M = 120L, gamma = 5, draws = 10000L, block_length = 5))
    testthat::expect_setequal(saved$table$name, demiguel_table1_paper$name)
  })

  testthat::test_that("saved Sharpe ratios are within data-vintage distance of the paper", {
    ## 0.03 is the pre-set tolerance of the unmanaged Sharpe check in Step 1.
    ## The same code reproduces the paper exactly on the authors' own data.
    cmp <- compare_table1(saved$table)
    sr <- dplyr::filter(cmp, startsWith(statistic, "sr_"))
    testthat::expect_true(all(abs(sr$difference) <= .03))
  })
}
