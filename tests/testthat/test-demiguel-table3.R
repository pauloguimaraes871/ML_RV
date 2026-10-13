## Tests for the Table III helpers (R/demiguel_table3_helpers.R): the ports of
## the authors' cov1para.m and nwest.m, the nonnegative optimizers and the
## panel statistics. The full replication is checked against the paper on the
## authors' own data (docs/DEMIGUEL_REPLICATION.md, Section 12).

library("testthat")
`%>%` <- dplyr::`%>%`
source(file.path(here::here(), "R", "portfolio_helpers.R"))
source(file.path(here::here(), "R", "allocation_helpers.R"))
source(file.path(here::here(), "R", "portfolio_inference_helpers.R"))
source(file.path(here::here(), "R", "demiguel_cost_helpers.R"))
source(file.path(here::here(), "R", "demiguel_replication_helpers.R"))
source(file.path(here::here(), "R", "demiguel_table3_helpers.R"))

set.seed(31)
X <- matrix(stats::rnorm(600, .005, .04), ncol = 3) %*% matrix(c(1, .3, 0, 0, 1, .2, 0, 0, 1), 3)

testthat::test_that("cov1para shrinks the 1/T sample covariance toward a scaled identity", {
  s <- cov1para(X)
  sample <- crossprod(sweep(X, 2, colMeans(X))) / nrow(X)
  prior <- diag(mean(diag(sample)), 3)
  ## s = k * prior + (1 - k) * sample for one k in [0, 1]
  k <- (s[1, 2] - sample[1, 2]) / (prior[1, 2] - sample[1, 2])
  testthat::expect_true(k >= 0 && k <= 1)
  testthat::expect_equal(s, k * prior + (1 - k) * sample)
  testthat::expect_equal(sum(diag(s)), sum(diag(sample)))
})

testthat::test_that("newey_west matches OLS coefficients and White errors at lag 0", {
  y <- X[, 1]; x <- cbind(1, X[, 2])
  nw0 <- newey_west(y, x, 0L)
  fit <- stats::lm(y ~ X[, 2])
  testthat::expect_equal(nw0$beta, unname(stats::coef(fit)))
  e <- stats::residuals(fit)
  bread <- solve(crossprod(x))
  white <- sqrt(diag(bread %*% crossprod(x * e) %*% bread))
  testthat::expect_equal(nw0$t, nw0$beta / white)
  ## Lag 1 adds the Bartlett-weighted first autocovariance
  testthat::expect_false(isTRUE(all.equal(newey_west(y, x, 1L)$t, nw0$t)))
})

testthat::test_that("nonnegative MV equals the closed form when that is positive, else clips", {
  S <- matrix(c(.04, .01, .01, .09), 2)
  testthat::expect_equal(nonneg_mv(c(.02, .015), S, 5), as.vector(solve(S, c(.02, .015))) / 5,
                         tolerance = 1e-10)
  w <- nonneg_mv(c(.02, -.01), S, 5)
  testthat::expect_equal(w, c(.02 / (5 * .04), 0), tolerance = 1e-10)
})

testthat::test_that("the cost-aware nonnegative optimizer is certified and consistent", {
  S <- stats::cov(X); mu <- colMeans(X)
  ## No trades: plain nonnegative MV
  x0 <- mv_l1_nonneg(mu, S, 5, matrix(0, 4, 3))
  testthat::expect_equal(as.vector(x0), nonneg_mv(mu, S, 5), tolerance = 1e-8)
  ## With trades: certified gap, and the objective is no worse than nearby points
  ## With trades (two cost levels: weights shrunk, and priced out to zero): a
  ## certified relative gap, and no nearby feasible point does better
  P <- function(z, A) 5 / 2 * sum(z * (S %*% z)) - sum(mu * z) + sum(abs(A %*% z))
  scale <- abs(P(nonneg_mv(mu, S, 5), matrix(0, 1, 3)))
  for (sd_trade in c(2e-6, 2e-4)) {
    A <- matrix(stats::rnorm(1500, 0, sd_trade), ncol = 3)
    x <- mv_l1_nonneg(mu, S, 5, A)
    testthat::expect_lte(attr(x, "gap"), 1e-7)
    for (j in 1:50) {
      z <- pmax(as.vector(x) + stats::rnorm(3, 0, .01), 0)
      testthat::expect_gte(P(z, A), P(as.vector(x), A) - 1e-7 * scale)
    }
  }
  ## Two positive sleeves: agrees with the exact angular solver of Table I
  S2 <- matrix(c(.04, .01, .01, .09), 2); mu2 <- c(.03, .025)
  A2 <- matrix(stats::rnorm(400, 0, 5e-4), ncol = 2)
  exact <- mv_l1_phi_2d(mu2, S2, 5, window_rays(phi_rays(A2, rep(1L, nrow(A2))), 1L))
  if (all(exact > 0)) {
    testthat::expect_equal(as.vector(mv_l1_nonneg(mu2, S2, 5, A2)), exact, tolerance = 1e-5)
  }
})

testthat::test_that("run_table3 accepts per-factor sigma; equal columns reproduce the vector case", {
  stock_file <- file.path(here::here(), "data", "demiguel_stock_costs_v3.rds")
  testthat::skip_if_not(file.exists(stock_file), "Stock-level cost data not built")
  stock <- readRDS(stock_file)
  months <- seq(as.Date("1967-02-01"), by = "month", length.out = 36)
  R <- matrix(stats::rnorm(36 * 9, .005, .03), ncol = 9, dimnames = list(NULL, stock$factors))
  s <- stats::runif(36, .5, 1.5)
  run <- function(sig) suppressMessages(run_table3(R, months, sig, stock, M = 24L, draws = 200L))
  by_vector <- run(s)
  ## Same signal for every factor, columns deliberately out of order
  S <- matrix(s, 36, 9, dimnames = list(NULL, rev(stock$factors)))
  testthat::expect_identical(run(S), by_vector)
  ## Factor-specific signal: the managed sleeves are r_k / sigma_k (a different
  ## time pattern; a constant multiple would only rescale the sleeve weight)
  S[, "umd"] <- rev(s)
  own <- run(S)
  testthat::expect_false(identical(own$table, by_vector$table))
  testthat::expect_identical(own$weights$in_sample[, "umv_notc"], by_vector$weights$in_sample[, "umv_notc"])
  testthat::expect_error(run(S[, -1]), "one column per factor")
  testthat::expect_error(run(s[-1]), "one value per month")
})

testthat::test_that("panel statistics follow JF_performancematrixTable4.m", {
  dates <- seq(as.Date("2000-01-01"), by = "month", length.out = 60)
  cal <- draw_portfolio_months(dates, draws = 500L, block_length = 5, seed = 2L)
  g <- cbind(umv_notc = X[1:60, 1], cmv_notc = X[1:60, 1] + .002,
             umv_tc = X[1:60, 2], cmv_tc = X[1:60, 3])
  net <- g - .001
  s <- table3_panel_stats(g, net, cal)
  r <- s[s$column == "cmv_notc", ]
  testthat::expect_equal(r$mean, 12 * mean(net[, "cmv_notc"]))
  testthat::expect_equal(r$sd, sqrt(12) * stats::sd(g[, "cmv_notc"]))
  testthat::expect_equal(r$tc, 12 * .001)
  testthat::expect_equal(r$alpha, 12 * stats::coef(stats::lm(net[, "cmv_notc"] ~ net[, "umv_notc"]))[[1]])
  testthat::expect_true(is.na(s$p_value[s$column == "umv_notc"]))
})
