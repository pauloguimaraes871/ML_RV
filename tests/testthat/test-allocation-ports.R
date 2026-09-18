base::source(base::file.path(here::here(), "R", "portfolio_helpers.R"))
base::source(base::file.path(here::here(), "R", "multifactor_helpers.R"))
base::source(base::file.path(here::here(), "R", "cederburg_helpers.R"))

allocation_fixture <- function() {
  dates <- base::seq(base::as.Date("2000-01-01"), by = "month", length.out = 30)
  data <- tidyr::expand_grid(name = c("a", "b", "c"), month_id = dates) %>%
    dplyr::group_by(name) %>% dplyr::mutate(i = dplyr::row_number(),
      factor_exc = .003 + .03 * base::sin(i + base::match(name, c("a", "b", "c"))),
      sigma = .01 + .004 * base::cos(i / 2 + base::match(name, c("a", "b", "c"))),
      rf = .001, method = "vol_scaled", signal_rv = sigma^2, w_raw = .0001 / signal_rv) %>%
    dplyr::ungroup()
  base::list(w_compiled_df = data,
    settings = base::list(implementable = TRUE, normalization = "expanding_rv"))
}

testthat::test_that("signed MVO uses the full covariance, with no equal-variance assumption", {
  x <- base::cbind(unmanaged = c(.03, -.01, .02, .05), managed = c(.01, .02, -.03, .06))
  basis <- tibble::tibble(id = base::colnames(x), name = "a", sleeve = base::colnames(x), cluster = "one")
  expected <- base::as.vector(base::solve(stats::cov(x), base::colMeans(x))) / 5
  fit <- allocate_portfolio(x, basis, nonnegative = FALSE)
  testthat::expect_equal(fit$coef, expected, tolerance = 1e-8)
  x2 <- x
  x2[, 2] <- 100 * x2[, 2]
  changed <- allocate_portfolio(x2, basis, nonnegative = FALSE)
  testthat::expect_equal(base::as.vector(x %*% fit$coef),
                         base::as.vector(x2 %*% changed$coef), tolerance = 1e-8)
  x2[, 2] <- x2[, 1]
  testthat::expect_error(allocate_portfolio(x2, basis, nonnegative = FALSE), "Singular")
})

testthat::test_that("HEW respects cluster, factor and sleeve hierarchy", {
  basis <- tidyr::expand_grid(sleeve = c("unmanaged", "managed"), name = c("a", "b", "c")) %>%
    dplyr::mutate(cluster = dplyr::if_else(name == "a", "one", "two"),
                  id = base::paste(name, sleeve, sep = ":"))
  x <- base::matrix(base::seq_len(30) / 100, ncol = 6, dimnames = base::list(NULL, basis$id))
  hew <- allocate_portfolio(x, basis, allocation = "hew")$coef
  ew <- allocate_portfolio(x, basis, allocation = "ew")$coef
  testthat::expect_equal(hew, c(.25, .125, .125, .25, .125, .125))
  testthat::expect_equal(ew, base::rep(1/6, 6))
  testthat::expect_equal(sum(hew), 1)
  testthat::expect_equal(sum(ew), 1)
  testthat::expect_error(allocate_portfolio(x, basis[6:1, ], allocation = "hew"), "match")
})

testthat::test_that("own-factor timing reduces to common timing when signals agree", {
  d <- allocation_fixture()$w_compiled_df
  returns <- dplyr::select(d, name, month_id, factor_exc, rf)
  shared <- dplyr::select(dplyr::filter(d, name == "a"), month_id, sigma)
  own <- tidyr::expand_grid(name = c("a", "b", "c"), month_id = shared$month_id) %>%
    dplyr::left_join(shared, by = "month_id", relationship = "many-to-one")
  for (allocation in c("mvo", "ew", "hew")) {
    cluster <- tibble::tibble(name = c("a", "b", "c"), cluster = c("one", "two", "two"))
    cmv <- run_multifactor_ports(returns, shared, c("a", "b", "c"), base::min(shared$month_id),
      min_history = 15L, allocation = allocation, cluster_map = cluster)
    fcmv <- run_multifactor_ports(returns, own, c("a", "b", "c"), base::min(shared$month_id),
      min_history = 15L, allocation = allocation, cluster_map = cluster, signal_scope = "factor")
    testthat::expect_equal(dplyr::filter(cmv$weights, method == "cmv")$w_raw,
                           dplyr::filter(fcmv$weights, method == "fcmv")$w_raw)
  }
  own <- dplyr::select(d, name, month_id, sigma)
  fcmv <- run_multifactor_ports(returns, own, c("a", "b", "c"), base::min(shared$month_id),
    min_history = 15L, signal_scope = "factor", allocation = "ew")
  w <- dplyr::filter(fcmv$weights, method == "fcmv")
  testthat::expect_equal(w$w_raw, (1 + w$scaler) / 6)
  testthat::expect_true(base::all(fcmv$settings$managed_normalization == "training_vol"))
  testthat::expect_error(run_multifactor_ports(returns, own[-1, ], c("a", "b", "c"),
    base::min(shared$month_id), min_history = 15L, signal_scope = "factor"), "finite|consecutive")
})

testthat::test_that("Cederburg reuses saved scaling and calibrates only on prior months", {
  p <- allocation_fixture()
  for (normalization in c("saved", "training_vol")) {
    result <- run_cederburg_ports(p, normalization = normalization, min_history = 15L)
    w <- result$weights
    testthat::expect_equal(w$w_raw, w$a + w$b * w$scaler)
    testthat::expect_true(base::all(base::abs(w$w_capped) <= 5))
    testthat::expect_true(base::all(w$train_end < w$month_id))
    ew <- dplyr::filter(w, method == "ew")
    testthat::expect_equal(ew$w_raw, .5 + .5 * ew$scaler)
    first <- dplyr::filter(ew, name == "a", month_id == base::min(month_id))
    history <- dplyr::filter(p$w_compiled_df, name == "a", month_id < first$month_id)
    if (normalization == "training_vol") {
      c_t <- stats::sd(history$factor_exc) / stats::sd(history$factor_exc / history$signal_rv)
      testthat::expect_equal(first$normalizer, c_t)
      testthat::expect_equal(stats::sd(c_t * history$factor_exc / history$signal_rv),
                             stats::sd(history$factor_exc))
    }
    testthat::expect_equal(w$net_raw, w$gross_raw - .001 * w$turnover_raw)
    testthat::expect_equal(w$net_capped, w$gross_capped - .001 * w$turnover_capped)
    entry <- w %>% dplyr::group_by(name, method) %>% dplyr::slice_min(month_id) %>% dplyr::ungroup()
    testthat::expect_equal(entry$turnover_raw, base::abs(entry$w_raw))
    testthat::expect_equal(entry$turnover_capped, base::abs(entry$w_capped))
    if (normalization == "saved") {
      expected <- dplyr::left_join(ew, dplyr::select(p$w_compiled_df, name, month_id, saved = w_raw),
        by = c("name", "month_id"), relationship = "one-to-one")
      testthat::expect_equal(ew$scaler, expected$saved)
    }
    changed <- p
    cutoff <- base::as.Date("2002-02-01")
    changed$w_compiled_df$factor_exc[changed$w_compiled_df$month_id >= cutoff] <- .2
    later <- run_cederburg_ports(changed, normalization = normalization, min_history = 15L)$weights
    testthat::expect_equal(dplyr::filter(w, month_id <= cutoff)$w_raw,
                           dplyr::filter(later, month_id <= cutoff)$w_raw)
  }
  p$settings$implementable <- FALSE
  testthat::expect_silent(run_cederburg_ports(p, normalization = "training_vol", min_history = 15L))
  testthat::expect_error(run_cederburg_ports(p, normalization = "saved", min_history = 15L),
    "ex-post MM scale")
})
