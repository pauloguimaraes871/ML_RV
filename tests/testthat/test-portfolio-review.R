source(file.path(here::here(), "R", "portfolio_helpers.R"))
source(file.path(here::here(), "R", "multifactor_helpers.R"))

testthat::test_that("constrained MVO solves a rank-deficient, bounded problem", {
  ## Three observations and four identical bases: rank one, but a bounded
  ## optimum exists. The aggregate optimum is 1.5 (unbounded optimum is 40/9).
  x <- matrix(rep(c(-.01, .02, .05), 4), ncol = 4)
  fit <- fit_multifactor(x, rep(0, 4), exposure_load = rep(1, 4), exposure_limit = 1.5)
  testthat::expect_equal(sum(fit$coef), 1.5, tolerance = 1e-7)
  testthat::expect_equal(fit$covariance_rank, 1L)
  testthat::expect_lt(fit$covariance_rcond, 1e-12)
  testthat::expect_lt(fit$kkt_residual, 1e-5)
  testthat::expect_equal(fit$utility, .0249375, tolerance = 1e-8)

  ## Changing sleeve units must leave the economic exposure and payoff intact.
  changed <- fit_multifactor(100 * x, rep(0, 4),
    exposure_load = rep(100, 4), exposure_limit = 1.5)
  testthat::expect_equal(as.vector(x %*% fit$coef),
    as.vector(100 * x %*% changed$coef), tolerance = 1e-7)
  testthat::expect_error(fit_multifactor(x, rep(0, 4),
    exposure_load = c(1, 0, 1, 1), exposure_limit = 1), "exposure")
})

testthat::test_that("MVO limits and insolvency guards apply to evaluated portfolios", {
  dates <- seq(as.Date("2000-01-01"), by = "month", length.out = 8)
  returns <- tibble::tibble(name = "a", month_id = dates,
    factor_exc = c(-.01, .02, .05, -.01, .02, .05, -2, .01), rf = 0)
  signal <- tibble::tibble(month_id = dates, sigma = rep(.1, 8))
  p <- run_multifactor_ports(returns, signal, "a", dates[6], min_history = 5L, cost_per_unit = 0)
  testthat::expect_true(all(p$monthly_returns$exposure_raw <= 1.5 + 1e-8))
  exposures <- p$monthly_returns %>% dplyr::select(month_id, method, exposure_raw) %>%
    tidyr::pivot_wider(names_from = method, values_from = exposure_raw)
  testthat::expect_true(all(exposures$cmv <= 1.2 * exposures$umv + 1e-8))
  failed <- dplyr::filter(p$strategy_summary, insolvency_months > 0)
  testthat::expect_gt(nrow(failed), 0)
  testthat::expect_true(all(is.na(failed$sharpe_net)))
  testthat::expect_true(all(is.na(failed$sharpe_paper)))
  testthat::expect_true(all(is.na(failed$cer_paper_ann)))
  testthat::expect_true(all(p$diagnostics$covariance_rank <= p$diagnostics$n_train - 1))
  testthat::expect_true(all(is.finite(p$strategy_summary$exposure_raw)))
})

testthat::test_that("execution snapshot captures effective rather than default configuration", {
  source(file.path(here::here(), "R", "helpers.R"))
  folder <- tempfile("provenance-")
  dir.create(folder)
  script <- file.path(folder, "runner.R")
  writeLines(c('models_to_run <- c("har", "ew")', 'answer <- models_to_run'), script)
  write_port_run_provenance(script, list(models_to_run = "ew"), folder, script)
  replay <- new.env(parent = baseenv())
  sys.source(file.path(folder, "runner_executed.R"), envir = replay)
  testthat::expect_identical(replay$answer, "ew")
  manifest <- utils::read.csv(file.path(folder, "input_manifest.csv"))
  testthat::expect_true(all(c("settings.rds", "runner_executed.R") %in% basename(manifest$file)))
  testthat::expect_identical(purrr::map_chr(manifest$file, hash_artifact), manifest$sha256)
})

testthat::test_that("upstream code changes warn but changed data invalidate the run", {
  source(file.path(here::here(), "R", "helpers.R"))
  folder <- tempfile("manifest-")
  dir.create(file.path(folder, "data"), recursive = TRUE)
  data <- file.path(folder, "data", "returns.txt")
  code <- file.path(folder, "runner.R")
  writeLines("unchanged", data)
  writeLines("version one", code)
  manifest <- file.path(folder, "input_manifest.csv")
  files <- c(data, code)
  utils::write.csv(tibble::tibble(file = files, sha256 = purrr::map_chr(files, hash_artifact)),
    manifest, row.names = FALSE)
  testthat::expect_silent(validate_port_manifest(manifest))
  writeLines("version two", code)
  testthat::expect_warning(validate_port_manifest(manifest), "code differs")
  writeLines("changed", data)
  testthat::expect_error(validate_port_manifest(manifest), "model/data inputs have changed")
})

testthat::test_that("report risk is measured after aggregation on the payoff dates", {
  source(file.path(here::here(), "R", "reporting_helpers.R"))
  dates <- seq(as.Date("2000-01-01"), by = "month", length.out = 4)
  data <- tibble::tibble(month_id = dates, excess_return = c(-.04, .06, -.02, .04),
    total_return = excess_return, factor_exc = excess_return / 2, w_raw = 2)
  result <- summarise_portfolio_returns(data, character(),
    unmanaged_col = "factor_exc", weight_col = "w_raw")
  testthat::expect_equal(result$vol_ratio, 2)
  testthat::expect_equal(result$avg_weight_raw, 2)
  testthat::expect_equal(result$cer_ann, .052, tolerance = 1e-10)
  single <- summarise_portfolio_returns(data[1, ], character(),
    unmanaged_col = "factor_exc", weight_col = "w_raw")
  testthat::expect_true(is.na(single$vol_ratio))
})

testthat::test_that("risk diagnostics reveal the normalization level", {
  dates <- seq(as.Date("2000-01-01"), by = "month", length.out = 10)
  target <- tibble::tibble(name = "a", month_id = dates, rv_month = .04)
  forecast <- target %>% dplyr::filter(month_id >= dates[4]) %>%
    dplyr::transmute(name, month_id, target = log(rv_month + 1e-8), pred = log(.02 + 1e-8))
  returns <- target %>% dplyr::transmute(name, month_id,
    factor_exc = rep(c(-.02, .03), 5), rf = 0)
  p <- run_oos_ports(forecast, returns, target, min_history = 3L, cost_per_unit = 0)
  s <- dplyr::filter(p$strategy_summary, method == "vol_scaled")
  testthat::expect_equal(s$avg_weight_raw, 2)
  testthat::expect_equal(s$vol_ratio_raw, 2)
  testthat::expect_equal(s$vol_ratio_capped, 1.5)
  testthat::expect_equal(s$vol_ratio_net_raw, 2)
})

testthat::test_that("an optimized exposure constraint changes the allocation direction", {
  ## Orthogonal shocks give covariance .01 I and means (.10, .08).
  ## Unbounded coefficients are (2, 1.6). On sum(w)=1.5, equal marginal
  ## utility requires w1-w2=.4: the optimum is (.95, .55), NOT rescaled MVO.
  shocks <- sqrt(.0075) * cbind(c(-1, -1, 1, 1), c(-1, 1, -1, 1))
  x <- sweep(shocks, 2, c(.10, .08), "+")
  fit <- fit_multifactor(x, c(0, 0), exposure_limit = 1.5)
  testthat::expect_equal(fit$coef, c(.95, .55), tolerance = 1e-7)
  basis <- tibble::tibble(id = c("a", "b"), name = c("a", "b"),
    sleeve = "unmanaged", cluster = "one")
  colnames(x) <- basis$id
  testthat::expect_equal(allocate_portfolio(x, basis, nonnegative = FALSE)$coef,
    c(2, 1.6), tolerance = 1e-8)
})

testthat::test_that("Cederburg's lagged baseline is distinct and normalizer independence is conditional", {
  source(file.path(here::here(), "R", "cederburg_helpers.R"))
  dates <- seq(as.Date("2000-01-01"), by = "month", length.out = 15)
  i <- seq_along(dates)
  ml <- tibble::tibble(name = "a", month_id = dates, method = "vol_scaled",
    factor_exc = .01 + .03 * sin(i), rf = 0,
    signal_rv = .001 * (2 + cos(i)), w_raw = 1 / (2 + cos(i)))
  rw <- ml %>% dplyr::mutate(method = "rw", signal_rv = .001 * (2 + sin(i)),
    w_raw = 1 / (2 + sin(i)))
  p <- list(w_compiled_df = dplyr::bind_rows(ml, rw), settings = list(implementable = TRUE))
  baseline <- run_cederburg_ports(p, managed_method = "rw", min_history = 8L)
  prediction <- run_cederburg_ports(p, min_history = 8L)
  testthat::expect_false(isTRUE(all.equal(baseline$weights$w_raw, prediction$weights$w_raw)))
  changed <- p
  changed$w_compiled_df$w_raw <- 2 * changed$w_compiled_df$w_raw
  testthat::expect_equal(run_cederburg_ports(changed, min_history = 8L), prediction)
  original_saved <- run_cederburg_ports(p, normalization = "saved", min_history = 8L)
  changed_saved <- run_cederburg_ports(changed, normalization = "saved", min_history = 8L)
  testthat::expect_false(isTRUE(all.equal(original_saved$weights$w_raw, changed_saved$weights$w_raw)))
  testthat::expect_error(run_cederburg_ports(p, min_history = 15L), "Insufficient")
})
