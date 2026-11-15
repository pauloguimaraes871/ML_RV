## Invariants of the saved ensemble-versus-DeMiguel inference
## (R/demiguel_ml_inference.R): the design, the manuscript settings, complete
## coverage, and that tested series are rebuilt from their sources exactly.
## Guards follow the Codex review of 2026-11-06 (findings 2 and 3).

library("testthat")
`%>%` <- dplyr::`%>%`

inference_path <- file.path(here::here(), "data", "demiguel_ml_inference_v3.rds")
skip_without_outputs <- function() {
  testthat::skip_if_not(file.exists(inference_path),
                         "Inference outputs not built: run R/demiguel_ml_inference.R")
}
saved <- if (file.exists(inference_path)) readRDS(inference_path) else NULL
eval_months <- seq(as.Date("1977-02-01"), as.Date("2020-12-01"), by = "month")
nonmarket <- c("smb", "hml", "rmw", "cma", "umd", "roe", "ia", "bab")

testthat::test_that("design: one primary, DeMiguel specifications only, manuscript settings", {
  skip_without_outputs()
  con <- saved$contrasts
  testthat::expect_equal(sum(con$primary), 1L)
  primary <- dplyr::filter(con, primary)
  testthat::expect_equal(c(primary$family, primary$strategy, primary$benchmark),
                         c("T3_primary", "t3_cmv_D_cmv_tc_ew", "t3_cmv_D_cmv_tc_lagvar"))
  testthat::expect_setequal(unique(con$family), c("T3_primary", "T3_cmv", "T1_mvo", "EXT_fcmv"))
  testthat::expect_setequal(unique(con$scope), c("aggregate_8", "market", "cmv", "fcmv"))
  ## No standalone, unmanaged or UMV contrasts; FCMV only in its own family
  testthat::expect_true(all(con$panel %in% c("B", "C", "D", "E")))
  testthat::expect_true(all(con$comparator %in% c("lagvar", "lagss", "har", "harx")))
  testthat::expect_true(all(con$family[con$scope == "fcmv"] == "EXT_fcmv"))
  testthat::expect_false(any(con$family[con$scope != "fcmv"] == "EXT_fcmv"))
  testthat::expect_equal(saved$settings[c("draws", "seed", "block_lengths", "hac_lags", "gamma",
                                          "seed_dmu", "block_length_dmu")],
                         list(draws = 10000L, seed = 20260918L, block_lengths = c(5, 3, 10),
                              hac_lags = c(1L, 5L, 10L), gamma = 5, seed_dmu = 20261011L,
                              block_length_dmu = 5))
  testthat::expect_equal(nrow(saved$bootstrap), 3L * 3L * nrow(con))
  testthat::expect_equal(nrow(saved$spanning), 3L * nrow(con))
})

testthat::test_that("the complete series x month grid is present before any comparison", {
  skip_without_outputs()
  expected <- c(
    as.vector(outer(outer(c("t1_aggregate_8", "t1_market"), c("C", "D", "E"), paste, sep = "_"),
                    c("ew", "lagvar", "lagss", "har", "harx"), paste, sep = "_")),
    as.vector(outer(outer(outer(c("t3_cmv", "t3_fcmv"), c("B", "C", "D"), paste, sep = "_"),
                          c("cmv_notc", "cmv_tc"), paste, sep = "_"),
                    c("ew", "lagvar", "lagss", "harx"), paste, sep = "_")))
  testthat::expect_setequal(unique(saved$series$series_id), expected)
  coverage <- saved$series %>% dplyr::group_by(series_id) %>%
    dplyr::summarise(ok = identical(sort(month_id), eval_months), .groups = "drop")
  testthat::expect_true(all(coverage$ok))
})

testthat::test_that("aggregates are plain means of the eight non-market factors, gross and net", {
  skip_without_outputs()
  agg <- saved$t1_factor %>% dplyr::filter(factor %in% nonmarket, panel %in% c("C", "D", "E")) %>%
    dplyr::group_by(panel, signal, month_id) %>%
    dplyr::summarise(n = dplyr::n(), r = mean(excess_return), g = mean(gross_return), .groups = "drop") %>%
    dplyr::mutate(series_id = paste("t1_aggregate_8", panel, signal, sep = "_"))
  testthat::expect_true(all(agg$n == 8L))
  tested <- dplyr::filter(saved$series, startsWith(series_id, "t1_aggregate_8"))
  ## Full outer comparison: no aggregate row may be missing on either side
  joined <- dplyr::full_join(dplyr::select(agg, series_id, month_id, r, g),
                             dplyr::select(tested, series_id, month_id, excess_return, gross_return),
                             by = c("series_id", "month_id"))
  testthat::expect_false(anyNA(joined))
  testthat::expect_equal(joined$excess_return, joined$r)
  testthat::expect_equal(joined$gross_return, joined$g)
  ## MKT scope is the market factor alone, gross and net
  mkt <- saved$t1_factor %>% dplyr::filter(factor == "mkt", panel %in% c("C", "D", "E")) %>%
    dplyr::mutate(series_id = paste("t1_market", panel, signal, sep = "_")) %>%
    dplyr::full_join(dplyr::select(saved$series, series_id, month_id, s = excess_return, sg = gross_return) %>%
                       dplyr::filter(startsWith(series_id, "t1_market")), by = c("series_id", "month_id"))
  testthat::expect_false(anyNA(mkt[c("excess_return", "s", "gross_return", "sg")]))
  testthat::expect_equal(mkt$s, mkt$excess_return)
  testthat::expect_equal(mkt$sg, mkt$gross_return)
})

testthat::test_that("tested series are rebuilt exactly from the saved Table I/III results", {
  skip_without_outputs()
  t1 <- readRDS(file.path(here::here(), "data", "demiguel_table1_ml_v3.rds"))$paper_window
  t3 <- readRDS(file.path(here::here(), "data", "demiguel_table3_ml_v3.rds"))
  series_of <- function(id) { s <- saved$series[saved$series$series_id == id, ]; s[order(s$month_id), ] }
  ## Table I Panel E, ensemble, eight-factor aggregate: mean of net_e
  net_e <- purrr::map(nonmarket, ~ t1$ew$bde$results[[.x]]$oos$net_e)
  testthat::expect_equal(series_of("t1_aggregate_8_E_ew")$excess_return, Reduce(`+`, net_e) / 8)
  ## Table III Panel D, column (6), lagvar CMV: gross minus netted monthly cost
  run <- which(t3$runs$variant == "cmv" & t3$runs$signal == "lagvar" & t3$runs$start == as.Date("1967-02-01"))
  r <- t3$results[[run]]$returns
  testthat::expect_equal(series_of("t3_cmv_D_cmv_tc_lagvar")$excess_return,
                         unname(r$gross[, "cmv_tc"] - r$tc_td[, "cmv_tc"]))
  testthat::expect_equal(series_of("t3_cmv_D_cmv_tc_lagvar")$gross_return, unname(r$gross[, "cmv_tc"]))
})

testthat::test_that("the primary pair is Table III's column (6) and the estimate is its Sharpe gap", {
  skip_without_outputs()
  t3 <- utils::read.csv(file.path(here::here(), "data", "demiguel", "table3.csv"))
  t3ml <- utils::read.csv(file.path(here::here(), "data", "demiguel", "table3_ml.csv"))
  p <- dplyr::filter(saved$bootstrap, primary, metric == "sharpe", block_length == 5)
  testthat::expect_equal(nrow(p), 1L)
  testthat::expect_equal(p$benchmark_sharpe_dmu, t3$sr[t3$panel == "D" & t3$column == "cmv_tc"])
  testthat::expect_equal(p$strategy_sharpe_dmu,
                         t3ml$sr[t3ml$variant == "cmv" & t3ml$signal == "ew" & t3ml$start == "1967-02-01" &
                                   t3ml$panel == "D" & t3ml$column == "cmv_tc"])
  sr <- function(id) { r <- saved$series$excess_return[saved$series$series_id == id]; sqrt(12) * mean(r) / stats::sd(r) }
  testthat::expect_equal(p$estimate, sr(p$strategy) - sr(p$benchmark))
})

testthat::test_that("p-values are valid; DeMiguel's p-value is reported for every Sharpe contrast only", {
  skip_without_outputs()
  b <- saved$bootstrap
  testthat::expect_true(all(b$p_value > 0 & b$p_value <= 1))
  testthat::expect_true(all(b$p_holm >= b$p_value - 1e-12))
  testthat::expect_true(all(b$draws == 10000L & b$seed == 20260918L & b$status == "ok"))
  testthat::expect_false(anyNA(b$p_value_dmu[b$metric == "sharpe"]))
  testthat::expect_true(all(is.na(b$p_value_dmu[b$metric != "sharpe"])))
  testthat::expect_true(all(b$p_value_dmu[b$metric == "sharpe"] >= 0 & b$p_value_dmu[b$metric == "sharpe"] <= 1))
})

testthat::test_that("recorded input hashes match the files on disk", {
  skip_without_outputs()
  inputs <- saved$inputs
  testthat::expect_true(all(c("ensemble_cache", "table1_ml", "table3_ml", "stock") %in% inputs$input))
  current <- purrr::map_chr(file.path(here::here(), inputs$file),
                            ~ digest::digest(file = .x, algo = "sha256"))
  testthat::expect_identical(unname(current), unname(inputs$sha256))
})
