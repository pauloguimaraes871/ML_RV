#####################################################################
##  DeMiguel et al. (2024) Table III, equal-weighted: ML vs lagvar  ##
#####################################################################
#
# Equal-weighted counterpart of the Table III part of
# R/demiguel_ml_inference.R. Each contrast pairs the EW CMV portfolio
# (column cmv_ew: 1/18 per unmanaged factor and per volatility-matched managed
# sleeve) built with one signal (strategy: ew ensemble, harx, lagss) with the
# SAME portfolio built with lagvar, DeMiguel's lagged daily variance
# (benchmark). Same estimation history, costs and evaluation months (February
# 1977 - December 2020). Panels B (gross), C (minus sleeve-by-sleeve costs),
# D (minus portfolio-netted costs).
#
#   cmv   every factor scaled by MARKET volatility, 1967 sample, M = 120;
#   fcmv  each factor scaled by its OWN volatility, 1970 sample, M = 83 (our
#         extension, its own family).
#
# Inference: the manuscript's machinery, settings as R/demiguel_ml_inference.R:
# paired stationary block bootstrap on one common calendar, 10,000 draws, seed
# 20260918, block length 5 (3 and 10 sensitivity); centered one-sided test of
# H0: difference <= 0 for Sharpe, mean and CER (gamma 5); Holm and BH within
# (block length, family); Newey-West spanning alpha at lags 1, 5, 10; and
# DeMiguel's own uncentered Sharpe p-value as robustness (p_value_dmu).
# Families: T3EW_cmv, EXT_fcmv_ew. No contrast is prespecified as primary:
# all are exploratory.
#
# Return series: net where the panel has costs, gross otherwise; total return
# = excess return + RF (insolvency check only).
#
# Requires, in this order: allocation <- "ew"; source("R/demiguel_table3.R");
# source("R/demiguel_table3_ml.R"). Reads saved results only. About a minute.

  `%>%` <- dplyr::`%>%`

  for (helper in c("helpers", "classes", "reporting_helpers", "portfolio_helpers", "allocation_helpers",
                   "portfolio_inference_helpers", "demiguel_cost_helpers",
                   "demiguel_replication_helpers", "demiguel_table3_helpers")) {
    source(file.path(here::here(), "R", paste0(helper, ".R")))
  }

# Configuration-----------------------------------------------------------------

  data_dir <- file.path(here::here(), "data")
  output_dir <- file.path(data_dir, "demiguel")

  ## Manuscript settings, as R/demiguel_ml_inference.R
  draws <- 10000L
  seed <- 20260918L
  block_lengths <- c(5, 3, 10)          # 5 primary; 3 and 10 sensitivity only
  hac_lags <- c(1L, 5L, 10L)            # 1 primary
  gamma <- 5
  seed_dmu <- 20261011L
  block_length_dmu <- 5

  eval_months <- seq(as.Date("1977-02-01"), as.Date("2020-12-01"), by = "month")
  benchmark_signal <- "lagvar"
  strategy_signals <- c("ew", "harx", "lagss")
  t3_panels <- c("B", "C", "D")
  column <- "cmv_ew"

# Inputs------------------------------------------------------------------------

  t3_path <- file.path(data_dir, "demiguel_table3_ml_ew_v3.rds")
  t3 <- readRDS(t3_path)
  if (!identical(t3$settings$allocation, "ew")) rlang::abort("Expected the equal-weighted Table III-ML runs.")
  monthly <- readRDS(file.path(data_dir, "demiguel_returns_monthly_v3.rds"))

  ## Collateral RF by month (the same for every factor row)
  rf <- monthly %>% dplyr::distinct(month_id, rf) %>% dplyr::filter(month_id %in% eval_months) %>%
    dplyr::arrange(month_id)
  if (!identical(rf$month_id, eval_months)) rlang::abort("RF must be unique and complete per evaluation month.")

# Series--------------------------------------------------------------------------

  ## CMV on the 1967 sample and FCMV on the 1970 sample; the cmv lagvar 1970
  ## control run is not part of any contrast.
  signals <- c(benchmark_signal, strategy_signals)
  runs <- t3$runs %>% dplyr::mutate(run = dplyr::row_number()) %>%
    dplyr::filter(signal %in% signals,
                  (variant == "cmv" & start == as.Date("1967-02-01")) | variant == "fcmv")
  ## Exact estimation window per variant: a different start with the same
  ## evaluation months would otherwise pass every later check.
  expected <- tibble::tibble(variant = c("cmv", "fcmv"),
                             start = as.Date(c("1967-02-01", "1970-03-01")), M = c(120L, 83L))
  if (nrow(runs) != 2L * length(signals) || anyDuplicated(runs[c("variant", "signal")]) ||
      nrow(dplyr::anti_join(runs, expected, by = c("variant", "start", "M"))) > 0L) {
    rlang::abort("Unexpected equal-weighted Table III-ML runs.")
  }

  series <- purrr::pmap_dfr(runs, function(variant, signal, start, M, run) {
    r <- t3$results[[run]]$returns
    if (!identical(r$months, eval_months)) rlang::abort(paste("Run off the calendar:", variant, signal))
    gross <- r$gross[, column]
    net <- list(B = gross, C = gross - r$tc_ntd[, column], D = gross - r$tc_td[, column])
    purrr::imap_dfr(net, function(x, panel) {
      tibble::tibble(scope = variant, panel = panel, signal = signal, month_id = eval_months,
                     excess_return = x, gross_return = gross)
    })
  }) %>%
    dplyr::mutate(series_id = paste("t3ew", scope, panel, signal, sep = "_"))

  ## Reconciliation: DeMiguel-convention Sharpe (net mean over gross sd) must
  ## equal the saved table, run by run and panel by panel.
  dmu_sr <- function(x, g) sqrt(12) * mean(x) / stats::sd(g)
  ok <- purrr::pmap_lgl(runs, function(variant, signal, start, M, run) {
    saved <- t3$table %>% dplyr::filter(variant == !!variant, signal == !!signal, start == !!start,
                                        column == !!column, panel %in% t3_panels)
    if (!setequal(saved$panel, t3_panels) || anyDuplicated(saved$panel)) return(FALSE)
    all(purrr::map2_lgl(saved$panel, saved$sr, function(p, sr) {
      s <- series[series$series_id == paste("t3ew", variant, p, signal, sep = "_"), ]
      nrow(s) == length(eval_months) && isTRUE(all.equal(dmu_sr(s$excess_return, s$gross_return), sr))
    }))
  })
  if (!all(ok)) rlang::abort("EW series do not reproduce the saved Table III-ML Sharpe ratios.")

  series <- series %>%
    dplyr::left_join(rf, by = "month_id", relationship = "many-to-one") %>%
    dplyr::mutate(total_return = excess_return + rf)

  series_stats <- series %>%
    dplyr::group_by(series_id, scope, panel, signal) %>%
    dplyr::summarise(mean_ann = 12 * mean(excess_return), sd_ann = sqrt(12) * stats::sd(excess_return),
                     sharpe = sqrt(12) * mean(excess_return) / stats::sd(excess_return),
                     sharpe_dmu = dmu_sr(excess_return, gross_return),
                     cer_ann = 12 * (mean(excess_return) - gamma / 2 * stats::var(excess_return)),
                     .groups = "drop")

# Contrasts-----------------------------------------------------------------------

  contrasts <- tidyr::expand_grid(scope = c("cmv", "fcmv"), panel = t3_panels, signal = strategy_signals) %>%
    dplyr::mutate(strategy = paste("t3ew", scope, panel, signal, sep = "_"),
                  benchmark = paste("t3ew", scope, panel, benchmark_signal, sep = "_"),
                  family = ifelse(scope == "cmv", "T3EW_cmv", "EXT_fcmv_ew"),
                  comparison_id = paste(strategy, "vs", benchmark_signal, sep = "_")) %>%
    dplyr::select(comparison_id, family, strategy, benchmark, scope, panel, signal)
  if (!all(c(contrasts$strategy, contrasts$benchmark) %in% series$series_id)) {
    rlang::abort("A contrast references a missing series.")
  }

# Paired stationary bootstrap (manuscript) and DeMiguel's procedure---------------

  inference_data <- dplyr::select(series, series_id, month_id, excess_return, total_return)
  bootstrap <- purrr::map_dfr(block_lengths, function(block_length) {
    message("Paired stationary bootstrap: ", draws, " draws, expected block length ", block_length, ".")
    calendar <- draw_portfolio_months(eval_months, draws, block_length, seed)
    infer_portfolio_pairs(inference_data, dplyr::select(contrasts, comparison_id, family, strategy, benchmark),
                          calendar, gamma = gamma)
  }) %>%
    dplyr::group_by(block_length, family) %>%
    dplyr::mutate(p_holm = stats::p.adjust(p_value, "holm"), p_bh = stats::p.adjust(p_value, "BH")) %>%
    dplyr::ungroup()

  calendar_dmu <- draw_portfolio_months(eval_months, draws, block_length_dmu, seed_dmu)
  returns_of <- function(id) {
    s <- series[series$series_id == id, ]
    s$excess_return[order(s$month_id)]
  }
  dmu_pvalues <- purrr::pmap_dfr(contrasts, function(comparison_id, strategy, benchmark, ...) {
    tibble::tibble(comparison_id = comparison_id, metric = "sharpe",
                   p_value_dmu = demiguel_sr_pvalue(returns_of(benchmark), returns_of(strategy), calendar_dmu))
  })
  bootstrap <- bootstrap %>%
    dplyr::left_join(dmu_pvalues, by = c("comparison_id", "metric"), relationship = "many-to-one") %>%
    dplyr::left_join(dplyr::select(contrasts, comparison_id, scope, panel, signal),
                     by = "comparison_id", relationship = "many-to-one") %>%
    dplyr::left_join(dplyr::select(series_stats, strategy = series_id, strategy_sharpe = sharpe,
                                   strategy_sharpe_dmu = sharpe_dmu), by = "strategy") %>%
    dplyr::left_join(dplyr::select(series_stats, benchmark = series_id, benchmark_sharpe = sharpe,
                                   benchmark_sharpe_dmu = sharpe_dmu), by = "benchmark")

# Spanning regressions (manuscript)---------------------------------------------

  spanning <- purrr::map_dfr(hac_lags, function(lag) {
    purrr::pmap_dfr(contrasts, function(comparison_id, family, strategy, benchmark, ...) {
      data <- tibble::tibble(month_id = eval_months, excess_return = returns_of(strategy),
                             benchmark_return = returns_of(benchmark))
      span_portfolio_returns(data, lag) %>% dplyr::mutate(comparison_id, family, .before = 1)
    })
  }) %>% dplyr::left_join(dplyr::select(contrasts, comparison_id, scope, panel, signal),
                          by = "comparison_id", relationship = "many-to-one")

# Report------------------------------------------------------------------------

  cat("\nEqual-weighted Table III, signal vs lagvar, block 5 (Sharpe and CER):\n")
  print(as.data.frame(bootstrap %>% dplyr::filter(block_length == 5, metric %in% c("sharpe", "cer_ann")) %>%
                        dplyr::select(family, scope, panel, signal, metric, strategy_sharpe_dmu,
                                      benchmark_sharpe_dmu, estimate, p_value, p_holm, p_value_dmu) %>%
                        dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ~ round(.x, 4)))),
        row.names = FALSE)

# Save--------------------------------------------------------------------------

  save_if_changed(bootstrap, file.path(output_dir, "table3_ew_inference_bootstrap.csv"))
  save_if_changed(spanning, file.path(output_dir, "table3_ew_inference_spanning.csv"))
  save_if_changed(series_stats, file.path(output_dir, "table3_ew_inference_series.csv"))
  save_if_changed(list(series = series, contrasts = contrasts, bootstrap = bootstrap, spanning = spanning,
                       settings = list(draws = draws, seed = seed, block_lengths = block_lengths,
                                       hac_lags = hac_lags, gamma = gamma, seed_dmu = seed_dmu,
                                       block_length_dmu = block_length_dmu, eval_months = range(eval_months),
                                       benchmark_signal = benchmark_signal, strategy_signals = strategy_signals,
                                       input = basename(t3_path), input_sha256 = hash_artifact(t3_path))),
                  file.path(data_dir, "demiguel_table3_ew_inference_v3.rds"), "rds")
  cat("\nSaved data/demiguel/table3_ew_inference_{bootstrap,spanning,series}.csv and",
      "data/demiguel_table3_ew_inference_v3.rds\n")
