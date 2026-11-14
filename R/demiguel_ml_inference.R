################################################################
##  DeMiguel et al. (2024): ML ensemble versus lagged variance  ##
################################################################
#
# Does the ML ensemble improve on DeMiguel's own specifications? Each
# contrast pairs a DeMiguel portfolio built with the EW ensemble variance
# forecast (strategy) with the SAME portfolio, built by his unchanged code,
# with a benchmark signal: lagvar (the paper's lagged daily variance), lagss
# (lagged sum of squared returns, the manuscript's random walk), har, harx.
# Same estimation history, costs and evaluation months (February 1977 -
# December 2020).
#
# Scope (owner decisions, 2026-11-06), DeMiguel specifications only:
#   * Table III CMV (columns 3 and 6, Panels B, C, D) and Table I MVO
#     combination (Panels C, D, E). In both, DeMiguel's normalizing constant
#     c is absorbed by the estimated weights, so his full-sample c has no
#     effect on any tested return.
#   * NOT tested: the standalone managed factor (not a DeMiguel portfolio;
#     with his full-sample c, a pooled average of standalone sleeves would
#     weight factors with future information). Its per-factor differences
#     stay as descriptive output (c cancels within one factor).
#   * FCMV (own-factor volatility, March 1970 sample) is OUR extension, in its
#     own family EXT_fcmv; it never enters the DeMiguel families.
#   * No contrast against the unmanaged factor or UMV (already tested in
#     Tables I and III; UMV column (1) is ~15x levered and fails the
#     manuscript's insolvency guard).
#
# Inference: the manuscript's machinery on top of DeMiguel's return series
# (R/portfolio_inference_helpers.R, as R/report_portfolio_inference.R):
#   * POOLED tests, never per factor (as portfolio_inference_inputs.R):
#     Table I MVO returns of the eight non-market factors are averaged month by
#     month (scope aggregate_8, as aggregate_153); MKT is its own scope (as
#     market_pooled). Table III is already multifactor.
#   * paired stationary block bootstrap on one common calendar, 10,000 draws,
#     seed 20260918, block length 5 (3 and 10 sensitivity only); centered
#     one-sided test of H0: difference <= 0 for Sharpe, mean and CER (gamma 5);
#     Holm and BH within (block length, family); Newey-West spanning alpha
#     at lags 1 (primary), 5, 10.
#   * families: T3_primary (ONLY the prespecified primary contrast), T3_cmv,
#     T1_mvo, EXT_fcmv.
#   * robustness column p_value_dmu: DeMiguel's own uncentered Politis-Romano
#     procedure (demiguel_sr_pvalue(), exact port of dmu_StatisticsPolitisRomano.m)
#     for every Sharpe contrast, with the replication's bootstrap settings
#     (10,000 draws, block 5, seed 20261011). Not multiplicity-adjusted.
#
# Prespecification and design history: the PRIMARY test (Table III Panel D,
# CMV column (6), Sharpe, ensemble vs lagvar, L = 5) was fixed before any
# p-value was computed. The pooled Table I design and the restriction to
# DeMiguel specifications were adopted after earlier per-factor and
# standalone results had been seen; Holm within families does not account
# for that. Secondary results are exploratory.
#
# Return series: net where the panel has costs, gross otherwise; total return
# = excess return + RF (insolvency check only). The manuscript Sharpe is
# mean / sd of the tested series; DeMiguel's tables divide the NET mean by the
# GROSS sd. Both are reported.
#
# Reads saved results only; fits and optimizes nothing. About a minute.

  `%>%` <- dplyr::`%>%`

  for (helper in c("helpers", "classes", "reporting_helpers", "portfolio_helpers", "allocation_helpers",
                   "portfolio_inference_helpers", "demiguel_cost_helpers",
                   "demiguel_replication_helpers", "demiguel_table3_helpers")) {
    source(file.path(here::here(), "R", paste0(helper, ".R")))
  }

# Configuration-----------------------------------------------------------------

  data_dir <- file.path(here::here(), "data")
  output_dir <- file.path(data_dir, "demiguel")
  model_dir <- file.path(here::here(), "models_v3", "demiguel", "1927")
  cache_path <- file.path(model_dir, "ensembles", "ew_demiguel_range_grid_1927.rds")

  ## Manuscript settings (report_portfolio_inference())
  draws <- 10000L
  seed <- 20260918L
  block_lengths <- c(5, 3, 10)          # 5 primary; 3 and 10 sensitivity only
  hac_lags <- c(1L, 5L, 10L)            # 1 primary
  gamma <- 5
  ## DeMiguel's procedure, with the replication's settings (Tables I and III)
  seed_dmu <- 20261011L
  block_length_dmu <- 5

  eval_months <- seq(as.Date("1977-02-01"), as.Date("2020-12-01"), by = "month")
  strategy_signal <- "ew"
  signals_t1 <- c("lagvar", "lagss", "har", "harx")    # Table I-ML signals besides ew
  signals_t3 <- c("lagvar", "lagss", "harx")           # signals run in Table III-ML
  ensemble_members <- c("harx", "glmnet", "rf", "xgb", "nn1", "nn2", "nn3")
  market <- "mkt"                                      # tested as its own scope
  mvo_panels <- c("C", "D", "E")                       # tested Table I panels
  t3_panels <- c("B", "C", "D")
  t3_columns <- c("cmv_notc", "cmv_tc")

# Inputs------------------------------------------------------------------------

  input_files <- c(
    monthly = file.path(data_dir, "demiguel_returns_monthly_v3.rds"),
    daily = file.path(data_dir, "demiguel_returns_daily_v3.rds"),
    target = file.path(data_dir, "target_demiguel_v3.rds"),
    stock = file.path(data_dir, "demiguel_stock_costs_v3.rds"),
    table1_ml = file.path(data_dir, "demiguel_table1_ml_v3.rds"),
    table3_ml = file.path(data_dir, "demiguel_table3_ml_v3.rds"),
    ensemble_cache = cache_path
  )
  monthly <- readRDS(input_files[["monthly"]])
  daily <- readRDS(input_files[["daily"]])
  target <- readRDS(input_files[["target"]])
  stock <- readRDS(input_files[["stock"]])
  t1_saved <- readRDS(input_files[["table1_ml"]])
  t1 <- t1_saved$paper_window
  t3 <- readRDS(input_files[["table3_ml"]])

  ## Collateral RF by month (the same for every factor row)
  rf <- monthly %>% dplyr::distinct(month_id, rf) %>% dplyr::filter(month_id %in% eval_months) %>%
    dplyr::arrange(month_id)
  if (!identical(rf$month_id, eval_months)) rlang::abort("RF must be unique and complete per evaluation month.")

# Forecast provenance (Codex review 2026-11-06, finding 2)-----------------------

  ## Every model is read through read_model_result() (hashes, targets,
  ## metrics). The EW ensemble is REBUILT in memory from its seven audited
  ## members and must equal the cache; the members must be exactly the models
  ## recorded by the saved Table I-ML and Table III-ML runs. No stale cache or
  ## mixed ensemble version can enter.
  model_files <- list.files(model_dir, pattern = "_demiguel_range_(grid|none)_1927\\.rds$", full.names = TRUE)
  models <- purrr::map(model_files, read_model_result, data_dir = data_dir) %>%
    stats::setNames(sub("_.*$", "", basename(model_files)))
  if (!setequal(names(models), c("har", ensemble_members))) rlang::abort("Unexpected set of saved DeMiguel models.")
  rebuilt <- build_ew_ensemble(models, ensemble_members)
  cache <- readRDS(cache_path)
  cache_oos <- if (isS4(cache)) cache@oos_outputs else cache$oos_outputs
  paired <- dplyr::full_join(dplyr::select(rebuilt$oos_outputs, name, month_id, pred),
                             dplyr::select(cache_oos, name, month_id, pred),
                             by = c("name", "month_id"), suffix = c("_rebuilt", "_cache"),
                             relationship = "one-to-one")
  if (anyNA(paired) || !isTRUE(all.equal(paired$pred_rebuilt, paired$pred_cache, tolerance = 1e-12))) {
    rlang::abort("The EW ensemble cache does not equal the ensemble rebuilt from its audited members.")
  }
  source_key <- function(s) sort(paste(basename(s$file), s$sha256))
  current_sources <- purrr::map_dfr(models, "source")
  if (!identical(source_key(current_sources), source_key(t1_saved$settings$sources)) ||
      !identical(source_key(current_sources), source_key(t3$settings$sources))) {
    rlang::abort("Saved Table I/III-ML runs used different model files than the current ones.")
  }
  input_manifest <- tibble::tibble(
    input = c(names(input_files), paste0("model_", names(models))),
    file = c(sub(paste0("^", here::here(), "/"), "", input_files),
             sub(paste0("^", here::here(), "/"), "", model_files)),
    sha256 = c(purrr::map_chr(input_files, hash_artifact), purrr::map_chr(model_files, hash_artifact))
  )

  ## Variance signals for the per-factor standalone description, as in
  ## R/demiguel_table1_ml.R.
  ml_signal <- function(oos) dplyr::transmute(oos, name, month_id, variance = exp(pred) - 1e-8)
  signals <- list(
    lagvar = prepare_factor_variance(daily),
    lagss = target %>% dplyr::transmute(
      name, month_id = lubridate::add_with_rollback(month_id, lubridate::period(month = 1)),
      variance = rv_month),
    har = ml_signal(models$har$oos_outputs),
    harx = ml_signal(models$harx$oos_outputs),
    ew = ml_signal(rebuilt$oos_outputs)
  )
  if (any(purrr::map_lgl(signals, ~ any(!is.finite(.x$variance) | .x$variance <= 0)))) {
    rlang::abort("Variance signals must be finite and positive.")
  }

# Per-factor monthly series (Table I)-------------------------------------------

  ## One row per factor, panel, signal and month: excess_return (tested) and
  ## gross_return (for DeMiguel's Sharpe convention). Panels: MVO combination
  ## C (gross), D and E (net) -- tested after pooling; standalone managed
  ## factor SAg (gross), SAn (net) -- per-factor description only.
  factors <- demiguel_table1_paper$name
  t1_start <- function(k) factor_setting(t1[[strategy_signal]]$bde$settings$start, k)
  if (!all(purrr::map_lgl(signals_t1, function(s) {
    identical(t1[[s]]$bde$settings$start, t1[[strategy_signal]]$bde$settings$start)
  }))) rlang::abort("Table I signals must share their estimation history (paper window).")

  panel_rows <- function(k, signal, parts) {
    n <- length(eval_months)
    purrr::imap_dfr(parts, function(v, panel) {
      tibble::tibble(factor = k, panel = panel, signal = signal, month_id = eval_months,
                     excess_return = v[seq_len(n)], gross_return = v[n + seq_len(n)])
    })
  }
  t1_factor <- purrr::map_dfr(c(strategy_signal, signals_t1), function(sig) {
    purrr::map_dfr(factors, function(k) {
      c_ret <- t1[[sig]]$ac$returns %>% dplyr::filter(name == k, panel == "C_out_of_sample") %>%
        dplyr::arrange(month_id)
      oos <- t1[[sig]]$bde$results[[k]]$oos
      sa <- standalone_managed_series(monthly, signals[[sig]], stock, k, t1_start(k),
                                      as.Date("2020-12-01"), min(eval_months))
      if (!identical(c_ret$month_id, eval_months) || !identical(oos$month_id, eval_months) ||
          !identical(sa$month_id, eval_months)) {
        rlang::abort(paste("Table I series off the evaluation calendar:", sig, k))
      }
      panel_rows(k, sig, list(C = c(c_ret$managed_combo, c_ret$managed_combo),
                              D = c(oos$net_d, oos$gross_c), E = c(oos$net_e, oos$gross_e),
                              SAg = c(sa$gross, sa$gross), SAn = c(sa$net, sa$gross)))
    })
  })

  ## Reconciliation with the saved tables (same returns, nothing recomputed):
  ## DeMiguel-convention Sharpe ratios must equal the published-style outputs,
  ## factor by factor, matched by name.
  dmu_sr <- function(x, g) sqrt(12) * mean(x) / stats::sd(g)
  factor_sr <- t1_factor %>% dplyr::group_by(factor, panel, signal) %>%
    dplyr::summarise(n = dplyr::n(), sr = dmu_sr(excess_return, gross_return), .groups = "drop")
  t1_ok <- purrr::map_lgl(c(strategy_signal, signals_t1), function(sig) {
    tab <- t1[[sig]]$table
    if (!setequal(tab$name, factors) || anyDuplicated(tab$name)) return(FALSE)
    all(purrr::imap_lgl(c(C = "sr_oos", D = "sr_oos_net", E = "sr_oos_td"), function(column, pn) {
        ours <- factor_sr[factor_sr$panel == pn & factor_sr$signal == sig, ]
        nrow(ours) == length(factors) && all(ours$n == length(eval_months)) &&
          isTRUE(all.equal(ours$sr[match(factors, ours$factor)], tab[[column]][match(factors, tab$name)]))
      }))
  })
  if (!all(t1_ok)) rlang::abort("Table I series do not reproduce the saved Sharpe ratios.")

# Pooled Table I series and Table III series--------------------------------------

  ## Equal-weighted eight-factor aggregate of the MVO returns, month by month
  ## (as aggregate_153: the return of an equal-weighted factor portfolio, not
  ## an average of factor Sharpe ratios), and MKT as its own scope (as
  ## market_pooled). Gross and net are averaged alike. No cross-factor trade
  ## netting: each factor's book is priced separately.
  nonmarket <- setdiff(factors, market)
  mvo_factor <- dplyr::filter(t1_factor, panel %in% mvo_panels)
  t1_pooled <- dplyr::bind_rows(
    mvo_factor %>% dplyr::filter(factor %in% nonmarket) %>%
      dplyr::group_by(panel, signal, month_id) %>%
      dplyr::summarise(n_factors = dplyr::n(), excess_return = mean(excess_return),
                       gross_return = mean(gross_return), .groups = "drop") %>%
      dplyr::mutate(scope = "aggregate_8"),
    mvo_factor %>% dplyr::filter(factor == market) %>%
      dplyr::transmute(panel, signal, month_id, n_factors = 1L, excess_return, gross_return,
                       scope = "market")
  )
  if (any(t1_pooled$n_factors[t1_pooled$scope == "aggregate_8"] != length(nonmarket))) {
    rlang::abort("Every aggregate month must hold all eight non-market factors.")
  }
  t1_pooled <- t1_pooled %>%
    dplyr::mutate(table = "I", portfolio = "mvo", series_id = paste("t1", scope, panel, signal, sep = "_"))

  ## Table III: CMV (market signal, 1967 sample) and FCMV (own signal, 1970
  ## sample, our extension), columns (3) cmv_notc and (6) cmv_tc, Panels B
  ## (gross), C (minus sleeve-by-sleeve costs), D (minus portfolio-netted costs).
  t3_runs <- t3$runs %>% dplyr::mutate(run = dplyr::row_number()) %>%
    dplyr::filter(signal %in% c(strategy_signal, signals_t3),
                  (variant == "cmv" & start == as.Date("1967-02-01")) | variant == "fcmv")
  if (nrow(t3_runs) != 2L * (1L + length(signals_t3)) ||
      anyDuplicated(t3_runs[c("variant", "signal")])) rlang::abort("Unexpected Table III-ML runs.")
  t3_parts <- function(r, col) {
    gross <- r$gross[, col]
    list(B = c(gross, gross), C = c(gross - r$tc_ntd[, col], gross), D = c(gross - r$tc_td[, col], gross))
  }
  t3_series <- purrr::pmap_dfr(t3_runs, function(variant, signal, start, M, run) {
    r <- t3$results[[run]]$returns
    if (!identical(r$months, eval_months)) rlang::abort(paste("Table III run off the calendar:", variant, signal))
    purrr::map_dfr(t3_columns, function(col) panel_rows(col, signal, t3_parts(r, col))) %>%
      dplyr::mutate(variant = variant)
  }) %>%
    dplyr::transmute(table = "III", scope = variant, panel, portfolio = factor, signal, month_id,
                     n_factors = 9L, excess_return, gross_return,
                     series_id = paste("t3", scope, panel, portfolio, signal, sep = "_"))

  series <- dplyr::bind_rows(t1_pooled, t3_series)

  ## Complete grid BEFORE any join (Codex finding 3): exactly the expected
  ## series ids, each with exactly the 527 evaluation months.
  expected_ids <- c(
    tidyr::expand_grid(scope = c("aggregate_8", "market"), panel = mvo_panels,
                       signal = c(strategy_signal, signals_t1)) %>%
      purrr::pmap_chr(function(scope, panel, signal) paste("t1", scope, panel, signal, sep = "_")),
    tidyr::expand_grid(scope = c("cmv", "fcmv"), panel = t3_panels, portfolio = t3_columns,
                       signal = c(strategy_signal, signals_t3)) %>%
      purrr::pmap_chr(function(scope, panel, portfolio, signal) paste("t3", scope, panel, portfolio, signal, sep = "_"))
  )
  grid <- series %>% dplyr::group_by(series_id) %>%
    dplyr::summarise(ok = identical(sort(month_id), eval_months) && !anyDuplicated(month_id), .groups = "drop")
  if (!setequal(grid$series_id, expected_ids) || length(expected_ids) != nrow(grid) || !all(grid$ok)) {
    rlang::abort("Tested series do not form the complete expected series x month grid.")
  }

  ## Reconciliation: Table III series reproduce the saved Table III-ML Sharpe
  ## ratios, requiring the EXACT unique (panel, column) reference keys.
  expected_keys <- sort(paste(rep(t3_panels, each = length(t3_columns)), t3_columns))
  t3_ok <- purrr::pmap_lgl(t3_runs, function(variant, signal, start, M, run) {
    saved <- t3$table %>% dplyr::filter(variant == !!variant, signal == !!signal, start == !!start,
                                        panel %in% t3_panels, column %in% t3_columns)
    keys <- paste(saved$panel, saved$column)
    if (anyDuplicated(keys) || !identical(sort(keys), expected_keys)) return(FALSE)
    all(purrr::pmap_lgl(saved, function(panel, column, sr, ...) {
      ## Build the id OUTSIDE the data mask: series has its own `panel` column
      id <- paste("t3", variant, panel, column, signal, sep = "_")
      s <- series[series$series_id == id, ]
      nrow(s) == length(eval_months) && isTRUE(all.equal(dmu_sr(s$excess_return, s$gross_return), sr))
    }))
  })
  if (!all(t3_ok)) rlang::abort("Table III series do not reproduce the saved Sharpe ratios.")

  series <- series %>%
    dplyr::left_join(rf, by = "month_id", relationship = "many-to-one") %>%
    dplyr::mutate(total_return = excess_return + rf)

  ## Per-series statistics: manuscript Sharpe (own sd) and DeMiguel's
  series_stats <- series %>%
    dplyr::group_by(series_id, table, scope, panel, portfolio, signal, n_factors) %>%
    dplyr::summarise(mean_ann = 12 * mean(excess_return), sd_ann = sqrt(12) * stats::sd(excess_return),
                     sharpe = sqrt(12) * mean(excess_return) / stats::sd(excess_return),
                     sharpe_dmu = dmu_sr(excess_return, gross_return),
                     cer_ann = 12 * (mean(excess_return) - gamma / 2 * stats::var(excess_return)),
                     .groups = "drop")

# Contrasts and families-----------------------------------------------------------

  contrasts <- dplyr::bind_rows(
    tidyr::expand_grid(scope = c("aggregate_8", "market"), panel = mvo_panels, comparator = signals_t1) %>%
      dplyr::mutate(table = "I", portfolio = "mvo",
                    strategy = paste("t1", scope, panel, strategy_signal, sep = "_"),
                    benchmark = paste("t1", scope, panel, comparator, sep = "_"),
                    primary = FALSE, family = "T1_mvo"),
    tidyr::expand_grid(scope = c("cmv", "fcmv"), panel = t3_panels, portfolio = t3_columns,
                       comparator = signals_t3) %>%
      dplyr::mutate(table = "III",
                    strategy = paste("t3", scope, panel, portfolio, strategy_signal, sep = "_"),
                    benchmark = paste("t3", scope, panel, portfolio, comparator, sep = "_"),
                    primary = scope == "cmv" & panel == "D" & portfolio == "cmv_tc" & comparator == "lagvar",
                    family = dplyr::case_when(primary ~ "T3_primary", scope == "cmv" ~ "T3_cmv",
                                              TRUE ~ "EXT_fcmv"))
  ) %>%
    dplyr::mutate(comparison_id = paste(strategy, "vs", comparator, sep = "_")) %>%
    dplyr::select(comparison_id, family, strategy, benchmark, table, scope, panel, portfolio, comparator, primary)
  if (sum(contrasts$primary) != 1L || sum(contrasts$family == "T3_primary") != 1L ||
      anyDuplicated(contrasts$comparison_id)) {
    rlang::abort("Exactly one prespecified primary contrast and unique comparison ids are required.")
  }
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
    ## As report_portfolio_inference(): Holm and BH within (block length, family)
    dplyr::group_by(block_length, family) %>%
    dplyr::mutate(p_holm = stats::p.adjust(p_value, "holm"), p_bh = stats::p.adjust(p_value, "BH")) %>%
    dplyr::ungroup()

  ## DeMiguel's own uncentered procedure (robustness), Sharpe contrasts only
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
    dplyr::left_join(dplyr::select(contrasts, comparison_id, table, scope, panel, portfolio, comparator, primary),
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
  }) %>% dplyr::left_join(dplyr::select(contrasts, comparison_id, table, scope, panel, portfolio, comparator, primary),
                          by = "comparison_id", relationship = "many-to-one")

# Per-factor effects: DESCRIPTIVE only (as factor_effects / theme_effects)-------

  ## Includes the standalone managed factor (SAg, SAn): DeMiguel's constant c
  ## cancels from one factor's gross and net Sharpe ratios, so these are not
  ## affected by its full-sample estimation. No p-values.
  factor_stats <- t1_factor %>% dplyr::group_by(factor, panel, signal) %>%
    dplyr::summarise(sharpe = sqrt(12) * mean(excess_return) / stats::sd(excess_return),
                     cer_ann = 12 * (mean(excess_return) - gamma / 2 * stats::var(excess_return)),
                     .groups = "drop")
  factor_effects <- factor_stats %>% dplyr::filter(signal == strategy_signal) %>%
    dplyr::inner_join(factor_stats %>% dplyr::filter(signal != strategy_signal) %>%
                        dplyr::select(factor, panel, comparator = signal, benchmark_sharpe = sharpe,
                                      benchmark_cer = cer_ann),
                      by = c("factor", "panel"), relationship = "one-to-many") %>%
    dplyr::transmute(factor, panel, comparator, ensemble_sharpe = sharpe, benchmark_sharpe,
                     delta_sharpe = sharpe - benchmark_sharpe, delta_cer_ann = cer_ann - benchmark_cer)
  factor_summary <- factor_effects %>% dplyr::filter(factor %in% nonmarket) %>%
    dplyr::group_by(panel, comparator) %>%
    dplyr::summarise(n_factors = dplyr::n(),
                     dplyr::across(c(delta_sharpe, delta_cer_ann), list(
                       mean = mean, median = stats::median, sd = stats::sd,
                       q25 = ~ stats::quantile(.x, .25), q75 = ~ stats::quantile(.x, .75),
                       fraction_positive = ~ mean(.x > 0))), .groups = "drop")

# Report------------------------------------------------------------------------

  primary <- bootstrap %>% dplyr::filter(primary, block_length == 5)
  cat("\nPRIMARY: Table III Panel D, CMV column (6), ensemble vs lagvar (block 5):\n")
  print(as.data.frame(dplyr::select(primary, metric, estimate, ci_lower, ci_upper, p_value, p_holm, p_value_dmu,
                                    strategy_sharpe_dmu, benchmark_sharpe_dmu)), digits = 4, row.names = FALSE)
  cat("\nPrimary Sharpe under block lengths 3 and 10 (sensitivity):\n")
  print(as.data.frame(bootstrap %>% dplyr::filter(primary, metric == "sharpe") %>%
                        dplyr::select(block_length, estimate, p_value)), digits = 4, row.names = FALSE)
  cat("\nSecondary Sharpe contrasts, block 5:\n")
  print(as.data.frame(bootstrap %>% dplyr::filter(!primary, block_length == 5, metric == "sharpe") %>%
                        dplyr::select(family, scope, panel, portfolio, comparator, strategy_sharpe_dmu,
                                      benchmark_sharpe_dmu, estimate, p_value, p_holm, p_value_dmu) %>%
                        dplyr::mutate(dplyr::across(where(is.numeric), ~ round(.x, 4)))), row.names = FALSE)

# Save--------------------------------------------------------------------------

  save_if_changed(bootstrap, file.path(output_dir, "ml_inference_bootstrap.csv"))
  save_if_changed(spanning, file.path(output_dir, "ml_inference_spanning.csv"))
  save_if_changed(series_stats, file.path(output_dir, "ml_inference_series.csv"))
  save_if_changed(contrasts, file.path(output_dir, "ml_inference_contrasts.csv"))
  save_if_changed(factor_effects, file.path(output_dir, "ml_inference_factor_effects.csv"))
  save_if_changed(factor_summary, file.path(output_dir, "ml_inference_factor_summary.csv"))
  save_if_changed(input_manifest, file.path(output_dir, "ml_inference_inputs.csv"))
  ## Calendars are not stored (10,000 x 527 per block length); they are
  ## reproduced exactly by draw_portfolio_months(eval_months, draws, L, seed).
  save_if_changed(list(series = series, t1_factor = t1_factor, contrasts = contrasts, bootstrap = bootstrap,
                       spanning = spanning, factor_effects = factor_effects, factor_summary = factor_summary,
                       inputs = input_manifest,
                       settings = list(draws = draws, seed = seed, block_lengths = block_lengths,
                                       hac_lags = hac_lags, gamma = gamma, seed_dmu = seed_dmu,
                                       block_length_dmu = block_length_dmu, eval_months = range(eval_months),
                                       strategy_signal = strategy_signal, signals_t1 = signals_t1,
                                       signals_t3 = signals_t3, ensemble_members = ensemble_members,
                                       market = market, nonmarket = nonmarket,
                                       primary = "T3 Panel D CMV column (6), Sharpe, ew vs lagvar, L = 5")),
                  file.path(data_dir, "demiguel_ml_inference_v3.rds"), "rds")
  cat("\nSaved data/demiguel/ml_inference_{bootstrap,spanning,series,contrasts,factor_effects,",
      "factor_summary,inputs}.csv and data/demiguel_ml_inference_v3.rds\n")
