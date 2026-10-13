##############################################################
##  DeMiguel et al. (2024) Table III: lagged RV versus ML    ##
##############################################################
#
# Same multifactor portfolios as R/demiguel_table3.R (columns 1, 3, 4, 6;
# Panels A-D; the authors' specification and stock-level costs), with the
# managed sleeves scaled by different volatility signals for the holding month:
#
#   lagvar   the paper's signal: sample variance of the previous month's daily
#            returns (sigma = its square root)
#   lagss    previous month's SUM of squared daily returns (our target, lagged)
#   harx, ew our pooled-panel forecasts of the holding month's realized
#            variance (long sample, models_v3/demiguel/1927), exp(pred) - 1e-8
#
# Two variants (sigma_t = sqrt(signal); its scale does not affect any weight):
#
#   cmv   every factor scaled by MARKET volatility, as in the paper. Sample
#         February 1967 - December 2020, M = 120: the paper's months. The
#         lagvar run must reproduce data/demiguel/table3.csv (checked).
#   fcmv  each factor scaled by its OWN volatility. ROE and IA forecasts start
#         in March 1970, so every signal uses March 1970 - December 2020 with
#         M = 83: evaluation still February 1977 - December 2020. A cmv lagvar
#         run on the same sample is the like-for-like reference.
#
# In the fcmv rows the "cmv_*" columns hold FCMV. About 2 hours (9 runs).

  `%>%` <- dplyr::`%>%`

  for (helper in c("helpers", "classes", "reporting_helpers", "portfolio_helpers",
                   "allocation_helpers", "multifactor_helpers", "portfolio_inference_helpers",
                   "demiguel_cost_helpers", "demiguel_replication_helpers",
                   "demiguel_table3_helpers")) {
    source(file.path(here::here(), "R", paste0(helper, ".R")))
  }

# Configuration-----------------------------------------------------------------

  data_dir <- file.path(here::here(), "data")
  output_dir <- file.path(data_dir, "demiguel")
  model_dir <- file.path(here::here(), "models_v3", "demiguel", "1927")
  ml_models <- c("harx", "ew")
  ensemble_members <- c("harx", "glmnet", "rf", "xgb", "nn1", "nn2", "nn3")

  paper_start <- as.Date("1967-02-01")
  fcmv_start <- as.Date("1970-03-01")
  sample_end <- as.Date("2020-12-01")
  evaluation_start <- as.Date("1977-02-01")
  gamma <- 5
  refit <- 12L
  draws <- 10000L
  block_length <- 5
  seed <- 20261011L

# Inputs------------------------------------------------------------------------

  monthly <- readRDS(file.path(data_dir, "demiguel_returns_monthly_v3.rds"))
  daily <- readRDS(file.path(data_dir, "demiguel_returns_daily_v3.rds"))
  target <- readRDS(file.path(data_dir, "target_demiguel_v3.rds"))
  stock <- readRDS(file.path(data_dir, "demiguel_stock_costs_v3.rds"))
  factors <- stock$factors

  ## Saved forecasts, audited as in R/demiguel_table1_ml.R
  model_files <- list.files(model_dir, pattern = "_demiguel_range_(grid|none)_1927\\.rds$",
                            full.names = TRUE)
  model_results <- purrr::map(model_files, read_model_result, data_dir = data_dir) %>%
    stats::setNames(sub("_.*$", "", basename(model_files)))
  if (!all(ensemble_members %in% names(model_results))) {
    rlang::abort("Missing saved DeMiguel models for the long sample.")
  }
  model_results$ew <- load_or_build_ew_ensemble(
    model_results, file.path(model_dir, "ensembles", "ew_demiguel_range_grid_1927.rds"),
    ensemble_members)

# Signals, each stamped to the month whose return it scales----------------------

  ml_signal <- function(result) {
    s <- result$oos_outputs %>%
      dplyr::transmute(name, month_id, variance = exp(pred) - 1e-8)
    if (any(!is.finite(s$variance) | s$variance <= 0)) {
      rlang::abort("Back-transformed forecasts must be finite and positive.")
    }
    s
  }
  signals <- c(
    list(
      lagvar = prepare_factor_variance(daily),
      lagss = target %>% dplyr::transmute(
        name, month_id = lubridate::add_with_rollback(month_id, lubridate::period(month = 1)),
        variance = rv_month)
    ),
    purrr::map(model_results[ml_models], ml_signal)
  )
  purrr::walk(signals, validate_port_data, keys = c("name", "month_id"),
              numeric_cols = "variance", consecutive = FALSE)

  ## lagvar's market sigma is the sigma of R/demiguel_table3.R (sqrt(var) and
  ## sd() differ only by floating-point rounding, ~1e-18)
  market_sigma <- prepare_market_sigma(daily %>% dplyr::filter(name == "mkt") %>% dplyr::select(date, ret))
  check <- signals$lagvar %>% dplyr::filter(name == "mkt") %>%
    dplyr::inner_join(market_sigma, by = "month_id")
  if (nrow(check) != nrow(market_sigma) || !isTRUE(all.equal(sqrt(check$variance), check$sigma, tolerance = 1e-12))) {
    rlang::abort("lagvar market sigma must equal prepare_market_sigma().")
  }

  ## T x length(keys) sigma matrix for the given months; aborts on any gap
  sigma_matrix <- function(signal, months, keys) {
    wide <- signal %>%
      dplyr::filter(month_id %in% months, name %in% keys) %>%
      dplyr::transmute(name, month_id, sigma = sqrt(variance)) %>%
      tidyr::pivot_wider(names_from = name, values_from = sigma) %>%
      dplyr::arrange(month_id)
    if (!identical(wide$month_id, months) || !all(keys %in% names(wide)) ||
        anyNA(wide[, keys])) {
      rlang::abort("A volatility signal does not cover every sample month for the required factors.")
    }
    as.matrix(wide[, keys])
  }

  returns_matrix <- function(months) {
    wide <- monthly %>%
      dplyr::filter(month_id %in% months) %>%
      dplyr::select(month_id, name, factor_exc) %>%
      tidyr::pivot_wider(names_from = name, values_from = factor_exc) %>%
      dplyr::arrange(month_id)
    if (!identical(wide$month_id, months) || anyNA(wide[, factors])) {
      rlang::abort("Monthly factor returns must cover every sample month for all nine factors.")
    }
    as.matrix(wide[, factors])
  }

# Runs--------------------------------------------------------------------------

  runs <- dplyr::bind_rows(
    tibble::tibble(variant = "cmv", signal = names(signals), start = paper_start),
    tibble::tibble(variant = "cmv", signal = "lagvar", start = fcmv_start),
    tibble::tibble(variant = "fcmv", signal = names(signals), start = fcmv_start)
  ) %>%
    dplyr::mutate(M = purrr::map_int(start, ~ length(seq(.x, evaluation_start, by = "month")) - 1L))
  if (any(runs$M[runs$start == paper_start] != 120L) || any(runs$M[runs$start == fcmv_start] != 83L)) {
    rlang::abort("First estimation windows must be 120 (1967) and 83 (1970) months.")
  }

  results <- purrr::pmap(runs, function(variant, signal, start, M) {
    message("Table III: ", variant, ", ", signal, ", from ", format(start))
    months <- seq(start, sample_end, by = "month")
    sig <- if (variant == "cmv") {
      sigma_matrix(signals[[signal]], months, "mkt")[, "mkt"]
    } else {
      sigma_matrix(signals[[signal]], months, factors)
    }
    run_table3(returns_matrix(months), months, sig, stock, M = M, gamma = gamma, refit = refit,
               draws = draws, block_length = block_length, seed = seed)
  })

  table <- purrr::map2_dfr(results, seq_len(nrow(runs)), function(r, i) {
    dplyr::bind_cols(runs[i, ], r$table)
  })

# Checks------------------------------------------------------------------------

  ## Every run evaluates February 1977 - December 2020
  purrr::walk(results, function(r) {
    if (min(r$returns$months) != evaluation_start || max(r$returns$months) != sample_end) {
      rlang::abort("Every run must evaluate February 1977 - December 2020.")
    }
  })

  ## UMV does not depend on the signal: identical within each sample
  umv <- table %>% dplyr::filter(startsWith(column, "umv")) %>%
    dplyr::group_by(start, panel, column) %>%
    dplyr::summarise(ok = dplyr::n_distinct(sr) == 1L && dplyr::n_distinct(mean) == 1L, .groups = "drop")
  if (!all(umv$ok)) rlang::abort("UMV columns must not depend on the volatility signal.")

  ## The cmv lagvar run on the paper sample reproduces R/demiguel_table3.R
  committed <- utils::read.csv(file.path(output_dir, "table3.csv"))
  ours <- table %>% dplyr::filter(variant == "cmv", signal == "lagvar", start == paper_start) %>%
    dplyr::select(dplyr::all_of(names(committed)))
  stats_cols <- setdiff(names(committed), c("panel", "column"))
  if (!identical(ours$panel, committed$panel) || !identical(ours$column, committed$column) ||
      !isTRUE(all.equal(as.matrix(ours[, stats_cols]), as.matrix(committed[, stats_cols]),
                        tolerance = 1e-10, check.attributes = FALSE))) {
    rlang::abort("The cmv lagvar run must reproduce data/demiguel/table3.csv.")
  }

  cat("\nTable III by signal, Panel D (trading diversification):\n")
  print(as.data.frame(table %>% dplyr::filter(panel == "D") %>%
                        dplyr::select(variant, signal, start, column, mean, sd, sr, p_value, t_alpha, tc) %>%
                        dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ~ round(.x, 3)))),
        row.names = FALSE)

# Save--------------------------------------------------------------------------

  save_if_changed(table, file.path(output_dir, "table3_ml.csv"))
  save_if_changed(list(table = table, runs = runs, results = results,
                       settings = list(gamma = gamma, refit = refit, draws = draws,
                                       block_length = block_length, seed = seed,
                                       models = ml_models, ensemble_members = ensemble_members,
                                       sources = purrr::map_dfr(model_results[setdiff(names(model_results), "ew")],
                                                                "source"))),
                  file.path(data_dir, "demiguel_table3_ml_v3.rds"), "rds")
  cat("\nSaved data/demiguel/table3_ml.csv and data/demiguel_table3_ml_v3.rds\n")
