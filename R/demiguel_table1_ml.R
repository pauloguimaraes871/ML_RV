###########################################################
##  DeMiguel et al. (2024) Table I: lagged RV versus ML   ##
###########################################################
#
# Same volatility-managed individual-factor portfolios as R/demiguel_replication.R
# (Panels A-E, the authors' specification and stock-level costs), with the
# managed factor scaled by different signals for the holding month:
#
#   lagvar   the paper's signal: sample variance of the previous month's daily
#            returns (the replication itself)
#   lagss    previous month's SUM of squared daily returns: the quantity our
#            models forecast, lagged. Separates "definition" from "forecast".
#   har, harx, ew
#            our pooled-panel forecasts of the holding month's realized
#            variance (long sample, models_v3/demiguel/1927), inverted from
#            log(RV + 1e-8) as in run_oos_ports(); ew is the equal-weight
#            ensemble of the seven non-HAR models, as in R/oos_ports.R
#
# Two tables, every signal on identical months within each factor:
#
#   paper_window  evaluation February 1977 - December 2020 (527 months) for all
#                 nine factors, as in the paper. A factor's sample starts at
#                 the later of February 1967 and its first ML forecast, so ROE
#                 and IA (forecasts from March 1970) have an 83-month first
#                 estimation window instead of 120. Panels A-E.
#   full_oos      each factor's whole ML forecast history through December
#                 2024, first estimation window 120 months. Panels A and C
#                 only: the stock-level cost data end in December 2020.

  `%>%` <- dplyr::`%>%`

  for (helper in c("helpers", "classes", "reporting_helpers", "portfolio_helpers",
                   "allocation_helpers", "portfolio_inference_helpers",
                   "demiguel_cost_helpers", "demiguel_replication_helpers")) {
    source(file.path(here::here(), "R", paste0(helper, ".R")))
  }

# Configuration-----------------------------------------------------------------

  data_dir <- file.path(here::here(), "data")
  output_dir <- file.path(data_dir, "demiguel")
  model_dir <- file.path(here::here(), "models_v3", "demiguel", "1927")
  ml_models <- c("har", "harx", "ew")
  ensemble_members <- c("harx", "glmnet", "rf", "xgb", "nn1", "nn2", "nn3")

  paper_start <- as.Date("1967-02-01")
  paper_evaluation_start <- as.Date("1977-02-01")
  paper_end <- as.Date("2020-12-01")
  M_full <- 120L
  gamma <- 5
  draws <- 10000L
  block_length <- 5
  seed <- 20261011L

  ## Panels B, D, E take about one hour per signal (Panel E re-solves the
  ## stock-level cost problem for every OOS window).
  run_cost_panels <- TRUE

# Inputs------------------------------------------------------------------------

  monthly <- readRDS(file.path(data_dir, "demiguel_returns_monthly_v3.rds"))
  daily <- readRDS(file.path(data_dir, "demiguel_returns_daily_v3.rds"))
  target <- readRDS(file.path(data_dir, "target_demiguel_v3.rds"))

  ## Saved forecasts, audited as in R/oos_ports.R (hashes, targets, metrics)
  model_files <- list.files(model_dir, pattern = "_demiguel_range_(grid|none)_1927\\.rds$",
                            full.names = TRUE)
  model_results <- purrr::map(model_files, read_model_result, data_dir = data_dir) %>%
    stats::setNames(sub("_.*$", "", basename(model_files)))
  if (!all(c("har", ensemble_members) %in% names(model_results))) {
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

  ## All ML signals share one forecast calendar; its first month per
  ## factor fixes both windows.
  coverage <- purrr::map(signals[ml_models], ~ dplyr::select(.x, name, month_id) %>%
                           dplyr::arrange(name, month_id))
  if (!all(purrr::map_lgl(coverage, identical, coverage[[1]]))) {
    rlang::abort("ML forecasts must share one coverage.")
  }
  ml_start <- coverage[[1]] %>% dplyr::group_by(name) %>%
    dplyr::summarise(start = min(month_id), end = max(month_id), .groups = "drop")
  ml_start <- ml_start[match(demiguel_table1_paper$name, ml_start$name), ]
  months_between <- function(a, b) {
    12L * (as.integer(format(b, "%Y")) - as.integer(format(a, "%Y"))) +
      as.integer(format(b, "%m")) - as.integer(format(a, "%m"))
  }

  ## paper_window: same evaluation months as the paper for every factor
  paper_start_k <- stats::setNames(pmax(paper_start, ml_start$start), ml_start$name)
  paper_M_k <- stats::setNames(purrr::map_int(paper_start_k, months_between,
                                              b = paper_evaluation_start), ml_start$name)
  ## full_oos: whole forecast history
  full_start_k <- stats::setNames(ml_start$start, ml_start$name)
  full_end_k <- stats::setNames(ml_start$end, ml_start$name)

  cat("\nPaper-window samples (start, first estimation window):\n")
  print(tibble::tibble(name = names(paper_start_k), start = paper_start_k, M = paper_M_k))
  if (any(paper_M_k < 60L)) rlang::abort("A first estimation window would be shorter than 60 months.")

# Tables------------------------------------------------------------------------

  run_signal <- function(signal_name, start, end, M, costs) {
    message("Table I with signal: ", signal_name)
    args <- list(monthly = monthly, daily = daily, start = start, end = end, M = M, gamma = gamma,
                 draws = draws, block_length = block_length, seed = seed,
                 variance = signals[[signal_name]])
    ac <- do.call(run_table1_cost_free, args)
    out <- ac$table %>% dplyr::mutate(signal = signal_name, .before = 1)
    if (costs) {
      bde <- do.call(run_table1_costs, c(args, list(stock = stock)))
      out <- out %>% dplyr::left_join(dplyr::select(bde$table, -a_is, -b_is), by = "name")
    }
    list(table = out, ac = ac, bde = if (costs) bde else NULL)
  }

  if (run_cost_panels) stock <- readRDS(file.path(data_dir, "demiguel_stock_costs_v3.rds"))

  paper_window <- purrr::map(stats::setNames(names(signals), names(signals)), run_signal,
                             start = paper_start_k, end = paper_end, M = paper_M_k,
                             costs = run_cost_panels)
  full_oos <- purrr::map(stats::setNames(names(signals), names(signals)), run_signal,
                         start = full_start_k, end = full_end_k, M = M_full, costs = FALSE)

  paper_table <- purrr::map_dfr(paper_window, "table")
  full_table <- purrr::map_dfr(full_oos, "table")

  ## Every signal must be evaluated on the same months within each factor
  for (tab in list(paper_table, full_table)) {
    same <- tab %>% dplyr::group_by(name) %>%
      dplyr::summarise(ok = dplyr::n_distinct(start) == 1 && dplyr::n_distinct(end) == 1 &&
                         dplyr::n_distinct(n) == 1, .groups = "drop")
    if (!all(same$ok)) rlang::abort("Signals were evaluated on different months.")
  }
  if (any(paper_table$start != paper_evaluation_start) || any(paper_table$end != paper_end)) {
    rlang::abort("The paper-window table must evaluate February 1977 - December 2020.")
  }

  cat("\nPaper window, Sharpe ratios by signal:\n")
  print(as.data.frame(paper_table %>% dplyr::select(signal, name, dplyr::starts_with("sr_")) %>%
                        dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ~ round(.x, 3)))),
        row.names = FALSE)

# Save--------------------------------------------------------------------------

  save_if_changed(paper_table, file.path(output_dir, "table1_ml_paper_window.csv"))
  save_if_changed(full_table, file.path(output_dir, "table1_ml_full_oos.csv"))
  save_if_changed(list(paper_window = paper_window, full_oos = full_oos,
                       settings = list(paper_start = paper_start_k, paper_M = paper_M_k,
                                       full_start = full_start_k, full_end = full_end_k,
                                       M_full = M_full, gamma = gamma, draws = draws,
                                       block_length = block_length, seed = seed,
                                       models = ml_models, ensemble_members = ensemble_members,
                                       sources = purrr::map_dfr(model_results[setdiff(names(model_results), "ew")],
                                                                "source"))),
                  file.path(data_dir, "demiguel_table1_ml_v3.rds"), "rds")
  cat("\nSaved data/demiguel/table1_ml_paper_window.csv, table1_ml_full_oos.csv",
      "and data/demiguel_table1_ml_v3.rds\n")
