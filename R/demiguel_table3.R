#########################################
##  DeMiguel et al. (2024): Table III  ##
#########################################
#
# Unconditional (UMV) and conditional (CMV) mean-variance multifactor
# portfolios of the nine factors, optimized ignoring (columns 1, 3) or
# accounting for (columns 4, 6) stock-level transaction costs; Panels A-D.
# Specification mirrors the authors' code (R/demiguel_table3_helpers.R). Our
# provider returns, our daily market returns for sigma_t, and the authors'
# stock-level weights and costs (R/load_demiguel_costs.R must have run).
# About 15 minutes. CFW (columns 2, 5) is deferred.

  # This is the execution script: it loads saved inputs, calls the helpers,
  # prints results and writes output files. The optimization formulas live
  # in demiguel_table3_helpers.R, especially estimate() and mv_l1_nonneg().
  #
  # Two separate choices govern Table III:
  # 1) Columns: choose weights ignoring costs (_notc) or considering netted
  #    stock costs (_tc), using unmanaged only (umv) or combined sleeves (cmv).
  # 2) Panels: A=in-sample gross; B=out-of-sample gross; C=B less separate
  #    sleeve costs; D=B less costs after stock-level netting across sleeves.
  # C and D use the same weights as B; deducting different costs does not
  # trigger another optimization. _notc portfolios also pay costs in C/D.
  # The cost-aware solver certifies each fit with a duality gap (its dual sign
  # was fixed on 2026-10-25; docs/DEMIGUEL_REPLICATION.md, Section 12).
  `%>%` <- dplyr::`%>%`

  # Load existing portfolio, trading-cost and inference infrastructure before
  # running the experiment. source() evaluates these files in this session;
  # the helpers are reused rather than redefined in the runner.
  for (helper in c("helpers", "portfolio_helpers", "allocation_helpers", "multifactor_helpers",
                   "portfolio_inference_helpers", "demiguel_cost_helpers",
                   "demiguel_replication_helpers", "demiguel_table3_helpers")) {
    source(file.path(here::here(), "R", paste0(helper, ".R")))
  }

# Configuration-----------------------------------------------------------------

  # Input and output locations are relative to the repository found by here.
  # The sample is monthly; dates identify holding months, not daily dates.
  data_dir <- file.path(here::here(), "data")
  output_dir <- file.path(data_dir, "demiguel")
  sample_start <- as.Date("1967-02-01")
  sample_end <- as.Date("2020-12-01")
  # Reserve 120 initial months to estimate the first OOS portfolio. The
  # evaluation period begins at sample month 121. gamma=5 penalizes variance;
  # it is not a volatility target or a constraint that weights sum to one.
  # refit=12 keeps strategy coefficients fixed for 12 holding months, then
  # re-estimates using an expanding return history. Managed factor exposure
  # still changes monthly with the volatility signal between these refits.
  M <- 120L
  gamma <- 5
  refit <- 12L
  # Inference controls: 10,000 resamples, block-length setting 5, and a
  # fixed random seed. These affect p-values, not the weight optimizer.
  draws <- 10000L
  block_length <- 5
  seed <- 20261011L
  # Allocation rule: "mvo" (the paper) unless set before sourcing, e.g.
  # allocation <- "ew"; source("R/demiguel_table3.R"). EW has no published
  # reference and writes its own files; Panel A (in sample) is not reported.
  if (!exists("allocation")) allocation <- "mvo"
  allocation <- match.arg(allocation, c("mvo", "ew"))
  suffix <- if (allocation == "mvo") "" else paste0("_", allocation)

# Inputs------------------------------------------------------------------------

  # These files must already exist from the upstream data preparation.
  # Monthly factor_exc contains factor excess returns; daily ret supplies
  # the market-volatility signal; stock contains underlying holdings/costs.
  # The runner reads local saved inputs rather than downloading paper data.
  monthly <- readRDS(file.path(data_dir, "demiguel_returns_monthly_v3.rds"))
  daily <- readRDS(file.path(data_dir, "demiguel_returns_daily_v3.rds"))
  stock <- readRDS(file.path(data_dir, "demiguel_stock_costs_v3.rds"))

  months <- seq(sample_start, sample_end, by = "month")
  # Select the sample, widen to one return column per factor and sort months.
  # Do not fill missing returns with zero: abort if the complete consecutive
  # calendar is not present or any selected factor return is missing.
  returns <- monthly %>%
    dplyr::filter(month_id %in% months) %>%
    dplyr::select(month_id, name, factor_exc) %>%
    tidyr::pivot_wider(names_from = name, values_from = factor_exc) %>%
    dplyr::arrange(month_id)
  if (!identical(returns$month_id, months) || anyNA(returns)) {
    rlang::abort("Monthly factor returns must cover every sample month for all nine factors.")
  }

  ## sigma_t: sample SD of the previous month's daily market returns (French
  ## Mkt-RF), stamped to the holding month by prepare_market_sigma()
  # Use PREVIOUS-month market daily returns to form the holding-month signal.
  # The helper's date shift prevents using the holding month's realized
  # daily volatility as its own predictor. This runner passes a vector, so
  # all factors share the same market signal (CMV); a factor-specific matrix
  # is supported by run_table3() for the FCMV extension, but is not passed here.
  # This sigma object is a signal table, not the optimizer's covariance.
  sigma <- prepare_market_sigma(daily %>% dplyr::filter(name == "mkt") %>% dplyr::select(date, ret)) %>%
    dplyr::filter(month_id %in% months) %>%
    dplyr::arrange(month_id)
  if (!identical(sigma$month_id, months)) rlang::abort("Market sigma must cover every sample month.")

# Table III---------------------------------------------------------------------

  # Select factor return columns in stock$factors order so returns and stock
  # exposures refer to the same named factors. run_table3() forms unmanaged
  # and managed return columns, estimates four portfolio specifications and
  # evaluates all four panels. In-sample A and OOS B-D cover the same holding
  # dates after M months, but differ in which data estimate the coefficients.
  result <- run_table3(as.matrix(returns[, stock$factors]), months, sigma$sigma, stock,
                       M = M, gamma = gamma, refit = refit, draws = draws,
                       block_length = block_length, seed = seed, allocation = allocation)
  if (allocation == "ew") result$table <- dplyr::filter(result$table, panel %in% c("B", "C", "D"))
  # Compare statistics with hard-coded published references; no parameter
  # is chosen to minimize these differences. Provider inputs may differ
  # from the paper, so matching methodology need not give matching numbers.
  # EW columns have no published counterpart, so the comparison is empty.
  comparison <- compare_table3(result$table)

  # Round only the DISPLAY to three decimal places. The result objects and
  # saved outputs retain unrounded numbers. Decimal return statistics such
  # as .415 represent 41.5%, while Sharpe ratios and t-statistics are ratios.
  cat("\nTable III (ours, ", allocation, "):\n", sep = "")
  print(as.data.frame(result$table %>% dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ~ round(.x, 3)))),
        row.names = FALSE)
  if (allocation == "mvo") cat("\nDifference from the paper:\n")
  if (allocation == "mvo")
  print(as.data.frame(tidyr::pivot_wider(dplyr::select(comparison, panel, column, statistic, difference),
                                         names_from = statistic, values_from = difference) %>%
                        dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ~ round(.x, 3)))),
        row.names = FALSE)

# Save--------------------------------------------------------------------------

  # Persist three views: the compact panel statistics, published-value
  # differences, and the full result (monthly returns/costs, weights/settings).
  # Unlike the computational helpers, executing this runner writes files.
  # save_if_changed avoids rewriting unchanged output content.
  save_if_changed(result$table, file.path(output_dir, paste0("table3", suffix, ".csv")))
  if (allocation == "mvo") save_if_changed(comparison, file.path(output_dir, "table3_vs_paper.csv"))
  save_if_changed(result, file.path(data_dir, paste0("demiguel_table3", suffix, "_v3.rds")), "rds")
  cat("\nSaved data/demiguel/table3", suffix, ".csv and data/demiguel_table3", suffix, "_v3.rds\n", sep = "")
