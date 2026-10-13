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

  `%>%` <- dplyr::`%>%`

  for (helper in c("helpers", "portfolio_helpers", "allocation_helpers", "multifactor_helpers",
                   "portfolio_inference_helpers", "demiguel_cost_helpers",
                   "demiguel_replication_helpers", "demiguel_table3_helpers")) {
    source(file.path(here::here(), "R", paste0(helper, ".R")))
  }

# Configuration-----------------------------------------------------------------

  data_dir <- file.path(here::here(), "data")
  output_dir <- file.path(data_dir, "demiguel")
  sample_start <- as.Date("1967-02-01")
  sample_end <- as.Date("2020-12-01")
  M <- 120L
  gamma <- 5
  refit <- 12L
  draws <- 10000L
  block_length <- 5
  seed <- 20261011L

# Inputs------------------------------------------------------------------------

  monthly <- readRDS(file.path(data_dir, "demiguel_returns_monthly_v3.rds"))
  daily <- readRDS(file.path(data_dir, "demiguel_returns_daily_v3.rds"))
  stock <- readRDS(file.path(data_dir, "demiguel_stock_costs_v3.rds"))

  months <- seq(sample_start, sample_end, by = "month")
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
  sigma <- prepare_market_sigma(daily %>% dplyr::filter(name == "mkt") %>% dplyr::select(date, ret)) %>%
    dplyr::filter(month_id %in% months) %>%
    dplyr::arrange(month_id)
  if (!identical(sigma$month_id, months)) rlang::abort("Market sigma must cover every sample month.")

# Table III---------------------------------------------------------------------

  result <- run_table3(as.matrix(returns[, stock$factors]), months, sigma$sigma, stock,
                       M = M, gamma = gamma, refit = refit, draws = draws,
                       block_length = block_length, seed = seed)
  comparison <- compare_table3(result$table)

  cat("\nTable III (ours):\n")
  print(as.data.frame(result$table %>% dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ~ round(.x, 3)))),
        row.names = FALSE)
  cat("\nDifference from the paper:\n")
  print(as.data.frame(tidyr::pivot_wider(dplyr::select(comparison, panel, column, statistic, difference),
                                         names_from = statistic, values_from = difference) %>%
                        dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ~ round(.x, 3)))),
        row.names = FALSE)

# Save--------------------------------------------------------------------------

  save_if_changed(result$table, file.path(output_dir, "table3.csv"))
  save_if_changed(comparison, file.path(output_dir, "table3_vs_paper.csv"))
  save_if_changed(result, file.path(data_dir, "demiguel_table3_v3.rds"), "rds")
  cat("\nSaved data/demiguel/table3.csv, table3_vs_paper.csv and data/demiguel_table3_v3.rds\n")
