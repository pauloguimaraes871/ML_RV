#######################################
##  DeMiguel et al. (2024): Table I  ##
#######################################
#
# Volatility-managed individual-factor portfolios (Panels A and C without
# costs), on the nine provider factors built by R/load_demiguel_data.R. No ML input is
# used: the managed factor scales by the previous month's realized variance,
# exactly as in the paper. The specification mirrors the authors' replication
# code (see R/demiguel_replication_helpers.R and docs/DEMIGUEL_REPLICATION.md).
#
# Panels B, D (costs without trading diversification) and E (with trading
# diversification) price stock-level trades with the authors' stock weights
# and transaction-cost parameters (R/load_demiguel_costs.R must have run).

  `%>%` <- dplyr::`%>%`

  source(file.path(here::here(), "R", "helpers.R"))
  source(file.path(here::here(), "R", "portfolio_helpers.R"))
  source(file.path(here::here(), "R", "allocation_helpers.R"))
  source(file.path(here::here(), "R", "portfolio_inference_helpers.R"))
  source(file.path(here::here(), "R", "demiguel_replication_helpers.R"))

# Configuration-----------------------------------------------------------------

  data_dir <- file.path(here::here(), "data")
  output_dir <- file.path(data_dir, "demiguel")

  ## The paper's sample: daily data from January 1967, so the first month with
  ## a lagged variance is February 1967; the last month is December 2020.
  ## M = 120 makes the evaluation window February 1977 - December 2020.
  sample_start <- as.Date("1967-02-01")
  sample_end <- as.Date("2020-12-01")
  M <- 120L
  gamma <- 5
  draws <- 10000L
  block_length <- 5
  seed <- 20261011L

# Inputs------------------------------------------------------------------------

  monthly <- readRDS(file.path(data_dir, "demiguel_returns_monthly_v3.rds"))
  ## Only the month before the first sample month onwards is needed for the
  ## lagged variances; earlier daily history is dropped to save work. (The
  ## scale constant c is computed on the sample months either way.)
  daily <- readRDS(file.path(data_dir, "demiguel_returns_daily_v3.rds")) %>%
    dplyr::filter(date >= lubridate::add_with_rollback(sample_start, lubridate::period(month = -1)))

# Table I, Panels A and C-------------------------------------------------------

  result <- run_table1_cost_free(monthly, daily, start = sample_start, end = sample_end,
                                 M = M, gamma = gamma, draws = draws,
                                 block_length = block_length, seed = seed)

  ## The evaluation window is fixed by the paper; anything else is a bug.
  if (any(result$table$start != as.Date("1977-02-01")) ||
      any(result$table$end != sample_end) || any(result$table$n != 527L)) {
    rlang::abort("Table I window is not February 1977 - December 2020 (527 months).")
  }

  comparison <- compare_table1(result$table)

  cat("\nTable I, Panels A and C (ours vs paper):\n")
  print(as.data.frame(comparison %>% dplyr::mutate(dplyr::across(c(ours, paper, difference),
                                                                 ~ round(.x, 3)))),
        row.names = FALSE)

# Save--------------------------------------------------------------------------

  save_if_changed(result$table, file.path(output_dir, "table1_panels_ac.csv"))
  save_if_changed(comparison, file.path(output_dir, "table1_panels_ac_vs_paper.csv"))
  save_if_changed(result, file.path(data_dir, "demiguel_table1_ac_v3.rds"), "rds")

  cat("\nSaved data/demiguel/table1_panels_ac.csv, table1_panels_ac_vs_paper.csv",
      "and data/demiguel_table1_ac_v3.rds\n")

# Table I, Panels B, D and E (stock-level costs)--------------------------------

  ## Uses the authors' stock weights and kappas (R/load_demiguel_costs.R) with
  ## our provider returns and our own-variance signal. About one hour: Panel E
  ## re-solves the stock-level cost problem for each of 9 x 527 OOS windows.
  run_cost_panels <- TRUE

  if (run_cost_panels) {
    source(file.path(here::here(), "R", "demiguel_cost_helpers.R"))
    stock <- readRDS(file.path(data_dir, "demiguel_stock_costs_v3.rds"))

    costs <- run_table1_costs(monthly, daily, stock, start = sample_start, end = sample_end,
                              M = M, gamma = gamma, draws = draws,
                              block_length = block_length, seed = seed)
    if (!identical(costs$results[[1]]$oos$month_id, result$returns$month_id[
          result$returns$name == "mkt" & result$returns$panel == "C_out_of_sample"])) {
      rlang::abort("Cost panels and Panels A/C must share the evaluation months.")
    }
    comparison_costs <- compare_table1_costs(costs$table)

    cat("\nTable I, Panels B, D and E (ours vs paper):\n")
    print(as.data.frame(comparison_costs %>% dplyr::mutate(dplyr::across(c(ours, paper, difference),
                                                                         ~ round(.x, 3)))),
          row.names = FALSE)

    save_if_changed(costs$table, file.path(output_dir, "table1_panels_bde.csv"))
    save_if_changed(comparison_costs, file.path(output_dir, "table1_panels_bde_vs_paper.csv"))
    save_if_changed(costs, file.path(data_dir, "demiguel_table1_bde_v3.rds"), "rds")
    cat("\nSaved data/demiguel/table1_panels_bde.csv, table1_panels_bde_vs_paper.csv",
        "and data/demiguel_table1_bde_v3.rds\n")
  }
