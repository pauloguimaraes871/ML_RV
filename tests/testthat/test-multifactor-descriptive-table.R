base::source(base::file.path(here::here(), "R", "reporting_helpers.R"))
base::source(base::file.path(here::here(), "R", "report_portfolio_inference.R"))

testthat::test_that("multifactor economic table matches completed net-return summaries", {
  root <- here::here()
  saved <- base::file.path(root, "ports", "v3", "1986", "allocations",
    "expanding_vol_10bps_60m_alloc_60_60_60m", "multifactor_ew_cmv_panel_ew.rds")
  cached <- base::file.path(root, "latex_paper", "results_v3", "1986", "data",
    "portfolio_inference_statistics.csv")
  testthat::skip_if_not(base::file.exists(saved) && base::file.exists(cached),
    "Completed multifactor portfolios and their prior report are not installed.")

  inputs <- read_multifactor_descriptive_inputs(root)
  testthat::expect_equal(base::nrow(inputs), 2L * 8L * 2L * 162L)
  testthat::expect_equal(base::anyDuplicated(inputs[c("family", "model", "version", "month_id")]), 0L)
  testthat::expect_true(base::all(base::abs(inputs$gross_return - inputs$cost -
    inputs$excess_return) < 1e-12))

  statistics <- summarise_single_factor_implementation(inputs)
  reference <- utils::read.csv(cached) %>%
    dplyr::select(family, model, version, sharpe_ratio, cer_ann)
  comparison <- statistics %>%
    dplyr::left_join(reference, by = c("family", "model", "version"),
      relationship = "one-to-one")
  testthat::expect_equal(base::nrow(comparison), 32L)
  testthat::expect_true(base::all(base::abs(comparison$net_sharpe -
    comparison$sharpe_ratio) < 1e-12))
  testthat::expect_true(base::all(base::abs(comparison$net_cer -
    comparison$cer_ann) < 1e-12))

  output_dir <- base::tempfile("multifactor-table-")
  base::dir.create(base::file.path(output_dir, "main"), recursive = TRUE)
  base::on.exit(base::unlink(output_dir, recursive = TRUE), add = TRUE)
  output <- write_multifactor_descriptive_table(statistics, output_dir, root)
  table_path <- base::file.path(output_dir, "main", output$main)
  testthat::expect_true(base::file.exists(table_path))
  table_text <- base::readLines(table_path)
  testthat::expect_true(base::any(base::grepl("Panel A. Equal-weight allocation",
    table_text, fixed = TRUE)))
  testthat::expect_true(base::any(base::grepl("Panel B. Hierarchical equal-weight allocation",
    table_text, fixed = TRUE)))
  testthat::expect_true(base::any(base::grepl("CMV, lagged market RV",
    table_text, fixed = TRUE)))
})
