#######################################################
##  DeMiguel et al. (2024): stock-level cost inputs  ##
#######################################################
#
# Compacts the stock weights, transaction-cost parameters and stock returns of
# the authors' replication package (data/demiguel/raw/MainDataRep.mat) into
# data/demiguel_stock_costs_v3.rds, checks them against the paper, and records
# hashes in data/manifest_demiguel_costs_v3.csv. The cost arithmetic lives in
# R/demiguel_cost_helpers.R. See docs/DEMIGUEL_REPLICATION.md, Section 10.

  `%>%` <- dplyr::`%>%`

  source(file.path(here::here(), "R", "helpers.R"))
  source(file.path(here::here(), "R", "portfolio_helpers.R"))
  source(file.path(here::here(), "R", "demiguel_cost_helpers.R"))

# Configuration-----------------------------------------------------------------

  data_dir <- file.path(here::here(), "data")
  mat_path <- file.path(data_dir, "demiguel", "raw", "MainDataRep.mat")

  ## Provenance of the package. The .mat is the unmodified member of this zip.
  package <- list(
    source = "Mendeley Data, DOI 10.17632/dmjfztbc4y.3 (CC BY 4.0), downloaded 2026-10-11",
    zip_sha256 = "e2dc5e677004141a25cdce575c853bb2ce415c00cfc3e989a0f9a25680dbb5a7",
    mat_sha256 = "9f3391e6d0beb191851b11e368fa276d6d3a4f4fee34d7c3093ac547a74d30df"
  )

  if (!identical(hash_artifact(mat_path), package$mat_sha256)) {
    rlang::abort("MainDataRep.mat does not match the recorded package hash.")
  }

# Read and compact--------------------------------------------------------------

  stock <- read_demiguel_stock_data(mat_path)

  cat("\nStock-level data:", length(stock$months), "months,",
      format(min(stock$months)), "to", format(max(stock$months)), "|",
      stock$n_stocks, "stock slots | stocks kept per month:",
      paste(range(purrr::map_int(stock$panels, ~ length(.x$id))), collapse = "-"), "\n")

# Checks against the paper------------------------------------------------------

  ## Figure 2B: average monthly cost of each unmanaged factor over the paper's
  ## window (rows M + 1 to T of their code, so trades March 1977 - Dec 2020).
  fig2b <- factor_rebalancing_costs(stock, as.Date("1977-02-01"), as.Date("2020-12-01")) %>%
    dplyr::filter(!is.na(cost)) %>%
    dplyr::group_by(name) %>%
    dplyr::summarise(months = dplyr::n(), cost_bp = 1e4 * mean(cost), .groups = "drop") %>%
    dplyr::left_join(tibble::tibble(
      name = unname(demiguel_package_factors),
      paper_bp = c(1.5, 7.5, 10, 10, 13, 46, 31, 22, 28)  # p. 3870; MKT read off Figure 2B
    ), by = "name") %>%
    dplyr::arrange(match(name, demiguel_package_factors))

  cat("\nFigure 2B, average monthly factor trading cost (bp):\n")
  print(as.data.frame(fig2b), digits = 4, row.names = FALSE)
  if (any(abs(fig2b$cost_bp - fig2b$paper_bp) > 1)) {
    rlang::abort("Factor trading costs differ from the paper by more than 1 bp.")
  }

# Save--------------------------------------------------------------------------

  stock_path <- file.path(data_dir, "demiguel_stock_costs_v3.rds")
  saveRDS(stock, stock_path)
  utils::write.csv(fig2b, file.path(data_dir, "demiguel", "fig2b_factor_costs.csv"), row.names = FALSE)

  manifest <- tibble::tibble(
    role = c("input", "input", "output"),
    artifact = c("ReplicationPackage.zip (Mendeley)", "demiguel/raw/MainDataRep.mat",
                 "demiguel_stock_costs_v3.rds"),
    sha256 = c(package$zip_sha256, package$mat_sha256, hash_artifact(stock_path)),
    notes = c(package$source,
              "unmodified member of the zip; NaN = stock outside CRSP that month",
              "compacted RPS / BidAskSpreads / rRisky / Rchar; NA kept as in the package")
  )
  utils::write.csv(manifest, file.path(data_dir, "manifest_demiguel_costs_v3.csv"), row.names = FALSE)
  cat("\nSaved data/demiguel_stock_costs_v3.rds and data/manifest_demiguel_costs_v3.csv\n")

# Tests-------------------------------------------------------------------------

  testthat::test_file(
    file.path(here::here(), "tests", "testthat", "test-demiguel-costs.R"),
    reporter = "summary",
    stop_on_failure = TRUE
  )
