# Out-of-sample economic gains from factor-specific volatility management
#
# This script is intentionally an orchestration layer. The validation,
# return-construction, and portfolio calculations live in portfolio_helpers.R.
# Keeping those responsibilities separate makes it possible to test the core
# calculations without reading every fitted model in models_v3.

  `%>%` <- dplyr::`%>%`

  project_dir <- here::here()

  source(file.path(project_dir, "R", "helpers.R"))
  source(file.path(project_dir, "R", "reporting_helpers.R"))
  source(file.path(project_dir, "R", "portfolio_helpers.R"))

# Configuration
# ------------------------------------------------------------------------------

  ## Read all available V3 models in these samples. No refitting, package
  ## installation or fallback to archived results is performed.
  sample_ids <- c("1986")
  preprocessing <- "range"
  tuning <- "grid"
  ensemble_members <- c("harx", "glmnet", "rf", "xgb", "nn1", "nn2", "nn3")
  normalization <- "full_sample"
  min_history <- 120L
  weight_cap <- 1.5
  cost_per_unit <- .001  # 10 bp per unit of exposure; alternatives .0001/.0014
  initial_weight <- 0    # Charge entry from cash; no forced terminal liquidation
  cer_gamma <- 5          # Evaluation metric only; no MVO strategy

  data_dir <- file.path(project_dir, "data")

  ## RF is an observed one-month holding-period return, not TB3MS's annualized
  ## discount yield. Use this saved snapshot; update its vintage deliberately.
  ## https://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/F-F_Research_Data_Factors_CSV.zip
  rf_path <- file.path(data_dir, "F-F_Research_Data_Factors_CSV.zip")
  rf <- read_french_rf(rf_path)

# Return inputs
# ------------------------------------------------------------------------------

  ## Match load_data.R's identifiers, units and already oriented JKP returns.
  ## Never multiply by direction or subtract RF from a zero-cost factor again.
  jkp_path <- file.path(data_dir, "[usa]_[all_factors]_[daily]_[vw_cap].csv")
  factor_daily <- data.table::fread(
    jkp_path,
    select = c("location", "name", "date", "ret"),
    showProgress = FALSE
  ) %>%
    tibble::as_tibble() %>%
    dplyr::filter(location == "usa") %>%
    dplyr::transmute(name, date = as.Date(date), ret)

  crsp <- utils::read.csv(file.path(data_dir, "crsp.csv")) %>%
    tibble::as_tibble() %>%
    dplyr::transmute(
      date = as.Date(caldt, format = "%m/%d/%y"),
      ret = vwretd
    ) %>%
    ## This frozen source ends in 2024 and begins in 1926. Retain its existing
    ## century convention; do not reuse this parser for a future-vintage file.
    dplyr::mutate(
      date = dplyr::if_else(
        date >= as.Date("2025-01-01"),
        lubridate::add_with_rollback(date, lubridate::years(-100)),
        date
      )
    )
  ## French RF starts July 1926. Earlier RV remains available for calibration;
  ## no portfolio returns are claimed before the return inputs begin.
  factor_returns <- prepare_factor_returns(
    dplyr::filter(factor_daily, date >= min(rf$month_id)),
    dplyr::filter(crsp, date >= min(rf$month_id)),
    rf
  )

  target_panel <- readRDS(file.path(data_dir, "target_panel_v3.rds"))
  target_spx <- readRDS(file.path(data_dir, "target_spx_v3.rds")) %>%
    dplyr::mutate(name = "spx_vw", .before = 1)

# Audit saved forecasts and run each factor independently
# ------------------------------------------------------------------------------

  for (sample_id in sample_ids) {
    model_dir <- file.path(project_dir, "models_v3", sample_id)
    paths <- list.files(
      model_dir,
      pattern = paste0("_", preprocessing, "_(grid|none)_", sample_id, "\\.rds$"),
      full.names = TRUE
    )

    if (!length(paths)) {
      rlang::abort(paste("No V3 models found for", sample_id))
    }

    model_map <- tibble::tibble(
      path = paths,
      model = sub("_.*$", "", basename(paths)),
      universe = sub("^[^_]+_([^_]+)_.*$", "\\1", basename(paths))
    )

    if (anyDuplicated(model_map[c("model", "universe")]) ||
        any(!model_map$universe %in% c("panel", "spx"))) {
      rlang::abort("Ambiguous V3 model filenames.")
    }

    for (universe in unique(model_map$universe)) {
      selected <- dplyr::filter(model_map, .data$universe == .env$universe)

      model_results <- purrr::map(selected$path, function(path) {
        ## Reuse the current audit: hashes, transform, keys, dates and reproduced
        ## OOS errors/metrics. Final in-sample predictions are never substituted.
        result <- read_model_result(path, data_dir)
        if (result$backtest_meta$provenance$universe !=
            paste(universe, sample_id, sep = "_")) {
          rlang::abort("Filename and model provenance disagree.")
        }

        result
      }) %>%
        stats::setNames(selected$model)

      ensemble_path <- file.path(
        model_dir,
        "ensembles",
        paste0("ew_", universe, "_", preprocessing, "_", tuning, "_", sample_id, ".rds")
      )
      model_results$ew <- load_or_build_ew_ensemble(
        model_results,
        ensemble_path,
        ensemble_members
      )

      forecasts <- purrr::map(model_results, function(result) {
        oos <- result$oos_outputs
        if (universe == "spx") {
          oos <- dplyr::mutate(oos, name = "spx_vw", .before = 1)
        }

        oos %>%
          dplyr::arrange(name, month_id)
      })

      ## Incomplete fits must not silently shorten model comparisons.
      reference <- dplyr::select(forecasts[[1]], name, month_id)
      same_coverage <- purrr::map_lgl(
        forecasts,
        ~ identical(dplyr::select(.x, name, month_id), reference)
      )

      if (!all(same_coverage)) {
        rlang::abort(paste("Models have different OOS coverage:", sample_id, universe))
      }

      target <- if (universe == "spx") target_spx else target_panel

      portfolios <- purrr::imap(forecasts, function(forecast, model) {
        message("Portfolio backtest: ", sample_id, " / ", universe, " / ", model)

        run_oos_ports(forecast, factor_returns, target,
          normalization = normalization, min_history = min_history,
          weight_cap = weight_cap, cost_per_unit = cost_per_unit,
          initial_weight = initial_weight, cer_gamma = cer_gamma)
      })

      summary <- purrr::imap_dfr(
        portfolios,
        ~ dplyr::mutate(.x$strategy_summary, model = .y, .before = 1)
      )
      coverage <- purrr::imap_dfr(
        portfolios,
        ~ dplyr::mutate(.x$coverage, model = .y, .before = 1)
      )

      if (any(coverage$n_warmup > 0)) {
        message("Calibration warm-up is recorded in coverage.csv.")
      }

      ## Fixed calendar blocks reveal temporal variation without choosing a
      ## favorable subsample or interpreting spanning alpha as a tradable gain.
      stability <- purrr::imap_dfr(portfolios, function(result, model) {
        result$w_compiled_df %>%
          dplyr::mutate(
            period = paste0(
              5 * (lubridate::year(month_id) %/% 5), "-",
              5 * (lubridate::year(month_id) %/% 5) + 4
            )
          ) %>%
          dplyr::group_by(name, method, period) %>%
          dplyr::summarise(
            n = dplyr::n(),
            mean_exc_net = mean(r_exc_fwd1_net_capped),
            sd_exc_net = stats::sd(r_exc_fwd1_net_capped),
            sharpe_net = dplyr::if_else(sd_exc_net > 0,
              sqrt(12) * mean_exc_net / sd_exc_net, NA_real_),
            .groups = "drop"
          ) %>%
          dplyr::mutate(model = model, .before = 1)
      })
      output_dir <- file.path(
        project_dir, "ports", "v3", sample_id, universe,
        paste0(normalization, "_", format(cost_per_unit * 10000, trim = TRUE), "bps")
      )

      dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

      saveRDS(portfolios, file.path(output_dir, "all_ports.rds"))
      utils::write.csv(summary, file.path(output_dir, "strategy_summary.csv"), row.names = FALSE)
      utils::write.csv(coverage, file.path(output_dir, "coverage.csv"), row.names = FALSE)
      utils::write.csv(stability, file.path(output_dir, "calendar_blocks.csv"), row.names = FALSE)
      inputs <- c(
        selected$path,
        ensemble_path,
        rf_path,
        jkp_path,
        file.path(data_dir, "crsp.csv"),
        file.path(
          data_dir,
          if (universe == "spx") "target_spx_v3.rds" else "target_panel_v3.rds"
        ),
        file.path(
          project_dir, "R",
          c("oos_ports.R", "portfolio_helpers.R", "reporting_helpers.R", "helpers.R")
        )
      )

      write_port_run_provenance(file.path(project_dir, "R", "oos_ports.R"),
        list(sample_ids = sample_id, universe = universe, preprocessing = preprocessing,
          tuning = tuning, ensemble_members = ensemble_members,
          normalization = normalization, min_history = min_history,
          weight_cap = weight_cap, cost_per_unit = cost_per_unit,
          initial_weight = initial_weight, cer_gamma = cer_gamma), output_dir, inputs)

      writeLines(capture.output(utils::sessionInfo()), file.path(output_dir, "session_info.txt"))
      message("Saved ", nrow(summary), " strategy summaries to ", output_dir)

    }
  }

# Next-stage Cederburg and multifactor allocations read these saved results in
# R/allocation_ports.R. They can be rerun without rebuilding individual MM paths.
