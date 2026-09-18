# Second-stage allocation using saved single-factor MM portfolios
# ------------------------------------------------------------------------------

  `%>%` <- dplyr::`%>%`
  project_dir <- here::here()
  source(file.path(project_dir, "R", "helpers.R"))
  source(file.path(project_dir, "R", "portfolio_helpers.R"))
  source(file.path(project_dir, "R", "multifactor_helpers.R"))
  source(file.path(project_dir, "R", "cederburg_helpers.R"))

# Configuration-----------------------------------------------------------------

  ## Add "1927" only after its MM results exist for both estimation universes.
  ## Restrict models_to_run to "ew" for a quick complete specification comparison.
  sample_ids <- c("1986")
  models_to_run <- c("har", "harx", "glmnet", "rf", "xgb", "nn1", "nn2", "nn3", "ew")
  mm_run <- "full_sample_10bps"
  ## EW and HEW only estimate the trailing volatility normalizer. Five years
  ## provides a stable estimate while preserving a longer OOS evaluation window.
  robust_min_history <- 60L
  ## MVO remains an appendix comparison and retains the paper's ten-year window.
  mvo_min_history <- 120L
  cederburg_min_history <- 120L
  gamma <- 5
  cost_per_unit <- .001
  multifactor_cap <- 1.5
  cederburg_cap <- 5
  ## Robust rules lead the comparison; constrained MVO remains a comparator.
  allocations <- c("ew", "hew", "mvo")
  conditional_exposure_multiple <- 1.2

  cluster_path <- file.path(project_dir, "data", "cluster_labels.csv")
  clusters <- utils::read.csv(cluster_path) %>% tibble::as_tibble() %>%
    dplyr::rename(name = characteristic) %>%
    ## The market is an explicit separate cluster, not an arbitrary JKP style.
    dplyr::bind_rows(tibble::tibble(name = "spx_vw", cluster = "Market"))
  if (anyDuplicated(clusters$name) || anyNA(clusters)) {
    rlang::abort("Invalid cluster membership for hierarchical EW.")
  }

# Read existing MM paths and verify their model/data inputs----------------------

  for (sample_id in sample_ids) {
    source_paths <- stats::setNames(file.path(project_dir, "ports", "v3",
      sample_id, c("panel", "spx"), mm_run, "all_ports.rds"), c("panel", "spx"))
    if (!all(file.exists(source_paths))) {
      rlang::abort(paste("Run oos_ports.R for panel and spx first:", sample_id))
    }
    source_manifests <- file.path(dirname(source_paths), "input_manifest.csv")
    purrr::walk(source_manifests, validate_port_manifest)
    saved <- purrr::map(source_paths, readRDS)
    purrr::walk(saved, function(models) {
      if (!all(models_to_run %in% names(models))) {
        rlang::abort("A requested model is missing from saved MM results.")
      }
    })

    panel_reference <- saved$panel[[models_to_run[1]]]$w_compiled_df %>%
      dplyr::filter(method == "vol_scaled") %>% dplyr::arrange(name, month_id)
    factor_returns <- dplyr::select(panel_reference, name, month_id, factor_exc, rf)
    factor_names <- sort(unique(factor_returns$name))
    dates <- sort(unique(factor_returns$month_id))

    output_dir <- file.path(project_dir, "ports", "v3", sample_id, "allocations", mm_run)
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
    summaries <- list()

    for (model in models_to_run) {
      panel <- saved$panel[[model]]$w_compiled_df %>%
        dplyr::filter(method == "vol_scaled") %>% dplyr::arrange(name, month_id)
      if (!identical(dplyr::select(panel, name, month_id, factor_exc, rf), factor_returns)) {
        rlang::abort("Model portfolios must use identical factor returns and dates.")
      }
      market <- saved$spx[[model]]$w_compiled_df %>%
        dplyr::filter(method == "vol_scaled", name == "spx_vw") %>% dplyr::arrange(month_id)
      if (!identical(market$month_id, dates)) {
        rlang::abort("Pooled and standalone market forecasts must cover identical months.")
      }

      ## The own RV prediction was already inverted and checked by MM. No model
      ## is refitted and no alternative EW forecast is constructed here.
      signals <- list(
        cmv_panel = panel %>% dplyr::filter(name == "spx_vw") %>%
          dplyr::transmute(month_id, sigma = sqrt(signal_rv)),
        cmv_spx = market %>% dplyr::transmute(month_id, sigma = sqrt(signal_rv)),
        fcmv = panel %>% dplyr::transmute(name, month_id, sigma = sqrt(signal_rv)),
        ## Saved lagged market RV avoids a raw-data dependency. This is a
        ## second-moment signal, NOT exactly DeMiguel's demeaned daily SD.
        lagged_market = saved$panel[[model]]$w_compiled_df %>%
          dplyr::filter(method == "rw", name == "spx_vw") %>%
          dplyr::transmute(month_id, sigma = sqrt(signal_rv))
      )

      for (specification in names(signals)) {
        for (allocation in allocations) {
          min_history <- if (allocation == "mvo") mvo_min_history else robust_min_history
          id <- paste("multifactor", model, specification, allocation, sep = "_")
          message("Allocating: ", sample_id, " / ", id)
          result <- run_multifactor_ports(factor_returns, signals[[specification]],
            factor_names, min(dates), min_history = min_history, gamma = gamma,
            cost_per_unit = cost_per_unit, exposure_cap = multifactor_cap,
            signal_label = specification, allocation = allocation,
            signal_scope = if (specification == "fcmv") "factor" else "market",
            cluster_map = clusters,
            conditional_exposure_multiple = conditional_exposure_multiple)
          saveRDS(result, file.path(output_dir, paste0(id, ".rds")))
          summaries[[id]] <- result$strategy_summary %>% dplyr::mutate(
            family = "multifactor", model = model, specification = specification,
            allocation = allocation, .before = 1)
        }
      }

      ## Keep the panel-market and standalone-market results distinguishable
      ## here too. Both variants use the factor's OWN variance estimate.
      for (universe in names(saved)) {
        for (managed_method in c("vol_scaled", "rw")) {
          for (normalization in c("training_vol", "saved")) {
            ## A full-sample MM scale is valid input for signal-based allocations
            ## and for Cederburg's re-estimated c_t. It cannot be reused directly.
            if (normalization == "saved" &&
                !isTRUE(saved[[universe]][[model]]$settings$implementable)) {
              next
            }
            id <- paste("cederburg", model, universe, managed_method, normalization, sep = "_")
            message("Allocating: ", sample_id, " / ", id)
            result <- run_cederburg_ports(saved[[universe]][[model]],
              managed_method = managed_method,
              normalization = normalization, min_history = cederburg_min_history, gamma = gamma,
              exposure_cap = cederburg_cap, cost_per_unit = cost_per_unit)
            saveRDS(result, file.path(output_dir, paste0(id, ".rds")))
            summaries[[id]] <- result$strategy_summary %>% dplyr::mutate(
              family = "cederburg", model = model, specification = universe,
              managed_method = managed_method, normalization = normalization, .before = 1)
          }
        }
      }
    }
    utils::write.csv(dplyr::bind_rows(summaries), file.path(output_dir, "summary.csv"), row.names = FALSE)
    inputs <- c(source_paths, source_manifests, cluster_path,
      file.path(project_dir, "R", c("allocation_ports.R", "allocation_helpers.R",
        "multifactor_helpers.R", "cederburg_helpers.R", "portfolio_helpers.R", "helpers.R")))
    write_port_run_provenance(file.path(project_dir, "R", "allocation_ports.R"),
      list(sample_id = sample_id, models = models_to_run,
      sample_ids = sample_id, models_to_run = models_to_run, mm_run = mm_run, gamma = gamma,
      robust_min_history = robust_min_history, mvo_min_history = mvo_min_history,
      cederburg_min_history = cederburg_min_history, cost_per_unit = cost_per_unit,
      allocations = allocations,
      multifactor_cap = multifactor_cap, cederburg_cap = cederburg_cap,
      conditional_exposure_multiple = conditional_exposure_multiple), output_dir, inputs)
    writeLines(utils::capture.output(utils::sessionInfo()),
      file.path(output_dir, "session_info.txt"))
  }
