# Second-stage allocation using saved single-factor MM portfolios
# ------------------------------------------------------------------------------

  `%>%` <- dplyr::`%>%`
  project_dir <- here::here()
  base::source(base::file.path(project_dir, "R", "helpers.R"))
  base::source(base::file.path(project_dir, "R", "portfolio_helpers.R"))
  base::source(base::file.path(project_dir, "R", "multifactor_helpers.R"))
  base::source(base::file.path(project_dir, "R", "cederburg_helpers.R"))

# Configuration-----------------------------------------------------------------

  ## Add "1927" only after its MM results exist for both estimation universes.
  ## Restrict models_to_run to "ew" for a quick complete specification comparison.
  sample_ids <- c("1986")
  models_to_run <- c("har", "harx", "glmnet", "rf", "xgb", "nn1", "nn2", "nn3", "ew")
  mm_run <- "expanding_rv_10bps"
  min_history <- 120L
  gamma <- 5
  cost_per_unit <- .001
  multifactor_cap <- 1.5
  cederburg_cap <- 5
  allocations <- c("mvo", "ew", "hew")

  cluster_path <- base::file.path(project_dir, "data", "cluster_labels.csv")
  clusters <- utils::read.csv(cluster_path) %>% tibble::as_tibble() %>%
    dplyr::rename(name = characteristic) %>%
    ## The market is an explicit separate cluster, not an arbitrary JKP style.
    dplyr::bind_rows(tibble::tibble(name = "spx_vw", cluster = "Market"))
  if (base::anyDuplicated(clusters$name) || base::anyNA(clusters)) {
    rlang::abort("Invalid cluster membership for hierarchical EW.")
  }

# Read existing MM paths and verify their model/data inputs----------------------

  for (sample_id in sample_ids) {
    source_paths <- stats::setNames(base::file.path(project_dir, "ports", "v3",
      sample_id, c("panel", "spx"), mm_run, "all_ports.rds"), c("panel", "spx"))
    if (!base::all(base::file.exists(source_paths))) {
      rlang::abort(base::paste("Run oos_ports.R for panel and spx first:", sample_id))
    }
    source_manifests <- base::file.path(base::dirname(source_paths), "input_manifest.csv")
    purrr::walk(source_manifests, function(path) {
      manifest <- utils::read.csv(path)
      ## Source-code changes do not rewrite already saved returns. But revised
      ## data or refitted model artifacts must first propagate through MM.
      model_data <- manifest %>% dplyr::filter(base::grepl("/(models_v3|data)/",
        base::gsub("\\", "/", file, fixed = TRUE)))
      if (!base::nrow(model_data) || !base::all(base::file.exists(model_data$file)) ||
          !base::identical(purrr::map_chr(model_data$file, hash_artifact), model_data$sha256)) {
        rlang::abort("Saved MM model/data inputs have changed; regenerate MM results first.")
      }
    })
    saved <- purrr::map(source_paths, base::readRDS)
    purrr::walk(saved, function(models) {
      if (!base::all(models_to_run %in% base::names(models))) {
        rlang::abort("A requested model is missing from saved MM results.")
      }
      purrr::walk(models[models_to_run], function(result) {
        if (!base::isTRUE(result$settings$implementable)) {
          rlang::abort("Allocation experiments cannot use ex-post-normalized MM results.")
        }
      })
    })

    panel_reference <- saved$panel[[models_to_run[1]]]$w_compiled_df %>%
      dplyr::filter(method == "vol_scaled") %>% dplyr::arrange(name, month_id)
    factor_returns <- dplyr::select(panel_reference, name, month_id, factor_exc, rf)
    factor_names <- base::sort(base::unique(factor_returns$name))
    dates <- base::sort(base::unique(factor_returns$month_id))

    output_dir <- base::file.path(project_dir, "ports", "v3", sample_id, "allocations", mm_run)
    base::dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
    summaries <- base::list()

    for (model in models_to_run) {
      panel <- saved$panel[[model]]$w_compiled_df %>%
        dplyr::filter(method == "vol_scaled") %>% dplyr::arrange(name, month_id)
      if (!base::identical(dplyr::select(panel, name, month_id, factor_exc, rf), factor_returns)) {
        rlang::abort("Model portfolios must use identical factor returns and dates.")
      }
      market <- saved$spx[[model]]$w_compiled_df %>%
        dplyr::filter(method == "vol_scaled", name == "spx_vw") %>% dplyr::arrange(month_id)
      if (!base::identical(market$month_id, dates)) {
        rlang::abort("Pooled and standalone market forecasts must cover identical months.")
      }

      ## The own RV prediction was already inverted and checked by MM. No model
      ## is refitted and no alternative EW forecast is constructed here.
      signals <- base::list(
        cmv_panel = panel %>% dplyr::filter(name == "spx_vw") %>%
          dplyr::transmute(month_id, sigma = base::sqrt(signal_rv)),
        cmv_spx = market %>% dplyr::transmute(month_id, sigma = base::sqrt(signal_rv)),
        fcmv = panel %>% dplyr::transmute(name, month_id, sigma = base::sqrt(signal_rv))
      )

      for (specification in base::names(signals)) {
        for (allocation in allocations) {
          id <- base::paste("multifactor", model, specification, allocation, sep = "_")
          base::message("Allocating: ", sample_id, " / ", id)
          result <- run_multifactor_ports(factor_returns, signals[[specification]],
            factor_names, base::min(dates), min_history = min_history, gamma = gamma,
            cost_per_unit = cost_per_unit, exposure_cap = multifactor_cap,
            signal_label = specification, allocation = allocation,
            signal_scope = if (specification == "fcmv") "factor" else "market",
            cluster_map = clusters)
          base::saveRDS(result, base::file.path(output_dir, base::paste0(id, ".rds")))
          summaries[[id]] <- result$strategy_summary %>% dplyr::mutate(
            family = "multifactor", model = model, specification = specification,
            allocation = allocation, .before = 1)
        }
      }

      ## Keep the panel-market and standalone-market results distinguishable
      ## here too. Both variants use the factor's OWN variance estimate.
      for (universe in base::names(saved)) {
        for (normalization in c("training_vol", "saved")) {
          id <- base::paste("cederburg", model, universe, normalization, sep = "_")
          base::message("Allocating: ", sample_id, " / ", id)
          result <- run_cederburg_ports(saved[[universe]][[model]],
            normalization = normalization, min_history = min_history, gamma = gamma,
            exposure_cap = cederburg_cap, cost_per_unit = cost_per_unit)
          base::saveRDS(result, base::file.path(output_dir, base::paste0(id, ".rds")))
          summaries[[id]] <- result$strategy_summary %>% dplyr::mutate(
            family = "cederburg", model = model, specification = universe,
            normalization = normalization, .before = 1)
        }
      }
    }
    utils::write.csv(dplyr::bind_rows(summaries), base::file.path(output_dir, "summary.csv"), row.names = FALSE)
    inputs <- base::c(source_paths, source_manifests, cluster_path,
      base::file.path(project_dir, "R", c("allocation_ports.R", "allocation_helpers.R",
        "multifactor_helpers.R", "cederburg_helpers.R", "portfolio_helpers.R")))
    utils::write.csv(tibble::tibble(file = inputs, sha256 = purrr::map_chr(inputs, hash_artifact)),
      base::file.path(output_dir, "input_manifest.csv"), row.names = FALSE)
    base::saveRDS(base::list(sample_id = sample_id, models = models_to_run, gamma = gamma,
      min_history = min_history, cost_per_unit = cost_per_unit, allocations = allocations,
      multifactor_cap = multifactor_cap, cederburg_cap = cederburg_cap),
      base::file.path(output_dir, "settings.rds"))
    base::writeLines(utils::capture.output(utils::sessionInfo()),
      base::file.path(output_dir, "session_info.txt"))
  }
