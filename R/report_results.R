# V3 forecast results for the paper---------------------------------------------

  ## Run from the ML_RV project. Reads fitted results; never estimates a model.
  `%>%` <- dplyr::`%>%`
  project_dir <- here::here()
  source(file.path(project_dir, "R", "helpers.R"))
  source(file.path(project_dir, "R", "reporting_helpers.R"))

# Configuration-----------------------------------------------------------------

  sample_id <- "1986"
  preprocessing <- "range"
  tuning <- "grid"
  base_models <- c("har", "harx", "glmnet", "rf", "xgb", "nn1", "nn2", "nn3")
  ensemble_members <- setdiff(base_models, "har")
  model_order <- c(base_models, "ew")
  model_labels <- c("HAR", "HARX", "ENET", "RF", "XGB", "NN1", "NN2", "NN3", "EW")
  model_map <- tibble::tibble(model = model_order, label = model_labels)
  rolling_months <- 12L
  universe_map <- tibble::tibble(universe = c("panel", "spx"),
    universe_label = c("Pooled factor and market panel", "Market portfolio"))
  paper_dir <- file.path(project_dir, "latex_paper")
  output_dir <- file.path(paper_dir, "results_v3", sample_id)
  for (directory in c("data", "main", "main_appendix", "internet_appendix")) {
    dir.create(file.path(output_dir, directory), recursive = TRUE, showWarnings = FALSE)
  }
  model_dir <- file.path(project_dir, "models_v3", sample_id)
  data_dir <- file.path(project_dir, "data")
  generated <- list(main = character(), main_appendix = character(), internet_appendix = character())
  as_model_label <- function(x) factor(x, levels = model_order, labels = model_labels)
  as_universe_label <- function(x) factor(x, levels = universe_map$universe,
                                          labels = universe_map$universe_label)
  save_data <- function(data, filename) {
    utils::write.csv(data, file.path(output_dir, "data", paste0(filename, ".csv")), row.names = FALSE)
  }
  register_output <- function(section, filename) {
    generated[[section]] <<- c(generated[[section]], file.path(section, filename))
  }
  save_table <- function(data, section, filename, caption, long = FALSE, digits = 4) {
    write_report_table(data, file.path(output_dir, section, paste0(filename, ".tex")),
                       caption, paste0("tab:rv-", filename), long, digits)
    register_output(section, paste0(filename, ".tex"))
  }
  save_panel_table <- function(panels, section, filename, caption, digits = 4,
                               font_size = "footnotesize", tabcolsep = 4,
                               landscape = FALSE, placement = "p") {
    write_report_panel_table(panels, file.path(output_dir, section, paste0(filename, ".tex")),
                             caption, paste0("tab:rv-", filename), digits,
                             font_size, tabcolsep, landscape, placement)
    if (startsWith(filename, "portfolio")) {
      ## Portfolio panels contain several exposure versions. Compact rows keep
      ## the complete comparison inside the manuscript's shorter text block.
      path <- file.path(output_dir, section, paste0(filename, ".tex"))
      writeLines(gsub("{1.1}", "{1.0}", readLines(path), fixed = TRUE), path)
    }
    register_output(section, paste0(filename, ".tex"))
  }
  save_cluster_table <- function(data, section, filename, caption, direction, digits = 4,
                                 benchmark_columns = "HAR", shade_columns = NULL,
                                 landscape = FALSE) {
    write_cluster_metric_table(data, file.path(output_dir, section, paste0(filename, ".tex")),
                               caption, paste0("tab:rv-", filename), direction, digits,
                               benchmark_columns, shade_columns, landscape)
    register_output(section, paste0(filename, ".tex"))
  }
  palette <- stats::setNames(c("#222222", "#888888", "#0072B2", "#009E73", "#E69F00",
                              "#CC79A7", "#56B4E9", "#D55E00", "#6A3D9A"), model_labels)
  paper_theme <- ggplot2::theme_bw(base_size = 10) +
    ggplot2::theme(legend.position = "bottom", panel.grid.minor = ggplot2::element_blank(),
                   plot.title = ggplot2::element_text(face = "bold", size = 12),
                   strip.background = ggplot2::element_rect(fill = "grey95"))
  save_figure <- function(plot, section, filename, caption, width = 9, height = 5) {
    extensions <- c("pdf", "png")
    targets <- stats::setNames(
      file.path(output_dir, section, paste0(filename, ".", extensions)), extensions
    )
    temporary <- stats::setNames(
      purrr::map_chr(extensions, ~ tempfile(pattern = paste0(filename, "-"),
                                            fileext = paste0(".", .x))), extensions
    )
    purrr::iwalk(temporary, ~ ggplot2::ggsave(
      .x, plot = plot, width = width, height = height, dpi = 180,
      bg = "white", limitsize = TRUE
    ))
    unchanged <- file.exists(targets[["png"]]) &&
      identical(hash_artifact(targets[["png"]]), hash_artifact(temporary[["png"]]))
    if (!unchanged) {
      purrr::iwalk(temporary, function(source, extension) {
        if (!file.copy(source, targets[[extension]], overwrite = TRUE)) {
          rlang::abort(paste("Could not replace generated figure:", targets[[extension]]))
        }
      })
    }
    unlink(temporary)
    relative <- paste("results_v3", sample_id, section, paste0(filename, ".pdf"), sep = "/")
    caption_tex <- format_report_caption(caption)
    placement <- if (section == "main_appendix") "htbp" else "p"
    writeLines(c(paste0("\\begin{figure}[", placement, "]"), "\\centering",
      "\\captionsetup{justification=raggedright,labelsep=none,singlelinecheck=false,labelformat=boldlabel}",
      paste0("\\caption[", caption_tex$short, "]{", caption_tex$full, "}"),
      paste0("\\includegraphics[width=\\linewidth,height=0.78\\textheight,keepaspectratio]{", relative, "}"),
      paste0("\\label{fig:rv-", filename, "}"),
      "\\end{figure}"), file.path(output_dir, section, paste0(filename, ".tex")))
    register_output(section, paste0(filename, ".tex"))
  }

# Read and validate every model-------------------------------------------------

  results <- stats::setNames(purrr::map(c("panel", "spx"), function(universe) {
    stats::setNames(purrr::map(base_models, function(model) {
      method <- if (model %in% c("har", "harx")) "none" else tuning
      path <- file.path(model_dir, paste0(paste(model, universe, preprocessing, method,
                                               sample_id, sep = "_"), ".rds"))
      message("Auditing ", basename(path))
      result <- read_model_result(path, data_dir)
      p <- result$backtest_meta$provenance
      if (!identical(p$model_id, model) || !identical(p$universe, paste(universe, sample_id, sep = "_")) ||
          !identical(p$preprocessing, preprocessing)) rlang::abort("Filename and provenance disagree.")
      result
    }), base_models)
  }), c("panel", "spx"))

  audit <- purrr::imap_dfr(results, function(models, universe) {
    purrr::imap_dfr(models, function(result, model) {
      o <- result$oos_outputs
      tibble::tibble(universe = universe, model = model, file = result$source$file,
        sha256 = result$source$sha256, run_signature = result$backtest_meta$provenance$run_signature,
        n = nrow(o), n_targets = if ("name" %in% names(o)) dplyr::n_distinct(o$name) else 1L,
        start = min(o$month_id), end = max(o$month_id), metrics_reproduced = TRUE,
        surrogate_r2 = result$surrogate_r2,
        final_aliases = paste(result$final_aliases, collapse = ", "),
        serialized_nn_members = result$serialized_members)
    })
  })
  save_data(audit, "model_audit")

# Equal-weight ensemble---------------------------------------------------------

  for (universe in names(results)) {
    ensemble_path <- file.path(
      model_dir,
      "ensembles",
      paste0("ew_", universe, "_", preprocessing, "_", tuning, "_", sample_id, ".rds")
    )
    ## Reporting completed backtests must not replace their forecast cache.
    ## Fail on absent/stale members instead of invoking a prediction builder.
    if (!file.exists(ensemble_path)) rlang::abort("The completed EW forecast cache is missing.")
    ensemble <- readRDS(ensemble_path)
    current_sources <- purrr::imap_dfr(results[[universe]][ensemble_members],
      ~ dplyr::mutate(.x$source, model = .y, .before = 1)) %>% dplyr::arrange(model)
    if (!identical(ensemble$backtest_meta$sources, current_sources) ||
        !identical(ensemble$backtest_meta$weights$model, ensemble_members) ||
        !isTRUE(all.equal(ensemble$backtest_meta$weights$weight,
                          rep(1 / length(ensemble_members), length(ensemble_members))))) {
      rlang::abort("The completed EW forecast cache does not match the current model members.")
    }
    keys <- c("month_id", ensemble$backtest_meta$key_col)
    validate_forecasts(ensemble$oos_outputs, keys)
    targets <- dplyr::full_join(
      dplyr::select(results[[universe]]$har$oos_outputs, dplyr::all_of(keys), target),
      dplyr::select(ensemble$oos_outputs, dplyr::all_of(keys), target),
      by = keys, suffix = c("_har", "_ew"), relationship = "one-to-one")
    if (anyNA(targets) || any(targets$target_har != targets$target_ew)) {
      rlang::abort("The completed EW forecast cache has different targets or dates.")
    }
    results[[universe]]$ew <- ensemble
    saveRDS(ensemble, file.path(output_dir, "data", paste0("ew_", universe, "_", sample_id, ".rds")))
  }

# Consolidated and per-target performance---------------------------------------

  # These numbers come directly from eval_metrics, including the original panel
  # membership. Factor-only summaries are separately named and recomputed below.
  consolidated <- purrr::imap_dfr(results, function(models, universe) {
    purrr::imap_dfr(models, ~ dplyr::mutate(.x$eval_metrics, universe = universe, model = .y))
  })
  forecasts <- purrr::imap_dfr(results, function(models, universe) {
    purrr::imap_dfr(models, function(result, model) {
      data <- result$oos_outputs
      if (!"name" %in% names(data)) data$name <- "spx_vw"
      dplyr::mutate(data, universe = universe, model = model)
    })
  })
  validate_forecasts(forecasts, c("universe", "model", "month_id", "name"))
  # Explicit joins ensure the market target means the same thing in both fits.
  market_check <- forecasts %>% dplyr::filter(name == "spx_vw", model == "har") %>%
    dplyr::select(universe, month_id, target) %>%
    tidyr::pivot_wider(names_from = universe, values_from = target)
  if (anyNA(market_check) || any(market_check$panel != market_check$spx)) {
    rlang::abort("Market targets or dates differ between estimation universes.")
  }
  per_target <- summarise_forecasts(forecasts, c("universe", "model", "name"))
  factor_forecasts <- dplyr::filter(forecasts, universe == "panel", name != "spx_vw")
  cluster_map <- utils::read.csv(file.path(data_dir, "cluster_labels.csv"),
                                 stringsAsFactors = FALSE) %>%
    tibble::as_tibble() %>%
    dplyr::rename(name = characteristic)
  if (!identical(names(cluster_map), c("name", "cluster")) ||
      anyNA(cluster_map) || anyDuplicated(cluster_map$name) ||
      !setequal(cluster_map$name, unique(factor_forecasts$name))) {
    rlang::abort("JKP cluster labels must map every factor exactly once.")
  }
  cluster_order <- sort(unique(cluster_map$cluster))
  factor_forecasts <- factor_forecasts %>%
    dplyr::left_join(cluster_map, by = "name", relationship = "many-to-one")
  scope_forecasts <- dplyr::bind_rows(
    dplyr::mutate(forecasts, scope = ifelse(universe == "panel", "Pooled panel", "Market portfolio, separate model")),
    forecasts %>% dplyr::filter(universe == "panel", name == "spx_vw") %>%
      dplyr::mutate(scope = "Market portfolio, pooled model")
  )
  scope_metrics <- summarise_forecasts(scope_forecasts, c("scope", "model"))
  save_data(consolidated, "consolidated_metrics")
  save_data(forecasts, "oos_forecasts")
  save_data(per_target, "per_target_metrics")
  save_data(scope_metrics, "scope_metrics")
  save_data(cluster_map, "jkp_cluster_map")
  unlink(file.path(output_dir, "data", "factor_only_metrics.csv"))

  performance_scopes <- c("Pooled panel", "Market portfolio, pooled model")
  performance_panels <- stats::setNames(purrr::map(performance_scopes, function(current_scope) {
    scope_metrics %>%
      dplyr::filter(scope == current_scope) %>%
      dplyr::mutate(model = as_model_label(model)) %>% dplyr::arrange(model) %>%
      dplyr::select(Model = model, RMSE = rmse, MAE = mae, `R-squared` = rss, Bias = mb)
  }), c("Panel A. Pooled factor and market panel",
        "Panel B. Market portfolio from pooled estimation"))
  save_panel_table(performance_panels, "main", "performance", paste(
    "Out-of-sample predictive performance.",
    "Panel A pools forecast errors across all factor-month and market-month observations before computing each statistic.",
    "Panel B evaluates the market-portfolio forecasts from the same pooled estimation.",
    "The evaluation period runs from July 2006 to December 2024 and the dependent variable is monthly log realized variance.",
    "R-squared is computed relative to the mean in each evaluation sample. Bias is realized minus predicted log variance.",
    "The equal-weighted forecast averages HARX, elastic net, random forest, XGBoost, and the three neural networks."))

  factor_har <- per_target %>% dplyr::filter(universe == "panel", model == "har", name != "spx_vw") %>%
    dplyr::select(name, har_rmse = rmse)
  factor_gains <- per_target %>% dplyr::filter(universe == "panel", name != "spx_vw") %>%
    dplyr::left_join(factor_har, by = "name", relationship = "many-to-one") %>%
    dplyr::left_join(cluster_map, by = "name", relationship = "many-to-one") %>%
    dplyr::mutate(rmse_gain_pct = 100 * (1 - rmse / har_rmse))
  factor_summary <- factor_gains %>% dplyr::group_by(model) %>%
    dplyr::summarise(n_factors = dplyr::n(), beating_har = sum(rmse < har_rmse),
      median_gain_pct = stats::median(rmse_gain_pct), p25 = stats::quantile(rmse_gain_pct, 0.25),
      p75 = stats::quantile(rmse_gain_pct, 0.75), .groups = "drop")
  save_data(factor_gains, "factor_gains")
  save_data(factor_summary, "factor_gain_summary")
  save_table(factor_summary %>% dplyr::mutate(model = as_model_label(model)) %>%
    dplyr::arrange(model) %>% dplyr::transmute(Model = model,
      Factors = as.character(n_factors), `Beating HAR` = as.character(beating_har),
      `Median gain (%)` = median_gain_pct, `P25 (%)` = p25, `P75 (%)` = p75),
    "main", "factor_gain_summary", "Distribution of out-of-sample RMSE gains across the 153 equity factors. Gains are measured relative to HAR, and positive values indicate lower forecast error. The table reports the number of factors for which each model outperforms HAR and the cross-sectional distribution of percentage RMSE gains.", digits = 2)

  cluster_metrics <- summarise_forecasts(factor_forecasts, c("cluster", "model"))
  save_data(cluster_metrics, "cluster_metrics")
  for (metric in c("rmse", "rss")) {
    aggregate <- cluster_metrics %>%
      dplyr::mutate(model = as_model_label(model)) %>%
      dplyr::select(cluster, model, value = dplyr::all_of(metric)) %>%
      tidyr::pivot_wider(names_from = model, values_from = value) %>%
      dplyr::mutate(Factor = "Cluster aggregate", row_type = "aggregate")
    detail <- factor_gains %>%
      dplyr::mutate(model = as_model_label(model)) %>%
      dplyr::select(cluster, Factor = name, model, value = dplyr::all_of(metric)) %>%
      tidyr::pivot_wider(names_from = model, values_from = value) %>%
      dplyr::mutate(row_type = "factor")
    table <- dplyr::bind_rows(aggregate, detail) %>%
      dplyr::mutate(cluster = factor(cluster, levels = cluster_order),
                    row_order = ifelse(row_type == "aggregate", 0L, 1L)) %>%
      dplyr::arrange(cluster, row_order, Factor) %>%
      dplyr::mutate(cluster = as.character(cluster)) %>%
      dplyr::select(cluster, Factor, row_type, dplyr::all_of(model_labels))
    metric_label <- c(rmse = "root mean squared error", rss = "R-squared")[[metric]]
    comparison_note <- if (metric == "rmse") "lower" else "higher"
    save_cluster_table(table, "internet_appendix", paste0("factor_", metric), paste(
      "Out-of-sample", metric_label, "by Jensen, Kelly, and Pedersen factor cluster.",
      "The first row of each panel pools all factor-month forecast errors in the cluster before computing the statistic.",
      "The remaining rows report individual factors.",
      "Light blue cells outperform HAR and bold cells identify the best model in each row.",
      "For this statistic,", comparison_note, "values indicate better predictive performance."),
      if (metric == "rmse") "min" else "max")
  }
  save_table(scope_metrics %>% dplyr::filter(scope == "Market portfolio, separate model") %>%
    dplyr::mutate(model = as_model_label(model)) %>% dplyr::arrange(scope, model) %>%
    dplyr::select(Model = model, RMSE = rmse, MAE = mae, `R-squared` = rss),
    "internet_appendix", "additional_scopes", paste(
      "Out-of-sample predictive performance for the market portfolio estimated separately.",
      "Each forecasting model is estimated only on the market-portfolio time series.",
      "The evaluation period runs from July 2006 to December 2024 and the dependent variable is monthly log realized variance."))

# Sequential predictive comparisons---------------------------------------------

  baselines <- scope_metrics %>% dplyr::filter(model %in% c("har", "harx", "ew")) %>%
    dplyr::select(scope, model, rmse) %>% tidyr::pivot_wider(names_from = model, values_from = rmse)
  comparisons <- scope_metrics %>% dplyr::filter(!model %in% c("har", "harx", "ew")) %>%
    dplyr::left_join(baselines, by = "scope", relationship = "many-to-one") %>%
    dplyr::mutate(har_to_harx = har - harx, harx_to_model = harx - rmse,
      model_to_ew = rmse - ew, total = har - ew,
      extension = ifelse(model == "glmnet", "Shrinkage", "Nonlinear model"))
  if (max(abs(with(comparisons, har_to_harx + harx_to_model + model_to_ew - total))) > 1e-12) {
    rlang::abort("Sequential RMSE differences do not add to the total.")
  }
  save_data(comparisons, "sequential_rmse_comparisons")
  waterfall <- comparisons %>%
    dplyr::filter(scope %in% c("Pooled panel", "Market portfolio, separate model")) %>%
    dplyr::mutate(model_label = as_model_label(model)) %>%
    tidyr::pivot_longer(c(har_to_harx, harx_to_model, model_to_ew),
      names_to = "step", values_to = "increment") %>%
    dplyr::mutate(step = factor(step,
      levels = c("har_to_harx", "harx_to_model", "model_to_ew"),
      labels = c("Exogenous\ncovariates", "Model\nextension", "Equal-weighted\ncombination"))) %>%
    dplyr::arrange(scope, model, step) %>%
    dplyr::group_by(scope, model, model_label) %>%
    dplyr::mutate(end = cumsum(increment), start = dplyr::lag(end, default = 0)) %>%
    dplyr::ungroup()
  plot <- ggplot2::ggplot(waterfall,
      ggplot2::aes(x = step, ymin = pmin(start, end), ymax = pmax(start, end), colour = increment > 0)) +
    ggplot2::geom_hline(yintercept = 0, colour = "grey55") +
    ggplot2::geom_linerange(ggplot2::aes(ymin = start, ymax = end), linewidth = 9) +
    ggplot2::facet_grid(scope ~ model_label) +
    ggplot2::scale_colour_manual(values = c(`TRUE` = "#0072B2", `FALSE` = "#D55E00"), guide = "none") +
    ggplot2::labs(x = NULL, y = "Cumulative RMSE reduction relative to HAR") + paper_theme +
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = 6))
  save_figure(plot, "internet_appendix", "sequential_waterfall",
    "Sequential changes in out-of-sample RMSE relative to HAR. Each column considers one extension of HARX. The bars first add exogenous covariates, then the indicated model extension, and finally the equal-weighted forecast combination. Positive changes represent lower RMSE. These arithmetic comparisons do not identify causal contributions.",
    width = 12, height = 6)
  relative <- scope_metrics %>% dplyr::left_join(dplyr::select(baselines, scope, har),
                                                 by = "scope", relationship = "many-to-one") %>%
    dplyr::mutate(gain = 100 * (1 - rmse / har), model = as_model_label(model))
  main_relative <- relative %>%
    dplyr::filter(scope %in% c("Pooled panel", "Market portfolio, pooled model")) %>%
    dplyr::mutate(scope = factor(scope,
      levels = c("Pooled panel", "Market portfolio, pooled model"),
      labels = c("Pooled factor and market panel", "Market portfolio from pooled estimation")))
  plot <- ggplot2::ggplot(main_relative, ggplot2::aes(model, gain, colour = model)) +
    ggplot2::geom_hline(yintercept = 0, colour = "grey60") + ggplot2::geom_point(size = 2.5) +
    ggplot2::facet_wrap(~ scope, ncol = 2) + ggplot2::scale_colour_manual(values = palette, drop = FALSE) +
    ggplot2::labs(x = NULL, y = "RMSE reduction relative to HAR (%)") + paper_theme +
    ggplot2::theme(legend.position = "none")
  save_figure(plot, "main", "har_relative_gains", paste(
    "Out-of-sample RMSE gains relative to HAR.",
    "The pooled-panel result takes the square root after pooling squared errors across all factor-month and market-month observations and is not an average of target-level RMSE.",
    "The market-portfolio result evaluates that target within the same pooled estimation.",
    "Positive values indicate lower RMSE than HAR."))
  separate_relative <- relative %>%
    dplyr::filter(scope == "Market portfolio, separate model") %>%
    dplyr::mutate(scope = "Market portfolio estimated separately")
  plot <- ggplot2::ggplot(separate_relative, ggplot2::aes(model, gain, colour = model)) +
    ggplot2::geom_hline(yintercept = 0, colour = "grey60") + ggplot2::geom_point(size = 2.5) +
    ggplot2::facet_wrap(~ scope) + ggplot2::scale_colour_manual(values = palette, drop = FALSE) +
    ggplot2::labs(x = NULL, y = "RMSE reduction relative to HAR (%)") + paper_theme +
    ggplot2::theme(legend.position = "none")
  save_figure(plot, "internet_appendix", "har_relative_gains_market_separate", paste(
    "Out-of-sample RMSE gains relative to HAR for the market portfolio estimated separately.",
    "Each model is estimated only on the market-portfolio time series.",
    "Positive values indicate lower RMSE than HAR."))

# Forecast error through time---------------------------------------------------

  monthly <- summarise_forecasts(scope_forecasts, c("scope", "model", "month_id"))
  rolling <- rolling_forecast_rmse(scope_forecasts, c("scope", "model"), rolling_months)
  save_data(monthly, "monthly_metrics")
  save_data(rolling, "rolling_rmse")
  plot <- rolling %>% dplyr::filter(is.finite(rmse)) %>% dplyr::mutate(model = as_model_label(model)) %>%
    ggplot2::ggplot(ggplot2::aes(month_id, rmse, colour = model)) + ggplot2::geom_line(linewidth = 0.55) +
    ggplot2::facet_wrap(~ scope, ncol = 2) + ggplot2::scale_colour_manual(values = palette, drop = FALSE) +
    ggplot2::labs(x = NULL, y = "Trailing 12-month RMSE (log RV)", colour = NULL) + paper_theme
  save_figure(plot, "internet_appendix", "rolling_rmse", "Trailing 12-month out-of-sample RMSE. For the pooled-panel result, squared forecast errors are pooled across all available target-month observations in each window before taking the square root. The market-portfolio results evaluate a single monthly series. The figure omits incomplete initial windows.", height = 6)
  factor_rolling <- rolling_forecast_rmse(factor_forecasts, c("model", "name"), rolling_months)
  save_data(factor_rolling, "factor_rolling_rmse")
  cluster_rolling <- rolling_forecast_rmse(
    dplyr::filter(factor_forecasts, model == "ew"), "cluster", rolling_months)
  cluster_dispersion <- factor_rolling %>%
    dplyr::filter(model == "ew", is.finite(rmse)) %>%
    dplyr::left_join(cluster_map, by = "name", relationship = "many-to-one") %>%
    dplyr::group_by(cluster, month_id) %>%
    dplyr::summarise(p25 = stats::quantile(rmse, 0.25), p75 = stats::quantile(rmse, 0.75), .groups = "drop")
  cluster_rolling <- cluster_rolling %>%
    dplyr::left_join(cluster_dispersion, by = c("cluster", "month_id"), relationship = "one-to-one") %>%
    dplyr::mutate(cluster = factor(cluster, levels = cluster_order))
  save_data(cluster_rolling, "cluster_rolling_rmse")
  plot <- cluster_rolling %>% dplyr::filter(is.finite(rmse)) %>%
    ggplot2::ggplot(ggplot2::aes(month_id, rmse)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = p25, ymax = p75), fill = "#56B4E9", alpha = 0.25) +
    ggplot2::geom_line(colour = "#0072B2", linewidth = 0.55) +
    ggplot2::facet_wrap(~ cluster, ncol = 4, scales = "free_y") +
    ggplot2::scale_x_date(date_breaks = "8 years", date_labels = "%Y") +
    ggplot2::labs(x = NULL, y = "Trailing 12-month RMSE (log RV)") + paper_theme
  save_figure(plot, "internet_appendix", "cluster_rolling_rmse",
    "Trailing 12-month out-of-sample RMSE for the equal-weighted forecast by Jensen, Kelly, and Pedersen factor cluster. The line pools squared forecast errors across all factors and months in each cluster window before taking the square root. The shaded region spans the interquartile range of factor-level RMSE within the cluster.",
    width = 11, height = 8)

# Validation and hyperparameter diagnostics-------------------------------------

  validation_blocks <- purrr::imap_dfr(results, function(models, universe) {
    purrr::imap_dfr(models[setdiff(base_models, c("har", "harx"))], function(result, model) {
      meta <- result$backtest_meta
      scored <- assign_refit_dates(result$oos_outputs, meta$rebal_dates) %>%
        summarise_forecasts("refit_date", meta)
      val <- result$val_eval_metrics_hyper_choice
      validation <- tibble::tibble(refit_date = as.Date(zoo::index(val)), validation_rmse = as.numeric(val[, "rmse"]))
      coverage <- assign_refit_dates(result$oos_outputs, meta$rebal_dates) %>%
        dplyr::group_by(refit_date) %>% dplyr::summarise(test_months = dplyr::n_distinct(month_id), .groups = "drop")
      scored %>% dplyr::select(refit_date, test_rmse = rmse) %>%
        dplyr::left_join(validation, by = "refit_date", relationship = "one-to-one") %>%
        dplyr::left_join(coverage, by = "refit_date", relationship = "one-to-one") %>%
        dplyr::mutate(model = model, universe = universe)
    })
  })
  if (anyNA(validation_blocks)) rlang::abort("Validation/test block alignment failed.")
  save_data(validation_blocks, "validation_test_blocks")
  plot <- validation_blocks %>%
    tidyr::pivot_longer(c(test_rmse, validation_rmse), names_to = "sample", values_to = "rmse") %>%
    dplyr::mutate(model = as_model_label(model), universe = as_universe_label(universe)) %>%
    ggplot2::ggplot(ggplot2::aes(refit_date, rmse, colour = sample, linetype = sample)) +
    ggplot2::geom_line(linewidth = 0.6) + ggplot2::facet_grid(universe ~ model) +
    ggplot2::scale_colour_manual(values = c(test_rmse = "#D55E00", validation_rmse = "#0072B2"),
      labels = c("Subsequent test block", "Selected validation")) +
    ggplot2::scale_linetype_manual(values = c(test_rmse = "solid", validation_rmse = "dashed"),
      labels = c("Subsequent test block", "Selected validation")) +
    ggplot2::scale_x_date(date_breaks = "8 years", date_labels = "%Y") +
    ggplot2::labs(x = "Refit date", y = "RMSE (log RV)", colour = NULL, linetype = NULL) + paper_theme
  save_figure(plot, "internet_appendix", "validation_test", "Validation RMSE and subsequent out-of-sample RMSE after each annual model selection. Validation windows contain 90 months and subsequent forecast blocks normally contain 12 months. Neural-network validation uses one fitted network, while out-of-sample forecasts average ten independent refits. Differences may therefore reflect refitting, sample length, and changes in volatility conditions.", width = 12, height = 5.5)

  hyperparameters <- purrr::imap_dfr(results, function(models, universe) {
    purrr::imap_dfr(models[setdiff(base_models, c("har", "harx"))], function(result, model) {
      x <- result$hyper_choice
      data <- tibble::as_tibble(as.data.frame(zoo::coredata(x)))
      data$refit_date <- as.Date(zoo::index(x))
      data %>% tidyr::pivot_longer(-refit_date, names_to = "parameter", values_to = "value") %>%
        dplyr::mutate(model = model, universe = universe)
    })
  })
  save_data(hyperparameters, "hyperparameters")
  settings <- purrr::imap_dfr(results, function(models, universe) {
    purrr::imap_dfr(models[base_models], function(result, model) {
      meta <- result$backtest_meta
      domain <- meta$hyper_grid_domain_list
      fixed <- meta$fixed_params
      dplyr::bind_rows(purrr::imap_dfr(domain, ~ tibble::tibble(parameter = .y, setting = paste(.x, collapse = ", "), role = "Searched")),
        purrr::imap_dfr(fixed, ~ tibble::tibble(parameter = .y, setting = paste(.x, collapse = ", "), role = "Fixed"))) %>%
        dplyr::mutate(universe = universe, model = model)
    })
  })
  save_data(settings, "hyperparameter_settings")
  parameter_labels <- c(alpha = "L1 share", best_lam = "Penalty",
    mtry = "Feature fraction", max.depth = "Maximum depth", eta = "Learning rate",
    max_depth = "Maximum depth", min_child_weight = "Minimum child weight",
    best_iter = "Early-stopping iteration", lr = "Learning rate",
    droprate = "Dropout rate", regularizer_l1 = "L1 penalty")
  hyperparameter_plot <- hyperparameters %>%
    dplyr::mutate(model_label = as_model_label(model),
      universe = as_universe_label(universe),
      parameter_label = dplyr::recode(parameter, !!!parameter_labels),
      panel = paste(model_label, parameter_label, sep = ": "))
  plot <- ggplot2::ggplot(hyperparameter_plot,
      ggplot2::aes(refit_date, value, colour = universe)) +
    ggplot2::geom_step(linewidth = 0.5) + ggplot2::geom_point(size = 0.7) +
    ggplot2::facet_wrap(~ panel, scales = "free_y", ncol = 4) +
    ggplot2::scale_x_date(date_breaks = "10 years", date_labels = "%Y") +
    ggplot2::labs(x = NULL, y = "Selected value", colour = NULL) + paper_theme +
    ggplot2::theme(strip.text = ggplot2::element_text(size = 7),
      axis.text.x = ggplot2::element_text(size = 6))
  save_figure(plot, "internet_appendix", "hyperparameters_all",
    "Selected hyperparameters at each annual refit. Each panel identifies the model and parameter. Lines compare pooled-panel estimation with separate market-portfolio estimation. The boosting iteration count is selected by early stopping.",
    width = 11, height = 8.5)

# Surrogate feature loadings----------------------------------------------------

  importance <- purrr::imap_dfr(results, function(models, universe) {
    purrr::imap_dfr(models[base_models], ~ dplyr::mutate(.x$feat_imp, universe = universe, model = .y))
  })
  final_importance <- purrr::imap_dfr(results, function(models, universe) {
    purrr::imap_dfr(models[base_models], function(result, model) {
      result$final_feat_imp %>% dplyr::mutate(universe = universe, model = model,
        aliased = feat %in% result$final_aliases)
    })
  })
  theme_map <- report_theme_map()
  theme_history <- summarise_theme_loadings(importance, theme_map)
  theme_final <- summarise_theme_loadings(final_importance, theme_map)
  save_data(theme_map, "feature_theme_map")
  save_data(importance, "feature_loadings_over_time")
  save_data(final_importance, "final_feature_loadings")
  save_data(theme_history, "theme_loadings_over_time")
  save_data(theme_final, "final_theme_loadings")
  loading_note <- paste("The figure reports descriptive loadings from linear surrogate models.",
    "Columns report HAR, HARX, elastic net, random forest, XGBoost, and the three neural-network architectures.",
    "The intercept is excluded. Each coefficient is divided by the sum of absolute non-intercept coefficients.",
    "Gross shares sum absolute loadings within themes. Net shares retain coefficient signs.",
    "A zero loading caused by exact collinearity does not establish that a predictor is economically irrelevant.",
    "Own-series terms refer to each factor's realized measures. The equal-weighted forecast has no fitted surrogate model.")
  for (measure in c("gross_share", "net_share")) {
    plot <- theme_final %>% dplyr::mutate(model = as_model_label(model), universe = as_universe_label(universe)) %>%
      ggplot2::ggplot(ggplot2::aes(model, theme, fill = .data[[measure]])) +
      ggplot2::geom_tile(colour = "white") +
      ggplot2::geom_text(ggplot2::aes(label = sprintf("%.2f", .data[[measure]])), size = 2.4) +
      ggplot2::facet_wrap(~ universe, ncol = 1) +
      ggplot2::labs(x = NULL, y = NULL, fill = "Share") + paper_theme
    plot <- if (measure == "net_share") plot + ggplot2::scale_fill_gradient2(low = "#D55E00", mid = "white", high = "#0072B2", midpoint = 0)
    else plot + ggplot2::scale_fill_gradient(low = "white", high = "#56B4E9", limits = c(0, 1))
    measure_label <- if (measure == "gross_share") "absolute loading shares" else "signed loading shares"
    section <- if (measure == "gross_share") "main_appendix" else "internet_appendix"
    save_figure(plot, section, paste0("final_theme_", measure),
      paste("Predictor-theme", measure_label, "at the final model refit.", loading_note), width = 9, height = 7)
  }
  for (universe in names(results)) {
    plot <- theme_history %>% dplyr::filter(.data$universe == .env$universe) %>%
      dplyr::mutate(model = as_model_label(model)) %>%
      ggplot2::ggplot(ggplot2::aes(dates, theme, fill = gross_share)) +
      ggplot2::geom_tile() + ggplot2::facet_wrap(~ model, ncol = 2) +
      ggplot2::scale_fill_gradient(low = "white", high = "#0072B2", limits = c(0, 1)) +
      ggplot2::scale_x_date(date_breaks = "5 years", date_labels = "%Y") +
      ggplot2::labs(x = "Refit date", y = NULL, fill = "Gross share") + paper_theme
    universe_label <- universe_map$universe_label[universe_map$universe == universe]
    save_figure(plot, "internet_appendix", paste0("theme_history_", universe),
      paste("Predictor-theme loading shares over time for the", universe_label, "estimation.", loading_note), width = 10, height = 10)
  }

# Economic performance of volatility-managed portfolios-----------------------

  portfolio_headings <- c(
    main_appendix = paste("\\clearpage",
                          "\\subsubsection{Portfolio Performance by Forecasting Method}", sep = "\n")
  )
  purrr::iwalk(portfolio_headings, function(heading, section) {
    writeLines(heading, file.path(output_dir, section, "portfolio_section.tex"))
    register_output(section, "portfolio_section.tex")
  })

  portfolio_root <- file.path(project_dir, "ports", "v3", sample_id)
  portfolio_paths <- c(
    panel = file.path(portfolio_root, "panel", "expanding_vol_10bps_60m", "all_ports.rds"),
    spx = file.path(portfolio_root, "spx", "expanding_vol_10bps_60m", "all_ports.rds")
  )
  if (any(!file.exists(portfolio_paths))) {
    rlang::abort(paste("Missing V3 portfolio results:",
                       paste(portfolio_paths[!file.exists(portfolio_paths)], collapse = ", ")))
  }
  portfolio_results <- purrr::map(portfolio_paths, readRDS)
  portfolio_model_order <- c("glmnet", "har", "harx", "nn1", "nn2", "nn3", "rf", "xgb", "ew")
  required_portfolio_columns <- c(
    "name", "month_id", "method", "w_raw", "w_capped", "turnover_raw", "turnover",
    "tc_raw", "tc", "r_exc_fwd1_raw", "r_exc_fwd1_capped", "r_exc_fwd1_net_raw",
    "r_exc_fwd1_net_capped", "r_strat_fwd1_raw", "r_strat_fwd1_capped",
    "r_strat_fwd1_net_raw", "r_strat_fwd1_net_capped"
  )
  purrr::iwalk(portfolio_results, function(models, source_universe) {
    if (!setequal(names(models), portfolio_model_order)) {
      rlang::abort(paste("Unexpected portfolio models for", source_universe))
    }
    reference_settings <- models$ew$settings
    purrr::iwalk(models, function(result, model) {
      weights <- result$w_compiled_df
      numeric_columns <- setdiff(required_portfolio_columns, c("name", "month_id", "method"))
      if (!is.data.frame(weights) || !all(required_portfolio_columns %in% names(weights)) ||
          anyDuplicated(weights[c("name", "month_id", "method")]) ||
          !inherits(weights$month_id, "Date") ||
          any(!is.finite(as.matrix(weights[numeric_columns])))) {
        rlang::abort(paste("Invalid portfolio output for", source_universe, model))
      }
      if (!identical(result$settings, reference_settings)) {
        rlang::abort(paste("Portfolio settings differ across models for", source_universe))
      }
    })
    if (!isTRUE(reference_settings$implementable) ||
        !identical(reference_settings$normalization, "expanding_vol") ||
        reference_settings$min_history != 60L ||
        !isTRUE(all.equal(reference_settings$weight_cap, 1.5)) ||
        !isTRUE(all.equal(reference_settings$cost_per_unit, 0.001))) {
      rlang::abort(paste("Unexpected portfolio design for", source_universe))
    }
  })
  panel_names <- unique(portfolio_results$panel$ew$w_compiled_df$name)
  spx_names <- unique(portfolio_results$spx$ew$w_compiled_df$name)
  if (!setequal(panel_names, c(cluster_map$name, "spx_vw")) ||
      !identical(spx_names, "spx_vw")) {
    rlang::abort("Portfolio universes do not match the forecast universes.")
  }

  principal_strategy_order <- c("unmanaged", "rw", "har", "harx", "ew")
  detailed_strategy_order <- c("unmanaged", "rw", "har", "harx", "glmnet",
                               "rf", "xgb", "nn1", "nn2", "nn3", "ew")
  portfolio_strategy_labels <- c(
    unmanaged = "Unmanaged", rw = "Random walk", har = "HAR", harx = "HARX",
    glmnet = "ENET", rf = "RF", xgb = "XGB", nn1 = "NN1", nn2 = "NN2",
    nn3 = "NN3", ew = "EW"
  )
  principal_palette <- c(
    Unmanaged = "#222222", `Random walk` = "#8C8C8C", HAR = "#D55E00",
    HARX = "#E69F00", EW = "#0072B2"
  )
  universe_labels <- c(
    market_separate = "Market portfolio, separate estimation",
    market_pooled = "Market portfolio, pooled estimation",
    factor_panel = "Equal-weighted factor panel"
  )
  universe_order <- names(universe_labels)

  select_portfolio_strategies <- function(models, names_to_keep, strategies) {
    benchmark_rows <- dplyr::bind_rows(
      models$ew$w_compiled_df %>%
        dplyr::filter(method == "buyhold", name %in% names_to_keep) %>%
        dplyr::mutate(strategy = "unmanaged"),
      models$ew$w_compiled_df %>%
        dplyr::filter(method == "rw", name %in% names_to_keep) %>%
        dplyr::mutate(strategy = "rw")
    )
    managed_rows <- purrr::map_dfr(intersect(strategies, names(models)), function(model) {
      models[[model]]$w_compiled_df %>%
        dplyr::filter(method == "vol_scaled", name %in% names_to_keep) %>%
        dplyr::mutate(strategy = model)
    })
    dplyr::bind_rows(benchmark_rows, managed_rows) %>%
      dplyr::filter(strategy %in% strategies) %>%
      dplyr::arrange(strategy, name, month_id)
  }
  principal_returns <- dplyr::bind_rows(
    select_portfolio_strategies(portfolio_results$spx, "spx_vw", principal_strategy_order) %>%
      dplyr::mutate(universe = "market_separate"),
    select_portfolio_strategies(portfolio_results$panel, "spx_vw", principal_strategy_order) %>%
      dplyr::mutate(universe = "market_pooled"),
    select_portfolio_strategies(portfolio_results$panel, cluster_map$name, principal_strategy_order) %>%
      dplyr::mutate(universe = "factor_panel")
  )
  detailed_factor_returns <- select_portfolio_strategies(
    portfolio_results$panel, cluster_map$name, detailed_strategy_order
  ) %>%
    dplyr::left_join(cluster_map, by = "name", relationship = "many-to-one")
  if (anyDuplicated(principal_returns[c("universe", "strategy", "name", "month_id")]) ||
      anyNA(detailed_factor_returns$cluster)) {
    rlang::abort("Portfolio strategy keys or Jensen, Kelly, and Pedersen clusters are invalid.")
  }

  return_versions <- tibble::tribble(
    ~version, ~excess_column, ~total_column,
    "raw", "r_exc_fwd1_raw", "r_strat_fwd1_raw",
    "capped", "r_exc_fwd1_capped", "r_strat_fwd1_capped",
    "net_raw", "r_exc_fwd1_net_raw", "r_strat_fwd1_net_raw",
    "net_capped", "r_exc_fwd1_net_capped", "r_strat_fwd1_net_capped"
  )
  stack_return_versions <- function(data) {
    purrr::pmap_dfr(return_versions, function(version, excess_column, total_column) {
      data %>%
        dplyr::mutate(version = version, excess_return = .data[[excess_column]],
                      total_return = .data[[total_column]])
    })
  }
  principal_versions <- stack_return_versions(principal_returns)
  principal_aggregate <- aggregate_portfolio_returns(
    principal_versions, c("universe", "strategy", "version"),
    c("excess_return", "total_return", "factor_exc", "w_raw")
  )
  portfolio_summary <- summarise_portfolio_returns(
    principal_aggregate, c("universe", "strategy", "version"),
    unmanaged_col = "factor_exc", weight_col = "w_raw"
  )
  save_data(principal_returns, "portfolio_principal_returns")
  save_data(portfolio_summary, "portfolio_summary")
  save_data(tibble::tibble(
    source_universe = names(portfolio_paths), file = unname(portfolio_paths),
    sha256 = purrr::map_chr(portfolio_paths, hash_artifact)
  ), "portfolio_input_manifest")

  summary_table <- portfolio_summary %>%
    dplyr::select(universe, strategy, version, sharpe_ratio, cer_ann, vol_ratio, avg_weight_raw) %>%
    tidyr::pivot_longer(c(sharpe_ratio, cer_ann, vol_ratio, avg_weight_raw),
      names_to = "statistic", values_to = "value") %>%
    dplyr::mutate(
      universe = factor(universe, levels = universe_order),
      strategy = factor(strategy, levels = principal_strategy_order),
      statistic = factor(statistic, levels = c("sharpe_ratio", "cer_ann", "vol_ratio", "avg_weight_raw")),
      value = ifelse(statistic == "cer_ann", 100 * value, value),
      Model = portfolio_strategy_labels[as.character(strategy)],
      Statistic = dplyr::case_when(statistic == "sharpe_ratio" ~ "Sharpe ratio",
        statistic == "cer_ann" ~ "CER (%)", statistic == "vol_ratio" ~ "Volatility ratio",
        statistic == "avg_weight_raw" ~ "Mean raw exposure")
    ) %>%
    dplyr::select(universe, strategy, statistic, Model, Statistic, version, value) %>%
    tidyr::pivot_wider(names_from = version, values_from = value) %>%
    dplyr::arrange(universe, strategy, statistic) %>%
    dplyr::select(universe, Model, Statistic, Raw = raw, Capped = capped,
                  `Net raw` = net_raw, `Net capped` = net_capped)
  main_universes <- c("market_pooled", "factor_panel")
  summary_panels <- stats::setNames(
    purrr::map(main_universes, ~ summary_table %>%
                 dplyr::filter(universe == .x) %>% dplyr::select(-universe)),
    paste0("Panel ", LETTERS[seq_along(main_universes)], ". ", universe_labels[main_universes])
  )
  save_panel_table(
    summary_panels, "main", "portfolio_performance",
    paste(
      "Economic performance of volatility-managed portfolios.",
      "Panel A uses market-portfolio forecasts from the pooled estimation.",
      "Panel B equal-weights the 153 factor strategies each month.",
      "SR denotes the annualized excess-return Sharpe ratio.",
      "CER denotes annualized certainty-equivalent return in percent for relative risk aversion of five.",
      "Volatility ratios compare each return version with the gross unmanaged portfolio on identical months.",
      "Mean raw exposure is averaged over factor-months before capping and is shared across versions.",
      "Capped strategies restrict exposure to 1.5. Net strategies deduct 10 basis points per unit change in factor exposure.",
      "EW combines HARX, elastic net, random forest, XGBoost, and the three neural networks."
    ), digits = 3, font_size = "scriptsize", placement = "htbp"
  )
  separate_market_panel <- stats::setNames(
    list(summary_table %>%
      dplyr::filter(universe == "market_separate") %>%
      dplyr::select(-universe)),
    "Market portfolio estimated separately"
  )

  cumulative_returns <- principal_aggregate %>%
    dplyr::filter(version == "net_capped") %>%
    dplyr::group_by(universe, strategy) %>%
    dplyr::arrange(month_id, .by_group = TRUE) %>%
    dplyr::mutate(wealth = cumprod(1 + total_return)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(strategy_label = portfolio_strategy_labels[strategy])
  if (any(cumulative_returns$wealth <= 0)) {
    rlang::abort("Portfolio wealth is nonpositive and cannot be displayed on a log scale.")
  }
  cumulative_captions <- c(
    market_separate = paste(
      "Cumulative wealth for the market portfolio from separate forecasts.",
      "Forecasting models are estimated only on the market portfolio.",
      "Returns are net of transaction costs and exposure is capped at 1.5."
    ),
    market_pooled = paste(
      "Cumulative wealth for the market portfolio from pooled forecasts.",
      "Forecasting models are estimated on the pooled factor and market panel.",
      "Returns are net of transaction costs and exposure is capped at 1.5."
    ),
    factor_panel = paste(
      "Cumulative wealth for the factor panel.",
      "Monthly returns are equal-weighted across the 153 factor strategies, net of transaction costs, and capped at 1.5."
    )
  )
  cumulative_filenames <- c(
    market_separate = "portfolio_wealth_market_separate",
    market_pooled = "portfolio_wealth_market_pooled",
    factor_panel = "portfolio_wealth_factor_panel"
  )
  unlink(file.path(output_dir, "main_appendix",
                   paste0("portfolio_wealth_market_separate.", c("pdf", "png", "tex"))))
  purrr::walk(universe_order, function(current_universe) {
    plot_data <- cumulative_returns %>% dplyr::filter(universe == current_universe)
    comparison <- plot_data %>%
      dplyr::filter(strategy != "ew") %>%
      dplyr::group_by(month_id) %>%
      dplyr::summarise(best_benchmark = max(wealth), .groups = "drop") %>%
      dplyr::left_join(
        plot_data %>% dplyr::filter(strategy == "ew") %>%
          dplyr::select(month_id, ew_wealth = wealth),
        by = "month_id", relationship = "one-to-one"
      ) %>%
      dplyr::mutate(
        ew_outperforms = ew_wealth >= best_benchmark,
        run = cumsum(ew_outperforms != dplyr::lag(ew_outperforms,
                                                  default = dplyr::first(ew_outperforms))),
        lower = pmin(ew_wealth, best_benchmark), upper = pmax(ew_wealth, best_benchmark)
      )
    plot <- ggplot2::ggplot() +
      ggplot2::geom_ribbon(
        data = comparison,
        ggplot2::aes(month_id, ymin = lower, ymax = upper,
                     fill = ew_outperforms, group = interaction(ew_outperforms, run)),
        alpha = 0.28
      ) +
      ggplot2::geom_line(
        data = plot_data,
        ggplot2::aes(month_id, wealth, colour = strategy_label), linewidth = 0.65
      ) +
      ggplot2::scale_y_log10() +
      ggplot2::scale_colour_manual(values = principal_palette) +
      ggplot2::scale_fill_manual(
        values = c(`TRUE` = "#56B4E9", `FALSE` = "#E69F00"), guide = "none"
      ) +
      ggplot2::labs(x = NULL, y = "Cumulative wealth, log scale", colour = NULL) + paper_theme
    figure_section <- if (current_universe == "market_separate") {
      "internet_appendix"
    } else {
      "main_appendix"
    }
    save_figure(
      plot, figure_section, cumulative_filenames[[current_universe]],
      paste(
        cumulative_captions[[current_universe]],
        "Lines show the unmanaged, random-walk, HAR, HARX, and equal-weighted forecast strategies.",
        "Blue shading marks dates when EW wealth exceeds the strongest benchmark. Orange shading marks dates when it does not."
      ), width = 9, height = 5
    )
  })

  detailed_factor_versions <- stack_return_versions(detailed_factor_returns) %>%
    dplyr::filter(version == "net_capped")
  factor_portfolio_metrics <- summarise_portfolio_returns(
    detailed_factor_versions, c("cluster", "strategy", "name")
  )
  cluster_portfolio_returns <- aggregate_portfolio_returns(
    detailed_factor_versions, c("cluster", "strategy"),
    c("excess_return", "total_return")
  )
  cluster_portfolio_metrics <- summarise_portfolio_returns(
    cluster_portfolio_returns, c("cluster", "strategy")
  )
  save_data(factor_portfolio_metrics, "portfolio_factor_metrics")
  save_data(cluster_portfolio_metrics, "portfolio_cluster_metrics")

  cluster_aggregate_table <- cluster_portfolio_metrics %>%
    dplyr::mutate(strategy = portfolio_strategy_labels[strategy]) %>%
    dplyr::select(cluster, strategy, value = sharpe_ratio) %>%
    tidyr::pivot_wider(names_from = strategy, values_from = value) %>%
    dplyr::mutate(Factor = "Cluster aggregate", row_type = "aggregate")
  cluster_detail_table <- factor_portfolio_metrics %>%
    dplyr::mutate(strategy = portfolio_strategy_labels[strategy]) %>%
    dplyr::select(cluster, Factor = name, strategy, value = sharpe_ratio) %>%
    tidyr::pivot_wider(names_from = strategy, values_from = value) %>%
    dplyr::mutate(row_type = "factor")
  detailed_labels <- unname(portfolio_strategy_labels[detailed_strategy_order])
  cluster_portfolio_table <- dplyr::bind_rows(cluster_aggregate_table, cluster_detail_table) %>%
    dplyr::mutate(
      cluster = factor(cluster, levels = cluster_order),
      row_order = ifelse(row_type == "aggregate", 0L, 1L)
    ) %>%
    dplyr::arrange(cluster, row_order, Factor) %>%
    dplyr::mutate(cluster = as.character(cluster)) %>%
    dplyr::select(cluster, Factor, row_type, dplyr::all_of(detailed_labels))
  save_cluster_table(
    cluster_portfolio_table, "internet_appendix", "portfolio_factor_sharpe",
    paste(
      "Net Sharpe ratios of volatility-managed factor portfolios by Jensen, Kelly, and Pedersen factor cluster.",
      "The first row of each panel reports the Sharpe ratio of an equal-weighted portfolio of the factors in that cluster.",
      "The remaining rows report individual factor strategies.",
      "All managed strategies cap exposure at 1.5 and deduct 10 basis points per unit change in factor exposure.",
      "Light blue cells identify strategies that outperform the strongest result between the unmanaged and random-walk benchmarks. Bold cells identify the highest Sharpe ratio in each row."
    ), "max", digits = 3,
    benchmark_columns = c("Unmanaged", "Random walk"),
    shade_columns = c("HAR", "HARX", "ENET", "RF", "XGB", "NN1", "NN2", "NN3", "EW"),
    landscape = TRUE
  )
  save_panel_table(
    separate_market_panel, "internet_appendix", "portfolio_performance_market_separate",
    paste(
      "Economic performance of volatility-managed portfolios for the market portfolio estimated separately.",
      "Each forecasting model is estimated only on the market-portfolio time series.",
      "SR denotes the annualized excess-return Sharpe ratio.",
      "CER denotes annualized certainty-equivalent return in percent for relative risk aversion of five.",
      "Capped strategies restrict exposure to 1.5. Net strategies deduct 10 basis points per unit change in factor exposure.",
      "EW combines HARX, elastic net, random forest, XGBoost, and the three neural networks."
    ), digits = 3, font_size = "scriptsize", placement = "htbp"
  )

  cluster_plot_data <- factor_portfolio_metrics %>%
    dplyr::filter(strategy %in% principal_strategy_order) %>%
    dplyr::mutate(
      cluster = factor(cluster, levels = cluster_order),
      strategy_label = factor(portfolio_strategy_labels[strategy],
                              levels = unname(portfolio_strategy_labels[principal_strategy_order]))
    )
  plot <- ggplot2::ggplot(
      cluster_plot_data, ggplot2::aes(strategy_label, sharpe_ratio, fill = strategy_label)
    ) +
    ggplot2::geom_hline(yintercept = 0, colour = "grey75", linewidth = 0.35) +
    ggplot2::geom_boxplot(width = 0.72, linewidth = 0.35, outlier.alpha = 0.35,
                         outlier.size = 0.7) +
    ggplot2::facet_wrap(~ cluster, ncol = 3, scales = "free_y") +
    ggplot2::scale_fill_manual(values = principal_palette) +
    ggplot2::labs(x = NULL, y = "Annualized net Sharpe ratio", fill = NULL) + paper_theme +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
                   legend.position = "none")
  save_figure(
    plot, "main_appendix", "portfolio_cluster_sharpe",
    paste(
      "Cross-sectional distribution of net Sharpe ratios for volatility-managed factor portfolios by Jensen, Kelly, and Pedersen factor cluster.",
      "Each box summarizes the factor-level Sharpe ratios within the indicated cluster. The center line is the median, the box spans the interquartile range, and the whiskers extend to the most extreme observations within 1.5 interquartile ranges.",
      "Managed strategies cap exposure at 1.5 and deduct 10 basis points per unit change in factor exposure."
    ), width = 10, height = 11
  )

  ew_factor_metrics <- summarise_portfolio_returns(
    stack_return_versions(dplyr::filter(detailed_factor_returns, strategy == "ew")),
    c("name", "version")
  ) %>%
    dplyr::select(name, version, sharpe_ratio) %>%
    tidyr::pivot_wider(names_from = version, values_from = sharpe_ratio)
  waterfall_data <- ew_factor_metrics %>%
    dplyr::summarise(
      raw = mean(raw), capped = mean(capped), net_capped = mean(net_capped),
      raw_p25 = stats::quantile(raw, 0.25), raw_p75 = stats::quantile(raw, 0.75),
      cap_p25 = stats::quantile(capped - raw, 0.25),
      cap_p75 = stats::quantile(capped - raw, 0.75),
      cost_p25 = stats::quantile(net_capped - capped, 0.25),
      cost_p75 = stats::quantile(net_capped - capped, 0.75),
      net_p25 = stats::quantile(net_capped, 0.25),
      net_p75 = stats::quantile(net_capped, 0.75)
    )
  waterfall_plot_data <- tibble::tibble(
    step = factor(c("Raw", "Exposure cap", "Transaction costs", "Net capped"),
                  levels = c("Raw", "Exposure cap", "Transaction costs", "Net capped")),
    start = c(0, waterfall_data$raw, waterfall_data$capped, 0),
    end = c(waterfall_data$raw, waterfall_data$capped,
            waterfall_data$net_capped, waterfall_data$net_capped),
    lower = c(waterfall_data$raw_p25, waterfall_data$raw + waterfall_data$cap_p25,
              waterfall_data$capped + waterfall_data$cost_p25, waterfall_data$net_p25),
    upper = c(waterfall_data$raw_p75, waterfall_data$raw + waterfall_data$cap_p75,
              waterfall_data$capped + waterfall_data$cost_p75, waterfall_data$net_p75)
  ) %>%
    dplyr::mutate(effect = ifelse(end >= start, "Increase", "Decrease"))
  save_data(ew_factor_metrics, "portfolio_ew_factor_sharpe_decomposition")
  save_data(waterfall_plot_data, "portfolio_ew_sharpe_waterfall")
  plot <- ggplot2::ggplot(waterfall_plot_data, ggplot2::aes(step)) +
    ggplot2::geom_linerange(
      ggplot2::aes(ymin = pmin(start, end), ymax = pmax(start, end), colour = effect),
      linewidth = 14, lineend = "butt"
    ) +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = lower, ymax = upper), width = 0.18) +
    ggplot2::geom_hline(yintercept = 0, colour = "grey50", linewidth = 0.35) +
    ggplot2::scale_colour_manual(values = c(Increase = "#0072B2", Decrease = "#D55E00"),
                                guide = "none") +
    ggplot2::labs(x = NULL, y = "Cross-sectional mean Sharpe ratio", colour = NULL) + paper_theme
  save_figure(
    plot, "internet_appendix", "portfolio_ew_sharpe_waterfall",
    paste(
      "Decomposition of the equal-weighted forecast strategy's Sharpe ratio across the 153 factors.",
      "The bars show the cross-sectional mean raw Sharpe ratio, the change associated with capping exposure at 1.5, the change associated with transaction costs, and the resulting net capped Sharpe ratio.",
      "Error bars report the interquartile range across factors for the raw and final levels and for each intermediate change."
    ), width = 8, height = 5
  )

  implementation_metrics <- portfolio_summary %>%
    dplyr::filter(version == "net_capped") %>%
    dplyr::select(universe, strategy, annualized_excess_return, annualized_volatility,
                  sharpe_ratio, cer_ann, vol_ratio, avg_weight_raw, max_drawdown)
  implementation_trading <- principal_returns %>%
    dplyr::group_by(universe, strategy) %>%
    dplyr::summarise(
      annualized_cost = 12 * mean(tc), average_turnover = mean(turnover),
      maximum_raw_exposure = max(w_raw), cap_fraction = mean(w_raw > 1.5),
      .groups = "drop"
    )
  implementation_table <- implementation_metrics %>%
    dplyr::left_join(implementation_trading, by = c("universe", "strategy"),
                     relationship = "one-to-one") %>%
    dplyr::mutate(
      universe = factor(universe, levels = universe_order),
      strategy = factor(strategy, levels = principal_strategy_order),
      Model = portfolio_strategy_labels[as.character(strategy)]
    ) %>%
    dplyr::arrange(universe, strategy) %>%
    dplyr::transmute(
      universe, Model, `Return (%)` = 100 * annualized_excess_return,
      `Vol. (%)` = 100 * annualized_volatility, Sharpe = sharpe_ratio,
      `CER (%)` = 100 * cer_ann, `Vol. ratio` = vol_ratio,
      `Raw exposure` = avg_weight_raw, `MDD (%)` = 100 * max_drawdown,
      `Cost (%)` = 100 * annualized_cost, Turnover = average_turnover,
      `Max. exposure` = maximum_raw_exposure, `Capped (%)` = 100 * cap_fraction
    )
  implementation_panels <- stats::setNames(
    purrr::map(universe_order, ~ implementation_table %>%
                 dplyr::filter(universe == .x) %>% dplyr::select(-universe)),
    paste0("Panel ", LETTERS[seq_along(universe_order)], ". ", universe_labels)
  )
  save_data(implementation_table, "portfolio_implementation_metrics")
  save_panel_table(
    implementation_panels, "internet_appendix", "portfolio_implementation",
    paste(
      "Implementation statistics for net capped volatility-managed portfolios.",
      "Return, volatility, certainty-equivalent return, transaction cost, and maximum drawdown are reported in percent.",
      "Sharpe ratios use excess returns and maximum drawdowns use total returns.",
      "Turnover is the average monthly absolute change in factor exposure. Maximum exposure is measured before capping.",
      "The capped percentage is the share of factor-month observations for which uncapped exposure exceeds 1.5.",
      "Panel C first equal-weights factor-strategy returns within each month. Its implementation statistics summarize the underlying factor-month exposures."
    ), digits = 3, font_size = "scriptsize", tabcolsep = 2.5, landscape = TRUE
  )

# LaTeX integration and reproducibility record----------------------------------

  ## The standalone entry point uses the same saved portfolios and may also be
  ## run by itself while developing inference. It never constructs portfolios.
  source(file.path(project_dir, "R", "report_portfolio_inference.R"))
  portfolio_inference <- report_portfolio_inference(project_dir, output_dir)
  purrr::iwalk(portfolio_inference$outputs, function(files, section) {
    purrr::walk(files, ~ register_output(section, .x))
  })

  for (section in names(generated)) {
    input_lines <- paste0("\\input{results_v3/", sample_id, "/",
                           gsub("\\", "/", generated[[section]], fixed = TRUE), "}")
    writeLines(input_lines, file.path(output_dir, paste0(section, ".tex")))
  }
  notes <- c(
    "\\section*{Reading these results}",
    "All forecasts concern monthly log realized variance, log(RV + 1e-8). The common test period is July 2006 to December 2024 and contains 222 months. The pooled sample contains 153 factors and the market portfolio. The raw sample begins in 1986. Expanding preprocessing requires 36 initial observations. The initial estimation split contains 119 training months and 90 validation months, followed by a 209-month refit. Training then expands, validation remains fixed at 90 months, and models are selected each June.",
    "The market portfolio is estimated both within the pooled factor panel and as a separate time series.",
    "Pooled RMSE is the square root of the mean squared error across all series and months. It is therefore distinct from the average factor RMSE. R-squared is computed relative to the mean within each evaluation sample.",
    "EW gives HARX, ENET, RF, XGB, NN1, NN2 and NN3 one-seventh each within each target-month, on the log scale. Each NN architecture counts once despite its ten internal members. Exponentiating this average is not the arithmetic mean of variance-level forecasts. No missing-member renormalization is allowed.",
    "Validation scores describe selected hyperparameters on overlapping 90-month windows. Neural network tuning evaluates one network, while test forecasts average ten refitted networks. The comparison may also reflect refitting, window length, and changes in volatility conditions.",
    "Feature summaries are descriptive linear surrogate loadings. Gross and net shares exclude the intercept. Exact collinearity can produce zero loadings and does not establish economic irrelevance. Predictor scales differ, and the equal-weighted forecast has no fitted surrogate model.",
    "The sequential RMSE differences are arithmetic comparisons across model classes. They should not be interpreted as causal contributions. Elastic net adds shrinkage, while nonlinear learners also change regularization and estimation.",
    "Portfolio Sharpe ratios and certainty-equivalent returns use excess returns. Cumulative wealth and maximum drawdowns use total returns, including the risk-free collateral return. Factor-panel portfolio returns are equal-weighted across the 153 factors within each month.",
    "Managed exposure uses expanding volatility matching with at least 60 prior calibration observations. At each formation date, the normalizer matches unmanaged and inverse-variance-managed volatility over the available past observations. Exposure is capped at 1.5. Net returns deduct 10 basis points per unit change in factor exposure. These costs exclude trading within the underlying long-short factors.",
    "Portfolio inference uses the common July 2011 to December 2024 period, with 162 holding months. It uses 10,000 paired stationary-bootstrap draws with expected block length five, with three and ten as sensitivity analyses. Mean and certainty-equivalent return inference extends the Sharpe comparison. One-sided tests use centered bootstrap errors and Holm adjustment across prespecified comparison families. Spanning regressions use Newey-West lag one, with lags five and ten as sensitivity analyses. Inference is conditional on the fitted strategies. The standalone portfolio inference preview contains only these new tests."
  )
  writeLines(notes, file.path(output_dir, "notes.tex"))
  preview <- c("\\documentclass[11pt]{article}",
    "\\usepackage[a4paper,margin=18mm]{geometry}", "\\usepackage[T1]{fontenc}",
    "\\usepackage{graphicx,longtable,hyperref,caption,pdflscape}", "\\usepackage[table]{xcolor}",
    "\\DeclareCaptionLabelFormat{boldlabel}{\\textbf{#1 #2}}",
    "\\captionsetup{justification=raggedright,singlelinecheck=false}", "\\begin{document}",
    "\\title{ML RV: Forecast Results}", "\\author{}", "\\date{}", "\\maketitle",
    paste0("\\input{results_v3/", sample_id, "/notes.tex}"), "\\clearpage",
    "\\section{Main-paper selection}", paste0("\\input{results_v3/", sample_id, "/main.tex}"),
    "\\clearpage\\appendix\\section{Main appendix}",
    paste0("\\input{results_v3/", sample_id, "/main_appendix.tex}"),
    "\\clearpage\\section{Internet appendix}",
    paste0("\\input{results_v3/", sample_id, "/internet_appendix.tex}"), "\\end{document}")
  writeLines(preview, file.path(output_dir, "results_preview.tex"))
  readme <- c("# V3 forecast reporting", "",
    "Regenerate from the ML_RV project with source('R/report_results.R'). No estimation is performed.",
    "The project R library must be available. The script stops on inconsistent inputs.", "",
    "Compile from latex_paper: pdflatex -interaction=nonstopmode -halt-on-error -output-directory=results_v3/1986 results_v3/1986/results_preview.tex", "",
    "Integration: manuscript.tex reads main.tex, main_appendix.tex and internet_appendix.tex from this directory. The snippets require graphicx, longtable and pdflscape.",
    "See notes.tex for units, aggregation, validation, ensemble and surrogate conventions.",
    "data/ contains the EW RDS objects, forecasts, all metrics, tuning choices, feature coefficients and source hashes.",
    "main/, main_appendix/ and internet_appendix/ contain generated LaTeX tables and figure wrappers plus PDF/PNG figures.",
    "Only this generated results_v3/1986 directory is overwritten on rerun. Existing manuscript and estimation artifacts are not changed.")
  writeLines(readme, file.path(output_dir, "README.md"))
  capture.output(utils::sessionInfo(), file = file.path(output_dir, "data", "session_info.txt"))
  save_data(tibble::tibble(file = c("R/helpers.R", "R/reporting_helpers.R", "R/report_results.R"),
    sha256 = purrr::map_chr(file.path(project_dir, c("R/helpers.R", "R/reporting_helpers.R", "R/report_results.R")), hash_artifact)),
    "report_code_manifest")
  message("Reporting complete: ", output_dir)
