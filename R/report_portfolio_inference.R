# Inference for completed portfolio backtests ----------------------------------
# Run independently with source('R/report_portfolio_inference.R'), followed by
# report_portfolio_inference(). report_results.R uses the same entry point.
# This report deliberately uses only stationary blocks and HAC spanning alpha.

#' Report paired inference without re-estimating portfolio strategies
#'
#' @param project_dir Repository root.
#' @param output_dir Existing paper-results root. Outputs have an inference prefix
#'   so they cannot overwrite the older descriptive portfolio tables.
#' @param draws Bootstrap draws, 10000 for paper output. Smaller values support
#'   development checks and are explicitly recorded in every output.
#' @param seed Common random seed, fixed before looking at the inference results.
#' @return Invisibly, output registry, bootstrap results and spanning regressions.
#'   Writes LaTeX, CSV, RDS calendars/settings and a self-contained preview source.
report_portfolio_inference <- function(project_dir = here::here(),
    output_dir = file.path(project_dir, "latex_paper", "results_v3", "1986"),
    draws = 10000L, seed = 20260918L) {
  source(file.path(project_dir, "R", "portfolio_inference_helpers.R"), local = TRUE)
  source(file.path(project_dir, "R", "portfolio_inference_inputs.R"), local = TRUE)
  source(file.path(project_dir, "R", "reporting_helpers.R"), local = TRUE)
  code <- file.path(project_dir, "R", c("portfolio_inference_helpers.R", "portfolio_inference_inputs.R",
                                        "report_portfolio_inference.R", "reporting_helpers.R"))
  code_manifest <- tibble::tibble(file = code,
    sha256 = purrr::map_chr(code, ~ digest::digest(file = .x, algo = "sha256")))
  for (folder in c("data", "main_appendix", "internet_appendix")) {
    dir.create(file.path(output_dir, folder), recursive = TRUE, showWarnings = FALSE)
  }
  save_data <- function(x, name) utils::write.csv(x,
    file.path(output_dir, "data", paste0("portfolio_inference_", name, ".csv")), row.names = FALSE)
  message("Checking completed portfolios, matching benchmarks and provenance.")
  inputs <- read_primary_portfolio_inference(project_dir)
  aligned <- align_portfolio_inference(inputs$returns)
  save_data(inputs$returns, "returns")
  save_data(inputs$contrasts, "contrasts")
  save_data(inputs$manifest, "sources")
  save_data(inputs$provenance_checks, "provenance_checks")
  save_data(inputs$scaler_checks, "scaler_checks")
  save_data(inputs$ensemble_checks, "ensemble_checks")
  saveRDS(inputs$effective_settings,
    file.path(output_dir, "data", "portfolio_inference_effective_settings.rds"))
  metrics <- summarise_portfolio_returns(inputs$returns, "series_id") %>%
    dplyr::left_join(dplyr::distinct(inputs$returns, series_id, family, scope, version, model, n_factors),
      by = "series_id", relationship = "one-to-one")
  save_data(metrics, "statistics")

  ## Identical month draws enter every strategy in every reporting scope. In
  ## particular, aggregate-factor inference never samples factors independently.
  ## Primary results use L=5. L=3 and L=10 are sensitivity analyses, not additional
  ## opportunities to select a favorable p-value.
  bootstrap <- purrr::map_dfr(c(5, 3, 10), function(block_length) {
    message("Paired stationary bootstrap: ", draws, " draws, expected block length ", block_length, ".")
    calendar <- draw_portfolio_months(aligned$dates, draws, block_length, seed)
    saveRDS(calendar, file.path(output_dir, "data",
      paste0("portfolio_inference_calendar_", block_length, "m.rds")))
    infer_portfolio_pairs(inputs$returns, inputs$contrasts, calendar) %>%
      dplyr::left_join(dplyr::select(inputs$contrasts, comparison_id, portfolio_family,
                                   scope, version, forecast, comparator),
        by = "comparison_id", relationship = "many-to-one")
  })
  save_data(bootstrap, "bootstrap")
  spanning <- purrr::map_dfr(c(1L, 5L, 10L), function(lag) {
    purrr::pmap_dfr(inputs$contrasts, function(comparison_id, family, portfolio_family,
      scope, version, forecast, comparator, strategy, benchmark) {
      data <- tibble::tibble(month_id = aligned$dates,
        excess_return = aligned$returns[, strategy], benchmark_return = aligned$returns[, benchmark])
      span_portfolio_returns(data, lag) %>% dplyr::mutate(comparison_id, family,
        portfolio_family, scope, version, forecast, comparator)
    })
  })
  save_data(spanning, "spanning")

  ## Cross-sectional distributions describe individual factors. Their statistics
  ## are never substituted for statistics of the aggregate return series above.
  individual <- summarise_portfolio_returns(inputs$individual,
    c("family", "version", "model", "name", "cluster"))
  save_data(individual, "factor_statistics")
  factor_effects <- purrr::map_dfr(c("mm", "ced_ew"), function(family_id) {
    strategy <- individual %>% dplyr::filter(family == family_id, model == "ew")
    benchmarks <- individual %>% dplyr::filter(family == family_id,
      model %in% c("buyhold", "rw", "har", "harx")) %>%
      dplyr::select(family, version, name, comparator = model,
        benchmark_sharpe = sharpe_ratio, benchmark_cer = cer_ann)
    strategy %>% dplyr::inner_join(benchmarks, by = c("family", "version", "name"),
      relationship = "one-to-many") %>%
      dplyr::transmute(family, version, name, cluster, comparator,
        delta_sharpe = sharpe_ratio - benchmark_sharpe, delta_cer_ann = cer_ann - benchmark_cer)
  })
  save_data(factor_effects, "factor_effects")
  theme_effects <- factor_effects %>% dplyr::group_by(family, version, cluster, comparator) %>%
    dplyr::summarise(n_factors = dplyr::n(),
      dplyr::across(c(delta_sharpe, delta_cer_ann), list(
        mean = mean, median = stats::median, sd = stats::sd,
        q25 = ~ stats::quantile(.x, .25), q75 = ~ stats::quantile(.x, .75),
        fraction_positive = ~ mean(.x > 0))), .groups = "drop")
  save_data(theme_effects, "theme_effects")

  ## Prespecified subperiod results are descriptive. Short crisis windows do not
  ## receive annualized Sharpe tests. No window is selected based on its outcome.
  periods <- tibble::tibble(period = c("2011H2-2014", "2015-2019", "2020-2024"),
    start = as.Date(c("2011-07-01", "2015-01-01", "2020-01-01")),
    end = as.Date(c("2014-12-01", "2019-12-01", "2024-12-01")))
  subperiods <- purrr::pmap_dfr(periods, function(period, start, end) {
    inputs$returns %>% dplyr::filter(month_id >= start, month_id <= end) %>%
      summarise_portfolio_returns("series_id") %>% dplyr::mutate(period = period)
  })
  save_data(subperiods, "subperiod_statistics")

  outputs <- write_portfolio_inference_tables(bootstrap, output_dir, project_dir)
  if (!identical(code_manifest$sha256, purrr::map_chr(code,
      ~ digest::digest(file = .x, algo = "sha256")))) {
    rlang::abort("Reporting code changed during execution. Rerun before using the outputs.")
  }
  save_data(code_manifest, "code_manifest")
  capture.output(utils::sessionInfo(), file = file.path(output_dir, "data", "portfolio_inference_session.txt"))
  message("Portfolio inference complete: ", output_dir)
  invisible(list(outputs = outputs, bootstrap = bootstrap, spanning = spanning))
}

#' Render existing portfolio inference in the paper's table style
#'
#' @param bootstrap Output of the paired inference report, including scope labels.
#' @param output_dir Paper-results root. Existing table files are overwritten.
#' @param project_dir Repository root, used to source the existing table helper.
#' @return Invisible output registry. This function does not resample returns.
write_portfolio_inference_tables <- function(bootstrap, output_dir, project_dir = here::here()) {
  source(file.path(project_dir, "R", "reporting_helpers.R"), local = TRUE)
  draws <- unique(bootstrap$draws)
  if (length(draws) != 1L || !is.numeric(draws) || !is.finite(draws)) {
    rlang::abort("Inference tables require a common recorded draw count.")
  }
  family_label <- function(x) dplyr::recode(x, mm = "MM management",
    ced_ew = "50/50 single-factor combination", ced_mvo = "Signed two-sleeve MVO",
    multi_ew = "EW allocation", multi_hew = "HEW allocation")
  scope_label <- function(x) dplyr::recode(x, aggregate_153 = "153-factor aggregate",
    market_pooled = "Pooled market", market_separate = "Standalone market", all_154 = "154 factors")
  forecast_label <- function(x) dplyr::recode(x, ew = "EW forecast ensemble",
    cmv_panel_ew = "CMV, pooled market", cmv_spx_ew = "CMV, standalone market", fcmv_ew = "FCMV")
  benchmark_label <- function(x) dplyr::case_when(
    x == "buyhold" ~ "Unmanaged", x == "umv" ~ "UMV", x == "rw" ~ "RW",
    x == "unmanaged_mvo_ew" ~ "Unmanaged-only MVO", x == "lagged_market" ~ "Lagged market",
    x == "har" | grepl("_har$", x) ~ "HAR", x == "harx" | grepl("_harx$", x) ~ "HARX",
    TRUE ~ x)
  labels <- function(x) x %>% dplyr::mutate(
    context = paste(family_label(portfolio_family), scope_label(scope), sep = ", "),
    context = dplyr::if_else(startsWith(portfolio_family, "multi_"),
      paste(family_label(portfolio_family), forecast_label(forecast), sep = ", "), context),
    comparator_label = benchmark_label(comparator),
    version_label = dplyr::if_else(version == "raw", "Raw", "Capped"))
  outputs <- list(main_appendix = character(), internet_appendix = character())
  write_panels <- function(data, filename, caption, section) {
    contexts <- unique(data$context)
    panels <- stats::setNames(purrr::map(contexts, function(context_id) {
      data %>% dplyr::filter(context == context_id) %>% dplyr::select(-context)
    }), paste0("Panel ", LETTERS[seq_along(contexts)], ". ", contexts))
    write_report_panel_table(panels, file.path(output_dir, section, paste0(filename, ".tex")),
      caption, paste0("tab:rv-", filename), digits = 3, font_size = "scriptsize", tabcolsep = 3)
    # Four-panel tables need compact rows to fit the manuscript text height.
    path <- file.path(output_dir, section, paste0(filename, ".tex"))
    lines <- readLines(path, warn = FALSE)
    writeLines(gsub("{1.1}", "{1.0}", lines, fixed = TRUE), path)
    outputs[[section]] <<- c(outputs[[section]], paste0(filename, ".tex"))
  }
  primary <- bootstrap %>% dplyr::filter(block_length == 5) %>% labels()
  caption_method <- paste0("Each row compares the EW forecast ensemble with the stated benchmark within the same portfolio construction. ",
    "All returns are net of modeled factor-exposure costs. Raw and capped exposures are reported separately. ",
    "Brackets report pointwise 95 percent percentile intervals from ", draws,
    " paired stationary-bootstrap draws with expected block length five months. ",
    "The one-sided null is that the ensemble's performance difference is nonpositive. ",
    "Tests use centered bootstrap errors. Adjusted p-values use Holm's correction across Sharpe, mean and certainty-equivalent return comparisons in the stated family. ",
    "The evaluation period is July 2011 to December 2024. Inference is conditional on the fitted strategies.")
  for (family_id in c("single_factor_primary", "multifactor_primary")) {
    table <- primary %>% dplyr::filter(family == family_id, metric == "sharpe") %>%
      dplyr::transmute(context, Benchmark = comparator_label, Exposure = version_label,
        `Sharpe difference` = estimate,
        `95% interval` = sprintf("[%.3f, %.3f]", ci_lower, ci_upper),
        `p-value` = p_value, `Holm p-value` = p_holm)
    title <- if (family_id == "single_factor_primary") "Paired Sharpe comparisons for single-factor strategies." else
      "Paired Sharpe comparisons for multifactor strategies."
    family_size <- nrow(dplyr::filter(primary, family == family_id))
    write_panels(table, paste0("portfolio_inference_", family_id),
      paste(title, caption_method, "The adjustment family contains", family_size, "tests."), "main_appendix")
  }
  # Keep all endpoints, raw p-values and sensitivity results machine-readable.
  # Appendix tables emphasize the same tests rather than adding other methods.
  for (family_id in unique(primary$family)) {
    for (metric_id in c("mean_ann", "cer_ann")) {
      table <- primary %>% dplyr::filter(family == family_id, metric == metric_id) %>%
        dplyr::transmute(context, Benchmark = comparator_label, Exposure = version_label,
          `Difference (pp)` = 100 * estimate,
          `95% interval` = sprintf("[%.3f, %.3f]", 100 * ci_lower, 100 * ci_upper),
          `p-value` = p_value, `Holm p-value` = p_holm)
      # Standalone-market contexts include two allocators and three single-factor
      # families. Split only for page height, preserving the adjustment family.
      context_groups <- split(unique(table$context), ceiling(seq_along(unique(table$context)) / 4))
      purrr::iwalk(context_groups, function(contexts, page) {
        title <- if (metric_id == "cer_ann") "Paired certainty-equivalent return comparisons." else
          "Paired mean excess return comparisons."
        write_panels(dplyr::filter(table, context %in% contexts),
          paste0("portfolio_inference_", family_id, "_", metric_id, "_", page),
          paste(title, "Differences are annual percentage points. Certainty-equivalent returns use risk aversion five and net-return variance.",
                caption_method), "internet_appendix")
      })
    }
  }
  for (family_id in c("signed_cederburg", "standalone_market")) {
    table <- primary %>% dplyr::filter(family == family_id, metric == "sharpe") %>%
      dplyr::transmute(context, Benchmark = comparator_label, Exposure = version_label,
        `Sharpe difference` = estimate,
        `95% interval` = sprintf("[%.3f, %.3f]", ci_lower, ci_upper),
        `p-value` = p_value, `Holm p-value` = p_holm)
    context_groups <- split(unique(table$context), ceiling(seq_along(unique(table$context)) / 4))
    purrr::iwalk(context_groups, function(contexts, page) {
      write_panels(dplyr::filter(table, context %in% contexts),
        paste0("portfolio_inference_", family_id, "_sharpe_", page),
        paste("Additional paired Sharpe comparisons.", caption_method,
          "Unmanaged-only MVO optimizes factor and cash exposure. It is distinct from buy-and-hold."),
        "internet_appendix")
    })
  }
  # A standalone preview prevents accidental combination with older generated
  # portfolio tables when running this focused inference report by itself.
  relative_root <- "results_v3/1986"
  preview <- c("\\documentclass[11pt]{article}",
    "\\usepackage[a4paper,margin=18mm]{geometry}", "\\usepackage[T1]{fontenc}",
    "\\usepackage{longtable,caption,pdflscape}", "\\usepackage[table]{xcolor}",
    "\\DeclareCaptionLabelFormat{boldlabel}{\\textbf{#1 #2}}", "\\begin{document}",
    "\\section*{Portfolio performance inference}",
    "July 2011--December 2024. Sharpe ratios and certainty-equivalent returns use monthly net excess returns. The 153-factor aggregate equal-weights nonmarket factor returns within each month. Multifactor allocations include the market as a 154th factor.",
    unlist(purrr::imap(outputs, function(files, section) {
      c("\\clearpage", paste0("\\section*{", gsub("_", " ", section), "}"),
        paste0("\\input{", relative_root, "/", section, "/", files, "}"))
    }), use.names = FALSE), "\\end{document}")
  writeLines(preview, file.path(output_dir, "portfolio_inference_preview.tex"))
  invisible(outputs)
}
