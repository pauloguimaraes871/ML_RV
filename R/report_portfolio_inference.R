# Inference for completed portfolio backtests ----------------------------------
# Run independently with source('R/report_portfolio_inference.R'), followed by
# report_portfolio_inference(). report_results.R uses the same entry point.
# This report deliberately uses only stationary blocks and HAC spanning alpha.
# Define the descriptive helpers in the same environment as their dependencies,
# including when this file is sourced from a clean, noninteractive R session.
source(here::here("R", "reporting_helpers.R"), local = TRUE)

#' Summarize saved single-factor allocation implementations
#'
#' @param data Monthly aggregate returns and implementation quantities returned
#'   by read_primary_portfolio_inference().
#' @param gamma Risk aversion used for annualized certainty-equivalent returns.
#' @return One row per portfolio family, scope, exposure version and model.
summarise_single_factor_implementation <- function(data, gamma = 5) {
  groups <- c("family", "scope", "version", "model")
  required <- c(groups, "month_id", "gross_return", "excess_return",
                "total_return", "turnover", "cost", "exposure", "n_factors")
  if (!is.data.frame(data) || !nrow(data) || !all(required %in% names(data)) ||
      anyNA(data[required]) || anyDuplicated(data[c(groups, "month_id")]) ||
      any(!purrr::map_lgl(data[c("gross_return", "excess_return", "total_return",
                                  "turnover", "cost", "exposure")], is.numeric)) ||
      any(!is.finite(as.matrix(data[c("gross_return", "excess_return", "total_return",
                                      "turnover", "cost", "exposure")]))) ||
      any(data$turnover < 0) || any(data$cost < 0) ||
      any(abs(data$gross_return - data$cost - data$excess_return) > 1e-12)) {
    rlang::abort("Single-factor implementation data are incomplete or inconsistent.")
  }
  net <- summarise_portfolio_returns(data, groups, cer_gamma = gamma) %>%
    dplyr::select(dplyr::all_of(groups),
      net_return = annualized_excess_return, net_volatility = annualized_volatility,
      net_sharpe = sharpe_ratio, net_cer = cer_ann, net_max_drawdown = max_drawdown)
  gross <- data %>%
    dplyr::mutate(gross_total_return = gross_return + total_return - excess_return) %>%
    summarise_portfolio_returns(groups, excess_col = "gross_return",
                                total_col = "gross_total_return", cer_gamma = gamma) %>%
    dplyr::select(dplyr::all_of(groups), gross_return = annualized_excess_return,
                  gross_sharpe = sharpe_ratio)
  implementation <- data %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(groups))) %>%
    dplyr::summarise(annualized_cost = 12 * mean(cost),
      average_turnover = mean(turnover), mean_exposure = mean(exposure),
      n_factors = dplyr::first(n_factors), .groups = "drop")
  net %>%
    dplyr::left_join(gross, by = groups, relationship = "one-to-one") %>%
    dplyr::left_join(implementation, by = groups, relationship = "one-to-one")
}

#' Write descriptive Cederburg-style performance tables
#'
#' @param statistics Output of summarise_single_factor_implementation().
#' @param output_dir Paper-results root.
#' @param project_dir Repository root, used to source the table helper.
#' @return Output registry for the main text and Internet Appendix.
write_cederburg_descriptive_tables <- function(statistics, output_dir,
                                                project_dir = here::here()) {
  source(file.path(project_dir, "R", "reporting_helpers.R"), local = TRUE)
  model_order <- c("buyhold", "unmanaged_mvo_ew", "rw", "har", "harx", "ew")
  model_labels <- c(buyhold = "Unmanaged", unmanaged_mvo_ew = "Unmanaged-only MVO",
    rw = "Random walk", har = "HAR", harx = "HARX", ew = "EW forecast ensemble")
  scope_order <- c("market_pooled", "aggregate_153", "market_separate")
  scope_labels <- c(market_pooled = "Market portfolio, pooled estimation",
    aggregate_153 = "Equal-weighted 153-factor aggregate",
    market_separate = "Market portfolio, separate estimation")
  format_table <- function(data, include_diagnostics = FALSE) {
    data %>%
      dplyr::mutate(
        scope = factor(scope, levels = scope_order),
        model = factor(model, levels = model_order),
        version = factor(version, levels = c("raw", "capped")),
        Model = unname(model_labels[as.character(model)])) %>%
      dplyr::arrange(scope, model, version) %>%
      dplyr::transmute(Model,
        `Return (%)` = 100 * net_return, `Vol. (%)` = 100 * net_volatility,
        `Gross SR` = gross_sharpe, `Net SR` = net_sharpe,
        `CER (%)` = 100 * net_cer,
        !!!if (include_diagnostics) rlang::exprs(`MDD (%)` = 100 * net_max_drawdown)
            else rlang::exprs(),
        Turnover = average_turnover, `Cost (%)` = 100 * annualized_cost,
        !!!if (include_diagnostics) rlang::exprs(`Mean exposure` = mean_exposure)
            else rlang::exprs())
  }
  make_panels <- function(data, scopes, include_diagnostics = FALSE) {
    stats::setNames(purrr::map(scopes, function(scope_id) {
      format_table(dplyr::filter(data, scope == scope_id), include_diagnostics)
    }), paste0("Panel ", LETTERS[seq_along(scopes)], ". ", unname(scope_labels[scopes])))
  }
  main_scopes <- c("market_pooled", "aggregate_153")
  main_data <- statistics %>%
    dplyr::filter(family == "ced_ew", version == "capped", scope %in% main_scopes,
                  model %in% c("buyhold", "rw", "har", "harx", "ew"))
  main_name <- "portfolio_performance_cederburg_ew"
  write_report_panel_table(make_panels(main_data, main_scopes),
    file.path(output_dir, "main", paste0(main_name, ".tex")),
    paste(
      "Economic performance of fixed 50/50 unmanaged and volatility-managed combinations.",
      "Each managed row assigns one-half to the unmanaged factor and one-half to the volatility-managed factor.",
      "The EW row refers to the equal-weighted forecast ensemble.",
      "Returns, volatility, certainty-equivalent return, and modeled costs are annualized percentages.",
      "Gross and net Sharpe ratios use excess returns. Net results deduct 10 basis points per unit change in total factor exposure.",
      "Turnover is the average monthly absolute exposure change after applying the bound of five.",
      "Panel B first equal-weights the 153 factor-strategy returns within each month."
    ), paste0("tab:rv-", main_name), digits = 3, font_size = "scriptsize",
    tabcolsep = 2.2, placement = "htbp")

  appendix_scopes <- c("market_pooled", "aggregate_153", "market_separate")
  appendix_data <- statistics %>%
    dplyr::filter(family == "ced_mvo", version == "capped", scope %in% appendix_scopes,
                  model %in% c("unmanaged_mvo_ew", "rw", "har", "harx", "ew"))
  appendix_name <- "portfolio_performance_cederburg_mvo"
  write_report_panel_table(make_panels(appendix_data, appendix_scopes, TRUE),
    file.path(output_dir, "internet_appendix", paste0(appendix_name, ".tex")),
    paste(
      "Economic performance of signed two-sleeve mean-variance portfolios.",
      "Managed rows estimate allocations to the unmanaged factor, its volatility-managed counterpart, and risk-free collateral over the expanding training window.",
      "Unmanaged-only MVO estimates the factor and collateral allocation with the managed-sleeve coefficient set to zero and is distinct from buy-and-hold.",
      "The reported implementation restricts total factor exposure to the interval from minus five to five. Raw results remain available in the accompanying inference tables and machine-readable output.",
      "Return, volatility, certainty-equivalent return, maximum drawdown, and modeled costs are reported in percent. Net results deduct 10 basis points per unit change in total factor exposure. Mean exposure is the average bounded total factor exposure."
    ), paste0("tab:rv-", appendix_name), digits = 3, font_size = "scriptsize",
    tabcolsep = 2.2, landscape = TRUE)
  list(main = paste0(main_name, ".tex"),
       internet_appendix = paste0(appendix_name, ".tex"))
}

#' Read completed multifactor implementations for the main performance table
#'
#' @param project_dir Repository root containing the primary saved allocations.
#' @param sample_id Saved model vintage, 1986 or 1927.
#' @return Monthly gross and net excess returns, costs, turnover, and exposure.
#'   UMV is read once per allocator. All rows use the saved factor-level cost
#'   convention and the chosen sample's balanced holding calendar.
read_multifactor_descriptive_inputs <- function(project_dir = here::here(),
                                                sample_id = c("1986", "1927")) {
  sample_id <- match.arg(sample_id)
  allocation_dir <- file.path(project_dir, "ports", "v3", sample_id, "allocations",
    "expanding_vol_10bps_60m_alloc_60_60_60m")
  first_month <- if (sample_id == "1986") "2011-07-01" else "1980-01-01"
  dates <- seq(as.Date(first_month), as.Date("2024-12-01"), by = "month")
  specifications <- tibble::tribble(
    ~specification, ~forecast, ~method, ~model,
    "cmv_panel", "ew", "umv", "umv",
    "lagged_market", "ew", "cmv", "lagged_market",
    "cmv_panel", "har", "cmv", "cmv_panel_har",
    "cmv_panel", "harx", "cmv", "cmv_panel_harx",
    "cmv_panel", "ew", "cmv", "cmv_panel_ew",
    "fcmv", "har", "fcmv", "fcmv_har",
    "fcmv", "harx", "fcmv", "fcmv_harx",
    "fcmv", "ew", "fcmv", "fcmv_ew"
  )
  purrr::map_dfr(c("ew", "hew"), function(allocator) {
    purrr::pmap_dfr(specifications, function(specification, forecast, method, model) {
      path <- file.path(allocation_dir,
        paste0("multifactor_", forecast, "_", specification, "_", allocator, ".rds"))
      if (!file.exists(path)) rlang::abort(paste("Missing completed multifactor portfolio:", path))
      saved <- readRDS(path)
      settings <- saved$settings
      if (!identical(settings$allocation, allocator) ||
          !identical(settings$managed_normalization, "training_vol") ||
          !isTRUE(all.equal(settings$exposure_cap, 1.5)) ||
          !isTRUE(all.equal(settings$cost_per_unit, .001))) {
        rlang::abort(paste("Unexpected multifactor portfolio settings:", path))
      }
      x <- saved$monthly_returns %>% dplyr::filter(.data$method == .env$method) %>%
        dplyr::arrange(month_id)
      if (nrow(x) != length(dates) || !identical(x$month_id, dates)) {
        rlang::abort(paste("Unexpected multifactor holding months:", path))
      }
      purrr::map_dfr(c("raw", "capped"), function(version) {
        gross <- paste0("gross_", version)
        net <- paste0("net_", version)
        x %>% dplyr::transmute(
          family = paste0("multi_", allocator), scope = "all_154", version, model,
          month_id, gross_return = .data[[gross]], excess_return = .data[[net]],
          total_return = .data[[net]] + rf,
          turnover = .data[[paste0("turnover_", version)]],
          cost = .data[[paste0("tc_", version)]],
          exposure = .data[[paste0("exposure_", version)]], n_factors = 154L)
      })
    })
  })
}

#' Write economic performance of the primary multifactor portfolios
#'
#' @param statistics Output of summarise_single_factor_implementation() for
#'   completed multifactor monthly returns.
#' @param output_dir Paper-results root.
#' @param project_dir Repository root, used to source the existing table helper.
#' @return Output registry for the main manuscript.
write_multifactor_descriptive_table <- function(statistics, output_dir,
                                                project_dir = here::here()) {
  source(file.path(project_dir, "R", "reporting_helpers.R"), local = TRUE)
  model_order <- c("umv", "lagged_market", "cmv_panel_har", "cmv_panel_harx",
                   "cmv_panel_ew", "fcmv_har", "fcmv_harx", "fcmv_ew")
  model_labels <- c(umv = "UMV", lagged_market = "CMV, lagged market RV",
    cmv_panel_har = "CMV, HAR", cmv_panel_harx = "CMV, HARX",
    cmv_panel_ew = "CMV, EW forecast ensemble", fcmv_har = "FCMV, HAR",
    fcmv_harx = "FCMV, HARX", fcmv_ew = "FCMV, EW forecast ensemble")
  primary <- statistics %>% dplyr::filter(version == "capped",
    family %in% c("multi_ew", "multi_hew"), model %in% model_order)
  if (nrow(primary) != 2L * length(model_order) ||
      anyDuplicated(primary[c("family", "model")]) ||
      any(primary$n_factors != 154L)) {
    rlang::abort("Unexpected primary multifactor performance coverage.")
  }
  panels <- stats::setNames(purrr::map(c("multi_ew", "multi_hew"), function(allocator) {
    primary %>% dplyr::filter(family == allocator) %>%
      dplyr::mutate(model = factor(model, levels = model_order)) %>%
      dplyr::arrange(model) %>%
      dplyr::transmute(Model = unname(model_labels[as.character(model)]),
        `Return (%)` = 100 * net_return, `Vol. (%)` = 100 * net_volatility,
        `Gross SR` = gross_sharpe, `Net SR` = net_sharpe,
        `CER (%)` = 100 * net_cer, Turnover = average_turnover,
        `Cost (%)` = 100 * annualized_cost)
  }), c("Panel A. Equal-weight allocation", "Panel B. Hierarchical equal-weight allocation"))
  table_name <- "portfolio_performance_multifactor"
  write_report_panel_table(panels,
    file.path(output_dir, "main", paste0(table_name, ".tex")),
    paste(
      "Economic performance of multifactor volatility-managed portfolios.",
      "Each allocator combines 154 factors, including the market portfolio.",
      "UMV holds unmanaged factors. CMV conditions managed sleeves on market volatility, while FCMV uses each factor's own volatility.",
      "Lagged market RV is a CMV benchmark. EW denotes the equal-weighted forecast ensemble, not the allocation rule.",
      "Returns, volatility, certainty-equivalent return, and modeled costs are annualized percentages. Gross and net Sharpe ratios use excess returns.",
      "The table reports exposure capped at 1.5. Net results deduct 10 basis points per unit change in target factor exposure. Turnover is the monthly sum of absolute changes in factor exposures, averaged over time."
    ), paste0("tab:rv-", table_name), digits = 3, font_size = "scriptsize",
    tabcolsep = 2.2, placement = "htbp")
  list(main = paste0(table_name, ".tex"))
}

#' Write single-factor spanning-regression diagnostics
#'
#' @param spanning Saved-output spanning regressions from report_portfolio_inference().
#' @param output_dir Paper-results root.
#' @param project_dir Repository root, used to source the table helper.
#' @return Output registry for the Internet Appendix.
#'
#' The table is deliberately descriptive. A spanning alpha describes the ex post
#' opportunity set generated by adding the managed return to the benchmark; it is
#' not evidence that the managed strategy is a feasible superior allocation.
write_single_factor_spanning_table <- function(spanning, output_dir,
                                               project_dir = here::here()) {
  source(file.path(project_dir, "R", "reporting_helpers.R"), local = TRUE)
  required <- c("family", "portfolio_family", "scope", "version", "forecast",
                "comparator", "hac_lag", "alpha_ann", "t_alpha", "p_alpha",
                "beta", "r_squared", "appraisal_ratio")
  if (!is.data.frame(spanning) || !all(required %in% names(spanning))) {
    rlang::abort("Spanning table requires complete single-factor diagnostics.")
  }

  scope_order <- c("aggregate_153", "market_pooled")
  scope_labels <- c(aggregate_153 = "153-factor aggregate",
                    market_pooled = "Pooled market")
  format_panel <- function(data) {
    data %>%
      dplyr::mutate(
        scope = factor(scope, levels = scope_order),
        version = factor(version, levels = c("capped", "raw")),
        Scope = unname(scope_labels[as.character(scope)]),
        Exposure = dplyr::recode(as.character(version), capped = "Capped", raw = "Raw")) %>%
      dplyr::arrange(scope, version) %>%
      dplyr::transmute(Scope, Exposure, `Alpha (%)` = 100 * alpha_ann,
        `t(alpha)` = t_alpha, `p-value` = p_alpha, Beta = beta,
        `R2` = r_squared, `Appraisal ratio` = appraisal_ratio)
  }

  primary <- spanning %>%
    dplyr::filter(family == "single_factor_primary", forecast == "ew",
                  comparator == "buyhold", hac_lag == 1L,
                  portfolio_family %in% c("ced_ew", "mm"),
                  scope %in% scope_order)
  fixed_combination <- primary %>%
    dplyr::filter(portfolio_family == "ced_ew", version == "capped")
  mm_management <- primary %>% dplyr::filter(portfolio_family == "mm")
  if (nrow(fixed_combination) != length(scope_order) ||
      nrow(mm_management) != length(scope_order) * 2L ||
      anyDuplicated(fixed_combination[c("scope", "version")]) ||
      anyDuplicated(mm_management[c("scope", "version")])) {
    rlang::abort("Unexpected primary spanning-regression coverage.")
  }

  table_name <- "portfolio_spanning_single_factor"
  write_report_panel_table(
    panels = list(
      "Panel A. Fixed 50/50 combination" = format_panel(fixed_combination),
      "Panel B. Moreira--Muir management" = format_panel(mm_management)
    ),
    path = file.path(output_dir, "internet_appendix", paste0(table_name, ".tex")),
    caption = paste(
      "Spanning regressions for net single-factor portfolios relative to unmanaged exposure.",
      "Each row regresses the EW forecast-ensemble portfolio return on the corresponding unmanaged return over its full available evaluation history.",
      "Panel A reports the fixed 50/50 combination at the capped implementation. Panel B reports Moreira--Muir management at both exposure implementations.",
      "Alpha is annualized and reported in percent. The intercept t-statistic and p-value use Newey--West standard errors with one lag.",
      "The appraisal ratio is annualized alpha divided by the residual standard deviation.",
      "These regressions describe the ex post opportunity set and do not establish implementable allocation gains."
    ),
    label = paste0("tab:rv-", table_name), digits = 3,
    font_size = "scriptsize", tabcolsep = 2.2, placement = "htbp"
  )
  list(internet_appendix = paste0(table_name, ".tex"))
}

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
    output_dir = NULL,
    draws = 10000L, seed = 20260918L, sample_id = c("1986", "1927")) {
  sample_id <- match.arg(sample_id)
  if (is.null(output_dir)) output_dir <- file.path(project_dir, "latex_paper", "results_v3", sample_id)
  source(file.path(project_dir, "R", "portfolio_inference_helpers.R"), local = TRUE)
  source(file.path(project_dir, "R", "portfolio_inference_inputs.R"), local = TRUE)
  source(file.path(project_dir, "R", "reporting_helpers.R"), local = TRUE)
  code <- file.path(project_dir, "R", c("portfolio_inference_helpers.R", "portfolio_inference_inputs.R",
                                        "report_portfolio_inference.R", "reporting_helpers.R"))
  code_manifest <- tibble::tibble(file = code,
    sha256 = purrr::map_chr(code, ~ digest::digest(file = .x, algo = "sha256")))
  for (folder in c("data", "main", "main_appendix", "internet_appendix")) {
    dir.create(file.path(output_dir, folder), recursive = TRUE, showWarnings = FALSE)
  }
  save_data <- function(x, name) utils::write.csv(x,
    file.path(output_dir, "data", paste0("portfolio_inference_", name, ".csv")), row.names = FALSE)
  message("Checking completed portfolios, matching benchmarks and provenance.")
  inputs <- read_primary_portfolio_inference(project_dir, sample_id)
  # Market series retain their longer history. All contrasts sharing a calendar
  # receive identical month draws, including every factor and forecast model.
  calendars <- if (sample_id == "1986") list(common = inputs$returns) else
    split(inputs$returns, ifelse(inputs$returns$scope %in%
      c("market_pooled", "market_separate"), "market", "balanced"))
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
  single_factor_statistics <- summarise_single_factor_implementation(inputs$implementation)
  save_data(single_factor_statistics, "single_factor_descriptive")
  descriptive_outputs <- write_cederburg_descriptive_tables(
    single_factor_statistics, output_dir, project_dir)
  ## Multifactor monthly returns have the same gross/net/cost accounting fields,
  ## so reuse the single-factor summary rather than introduce another metric rule.
  multifactor_statistics <- summarise_single_factor_implementation(
    read_multifactor_descriptive_inputs(project_dir, sample_id))
  save_data(multifactor_statistics, "multifactor_descriptive")
  multifactor_outputs <- write_multifactor_descriptive_table(
    multifactor_statistics, output_dir, project_dir)

  ## Identical month draws enter every strategy in every reporting scope. In
  ## particular, aggregate-factor inference never samples factors independently.
  ## Primary results use L=5. L=3 and L=10 are sensitivity analyses, not additional
  ## opportunities to select a favorable p-value.
  bootstrap <- purrr::map_dfr(c(5, 3, 10), function(block_length) {
    message("Paired stationary bootstrap: ", draws, " draws, expected block length ", block_length, ".")
    purrr::imap_dfr(calendars, function(rows, calendar_name) {
    aligned <- align_portfolio_inference(rows)
    calendar <- draw_portfolio_months(aligned$dates, draws, block_length, seed)
    saveRDS(calendar, file.path(output_dir, "data",
      paste0("portfolio_inference_calendar_", calendar_name, "_", block_length, "m.rds")))
    contrasts <- dplyr::filter(inputs$contrasts, strategy %in% rows$series_id)
    infer_portfolio_pairs(rows, contrasts, calendar) %>%
      dplyr::left_join(dplyr::select(inputs$contrasts, comparison_id, portfolio_family,
                                   scope, version, forecast, comparator),
        by = "comparison_id", relationship = "many-to-one")
    })
  }) %>% dplyr::group_by(block_length, family) %>%
    dplyr::mutate(p_holm = stats::p.adjust(p_value, "holm"),
                  p_bh = stats::p.adjust(p_value, "BH")) %>% dplyr::ungroup()
  save_data(bootstrap, "bootstrap")
  spanning <- purrr::map_dfr(c(1L, 5L, 10L), function(lag) {
    purrr::pmap_dfr(inputs$contrasts, function(comparison_id, family, portfolio_family,
      scope, version, forecast, comparator, strategy, benchmark) {
      data <- inputs$returns %>% dplyr::filter(series_id == strategy) %>%
        dplyr::select(month_id, excess_return) %>%
        dplyr::inner_join(inputs$returns %>% dplyr::filter(series_id == benchmark) %>%
          dplyr::select(month_id, benchmark_return = excess_return),
          by = "month_id", relationship = "one-to-one") %>% dplyr::arrange(month_id)
      span_portfolio_returns(data, lag) %>% dplyr::mutate(comparison_id, family,
        portfolio_family, scope, version, forecast, comparator)
    })
  })
  save_data(spanning, "spanning")
  spanning_outputs <- write_single_factor_spanning_table(spanning, output_dir, project_dir)

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

  period_note <- if (sample_id == "1986") "The evaluation period is July 2011 to December 2024." else
    "Market strategies span June 1952 to December 2024. Balanced factor and multifactor portfolios span January 1980 to December 2024."
  inference_outputs <- write_portfolio_inference_tables(bootstrap, output_dir, project_dir,
    period_note = period_note)
  output_sections <- Reduce(union, list(names(descriptive_outputs), names(multifactor_outputs),
                                        names(inference_outputs), names(spanning_outputs)))
  outputs <- stats::setNames(purrr::map(output_sections, function(section) {
    unique(c(descriptive_outputs[[section]], multifactor_outputs[[section]],
             inference_outputs[[section]], spanning_outputs[[section]]))
  }), output_sections)
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
#' @param period_note Sentence describing the evaluation period in captions.
#' @param supplementary Whether to also render mean and CER appendix tables.
#' @return Invisible output registry. This function does not resample returns.
write_portfolio_inference_tables <- function(bootstrap, output_dir, project_dir = here::here(),
                                             period_note = "The evaluation period is July 2011 to December 2024.",
                                             supplementary = TRUE) {
  source(file.path(project_dir, "R", "reporting_helpers.R"), local = TRUE)
  # BH and Holm use the same prespecified family, never only the displayed
  # Sharpe rows. Sample vintages remain separate reporting families.
  bootstrap <- bootstrap %>% dplyr::group_by(block_length, family) %>%
    dplyr::mutate(p_bh = stats::p.adjust(p_value, "BH")) %>% dplyr::ungroup()
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
    "Tests use centered bootstrap errors. Holm and Benjamini-Hochberg adjustments use the same family of Sharpe, mean and certainty-equivalent return comparisons. ",
    period_note, " Inference is conditional on the fitted strategies.")
  for (family_id in c("single_factor_primary", "multifactor_primary")) {
    table <- primary %>% dplyr::filter(family == family_id, metric == "sharpe") %>%
      dplyr::transmute(context, Benchmark = comparator_label, Exposure = version_label,
        `Sharpe difference` = estimate,
        `95% interval` = sprintf("[%.3f, %.3f]", ci_lower, ci_upper),
        `p-value` = p_value, `Holm p-value` = p_holm, `BH p-value` = p_bh)
    title <- if (family_id == "single_factor_primary") "Paired Sharpe comparisons for single-factor strategies." else
      "Paired Sharpe comparisons for multifactor strategies."
    family_size <- nrow(dplyr::filter(primary, family == family_id))
    write_panels(table, paste0("portfolio_inference_", family_id),
      paste(title, caption_method, "The adjustment family contains", family_size, "tests."), "main_appendix")
  }
  # Keep all endpoints, raw p-values and sensitivity results machine-readable.
  # Appendix tables emphasize the same tests rather than adding other methods.
  # The paper reports supplementary mean and CER comparisons only for the
  # primary single-factor and multifactor designs. Signed MVO and standalone
  # market inference remain available in the machine-readable results.
  if (supplementary) for (family_id in c("single_factor_primary", "multifactor_primary")) {
    for (metric_id in c("mean_ann", "cer_ann")) {
      table <- primary %>% dplyr::filter(family == family_id, metric == metric_id) %>%
        dplyr::transmute(context, Benchmark = comparator_label, Exposure = version_label,
          `Difference (pp)` = 100 * estimate,
          `95% interval` = sprintf("[%.3f, %.3f]", 100 * ci_lower, 100 * ci_upper),
          `p-value` = p_value, `Holm p-value` = p_holm)
      # Split large tables for page height without changing the adjustment family.
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
  # A standalone preview prevents accidental combination with older generated
  # portfolio tables when running this focused inference report by itself.
  relative_root <- paste0("results_v3/", basename(output_dir))
  preview <- c("\\documentclass[11pt]{article}",
    "\\usepackage[a4paper,margin=18mm]{geometry}", "\\usepackage[T1]{fontenc}",
    "\\usepackage{longtable,caption,pdflscape}", "\\usepackage[table]{xcolor}",
    "\\DeclareCaptionLabelFormat{boldlabel}{\\textbf{#1 #2}}", "\\begin{document}",
    "\\section*{Portfolio performance inference}",
    paste(period_note, "Sharpe ratios and certainty-equivalent returns use monthly net excess returns. The 153-factor aggregate equal-weights nonmarket factor returns within each month. Multifactor allocations include the market as a 154th factor."),
    unlist(purrr::imap(outputs, function(files, section) {
      c("\\clearpage", paste0("\\section*{", gsub("_", " ", section), "}"),
        paste0("\\input{", relative_root, "/", section, "/", files, "}"))
    }), use.names = FALSE), "\\end{document}")
  writeLines(preview, file.path(output_dir, "portfolio_inference_preview.tex"))
  invisible(outputs)
}
