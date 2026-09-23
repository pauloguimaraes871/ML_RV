# Full-sample portfolio sensitivity for the Internet Appendix-----------------

## These helpers consume saved target exposures. They never refit forecasts or
## reconstruct portfolio normalizers.

#' Recompute portfolio costs over a common evaluation period
#'
#' Restrict saved target exposures to a common date range and apply the original
#' factor-exposure cost rule from a stated initial position. This permits a fair
#' comparison of saved runs with different native starting dates without
#' reconstructing forecasts, normalizers, or target exposures.
rebase_portfolio_costs <- function(data, evaluation_start,
                                   evaluation_end = NULL,
                                   cost_per_unit = .001,
                                   initial_weight = 0) {
  required <- c("universe", "strategy", "name", "month_id", "factor_exc",
                "rf", "w_raw", "w_capped")
  if (!is.data.frame(data) || !all(required %in% names(data))) {
    rlang::abort(paste("Portfolio cost rebasing requires:",
                       paste(required, collapse = ", ")))
  }
  if (!inherits(evaluation_start, "Date") || length(evaluation_start) != 1L ||
      is.na(evaluation_start)) {
    rlang::abort("evaluation_start must be one nonmissing Date.")
  }
  if (!is.null(evaluation_end) &&
      (!inherits(evaluation_end, "Date") || length(evaluation_end) != 1L ||
       is.na(evaluation_end) || evaluation_end < evaluation_start)) {
    rlang::abort("evaluation_end must be NULL or one Date on or after evaluation_start.")
  }
  for (value in c(cost_per_unit, initial_weight)) {
    if (!is.numeric(value) || length(value) != 1L || !is.finite(value) || value < 0) {
      rlang::abort("Cost and initial-exposure inputs must be finite nonnegative scalars.")
    }
  }
  numeric_columns <- c("factor_exc", "rf", "w_raw", "w_capped")
  if (!inherits(data$month_id, "Date") || anyNA(data[required]) ||
      any(!purrr::map_lgl(data[numeric_columns], is.numeric)) ||
      any(!is.finite(as.matrix(data[numeric_columns])))) {
    rlang::abort("Portfolio dates, exposures, and returns must be complete and finite.")
  }
  keys <- c("universe", "strategy", "name", "month_id")
  if (anyDuplicated(data[keys])) {
    rlang::abort("Portfolio evaluation keys must be unique.")
  }

  result <- data %>%
    dplyr::filter(month_id >= evaluation_start) %>%
    { if (is.null(evaluation_end)) . else dplyr::filter(., month_id <= evaluation_end) }
  if (!nrow(result)) rlang::abort("Common-period portfolio evaluation is empty.")

  result %>%
    dplyr::arrange(universe, strategy, name, month_id) %>%
    dplyr::group_by(universe, strategy, name) %>%
    dplyr::mutate(
      turnover_raw = abs(w_raw - dplyr::lag(w_raw, default = initial_weight)),
      turnover = abs(w_capped - dplyr::lag(w_capped, default = initial_weight)),
      tc_raw = cost_per_unit * turnover_raw,
      tc = cost_per_unit * turnover,
      r_exc_fwd1_raw = w_raw * factor_exc,
      r_exc_fwd1_capped = w_capped * factor_exc,
      r_exc_fwd1_net_raw = r_exc_fwd1_raw - tc_raw,
      r_exc_fwd1_net_capped = r_exc_fwd1_capped - tc,
      r_strat_fwd1_raw = rf + r_exc_fwd1_raw,
      r_strat_fwd1_capped = rf + r_exc_fwd1_capped,
      r_strat_fwd1_net_raw = rf + r_exc_fwd1_net_raw,
      r_strat_fwd1_net_capped = rf + r_exc_fwd1_net_capped
    ) %>%
    dplyr::ungroup()
}

#' Build a Table 6-style sensitivity from saved full-sample normalizers
#'
#' Reads only the pooled panel artifact because both reported scopes use pooled
#' forecasts. The full-sample normalizer remains an ex-post sensitivity. Returns
#' panels with the same statistics and row ordering as the primary table.
build_full_sample_portfolio_panels <- function(portfolio_path, factor_names,
                                               evaluation_start,
                                               evaluation_end = NULL) {
  if (!is.character(portfolio_path) || length(portfolio_path) != 1L ||
      !file.exists(portfolio_path)) {
    rlang::abort("The saved full-sample portfolio artifact is missing.")
  }
  if (!is.character(factor_names) || !length(factor_names) ||
      anyNA(factor_names) || anyDuplicated(factor_names) ||
      "spx_vw" %in% factor_names) {
    rlang::abort("factor_names must uniquely identify the nonmarket factors.")
  }

  models <- readRDS(portfolio_path)
  model_order <- c("glmnet", "har", "harx", "nn1", "nn2", "nn3", "rf", "xgb", "ew")
  if (!is.list(models) || !setequal(names(models), model_order)) {
    rlang::abort("Unexpected models in the full-sample portfolio artifact.")
  }
  reference_settings <- models$ew$settings
  purrr::iwalk(models, function(result, model) {
    if (!is.list(result) || !is.data.frame(result$w_compiled_df) ||
        !identical(result$settings, reference_settings)) {
      rlang::abort(paste("Invalid full-sample portfolio result for", model))
    }
  })
  if (!identical(reference_settings$normalization, "full_sample") ||
      isTRUE(reference_settings$implementable) ||
      reference_settings$min_history != 120L ||
      !isTRUE(all.equal(reference_settings$weight_cap, 1.5)) ||
      !isTRUE(all.equal(reference_settings$cost_per_unit, .001)) ||
      !isTRUE(all.equal(reference_settings$initial_weight, 0))) {
    rlang::abort("Unexpected design in the full-sample portfolio artifact.")
  }
  if (!setequal(unique(models$ew$w_compiled_df$name), c(factor_names, "spx_vw"))) {
    rlang::abort("Full-sample portfolio names do not match the reporting universe.")
  }

  strategy_order <- c("unmanaged", "rw", "har", "harx", "ew")
  strategy_labels <- c(
    unmanaged = "Unmanaged", rw = "Random walk", har = "HAR",
    harx = "HARX", ew = "EW"
  )
  select_strategies <- function(names_to_keep) {
    dplyr::bind_rows(
      models$ew$w_compiled_df %>%
        dplyr::filter(method == "buyhold", name %in% names_to_keep) %>%
        dplyr::mutate(strategy = "unmanaged"),
      models$ew$w_compiled_df %>%
        dplyr::filter(method == "rw", name %in% names_to_keep) %>%
        dplyr::mutate(strategy = "rw"),
      purrr::map_dfr(c("har", "harx", "ew"), function(model) {
        models[[model]]$w_compiled_df %>%
          dplyr::filter(method == "vol_scaled", name %in% names_to_keep) %>%
          dplyr::mutate(strategy = model)
      })
    )
  }
  returns <- dplyr::bind_rows(
    select_strategies("spx_vw") %>% dplyr::mutate(universe = "market_pooled"),
    select_strategies(factor_names) %>% dplyr::mutate(universe = "factor_panel")
  ) %>%
    rebase_portfolio_costs(
      evaluation_start = evaluation_start,
      evaluation_end = evaluation_end,
      cost_per_unit = reference_settings$cost_per_unit,
      initial_weight = reference_settings$initial_weight
    )
  coverage <- returns %>%
    dplyr::group_by(universe, strategy, name) %>%
    dplyr::summarise(start = min(month_id), end = max(month_id),
                     n = dplyr::n(), .groups = "drop")
  if (any(coverage$start != evaluation_start) ||
      (!is.null(evaluation_end) && any(coverage$end != evaluation_end)) ||
      dplyr::n_distinct(coverage$n) != 1L) {
    rlang::abort("Full-sample strategies do not share the requested common period.")
  }

  return_versions <- tibble::tribble(
    ~version, ~excess_column, ~total_column,
    "raw", "r_exc_fwd1_raw", "r_strat_fwd1_raw",
    "capped", "r_exc_fwd1_capped", "r_strat_fwd1_capped",
    "net_raw", "r_exc_fwd1_net_raw", "r_strat_fwd1_net_raw",
    "net_capped", "r_exc_fwd1_net_capped", "r_strat_fwd1_net_capped"
  )
  versions <- purrr::pmap_dfr(return_versions, function(version, excess_column, total_column) {
    returns %>%
      dplyr::mutate(version = version, excess_return = .data[[excess_column]],
                    total_return = .data[[total_column]])
  })
  aggregated <- aggregate_portfolio_returns(
    versions, c("universe", "strategy", "version"),
    c("excess_return", "total_return", "factor_exc", "w_raw")
  )
  summary <- summarise_portfolio_returns(
    aggregated, c("universe", "strategy", "version"),
    unmanaged_col = "factor_exc", weight_col = "w_raw"
  )
  table <- summary %>%
    dplyr::select(universe, strategy, version, sharpe_ratio, cer_ann,
                  vol_ratio, avg_weight_raw) %>%
    tidyr::pivot_longer(
      c(sharpe_ratio, cer_ann, vol_ratio, avg_weight_raw),
      names_to = "statistic", values_to = "value"
    ) %>%
    dplyr::mutate(
      universe = factor(universe, levels = c("market_pooled", "factor_panel")),
      strategy = factor(strategy, levels = strategy_order),
      statistic = factor(
        statistic,
        levels = c("sharpe_ratio", "cer_ann", "vol_ratio", "avg_weight_raw")
      ),
      value = ifelse(statistic == "cer_ann", 100 * value, value),
      Model = strategy_labels[as.character(strategy)],
      Statistic = dplyr::case_when(
        statistic == "sharpe_ratio" ~ "Sharpe ratio",
        statistic == "cer_ann" ~ "CER (%)",
        statistic == "vol_ratio" ~ "Volatility ratio",
        statistic == "avg_weight_raw" ~ "Mean raw exposure"
      )
    ) %>%
    dplyr::select(universe, strategy, statistic, Model, Statistic, version, value) %>%
    tidyr::pivot_wider(names_from = version, values_from = value) %>%
    dplyr::arrange(universe, strategy, statistic) %>%
    dplyr::select(universe, Model, Statistic, Raw = raw, Capped = capped,
                  `Net raw` = net_raw, `Net capped` = net_capped)

  universe_labels <- c(
    market_pooled = "Market portfolio, pooled estimation",
    factor_panel = "Equal-weighted factor panel"
  )
  stats::setNames(
    purrr::map(names(universe_labels), ~ table %>%
                 dplyr::filter(universe == .x) %>% dplyr::select(-universe)),
    paste0("Panel ", LETTERS[seq_along(universe_labels)], ". ", universe_labels)
  )
}

#' Save the Table 6-style full-sample sensitivity
#'
#' Keeps the Internet Appendix sensitivity to one call in the main reporting
#' script while using its existing table writer and portfolio-root convention.
save_full_sample_portfolio_table <- function(portfolio_root, factor_names,
                                             evaluation_dates) {
  panels <- build_full_sample_portfolio_panels(
    file.path(portfolio_root, "panel", "full_sample_10bps", "all_ports.rds"),
    factor_names, min(evaluation_dates), max(evaluation_dates)
  )
  caption <- paste(
    "Economic performance under full-sample volatility matching.",
    "Panel A uses market-portfolio forecasts from the pooled estimation.",
    "Panel B equal-weights the 153 factor strategies each month.",
    "The constant normalizer matches managed and unmanaged volatility over the full July 2006 to December 2024 forecast sample and is therefore an ex-post sensitivity.",
    "Statistics use the common July 2011 to December 2024 evaluation period of the primary analysis.",
    "Turnover starts from zero in July 2011 so net results use the same entry-cost convention as the primary specification.",
    "Capped strategies restrict exposure to 1.5. Net strategies deduct 10 basis points per unit change in factor exposure.",
    "EW combines HARX, elastic net, random forest, XGBoost, and the three neural networks."
  )
  save_panel_table(
    panels, "internet_appendix", "portfolio_performance_full_sample",
    caption, digits = 3, font_size = "scriptsize", placement = "htbp"
  )
}

