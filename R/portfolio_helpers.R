# Factor-level volatility management
# ------------------------------------------------------------------------------

  `%>%` <- dplyr::`%>%`

#' Check observation keys and numeric inputs without repairing observations
#'
#' Dates must be Date values; monthly inputs use first-of-month identifiers.
#' Row order is unrestricted. Historical gaps can be allowed explicitly;
#' missing values in supplied observations and duplicate keys always fail.
validate_port_data <- function(
    data,
    keys,
    numeric_cols,
    monthly = TRUE,
    consecutive = TRUE
) {
  if (!is.data.frame(data) || !all(c(keys, numeric_cols) %in% names(data))) {
    rlang::abort(
      paste("Required columns:", paste(c(keys, numeric_cols), collapse = ", "))
    )
  }
  if (!nrow(data)) rlang::abort("Portfolio input is empty.")
  if (anyNA(data[keys]) || any(vapply(
    data[keys],
    function(x) is.character(x) && any(!nzchar(x)),
    logical(1)
  ))) {
    rlang::abort("Missing/empty keys.")
  }
  if (anyDuplicated(data[keys])) rlang::abort("Duplicate portfolio keys.")
  date_col <- if (monthly) "month_id" else "date"
  if (!inherits(data[[date_col]], "Date") || any(!is.finite(data[[date_col]])) ||
      (monthly && any(format(data[[date_col]], "%d") != "01"))) {
    rlang::abort("Dates must be Date values, first-of-month for monthly inputs.")
  }
  if (!all(vapply(data[numeric_cols], is.numeric, logical(1))) ||
      any(!is.finite(as.matrix(data[numeric_cols])))) {
    rlang::abort("Inputs must be numeric and finite.")
  }
  if (monthly && consecutive) {
    blocks <- if ("name" %in% keys) split(data, data$name) else list(data)
    purrr::walk(blocks, function(x) {
      months <- sort(
        as.integer(format(x$month_id, "%Y")) * 12L +
          as.integer(format(x$month_id, "%m"))
      )

      if (any(diff(months) != 1L)) {
        rlang::abort("Each series must have consecutive months.")
      }
    })
  }
  invisible(data)
}

#' Read monthly holding-period RF returns from the saved French ZIP snapshot
#'
#' Input is the official F-F_Research_Data_Factors_CSV.zip. Only six-digit
#' monthly records are read; annual records are excluded. Percent becomes decimal.
#' No network request or annual-yield conversion occurs in this reader.
read_french_rf <- function(path) {
  if (!file.exists(path)) {
    rlang::abort(paste("Missing French RF snapshot:", path))
  }

  entries <- utils::unzip(path, list = TRUE)$Name
  entry <- entries[grepl("F-F_Research_Data_Factors.csv$", entries, ignore.case = TRUE)]
  if (length(entry) != 1L) {
    rlang::abort("Expected one French monthly factor CSV in ZIP.")
  }

  connection <- unz(path, entry)
  on.exit(close(connection))
  lines <- readLines(connection, warn = FALSE)
  monthly <- lines[grepl("^[0-9]{6},", lines)]
  data <- utils::read.csv(
    text = paste(monthly, collapse = "\n"),
    header = FALSE,
    col.names = c("date", "mkt", "smb", "hml", "rf")
  )

  result <- data %>%
    dplyr::transmute(
      month_id = as.Date(paste0(date, "01"), format = "%Y%m%d"),
      rf = rf / 100
    )

  validate_port_data(result, "month_id", "rf")
  if (any(result$rf <= -1)) {
    rlang::abort("Invalid risk-free holding-period return.")
  }

  result
}

#' Aggregate daily returns to monthly holding-period returns
#'
#' factor_daily: name/date/ret, signed decimal long-short factor returns, already
#' oriented by the provider. market_daily: date/ret, decimal CRSP total returns.
#' rf: month_id/rf, observed monthly holding-period return in decimals.
#' Daily factor and market returns are COMPOUNDED within each month. The resulting
#' factor returns need not equal provider monthly factors formed independently at
#' a monthly frequency; exact replication would require that native monthly series.
#' Returns name/month_id/factor_exc/rf. Does not subtract RF twice from factors.
prepare_factor_returns <- function(factor_daily, market_daily, rf) {
  validate_port_data(
    factor_daily,
    c("name", "date"),
    "ret",
    monthly = FALSE
  )
  validate_port_data(market_daily, "date", "ret", monthly = FALSE)
  validate_port_data(rf, "month_id", "rf")

  if ("spx_vw" %in% factor_daily$name) {
    rlang::abort("spx_vw is reserved for the market.")
  }

  if (any(market_daily$ret < -1)) {
    rlang::abort("Market total return below -100%.")
  }
  factors <- factor_daily %>%
    dplyr::mutate(month_id = lubridate::floor_date(date, "month")) %>%
    dplyr::group_by(name, month_id) %>%
    dplyr::summarise(factor_exc = prod(1 + ret) - 1, .groups = "drop")
  market <- market_daily %>%
    dplyr::mutate(month_id = lubridate::floor_date(date, "month")) %>%
    dplyr::group_by(month_id) %>%
    dplyr::summarise(factor_exc = prod(1 + ret) - 1, .groups = "drop") %>%
    dplyr::mutate(name = "spx_vw")
  result <- dplyr::bind_rows(factors, market) %>%
    dplyr::left_join(rf, by = "month_id", relationship = "many-to-one") %>%
    dplyr::mutate(
      factor_exc = dplyr::if_else(
        name == "spx_vw", factor_exc - rf, factor_exc
      )
    ) %>%
    dplyr::arrange(name, month_id)

  if (anyNA(result$rf)) {
    rlang::abort("Missing risk-free coverage for daily input months.")
  }

  validate_port_data(
    result,
    c("name", "month_id"),
    c("factor_exc", "rf"),
    consecutive = FALSE
  )
  result
}

#' Evaluate one MM-style managed portfolio per factor and prediction model
#'
#' oos_outputs: name/month_id/target/pred, V3 log(RV + 1e-8) OOS predictions.
#' factor_returns: name/month_id/factor_exc/rf in decimal monthly return units.
#' target_gold: name/month_id/rv_month, UNTRANSFORMED positive monthly sum of
#' squared daily returns. Input month_id names the month being predicted/earned;
#' its prediction is available before that month, its target/return only after.
#'
#' normalization = expanding_rv uses the factor's mean RV through t-1 (default).
#' expanding_vol matches historical unmanaged and unnormalized managed return
#' volatility, also through t-1. For ML this needs min_history PRIOR OOS forecasts;
#' no in-sample fitted values are substituted. full_sample is an EX POST diagnostic.
#' min_history controls warm-up, which is reported explicitly in coverage.
#'
#' cost_per_unit is a nonnegative decimal charge per unit of factor exposure
#' changed (0.001 = 10 bp). Raw and capped exposures are costed independently.
#' initial_weight is the pre-existing factor exposure at the first evaluated
#' month (default zero: pay entry). No liquidation cost is imposed at sample end.
#' This is MM target-weight turnover, NOT stock turnover or full factor costs.
#' In particular a long-short factor cap of 1.5 is NOT a 150% gross-stock cap.
#'
#' cer_gamma affects only the reported mean-variance certainty equivalent; no
#' portfolio optimization is performed. Returns the familiar w_compiled_df and
#' strategy_summary, plus explicit coverage and settings. Historical means use
#' observed months only and historical gaps are counted in coverage. Calendar
#' joins prevent lagging across a gap. OOS gaps and missing evaluation data abort.
run_oos_ports <- function(
    oos_outputs,
    factor_returns,
    target_gold,
    normalization = c("expanding_rv", "expanding_vol", "full_sample"),
    min_history = 120L,
    weight_cap = 1.5,
    cost_per_unit = .001,
    initial_weight = 0,
    cer_gamma = 5
) {
  
  normalization <- match.arg(normalization)
  parameters <- c(
    "min_history", "weight_cap", "cost_per_unit",
    "initial_weight", "cer_gamma"
  )

  for (parameter in parameters) {
    value <- get(parameter)

    if (!is.numeric(value) || length(value) != 1L || !is.finite(value) || value < 0) {
      rlang::abort(paste(parameter, "must be a finite nonnegative numeric scalar."))
    }
  }

  if (min_history < 2 || min_history != as.integer(min_history)) {
    rlang::abort("min_history must be an integer >= 2.")
  }

  if (weight_cap < 1 || initial_weight > weight_cap || cer_gamma <= 0) {
    rlang::abort("Require weight_cap >= 1, initial_weight <= weight_cap and cer_gamma > 0.")
  }

  keys <- c("name", "month_id")
  validate_port_data(oos_outputs, keys, c("target", "pred"))
  validate_port_data(factor_returns, keys, c("factor_exc", "rf"), consecutive = FALSE)
  validate_port_data(target_gold, keys, "rv_month", consecutive = FALSE)
  if (anyDuplicated(
    dplyr::distinct(factor_returns, month_id, rf)$month_id
  )) {
    rlang::abort("Risk-free returns must agree across factors within month.")
  }
  if (any(target_gold$rv_month <= 0)) rlang::abort("RV must be strictly positive.")
  pred_level <- exp(oos_outputs$pred) - 1e-8
  if (any(!is.finite(pred_level) | pred_level <= 0)) {
    rlang::abort("Back-transformed forecasts must be finite and positive.")
  }
  observed <- oos_outputs %>%
    dplyr::select(dplyr::all_of(c(keys, "target", "pred"))) %>%
    dplyr::mutate(pred_level = pred_level) %>%
    dplyr::left_join(target_gold, by = keys, relationship = "one-to-one")
  if (anyNA(observed$rv_month)) rlang::abort("Missing target coverage for forecasts.")
  if (any(
    abs(observed$target - log(observed$rv_month + 1e-8)) > 1e-10
  )) {
    rlang::abort("Forecast targets disagree with untransformed V3 RV.")
  }
  if (nrow(dplyr::anti_join(observed, factor_returns, by = keys))) {
    rlang::abort("Missing return coverage for forecasts.")
  }

  # Formation and calibration within each factor
  # ------------------------------------------------------------------------------
  results <- observed %>%
    dplyr::group_by(name) %>%
    dplyr::group_split() %>%
    purrr::map(function(forecasts) {
      factor_name <- forecasts$name[1]
      ### Tag lagged RV
      lagged_rv <- target_gold %>%
        dplyr::filter(name == factor_name) %>%
        dplyr::transmute(
          name,
          month_id = lubridate::add_with_rollback(
            month_id, lubridate::period(month = 1)
          ),
          rv_lag = rv_month
        )
      ### Combine into history panel
      history <- target_gold %>%
        dplyr::filter(name == factor_name, month_id <= max(forecasts$month_id)) %>%
        dplyr::arrange(month_id) %>%
        dplyr::mutate(mean_rv = dplyr::lag(dplyr::cummean(rv_month)),
                      n_rv = dplyr::row_number() - 1L) %>%
        dplyr::left_join(lagged_rv, by = keys, relationship = "one-to-one") %>%
        dplyr::left_join(dplyr::select(forecasts, dplyr::all_of(keys), pred_level),
                         by = keys, relationship = "one-to-one") %>%
        dplyr::left_join(factor_returns, by = keys, relationship = "one-to-one")
      ## Missing early RF (French starts July 1926) is allowed only before the
      ## supplied return history starts. Interior gaps must never become a
      ## shortened training sample. The runner starts its return inputs there.
      return_start <- min(factor_returns$month_id[factor_returns$name == factor_name])
      if (anyNA(history$factor_exc[history$month_id >= return_start])) {
        rlang::abort(paste("Missing historical return coverage for", factor_name))
      }
      calibrate <- function(signal) {
        if (normalization == "expanding_rv") return(history$mean_rv)
        unscaled <- history$factor_exc / signal
        valid_pair <- is.finite(history$factor_exc) & is.finite(signal)
        if (any(!is.finite(unscaled[valid_pair]))) rlang::abort("Overflow in calibration returns.")
        purrr::map_dbl(seq_len(nrow(history)), function(i) {
          indices <- if (normalization == "full_sample") {
            which(history$month_id %in% forecasts$month_id & history$n_rv >= min_history &
                    !is.na(history$rv_lag))
          } else seq_len(i - 1L)
          indices <- indices[is.finite(unscaled[indices])]
          if (length(indices) < min_history) return(NA_real_)
          denominator <- stats::sd(unscaled[indices])
          numerator <- stats::sd(history$factor_exc[indices])
          if (!is.finite(denominator) || denominator <= 0 || numerator <= 0) {
            rlang::abort(paste("Cannot match zero/nonfinite return variance for", factor_name))
          }
          numerator / denominator
        })
      }
      history$normalizer_ml <- calibrate(history$pred_level)
      history$normalizer_rw <- calibrate(history$rv_lag)
      evaluation <- history %>%
        dplyr::filter(month_id %in% forecasts$month_id) %>%
        dplyr::mutate(eligible = n_rv >= min_history & !is.na(rv_lag) &
                        !is.na(normalizer_ml) & !is.na(normalizer_rw))
      if (sum(evaluation$eligible) < 2L) {
        rlang::abort(paste("Insufficient calibration history for", factor_name))
      }

      coverage <- tibble::tibble(
        name = factor_name,
        n_forecasts = nrow(evaluation),
        n_warmup = sum(!evaluation$eligible),
        n_evaluated = sum(evaluation$eligible),
        first_forecast = min(evaluation$month_id),
        first_evaluated = min(evaluation$month_id[evaluation$eligible]),
        last_evaluated = max(evaluation$month_id),
        historical_missing_months = length(
          seq(min(history$month_id), max(history$month_id), by = "month")
        ) - nrow(history)
      )

      evaluation <- dplyr::filter(evaluation, eligible)
      weights <- purrr::map_dfr(c("buyhold", "rw", "vol_scaled"), function(method) {
        normalizer <- switch(
          method,
          buyhold = rep(1, nrow(evaluation)),
          rw = evaluation$normalizer_rw,
          vol_scaled = evaluation$normalizer_ml
        )
        signal <- switch(
          method,
          buyhold = rep(1, nrow(evaluation)),
          rw = evaluation$rv_lag,
          vol_scaled = evaluation$pred_level
        )

        evaluation %>%
          dplyr::transmute(
            name,
            month_id,
            method = method,
            factor_exc,
            rf,
            normalizer = normalizer,
            signal_rv = signal,
            w_raw = normalizer / signal
          )
      })
      list(weights = weights, coverage = coverage)
    })
  weights <- purrr::map_dfr(results, "weights")
  if (any(!is.finite(weights$w_raw) | weights$w_raw < 0)) rlang::abort("Invalid managed exposure.")

  # Returns and incremental factor-exposure costs
  # ------------------------------------------------------------------------------
  weights <- weights %>%
    dplyr::arrange(name, method, month_id) %>%
    dplyr::group_by(name, method) %>%
    dplyr::mutate(
      w_capped = pmin(weight_cap, w_raw),
      turnover_raw = abs(
        w_raw - dplyr::lag(w_raw, default = initial_weight)
      ),
      turnover = abs(
        w_capped - dplyr::lag(w_capped, default = initial_weight)
      ),
      tc_raw = cost_per_unit * turnover_raw,
      tc = cost_per_unit * turnover,
      r_exc_fwd1_raw = w_raw * factor_exc,
      r_exc_fwd1_capped = w_capped * factor_exc,
      r_exc_fwd1_net_raw = r_exc_fwd1_raw - tc_raw,
      r_exc_fwd1_net_capped = r_exc_fwd1_capped - tc,
      ## Each long-short factor is already zero-cost/excess. One unit of
      ## collateral earns RF independently of the factor's exposure multiplier.
      r_strat_fwd1_raw = rf + r_exc_fwd1_raw,
      r_strat_fwd1_capped = rf + r_exc_fwd1_capped,
      r_strat_fwd1_net_raw = rf + r_exc_fwd1_net_raw,
      r_strat_fwd1_net_capped = rf + r_exc_fwd1_net_capped
    ) %>%
    dplyr::ungroup()
  if (any(!is.finite(as.matrix(dplyr::select(weights, dplyr::where(is.numeric)))))) {
    rlang::abort("Nonfinite portfolio return or cost; inspect leverage and RV scale.")
  }

  # Direct OOS comparisons, without an ex post optimal combination
  # ------------------------------------------------------------------------------
  summary <- weights %>%
    dplyr::group_by(name, method) %>%
    dplyr::group_modify(function(data, key) {
      metrics <- purrr::map(c("raw", "capped", "net_raw", "net_capped"), function(version) {
        returns <- data[[paste0("r_exc_fwd1_", version)]]
        volatility <- stats::sd(returns)
        stats::setNames(list(mean(returns), volatility,
          if (volatility > 0) sqrt(12) * mean(returns) / volatility else NA_real_,
          12 * (mean(returns) - cer_gamma / 2 * stats::var(returns))),
          paste0(c("avg_exc_ret_", "sd_return_", "sharpe_ratio_", "cer_ann_"), version))
      })
      tibble::as_tibble(
        c(
          list(
            n = nrow(data),
            start = min(data$month_id),
            end = max(data$month_id),
            avg_turnover_raw = mean(data$turnover_raw),
            avg_turnover_capped = mean(data$turnover),
            max_weight_raw = max(data$w_raw),
            cap_fraction = mean(data$w_raw > weight_cap),
            insolvency_months_raw = sum(data$r_strat_fwd1_net_raw <= -1)
          ),
          unlist(metrics, recursive = FALSE)
        )
      )
    }) %>% dplyr::ungroup()
  benchmark <- summary %>% dplyr::filter(method == "buyhold") %>%
    dplyr::select(
      name,
      dplyr::starts_with("sharpe_ratio_"),
      dplyr::starts_with("cer_ann_")
    ) %>%
    dplyr::rename_with(~ paste0(.x, "_unmanaged"), -name)
  summary <- summary %>%
    dplyr::left_join(benchmark, by = "name", relationship = "many-to-one")
  for (version in c("raw", "capped", "net_raw", "net_capped")) {
    for (metric in c("sharpe_ratio_", "cer_ann_")) {
      column <- paste0(metric, version)
      summary[[paste0("delta_", column)]] <-
        summary[[column]] - summary[[paste0(column, "_unmanaged")]]
    }
  }
  list(
    w_compiled_df = weights,
    strategy_summary = summary,
    coverage = purrr::map_dfr(results, "coverage"),
    settings = list(
      normalization = normalization,
      implementable = normalization != "full_sample",
      min_history = min_history,
      weight_cap = weight_cap,
      cost_per_unit = cost_per_unit,
      initial_weight = initial_weight,
      cer_gamma = cer_gamma,
      cost_scope = paste(
        "Incremental factor-exposure turnover only;",
        "excludes underlying stock trading costs"
      ),
      rv_definition = paste(
        "V3 monthly sum of squared daily returns;",
        "no demeaning or 22/J adjustment"
      )
    )
  )
}
