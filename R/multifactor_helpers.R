# DeMiguel-style multifactor volatility management
# ------------------------------------------------------------------------------

  `%>%` <- dplyr::`%>%`
  base::source(base::file.path(here::here(), "R", "allocation_helpers.R"))

#' Solve the nonnegative extended-factor mean-variance problem
#'
#' x: finite monthly excess-return matrix; one column per tradable basis.
#' cost: nonnegative expected monthly cost per unit of each basis coefficient.
#' gamma: positive risk aversion (5 in DeMiguel et al., equation 6).
#' Returns coefficients, utility and the maximum first-order optimality residual.
#' Sample covariance may be singular. No ridge, inverse or weight-sum constraint
#' is introduced. Failed convergence or first-order conditions stop the backtest.
fit_multifactor <- function(x, cost, gamma = 5) {

  if (!base::is.matrix(x) || !base::is.numeric(x) || base::nrow(x) < 2L ||
      !base::ncol(x) || base::any(!base::is.finite(x))) {
    rlang::abort("x must be a finite numeric return matrix with at least two rows.")
  }
  if (!base::is.numeric(cost) || base::length(cost) != base::ncol(x) ||
      base::any(!base::is.finite(cost) | cost < 0) ||
      !base::is.numeric(gamma) || base::length(gamma) != 1L ||
      !base::is.finite(gamma) || gamma <= 0) {
    rlang::abort("Require one nonnegative cost per column and positive finite gamma.")
  }

  ## Rescale columns for the solver only: inverse volatility can otherwise make
  ## managed columns orders of magnitude larger. Undo this exact change of
  ## variables below; it does not regularize the economic problem.
  scale <- base::apply(x, 2, stats::sd)
  if (base::any(scale <= 0)) {
    rlang::abort("Cannot optimize a constant return basis; inspect the estimation window.")
  }
  z <- base::sweep(x, 2, scale, "/")
  covariance <- stats::cov(z)
  mean_net <- base::colMeans(z) - cost / scale

  objective <- function(w) {
    gamma / 2 * base::sum(w * (covariance %*% w)) - base::sum(mean_net * w)
  }
  gradient <- function(w) base::as.vector(gamma * covariance %*% w - mean_net)

  fit <- stats::optim(
    par = base::rep(0, base::ncol(x)), fn = objective, gr = gradient,
    method = "L-BFGS-B", lower = 0,
    control = base::list(maxit = 20000L, factr = 1e4, pgtol = 1e-7)
  )

  ## At a positive coefficient the derivative must vanish. At its lower bound
  ## it may be positive, but not negative (which would improve the objective).
  residual <- base::max(base::abs(base::pmin(fit$par, gradient(fit$par))))
  if (fit$convergence != 0L || !base::is.finite(fit$value) ||
      base::any(!base::is.finite(fit$par)) || residual > 1e-5) {
    rlang::abort(base::sprintf(
      "Multifactor optimizer failed: convergence=%s, KKT=%.3g, message=%s; no weights saved.",
      fit$convergence, residual, fit$message
    ))
  }

  base::list(coef = fit$par / scale, utility = -fit$value, kkt_residual = residual)
}

#' Construct DeMiguel's lagged market-volatility signal
#'
#' market_daily: date/ret, daily decimal market returns. sigma is the sample
#' daily standard deviation within the PREVIOUS calendar month (equation 3).
#' month_id is the holding month. Single-day and zero-volatility months fail.
prepare_market_sigma <- function(market_daily) {

  validate_port_data(market_daily, "date", "ret", monthly = FALSE)
  signal <- market_daily %>%
    dplyr::mutate(month_id = lubridate::floor_date(date, "month")) %>%
    dplyr::group_by(month_id) %>%
    dplyr::summarise(sigma = stats::sd(ret), .groups = "drop") %>%
    dplyr::mutate(month_id = lubridate::add_with_rollback(
      month_id, lubridate::period(month = 1)
    ))

  validate_port_data(signal, "month_id", "sigma")
  if (base::any(signal$sigma <= 0)) rlang::abort("Market sigma must be positive.")
  signal
}

#' Estimate and evaluate conditional and unconditional multifactor portfolios
#'
#' factor_returns: name/month_id/factor_exc/rf; decimal monthly excess returns.
#' signal: month_id/sigma, positive volatility available BEFORE the holding month.
#' factor_names: fixed, explicitly chosen universe; never selected by performance.
#' evaluation_start: first requested OOS holding month. Every fit uses strictly
#' earlier returns. min_history counts common complete signal/return months.
#' gamma determines allocations; cost_per_unit is a factor-exposure cost proxy.
#' exposure_cap optionally scales the entire portfolio to this sum of positive
#' factor exposures, preserving relative weights. It is NOT a stock leverage cap.
#'
#' CMV uses [f, f/sigma] and nonnegative [a,b]; UMV uses f with b=0. The historical
#' cost proxy is cost_per_unit * mean(abs(diff(1/sigma))) * sum(b). Under fixed
#' coefficients a cancels from target-exposure changes. Entry/refit trades are
#' charged during OOS evaluation, but are not anticipated in that training proxy.
#' No stock turnover, netting, financing or internal factor costs are available.
#'
#' Earlier incomplete history is explicitly excluded up to the latest series
#' start; any subsequent gap fails. ML needs min_history prior OOS predictions.
#' Returns monthly returns, factor weights, summaries, fit diagnostics and settings.
run_multifactor_ports <- function(
    factor_returns, signal, factor_names, evaluation_start,
    min_history = 120L, gamma = 5, cost_per_unit = .001,
    exposure_cap = 1.5, signal_label = "lagged_market_sd",
    allocation = "mvo", signal_scope = "market", cluster_map = NULL,
    managed_normalization = if (allocation == "mvo") "none" else "training_vol"
) {

  allocation <- base::match.arg(allocation, c("mvo", "ew", "hew"))
  signal_scope <- base::match.arg(signal_scope, c("market", "factor"))
  managed_normalization <- base::match.arg(managed_normalization, c("none", "training_vol"))

  validate_port_data(factor_returns, c("name", "month_id"), c("factor_exc", "rf"),
                     consecutive = FALSE)
  signal_keys <- if (signal_scope == "factor") c("name", "month_id") else "month_id"
  validate_port_data(signal, signal_keys, "sigma")
  if (!base::is.character(factor_names) || !base::length(factor_names) ||
      base::anyNA(factor_names) || base::anyDuplicated(factor_names) ||
      !base::all(factor_names %in% factor_returns$name)) {
    rlang::abort("factor_names must identify unique available factors.")
  }
  if (!base::inherits(evaluation_start, "Date") || base::length(evaluation_start) != 1L ||
      !base::is.finite(evaluation_start) || base::format(evaluation_start, "%d") != "01") {
    rlang::abort("evaluation_start must be one first-of-month Date.")
  }
  parameters <- base::list(min_history = min_history, gamma = gamma,
                           cost_per_unit = cost_per_unit, exposure_cap = exposure_cap)
  purrr::walk(parameters, function(value) {
    if (!base::is.numeric(value) || base::length(value) != 1L || !base::is.finite(value)) {
      rlang::abort("Multifactor parameters must be finite numeric scalars.")
    }
  })
  if (min_history < 2 || min_history != base::as.integer(min_history) ||
      gamma <= 0 || cost_per_unit < 0 || exposure_cap <= 0 || base::any(signal$sigma <= 0)) {
    rlang::abort("Invalid history, risk aversion, cost, cap or volatility.")
  }

  if (signal_scope == "factor" && !base::setequal(signal$name, factor_names)) {
    rlang::abort("Own-factor signals must cover exactly the selected factor names.")
  }
  if (allocation == "hew") {
    if (!base::is.data.frame(cluster_map) ||
        !base::all(c("name", "cluster") %in% base::names(cluster_map)) ||
        base::anyNA(cluster_map) || base::anyDuplicated(cluster_map$name) ||
        !base::all(factor_names %in% cluster_map$name)) {
      rlang::abort("HEW requires one explicit cluster for every factor.")
    }
  } else {
    cluster_map <- tibble::tibble(name = factor_names, cluster = "all")
  }

  ## Use a common fixed universe. No pairwise covariance or missing-return
  ## imputation: those would change the opportunity set across portfolios.
  data <- factor_returns %>%
    dplyr::filter(name %in% factor_names)
  starts <- data %>% dplyr::group_by(name) %>%
    dplyr::summarise(start = base::min(month_id), .groups = "drop")
  common_start <- base::max(base::max(starts$start), base::min(signal$month_id))
  ends <- data %>% dplyr::group_by(name) %>%
    dplyr::summarise(end = base::max(month_id), .groups = "drop")
  common_end <- base::min(base::min(ends$end), base::max(signal$month_id))
  if (common_start > common_end) rlang::abort("No common multifactor history.")
  if (common_end < base::max(signal$month_id)) {
    rlang::abort("Missing factor returns at the end of the signal evaluation period.")
  }

  dates <- tibble::tibble(month_id = base::seq(common_start, common_end, by = "month"))
  keys <- tidyr::expand_grid(month_id = dates$month_id, name = base::sort(factor_names))
  data <- keys %>%
    dplyr::left_join(data, by = c("name", "month_id"), relationship = "one-to-one")
  validate_port_data(data, c("name", "month_id"), c("factor_exc", "rf"))
  cash <- dplyr::distinct(data, month_id, rf)
  if (base::anyDuplicated(cash$month_id)) rlang::abort("RF must agree across factors.")
  timing <- dates %>% dplyr::left_join(cash, by = "month_id", relationship = "one-to-one")
  signal_panel <- if (signal_scope == "factor") {
    keys %>% dplyr::left_join(signal, by = c("name", "month_id"), relationship = "one-to-one")
  } else {
    keys %>% dplyr::left_join(signal, by = "month_id", relationship = "many-to-one")
  }
  validate_port_data(signal_panel, c("name", "month_id"), "sigma")

  ## Pivot using explicit names, then select those same names when entering
  ## matrix algebra. Input row order never determines factor alignment.
  factor_names <- base::sort(factor_names)
  wide <- data %>% dplyr::select(month_id, name, factor_exc) %>%
    tidyr::pivot_wider(names_from = name, values_from = factor_exc) %>%
    dplyr::arrange(month_id)
  returns <- base::as.matrix(dplyr::select(wide, dplyr::all_of(factor_names)))
  sigma_wide <- signal_panel %>%
    tidyr::pivot_wider(names_from = name, values_from = sigma) %>% dplyr::arrange(month_id)
  sigma <- base::as.matrix(dplyr::select(sigma_wide, dplyr::all_of(factor_names)))
  inverse_sigma <- 1 / sigma
  if (base::any(!base::is.finite(inverse_sigma))) rlang::abort("Inverse sigma overflow.")
  indices <- base::which(dates$month_id >= evaluation_start &
                          base::seq_len(base::nrow(dates)) > min_history)
  if (base::length(indices) < 2L) rlang::abort("Insufficient common OOS history for multifactor evaluation.")

  fitted <- purrr::map(indices, function(i) {
    past <- base::seq_len(i - 1L)
    f <- returns[past, , drop = FALSE]
    z <- inverse_sigma[past, , drop = FALSE]
    k <- base::length(factor_names)

    ## A fixed normalization within each training window is a harmless change
    ## of units for MVO, but ESSENTIAL to interpret EW on comparable sleeves.
    ## It uses prior returns only, and is applied to the current scaler as well.
    normalization <- base::rep(1, k)
    if (managed_normalization == "training_vol") {
      normalization <- base::apply(f, 2, stats::sd) / base::apply(f * z, 2, stats::sd)
      if (base::any(!base::is.finite(normalization) | normalization <= 0)) {
        rlang::abort("Cannot match managed/unmanaged training volatility.")
      }
    }
    z <- base::sweep(z, 2, normalization, "*")
    current_scaler <- inverse_sigma[i, ] * normalization

    ##Following DeMiguel, UMV = "Unconditional Mean Variance MF", CMV = "Conditional Mean Variance MF"
    conditional_method <- if (signal_scope == "factor") "fcmv" else "cmv"
    purrr::map_dfr(c("umv", conditional_method), function(method) {
      x <- if (method == "umv") f else base::cbind(f, f * z)
      costs <- if (method == "umv") base::rep(0, k) else
        base::c(base::rep(0, k), cost_per_unit * base::colMeans(base::abs(z[-1, , drop = FALSE] -
          z[-base::nrow(z), , drop = FALSE])))
      basis <- tibble::tibble(name = factor_names, sleeve = "unmanaged")
      if (method != "umv") basis <- dplyr::bind_rows(basis,
        tibble::tibble(name = factor_names, sleeve = "managed"))
      basis <- basis %>% dplyr::left_join(cluster_map, by = "name", relationship = "many-to-one") %>%
        dplyr::mutate(id = base::paste(name, sleeve, sep = ":"))
      base::colnames(x) <- basis$id
      fit <- allocate_portfolio(x, basis, allocation, gamma, costs)
      a <- fit$coef[base::seq_len(k)]
      b <- if (method == "umv") base::rep(0, k) else fit$coef[k + base::seq_len(k)]

      tibble::tibble(
        month_id = dates$month_id[i], name = factor_names, method = method,
        a = a, b = b, sigma = sigma[i, ], scaler = current_scaler,
        normalizer = normalization, w_raw = a + b * current_scaler,
        factor_exc = returns[i, ], rf = timing$rf[i], n_train = i - 1L,
        n_basis = base::ncol(x), train_end = dates$month_id[i - 1L],
        kkt_residual = fit$kkt_residual
      )
    })
  })

  ## The capped version is an implementation diagnostic, not a constraint in
  ## equation (6). Both versions pay their OWN entry and monthly refit trades.
  weights <- dplyr::bind_rows(fitted) %>%
    dplyr::group_by(method, month_id) %>%
    dplyr::mutate(w_capped = w_raw * base::min(1, exposure_cap / base::sum(w_raw))) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(method, name, month_id) %>%
    dplyr::group_by(method, name) %>%
    dplyr::mutate(
      turnover_raw = base::abs(w_raw - dplyr::lag(w_raw, default = 0)),
      turnover_capped = base::abs(w_capped - dplyr::lag(w_capped, default = 0))
    ) %>% dplyr::ungroup()

  monthly <- weights %>% dplyr::group_by(method, month_id) %>%
    dplyr::summarise(
      rf = dplyr::first(rf), exposure_raw = base::sum(w_raw),
      exposure_capped = base::sum(w_capped),
      gross_raw = base::sum(w_raw * factor_exc),
      gross_capped = base::sum(w_capped * factor_exc),
      turnover_raw = base::sum(turnover_raw), turnover_capped = base::sum(turnover_capped),
      .groups = "drop"
    ) %>% dplyr::mutate(
      tc_raw = cost_per_unit * turnover_raw, tc_capped = cost_per_unit * turnover_capped,
      net_raw = gross_raw - tc_raw, net_capped = gross_capped - tc_capped
    )
  if (base::any(!base::is.finite(base::as.matrix(dplyr::select(monthly, -method, -month_id))))) {
    rlang::abort("Nonfinite multifactor exposure, cost or return.")
  }

  summary <- purrr::map_dfr(c("raw", "capped"), function(version) {
    monthly %>% dplyr::group_by(method) %>% dplyr::group_modify(function(x, key) {
      gross <- x[[base::paste0("gross_", version)]]
      net <- x[[base::paste0("net_", version)]]
      sd_gross <- stats::sd(gross)
      sd_net <- stats::sd(net)
      tibble::tibble(
        version = version, n = base::nrow(x), start = base::min(x$month_id),
        end = base::max(x$month_id), mean_net_ann = 12 * base::mean(net),
        sharpe_paper = if (sd_gross > 0) base::sqrt(12) * base::mean(net) / sd_gross else NA_real_,
        sharpe_net = if (sd_net > 0) base::sqrt(12) * base::mean(net) / sd_net else NA_real_,
        cer_paper_ann = 12 * (base::mean(net) - gamma / 2 * stats::var(gross)),
        cost_ann = 12 * base::mean(x[[base::paste0("tc_", version)]]),
        insolvency_months = base::sum(1 + x$rf + net <= 0)
      )
    }) %>% dplyr::ungroup()
  })

  base::list(
    weights = weights, monthly_returns = monthly, strategy_summary = summary,
    diagnostics = dplyr::distinct(weights, month_id, method, train_end,
                                 n_train, n_basis, kkt_residual),
    settings = base::list(signal = signal_label, factor_names = factor_names,
      allocation = allocation, signal_scope = signal_scope,
      managed_normalization = managed_normalization,
      common_start = common_start, common_end = common_end,
      requested_start = evaluation_start, first_evaluated = base::min(monthly$month_id),
      min_history = min_history, gamma = gamma, cost_per_unit = cost_per_unit,
      exposure_cap = exposure_cap, cost_scope = "Incremental factor exposure only; no stock netting",
      initial_weight = 0, covariance = "Sample covariance; no regularization")
  )
}
