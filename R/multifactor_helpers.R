# DeMiguel-style multifactor volatility management
# ------------------------------------------------------------------------------

  `%>%` <- dplyr::`%>%`
  source(file.path(here::here(), "R", "allocation_helpers.R"))

#' Solve the nonnegative extended-factor mean-variance problem
#'
#' x: finite monthly excess-return matrix; one column per tradable basis.
#' cost: nonnegative expected monthly cost per unit of each basis coefficient.
#' gamma: positive risk aversion (5 in DeMiguel et al., equation 6).
#' Returns coefficients, utility and the maximum first-order optimality residual.
#' exposure_load: positive factor exposure per unit of each coefficient at the
#' current formation date. exposure_limit bounds their sum inside the solver.
#' NULL retains the legacy unbounded specification for explicit comparisons.
#' Singular covariance is allowed; bounded solutions need not be unique.
#' No ridge is introduced. Failed feasibility or optimality checks abort.
fit_multifactor <- function(x, cost, gamma = 5,
                            exposure_load = rep(1, ncol(x)), exposure_limit = NULL) {

  if (!is.matrix(x) || !is.numeric(x) || nrow(x) < 2L ||
      !ncol(x) || any(!is.finite(x))) {
    rlang::abort("x must be a finite numeric return matrix with at least two rows.")
  }
  if (!is.numeric(cost) || length(cost) != ncol(x) ||
      any(!is.finite(cost) | cost < 0) ||
      !is.numeric(gamma) || length(gamma) != 1L ||
      !is.finite(gamma) || gamma <= 0) {
    rlang::abort("Require one nonnegative cost per column and positive finite gamma.")
  }

  ## Rescale columns for the solver only: inverse volatility can otherwise make
  ## managed columns orders of magnitude larger. Undo this exact change of
  ## variables below; it does not regularize the economic problem.
  scale <- apply(x, 2, stats::sd)
  if (any(scale <= 0)) {
    rlang::abort("Cannot optimize a constant return basis; inspect the estimation window.")
  }
  z <- sweep(x, 2, scale, "/")
  covariance <- stats::cov(z)
  ## Record the raw sample covariance, before numerical changes of units.
  raw_covariance <- stats::cov(x)
  singular_values <- svd(raw_covariance, nu = 0, nv = 0)$d
  covariance_rank <- sum(singular_values > max(singular_values) *
    ncol(x) * .Machine$double.eps)
  covariance_rcond <- rcond(raw_covariance)
  mean_net <- colMeans(z) - cost / scale

  if (!is.null(exposure_limit)) {
    if (!is.numeric(exposure_load) || length(exposure_load) != ncol(x) ||
        any(!is.finite(exposure_load) | exposure_load <= 0) ||
        !is.numeric(exposure_limit) || length(exposure_limit) != 1L ||
        !is.finite(exposure_limit) || exposure_limit < 0) {
      rlang::abort("Require positive exposure loads and a finite nonnegative exposure limit.")
    }

    ## Optimize in CURRENT exposure units: u_j = loading_j * coefficient_j.
    ## This makes the constraint sum(u) <= limit invariant to sleeve units.
    ## SLSQP accepts a positive-semidefinite (including singular) quadratic;
    ## nloptr is already present in renv.lock. No covariance repair is applied.
    q <- sweep(x, 2, exposure_load, "/")
    if (any(!is.finite(q))) rlang::abort("Return per unit of exposure overflowed.")
    sigma_q <- stats::cov(q)
    mu_q <- colMeans(q) - cost / exposure_load
    objective_scale <- max(diag(sigma_q)) * gamma
    if (!is.finite(objective_scale) || objective_scale <= 0 ||
        any(!is.finite(sigma_q)) || any(!is.finite(mu_q))) {
      rlang::abort("Nonfinite or degenerate exposure-scaled optimization moments.")
    }
    objective_q <- function(u) (gamma / 2 * sum(u * (sigma_q %*% u)) -
      sum(mu_q * u)) / objective_scale
    gradient_q <- function(u) as.vector(gamma * sigma_q %*% u - mu_q) / objective_scale

    if (exposure_limit == 0) {
      u <- rep(0, ncol(x))
      residual <- 0
    } else {
      fit <- nloptr::nloptr(
        x0 = rep(0, ncol(x)), eval_f = objective_q, eval_grad_f = gradient_q,
        lb = rep(0, ncol(x)), ub = rep(exposure_limit, ncol(x)),
        eval_g_ineq = function(u) sum(u) - exposure_limit,
        eval_jac_g_ineq = function(u) rep(1, length(u)),
        opts = list(algorithm = "NLOPT_LD_SLSQP", xtol_rel = 1e-10,
          ftol_abs = 1e-12, maxeval = 20000L)
      )
      u <- fit$solution
      if (!fit$status %in% 1:4 || any(!is.finite(u)) ||
          min(u) < -1e-8 || sum(u) > exposure_limit + 1e-8) {
        rlang::abort(paste("Constrained multifactor optimizer failed:", fit$message))
      }
      ## Only round-off-sized constraint violations may be cleaned up. Verify
      ## optimality AFTER cleanup, including the active leverage multiplier.
      u <- pmax(u, 0)
      if (sum(u) > exposure_limit) u <- u * exposure_limit / sum(u)
      g <- gradient_q(u)
      active <- u > 1e-7
      lambda <- if (sum(u) >= exposure_limit - 1e-7 && any(active)) {
        max(0, -mean(g[active]))
      } else 0
      residual <- max(c(abs((g + lambda)[active]),
        pmax(-(g + lambda)[!active], 0), abs(lambda * (sum(u) - exposure_limit))))
      if (!is.finite(residual) || residual > 1e-5) {
        rlang::abort(sprintf("Constrained multifactor KKT residual %.3g exceeds tolerance.", residual))
      }
    }
    return(list(coef = unname(u / exposure_load), utility = -objective_q(u) * objective_scale,
      kkt_residual = residual, covariance_rank = covariance_rank,
      covariance_rcond = covariance_rcond))
  }

  objective <- function(w) {
    gamma / 2 * sum(w * (covariance %*% w)) - sum(mean_net * w)
  }
  gradient <- function(w) as.vector(gamma * covariance %*% w - mean_net)

  fit <- stats::optim(
    par = rep(0, ncol(x)), fn = objective, gr = gradient,
    method = "L-BFGS-B", lower = 0,
    control = list(maxit = 20000L, factr = 1e4, pgtol = 1e-7)
  )

  ## At a positive coefficient the derivative must vanish. At its lower bound
  ## it may be positive, but not negative (which would improve the objective).
  residual <- max(abs(pmin(fit$par, gradient(fit$par))))
  if (fit$convergence != 0L || !is.finite(fit$value) ||
      any(!is.finite(fit$par)) || residual > 1e-5) {
    rlang::abort(sprintf(
      "Multifactor optimizer failed: convergence=%s, KKT=%.3g, message=%s; no weights saved.",
      fit$convergence, residual, fit$message
    ))
  }

  list(coef = fit$par / scale, utility = -fit$value, kkt_residual = residual,
    covariance_rank = covariance_rank, covariance_rcond = covariance_rcond)
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
  if (any(signal$sigma <= 0)) rlang::abort("Market sigma must be positive.")
  signal
}

#' Estimate and evaluate conditional and unconditional multifactor portfolios
#'
#' factor_returns: name/month_id/factor_exc/rf; decimal monthly excess returns.
#' signal: month_id/sigma (market), or name/month_id/sigma (own factor), with
#' positive volatility available BEFORE the holding month.
#' factor_names: fixed, explicitly chosen universe; never selected by performance.
#' evaluation_start: first requested OOS holding month. Every fit uses strictly
#' earlier returns. min_history counts common complete signal/return months.
#' gamma determines allocations; cost_per_unit is a factor-exposure cost proxy.
#' exposure_cap optionally scales the entire portfolio to this sum of positive
#' factor exposures, preserving relative weights. It is NOT a stock leverage cap.
#' mvo_exposure_limit bounds exposure INSIDE optimization (NULL = unbounded).
#' CMV/FCMV also obey conditional_exposure_multiple times fitted UMV exposure.
#' All constraints refer to the known current signal, never the holding return.
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
    managed_normalization = if (allocation == "mvo") "none" else "training_vol",
    mvo_exposure_limit = exposure_cap, conditional_exposure_multiple = 1.2
) {

  allocation <- match.arg(allocation, c("mvo", "ew", "hew"))
  signal_scope <- match.arg(signal_scope, c("market", "factor"))
  managed_normalization <- match.arg(managed_normalization, c("none", "training_vol"))

  validate_port_data(factor_returns, c("name", "month_id"), c("factor_exc", "rf"),
                     consecutive = FALSE)
  signal_keys <- if (signal_scope == "factor") c("name", "month_id") else "month_id"
  validate_port_data(signal, signal_keys, "sigma")
  if (!is.character(factor_names) || !length(factor_names) ||
      anyNA(factor_names) || anyDuplicated(factor_names) ||
      !all(factor_names %in% factor_returns$name)) {
    rlang::abort("factor_names must identify unique available factors.")
  }
  if (!inherits(evaluation_start, "Date") || length(evaluation_start) != 1L ||
      !is.finite(evaluation_start) || format(evaluation_start, "%d") != "01") {
    rlang::abort("evaluation_start must be one first-of-month Date.")
  }
  parameters <- list(min_history = min_history, gamma = gamma,
    cost_per_unit = cost_per_unit, exposure_cap = exposure_cap,
    conditional_exposure_multiple = conditional_exposure_multiple)
  if (!is.null(mvo_exposure_limit)) parameters$mvo_exposure_limit <- mvo_exposure_limit
  purrr::walk(parameters, function(value) {
    if (!is.numeric(value) || length(value) != 1L || !is.finite(value)) {
      rlang::abort("Multifactor parameters must be finite numeric scalars.")
    }
  })
  if (min_history < 2 || min_history != as.integer(min_history) ||
      gamma <= 0 || cost_per_unit < 0 || exposure_cap <= 0 || any(signal$sigma <= 0) ||
      conditional_exposure_multiple < 1 ||
      (!is.null(mvo_exposure_limit) && mvo_exposure_limit <= 0)) {
    rlang::abort("Invalid history, risk aversion, cost, cap or volatility.")
  }

  if (signal_scope == "factor" && !setequal(signal$name, factor_names)) {
    rlang::abort("Own-factor signals must cover exactly the selected factor names.")
  }
  if (allocation == "hew") {
    if (!is.data.frame(cluster_map) ||
        !all(c("name", "cluster") %in% names(cluster_map)) ||
        anyNA(cluster_map) || anyDuplicated(cluster_map$name) ||
        !all(factor_names %in% cluster_map$name)) {
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
    dplyr::summarise(start = min(month_id), .groups = "drop")
  common_start <- max(max(starts$start), min(signal$month_id))
  ends <- data %>% dplyr::group_by(name) %>%
    dplyr::summarise(end = max(month_id), .groups = "drop")
  common_end <- min(min(ends$end), max(signal$month_id))
  if (common_start > common_end) rlang::abort("No common multifactor history.")
  if (common_end < max(signal$month_id)) {
    rlang::abort("Missing factor returns at the end of the signal evaluation period.")
  }

  dates <- tibble::tibble(month_id = seq(common_start, common_end, by = "month"))
  keys <- tidyr::expand_grid(month_id = dates$month_id, name = sort(factor_names))
  data <- keys %>%
    dplyr::left_join(data, by = c("name", "month_id"), relationship = "one-to-one")
  validate_port_data(data, c("name", "month_id"), c("factor_exc", "rf"))
  cash <- dplyr::distinct(data, month_id, rf)
  if (anyDuplicated(cash$month_id)) rlang::abort("RF must agree across factors.")
  timing <- dates %>% dplyr::left_join(cash, by = "month_id", relationship = "one-to-one")
  signal_panel <- if (signal_scope == "factor") {
    keys %>% dplyr::left_join(signal, by = c("name", "month_id"), relationship = "one-to-one")
  } else {
    keys %>% dplyr::left_join(signal, by = "month_id", relationship = "many-to-one")
  }
  validate_port_data(signal_panel, c("name", "month_id"), "sigma")

  ## Pivot using explicit names, then select those same names when entering
  ## matrix algebra. Input row order never determines factor alignment.
  factor_names <- sort(factor_names)
  wide <- data %>% dplyr::select(month_id, name, factor_exc) %>%
    tidyr::pivot_wider(names_from = name, values_from = factor_exc) %>%
    dplyr::arrange(month_id)
  returns <- as.matrix(dplyr::select(wide, dplyr::all_of(factor_names)))
  sigma_wide <- signal_panel %>%
    tidyr::pivot_wider(names_from = name, values_from = sigma) %>% dplyr::arrange(month_id)
  sigma <- as.matrix(dplyr::select(sigma_wide, dplyr::all_of(factor_names)))
  inverse_sigma <- 1 / sigma
  if (any(!is.finite(inverse_sigma))) rlang::abort("Inverse sigma overflow.")
  indices <- which(dates$month_id >= evaluation_start &
                          seq_len(nrow(dates)) > min_history)
  if (length(indices) < 2L) rlang::abort("Insufficient common OOS history for multifactor evaluation.")

  fitted <- purrr::map(indices, function(i) {
    past <- seq_len(i - 1L)
    f <- returns[past, , drop = FALSE]
    z <- inverse_sigma[past, , drop = FALSE]
    k <- length(factor_names)

    ## A fixed normalization within each training window is a harmless change
    ## of units for MVO, but ESSENTIAL to interpret EW on comparable sleeves.
    ## It uses prior returns only, and is applied to the current scaler as well.
    normalization <- rep(1, k)
    if (managed_normalization == "training_vol") {
      normalization <- apply(f, 2, stats::sd) / apply(f * z, 2, stats::sd)
      if (any(!is.finite(normalization) | normalization <= 0)) {
        rlang::abort("Cannot match managed/unmanaged training volatility.")
      }
    }
    z <- sweep(z, 2, normalization, "*")
    current_scaler <- inverse_sigma[i, ] * normalization

    ##Following DeMiguel, UMV = "Unconditional Mean Variance MF", CMV = "Conditional Mean Variance MF"
    conditional_method <- if (signal_scope == "factor") "fcmv" else "cmv"
    ## Fit UMV first: its current exposure fixes the conditional relative limit.
    umv_exposure <- NULL
    purrr::map_dfr(c("umv", conditional_method), function(method) {
      x <- if (method == "umv") f else cbind(f, f * z)
      costs <- if (method == "umv") rep(0, k) else
        c(rep(0, k), cost_per_unit * colMeans(abs(z[-1, , drop = FALSE] -
          z[-nrow(z), , drop = FALSE])))
      basis <- tibble::tibble(name = factor_names, sleeve = "unmanaged")
      if (method != "umv") basis <- dplyr::bind_rows(basis,
        tibble::tibble(name = factor_names, sleeve = "managed"))
      basis <- basis %>% dplyr::left_join(cluster_map, by = "name", relationship = "many-to-one") %>%
        dplyr::mutate(id = paste(name, sleeve, sep = ":"))
      colnames(x) <- basis$id
      loading <- if (method == "umv") rep(1, k) else c(rep(1, k), current_scaler)
      limit <- mvo_exposure_limit
      if (!is.null(limit) && method != "umv") {
        limit <- min(limit, conditional_exposure_multiple * umv_exposure)
      }
      fit <- allocate_portfolio(x, basis, allocation = allocation, gamma = gamma, cost = costs,
        exposure_load = loading, exposure_limit = limit)
      if (method == "umv") umv_exposure <<- sum(fit$coef)
      a <- fit$coef[seq_len(k)]
      b <- if (method == "umv") rep(0, k) else fit$coef[k + seq_len(k)]

      tibble::tibble(
        month_id = dates$month_id[i], name = factor_names, method = method,
        a = a, b = b, sigma = sigma[i, ], scaler = current_scaler,
        normalizer = normalization, w_raw = a + b * current_scaler,
        factor_exc = returns[i, ], rf = timing$rf[i], n_train = i - 1L,
        n_basis = ncol(x), train_end = dates$month_id[i - 1L],
        kkt_residual = fit$kkt_residual,
        covariance_rank = if (allocation == "mvo") fit$covariance_rank else NA_integer_,
        covariance_rcond = if (allocation == "mvo") fit$covariance_rcond else NA_real_,
        optimizer_exposure_limit = if (allocation == "mvo" && !is.null(limit)) limit else NA_real_
      )
    })
  })

  ## This proportional-rescaling diagnostic is distinct from the optimizer
  ## constraint above. With the default MVO bound it is redundant, but retaining
  ## it keeps output schemas comparable with EW/HEW and legacy unbounded runs.
  ## Both versions pay their OWN entry and monthly refit trades.
  weights <- dplyr::bind_rows(fitted) %>%
    dplyr::group_by(method, month_id) %>%
    dplyr::mutate(w_capped = w_raw * min(1, exposure_cap / sum(w_raw))) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(method, name, month_id) %>%
    dplyr::group_by(method, name) %>%
    dplyr::mutate(
      turnover_raw = abs(w_raw - dplyr::lag(w_raw, default = 0)),
      turnover_capped = abs(w_capped - dplyr::lag(w_capped, default = 0))
    ) %>% dplyr::ungroup()

  monthly <- weights %>% dplyr::group_by(method, month_id) %>%
    dplyr::summarise(
      rf = dplyr::first(rf), exposure_raw = sum(w_raw),
      exposure_capped = sum(w_capped),
      gross_raw = sum(w_raw * factor_exc),
      gross_capped = sum(w_capped * factor_exc),
      turnover_raw = sum(turnover_raw), turnover_capped = sum(turnover_capped),
      .groups = "drop"
    ) %>% dplyr::mutate(
      tc_raw = cost_per_unit * turnover_raw, tc_capped = cost_per_unit * turnover_capped,
      net_raw = gross_raw - tc_raw, net_capped = gross_capped - tc_capped
    )
  if (any(!is.finite(as.matrix(dplyr::select(monthly, -method, -month_id))))) {
    rlang::abort("Nonfinite multifactor exposure, cost or return.")
  }

  summary <- purrr::map_dfr(c("raw", "capped"), function(version) {
    monthly %>% dplyr::group_by(method) %>% dplyr::group_modify(function(x, key) {
      gross <- x[[paste0("gross_", version)]]
      net <- x[[paste0("net_", version)]]
      sd_gross <- stats::sd(gross)
      sd_net <- stats::sd(net)
      insolvency_months <- sum(1 + x$rf + net <= 0)
      tibble::tibble(
        version = version, n = nrow(x), start = min(x$month_id),
        end = max(x$month_id), mean_net_ann = 12 * mean(net),
        sharpe_paper = if (!insolvency_months && sd_gross > 0) sqrt(12) * mean(net) / sd_gross else NA_real_,
        sharpe_net = if (!insolvency_months && sd_net > 0) sqrt(12) * mean(net) / sd_net else NA_real_,
        cer_paper_ann = if (!insolvency_months) 12 * (mean(net) - gamma / 2 * stats::var(gross)) else NA_real_,
        cost_ann = 12 * mean(x[[paste0("tc_", version)]]),
        exposure_raw = mean(x$exposure_raw), exposure_capped = mean(x$exposure_capped),
        implementation = if (version == "capped") "proportional_exposure_cap" else
          if (allocation == "mvo" && !is.null(mvo_exposure_limit)) "constrained_mvo" else "unscaled",
        insolvency_months = insolvency_months
      )
    }) %>% dplyr::ungroup()
  })

  list(
    weights = weights, monthly_returns = monthly, strategy_summary = summary,
    diagnostics = dplyr::distinct(weights, month_id, method, train_end,
      n_train, n_basis, kkt_residual, covariance_rank, covariance_rcond, optimizer_exposure_limit),
    settings = list(signal = signal_label, factor_names = factor_names,
      allocation = allocation, signal_scope = signal_scope,
      managed_normalization = managed_normalization,
      mvo_exposure_limit = mvo_exposure_limit,
      conditional_exposure_multiple = conditional_exposure_multiple,
      common_start = common_start, common_end = common_end,
      requested_start = evaluation_start, first_evaluated = min(monthly$month_id),
      min_history = min_history, gamma = gamma, cost_per_unit = cost_per_unit,
      exposure_cap = exposure_cap, cost_scope = "Incremental factor exposure only; no stock netting",
      initial_weight = 0, covariance = "Sample covariance; no regularization")
  )
}
