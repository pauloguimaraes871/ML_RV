# Paired inference for completed monthly portfolio strategies -------------------
# These functions consume saved holding-month returns. They never estimate a
# forecast or an allocation. Return units are monthly decimals throughout.
`%>%` <- dplyr::`%>%`

#' Draw a common stationary-bootstrap calendar
#'
#' @param dates Sorted, consecutive first-of-month Date vector.
#' @param draws Number of bootstrap samples, default 10000.
#' @param block_length Expected geometric block length in months, default five.
#' @param seed Fixed integer seed. The caller's random-number state is restored.
#' @return List containing the original dates, integer resampling indices (one
#'   sample per row), seed, and expected block length. Circular blocks wrap from
#'   the final observation to the first, as in the stationary bootstrap.
draw_portfolio_months <- function(dates, draws = 10000L, block_length = 5,
                                  seed = 20260918L) {
  if (!inherits(dates, "Date") || length(dates) < 3L || anyNA(dates) ||
      any(format(dates, "%d") != "01") ||
      !identical(dates, seq(min(dates), max(dates), by = "month"))) {
    rlang::abort("dates must be sorted, consecutive first-of-month dates (at least three).")
  }
  if (!is.numeric(draws) || length(draws) != 1L || !is.finite(draws) ||
      draws < 2 || draws != floor(draws)) rlang::abort("draws must be an integer of at least two.")
  if (!is.numeric(block_length) || length(block_length) != 1L ||
      !is.finite(block_length) || block_length < 1) {
    rlang::abort("block_length must be at least one month.")
  }
  if (!is.numeric(seed) || length(seed) != 1L || !is.finite(seed) ||
      seed < 0 || seed > .Machine$integer.max || seed != floor(seed)) {
    rlang::abort("seed must be a nonnegative integer.")
  }
  ## Inference must not alter subsequent simulation or estimation randomness.
  had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  old_seed <- if (had_seed) get(".Random.seed", envir = .GlobalEnv) else NULL
  old_kind <- RNGkind()
  on.exit({
    do.call(RNGkind, as.list(old_kind))
    if (had_seed) assign(".Random.seed", old_seed, envir = .GlobalEnv)
    else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
      rm(".Random.seed", envir = .GlobalEnv)
    }
  }, add = TRUE)
  RNGkind("Mersenne-Twister", "Inversion", "Rejection")
  set.seed(seed)
  n <- length(dates)
  indices <- matrix(NA_integer_, nrow = draws, ncol = n)
  indices[, 1L] <- sample.int(n, draws, replace = TRUE)
  for (j in seq.int(2L, n)) {
    restart <- stats::runif(draws) < 1 / block_length
    indices[, j] <- indices[, j - 1L] %% n + 1L
    indices[restart, j] <- sample.int(n, sum(restart), replace = TRUE)
  }
  list(dates = dates, indices = indices, seed = seed, block_length = block_length)
}

#' Validate and align saved portfolio returns for paired inference
#'
#' @param data Long data frame with series_id, month_id, excess_return and
#'   total_return. The latter is the collateralized total return used only for
#'   the insolvency check. Every series must share the full calendar.
#' @return List containing sorted dates and a named matrix of excess returns.
#'   Missing observations, nonfinite returns, duplicate keys and insolvency fail.
align_portfolio_inference <- function(data) {
  required <- c("series_id", "month_id", "excess_return", "total_return")
  if (!is.data.frame(data) || !all(required %in% names(data)) || !nrow(data)) {
    rlang::abort("Inference requires nonempty series_id/month_id/excess_return/total_return data.")
  }
  if (!is.character(data$series_id) || anyNA(data$series_id) ||
      any(!nzchar(data$series_id)) || !inherits(data$month_id, "Date") ||
      anyNA(data$month_id) || anyDuplicated(data[c("series_id", "month_id")])) {
    rlang::abort("Inference series-month keys must be complete and unique.")
  }
  if (!is.numeric(data$excess_return) || !is.numeric(data$total_return) ||
      any(!is.finite(data$excess_return)) || any(!is.finite(data$total_return))) {
    rlang::abort("Inference returns must be finite monthly decimals.")
  }
  if (any(data$total_return <= -1)) rlang::abort("Insolvent strategies cannot enter performance inference.")
  dates <- sort(unique(data$month_id))
  if (length(dates) < 3L || any(format(dates, "%d") != "01") ||
      !identical(dates, seq(min(dates), max(dates), by = "month"))) {
    rlang::abort("Inference requires at least three consecutive first-of-month dates.")
  }
  wide <- data %>%
    dplyr::select(month_id, series_id, excess_return) %>%
    tidyr::pivot_wider(names_from = series_id, values_from = excess_return) %>%
    dplyr::arrange(month_id)
  if (anyNA(wide)) rlang::abort("Strategies must have identical complete monthly coverage.")
  list(dates = dates, returns = as.matrix(dplyr::select(wide, -month_id)))
}

#' Paired stationary-block inference on completed OOS portfolios
#'
#' @param data Long saved-return data accepted by align_portfolio_inference().
#' @param contrasts Data frame with unique comparison_id, family, strategy and
#'   benchmark. Strategy and benchmark name series_id values. family specifies
#'   the prespecified multiplicity family, including all tested endpoints.
#' @param calendar Common resampling plan from draw_portfolio_months(). Reuse it
#'   across factors and reporting scopes to preserve their monthly dependence.
#' @param gamma Risk aversion for net-risk CER, default five.
#' @param confidence Two-sided percentile confidence level, default 0.95.
#' @return Tibble of annualized strategy-minus-benchmark effects for Sharpe,
#'   mean excess return and CER, percentile intervals, one-sided centered-error
#'   p-values, and Holm adjustment within family. All resamples must be defined.
#'   Zero-variance Sharpe is explicitly undefined, never silently dropped.
#'
#' The null is difference <= 0 against difference > 0. Bootstrap errors are
#' centered at the observed difference before comparison with the observed
#' effect. The plus-one correction avoids numerical zero p-values. This is a
#' centered bootstrap test, not DeMiguel's uncentered sign-fraction procedure.
#' Applying stationary blocks to mean and CER is our extension. Inference is
#' conditional on the fitted strategies and does not repeat their estimation.
infer_portfolio_pairs <- function(data, contrasts, calendar, gamma = 5,
                                   confidence = 0.95) {
  aligned <- align_portfolio_inference(data)
  required <- c("comparison_id", "family", "strategy", "benchmark")
  if (!is.data.frame(contrasts) || !all(required %in% names(contrasts)) ||
      !nrow(contrasts) || anyNA(contrasts[required]) ||
      !all(purrr::map_lgl(contrasts[required], is.character)) ||
      any(!nzchar(unlist(contrasts[required]))) || anyDuplicated(contrasts$comparison_id)) {
    rlang::abort("Contrasts need unique comparison_id and nonempty family/strategy/benchmark strings.")
  }
  if (!all(c(contrasts$strategy, contrasts$benchmark) %in% colnames(aligned$returns))) {
    rlang::abort("A contrast references an unavailable return series.")
  }
  if (!is.numeric(gamma) || length(gamma) != 1L || !is.finite(gamma) || gamma <= 0 ||
      !is.numeric(confidence) || length(confidence) != 1L || !is.finite(confidence) ||
      confidence <= 0 || confidence >= 1) rlang::abort("Invalid gamma or confidence level.")
  if (!is.list(calendar)) rlang::abort("The bootstrap calendar must be a resampling-plan list.")
  indices <- calendar$indices
  n <- length(aligned$dates)
  if (!all(c("dates", "indices", "seed", "block_length") %in% names(calendar)) ||
      !is.numeric(calendar$block_length) || length(calendar$block_length) != 1L ||
      !is.finite(calendar$block_length) || calendar$block_length < 1 ||
      !is.numeric(calendar$seed) || length(calendar$seed) != 1L || !is.finite(calendar$seed) ||
      !identical(calendar$dates, aligned$dates) || !is.matrix(indices) ||
      !is.numeric(indices) || ncol(indices) != n || nrow(indices) < 2L ||
      any(!is.finite(indices)) || any(indices < 1 | indices > n | indices != floor(indices))) {
    rlang::abort("The bootstrap calendar must match the exact evaluation dates and contain valid indices.")
  }
  ## Each row is a draw's month frequencies. Matrix products compute means and
  ## variances efficiently without copying every factor for every bootstrap.
  ## Block ordering determines frequencies, while these statistics depend only
  ## on the sampled monthly observations, not their order within each draw.
  counts <- t(vapply(seq_len(nrow(indices)), function(i) {
    tabulate(indices[i, ], nbins = n)
  }, integer(n)))
  metric_draws <- function(r) {
    sampled_mean <- as.vector(counts %*% r) / n
    ## Center before squaring to avoid subtracting nearly equal large moments.
    centered <- r - mean(r)
    sampled_center <- as.vector(counts %*% centered) / n
    variance <- (as.vector(counts %*% (centered^2)) - n * sampled_center^2) / (n - 1)
    # Roundoff can leave tiny negative variances for constant bootstrap draws.
    variance <- pmax(0, variance)
    sd_draw <- sqrt(variance)
    list(mean_ann = 12 * sampled_mean,
         sharpe = ifelse(sd_draw > sqrt(.Machine$double.eps) * max(abs(r)),
                         sqrt(12) * sampled_mean / sd_draw, NA_real_),
         cer_ann = 12 * (sampled_mean - gamma / 2 * variance))
  }
  point_metrics <- function(r) {
    volatility <- stats::sd(r)
    c(mean_ann = 12 * mean(r),
      sharpe = if (volatility > sqrt(.Machine$double.eps) * max(abs(r))) {
        sqrt(12) * mean(r) / volatility
      } else NA_real_,
      cer_ann = 12 * (mean(r) - gamma / 2 * stats::var(r)))
  }
  ids <- unique(c(contrasts$strategy, contrasts$benchmark))
  points <- stats::setNames(purrr::map(ids, ~ point_metrics(aligned$returns[, .x])), ids)
  draws <- stats::setNames(purrr::map(ids, ~ metric_draws(aligned$returns[, .x])), ids)
  result <- purrr::pmap_dfr(contrasts[required], function(comparison_id, family, strategy, benchmark) {
    identical_pair <- identical(aligned$returns[, strategy], aligned$returns[, benchmark])
    purrr::map_dfr(c("sharpe", "mean_ann", "cer_ann"), function(metric) {
      effect <- points[[strategy]][[metric]] - points[[benchmark]][[metric]]
      difference <- draws[[strategy]][[metric]] - draws[[benchmark]][[metric]]
      valid <- is.finite(difference)
      status <- if (!is.finite(effect)) "undefined_observed_metric" else
        if (!all(valid)) "undefined_bootstrap_metric" else
          if (identical_pair) "identical_strategies" else "ok"
      lower <- upper <- p_value <- NA_real_
      if (status %in% c("ok", "identical_strategies")) {
        interval <- stats::quantile(difference, c((1 - confidence) / 2, (1 + confidence) / 2), names = FALSE)
        lower <- interval[1L]
        upper <- interval[2L]
        p_value <- if (identical_pair) 1 else {
          (1 + sum(difference - effect >= effect)) / (length(difference) + 1)
        }
      }
      tibble::tibble(comparison_id, family, strategy, benchmark, metric,
        estimate = effect, ci_lower = lower, ci_upper = upper, p_value,
        n_months = n, draws = nrow(indices), valid_draws = sum(valid),
        block_length = calendar$block_length, seed = calendar$seed,
        confidence, gamma, status)
    })
  })
  result %>%
    dplyr::group_by(family) %>%
    dplyr::mutate(p_holm = stats::p.adjust(p_value, method = "holm", n = dplyr::n())) %>%
    dplyr::ungroup()
}

#' Spanning regression with Newey-West inference
#'
#' @param data Aligned monthly Date, excess_return, benchmark_return columns.
#' @param lag Bartlett-kernel lag in months, default one. No prewhitening.
#' @return One-row tibble with annualized alpha, HAC standard error and normal
#'   two-sided p-value, beta, R-squared and annualized alpha/residual-RMSE appraisal
#'   ratio. Covariance has n/(n-2) finite-sample adjustment. A constant benchmark
#'   fails. Perfect fits return undefined t-statistics and appraisal ratios.
#'   Alpha describes an ex-post opportunity set, not implementable MVO gains.
span_portfolio_returns <- function(data, lag = 1L) {
  if (!is.data.frame(data) || !all(c("month_id", "excess_return", "benchmark_return") %in% names(data))) {
    rlang::abort("Spanning regression requires monthly strategy and benchmark returns.")
  }
  # Reuse calendar/key checks without making assumptions about collateral RF.
  long <- dplyr::bind_rows(
    dplyr::transmute(data, month_id, series_id = "strategy", excess_return, total_return = 0),
    dplyr::transmute(data, month_id, series_id = "benchmark", excess_return = benchmark_return, total_return = 0))
  align_portfolio_inference(long)
  data <- dplyr::arrange(data, month_id)
  n <- nrow(data)
  if (!is.numeric(lag) || length(lag) != 1L || !is.finite(lag) ||
      lag < 0 || lag != floor(lag) || lag >= n) rlang::abort("HAC lag must be an integer between zero and n-1.")
  if (stats::sd(data$benchmark_return) == 0) rlang::abort("A constant benchmark cannot identify spanning beta.")
  fit <- stats::lm(excess_return ~ benchmark_return, data = data)
  if (fit$rank != 2L) rlang::abort("The spanning regression cannot identify intercept and beta.")
  design <- stats::model.matrix(fit)
  residual <- stats::residuals(fit)
  alpha <- unname(stats::coef(fit)[1L])
  beta <- unname(stats::coef(fit)[2L])
  scores <- design * residual
  meat <- crossprod(scores)
  if (lag > 0L) for (j in seq_len(lag)) {
    cross <- crossprod(scores[seq.int(j + 1L, n), , drop = FALSE],
                       scores[seq_len(n - j), , drop = FALSE])
    meat <- meat + (1 - j / (lag + 1)) * (cross + t(cross))
  }
  # QR avoids an explicit inverse of X'X. Bread applies two triangular solves.
  qr_r <- qr.R(fit$qr)
  bread <- backsolve(qr_r, forwardsolve(t(qr_r), diag(2L)))
  covariance <- n / (n - 2) * bread %*% meat %*% bread
  rmse <- sqrt(mean(residual^2))
  perfect <- rmse <= sqrt(.Machine$double.eps) * max(stats::sd(data$excess_return), .Machine$double.eps)
  se <- if (perfect) NA_real_ else sqrt(covariance[1L, 1L])
  statistic <- if (is.finite(se) && se > 0) alpha / se else NA_real_
  tibble::tibble(n_months = n, alpha_ann = 12 * alpha, alpha_se_ann = 12 * se,
    t_alpha = statistic, p_alpha = 2 * stats::pnorm(-abs(statistic)), beta,
    r_squared = if (stats::var(data$excess_return) > 0) 1 - sum(residual^2) /
      sum((data$excess_return - mean(data$excess_return))^2) else NA_real_,
    appraisal_ratio = if (perfect) NA_real_ else sqrt(12) * alpha / rmse,
    hac_lag = lag, status = if (perfect) "perfect_fit" else "ok")
}
