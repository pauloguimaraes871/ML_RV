# DeMiguel, Martin-Utrera and Uppal (2024): Table I replication helpers
# ------------------------------------------------------------------------------
#
# Volatility-managed INDIVIDUAL-factor portfolios (Table I), implemented to
# mirror the authors' replication package (Mendeley Data, DOI
# 10.17632/dmjfztbc4y.3): JF_Table1.m, VMPnew_TC_FFHXZBAB.m (in-sample) and
# VMP_OOS_noshortconst_FFHXZBAB.m (out-of-sample). Each function below names the
# MATLAB lines it reproduces. Conventions taken from that code, not guessed:
#
#   * the managed factor is r_{t+1} * c / sigma2_t, where sigma2_t is the SAMPLE
#     variance (N - 1) of the factor's daily returns in the previous calendar
#     month (checked against their VolatilityF: max ratio error 7e-16);
#   * c = sd(r) / sd(r / sigma2) over the whole available sample. It is a scale
#     constant: portfolio returns, Sharpe ratios and p-values do not depend on
#     it, except through the |b2| <= 0.001 fallback of the in-sample code;
#   * weights are SIGNED and unconstrained, b = (1 / gamma) Sigma^-1 mu, gamma = 5;
#   * the sample starts one month after the first daily month (the variance lag),
#     so with M = 120 both the in-sample window and the OOS period are rows
#     M + 1 to T (February 1977 - December 2020, 527 months in the paper);
#   * Sharpe = sqrt(12) * mean / sd (sample sd); p-values are the uncentred
#     stationary-bootstrap fraction of dmu_StatisticsPolitisRomano.m.
#
# Costs (Panels B, D, E) are out of scope here: the paper prices stock-level
# trades. See docs/DEMIGUEL_REPLICATION.md.

  `%>%` <- dplyr::`%>%`

  ## Table I as published, Panels A and C (the cost-free panels). SR(r_k) is
  ## the same in both panels (footnote 20).
  demiguel_table1_paper <- tibble::tibble(
    name = c("mkt", "smb", "hml", "rmw", "cma", "umd", "roe", "ia", "bab"),
    sr_unmanaged = c(.530, .208, .170, .506, .399, .474, .722, .508, .880),
    sr_is = c(.585, .246, .215, .739, .419, 1.088, 1.153, .621, 1.397),
    p_is = c(.242, .366, .337, .033, .311, .000, .001, .094, .000),
    sr_oos = c(.408, .068, .194, .527, .355, 1.035, 1.094, .605, 1.321),
    p_oos = c(.899, .929, .390, .467, .897, .000, .000, .062, .000)
  )

#' Own-factor realized variance, stamped to the holding month
#'
#' daily: name/date/ret, decimal daily returns. For each factor and calendar
#' month t, variance = stats::var() of that month's daily returns (sample
#' variance, N - 1), returned under month_id = t + 1: the month whose return it
#' scales. A month with fewer than two returns, or zero variance, aborts.
#' Calendar arithmetic (not row lags) stamps the month, so a gap in the daily
#' history cannot turn into a longer lag.
prepare_factor_variance <- function(daily) {

  validate_port_data(daily, c("name", "date"), "ret", monthly = FALSE)

  variance <- daily %>%
    dplyr::mutate(month = lubridate::floor_date(date, "month")) %>%
    dplyr::group_by(name, month) %>%
    dplyr::summarise(n_days = dplyr::n(), variance = stats::var(ret), .groups = "drop")

  if (any(variance$n_days < 2L) || any(!is.finite(variance$variance)) ||
      any(variance$variance <= 0)) {
    rlang::abort("Each factor-month needs at least two daily returns and positive variance.")
  }

  variance %>%
    dplyr::transmute(
      name,
      month_id = lubridate::add_with_rollback(month, lubridate::period(month = 1)),
      variance
    ) %>%
    dplyr::arrange(name, month_id)
}

#' Assemble one factor's Table I sample
#'
#' monthly: name/month_id/factor_exc. variance: output of
#' prepare_factor_variance(). Keeps the months where both exist inside
#' [start, end], which must be consecutive (no silent gaps). Adds the managed
#' return r * c / variance, with c = sd(r) / sd(r / variance) over the sample
#' (VMPnew_TC_FFHXZBAB.m lines 20-23).
build_table1_sample <- function(monthly, variance, factor_name, start, end) {

  data <- monthly %>%
    dplyr::filter(name == factor_name, month_id >= start, month_id <= end) %>%
    dplyr::select(month_id, factor_exc) %>%
    dplyr::inner_join(dplyr::filter(variance, name == factor_name) %>%
                        dplyr::select(month_id, variance),
                      by = "month_id", relationship = "one-to-one") %>%
    dplyr::arrange(month_id)

  if (!nrow(data) || min(data$month_id) != start || max(data$month_id) != end) {
    rlang::abort(paste("Return/variance coverage does not span the window for", factor_name))
  }
  validate_port_data(data, "month_id", c("factor_exc", "variance"))

  scale <- stats::sd(data$factor_exc) / stats::sd(data$factor_exc / data$variance)
  dplyr::mutate(data, managed = factor_exc * scale / variance, name = factor_name, .before = 1)
}

#' Signed mean-variance weights on the two sleeves, (1 / gamma) Sigma^-1 mu
#'
#' x: two-column return matrix (unmanaged, managed). Reuses allocate_portfolio()
#' (signed branch), which solves on standardized columns and aborts on a
#' singular or ill-conditioned covariance instead of adding a ridge.
mv_signed_weights <- function(x, gamma = 5) {
  basis <- tibble::tibble(id = colnames(x), name = "factor", sleeve = colnames(x),
                          cluster = "single_factor")
  allocate_portfolio(x, basis, allocation = "mvo", gamma = gamma, nonnegative = FALSE)$coef
}

#' Panel A: in-sample volatility-managed portfolio
#'
#' sample: build_table1_sample() output. Weights are estimated and evaluated on
#' rows M + 1 to T, then rescaled to b / |b2|; if |b2| <= 0.001 the portfolio is
#' the unmanaged factor (VMPnew_TC_FFHXZBAB.m lines 51-63). Returns the
#' evaluation months with the unmanaged and the combined gross returns.
table1_in_sample <- function(sample, M = 120L, gamma = 5) {

  if (nrow(sample) <= M + 2L) rlang::abort("Too few months after the first M.")
  window <- sample[(M + 1L):nrow(sample), ]
  x <- cbind(unmanaged = window$factor_exc, managed = window$managed)

  b <- mv_signed_weights(x, gamma)
  b <- if (abs(b[2]) > .001) b / abs(b[2]) else c(1, 0)

  tibble::tibble(month_id = window$month_id, unmanaged = window$factor_exc,
                 managed_combo = as.vector(x %*% b), a = b[1], b = b[2])
}

#' Panel C: out-of-sample volatility-managed portfolio
#'
#' For each month M + i, weights use rows 1 to M + i - 1 only (expanding window
#' from the first sample month) and are applied to row M + i
#' (VMP_OOS_noshortconst_FFHXZBAB.m lines 49-58). No rescaling or cap.
table1_out_of_sample <- function(sample, M = 120L, gamma = 5) {

  if (nrow(sample) <= M + 2L) rlang::abort("Too few months after the first M.")
  x <- cbind(unmanaged = sample$factor_exc, managed = sample$managed)

  purrr::map_dfr((M + 1L):nrow(sample), function(t) {
    b <- mv_signed_weights(x[seq_len(t - 1L), , drop = FALSE], gamma)
    tibble::tibble(month_id = sample$month_id[t], unmanaged = x[t, 1],
                   managed_combo = sum(x[t, ] * b), a = b[1], b = b[2],
                   train_end = sample$month_id[t - 1L])
  })
}

#' Annualized Sharpe ratio, sqrt(12) * mean / sd (sample sd)
annual_sharpe <- function(x) {
  if (!is.numeric(x) || length(x) < 2L || any(!is.finite(x))) {
    rlang::abort("Sharpe ratio needs at least two finite returns.")
  }
  s <- stats::sd(x)
  if (s <= 0) rlang::abort("Sharpe ratio is undefined for a constant return series.")
  sqrt(12) * mean(x) / s
}

#' One-sided bootstrap p-value of SR(policy) - SR(benchmark), DeMiguel's way
#'
#' Mirrors dmu_StatisticsPolitisRomano.m with onesided = 1: resample the PAIR
#' of realized return series with a common stationary-bootstrap calendar, form
#' Delta = SR(benchmark) - SR(policy) per draw, and return 1 - F(x_j), where F
#' is the empirical CDF of Delta and x_j the support point closest to zero.
#' This is the uncentred fraction of draws in which the policy does NOT beat
#' the benchmark (ties at x_j excluded). Identical series return 0.5, as in
#' their code. calendar: draw_portfolio_months() output for these dates; its
#' indices are the resampling plan (estimation is not repeated).
demiguel_sr_pvalue <- function(benchmark, policy, calendar) {

  if (!is.numeric(benchmark) || !is.numeric(policy) ||
      length(benchmark) != length(policy) || any(!is.finite(c(benchmark, policy)))) {
    rlang::abort("Benchmark and policy must be finite return vectors of equal length.")
  }
  indices <- calendar$indices
  if (!is.matrix(indices) || ncol(indices) != length(benchmark) ||
      any(indices < 1L | indices > length(benchmark))) {
    rlang::abort("The bootstrap calendar does not match these returns.")
  }
  if (sum(abs(policy - benchmark)) / sum(abs(benchmark)) < .Machine$double.eps) return(.5)

  sharpe_rows <- function(m) rowMeans(m) / apply(m, 1, stats::sd)
  delta <- sharpe_rows(matrix(benchmark[indices], nrow(indices))) -
    sharpe_rows(matrix(policy[indices], nrow(indices)))
  if (any(!is.finite(delta))) rlang::abort("A bootstrap draw has an undefined Sharpe ratio.")

  support <- sort(unique(delta))
  x_j <- support[which.min(abs(support))]
  mean(delta > x_j)
}

#' Table I Panels A and C for every factor
#'
#' monthly, daily: the DeMiguel return artifacts. start: first sample month
#' (one month after the first daily month common to all nine factors; February
#' 1967 in the paper). end: last month (December 2020). M, gamma: 120 and 5.
#' draws/block_length/seed: bootstrap settings (10,000 and 5 in the paper; the
#' authors' MATLAB stream cannot be reproduced, so p-values carry Monte Carlo
#' error). Returns list(table, returns): one row per factor, and the monthly
#' unmanaged/managed-combination returns behind it.
run_table1_cost_free <- function(monthly, daily, start, end, M = 120L, gamma = 5,
                                 draws = 10000L, block_length = 5, seed = 20261011L) {

  variance <- prepare_factor_variance(daily)
  factors <- demiguel_table1_paper$name
  if (!all(factors %in% monthly$name) || !all(factors %in% variance$name)) {
    rlang::abort("Monthly returns and daily variances must cover all nine factors.")
  }

  results <- purrr::map(stats::setNames(factors, factors), function(k) {
    sample <- build_table1_sample(monthly, variance, k, start, end)
    list(is = table1_in_sample(sample, M, gamma), oos = table1_out_of_sample(sample, M, gamma))
  })

  ## One calendar for all factors and both panels, so their p-values share
  ## draws (the evaluation months coincide by construction; checked here).
  dates <- results[[1]]$is$month_id
  purrr::walk(results, function(r) {
    if (!identical(r$is$month_id, dates) || !identical(r$oos$month_id, dates)) {
      rlang::abort("In-sample and OOS evaluation months must coincide across factors.")
    }
  })
  calendar <- draw_portfolio_months(dates, draws = draws, block_length = block_length, seed = seed)

  table <- purrr::imap_dfr(results, function(r, k) {
    tibble::tibble(
      name = k, n = nrow(r$is), start = min(r$is$month_id), end = max(r$is$month_id),
      sr_unmanaged = annual_sharpe(r$is$unmanaged),
      sr_is = annual_sharpe(r$is$managed_combo),
      p_is = demiguel_sr_pvalue(r$is$unmanaged, r$is$managed_combo, calendar),
      sr_oos = annual_sharpe(r$oos$managed_combo),
      p_oos = demiguel_sr_pvalue(r$oos$unmanaged, r$oos$managed_combo, calendar),
      a_is = r$is$a[1], b_is = r$is$b[1]
    )
  })

  returns <- purrr::imap_dfr(results, function(r, k) {
    dplyr::bind_rows(dplyr::mutate(r$is, panel = "A_in_sample"),
                     dplyr::mutate(r$oos, panel = "C_out_of_sample")) %>%
      dplyr::mutate(name = k, .before = 1)
  })

  list(table = table, returns = returns,
       settings = list(start = start, end = end, M = M, gamma = gamma, draws = draws,
                       block_length = block_length, seed = seed,
                       weights = "signed, (1/gamma) Sigma^-1 mu; in-sample rescaled b/|b2|"))
}

#' Compare a Table I result with the published values
compare_table1 <- function(table) {
  table %>%
    dplyr::select(name, sr_unmanaged, sr_is, p_is, sr_oos, p_oos) %>%
    tidyr::pivot_longer(-name, names_to = "statistic", values_to = "ours") %>%
    dplyr::inner_join(tidyr::pivot_longer(demiguel_table1_paper, -name,
                                          names_to = "statistic", values_to = "paper"),
                      by = c("name", "statistic"), relationship = "one-to-one") %>%
    dplyr::mutate(difference = ours - paper)
}
