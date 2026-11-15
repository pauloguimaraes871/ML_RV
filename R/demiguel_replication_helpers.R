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
  dplyr::mutate(data, managed = factor_exc * scale / variance, scale = scale,
                name = factor_name, .before = 1)
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

#' Value of a scalar or per-factor setting for factor k
#'
#' x is either one value used for every factor, or a vector named by factor.
factor_setting <- function(x, k) {
  if (is.null(names(x))) {
    if (length(x) != 1L) rlang::abort("An unnamed setting must be a single value.")
    return(x)
  }
  if (!k %in% names(x)) rlang::abort(paste("Setting has no value for factor", k))
  x[[k]]
}

#' Bootstrap calendars, shared across factors with the same evaluation months
#'
#' dates: list of evaluation-month vectors by factor. Factors with identical
#' months share one plan (as in the paper, where all nine coincide); a factor
#' with a different window gets its own plan from the same seed.
table1_calendars <- function(dates, draws, block_length, seed) {
  keys <- purrr::map_chr(dates, ~ paste(format(.x), collapse = ","))
  plans <- purrr::map(stats::setNames(unique(keys), unique(keys)), function(key) {
    draw_portfolio_months(dates[[match(key, keys)]], draws = draws,
                          block_length = block_length, seed = seed)
  })
  purrr::map(keys, ~ plans[[.x]]) %>% stats::setNames(names(dates))
}

#' Table I Panels A and C for every factor
#'
#' monthly, daily: the DeMiguel return artifacts. start: first sample month
#' (one month after the first daily month common to all nine factors; February
#' 1967 in the paper). end: last month (December 2020). M, gamma: 120 and 5.
#' start, end and M may also be vectors named by factor, for per-factor windows.
#' variance: optional name/month_id/variance signal stamped to the holding
#' month, replacing the paper's lagged daily variance (e.g. an ML forecast);
#' when NULL it is built from daily.
#' draws/block_length/seed: bootstrap settings (10,000 and 5 in the paper; the
#' authors' MATLAB stream cannot be reproduced, so p-values carry Monte Carlo
#' error). Returns list(table, returns): one row per factor, and the monthly
#' unmanaged/managed-combination returns behind it.
run_table1_cost_free <- function(monthly, daily, start, end, M = 120L, gamma = 5,
                                 draws = 10000L, block_length = 5, seed = 20261011L,
                                 variance = NULL) {

  if (is.null(variance)) variance <- prepare_factor_variance(daily)
  validate_port_data(variance, c("name", "month_id"), "variance", consecutive = FALSE)
  if (any(variance$variance <= 0)) rlang::abort("Scaling signals must be positive.")
  factors <- demiguel_table1_paper$name
  if (!all(factors %in% monthly$name) || !all(factors %in% variance$name)) {
    rlang::abort("Monthly returns and daily variances must cover all nine factors.")
  }

  results <- purrr::map(stats::setNames(factors, factors), function(k) {
    m <- factor_setting(M, k)
    sample <- build_table1_sample(monthly, variance, k, factor_setting(start, k), factor_setting(end, k))
    list(is = table1_in_sample(sample, m, gamma), oos = table1_out_of_sample(sample, m, gamma))
  })

  ## Within a factor both panels share their months and one bootstrap plan;
  ## factors with identical windows (all nine in the paper) share draws.
  purrr::walk(results, function(r) {
    if (!identical(r$is$month_id, r$oos$month_id)) {
      rlang::abort("In-sample and OOS evaluation months must coincide within a factor.")
    }
  })
  calendars <- table1_calendars(purrr::map(results, ~ .x$is$month_id), draws, block_length, seed)

  table <- purrr::imap_dfr(results, function(r, k) {
    calendar <- calendars[[k]]
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

#' Write an output only when its content changes
#'
#' Rewriting a byte-identical file still changes its timestamp, which the
#' OneDrive backup check (C:\git-backup) reports as a changed data file. Writes
#' to a temporary file, compares SHA-256 with the existing file and replaces it
#' only if they differ. type: "csv" (utils::write.csv, no row names) or "rds".
#' Returns TRUE if the file was (re)written.
save_if_changed <- function(x, path, type = c("csv", "rds")) {
  type <- match.arg(type)
  tmp <- tempfile(fileext = paste0(".", type))
  on.exit(unlink(tmp), add = TRUE)
  if (type == "csv") utils::write.csv(x, tmp, row.names = FALSE) else saveRDS(x, tmp)
  if (file.exists(path) &&
      identical(digest::digest(file = tmp, algo = "sha256"), digest::digest(file = path, algo = "sha256"))) {
    return(invisible(FALSE))
  }
  if (!file.copy(tmp, path, overwrite = TRUE)) rlang::abort(paste("Could not write", path))
  invisible(TRUE)
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

# Table I, Panels B, D and E: transaction costs from the stock-level data
# ------------------------------------------------------------------------------
#
# Costs come from R/demiguel_cost_helpers.R (the authors' stock weights and
# kappas). Each formula below names the MATLAB line it reproduces in
# VMPnew_TC_FFHXZBAB.m (in-sample) or VMP_OOS_noshortconst_FFHXZBAB.m (OOS).

  demiguel_table1_paper_costs <- tibble::tibble(
    name = demiguel_table1_paper$name,
    sr_unmanaged_net = c(.519, .125, .053, .356, .159, .114, .311, .107, .606),
    ## ROE .331 (paper p. 3871; package BarrosoDetzel_Cederburg_FFHXZBAB .tex).
    ## Corrected 2026-10-25: it had been mistyped as .311, the SR(r_k) row.
    sr_is_net = c(.521, .125, .053, .356, .159, .251, .331, .107, .703),
    p_is_net = c(.464, .500, .500, .500, .500, .249, .407, .500, .161),
    sr_oos_net = c(.324, -.295, -.041, -.453, -.047, .194, .269, -.127, .690),
    p_oos_net = c(.976, 1.000, .879, 1.000, 1.000, .342, .672, 1.000, .281),
    sr_oos_td = c(.433, .035, .089, .226, .153, .209, .324, .193, .746),
    p_oos_td = c(.917, .858, .243, .965, .547, .097, .405, .029, .064)
  )

#' Covariance used by the authors' cost-aware optimizers
#'
#' They shrink with cov1para.m when cond(Sigma) > 1e9. That never happens for a
#' two-sleeve problem on these data; abort rather than silently differ.
demiguel_covariance <- function(x) {
  sigma <- stats::cov(x)
  if (kappa(sigma, exact = TRUE) > 1e9) {
    rlang::abort("Covariance condition number exceeds 1e9: the authors would shrink (cov1para).")
  }
  sigma
}

#' Exact mean-variance weights with separable L1 costs (pppquadprog_sc.m)
#'
#' Solves min gamma/2 b'Sigma b - mu'b + sum_j cost_j |b_j| exactly, by
#' enumerating sign patterns (K is small) and checking the KKT conditions:
#' on the support, gamma Sigma b - mu + cost * sign(b) = 0; off it,
#' |gamma (Sigma b)_j - mu_j| <= cost_j. The problem is strictly convex, so the
#' feasible pattern is the unique optimum.
mv_l1_separable <- function(mu, sigma, gamma, cost, tol = 1e-12) {

  k <- length(mu)
  if (k > 8L || nrow(sigma) != k || length(cost) != k || any(cost < 0)) {
    rlang::abort("mv_l1_separable expects a small problem with nonnegative costs.")
  }
  patterns <- as.matrix(expand.grid(rep(list(c(-1, 0, 1)), k)))
  candidates <- purrr::map(seq_len(nrow(patterns)), function(p) {
    s <- patterns[p, ]
    on <- s != 0
    b <- rep(0, k)
    if (any(on)) b[on] <- solve(sigma[on, on, drop = FALSE], mu[on] - cost[on] * s[on]) / gamma
    if (any(on) && any(sign(b[on]) != s[on])) return(NULL)
    gradient <- gamma * as.vector(sigma %*% b) - mu
    if (any(abs(gradient[!on]) > cost[!on] + tol * (1 + abs(mu[!on])))) return(NULL)
    list(b = b, value = gamma / 2 * sum(b * (sigma %*% b)) - sum(mu * b) + sum(cost * abs(b)))
  }) %>% purrr::compact()
  if (!length(candidates)) rlang::abort("No KKT point found for the separable-cost problem.")
  candidates[[which.min(purrr::map_dbl(candidates, "value"))]]$b
}

#' Prepare the cancellation rays used by the two-sleeve cost optimizer
#'
#' phi has two columns, in the same sleeve order as the return matrix and
#' optimization weights b = (u, m). Each row normally represents one stock
#' in one trade month: (p, q) are its SIGNED trades at unit sleeve coefficients,
#' already multiplied by that stock's transaction-cost rate. A negative entry
#' describes a sale, not a negative expense. For arbitrary sleeve coefficients,
#' the row's signed net amount is p*u + q*m; its COST is abs(p*u + q*m).
#'
#' The row's net trade cancels on the line p*u + q*m = 0 in the weight plane
#' (horizontal = u, vertical = m). Example: (p, q) = (0.00104, -0.00242) gives
#' m = 0.429752*u. The line has two opposite half-lines starting at (0, 0);
#' these are its two rays. They mark where this row's net trade changes sign.
#' All rows' rays partition the plane into wedges, called cones below. Inside
#' one cone every row's trade SIGN stays fixed, although its amount can vary.
#' Consequently, absolute values can be removed using those signs, and the
#' total cost has a linear formula l[1]*u + l[2]*m within that cone.
#'
#' This function does NOT choose portfolio weights. It prepares the boundary
#' directions and the changes in the cost coefficients needed to visit cones
#' counter-clockwise. group labels each phi row (usually by trade month).
#' window_rays() later selects rows with group <= g and scales their cost
#' coefficients, e.g. by 1/D to obtain an average MONTHLY portfolio cost. D is
#' the number of selected monthly trade observations, not the number of stocks
#' or stock-month rows: costs add across stocks, then average across months.
#'
#' Returns phi, s0 and group in filtered input-row order; angle, delta and
#' ray_group are in sorted RAY order. Each retained row supplies two ray
#' records, so ray arrays have twice as many entries as phi has rows. Equal
#' directions are retained; several stock-month rows can share a boundary.
phi_rays <- function(phi, group) {
  ## A (0, 0) row costs zero for every possible (u, m), so it contributes
  ## neither a cost coefficient nor a meaningful cancellation boundary.
  keep <- rowSums(phi != 0) > 0
  phi <- phi[keep, , drop = FALSE]
  group <- group[keep]

  ## atan2(y, x) gives the direction of the TRADE-COEFFICIENT vector (p, q),
  ## not the cancellation line. The cancellation directions are perpendicular
  ## to it: (p, q) dot (-q, p) = -p*q + q*p = 0. Rotating beta by +/- pi/2
  ## (90 degrees) therefore gives the line's two opposite ray directions.
  beta <- atan2(phi[, 2], phi[, 1])
  angle <- c(beta + pi / 2, beta - pi / 2) %% (2 * pi)

  ## %% wraps negative angles into [0, 2*pi), allowing a single circular sort.
  ## Represent a ray exactly at direction zero by 2*pi: the sweep starts just
  ## ABOVE the positive horizontal axis, so it crosses that ray at the end.
  angle[angle == 0] <- 2 * pi

  ## When a row's signed net trade is positive, its cost coefficients are
  ## +phi_row; when negative, they are -phi_row. Crossing beta + pi/2
  ## counter-clockwise switches +phi_row to -phi_row, a change of -2*phi_row.
  ## At beta - pi/2 the switch is reversed, giving +2*phi_row. These updates
  ## let the optimizer change the total slope without repricing every row.
  delta <- rbind(-2 * phi, 2 * phi)

  ## Starting signs for direction 0+, equivalent to b = (1, epsilon) with
  ## epsilon positive and sufficiently small. A nonzero p determines the
  ## sign; if p is exactly zero, q determines it. window_rays() uses these
  ## signs to compute l0 = sum_rows(s0 * phi_row), with any requested scaling.
  s0 <- ifelse(phi[, 1] != 0, sign(phi[, 1]), sign(phi[, 2]))
  ord <- order(angle)

  ## Sort each ray's direction, slope update and month label TOGETHER.
  ## Duplicate group labels because each retained phi row creates two rays.
  list(phi = phi, s0 = s0, group = group,
       angle = angle[ord], delta = delta[ord, , drop = FALSE],
       ray_group = c(group, group)[ord])
}

#' Subset a phi_rays() structure to rows with group <= g, scaling phi by scale
window_rays <- function(rays, g, scale = 1) {
  rows <- rays$group <= g
  keep <- rays$ray_group <= g
  list(l0 = colSums(rays$phi[rows, , drop = FALSE] * rays$s0[rows]) * scale,
       angle = rays$angle[keep], delta = rays$delta[keep, , drop = FALSE] * scale)
}

#' Two-sleeve mean-variance weights with netted stock-level costs (pppfminunc.m)
#'
#' Choose signed sleeve coefficients b = (u, m) to minimize
#'   gamma/2 * b' Sigma b - mu' b + sum_rows abs(A_row' b).
#' mu and sigma are the two sleeves' estimated gross monthly mean returns and
#' covariance; gamma is positive risk aversion. A represents the selected
#' kappa-weighted stock trades divided by D, the number of monthly trade
#' observations. Thus the last term is an average MONTHLY portfolio expense,
#' comparable to the monthly payoff term, rather than an accumulated expense
#' that increases merely because a longer history was supplied. The caller's
#' window_rays(..., scale = 1/D) has already applied this normalization to l0
#' and delta; this function must not divide the cost coefficients again.
#'
#' rays is window_rays() output: starting cost coefficients l0, sorted boundary
#' angles and coefficient changes delta. The geometry is two-dimensional even
#' with many stocks and months: there are only two unknown sleeve coefficients.
#' No nonnegativity or sum-to-one restriction is imposed on those coefficients.
#' The argument below assumes a positive-definite covariance and gamma > 0.
#'
#' Within a cone the stock-trade signs are fixed, so the cost is l_c' b and the
#' objective is gamma/2*b'Sigma*b - (mu-l_c)'b. Its unconstrained stationary
#' candidate is Sigma^-1*(mu-l_c)/gamma, but is valid for that formula only if
#' it lies inside the associated cone. A constrained minimum can instead be
#' on a boundary ray or at zero. The intended search compares these cases.
#' The authors use a smoothed quasi-Newton optimizer for the underlying problem;
#' this implementation works directly with its piecewise-linear absolute costs.
#'
#' One call estimates ONE portfolio for the selected window. table1_cost_panels()
#' calls it again for each OOS holding month with updated moments and selected
#' cost rows. The inner cone search considers alternatives for that one
#' portfolio; it does not select a different portfolio for each historical row.
#' Returns the selected two coefficients without the in-sample rescaling.
mv_l1_phi_2d <- function(mu, sigma, gamma, rays) {

  ## With no nonzero trade rows, costs vanish and ordinary signed MV applies.
  inv <- solve(sigma)
  if (!length(rays$angle)) return(as.vector(inv %*% mu) / gamma)

  ## Build the two cost coefficients for every angular interval. Starting at
  ## l0, add delta at the first ray, then the second, etc. Each ROW of l is a
  ## cone's formula C(b) = l[c, 1]*u + l[c, 2]*m. l has one more row than there
  ## are ray records; lower[c] and upper[c] bound that row's angular interval.
  ## Coincident rays create zero-width intervals; the closed check below only
  ## accepts a candidate there if it lies exactly on that boundary.
  cumulative <- matrix(apply(rays$delta, 2, cumsum), ncol = 2)
  l <- rbind(rays$l0, sweep(cumulative, 2, rays$l0, "+"))
  lower <- c(0, rays$angle)
  upper <- c(rays$angle, 2 * pi)

  ## Solve gamma*Sigma*b_c = mu-l_c for every cone simultaneously. Transposing
  ## l makes each COLUMN a cone, so b is a 2 x number_of_cones matrix:
  ## b[1, c] is its candidate unmanaged coefficient; b[2, c] is managed.
  ## These are unconstrained candidates for each local formula, NOT necessarily
  ## optima constrained to that cone. A candidate outside its assumed cone was
  ## calculated with the wrong cost formula for its location and is rejected.
  b <- inv %*% (mu - t(l)) / gamma

  ## atan2(managed, unmanaged) describes each candidate's direction from the
  ## origin. It does not alter its weights. It operates column by column here:
  ## b[2, ] supplies all vertical coordinates and b[1, ] all horizontal ones.
  ## %% (2*pi) wraps negative directions into the same circular range as the
  ## ray angles (e.g. -5 degrees and 355 degrees describe the same direction).
  psi <- atan2(b[2, ], b[1, ]) %% (2 * pi)
  psi[psi == 0] <- 2 * pi

  ## Match candidate c to its OWN lower[c]/upper[c] interval. Squared weights
  ## sum to zero only at the origin, which is considered separately below.
  ## CLOSED intervals: on a cone's boundary the cost still equals that cone's
  ## linear formula, so l_c lies in the cost subdifferential there and a
  ## stationary candidate on the closed cone satisfies the optimality
  ## condition of the true problem (the unique optimum, by strict convexity).
  ## Fixed 2026-10-25: strict > and < used to drop a stationary candidate with
  ## u > 0, m = 0 exactly (psi = 2*pi, the artificial 0/2*pi seam, which is not
  ## a trade ray), and the ray search did not recover it. An exact-optimality
  ## scan of all 28,458 saved Panel E solutions found no affected window.
  inside <- which(psi >= lower & psi <= upper & colSums(b^2) > 0)

  ## Zero sleeve exposure is always available and has objective zero. Under
  ## strict convexity a stationary point genuinely inside a cone is a global
  ## optimum, so only the first accepted interior candidate needs evaluation.
  ## Evaluate with that cone's formula, retaining it only if better than cash.
  best <- list(b = c(0, 0), value = 0)
  if (length(inside)) {
    k <- inside[1]
    v <- gamma / 2 * sum(b[, k] * (sigma %*% b[, k])) - sum((mu - l[k, ]) * b[, k])
    if (v < best$value) best <- list(b = b[, k], value = v)
  }

  ## Check the corners as well as cone interiors. Each COLUMN of d is a unit
  ## direction vector (cos(angle), sin(angle)) for a ray. Along it, b = s*d
  ## with size s >= 0; the sleeve ratio is fixed and only total size varies.
  d <- rbind(cos(rays$angle), sin(rays$angle))

  ## l[-1, ] pairs each ray with the cone immediately after crossing it.
  ## slope = l'd is the nonnegative COST PER UNIT SIZE along the ray, not a
  ## signed stock trade. Either adjacent cone gives the same boundary cost:
  ## each row changing sign has zero net trade there, so cost is continuous.
  slope <- colSums(t(l[-1, , drop = FALSE]) * d)

  ## gross benefit per unit size = mu'd; net benefit = mu'd - slope.
  ## If this is nonpositive, the best feasible size on that ray is zero.
  ## curvature = gamma*d'Sigma*d measures the quadratic risk penalty's growth.
  ## With gain g and curvature h, F(s) = h*s^2/2 - g*s. Its best nonnegative
  ## size is g/h (gain has been floored at zero), with value -g^2/(2*h).
  gain <- pmax(0, colSums(mu * d) - slope)
  curvature <- gamma * colSums(d * (sigma %*% d))
  ray_value <- -gain^2 / (2 * curvature)

  ## Choose the best ray, compare it with the current interior/zero candidate,
  ## and turn its optimal size back into the two actual sleeve coefficients.
  k <- which.min(ray_value)
  if (ray_value[k] < best$value) best <- list(b = d[, k] * gain[k] / curvature[k], value = ray_value[k])
  best$b
}

#' Table I Panels B, D and E for one factor
#'
#' sample: build_table1_sample() output (months 1..T, February 1967 onwards).
#' stock: read_demiguel_stock_data() output. Mirrors the authors' code:
#'   B  in-sample (rows M+1..T): weights from the separable-cost problem with
#'      the sleeves' average own costs, rescaled b/|b2|;
#'      net = gross - mean_t |PTC_t . b| (lines 70-79 of VMPnew_TC_FFHXZBAB.m).
#'   D  OOS: Panel C weights; net = gross - mean_i (|b1_i| PTC_1 + |b2_i| PTC_2)
#'      at the holding month (lines 60-61, 88 of VMP_OOS_noshortconst).
#'   E  OOS: weights from the stock-level cost problem over months 1..t, with
#'      trades into months 2..t+1 as in their window (line 65); net = gross -
#'      mean cost of the current weights (lines 69-75, 92), entry from cash in
#'      the first month.
#' Benchmarks: Panel B, in-sample unmanaged net of its average own cost;
#' Panels D and E, OOS unmanaged net of its own costs, entry included.
table1_cost_panels <- function(sample, stock, factor_key, M = 120L, gamma = 5) {

  n <- nrow(sample)
  x <- cbind(unmanaged = sample$factor_exc, managed = sample$managed)
  multiplier <- sample$scale / sample$variance
  sleeves <- c("unmanaged", "managed")
  exposures <- dplyr::bind_rows(
    tibble::tibble(month_id = sample$month_id, sleeve = "managed", factor = factor_key, w = multiplier),
    tibble::tibble(month_id = sample$month_id, sleeve = "unmanaged", factor = factor_key, w = 1)
  )

  ## PTCs: each sleeve's own stock-level cost at unit weight; row 1 is NA
  ptc <- stock_cost(stock, exposures, "sleeve", charge_entry = FALSE, by_sleeve = TRUE) %>%
    tidyr::pivot_wider(names_from = sleeve, values_from = cost) %>%
    dplyr::arrange(month_id)
  if (!identical(ptc$month_id, sample$month_id)) rlang::abort("Sleeve costs misaligned with the sample.")
  ptc <- as.matrix(ptc[, sleeves])

  ## Phiext: kappa-weighted trades of both sleeves, months 2..T (row t of the sample)
  trades <- stock_trade_matrices(stock, exposures)
  if (!identical(trades$months, sample$month_id[-1])) rlang::abort("Trade matrices misaligned with the sample.")
  phi <- purrr::map(trades$phi, ~ .x[, sleeves, drop = FALSE])
  rays <- phi_rays(do.call(rbind, phi), rep(seq_along(phi) + 1L, purrr::map_int(phi, nrow)))

  ## Panel B
  is_rows <- (M + 1L):n
  tcp <- colMeans(ptc[is_rows, , drop = FALSE])
  b <- mv_l1_separable(colMeans(x[is_rows, ]), demiguel_covariance(x[is_rows, ]), gamma, tcp)
  b <- if (abs(b[2]) > .001) b / abs(b[2]) else c(1, 0)
  gross_b <- as.vector(x[is_rows, ] %*% b)
  net_b <- gross_b - mean(abs(ptc[is_rows, , drop = FALSE] %*% b))
  unmanaged_net_is <- x[is_rows, 1] - mean(ptc[is_rows, 1])

  ## OOS, i = 1..T-M; window 1..t with t = M + i - 1; holding month t + 1
  entry_cost <- function(w, t) {
    stock_cost(stock, tibble::tibble(month_id = sample$month_id[t], sleeve = sleeves,
                                     factor = factor_key, w = c(w[1], w[2] * multiplier[t])),
               "factor", charge_entry = TRUE)$cost
  }
  oos <- purrr::map_dfr(seq_len(n - M), function(i) {
    t <- M + i - 1L
    past <- seq_len(t)
    b_c <- mv_signed_weights(x[past, , drop = FALSE], gamma)
    b_e <- mv_l1_phi_2d(colMeans(x[past, ]), demiguel_covariance(x[past, ]), gamma,
                        window_rays(rays, t + 1L, scale = 1 / t))
    if (i == 1L) {
      tc_td <- entry_cost(b_e, t + 1L)
      tc_unmanaged <- entry_cost(c(1, 0), t + 1L)
    } else {
      tc_td <- sum(abs(phi[[t]] %*% b_e))
      tc_unmanaged <- ptc[t + 1L, 1]
    }
    tibble::tibble(month_id = sample$month_id[t + 1L], unmanaged = x[t + 1L, 1],
                   gross_c = sum(x[t + 1L, ] * b_c), tc_ntc = sum(abs(b_c) * ptc[t + 1L, ]),
                   gross_e = sum(x[t + 1L, ] * b_e), tc_td = tc_td, tc_unmanaged = tc_unmanaged,
                   a_e = b_e[1], b_e = b_e[2])
  }) %>%
    dplyr::mutate(net_d = gross_c - mean(tc_ntc), net_e = gross_e - mean(tc_td),
                  unmanaged_net = unmanaged - mean(tc_unmanaged))

  list(in_sample = tibble::tibble(month_id = sample$month_id[is_rows], unmanaged = x[is_rows, 1],
                                  unmanaged_net = unmanaged_net_is, gross_b = gross_b, net_b = net_b),
       oos = oos, b_is = b, tc_sleeves = tcp)
}

#' Table I Panels B, D and E for every factor
#'
#' Sharpe ratios divide the NET mean by the GROSS standard deviation
#' (JF_Table1.m lines 65 and 86; footnote 24). p-values compare net returns:
#' in-sample unmanaged vs Panel B; OOS unmanaged vs Panels D and E.
#' start, end, M and variance work as in run_table1_cost_free(); every month
#' of a factor's window must lie inside the stock-level data.
run_table1_costs <- function(monthly, daily, stock, start, end, M = 120L, gamma = 5,
                             draws = 10000L, block_length = 5, seed = 20261011L,
                             variance = NULL) {

  if (is.null(variance)) variance <- prepare_factor_variance(daily)
  validate_port_data(variance, c("name", "month_id"), "variance", consecutive = FALSE)
  if (any(variance$variance <= 0)) rlang::abort("Scaling signals must be positive.")
  factors <- demiguel_table1_paper$name
  sr_net <- function(net, gross) sqrt(12) * mean(net) / stats::sd(gross)

  results <- purrr::map(stats::setNames(factors, factors), function(k) {
    message("Table I costs: ", k)
    sample <- build_table1_sample(monthly, variance, k, factor_setting(start, k), factor_setting(end, k))
    table1_cost_panels(sample, stock, k, factor_setting(M, k), gamma)
  })
  purrr::walk(results, function(r) {
    if (!identical(r$oos$month_id, r$in_sample$month_id)) {
      rlang::abort("Cost-panel in-sample and OOS months must coincide within a factor.")
    }
  })
  calendars <- table1_calendars(purrr::map(results, ~ .x$oos$month_id), draws, block_length, seed)

  table <- purrr::imap_dfr(results, function(r, k) {
    calendar <- calendars[[k]]
    tibble::tibble(
      name = k,
      sr_unmanaged_net = sr_net(r$in_sample$unmanaged_net, r$in_sample$unmanaged),
      sr_is_net = sr_net(r$in_sample$net_b, r$in_sample$gross_b),
      p_is_net = demiguel_sr_pvalue(r$in_sample$unmanaged_net, r$in_sample$net_b, calendar),
      sr_oos_net = sr_net(r$oos$net_d, r$oos$gross_c),
      p_oos_net = demiguel_sr_pvalue(r$oos$unmanaged_net, r$oos$net_d, calendar),
      sr_oos_td = sr_net(r$oos$net_e, r$oos$gross_e),
      p_oos_td = demiguel_sr_pvalue(r$oos$unmanaged_net, r$oos$net_e, calendar),
      a_is = r$b_is[1], b_is = r$b_is[2],
      cost_unmanaged_bp = 1e4 * r$tc_sleeves[1], cost_managed_bp = 1e4 * r$tc_sleeves[2]
    )
  })
  list(table = table, results = results,
       settings = list(start = start, end = end, M = M, gamma = gamma, draws = draws,
                       block_length = block_length, seed = seed))
}

#' Standalone managed factor: monthly gross and net returns
#'
#' The Moreira-Muir managed factor alone (no unmanaged sleeve, no MVO), an
#' extension of Table I. managed_t = r_t * c / variance_t from
#' build_table1_sample() over [start, end], evaluated from eval_start on.
#' Net: the sleeve's own stock-level trades (exposure c / variance on the
#' factor's stock weights), entry from cash in the first evaluation month, and,
#' as in Table I, the AVERAGE realized monthly cost deducted from every month.
#' c cancels from gross and net Sharpe ratios (costs scale with exposure).
#' stock: read_demiguel_stock_data() output; needs R/demiguel_cost_helpers.R.
#' Returns month_id, unmanaged, gross, cost, net (monthly decimals).
standalone_managed_series <- function(monthly, variance, stock, factor_name, start, end,
                                      eval_start) {
  s <- build_table1_sample(monthly, variance, factor_name, start, end) %>%
    dplyr::filter(month_id >= eval_start)
  if (!nrow(s) || min(s$month_id) != eval_start) {
    rlang::abort(paste("Standalone sample does not start at the evaluation month for", factor_name))
  }
  cost <- stock_cost(stock, tibble::tibble(month_id = s$month_id, sleeve = "managed",
                                           factor = factor_name, w = s$scale / s$variance),
                     netting = "sleeve", charge_entry = TRUE)$cost
  tibble::tibble(month_id = s$month_id, unmanaged = s$factor_exc, gross = s$managed,
                 cost = cost, net = s$managed - mean(cost))
}

#' Compare Table I Panels B, D and E with the published values
compare_table1_costs <- function(table) {
  table %>%
    dplyr::select(dplyr::all_of(names(demiguel_table1_paper_costs))) %>%
    tidyr::pivot_longer(-name, names_to = "statistic", values_to = "ours") %>%
    dplyr::inner_join(tidyr::pivot_longer(demiguel_table1_paper_costs, -name,
                                          names_to = "statistic", values_to = "paper"),
                      by = c("name", "statistic"), relationship = "one-to-one") %>%
    dplyr::mutate(difference = ours - paper)
}
