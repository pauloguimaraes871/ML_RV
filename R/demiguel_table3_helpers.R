# DeMiguel, Martin-Utrera and Uppal (2024): Table III helpers
# ------------------------------------------------------------------------------
#
# Multifactor portfolios UMV (unmanaged factors) and CMV (unmanaged factors plus
# their market-volatility-managed versions, f / sigma_t), with nonnegative
# weights and gamma = 5, mirroring the authors' replication code:
# JF_Table4_IS_OOS_FFHXZBAB_Nonnegative.m, OOS_nonnegative.m,
# pppquadprog_notc_nonegative.m, pppfminuncNonnegative.m and
# JF_performancematrixTable4.m. Columns (1) UMV and (3) CMV are optimized
# ignoring costs; (4) UMV and (6) CMV account for stock-level costs with
# trading diversification. CFW, columns (2) and (5), is deferred. Our FCMV
# extension scales each factor by its own volatility (sigma as a matrix).
#
# Conventions taken from that code, not guessed:
#   * sigma_t is the sample SD of daily market returns in the previous month
#     (their VIXmonth, in percent; the scale does not affect any weight);
#   * OOS weights are re-estimated every 12 months from the first OOS month,
#     on an expanding window starting at the first sample month, and held
#     fixed in between; the cost-aware window includes the trade INTO the
#     holding month, divided by D = number of return months in the window;
#   * costs charge the actual stock trades between consecutive months, entry
#     from cash in the first OOS month: netted across all sleeves (trading
#     diversification, Panel D) or sleeve by sleeve (Panel C);
#   * BAB's zero stock weights are treated as missing (their lines 9-16), so
#     BAB entries and exits are not charged in this table;
#   * if cond(Sigma) > 1e9 they replace the sample covariance by cov1para.

  `%>%` <- dplyr::`%>%`

  ## Table III as published, columns (1), (3), (4), (6)
  demiguel_table3_paper <- tibble::tribble(
    ~panel, ~column, ~mean, ~sd, ~sr, ~p_value, ~alpha, ~t_alpha, ~tc,
    "A", "umv_notc", .415, .288, 1.441, NA, NA, NA, 0,
    "A", "cmv_notc", .680, .369, 1.844, .000, .213, 6.684, 0,
    "A", "umv_tc", .301, .218, 1.378, NA, NA, NA, 0,
    "A", "cmv_tc", .432, .250, 1.726, .000, .107, 7.638, 0,
    "B", "umv_notc", .753, .580, 1.299, NA, NA, NA, 0,
    "B", "cmv_notc", .925, .569, 1.625, .000, .239, 5.797, 0,
    "B", "umv_tc", .593, .458, 1.295, NA, NA, NA, 0,
    "B", "cmv_tc", .690, .449, 1.537, .000, .126, 6.414, 0,
    "C", "umv_notc", .412, .580, .710, NA, NA, NA, .341,
    "C", "cmv_notc", .349, .569, .613, .930, -.027, -.727, .576,
    "C", "umv_tc", .347, .458, .758, NA, NA, NA, .246,
    "C", "cmv_tc", .332, .449, .739, .721, .001, .031, .358,
    "D", "umv_notc", .517, .580, .891, NA, NA, NA, .236,
    "D", "cmv_notc", .575, .569, 1.010, .072, .103, 2.759, .350,
    "D", "umv_tc", .430, .458, .940, NA, NA, NA, .163,
    "D", "cmv_tc", .477, .449, 1.062, .006, .066, 3.637, .213
  )

#' Ledoit-Wolf shrinkage to a scaled identity (exact port of cov1para.m)
cov1para <- function(x) {
  t <- nrow(x)
  x <- sweep(x, 2, colMeans(x))
  sample <- crossprod(x) / t
  meanvar <- mean(diag(sample))
  prior <- diag(meanvar, ncol(x))
  phi <- sum(crossprod(x^2) / t - 2 * crossprod(x) * sample / t + sample^2)
  gamma <- sum((sample - prior)^2)
  shrinkage <- max(0, min(1, phi / gamma / t))
  shrinkage * prior + (1 - shrinkage) * sample
}

#' Newey-West regression t-statistics (exact port of nwest.m, LeSage)
#'
#' y on x (x includes the intercept column). Returns beta and t-statistics.
newey_west <- function(y, x, nlag = 1L) {
  xpxi <- solve(crossprod(x))
  beta <- as.vector(xpxi %*% crossprod(x, y))
  hhat <- t(x * as.vector(y - x %*% beta))
  n <- nrow(x)
  G <- matrix(0, ncol(x), ncol(x))
  for (a in 0:nlag) {
    za <- hhat[, (a + 1):n, drop = FALSE] %*% t(hhat[, 1:(n - a), drop = FALSE])
    G <- G + (nlag + 1 - a) / (nlag + 1) * (if (a == 0) za else za + t(za))
  }
  list(beta = beta, t = beta / sqrt(diag(xpxi %*% G %*% xpxi)))
}

#' Nonnegative mean-variance weights without costs (pppquadprog_notc_nonegative.m)
nonneg_mv <- function(mu, sigma, gamma) {
  k <- length(mu)
  quadprog::solve.QP(gamma * sigma, mu, diag(k), rep(0, k))$solution %>% pmax(0)
}

#' Nonnegative mean-variance weights with stock-level L1 costs
#'
#' Solves min gamma/2 x'Sigma x - mu'x + ||A x||_1, x >= 0, A = Phi / D (their
#' pppfminuncNonnegative.m, which uses SQP on a sign-gradient). We minimize a
#' smoothed objective (|z| ~ sqrt(z^2 + delta^2) - delta) with L-BFGS-B and a
#' decreasing delta, and CERTIFY the result with a duality gap: for any
#' u in [-1, 1]^n, P(x) >= min_{x >= 0} gamma/2 x'Sigma x - (mu - A'u)'x, a
#' nonnegative quadratic program solved exactly. The gap is measured relative
#' to the scale of the problem, the no-cost utility |P(x_mv)|, and the solver
#' aborts unless gap <= tol * scale. x = 0 (always feasible) is also compared.
mv_l1_nonneg <- function(mu, sigma, gamma, A, tol = 1e-7) {

  k <- length(mu)
  primal <- function(x) gamma / 2 * sum(x * (sigma %*% x)) - sum(mu * x) + sum(abs(A %*% x))
  dual <- function(u) {
    d <- mu - as.vector(crossprod(A, u))
    -quadprog::solve.QP(gamma * sigma, d, diag(k), rep(0, k))$value
  }
  ## Work in volatility units for conditioning; undo at the end
  s <- sqrt(diag(sigma))
  x <- nonneg_mv(mu, sigma, gamma)
  scale <- max(abs(gamma / 2 * sum(x * (sigma %*% x)) - sum(mu * x)), .Machine$double.eps)
  base <- max(mean(abs(A %*% x)), 1e-12)
  gap <- Inf
  for (delta in base * 10^-(1:8)) {
    f <- function(y) {
      z <- A %*% (y / s)
      gamma / 2 * sum((y / s) * (sigma %*% (y / s))) - sum(mu * y / s) + sum(sqrt(z^2 + delta^2) - delta)
    }
    g <- function(y) {
      x <- y / s
      z <- as.vector(A %*% x)
      as.vector(gamma * sigma %*% x - mu + crossprod(A, z / sqrt(z^2 + delta^2))) / s
    }
    fit <- stats::optim(x * s, f, g, method = "L-BFGS-B", lower = 0,
                        control = list(maxit = 5000L, factr = 10, pgtol = 0))
    x <- pmax(fit$par, 0) / s
    z <- as.vector(A %*% x)
    u <- z / sqrt(z^2 + delta^2)
    if (primal(x) > 0) x <- rep(0, k)
    p <- primal(x)
    gap <- p - dual(u)
    if (gap <= tol * scale) break
  }
  if (!is.finite(gap) || gap > tol * scale) {
    rlang::abort(sprintf("Cost-aware optimizer did not certify optimality (gap %.3g, scale %.3g).",
                         gap, scale))
  }
  structure(x, gap = gap / scale)
}

#' Treat a factor's zero stock weights as missing (their Table III lines 9-16)
stock_zero_as_missing <- function(stock, factor) {
  j <- match(factor, stock$factors)
  if (is.na(j)) rlang::abort(paste("Unknown factor:", factor))
  stock$panels <- purrr::map(stock$panels, function(p) {
    p$x[!is.na(p$x[, j]) & p$x[, j] == 0, j] <- NA_real_
    p
  })
  stock
}

#' Table III portfolio statistics for one panel (JF_performancematrixTable4.m)
#'
#' gross, net: month x column matrices with columns umv_notc, cmv_notc,
#' umv_tc, cmv_tc. Mean is the annualized NET mean, SD the annualized GROSS SD;
#' the CMV columns are compared with the UMV column optimized the same way:
#' bootstrap p-value of SR(CMV) - SR(UMV) on net returns, Newey-West(1) alpha
#' of CMV net on UMV net (annualized) and its t-statistic. TC = annualized
#' gross minus net mean.
table3_panel_stats <- function(gross, net, calendar) {
  purrr::map_dfr(colnames(gross), function(col) {
    bench <- if (startsWith(col, "cmv")) sub("cmv", "umv", col) else NA_character_
    row <- tibble::tibble(column = col, mean = 12 * mean(net[, col]),
                          sd = sqrt(12) * stats::sd(gross[, col]),
                          tc = 12 * (mean(gross[, col]) - mean(net[, col])))
    row$sr <- row$mean / row$sd
    if (is.na(bench)) {
      return(dplyr::mutate(row, p_value = NA_real_, alpha = NA_real_, t_alpha = NA_real_))
    }
    nw <- newey_west(net[, col], cbind(1, net[, bench]), 1L)
    dplyr::mutate(row, p_value = demiguel_sr_pvalue(net[, bench], net[, col], calendar),
                  alpha = 12 * nw$beta[1], t_alpha = nw$t[1])
  })
}

#' Table III, Panels A-D, columns (1), (3), (4), (6)
#'
#' returns: T x K matrix of monthly factor excess returns (columns named by
#' factor), rows = months (consecutive, first sample month first).
#' sigma: positive volatility signal for each holding month, either a length-T
#'   vector (market volatility scales every factor: CMV, as in the paper) or a
#'   T x K matrix with columns named by factor (each factor scaled by its own
#'   volatility: FCMV). A vector gives exactly the same results as before.
#' stock: read_demiguel_stock_data() output (BAB zeros are set missing here).
#' M = 120, gamma = 5, refit = 12 months. Returns list(table, returns, weights).
run_table3 <- function(returns, months, sigma, stock, M = 120L, gamma = 5, refit = 12L,
                       draws = 10000L, block_length = 5, seed = 20261011L) {

  n <- nrow(returns)
  factors <- colnames(returns)
  K <- length(factors)
  if (is.matrix(sigma)) {
    if (nrow(sigma) != n || !setequal(colnames(sigma), factors)) {
      rlang::abort("A sigma matrix must have one row per month and one column per factor.")
    }
    sigma <- sigma[, factors, drop = FALSE]
  } else {
    if (length(sigma) != n) rlang::abort("A sigma vector must have one value per month.")
    sigma <- matrix(sigma, n, K, dimnames = list(NULL, factors))
  }
  if (length(months) != n || any(!is.finite(sigma) | sigma <= 0) ||
      any(!is.finite(returns)) || !identical(months, seq(months[1], by = "month", length.out = n))) {
    rlang::abort("Table III inputs must be finite, positive and on consecutive months.")
  }
  stock <- stock_zero_as_missing(stock, "bab")
  ext <- cbind(returns, returns / sigma)
  colnames(ext) <- c(paste0("u_", factors), paste0("m_", factors))

  ## Unit-exposure stock trades of all 18 sleeves (their Phiext), months 2..T
  unit <- dplyr::bind_rows(
    tidyr::expand_grid(month_id = months, factor = factors) %>%
      dplyr::mutate(sleeve = paste0("u_", factor), w = 1),
    tibble::tibble(month_id = rep(months, K), factor = rep(factors, each = n)) %>%
      dplyr::mutate(sleeve = paste0("m_", factor), w = as.vector(1 / sigma))
  )
  trades <- stock_trade_matrices(stock, unit)
  phi <- do.call(rbind, purrr::map(trades$phi, ~ .x[, colnames(ext), drop = FALSE]))
  phi_month <- rep(seq_along(trades$phi) + 1L, purrr::map_int(trades$phi, nrow))
  keep <- rowSums(phi != 0) > 0
  phi <- phi[keep, , drop = FALSE]
  phi_month <- phi_month[keep]

  cov_used <- function(x) {
    s <- stats::cov(x)
    if (kappa(s, exact = TRUE) > 1e9) cov1para(x) else s
  }
  estimate <- function(rows, phi_rows, D) {
    xu <- returns[rows, , drop = FALSE]
    xe <- ext[rows, , drop = FALSE]
    Au <- phi[phi_rows, 1:K, drop = FALSE]
    Au <- Au[rowSums(Au != 0) > 0, , drop = FALSE] / D
    Ae <- phi[phi_rows, , drop = FALSE] / D
    w_umv <- nonneg_mv(colMeans(xu), cov_used(xu), gamma)
    w_cmv <- nonneg_mv(colMeans(xe), cov_used(xe), gamma)
    w_umv_tc <- mv_l1_nonneg(colMeans(xu), cov_used(xu), gamma, Au)
    w_cmv_tc <- mv_l1_nonneg(colMeans(xe), cov_used(xe), gamma, Ae)
    pad <- function(w) c(w, rep(0, K))
    cbind(umv_notc = pad(w_umv), cmv_notc = as.vector(w_cmv),
          umv_tc = pad(as.vector(w_umv_tc)), cmv_tc = as.vector(w_cmv_tc))
  }

  ## Panel A: in-sample on rows M+1..T; Phi of trades into months M+2..T
  is_rows <- (M + 1L):n
  w_is <- estimate(is_rows, phi_month >= M + 2L, n - M)
  gross_a <- ext[is_rows, , drop = FALSE] %*% w_is

  ## OOS: refit every `refit` months on rows 1..t, trades into months 2..t+1
  oos_index <- seq_len(n - M)
  weights <- vector("list", length(oos_index))
  for (i in oos_index) {
    t <- M + i - 1L
    weights[[i]] <- if ((i - 1L) %% refit == 0L) {
      message("Table III refit at ", format(months[t + 1L]))
      estimate(seq_len(t), phi_month <= t + 1L, t)
    } else weights[[i - 1L]]
  }
  hold <- months[M + oos_index]
  gross <- matrix(unlist(purrr::map(oos_index, ~ as.vector(ext[M + .x, ] %*% weights[[.x]]))),
                  ncol = 4, byrow = TRUE, dimnames = list(NULL, colnames(w_is)))

  ## Costs of the actual weight paths (entry from cash in the first OOS month)
  cost_paths <- purrr::map(colnames(w_is), function(col) {
    exposures <- purrr::map_dfr(oos_index, function(i) {
      w <- weights[[i]][, col]
      tibble::tibble(month_id = hold[i], sleeve = colnames(ext),
                     factor = rep(factors, 2), w = w * c(rep(1, K), 1 / sigma[M + i, ]))
    })
    list(td = stock_cost(stock, exposures, "portfolio", charge_entry = TRUE)$cost,
         ntd = stock_cost(stock, exposures, "sleeve", charge_entry = TRUE)$cost)
  }) %>% stats::setNames(colnames(w_is))
  tc_td <- sapply(cost_paths, `[[`, "td")
  tc_ntd <- sapply(cost_paths, `[[`, "ntd")

  calendar_is <- draw_portfolio_months(months[is_rows], draws = draws,
                                       block_length = block_length, seed = seed)
  calendar <- draw_portfolio_months(hold, draws = draws, block_length = block_length, seed = seed)
  table <- dplyr::bind_rows(
    dplyr::mutate(table3_panel_stats(gross_a, gross_a, calendar_is), panel = "A"),
    dplyr::mutate(table3_panel_stats(gross, gross, calendar), panel = "B"),
    dplyr::mutate(table3_panel_stats(gross, gross - tc_ntd, calendar), panel = "C"),
    dplyr::mutate(table3_panel_stats(gross, gross - tc_td, calendar), panel = "D")
  ) %>% dplyr::relocate(panel)

  list(table = table,
       returns = list(months_is = months[is_rows], gross_is = gross_a, months = hold, gross = gross,
                      tc_td = tc_td, tc_ntd = tc_ntd),
       weights = list(in_sample = w_is, oos = weights),
       settings = list(M = M, gamma = gamma, refit = refit, draws = draws,
                       block_length = block_length, seed = seed))
}

#' Compare a Table III result with the published values
compare_table3 <- function(table) {
  table %>%
    tidyr::pivot_longer(c(mean, sd, sr, p_value, alpha, t_alpha, tc),
                        names_to = "statistic", values_to = "ours") %>%
    dplyr::inner_join(tidyr::pivot_longer(demiguel_table3_paper, c(mean, sd, sr, p_value, alpha, t_alpha, tc),
                                          names_to = "statistic", values_to = "paper"),
                      by = c("panel", "column", "statistic")) %>%
    dplyr::filter(!is.na(paper)) %>%
    dplyr::mutate(difference = ours - paper)
}
