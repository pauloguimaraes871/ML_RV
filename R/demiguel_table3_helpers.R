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
#     (their VIXmonth is in percent). Changing signal units changes managed
#     coefficients inversely; with consistent rescaling the exposures agree;
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

  # Reading map: run_table3() prepares returns and stock-trade coefficients, then
  # its nested estimate() chooses four portfolios. The same estimator supplies
  # Panel A (in sample) and Panels B-D (out of sample). table3_panel_stats()
  # turns their realized monthly returns into the displayed statistics.
  #
  # Column names describe HOW WEIGHTS ARE CHOSEN, not whether returns are net:
  #   umv_notc = unmanaged only, ignore costs when choosing weights (paper col 1).
  #   cmv_notc = unmanaged + managed, ignore costs when choosing weights (col 3).
  #   umv_tc   = unmanaged only, consider netted costs when choosing weights (col 4).
  #   cmv_tc   = unmanaged + managed, consider netted costs when choosing weights (col 6).
  # Panel A reports in-sample gross returns. B reports out-of-sample gross returns.
  # C and D use B's SAME weight paths and deduct sleeve costs or portfolio-netted
  # costs, respectively. C does not reoptimize under a separate sleeve-cost rule.
  # Nonnegative strategy coefficients need not sum to one and do not imply that
  # the underlying stocks are all held long: a factor can contain stock shorts.
  `%>%` <- dplyr::`%>%`

  ## Table III as published, columns (1), (3), (4), (6)
  # Reference values are stored as decimals: .415 means 41.5% per year, not .415%.
  # These constants are comparison targets, not inputs to portfolio estimation.
  # NA marks statistics not reported for the unmanaged benchmark portfolios.
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
  # x has one observation per row and one strategy per column. Here t is
  # the observation count (a local variable, not the transpose function).
  # The routine reproduces the authors' covariance fallback, including its
  # 1/t covariance convention, rather than stats::cov()'s 1/(t-1).
  t <- nrow(x)
  # Subtract each column's own mean: covariance describes deviations from
  # average returns, not the level of those average returns.
  x <- sweep(x, 2, colMeans(x))
  # crossprod(x) = t(x) %*% x: sum pairwise products of demeaned returns.
  # The result has strategy variances on the diagonal and covariances off it.
  sample <- crossprod(x) / t
  # The shrinkage target gives every strategy the average sample variance
  # and sets all cross-strategy covariances to zero (a scaled identity).
  meanvar <- mean(diag(sample))
  prior <- diag(meanvar, ncol(x))
  # Estimate sampling noise in covariance entries (phi), then the squared
  # distance from the sample matrix to the target (gamma). This local gamma
  # is a covariance-estimation quantity, NOT portfolio risk aversion.
  phi <- sum(crossprod(x^2) / t - 2 * crossprod(x) * sample / t + sample^2)
  gamma <- sum((sample - prior)^2)
  # Restrict the target's mixing weight to [0,1]. A larger value moves the
  # estimate further toward the scaled identity. Degenerate inputs are not
  # separately handled here; this is the existing formula, not a new safeguard.
  shrinkage <- max(0, min(1, phi / gamma / t))
  shrinkage * prior + (1 - shrinkage) * sample
}

#' Newey-West regression t-statistics (exact port of nwest.m, LeSage)
#'
#' y on x (x includes the intercept column). Returns beta and t-statistics.
newey_west <- function(y, x, nlag = 1L) {
  # x includes an intercept column of ones. Solve the ordinary least-squares
  # regression first; the Newey-West adjustment changes standard errors, not
  # the fitted regression coefficients. This assumes x has full column rank.
  xpxi <- solve(crossprod(x))
  beta <- as.vector(xpxi %*% crossprod(x, y))
  # Multiply each observation's regressors by its fitted residual. After
  # transposing, each column is one month's regressor-times-residual vector.
  hhat <- t(x * as.vector(y - x %*% beta))
  n <- nrow(x)
  # Accumulate the long-run covariance of those vectors. Lag zero captures
  # same-month variation; positive lags allow residual serial dependence.
  G <- matrix(0, ncol(x), ncol(x))
  for (a in 0:nlag) {
    # Pair observations a months apart. At a=0 this is the contemporaneous
    # product. With the Table III choice nlag=1, the lag-one Bartlett weight
    # is (2-1)/2 = 1/2; adding the transpose includes both time directions.
    za <- hhat[, (a + 1):n, drop = FALSE] %*% t(hhat[, 1:(n - a), drop = FALSE])
    G <- G + (nlag + 1 - a) / (nlag + 1) * (if (a == 0) za else za + t(za))
  }
  # The sandwich matrix supplies coefficient variances; square roots of
  # its diagonal are standard errors. Each t-statistic is beta / its SE.
  list(beta = beta, t = beta / sqrt(diag(xpxi %*% G %*% xpxi)))
}

#' Nonnegative mean-variance weights without costs (pppquadprog_notc_nonegative.m)
nonneg_mv <- function(mu, sigma, gamma) {
  # mu contains k monthly expected strategy returns; sigma is their k-by-k
  # covariance matrix. Minimize gamma/2 * x' sigma x - mu' x, with x >= 0.
  # solve.QP uses 1/2 * x' Dmat x - dvec' x, hence Dmat=gamma*sigma and
  # dvec=mu. diag(k) and rep(0,k) impose each coefficient >= 0. No budget
  # constraint requires sum(x)=1. pmax removes negative numerical residues;
  # it does not replace solving the constrained problem by clipping an
  # unconstrained solution.
  k <- length(mu)
  quadprog::solve.QP(gamma * sigma, mu, diag(k), rep(0, k))$solution %>% pmax(0)
}

#' Nonnegative mean-variance weights with stock-level L1 costs
#'
#' Targets min gamma/2 x'Sigma x - mu'x + sum(abs(A %*% x)), x >= 0.
#' mu and Sigma are monthly return moments; A = Phi / D contains cost-weighted
#' stock-trade coefficients divided by the number D of estimation return months.
#' Rows of A are stock-month observations; columns correspond to strategy weights.
#' All strategy trades in the SAME stock-month are added before taking abs(), so
#' this objective already allows trading diversification across sleeves.
#'
#' Table I uses rays to organize sign regions for TWO coefficients. Here there
#' are K or 2K coefficients (9 or 18 in the main replication); numerical search
#' replaces that two-dimensional geometric enumeration. The method smooths the
#' absolute values, solves with nonnegative bounds, and certifies the result
#' with a duality gap on the exact objective. This is not an enumeration of
#' discrete weight choices.
#'
#' Fixed 2026-10-25: dual() used to NEGATE solve.QP()$value, which already is
#' the lower bound. The gap was then never positive, the loop always stopped
#' at the first (coarsest) delta and nothing was certified. Verified after the
#' fix: all 90 Table III fits certify below tol, within 8.4e-8 of an
#' independent certified reference (tmp review evidence, docs Section 12).
mv_l1_nonneg <- function(mu, sigma, gamma, A, tol = 1e-7) {

  # For a two-factor teaching example the order is
  # x = (unmanaged MKT, unmanaged BAB, managed MKT, managed BAB), so k=4.
  # x contains the ORIGINAL strategy coefficients that multiply returns and
  # stock positions. A may have 12 rows (two stocks over six trade months).
  # The definitions of primal() and dual() below create functions; their
  # bodies run later when those functions are called.
  k <- length(mu)
  # Exact objective P(x): risk penalty - expected gross return + average
  # monthly trading cost. %*% is matrix multiplication; * multiplies entries.
  # A %*% x is SIGNED; only sum(abs(A %*% x)) is the nonnegative cost.
  # For one row (a,b), its cost is |a*x1+b*x2|, not |a*x1|+|b*x2|.
  # P(x) can be negative because expected return is subtracted; this does
  # not mean trading costs are negative.
  primal <- function(x) gamma / 2 * sum(x * (sigma %*% x)) - sum(mu * x) + sum(abs(A %*% x))
  # u is an AUXILIARY vector, one entry per row of A; it is not an unmanaged
  # weight or a second portfolio. For -1 <= u_r <= 1, u_r*z_r <= |z_r|.
  # Thus replacing sum(|A*x|) by u'A*x gives a lower objective for every x.
  # Minimizing that replacement over x>=0 supplies a lower bound on the
  # unknown minimum of P. A valid gap is P(candidate) minus that lower bound.
  dual <- function(u) {
    # crossprod(A,u) = t(A) %*% u: one cost adjustment per strategy.
    # Grouping -mu'x + u'A*x gives -(mu-A'u)'x, hence adjusted means d.
    d <- mu - as.vector(crossprod(A, u))
    # solve.QP() minimizes 1/2 x'Dx - d'x with D = gamma*Sigma, so its $value
    # IS min_{x>=0} gamma/2*x'Sigma*x - d'x, the lower bound required above.
    # It is used as is: no sign change (a leading minus here was the bug).
    quadprog::solve.QP(gamma * sigma, d, diag(k), rep(0, k))$value
  }
  ## Work in volatility units for conditioning; undo at the end
  # diag(sigma) extracts strategy variances; sqrt gives standard deviations.
  # The numerical optimizer uses y=x*s, entry by entry, and recovers x=y/s.
  # Example: x=(10,.1), s=(.01,1) gives y=(.1,.1). The original weights
  # differ 100-fold but each isolated position has volatility .1. Working
  # in y reduces the scale disparity in the risk term (covariance becomes
  # correlation under this change of variables). Correlations still matter:
  # y is not the vector of exact contributions to total portfolio risk.
  # This transformation assumes positive strategy standard deviations.
  s <- sqrt(diag(sigma))
  # Start from the nonnegative optimum IGNORING costs. It is a starting
  # point, not a constraint tying the final weights to this portfolio.
  # Illustrative four-weight start: (1.395939,4.568528,.043147,0).
  x <- nonneg_mv(mu, sigma, gamma)
  # scale sets the accuracy benchmark; it does not rescale exposures.
  # Take the magnitude of the starting NO-COST objective, with a tiny
  # positive floor to avoid scale=0. Example: risk .06704738 minus return
  # .13409475 gives scale .06704738. With tol=1e-7, the allowed absolute
  # gap is tol*scale, about 6.70e-9. This scale is fixed across delta rounds.
  scale <- max(abs(gamma / 2 * sum(x * (sigma %*% x)) - sum(mu * x)), .Machine$double.eps)
  # base sets the smoothing magnitude from typical absolute stock-month
  # contributions at the starting portfolio. If their sum is .0022526565
  # over 12 rows, base=.0001877214. The 1e-12 floor avoids base=0.
  # This mean is ONLY a numerical reference: the objective still SUMS
  # row costs, and A has already been divided by D for monthly averaging.
  base <- max(mean(abs(A %*% x)), 1e-12)
  # No accuracy estimate exists yet. Infinity is a placeholder overwritten
  # after optimization, not an input to the objective.
  gap <- Inf
  # Try base/10, base/100, ..., base/1e8, at most eight rounds. A larger
  # delta rounds the |z| corner more, easing the smooth search but making
  # costs less exact. Smaller delta better approximates the true objective.
  # The loop refines until the EXACT objective is close enough to a valid
  # lower bound. Each round starts from the preceding round's x.
  for (delta in base * 10^-(1:8)) {
    # f receives each CURRENT trial y proposed by optim(), not always the
    # initial weights. y/s recovers that trial's original strategy weights.
    # Example s=(.02,.01,.10,.08), y=(.04,.03,.05,0) represents x=(2,3,.5,0).
    f <- function(y) {
      # Each row sums all sleeves' trades for one stock-month. Contributions
      # can cancel here before a cost magnitude is taken. A negative z means
      # one trading direction, not a negative expense.
      z <- A %*% (y / s)
      # Keep risk and return in original x=y/s units. Replace each |z| by
      # sqrt(z^2+delta^2)-delta: smooth at zero, zero cost at z=0, and tending
      # to |z| as delta tends to zero. At z=.001, delta=.0001 gives .00090499
      # instead of .001; delta=.00001 gives .00099005. f is approximate;
      # primal() later evaluates the exact absolute-value objective.
      gamma / 2 * sum((y / s) * (sigma %*% (y / s))) - sum(mu * y / s) + sum(sqrt(z^2 + delta^2) - delta)
    }
    # g returns one local slope per y coordinate, helping optim() choose
    # directions. Positive slope means a small increase raises the objective;
    # negative means it lowers it. At a zero lower bound, a positive slope
    # can be consistent with staying at zero (negative y is forbidden).
    g <- function(y) {
      # Recover the current original weights, just as in f(). This local x
      # does not overwrite the outer starting/final x when g() is evaluated.
      x <- y / s
      # Compute signed row contributions for this trial; as.vector removes
      # the one-column matrix shape without changing numerical values.
      z <- as.vector(A %*% x)
      # Gradient with respect to x: gamma*Sigma*x (risk), -mu (return),
      # and A'q (cost), where q=z/sqrt(z^2+delta^2) is the smooth sign.
      # q approaches +1 for positive z, -1 for negative z, and is 0 at z=0.
      # A'q sums each strategy's effect across all stock-month costs. Divide
      # by s because the optimizer changes y and x=y/s (the chain rule).
      as.vector(gamma * sigma %*% x - mu + crossprod(A, z / sqrt(z^2 + delta^2))) / s
    }
    # Start in y units with x*s. f is the smooth objective and g its analytic
    # gradient. L-BFGS-B permits bounds: lower=0 applies to every y coordinate
    # and implies x>=0 since s>0. There is no finite upper bound or sum-to-one
    # constraint. maxit allows 5000 iterations; factr controls relative
    # objective-change stopping, and pgtol sets projected-gradient tolerance.
    # Optimizer convergence and exact-objective certification are distinct;
    # the gap check below is intended to assess the latter.
    fit <- stats::optim(x * s, f, g, method = "L-BFGS-B", lower = 0,
                        control = list(maxit = 5000L, factr = 10, pgtol = 0))
    # fit$par contains optimized y, NOT original weights. Remove any negative
    # numerical residue and divide by s to recover x. This x also supplies
    # the next round's starting point if a smaller delta is needed.
    x <- pmax(fit$par, 0) / s
    # Recompute signed trade contributions at the new portfolio. The exact
    # cost is sum(abs(z)); the entries of z alone may have either sign.
    z <- as.vector(A %*% x)
    # Construct one auxiliary number per stock-month, all in [-1,1]. This
    # is the smoothed cost derivative with respect to z. The same interval
    # ensures u_r*z_r <= |z_r|, enabling the lower-bound construction in dual().
    # Example z=-.003,u=-.8: u*z=.0024 <= |z|=.003. u is NOT a portfolio.
    u <- z / sqrt(z^2 + delta^2)
    # Zero exposure is feasible and has exact objective P(0)=0. Since we
    # minimize, a positive candidate objective loses to zero, so use zero.
    # An admissible u remains usable for a bound even if x is replaced.
    if (primal(x) > 0) x <- rep(0, k)
    # Evaluate the candidate using TRUE absolute costs rather than smoothing.
    # Example: risk .06507954 - return .13206020 + cost .00193053
    # gives p=-.06505013. Negative p is allowed; its cost term is positive.
    p <- primal(x)
    # Gap: candidate objective minus a lower bound, hence >= 0 in exact
    # arithmetic. Example: p=-.06505013 and bound -.06507954 give
    # gap=+.00002941, too large for a 6.70e-9 tolerance: try a smaller delta.
    gap <- p - dual(u)
    # Stop when further possible improvement is below the requested relative
    # tolerance; otherwise try a smaller delta.
    if (gap <= tol * scale) break
  }
  # Reject non-finite or overly large gaps, and NEGATIVE gaps beyond rounding:
  # a bound above the objective means the bound itself is wrong (the old sign
  # bug produced exactly that) and must never pass as a certificate.
  if (!is.finite(gap) || gap > tol * scale || gap < -tol * scale) {
    rlang::abort(sprintf("Cost-aware optimizer did not certify optimality (gap %.3g, scale %.3g).",
                         gap, scale))
  }
  # Return k original strategy weights, with a relative-gap ATTRIBUTE.
  # The attribute is metadata, not an additional weight. estimate() removes
  # it with as.vector().
  structure(x, gap = gap / scale)
}

#' Treat a factor's zero stock weights as missing (their Table III lines 9-16)
stock_zero_as_missing <- function(stock, factor) {
  # Locate the requested factor column before changing any stock panel.
  # This reproduces the authors' BAB convention: zero is treated as an
  # unobserved holding, not an explicit observed position of zero.
  j <- match(factor, stock$factors)
  if (is.na(j)) rlang::abort(paste("Unknown factor:", factor))
  # Replace observed exact zeros only; existing missing values stay missing.
  # The stock-cost engine then omits the affected BAB entry/exit transitions
  # under its missing-data convention. Other factor columns are unchanged.
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
  # Produce one statistics row per portfolio column. A common resampling
  # calendar pairs the same months when comparing two portfolio returns.
  purrr::map_dfr(colnames(gross), function(col) {
    # Match cmv_notc with umv_notc, and cmv_tc with umv_tc. Thus the comparison
    # changes the available strategies, holding the optimization cost rule fixed.
    # UMV rows are the benchmarks and have no separate alpha/p-value here.
    bench <- if (startsWith(col, "cmv")) sub("cmv", "umv", col) else NA_character_
    # Monthly decimal returns are annualized arithmetically: mean times 12,
    # standard deviation times sqrt(12). Deliberately use NET mean but GROSS
    # SD, matching this replication's reporting convention. For A/B, net is
    # passed equal to gross, so reported TC=0 even for cost-aware weights.
    row <- tibble::tibble(column = col, mean = 12 * mean(net[, col]),
                          sd = sqrt(12) * stats::sd(gross[, col]),
                          tc = 12 * (mean(gross[, col]) - mean(net[, col])))
    # This reported SR uses the mean and gross SD just constructed. The
    # bootstrap comparison below instead receives the net return series.
    row$sr <- row$mean / row$sd
    if (is.na(bench)) {
      return(dplyr::mutate(row, p_value = NA_real_, alpha = NA_real_, t_alpha = NA_real_))
    }
    # Regress CMV net returns on an intercept and matching UMV net returns.
    # Lag-one Newey-West errors allow heteroskedasticity and serial dependence.
    # Annualize the monthly intercept by 12; its t-statistic is unchanged.
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

  # Inputs: one row per consecutive holding month, one column per factor.
  # months identifies those rows; stock supplies underlying stock holdings,
  # returns and cost rates to reconstruct trades. No data are loaded here.
  # M is the initial estimation length, refit the number of holding months
  # between weight updates, and gamma the mean-variance risk penalty.
  # draws/block_length/seed control inference, not portfolio optimization.
  # The returned list also includes settings, besides tables/returns/weights.
  n <- nrow(returns)
  factors <- colnames(returns)
  K <- length(factors)
  # Do not confuse this sigma INPUT (a volatility signal by month) with
  # the sigma ARGUMENT of mv_l1_nonneg() (a return covariance matrix).
  # A signal matrix scales each factor separately (FCMV). Reorder its
  # columns by factor names. A signal vector applies the SAME market
  # volatility to every factor (CMV) and is repeated across K columns.
  if (is.matrix(sigma)) {
    if (nrow(sigma) != n || !setequal(colnames(sigma), factors)) {
      rlang::abort("A sigma matrix must have one row per month and one column per factor.")
    }
    sigma <- sigma[, factors, drop = FALSE]
  } else {
    if (length(sigma) != n) rlang::abort("A sigma vector must have one value per month.")
    sigma <- matrix(sigma, n, K, dimnames = list(NULL, factors))
  }
  # Reject nonpositive/nonfinite signals, nonfinite returns, mismatched
  # calendar length, and missing or out-of-order monthly dates. This does
  # not itself establish economic availability; signals must already be
  # lagged appropriately before they are supplied to run_table3().
  if (length(months) != n || any(!is.finite(sigma) | sigma <= 0) ||
      any(!is.finite(returns)) || !identical(months, seq(months[1], by = "month", length.out = n))) {
    rlang::abort("Table III inputs must be finite, positive and on consecutive months.")
  }
  # Apply the special BAB zero-as-missing convention before constructing
  # optimization costs or evaluating realized costs; both use this copy.
  stock <- stock_zero_as_missing(stock, "bab")
  # The first K columns are unmanaged returns; the next K are managed
  # returns f/signal. A coefficient on a managed return therefore means
  # coefficient/signal units of the underlying factor in that holding month.
  # Example: factor return .02 and signal .01 give managed return 2;
  # a managed coefficient .03 contributes .03*2=.06 to portfolio return.
  ext <- cbind(returns, returns / sigma)
  colnames(ext) <- c(paste0("u_", factors), paste0("m_", factors))

  ## Unit-exposure stock trades of all 18 sleeves (their Phiext), months 2..T
  # Build unit STRATEGY exposures before choosing portfolio coefficients.
  # One unmanaged unit holds one factor unit; one managed unit holds
  # 1/signal factor units. These managed units vary monthly even while
  # the portfolio's strategy coefficients are held fixed between refits.
  # The long table retains month, factor and distinct sleeve identifiers.
  unit <- dplyr::bind_rows(
    tidyr::expand_grid(month_id = months, factor = factors) %>%
      dplyr::mutate(sleeve = paste0("u_", factor), w = 1),
    tibble::tibble(month_id = rep(months, K), factor = rep(factors, each = n)) %>%
      dplyr::mutate(sleeve = paste0("m_", factor), w = as.vector(1 / sigma))
  )
  # The trade engine accounts for underlying holdings and previous-month
  # stock-return drift. Its phi rows contain signed, cost-weighted trades
  # for unit sleeve coefficients. They are not final portfolio costs.
  trades <- stock_trade_matrices(stock, unit)
  # Stack monthly stock rows, explicitly ordering each block's columns
  # as ext: K unmanaged then K managed. Multiplying a row by a coefficient
  # vector combines its stock trades; taking abs AFTER that sum allows
  # cross-sleeve offsets for that stock, not offsets across different stocks.
  phi <- do.call(rbind, purrr::map(trades$phi, ~ .x[, colnames(ext), drop = FALSE]))
  # Trade blocks start at month 2 because month 1 has no preceding sample
  # holding for this unit-trade construction. Repeat each block's month
  # index once per stock row, so later masks select whole trading months.
  phi_month <- rep(seq_along(trades$phi) + 1L, purrr::map_int(trades$phi, nrow))
  # A stock-month with no trade coefficient in ANY sleeve contributes
  # zero under every candidate. Remove it and its matching month label.
  keep <- rowSums(phi != 0) > 0
  phi <- phi[keep, , drop = FALSE]
  phi_month <- phi_month[keep]

  # Usually use sample covariance. A condition number above 1e9 indicates
  # severe numerical sensitivity; reproduce the authors' scaled-identity
  # shrinkage fallback then. This choice is made separately for unmanaged
  # and extended return matrices in each estimation window.
  cov_used <- function(x) {
    s <- stats::cov(x)
    if (kappa(s, exact = TRUE) > 1e9) cov1para(x) else s
  }
  # Shared estimator for all panels. rows selects return observations;
  # phi_rows selects stock-trade observations; D is the number of estimation
  # RETURN months, not the number of stocks or nonzero trade rows.
  # Teaching example: K=2, rows=1:6, phi_rows selects trades into months
  # 2:7, D=6; returns are 6x2, extended returns 6x4, and with two stocks
  # the selected cost matrix can have 12 rows. drop=FALSE preserves matrices.
  estimate <- function(rows, phi_rows, D) {
    # Select unmanaged training returns only (all columns, requested rows).
    xu <- returns[rows, , drop = FALSE]
    # Select the same months, now including managed strategies as well.
    xe <- ext[rows, , drop = FALSE]
    # Use only unmanaged trade columns for the unmanaged-only optimization.
    Au <- phi[phi_rows, 1:K, drop = FALSE]
    # Discard rows inactive for unmanaged strategies, then divide by D.
    # For D=6, sum(abs((Phi/6)*x)) = sum(abs(Phi*x))/6: average monthly
    # cost, comparable with monthly means and covariance. Do not divide
    # by stock count or by the number of remaining nonzero observations.
    Au <- Au[rowSums(Au != 0) > 0, , drop = FALSE] / D
    # Combined strategies use all 2K columns, with the SAME monthly cost
    # normalization. Remaining zero rows would simply contribute zero.
    Ae <- phi[phi_rows, , drop = FALSE] / D
    # Paper column 1: K unmanaged means/covariances, no cost in the objective.
    # colMeans() estimates each strategy's expected monthly return.
    w_umv <- nonneg_mv(colMeans(xu), cov_used(xu), gamma)
    # Paper column 3: 2K unmanaged/managed strategies, still ignoring costs.
    w_cmv <- nonneg_mv(colMeans(xe), cov_used(xe), gamma)
    # Paper column 4: K unmanaged strategies, including netted stock costs.
    w_umv_tc <- mv_l1_nonneg(colMeans(xu), cov_used(xu), gamma, Au)
    # Paper column 6: all 2K strategies, including netted stock costs.
    w_cmv_tc <- mv_l1_nonneg(colMeans(xe), cov_used(xe), gamma, Ae)
    # Append K zero managed weights to unmanaged-only portfolios, so every
    # output has the same 2K row order. cbind creates four portfolio columns;
    # as.vector removes solver attributes without changing weight values.
    pad <- function(w) c(w, rep(0, K))
    cbind(umv_notc = pad(w_umv), cmv_notc = as.vector(w_cmv),
          umv_tc = pad(as.vector(w_umv_tc)), cmv_tc = as.vector(w_cmv_tc))
  }

  ## Panel A: in-sample on rows M+1..T; Phi of trades into months M+2..T
  # Use the evaluation segment itself to fit Panel A's fixed coefficients.
  # Example n=12,M=6: fit on returns July-December (rows 7:12), selecting
  # trades into August-December (8:12), and divide cost sums by D=6.
  # This is intentionally IN SAMPLE, not a forecast from January-June.
  is_rows <- (M + 1L):n
  w_is <- estimate(is_rows, phi_month >= M + 2L, n - M)
  # Apply those same estimated coefficients to those same return months.
  # Result: one row per evaluation month and four portfolio columns.
  # Panel A reports gross returns, including for cost-aware specifications.
  gross_a <- ext[is_rows, , drop = FALSE] %*% w_is

  ## OOS: refit every `refit` months on rows 1..t, trades into months 2..t+1
  # Panel B-D holding months start after M training months. For n=12,M=6,
  # i=1 denotes July, not January; t=M+i-1 is the last training-return row.
  # The estimator includes stock-trade rows into t+1 as in the authors'
  # convention. That timing selection is distinct from return rows 1:t;
  # it is not an independent proof of the availability of every input.
  oos_index <- seq_len(n - M)
  weights <- vector("list", length(oos_index))
  for (i in oos_index) {
    t <- M + i - 1L
    # Refit on the first holding month and every refit months afterward;
    # otherwise reuse the previous 2K-by-4 coefficient matrix. With refit=3,
    # July's fit uses Jan-Jun; Aug/Sep reuse it; October refits on Jan-Sep.
    # Training expands from the start; old observations are not rolled out.
    weights[[i]] <- if ((i - 1L) %% refit == 0L) {
      message("Table III refit at ", format(months[t + 1L]))
      estimate(seq_len(t), phi_month <= t + 1L, t)
    } else weights[[i - 1L]]
  }
  # Align every stored coefficient matrix with its actual holding month.
  # Compute four gross returns by multiplying that month's 2K strategy
  # returns by that month's 2K-by-4 weights. byrow=TRUE places each
  # four-return vector on one row. B, C and D all start from this matrix.
  hold <- months[M + oos_index]
  gross <- matrix(unlist(purrr::map(oos_index, ~ as.vector(ext[M + .x, ] %*% weights[[.x]]))),
                  ncol = 4, byrow = TRUE, dimnames = list(NULL, colnames(w_is)))

  ## Costs of the actual weight paths (entry from cash in the first OOS month)
  # Realized performance costs must follow the ACTUAL chosen weight paths,
  # including coefficient changes on refit dates, changing managed signals,
  # and initial entry from cash. They are not just the training-window A*x.
  # Evaluate each of the four portfolio specifications separately.
  cost_paths <- purrr::map(colnames(w_is), function(col) {
    exposures <- purrr::map_dfr(oos_index, function(i) {
      # Pick this portfolio's strategy coefficients for this holding month.
      # Convert managed coefficients to underlying factor exposures using
      # 1/signal; unmanaged coefficients multiply 1. Even unchanged strategy
      # coefficients can therefore imply different monthly factor exposures.
      w <- weights[[i]][, col]
      tibble::tibble(month_id = hold[i], sleeve = colnames(ext),
                     factor = rep(factors, 2), w = w * c(rep(1, K), 1 / sigma[M + i, ]))
    })
    # td nets trades across sleeves in the SAME stock before charging cost
    # (Panel D). ntd charges each sleeve separately (Panel C). For trades
    # +.03 and -.02 in one stock at rate .001, these charge .00001 and
    # .00005 respectively. Both charge entry from cash in the first month.
    # There is NO reoptimization between C and D. In particular, C charges
    # separate sleeve costs even for weights optimized assuming netting.
    list(td = stock_cost(stock, exposures, "portfolio", charge_entry = TRUE)$cost,
         ntd = stock_cost(stock, exposures, "sleeve", charge_entry = TRUE)$cost)
  }) %>% stats::setNames(colnames(w_is))
  # Collect one monthly cost column per portfolio, aligned with gross.
  # These costs are deducted even for _notc portfolios: that suffix means
  # costs were ignored during optimization, not that trading is free.
  tc_td <- sapply(cost_paths, `[[`, "td")
  tc_ntd <- sapply(cost_paths, `[[`, "ntd")

  # Build resampled month calendars for Sharpe-ratio inference. The seed
  # makes draws reproducible; sharing a calendar across portfolios preserves
  # paired return observations. In-sample and OOS periods get own calendars.
  calendar_is <- draw_portfolio_months(months[is_rows], draws = draws,
                                       block_length = block_length, seed = seed)
  calendar <- draw_portfolio_months(hold, draws = draws, block_length = block_length, seed = seed)
  # table3_panel_stats receives (gross returns, evaluated/net returns).
  # A: in-sample gross. B: OOS gross. C: B minus sleeve costs. D: B minus
  # portfolio-netted costs. A/B pass gross twice, so their reported TC is 0.
  # All panels contain the SAME four optimization specifications; only
  # A re-estimates on the evaluation sample. C/D reuse B's weights.
  table <- dplyr::bind_rows(
    dplyr::mutate(table3_panel_stats(gross_a, gross_a, calendar_is), panel = "A"),
    dplyr::mutate(table3_panel_stats(gross, gross, calendar), panel = "B"),
    dplyr::mutate(table3_panel_stats(gross, gross - tc_ntd, calendar), panel = "C"),
    dplyr::mutate(table3_panel_stats(gross, gross - tc_td, calendar), panel = "D")
  ) %>% dplyr::relocate(panel)

  # Return the displayed statistics plus underlying monthly series, weight
  # paths and settings for inspection. This function does not write files;
  # the separate demiguel_table3.R runner handles printing and persistence.
  list(table = table,
       returns = list(months_is = months[is_rows], gross_is = gross_a, months = hold, gross = gross,
                      tc_td = tc_td, tc_ntd = tc_ntd),
       weights = list(in_sample = w_is, oos = weights),
       settings = list(M = M, gamma = gamma, refit = refit, draws = draws,
                       block_length = block_length, seed = seed))
}

#' Compare a Table III result with the published values
compare_table3 <- function(table) {
  # Put each statistic into its own row and join by panel, portfolio column
  # and statistic. Only implemented columns with a published nonmissing
  # reference survive. difference is OUR value minus the published value;
  # comparison does not modify estimates or fit parameters to the targets.
  table %>%
    tidyr::pivot_longer(c(mean, sd, sr, p_value, alpha, t_alpha, tc),
                        names_to = "statistic", values_to = "ours") %>%
    dplyr::inner_join(tidyr::pivot_longer(demiguel_table3_paper, c(mean, sd, sr, p_value, alpha, t_alpha, tc),
                                          names_to = "statistic", values_to = "paper"),
                      by = c("panel", "column", "statistic")) %>%
    dplyr::filter(!is.na(paper)) %>%
    dplyr::mutate(difference = ours - paper)
}
