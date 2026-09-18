# Shared allocation rules for managed and unmanaged return sleeves
# ------------------------------------------------------------------------------

#' Allocate capital to a matrix of historical excess returns
#'
#' basis has one row per column, in the same explicit column-name order, and
#' columns id/name/sleeve/cluster. EW divides capital equally among columns.
#' HEW divides it equally among clusters, factors within clusters, then sleeves
#' within factors. MVO allows cash and does not impose a sum-to-one constraint.
#' Signed MVO implements Cederburg equation (15); nonnegative MVO uses DeMiguel.
allocate_portfolio <- function(x, basis, allocation = "mvo", gamma = 5,
                               cost = rep(0, ncol(x)), nonnegative = TRUE,
                               exposure_load = rep(1, ncol(x)), exposure_limit = NULL) {

  if (!is.matrix(x) || !is.numeric(x) || nrow(x) < 2L ||
      !ncol(x) || any(!is.finite(x))) {
    rlang::abort("Allocation returns must be a finite numeric matrix with at least two rows.")
  }
  if (!is.data.frame(basis) ||
      !all(c("id", "name", "sleeve", "cluster") %in% names(basis)) ||
      anyNA(basis) || anyDuplicated(basis$id) ||
      !identical(colnames(x), basis$id)) {
    rlang::abort("Basis metadata must match return-matrix column names exactly.")
  }
  allocation <- match.arg(allocation, c("mvo", "ew", "hew"))
  if (length(gamma) != 1L || !is.numeric(gamma) ||
      !is.finite(gamma) || gamma <= 0 ||
      !is.numeric(cost) || length(cost) != ncol(x) ||
      any(!is.finite(cost) | cost < 0)) {
    rlang::abort("Require positive gamma and one finite nonnegative cost per basis.")
  }

  ## EW and HEW are fully invested allocation rules. Check this explicitly so
  ## a future change to the hierarchy cannot silently create cash or leverage.
  validate_fully_invested <- function(coef) {
    if (any(!is.finite(coef) | coef < 0) || abs(sum(coef) - 1) > 1e-12) {
      rlang::abort("EW and HEW coefficients must be finite, nonnegative and sum to one.")
    }
    coef
  }

  if (allocation == "ew") {
    return(list(coef = validate_fully_invested(rep(1 / ncol(x), ncol(x))),
                kkt_residual = NA_real_))
  }
  if (allocation == "hew") {
    if (anyDuplicated(dplyr::distinct(basis, name, cluster)$name)) {
      rlang::abort("Every factor must belong to exactly one cluster.")
    }
    weights <- basis %>%
      dplyr::group_by(cluster) %>%
      dplyr::mutate(n_factors = dplyr::n_distinct(name)) %>%
      dplyr::group_by(cluster, name) %>%
      dplyr::mutate(weight = 1 / (dplyr::n_distinct(basis$cluster) * n_factors * dplyr::n())) %>%
      dplyr::ungroup()
    return(list(coef = validate_fully_invested(weights$weight), kkt_residual = NA_real_))
  }
  if (nonnegative) return(fit_multifactor(x, cost, gamma, exposure_load, exposure_limit))

  ## Cederburg permits short positions in either sleeve. Its sample-covariance
  ## formula requires identification: fail rather than adding a hidden ridge.
  ## A linear cost coefficient is not valid for signed coefficients, so this
  ## paper specification estimates gross moments and evaluates costs afterward.
  if (any(cost != 0)) rlang::abort("Signed MVO requires zero training cost coefficients.")
  scale <- apply(x, 2, stats::sd)
  if (any(!is.finite(scale) | scale <= 0)) rlang::abort("Constant return basis in signed MVO.")
  z <- sweep(x, 2, scale, "/")
  covariance <- stats::cov(z)
  if (rcond(covariance) < 1e-10) rlang::abort("Singular or ill-conditioned signed MVO covariance.")
  w <- as.vector(solve(covariance, colMeans(z))) / gamma
  list(coef = unname(w / scale),
    kkt_residual = max(abs(gamma * covariance %*% w - colMeans(z))))
}
