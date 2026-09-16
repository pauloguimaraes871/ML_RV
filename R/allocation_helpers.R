# Shared allocation rules for managed and unmanaged return sleeves
# ------------------------------------------------------------------------------

#' Allocate capital to a matrix of historical excess returns
#'
#' basis has one row per column, in the same explicit column-name order, and
#' columns id/name/sleeve/cluster. EW divides capital equally among columns.
#' HEW divides it equally among clusters, factors within clusters, then sleeves
#' within factors. MVO allows cash and does not impose a sum-to-one constraint.
#' Signed MVO implements Cederburg equation (15); nonnegative MVO uses DeMiguel.
allocate_portfolio <- function(x, basis, method = "mvo", gamma = 5,
                               cost = base::rep(0, base::ncol(x)), nonnegative = TRUE) {

  if (!base::is.matrix(x) || !base::is.numeric(x) || base::nrow(x) < 2L ||
      !base::ncol(x) || base::any(!base::is.finite(x))) {
    rlang::abort("Allocation returns must be a finite numeric matrix with at least two rows.")
  }
  if (!base::is.data.frame(basis) ||
      !base::all(c("id", "name", "sleeve", "cluster") %in% base::names(basis)) ||
      base::anyNA(basis) || base::anyDuplicated(basis$id) ||
      !base::identical(base::colnames(x), basis$id)) {
    rlang::abort("Basis metadata must match return-matrix column names exactly.")
  }
  method <- base::match.arg(method, c("mvo", "ew", "hew"))
  if (base::length(gamma) != 1L || !base::is.numeric(gamma) ||
      !base::is.finite(gamma) || gamma <= 0 ||
      !base::is.numeric(cost) || base::length(cost) != base::ncol(x) ||
      base::any(!base::is.finite(cost) | cost < 0)) {
    rlang::abort("Require positive gamma and one finite nonnegative cost per basis.")
  }

  if (method == "ew") {
    return(base::list(coef = base::rep(1 / base::ncol(x), base::ncol(x)),
                      kkt_residual = NA_real_))
  }
  if (method == "hew") {
    if (base::anyDuplicated(dplyr::distinct(basis, name, cluster)$name)) {
      rlang::abort("Every factor must belong to exactly one cluster.")
    }
    weights <- basis %>%
      dplyr::group_by(cluster) %>%
      dplyr::mutate(n_factors = dplyr::n_distinct(name)) %>%
      dplyr::group_by(cluster, name) %>%
      dplyr::mutate(weight = 1 / (dplyr::n_distinct(basis$cluster) * n_factors * dplyr::n())) %>%
      dplyr::ungroup()
    return(base::list(coef = weights$weight, kkt_residual = NA_real_))
  }
  if (nonnegative) return(fit_multifactor(x, cost, gamma))

  ## Cederburg permits short positions in either sleeve. Its sample-covariance
  ## formula requires identification: fail rather than adding a hidden ridge.
  ## A linear cost coefficient is not valid for signed coefficients, so this
  ## paper specification estimates gross moments and evaluates costs afterward.
  if (base::any(cost != 0)) rlang::abort("Signed MVO requires zero training cost coefficients.")
  scale <- base::apply(x, 2, stats::sd)
  if (base::any(!base::is.finite(scale) | scale <= 0)) rlang::abort("Constant return basis in signed MVO.")
  z <- base::sweep(x, 2, scale, "/")
  covariance <- stats::cov(z)
  if (base::rcond(covariance) < 1e-10) rlang::abort("Singular or ill-conditioned signed MVO covariance.")
  w <- base::as.vector(base::solve(covariance, base::colMeans(z))) / gamma
  base::list(coef = base::unname(w / scale),
    kkt_residual = base::max(base::abs(gamma * covariance %*% w - base::colMeans(z))))
}
