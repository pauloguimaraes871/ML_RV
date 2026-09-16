# Cederburg combinations of each factor and its own managed counterpart
# ------------------------------------------------------------------------------

#' Allocate across one unmanaged factor and its own-volatility-managed sleeve
#'
#' portfolio is one saved run_oos_ports() result, never an ex-post-normalized run.
#' managed_method selects vol_scaled (own ML variance) or rw (lagged own variance).
#' normalization = training_vol reproduces the rolling-in-time recalibration in
#' section 4.3.1: a SINGLE c_t matches training standard deviations at each fit.
#' normalization = saved instead reuses the saved historical w_raw unchanged,
#' including its expanding_rv normalization. Equation (15) works in either case.
#'
#' All moments use min_history or more strictly prior observations. MVO is signed,
#' with gamma=5 and a final |factor exposure| cap of five as in the paper. EW is
#' 50/50 in the two sleeve returns. Original-only MVO is the paired benchmark.
#' Costs are charged on the combined factor exposure, so opposing sleeve trades
#' net within the same factor. No stock-level or internal factor costs are claimed.
run_cederburg_ports <- function(portfolio, managed_method = "vol_scaled",
                                normalization = "training_vol", min_history = 120L,
                                gamma = 5, exposure_cap = 5, cost_per_unit = .001) {

  normalization <- base::match.arg(normalization, c("training_vol", "saved"))
  managed_method <- base::match.arg(managed_method, c("vol_scaled", "rw"))
  if (!base::is.list(portfolio) || !base::isTRUE(portfolio$settings$implementable)) {
    rlang::abort("Cederburg requires an implementable saved MM portfolio.")
  }
  parameters <- base::list(min_history, gamma, exposure_cap, cost_per_unit)
  purrr::walk(parameters, function(x) {
    if (!base::is.numeric(x) || base::length(x) != 1L || !base::is.finite(x)) {
      rlang::abort("Cederburg parameters must be finite numeric scalars.")
    }
  })
  if (min_history < 2 || min_history != base::as.integer(min_history) ||
      gamma <= 0 || exposure_cap <= 0 || cost_per_unit < 0) {
    rlang::abort("Invalid Cederburg history, gamma, cap or cost.")
  }
  data <- portfolio$w_compiled_df %>% dplyr::filter(method == managed_method)
  validate_port_data(data, c("name", "month_id"), c("factor_exc", "rf", "signal_rv", "w_raw"))
  if (base::any(data$signal_rv <= 0 | data$w_raw < 0)) rlang::abort("Invalid managed variance/exposure.")

  weights <- data %>% dplyr::group_by(name) %>% dplyr::group_split() %>%
    purrr::map_dfr(function(factor) {
      factor <- dplyr::arrange(factor, month_id)
      if (base::nrow(factor) < min_history + 2L) {
        rlang::abort(base::paste("Insufficient Cederburg history:", factor$name[1]))
      }
      purrr::map_dfr(base::seq.int(min_history + 1L, base::nrow(factor)), function(i) {
        past <- base::seq_len(i - 1L)
        f <- factor$factor_exc[past]
        scale <- factor$w_raw
        c_t <- NA_real_
        if (normalization == "training_vol") {
          c_t <- stats::sd(f) / stats::sd(f / factor$signal_rv[past])
          if (!base::is.finite(c_t) || c_t <= 0) rlang::abort("Cannot normalize Cederburg training returns.")
          scale <- c_t / factor$signal_rv
        }
        x <- base::cbind(unmanaged = f, managed = f * scale[past])
        basis <- tibble::tibble(id = base::colnames(x), name = factor$name[1],
                                sleeve = base::colnames(x), cluster = "single_factor")
        purrr::map_dfr(c("mvo", "ew", "unmanaged_mvo"), function(method) {
          selected <- if (method == "unmanaged_mvo") 1L else 1:2
          fit <- allocate_portfolio(x[, selected, drop = FALSE], basis[selected, ],
            if (method == "ew") "ew" else "mvo", gamma = gamma, nonnegative = FALSE)
          a <- fit$coef[1]
          b <- if (method == "unmanaged_mvo") 0 else fit$coef[2]
          tibble::tibble(name = factor$name[i], month_id = factor$month_id[i],
            method = method, a = a, b = b, scaler = scale[i], normalizer = c_t,
            w_raw = a + b * scale[i], factor_exc = factor$factor_exc[i], rf = factor$rf[i],
            n_train = i - 1L, train_end = factor$month_id[i - 1L],
            kkt_residual = fit$kkt_residual)
        })
      })
    }) %>% dplyr::arrange(name, method, month_id) %>%
    dplyr::group_by(name, method) %>% dplyr::mutate(
      w_capped = base::pmax(-exposure_cap, base::pmin(exposure_cap, w_raw)),
      turnover_raw = base::abs(w_raw - dplyr::lag(w_raw, default = 0)),
      turnover_capped = base::abs(w_capped - dplyr::lag(w_capped, default = 0)),
      gross_raw = w_raw * factor_exc, gross_capped = w_capped * factor_exc,
      tc_raw = cost_per_unit * turnover_raw, tc_capped = cost_per_unit * turnover_capped,
      net_raw = gross_raw - tc_raw, net_capped = gross_capped - tc_capped
    ) %>% dplyr::ungroup()

  validate_port_data(weights, c("name", "method", "month_id"),
    c("w_raw", "w_capped", "net_raw", "net_capped"), consecutive = FALSE)
  summary <- purrr::map_dfr(c("raw", "capped"), function(version) {
    weights %>% dplyr::group_by(name, method) %>% dplyr::group_modify(function(x, key) {
      net <- x[[base::paste0("net_", version)]]
      volatility <- stats::sd(net)
      tibble::tibble(version = version, n = base::nrow(x), start = base::min(x$month_id),
        end = base::max(x$month_id), mean_net_ann = 12 * base::mean(net),
        sharpe_net = if (volatility > 0) base::sqrt(12) * base::mean(net) / volatility else NA_real_,
        cer_net_ann = 12 * (base::mean(net) - gamma / 2 * stats::var(net)),
        insolvency_months = base::sum(1 + x$rf + net <= 0))
    }) %>% dplyr::ungroup()
  })
  base::list(weights = weights, strategy_summary = summary,
    settings = base::list(normalization = normalization, managed_method = managed_method,
      source_normalization = portfolio$settings$normalization, min_history = min_history,
      gamma = gamma, exposure_cap = exposure_cap, cost_per_unit = cost_per_unit,
      initial_weight = 0, estimation = "Signed sample MVO, gross moments, strictly prior returns"))
}
