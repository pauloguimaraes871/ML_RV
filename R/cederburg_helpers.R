# Cederburg combinations of each factor and its own managed counterpart
# ------------------------------------------------------------------------------

#' Allocate across one unmanaged factor and its own-volatility-managed sleeve
#'
#' portfolio is one saved run_oos_ports() result. An ex-post MM scale is usable
#' only when training_vol re-estimates Cederburg's real-time c_t.
#' managed_method selects vol_scaled (own ML variance) or rw (lagged own variance).
#' normalization = training_vol reproduces the expanding real-time recalibration
#' in section 4.3.1: c_t matches the two sleeve standard deviations using only
#' returns available before the holding month.
#' normalization = saved instead reuses the saved historical w_raw unchanged.
#' It is available only when that source MM normalization was implementable.
#'
#' All moments use min_history or more strictly prior observations. MVO is signed,
#' with gamma = 5 and a final |factor exposure| cap of five as in the paper. EW
#' is 50/50 in the two sleeve returns. Original-only MVO is the paired benchmark.
#' Costs are charged on the combined factor exposure, so opposing sleeve trades
#' net within the same factor. No stock-level or internal factor costs are claimed.
run_cederburg_ports <- function(portfolio, managed_method = "vol_scaled",
                                normalization = "training_vol", min_history = 120L,
                                gamma = 5, exposure_cap = 5, cost_per_unit = .001) {

# Validate saved inputs ---------------------------------------------------------

  normalization <- match.arg(normalization, c("training_vol", "saved"))
  managed_method <- match.arg(managed_method, c("vol_scaled", "rw"))
  if (!is.list(portfolio) || is.null(portfolio$settings$implementable)) {
    rlang::abort("Cederburg requires a saved MM portfolio with implementation metadata.")
  }
  if (normalization == "saved" && !isTRUE(portfolio$settings$implementable)) {
    rlang::abort("Cederburg's saved normalization cannot reuse an ex-post MM scale.")
  }
  parameters <- list(min_history, gamma, exposure_cap, cost_per_unit)
  purrr::walk(parameters, function(x) {
    if (!is.numeric(x) || length(x) != 1L || !is.finite(x)) {
      rlang::abort("Cederburg parameters must be finite numeric scalars.")
    }
  })
  if (min_history < 2 || min_history != as.integer(min_history) ||
      gamma <= 0 || exposure_cap <= 0 || cost_per_unit < 0) {
    rlang::abort("Invalid Cederburg history, gamma, cap or cost.")
  }

# Select the managed sleeve -----------------------------------------------------

  ## vol_scaled uses the factor's ML variance forecast. rw is Cederburg's
  ## lagged own-realized-variance benchmark. Both retain the same factor return.
  data <- portfolio$w_compiled_df %>% dplyr::filter(method == managed_method)
  validate_port_data(data, c("name", "month_id"), c("factor_exc", "rf", "signal_rv", "w_raw"))
  if (any(data$signal_rv <= 0 | data$w_raw < 0)) {
    rlang::abort("Invalid managed variance/exposure.")
  }

# Form portfolios using strictly prior returns ----------------------------------

  weights <- data %>% dplyr::group_by(name) %>% dplyr::group_split() %>%
    purrr::map_dfr(function(factor) {
      factor <- dplyr::arrange(factor, month_id)
      if (nrow(factor) < min_history + 2L) {
        rlang::abort(paste("Insufficient Cederburg history:", factor$name[1]))
      }
      purrr::map_dfr(seq.int(min_history + 1L, nrow(factor)), function(i) {
        past <- seq_len(i - 1L)
        f <- factor$factor_exc[past]

        ## The saved scaler is used only for the explicit saved-normalization
        ## sensitivity. Cederburg's real-time specification re-estimates c_t.
        scale <- factor$w_raw
        c_t <- NA_real_
        if (normalization == "training_vol") {
          c_t <- stats::sd(f) / stats::sd(f / factor$signal_rv[past])
          if (!is.finite(c_t) || c_t <= 0) {
            rlang::abort("Cannot normalize Cederburg training returns.")
          }
          scale <- c_t / factor$signal_rv
        }

        ## The training matrix has one unmanaged and one own-volatility-managed
        ## sleeve. The holding-month scale is applied only after this fit.
        x <- cbind(unmanaged = f, managed = f * scale[past])
        basis <- tibble::tibble(id = colnames(x), name = factor$name[1],
                                sleeve = colnames(x), cluster = "single_factor")

        ## MVO follows Cederburg's signed two-sleeve allocation. EW is the
        ## transparent 50/50 alternative; unmanaged_mvo is its paired benchmark.
        purrr::map_dfr(c("mvo", "ew", "unmanaged_mvo"), function(method) {
          selected <- if (method == "unmanaged_mvo") 1L else 1:2
          fit <- allocate_portfolio(x[, selected, drop = FALSE], basis[selected, ],
            allocation = if (method == "ew") "ew" else "mvo",
            gamma = gamma, nonnegative = FALSE)
          a <- fit$coef[1]
          b <- if (method == "unmanaged_mvo") 0 else fit$coef[2]
          tibble::tibble(
            name = factor$name[i], month_id = factor$month_id[i], method = method,
            a = a, b = b, scaler = scale[i], normalizer = c_t,
            w_raw = a + b * scale[i], factor_exc = factor$factor_exc[i], rf = factor$rf[i],
            n_train = i - 1L, train_end = factor$month_id[i - 1L],
            kkt_residual = fit$kkt_residual
          )
        })
      })
    }) %>% dplyr::arrange(name, method, month_id) %>%

    ## Costs apply to the combined factor exposure, so changes that offset
    ## between sleeves net before this factor-level transaction-cost proxy.
    dplyr::group_by(name, method) %>% dplyr::mutate(
      w_capped = pmax(-exposure_cap, pmin(exposure_cap, w_raw)),
      turnover_raw = abs(w_raw - dplyr::lag(w_raw, default = 0)),
      turnover_capped = abs(w_capped - dplyr::lag(w_capped, default = 0)),
      gross_raw = w_raw * factor_exc, gross_capped = w_capped * factor_exc,
      tc_raw = cost_per_unit * turnover_raw, tc_capped = cost_per_unit * turnover_capped,
      net_raw = gross_raw - tc_raw, net_capped = gross_capped - tc_capped
    ) %>% dplyr::ungroup()

  validate_port_data(weights, c("name", "method", "month_id"),
    c("w_raw", "w_capped", "net_raw", "net_capped"), consecutive = FALSE)

# Summarize gross exposure, costs and net returns --------------------------------

  summary <- purrr::map_dfr(c("raw", "capped"), function(version) {
    weights %>% dplyr::group_by(name, method) %>% dplyr::group_modify(function(x, key) {
      net <- x[[paste0("net_", version)]]
      volatility <- stats::sd(net)
      tibble::tibble(
        version = version, n = nrow(x), start = min(x$month_id),
        end = max(x$month_id), mean_net_ann = 12 * mean(net),
        sharpe_net = if (volatility > 0) sqrt(12) * mean(net) / volatility else NA_real_,
        cer_net_ann = 12 * (mean(net) - gamma / 2 * stats::var(net)),
        insolvency_months = sum(1 + x$rf + net <= 0)
      )
    }) %>% dplyr::ungroup()
  })

  list(weights = weights, strategy_summary = summary,
    settings = list(normalization = normalization, managed_method = managed_method,
      source_normalization = portfolio$settings$normalization, min_history = min_history,
      gamma = gamma, exposure_cap = exposure_cap, cost_per_unit = cost_per_unit,
      initial_weight = 0, estimation = "Signed sample MVO, gross moments, strictly prior returns"))
}
