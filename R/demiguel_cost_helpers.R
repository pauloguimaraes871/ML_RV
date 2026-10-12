# Stock-level transaction costs from the DeMiguel et al. (2024) package
# ------------------------------------------------------------------------------
#
# The replication package (Mendeley Data, DOI 10.17632/dmjfztbc4y.3) ships the
# stock-level inputs behind the paper's costs, in MainDataRep.mat (MATLAB 7.3):
#
#   RPS(t, k, i)          weight x_{k,i,t} of stock i in the authors' CRSP
#                         rebuild of factor k, held during month t
#   BidAskSpreads(t, i)   kappa_{i,t}: cost per dollar traded (Abdi-Ranaldo)
#   rRisky(t, i)          stock return during month t
#
# for February 1967 - December 2020 (647 months, 24,991 stock slots). Our
# portfolios are factor-exposure paths; this file turns any such path into the
# stock trades it implies and prices them, as the paper does (equations 7-11).
#
# Arithmetic mirrors the authors' MATLAB exactly (Phi construction in
# VMPnew_TC_FFHXZBAB.m, OOS_nonnegative.m, ConstructReturnVarianceTCfigures.m):
#
#   trade_{j,i,t} = x_{k(j),i,t} w_{j,t} - x_{k(j),i,t-1} w_{j,t-1} (1 + r_{i,t-1})
#
# where sleeve j trades factor k(j) with exposure w (w = theta for an unmanaged
# sleeve, theta * scaling signal for a managed one). Missing values propagate
# (NaN in MATLAB, NA here) and are then DROPPED by nansum: a stock outside the
# CRSP universe in either month (weight NaN, or NaN return) is not charged.
# A listed stock that is simply not held has weight 0 and IS charged when it
# enters or leaves. This is their convention, kept for exact replication.
#
# Netting levels:
#   "sleeve"    sum_j sum_i kappa |trade_j|        no trading diversification
#   "factor"    sum_k sum_i kappa |sum_{j in k}|   netting within each factor
#   "portfolio" sum_i kappa |sum_j trade_j|         full trading diversification

  `%>%` <- dplyr::`%>%`

  ## Order of the factors in the package (headingsFF) and our keys for them
  demiguel_package_factors <- c(Market = "mkt", SMB = "smb", HML = "hml", RMW = "rmw",
                                CMA = "cma", UMD = "umd", ROE = "roe", IA = "ia", BAB = "bab")

#' Read and compact the stock-level arrays of MainDataRep.mat
#'
#' Returns list(months, factors, panels, factor_returns). panels[[t]] holds the
#' stocks that are active (finite, nonzero weight in any factor) in month t-1,
#' t or t+1, with their kappa, return and 9-column weight matrix x, NA kept
#' exactly where the package has NaN. Every stock that can generate a nonzero
#' trade in month t or t+1 is therefore present with its true NA/0 status.
#' factor_returns is the package's Rchar (provider returns), for checks only.
read_demiguel_stock_data <- function(path) {

  if (!file.exists(path)) rlang::abort(paste("Missing package data file:", path))
  file <- hdf5r::H5File$new(path, mode = "r")
  on.exit(file$close_all(), add = TRUE)

  yyyymm <- as.integer(file[["Dates"]]$read())
  months <- as.Date(sprintf("%06d01", yyyymm), "%Y%m%d")
  if (anyNA(months) || !identical(months, seq(months[1], by = "month", length.out = length(months)))) {
    rlang::abort("Package dates must be consecutive first-of-month values.")
  }
  ## headingsFF is a MATLAB cell array: object references to char arrays
  headings <- purrr::map_chr(file[["headingsFF"]]$read()$dereference(obj = file),
                             function(o) intToUtf8(as.integer(o$read())))
  if (!identical(headings, names(demiguel_package_factors))) {
    rlang::abort(paste("Unexpected package factor order:", paste(headings, collapse = ", ")))
  }

  dims <- file[["RPS"]]$dims
  n_months <- length(months)
  if (!identical(as.integer(dims[1:2]), c(n_months, 9L))) rlang::abort("Unexpected RPS dimensions.")
  n_stocks <- dims[3]

  kappa <- file[["BidAskSpreads"]]$read()
  ret <- file[["rRisky"]]$read()
  if (!identical(dim(kappa), c(n_months, n_stocks)) || !identical(dim(ret), c(n_months, n_stocks))) {
    rlang::abort("Spread/return arrays do not match the weight array.")
  }
  if (anyNA(kappa) || any(kappa < 0)) rlang::abort("Kappa must be finite and nonnegative.")

  ## Pass 1: which stocks carry a finite, nonzero weight in any factor
  active <- matrix(FALSE, n_months, n_stocks)
  for (k in 1:9) {
    x <- file[["RPS"]][, k, ]
    active <- active | (is.finite(x) & x != 0)
  }
  keep <- active
  keep[-1, ] <- keep[-1, ] | active[-n_months, ]
  keep[-n_months, ] <- keep[-n_months, ] | active[-1, ]
  ids <- purrr::map(seq_len(n_months), ~ which(keep[.x, ]))

  ## Pass 2: compact weights month by month, one factor at a time
  panels <- purrr::map(seq_len(n_months), function(t) {
    list(id = ids[[t]], kappa = kappa[t, ids[[t]]], ret = ret[t, ids[[t]]],
         x = matrix(NA_real_, length(ids[[t]]), 9,
                    dimnames = list(NULL, unname(demiguel_package_factors))))
  })
  for (k in 1:9) {
    x <- file[["RPS"]][, k, ]
    for (t in seq_len(n_months)) panels[[t]]$x[, k] <- x[t, ids[[t]]]
  }

  rchar <- file[["Rchar"]]$read()
  colnames(rchar) <- unname(demiguel_package_factors)

  list(months = months, factors = unname(demiguel_package_factors), panels = panels,
       n_stocks = n_stocks,
       factor_returns = tibble::as_tibble(rchar) %>%
         dplyr::mutate(month_id = months, .before = 1) %>%
         tidyr::pivot_longer(-month_id, names_to = "name", values_to = "factor_exc"))
}

#' Validate exposures and return a per-month trade builder (internal)
#'
#' Shared by stock_cost() and stock_trade_matrices() so both price exactly the
#' same trades. trades(i) returns the stock x sleeve trade matrix of month i of
#' the window (NA kept) and the month's kappa; month 1 has no previous position.
prepare_stock_trades <- function(stock, exposures) {

  validate_port_data(exposures, c("month_id", "sleeve"), "w", consecutive = FALSE)
  if (!all(exposures$factor %in% stock$factors)) rlang::abort("Unknown factor in exposures.")
  sleeves <- exposures %>% dplyr::distinct(sleeve, factor)
  if (anyDuplicated(sleeves$sleeve)) rlang::abort("Each sleeve must trade exactly one factor.")
  sleeves <- dplyr::arrange(sleeves, sleeve)

  wide <- exposures %>% dplyr::select(month_id, sleeve, w) %>%
    tidyr::pivot_wider(names_from = sleeve, values_from = w) %>% dplyr::arrange(month_id)
  window <- wide$month_id
  if (anyNA(wide) || !identical(window, seq(window[1], by = "month", length.out = length(window)))) {
    rlang::abort("Exposures must cover every sleeve in every month of a consecutive window.")
  }
  rows <- match(window, stock$months)
  if (anyNA(rows)) rlang::abort("Exposure months fall outside the stock-level data (Feb 1967 - Dec 2020).")
  W <- as.matrix(wide[, sleeves$sleeve, drop = FALSE])
  col <- match(sleeves$factor, stock$factors)

  ## Per-stock, per-sleeve trades for one month; previous values aligned by id
  trades <- function(i) {
    now <- stock$panels[[rows[i]]]
    current <- sweep(now$x[, col, drop = FALSE], 2, W[i, ], "*")
    colnames(current) <- sleeves$sleeve
    if (i == 1L) return(list(trade = current, kappa = now$kappa))
    prev <- stock$panels[[rows[i] - 1L]]
    at <- match(now$id, prev$id)
    drift <- prev$x[at, col, drop = FALSE] * (1 + prev$ret[at])
    list(trade = current - sweep(drift, 2, W[i - 1L, ], "*"), kappa = now$kappa)
  }

  list(sleeves = sleeves, window = window, trades = trades)
}

#' The authors' Phi matrices: kappa-weighted trades per stock and sleeve
#'
#' For each month of the window after the first, kappa_{i,t} * trade_{j,i,t},
#' with NA set to zero exactly as `Phiext(isnan(Phiext)) = 0` in their code.
#' Rows of month t are stocks; columns are the sleeves (sorted by name). The
#' L1 cost of constant sleeve weights b in month t is sum(abs(Phi_t %*% b)),
#' i.e. trades netted across the sleeves (within the factor, for Table I).
#' Returns list(months, phi) with one matrix per month 2, ..., T of the window.
stock_trade_matrices <- function(stock, exposures) {
  prepared <- prepare_stock_trades(stock, exposures)
  idx <- seq_along(prepared$window)[-1]
  phi <- purrr::map(idx, function(i) {
    tr <- prepared$trades(i)
    m <- tr$trade * tr$kappa
    m[is.na(m)] <- 0
    m
  })
  list(months = prepared$window[idx], sleeves = prepared$sleeves$sleeve, phi = phi)
}

#' Price a path of factor exposures through the stock trades it implies
#'
#' stock: read_demiguel_stock_data() output.
#' exposures: month_id/sleeve/factor/w. One row per sleeve and month over a
#'   consecutive window; w is the multiplier on factor `factor`'s stock weights
#'   (theta for an unmanaged sleeve, theta * signal for a managed one).
#' netting: "sleeve", "factor" or "portfolio" (see the file header).
#' charge_entry: TRUE charges the first month as entry from cash (their
#'   first-OOS-month branch); FALSE returns no cost for the first month, which
#'   only serves as the previous position (Figure 2B and in-sample Phi).
#' by_sleeve: return one cost per sleeve and month (netting = "sleeve" only).
#' Returns month_id/cost (or month_id/sleeve/cost) in decimal return units.
stock_cost <- function(stock, exposures, netting = c("sleeve", "factor", "portfolio"),
                       charge_entry = TRUE, by_sleeve = FALSE) {

  netting <- match.arg(netting)
  if (by_sleeve && netting != "sleeve") rlang::abort("by_sleeve requires netting = 'sleeve'.")
  prepared <- prepare_stock_trades(stock, exposures)
  sleeves <- prepared$sleeves
  window <- prepared$window

  ## MATLAB nansum: NA terms dropped, an all-NA sum is zero
  nansum_rows <- function(m) rowSums(m, na.rm = TRUE)

  per_month <- purrr::map(seq_along(window), function(i) {
    if (i == 1L && !charge_entry) return(rep(NA_real_, if (by_sleeve) nrow(sleeves) else 1L))
    tr <- prepared$trades(i)
    if (netting == "sleeve") {
      costs <- colSums(abs(tr$trade) * tr$kappa, na.rm = TRUE)
      return(if (by_sleeve) unname(costs) else sum(costs))
    }
    groups <- if (netting == "portfolio") rep(1L, nrow(sleeves)) else match(sleeves$factor, unique(sleeves$factor))
    sum(purrr::map_dbl(unique(groups), function(g) {
      sum(abs(nansum_rows(tr$trade[, groups == g, drop = FALSE])) * tr$kappa, na.rm = TRUE)
    }))
  })

  if (by_sleeve) {
    return(tibble::tibble(month_id = rep(window, each = nrow(sleeves)),
                          sleeve = rep(sleeves$sleeve, length(window)),
                          cost = unlist(per_month)))
  }
  tibble::tibble(month_id = window, cost = unlist(per_month))
}

#' Monthly cost of holding one unit of each factor (Figure 2B, equation 11)
#'
#' Constant exposure w = 1 per factor, no netting, over [start, end]. The first
#' month is the reference position (no cost), as in their figure code.
factor_rebalancing_costs <- function(stock, start, end) {
  months <- seq(start, end, by = "month")
  exposures <- tidyr::expand_grid(month_id = months, factor = stock$factors) %>%
    dplyr::mutate(sleeve = factor, w = 1)
  stock_cost(stock, exposures, netting = "sleeve", charge_entry = FALSE, by_sleeve = TRUE) %>%
    dplyr::rename(name = sleeve)
}
