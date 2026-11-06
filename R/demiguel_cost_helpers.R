# Stock-level transaction costs from the DeMiguel et al. (2024) package
# ------------------------------------------------------------------------------
#
# WHAT THIS FILE DOES
#
# A factor is a long-short portfolio of stocks. Holding "w units of factor k"
# in month t means holding w * x_{k,i,t} dollars of every stock i, where
# x_{k,i,t} are the factor's stock weights. When w changes, or when the factor
# itself rebalances (x changes, or past returns make the weights drift), stocks
# must be bought and sold, and each dollar traded in stock i costs kappa_{i,t}.
# This file turns a path of factor exposures w (what our portfolios decide)
# into the stock trades it implies and prices them, exactly as the paper does
# (equations 7-11).
#
# THE INPUTS (replication package, Mendeley Data, DOI 10.17632/dmjfztbc4y.3,
# file MainDataRep.mat, MATLAB 7.3 = HDF5):
#
#   RPS(t, k, i)          x_{k,i,t}: weight of stock i in the authors' CRSP
#                         rebuild of factor k, held during month t
#   BidAskSpreads(t, i)   kappa_{i,t}: cost per dollar traded (Abdi-Ranaldo
#                         effective half-spread estimate)
#   rRisky(t, i)          r_{i,t}: stock return during month t
#
# for February 1967 - December 2020 (647 months, 24,991 stock slots). Months
# outside that range cannot be priced (prepare_stock_trades() aborts).
#
# VOCABULARY
#
#   sleeve     one position the portfolio holds on ONE factor, with its own
#              exposure path w_{j,t}. Table I has two sleeves per factor
#              ("unmanaged": w = a; "managed": w = b * c / sigma2_t); Table III
#              has 18 (u_<factor> and m_<factor> for the nine factors).
#   exposures  a long table month_id / sleeve / factor / w, one row per sleeve
#              and month, over a consecutive window of months.
#
# THE TRADE (their Phi construction: VMPnew_TC_FFHXZBAB.m lines 35-40,
# OOS_nonnegative.m, ConstructReturnVarianceTCfigures.m):
#
#   trade_{j,i,t} = x_{k(j),i,t} w_{j,t} - x_{k(j),i,t-1} w_{j,t-1} (1 + r_{i,t-1})
#                   \_____ target _____/   \_______ what we hold after the ______/
#                                                   previous month's return
#
# Sleeve j trades factor k(j). The second term is last month's dollar position
# grown by the stock's return over last month ("drift"): you only trade the gap
# between that and the new target.
#
# MISSING VALUES. A stock outside the CRSP universe in a month has weight NaN
# (MATLAB) / NA (here); a missing return is NA too. NA propagates through the
# trade formula and is then DROPPED by MATLAB's nansum (na.rm = TRUE here), so
# such a stock is simply not charged. A listed stock that the factor just does
# not hold has weight 0, not NA, and IS charged when it enters or leaves. This
# is the authors' convention, kept for exact replication.
#
# NETTING LEVELS (stock_cost(), argument netting). With trades T_{j,i,t}:
#
#   "sleeve"    sum_j sum_i kappa_i |T_{j,i}|             each sleeve trades alone:
#                                                         NO trading diversification
#   "factor"    sum_k sum_i kappa_i |sum_{j on k} T_{j,i}| sleeves on the SAME factor
#                                                         net their stock trades first
#   "portfolio" sum_i kappa_i |sum_j T_{j,i}|             all sleeves net (one book):
#                                                         FULL trading diversification
#
# Since |a + b| <= |a| + |b|: portfolio <= factor <= sleeve, always. Example
# (stock A, kappa = .01): sleeve u_umd buys .25 of A and sleeve m_umd sells .25
# of A. "sleeve" charges .01 * (.25 + .25) = .005; "factor" nets them to 0 and
# charges nothing. If instead u_hml (a different factor) sold the .25, "factor"
# would still charge .005 (different factors), "portfolio" would charge 0.
# Paper mapping: Table I Panel D and Table III Panel C use "sleeve"; Table I
# Panel E uses netting within the factor; Table III Panel D uses "portfolio".

  `%>%` <- dplyr::`%>%`

  ## Order of the factors in the package (headingsFF) and our keys for them.
  ## The column order of every weight matrix below follows this vector.
  demiguel_package_factors <- c(Market = "mkt", SMB = "smb", HML = "hml", RMW = "rmw",
                                CMA = "cma", UMD = "umd", ROE = "roe", IA = "ia", BAB = "bab")

#' Read and compact the stock-level arrays of MainDataRep.mat
#'
#' Returns list(months, factors, panels, n_stocks, factor_returns).
#'
#' The raw RPS array is 647 x 9 x 24,991 doubles (about 1.2 GB), almost all of
#' it NaN or 0 (a stock exists only for part of the sample). It is compacted to
#' one small "panel" per month: panels[[t]] = list(id, kappa, ret, x), where
#'   id     the package column numbers of the stocks kept for month t
#'   kappa  their kappa_{i,t};  ret  their r_{i,t}
#'   x      length(id) x 9 weight matrix (columns = factors), NA where the
#'          package has NaN.
#' Which stocks are kept for month t: those active (finite AND nonzero weight
#' in at least one factor) in month t-1, t or t+1. A trade in month t involves
#' the positions of t-1 and t; the t+1 neighbour makes panel t also serve as
#' the "previous month" of t+1. Every stock that can generate a nonzero trade
#' is therefore present, with its true NA/0 status. Stocks never active nearby
#' would only produce 0 or NA trades, which cost nothing either way.
#' factor_returns is the package's Rchar (the authors' factor returns), kept
#' for validation only; our tables earn our provider returns.
read_demiguel_stock_data <- function(path) {

  if (!file.exists(path)) rlang::abort(paste("Missing package data file:", path))
  file <- hdf5r::H5File$new(path, mode = "r")
  on.exit(file$close_all(), add = TRUE)

  ## Dates are stored as yyyymm numbers; turn them into first-of-month dates
  ## and insist on an unbroken monthly sequence.
  yyyymm <- as.integer(file[["Dates"]]$read())
  months <- as.Date(sprintf("%06d01", yyyymm), "%Y%m%d")
  if (anyNA(months) || !identical(months, seq(months[1], by = "month", length.out = length(months)))) {
    rlang::abort("Package dates must be consecutive first-of-month values.")
  }
  ## headingsFF is a MATLAB cell array: object references to char arrays.
  ## Its order fixes which RPS slice is which factor, so it must match ours.
  headings <- purrr::map_chr(file[["headingsFF"]]$read()$dereference(obj = file),
                             function(o) intToUtf8(as.integer(o$read())))
  if (!identical(headings, names(demiguel_package_factors))) {
    rlang::abort(paste("Unexpected package factor order:", paste(headings, collapse = ", ")))
  }

  ## Shape checks: RPS is months x factors x stocks; kappa and returns are
  ## months x stocks on the same stock columns.
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
  ## (read one factor slice at a time to bound memory), then widen each month's
  ## set by its two neighbours (see the roxygen block above).
  active <- matrix(FALSE, n_months, n_stocks)
  for (k in 1:9) {
    x <- file[["RPS"]][, k, ]
    active <- active | (is.finite(x) & x != 0)
  }
  keep <- active
  keep[-1, ] <- keep[-1, ] | active[-n_months, ]          # active in t - 1
  keep[-n_months, ] <- keep[-n_months, ] | active[-1, ]   # active in t + 1
  ids <- purrr::map(seq_len(n_months), ~ which(keep[.x, ]))

  ## Pass 2: allocate each month's panel, then fill its weight matrix one
  ## factor at a time (NA preserved exactly where the package has NaN)
  panels <- purrr::map(seq_len(n_months), function(t) {
    list(id = ids[[t]], kappa = kappa[t, ids[[t]]], ret = ret[t, ids[[t]]],
         x = matrix(NA_real_, length(ids[[t]]), 9,
                    dimnames = list(NULL, unname(demiguel_package_factors))))
  })
  for (k in 1:9) {
    x <- file[["RPS"]][, k, ]
    for (t in seq_len(n_months)) panels[[t]]$x[, k] <- x[t, ids[[t]]]
  }

  ## The authors' monthly factor returns, long format, for validation checks
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
#' same trades. Returns list(sleeves, window, trades):
#'   sleeves  sleeve/factor table, sorted by sleeve name (this order is the
#'            column order of every trade matrix)
#'   window   the consecutive months covered by the exposures
#'   trades   function(i): for month i of the window (1 = first month),
#'            list(trade = stocks x sleeves matrix of trade_{j,i,t} with NA
#'            kept, kappa = kappa of those stocks). For i = 1 there is no
#'            previous position inside the window, so the "trade" is the full
#'            target position: buying it from cash (entry).
prepare_stock_trades <- function(stock, exposures) {

  ## (1) Exposure table: unique month/sleeve keys, numeric finite w; every
  ##     sleeve trades exactly one known factor.
  validate_port_data(exposures, c("month_id", "sleeve"), "w", consecutive = FALSE)
  if (!all(exposures$factor %in% stock$factors)) rlang::abort("Unknown factor in exposures.")
  sleeves <- exposures %>% dplyr::distinct(sleeve, factor)
  if (anyDuplicated(sleeves$sleeve)) rlang::abort("Each sleeve must trade exactly one factor.")
  sleeves <- dplyr::arrange(sleeves, sleeve)

  ## (2) Wide exposure matrix W: one row per month, one column per sleeve.
  ##     A missing sleeve-month would appear as NA and aborts; months must be
  ##     consecutive and inside the package's Feb 1967 - Dec 2020 range.
  wide <- exposures %>% dplyr::select(month_id, sleeve, w) %>%
    tidyr::pivot_wider(names_from = sleeve, values_from = w) %>% dplyr::arrange(month_id)
  window <- wide$month_id
  if (anyNA(wide) || !identical(window, seq(window[1], by = "month", length.out = length(window)))) {
    rlang::abort("Exposures must cover every sleeve in every month of a consecutive window.")
  }
  rows <- match(window, stock$months)   # month i of the window = panel rows[i]
  if (anyNA(rows)) rlang::abort("Exposure months fall outside the stock-level data (Feb 1967 - Dec 2020).")
  W <- as.matrix(wide[, sleeves$sleeve, drop = FALSE])
  col <- match(sleeves$factor, stock$factors)   # weight-matrix column of each sleeve's factor

  ## (3) The trade builder. Per-stock, per-sleeve trades for one month.
  trades <- function(i) {
    now <- stock$panels[[rows[i]]]
    ## Target dollar position of every sleeve: x_{k(j),i,t} * w_{j,t}
    ## (stocks x sleeves; a sleeve's column repeats its factor's weights,
    ## multiplied by that sleeve's exposure this month)
    current <- sweep(now$x[, col, drop = FALSE], 2, W[i, ], "*")
    colnames(current) <- sleeves$sleeve
    ## First month of the window: entry from cash, the trade is the position
    if (i == 1L) return(list(trade = current, kappa = now$kappa))
    ## Previous month's position, aligned to THIS month's stocks by package id
    ## (panels hold different stock sets; a stock absent from the previous
    ## panel gets NA from match(), i.e. it is not charged, as with NaN in
    ## MATLAB), grown by last month's stock return: x_{t-1} (1 + r_{t-1})
    prev <- stock$panels[[rows[i] - 1L]]
    at <- match(now$id, prev$id)
    drift <- prev$x[at, col, drop = FALSE] * (1 + prev$ret[at])
    ## trade = target - drifted previous position (times last month's w)
    list(trade = current - sweep(drift, 2, W[i - 1L, ], "*"), kappa = now$kappa)
  }

  list(sleeves = sleeves, window = window, trades = trades)
}

#' The authors' Phi matrices: kappa-weighted trades per stock and sleeve
#'
#' Used by the cost-AWARE optimizers, which need the trades as a matrix rather
#' than a priced total. For each month t of the window after the first,
#' Phi_t[i, j] = kappa_{i,t} * trade_{j,i,t} computed with UNIT exposures (the
#' caller passes the exposure per unit of optimization weight), and NA set to
#' zero exactly as `Phiext(isnan(Phiext)) = 0` in their code. Rows of month t
#' are stocks; columns are the sleeves (sorted by name). If the optimizer holds
#' constant weights b on the sleeves, the month-t cost with trades netted
#' across those sleeves is sum(abs(Phi_t %*% b)): linear inside, absolute
#' value per stock. (Within one factor for Table I; across all 18 sleeves for
#' Table III.)
#' Returns list(months, sleeves, phi) with one matrix per month 2, ..., T.
stock_trade_matrices <- function(stock, exposures) {
  prepared <- prepare_stock_trades(stock, exposures)
  idx <- seq_along(prepared$window)[-1]    # month 1 has no previous position
  phi <- purrr::map(idx, function(i) {
    tr <- prepared$trades(i)
    m <- tr$trade * tr$kappa               # kappa recycles down rows (per stock)
    m[is.na(m)] <- 0                       # their Phiext(isnan(Phiext)) = 0
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
#'   first-OOS-month branch); FALSE returns NA cost for the first month, which
#'   only serves as the previous position (Figure 2B and in-sample Phi).
#' by_sleeve: return one cost per sleeve and month (netting = "sleeve" only:
#'   with netting there is no per-sleeve cost to report).
#' Returns month_id/cost (or month_id/sleeve/cost) in decimal return units:
#' a cost of .002 means 0.2% of capital that month.
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
    ## First month without entry charge: no cost (NA), it is only the base
    if (i == 1L && !charge_entry) return(rep(NA_real_, if (by_sleeve) nrow(sleeves) else 1L))
    tr <- prepared$trades(i)

    ## "sleeve": absolute value FIRST, per stock and per sleeve, then add up.
    ## Opposite trades of two sleeves in the same stock both pay.
    ##   cost_j = sum_i kappa_i |T_{j,i}|  (NA terms dropped)
    if (netting == "sleeve") {
      costs <- colSums(abs(tr$trade) * tr$kappa, na.rm = TRUE)
      return(if (by_sleeve) unname(costs) else sum(costs))
    }

    ## "factor" / "portfolio": ADD the sleeves' trades in each stock first
    ## (within a group), then take the absolute value, so opposite trades
    ## cancel. Groups: one per factor ("factor"), or a single group holding
    ## every sleeve ("portfolio").
    ##   cost = sum_g sum_i kappa_i |sum_{j in g} T_{j,i}|
    ## nansum across sleeves: a sleeve whose trade in stock i is NA drops out of
    ## that stock's net trade (as their Phiext(isnan) = 0 before summing); a
    ## stock with NA in every sleeve of the group costs nothing.
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
#' Constant exposure w = 1 per factor, no netting, over [start, end]. With w
#' constant, every trade comes from the factor's own rebalancing (new weights
#' and drift). The first month is the reference position (no cost), as in
#' their figure code. Its average over Feb 1977 - Dec 2020 is the paper's
#' Figure 2B (R/load_demiguel_costs.R aborts beyond 1 bp; observed within 0.4).
factor_rebalancing_costs <- function(stock, start, end) {
  months <- seq(start, end, by = "month")
  exposures <- tidyr::expand_grid(month_id = months, factor = stock$factors) %>%
    dplyr::mutate(sleeve = factor, w = 1)
  stock_cost(stock, exposures, netting = "sleeve", charge_entry = FALSE, by_sleeve = TRUE) %>%
    dplyr::rename(name = sleeve)
}
