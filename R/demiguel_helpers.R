# DeMiguel, Martin-Utrera and Uppal (2024) factor dataset
# ------------------------------------------------------------------------------
#
# Readers for the nine factors of DeMiguel et al. (2024, JF, DOI
# 10.1111/jofi.13395, Appendix): MKT, SMB, HML, RMW, CMA and UMD from Kenneth
# French's library, ROE and IA from Hou, Xue and Zhang (global-q.org) and BAB
# from AQR. Every reader returns DECIMAL returns keyed by an explicit date and
# never repairs an observation: missing codes, duplicate dates or malformed
# rows abort.
#
# Two frequencies are kept on purpose, mirroring the paper:
#   * monthly provider files supply the returns that portfolios EARN;
#   * daily provider files supply realized variance (ML targets/features) and
#     the paper's volatility signals (sample variance of daily returns).
# Compounding the daily files does not reproduce the providers' monthly
# factors exactly, so neither frequency is derived from the other.

  `%>%` <- dplyr::`%>%`

  ## Keys used throughout the DeMiguel dataset, in the paper's column order.
  ## `mkt` is French's Mkt-RF. It is NOT the CRSP vwretd series that the JKP
  ## panel stores as `spx_vw`; the two keys must never be confused.
  demiguel_factor_names <- c("mkt", "smb", "hml", "rmw", "cma", "umd", "roe", "ia", "bab")

  ## Table I, Panel A, first row: annualized Sharpe ratio of each unmanaged
  ## factor, January 1977 to December 2020. Used only as a data check.
  demiguel_table1_sharpe <- tibble::tibble(
    name = demiguel_factor_names,
    sharpe_paper = c(.530, .208, .170, .506, .399, .474, .722, .508, .880)
  )

  ## Name of the date key at each frequency, shared by every reader below
  date_key <- function(frequency) if (frequency == "monthly") "month_id" else "date"

#' Read one return table from a Kenneth French CSV ZIP
#'
#' path: official *_CSV.zip. frequency selects six-digit (YYYYMM) or
#' eight-digit (YYYYMMDD) records, so annual blocks are never read as months.
#' Percent returns are divided by 100 once. Column names are lower-cased with
#' "-" replaced by "_" (Mkt-RF -> mkt_rf, Mom -> mom).
#' Returns month_id (monthly) or date (daily) plus one column per factor.
read_french_zip <- function(path, frequency = c("monthly", "daily")) {

  frequency <- match.arg(frequency)
  if (!file.exists(path)) rlang::abort(paste("Missing French file:", path))

  entries <- utils::unzip(path, list = TRUE)$Name
  if (length(entries) != 1L) rlang::abort(paste("Expected one CSV inside", basename(path)))

  connection <- unz(path, entries)
  on.exit(close(connection))
  parse_french_lines(readLines(connection, warn = FALSE), frequency, basename(path))
}

#' Parse the text lines of one French CSV (the body of read_french_zip())
#'
#' Kept separate so the parsing rules can be tested on synthetic lines.
#' label only names the source in error messages.
parse_french_lines <- function(lines, frequency = c("monthly", "daily"), label = "French CSV") {

  frequency <- match.arg(frequency)

  ## Records start with the date stamp. The header is the line right before
  ## the first record; a file whose records are not one contiguous block is
  ## rejected rather than silently stitched together.
  digits <- if (frequency == "monthly") 6L else 8L
  is_record <- grepl(sprintf("^\\s*[0-9]{%d}\\s*,", digits), lines)
  first <- which(is_record)[1]
  if (is.na(first) || first < 2L) rlang::abort(paste("No", frequency, "records in", label))
  block <- first:(first + sum(is_record) - 1L)
  if (!all(is_record[block])) rlang::abort(paste("Non-contiguous", frequency, "records in", label))

  header <- trimws(strsplit(lines[first - 1L], ",")[[1]])
  if (!identical(header[1], "") || any(!nzchar(header[-1]))) {
    rlang::abort(paste("Unexpected header in", label))
  }
  columns <- gsub("-", "_", tolower(header[-1]))

  fields <- strsplit(lines[block], ",")
  if (any(lengths(fields) != length(header))) {
    rlang::abort(paste("Ragged records in", label))
  }
  values <- purrr::map(seq_along(columns), function(j) {
    as.numeric(purrr::map_chr(fields, j + 1L))
  }) %>% stats::setNames(columns)

  stamp <- trimws(purrr::map_chr(fields, 1L))
  dates <- if (frequency == "monthly") {
    as.Date(paste0(stamp, "01"), format = "%Y%m%d")
  } else as.Date(stamp, format = "%Y%m%d")

  result <- tibble::as_tibble(values) %>%
    ## French codes missing values as -99.99 or -999. None may survive.
    dplyr::mutate(dplyr::across(dplyr::everything(), function(x) {
      if (any(!is.finite(x) | x <= -99.99)) {
        rlang::abort(paste("Missing-value code in", label))
      }
      x / 100
    })) %>%
    dplyr::mutate(!!date_key(frequency) := dates, .before = 1)

  validate_port_data(result, date_key(frequency),
                     columns, monthly = frequency == "monthly",
                     consecutive = frequency == "monthly")
  result
}

#' Read the Hou-Xue-Zhang q5 factors (global-q.org)
#'
#' Monthly files carry year/month; daily files carry an ISO date. Percent is
#' divided by 100 once. Returns month_id or date plus r_f, r_mkt, r_me, r_ia,
#' r_roe and r_eg.
read_hxz_q5 <- function(path, frequency = c("monthly", "daily")) {

  frequency <- match.arg(frequency)
  if (!file.exists(path)) rlang::abort(paste("Missing q5 file:", path))

  raw <- utils::read.csv(path, check.names = FALSE) %>% tibble::as_tibble()
  names(raw) <- tolower(names(raw))
  factors <- c("r_f", "r_mkt", "r_me", "r_ia", "r_roe", "r_eg")
  id <- if (frequency == "monthly") c("year", "month") else "date"
  if (!all(c(id, factors) %in% names(raw))) {
    rlang::abort(paste("Unexpected q5 columns in", basename(path)))
  }

  result <- raw %>%
    dplyr::transmute(
      key = if (frequency == "monthly") {
        as.Date(sprintf("%04d-%02d-01", as.integer(year), as.integer(month)))
      } else as.Date(date),
      dplyr::across(dplyr::all_of(factors), ~ as.numeric(.x) / 100)
    ) %>%
    dplyr::rename(!!date_key(frequency) := key)

  validate_port_data(result, date_key(frequency),
                     factors, monthly = frequency == "monthly",
                     consecutive = frequency == "monthly")
  result
}

#' Read AQR's U.S. betting-against-beta factor
#'
#' path: AQR "Betting Against Beta Equity Factors" Monthly or Daily workbook.
#' Reads the USA column of the "BAB Factors" sheet, whose header row starts
#' with DATE. Returns are already decimal. Leading empty cells (before the
#' series starts) are dropped; any later gap aborts. Monthly month-end dates
#' become first-of-month month_id.
read_aqr_bab <- function(path, frequency = c("monthly", "daily")) {

  frequency <- match.arg(frequency)
  if (!file.exists(path)) rlang::abort(paste("Missing AQR file:", path))

  sheet <- readxl::read_excel(path, sheet = "BAB Factors", col_names = FALSE,
                              col_types = "text", .name_repair = "minimal")
  header_row <- which(sheet[[1]] == "DATE")
  if (length(header_row) != 1L) rlang::abort("Expected one DATE header row in the BAB sheet.")
  usa_col <- which(unlist(sheet[header_row, ]) == "USA")
  if (length(usa_col) != 1L) rlang::abort("Expected one USA column in the BAB sheet.")

  result <- tibble::tibble(
    date = as.Date(sheet[[1]][-seq_len(header_row)], format = "%m/%d/%Y"),
    bab = suppressWarnings(as.numeric(sheet[[usa_col]][-seq_len(header_row)]))
  ) %>%
    ## Footnotes below the table have no date; they are not observations.
    dplyr::filter(!is.na(date)) %>%
    dplyr::arrange(date)

  observed <- which(!is.na(result$bab))
  if (!length(observed)) rlang::abort("The USA BAB series is empty.")
  result <- result[min(observed):max(observed), ]
  if (anyNA(result$bab)) rlang::abort("Interior gap in the USA BAB series.")

  if (frequency == "monthly") {
    result <- result %>%
      dplyr::transmute(month_id = lubridate::floor_date(date, "month"), bab)
  }

  validate_port_data(result, date_key(frequency),
                     "bab", monthly = frequency == "monthly",
                     consecutive = frequency == "monthly")
  result
}

#' Assemble the nine DeMiguel factors at one frequency
#'
#' raw_dir: folder holding the provider files under their official names.
#' mkt_source/smb_source/hml_source: "ff5" (Fama-French 2x3 five-factor file,
#' the paper's Fama-French (2018) reference) or "ff3" (three-factor file).
#' RMW and CMA exist only in the five-factor file. Each factor is read from ONE
#' file over its whole history: nothing is spliced, filled or truncated to a
#' common start here, so an "ff5" factor starts in July 1963.
#'
#' The files are checked against each other before use. RF must agree exactly.
#' Daily Mkt-RF is identical in the two files (vintage 202608), but French's
#' MONTHLY three- and five-factor Mkt-RF differ by up to 8 bp in a minority of
#' months, so monthly market differences below 10 bp are tolerated and the
#' choice is left to mkt_source. A larger gap means mismatched vintages.
#'
#' Defaults follow the evidence, not convenience. The paper's Appendix defines
#' SMB as three small minus three big book-to-market portfolios, which is the
#' THREE-factor SMB, and that series is also the closer match to Table I
#' (Sharpe .221 vs .235 for the 2x3 SMB; paper .208, 202608 vintage). HML is
#' identical in both files and MKT has the same 1977-2020 Sharpe in both, so
#' all three come from the three-factor file, which also carries the longer
#' history used by the unbalanced long sample.
#' Returns list(returns = name/<date key>/ret, rf = <date key>/rf), decimal.
build_demiguel_returns <- function(raw_dir, frequency = c("monthly", "daily"),
                                   mkt_source = "ff3", smb_source = "ff3",
                                   hml_source = "ff3", q5_vintage = "2025") {

  frequency <- match.arg(frequency)
  mkt_source <- match.arg(mkt_source, c("ff5", "ff3"))
  smb_source <- match.arg(smb_source, c("ff5", "ff3"))
  hml_source <- match.arg(hml_source, c("ff5", "ff3"))
  key <- date_key(frequency)
  suffix <- if (frequency == "monthly") "_CSV.zip" else "_daily_CSV.zip"

  ff3 <- read_french_zip(file.path(raw_dir, paste0("F-F_Research_Data_Factors", suffix)), frequency)
  ff5 <- read_french_zip(file.path(raw_dir, paste0("F-F_Research_Data_5_Factors_2x3", suffix)), frequency)
  mom <- read_french_zip(file.path(raw_dir, paste0("F-F_Momentum_Factor", suffix)), frequency)
  q5 <- read_hxz_q5(file.path(raw_dir, sprintf("q5_factors_%s_%s.csv", frequency, q5_vintage)), frequency)
  bab <- read_aqr_bab(file.path(raw_dir, sprintf("Betting-Against-Beta-Equity-Factors-%s.xlsx",
    if (frequency == "monthly") "Monthly" else "Daily")), frequency)

  if (!identical(names(mom)[-1], "mom")) rlang::abort("Unexpected momentum column.")

  ## The two French files must come from one vintage before they are combined.
  ## Thresholds are documented in the roxygen block above.
  overlap <- dplyr::inner_join(ff3, ff5, by = key, suffix = c("_ff3", "_ff5"))
  market_tolerance <- if (frequency == "monthly") .001 else 1e-12
  if (!nrow(overlap) || max(abs(overlap$rf_ff3 - overlap$rf_ff5)) > 1e-12 ||
      max(abs(overlap$mkt_rf_ff3 - overlap$mkt_rf_ff5)) >= market_tolerance) {
    rlang::abort("French three- and five-factor files disagree on RF or Mkt-RF.")
  }

  pick <- function(source, column) {
    data <- if (source == "ff5") ff5 else ff3
    dplyr::transmute(data, !!key := .data[[key]], ret = .data[[column]])
  }

  returns <- list(
    mkt = pick(mkt_source, "mkt_rf"),
    smb = pick(smb_source, "smb"),
    hml = pick(hml_source, "hml"),
    rmw = dplyr::transmute(ff5, !!key := .data[[key]], ret = rmw),
    cma = dplyr::transmute(ff5, !!key := .data[[key]], ret = cma),
    umd = dplyr::transmute(mom, !!key := .data[[key]], ret = mom),
    roe = dplyr::transmute(q5, !!key := .data[[key]], ret = r_roe),
    ia  = dplyr::transmute(q5, !!key := .data[[key]], ret = r_ia),
    bab = dplyr::transmute(bab, !!key := .data[[key]], ret = bab)
  ) %>%
    purrr::imap_dfr(~ dplyr::mutate(.x, name = .y, .before = 1)) %>%
    dplyr::arrange(name, .data[[key]])

  validate_port_data(returns, c("name", key), "ret", monthly = frequency == "monthly",
                     consecutive = frequency == "monthly")
  if (!setequal(unique(returns$name), demiguel_factor_names)) {
    rlang::abort("The DeMiguel dataset must contain exactly the nine paper factors.")
  }
  ## Unit guard: a percent value that escaped the /100 would be ~100x too big.
  ## No daily long-short or market excess return reaches 50%, nor a monthly 100%.
  if (max(abs(returns$ret)) >= if (frequency == "monthly") 1 else .5) {
    rlang::abort("Implausible factor return magnitude; check percent/decimal units.")
  }

  list(returns = returns,
       rf = dplyr::transmute(ff3, !!key := .data[[key]], rf))
}

#' Compare unmanaged factor Sharpe ratios with Table I, Panel A
#'
#' monthly: name/month_id/ret monthly decimal excess returns. The annualized
#' Sharpe ratio is sqrt(12) * mean / sd over [start, end] inclusive, as in the
#' paper. tolerance is the absolute Sharpe difference accepted as a match;
#' provider data are revised between vintages, so exact equality is not
#' expected. Returns one row per factor; missing months in the window abort.
check_demiguel_sharpe <- function(monthly, start = as.Date("1977-01-01"),
                                  end = as.Date("2020-12-01"), tolerance = .03) {

  validate_port_data(monthly, c("name", "month_id"), "ret", consecutive = FALSE)
  months <- seq(start, end, by = "month")

  check <- monthly %>%
    dplyr::filter(month_id >= start, month_id <= end) %>%
    dplyr::group_by(name) %>%
    dplyr::summarise(
      n = dplyr::n(),
      complete = setequal(month_id, months),
      sharpe = sqrt(12) * mean(ret) / stats::sd(ret),
      .groups = "drop"
    ) %>%
    dplyr::inner_join(demiguel_table1_sharpe, by = "name", relationship = "one-to-one") %>%
    dplyr::mutate(
      difference = sharpe - sharpe_paper,
      within_tolerance = abs(difference) <= tolerance
    ) %>%
    dplyr::arrange(match(name, demiguel_factor_names))

  if (nrow(check) != length(demiguel_factor_names) || any(!check$complete)) {
    rlang::abort("Sharpe check needs all nine factors over every month of the window.")
  }
  check
}
