# Data outputs (paper section 3)------------------------------------------------

  ## Builds every table and figure the Methods and Data section refers to, from
  ## the covariate panels and targets produced by load_data.R. It never fits a
  ## model and never alters an artifact under data/.
  ##
  ## Outputs
  ##   tables/summary_covariates.tex    main paper
  ##   tables/summary_targets.tex       main paper
  ##   tables/variable_definitions.tex  Internet Appendix
  ##   figures/data_covariates_grid.png + .tex   main paper
  ##   figures/data_targets.png         + .tex   main paper
  ##   figures/data_correlogram.png     + .tex   Internet Appendix
  ##   figures/data_coverage.png        + .tex   Internet Appendix
  ##
  ## As in the literature outputs, nothing written here may name a file, a
  ## column identifier or a pipeline stage: the reader has none of that context.

  `%>%` <- dplyr::`%>%`
  source(file.path(here::here(), "R", "helpers.R"))

  project_dir <- here::here()
  tables_dir  <- file.path(project_dir, "tables")
  figures_dir <- file.path(project_dir, "figures")
  dir.create(tables_dir,  recursive = TRUE, showWarnings = FALSE)
  dir.create(figures_dir, recursive = TRUE, showWarnings = FALSE)

  if (.Platform$OS.type == "windows") {
    grDevices::windowsFonts(Helvetica = grDevices::windowsFont("Arial"))
  }

  esc_tex <- function(x) {
    x <- as.character(x)
    x <- gsub("&", "\\&", x, fixed = TRUE)
    x <- gsub("%", "\\%", x, fixed = TRUE)
    x <- gsub("_", "\\_", x, fixed = TRUE)
    x <- gsub("#", "\\#", x, fixed = TRUE)
    x
  }

# Inputs------------------------------------------------------------------------

  paths <- c(
    p1927 = file.path(project_dir, "data", "covariates_panel_1927_v3.rds"),
    p1986 = file.path(project_dir, "data", "covariates_panel_1986_v3.rds"),
    tpan  = file.path(project_dir, "data", "target_panel_v3.rds"),
    tspx  = file.path(project_dir, "data", "target_spx_v3.rds")
  )
  missing <- paths[!file.exists(paths)]
  if (length(missing) > 0L) {
    stop("Missing covariate or target artifact(s):\n", paste(missing, collapse = "\n"))
  }

  panel_1927 <- readRDS(paths[["p1927"]])
  panel_1986 <- readRDS(paths[["p1986"]])
  target_pan <- readRDS(paths[["tpan"]])
  target_spx <- readRDS(paths[["tspx"]])

  market_key <- "spx_vw"
  spx_1927 <- panel_1927 %>% dplyr::filter(.data$name == market_key) %>%
    dplyr::select(-"name") %>% dplyr::arrange(.data$month_id)
  spx_1986 <- panel_1986 %>% dplyr::filter(.data$name == market_key) %>%
    dplyr::select(-"name") %>% dplyr::arrange(.data$month_id)

# Variable dictionary-----------------------------------------------------------

  ## Reader-facing name, definition and source for every constructed series.
  ## `var` is the internal column and never reaches the page.
  dict <- tibble::tribble(
    ~var, ~label, ~definition, ~source,
    ## The three horizons are the heterogeneous autoregressive aggregation of
    ## Corsi (2009) applied to each family, all expressed in monthly units: the
    ## daily term is the last trading day scaled by 22, the weekly term is the
    ## last five trading days scaled by 22/5, and the monthly term is the sum
    ## over the month. Every term is lagged one month.
    "rv_td_lag1", "Realized variance, daily term", "Realized variance of the last trading day of the previous month, scaled to monthly units", "CRSP; JKP",
    "rv_tw_lag1", "Realized variance, weekly term", "Realized variance summed over the last five trading days of the previous month, scaled to monthly units", "CRSP; JKP",
    "rv_month_lag1", "Realized variance, monthly term", "Realized variance summed over the previous month", "CRSP; JKP",
    "rq_td_lag1", "Realized quarticity, daily term", "Realized quarticity of the last trading day of the previous month, scaled to monthly units", "CRSP; JKP",
    "rq_tw_lag1", "Realized quarticity, weekly term", "Realized quarticity summed over the last five trading days, scaled to monthly units", "CRSP; JKP",
    "rq_month_lag1", "Realized quarticity, monthly term", "Realized quarticity summed over the previous month", "CRSP; JKP",
    "jump_td_lag1", "Jump variation, daily term", "Jump component of the last trading day, scaled to monthly units", "CRSP; JKP",
    "jump_tw_lag1", "Jump variation, weekly term", "Jump component summed over the last five trading days, scaled to monthly units", "CRSP; JKP",
    "jump_month_lag1", "Jump variation, monthly term", "Jump component summed over the previous month", "CRSP; JKP",
    "cont_td_lag1", "Continuous variation, daily term", "Continuous component of the last trading day, scaled to monthly units", "CRSP; JKP",
    "cont_tw_lag1", "Continuous variation, weekly term", "Continuous component summed over the last five trading days, scaled to monthly units", "CRSP; JKP",
    "cont_month_lag1", "Continuous variation, monthly term", "Continuous component summed over the previous month", "CRSP; JKP",
    "negret_td_lag1", "Negative return, daily term", "Minimum of the last trading day return and zero, scaled to monthly units", "CRSP; JKP",
    "negret_tw_lag1", "Negative return, weekly term", "Minimum of the five-day return and zero, scaled to monthly units", "CRSP; JKP",
    "negret_month_lag1", "Negative return, monthly term", "Minimum of the previous month return and zero", "CRSP; JKP",
    "log_epu_diff_lag1", "Policy uncertainty growth", "Log change in the news-based economic policy uncertainty index", "Baker, Bloom and Davis (2016)",
    "ppi_vol", "Producer price volatility", "Schwert (1989) out-of-sample volatility of producer price growth", "FRED PPIACO",
    "ip_vol", "Industrial production volatility", "Schwert (1989) out-of-sample volatility of industrial production growth", "FRED IPB50001N",
    "mbas_vol", "Monetary base volatility", "Schwert (1989) out-of-sample volatility of monetary base growth", "FRED BOGMBASE",
    "dy_lag1", "Dividend yield", "Trailing dividend yield of the S\\&P 500", "Multpl",
    "ey_lag1", "Earnings yield", "Trailing earnings yield of the S\\&P 500", "Multpl",
    "dfr_lag1", "Default return spread", "Aaa corporate yield less the ten-year Treasury yield", "FRED AAA10YM",
    "dfy_lag1", "Default yield spread", "Baa corporate yield less the ten-year Treasury yield", "FRED BAA10YM",
    "tms_lag1", "Term spread", "Ten-year Treasury yield less the federal funds rate", "FRED T10YFFM",
    "tbill_lag1", "Treasury bill rate", "Three-month Treasury bill secondary market rate", "FRED TB3MS",
    "log_cons_sent_diff_lag1", "Consumer sentiment growth", "Log change in the index of consumer sentiment", "University of Michigan",
    "gpr_lag1", "Geopolitical risk", "News-based geopolitical risk index", "Caldara and Iacoviello (2022)",
    "oil_rv_td_lag1", "Crude oil variance, daily", "Squared daily return of West Texas Intermediate, last trading day", "FRED DCOILWTICO",
    "oil_rv_tw_lag1", "Crude oil variance, weekly", "Five-day mean of daily crude oil realized variance", "FRED DCOILWTICO",
    "oil_rv_month_lag1", "Crude oil variance, monthly", "Monthly mean of daily crude oil realized variance", "FRED DCOILWTICO",
    "vix_lag1", "Implied volatility", "Closing level of the volatility index, extended before 1990 with its predecessor", "CBOE",
    "month", "Calendar month", "Month of the year, entering as twelve indicators", "Calendar",
    "sqrt_days", "Trading days", "Square root of the number of trading days in the month; the level falls once Saturday trading ends in 1952", "CRSP"
  )

  key_cols  <- c("name", "month_id")
  vars_1927 <- setdiff(names(panel_1927), key_cols)
  vars_1986 <- setdiff(names(panel_1986), key_cols)
  all_vars  <- union(vars_1927, vars_1986)

  unknown <- setdiff(all_vars, dict$var)
  if (length(unknown) > 0L) {
    stop("Constructed series absent from the variable dictionary: ",
         paste(unknown, collapse = ", "))
  }
  dict <- dict[dict$var %in% all_vars, ]
  dict$from <- ifelse(dict$var %in% vars_1927, "1927", "1986")

# Table: summary statistics for the covariates----------------------------------

  ## Statistics are computed on the market series over the longest window each
  ## variable supports, which is what the models see before preprocessing.
  stat_row <- function(v) {
    src <- if (v %in% vars_1927) spx_1927 else spx_1986
    x <- src[[v]]
    if (!is.numeric(x)) x <- as.numeric(as.character(x))
    ok <- is.finite(x)
    tibble::tibble(
      var = v, n = sum(ok),
      start = format(min(src$month_id[ok]), "%Y"),
      mean = mean(x[ok]), sd = stats::sd(x[ok]),
      p10 = stats::quantile(x[ok], 0.10, names = FALSE),
      med = stats::median(x[ok]),
      p90 = stats::quantile(x[ok], 0.90, names = FALSE)
    )
  }
  stats_tbl <- dplyr::bind_rows(lapply(dict$var, stat_row)) %>%
    dplyr::left_join(dict[c("var", "label")], by = "var")

  ## Variance quantities are tiny in decimal units, so they are reported in
  ## basis points. Quarticity is a fourth moment and sits eight orders of
  ## magnitude below that, so basis points would still print as zeros and it
  ## carries its own factor. The implied volatility index is stored on the same
  ## monthly variance scale as realized variance and is grouped with it.
  num_cols <- c("mean", "sd", "p10", "med", "p90")

  bp <- grepl("^(rv|jump|cont|oil_rv)_", stats_tbl$var) | stats_tbl$var == "vix_lag1"
  stats_tbl[bp, num_cols] <- stats_tbl[bp, num_cols] * 1e4
  stats_tbl$label[bp] <- paste0(stats_tbl$label[bp], " (bp)")

  rq <- grepl("^rq_", stats_tbl$var)
  stats_tbl[rq, num_cols] <- stats_tbl[rq, num_cols] * 1e8
  stats_tbl$label[rq] <- paste0(stats_tbl$label[rq], " ($\\times 10^{-8}$)")

  ## The calendar month enters as twelve indicators, so its mean and dispersion
  ## describe nothing. It stays in the definitions table and leaves this one.
  stats_tbl <- stats_tbl[stats_tbl$var != "month", ]

  fmt <- function(x) formatC(x, format = "f", digits = 2, big.mark = ",")

  tbl_cov <- c(
    "{\\footnotesize",
    "\\captionsetup{justification=raggedright,labelsep=none,singlelinecheck=false,labelformat=boldlabel}",
    "\\setlength{\\tabcolsep}{3pt}",
    "\\renewcommand{\\arraystretch}{1.05}",
    ## Narrow ragged columns cannot always break a long source identifier;
    ## a little emergency stretch clears the residual overfull rows.
    "\\setlength{\\emergencystretch}{3em}",
    paste0("\\begin{longtable}{",
           ## Widths are set so the row fits the text block; widening any
           ## column overfulls every row of the table.
           ">{\\raggedright\\arraybackslash}m{4.7cm}",
           ">{\\raggedleft\\arraybackslash}m{0.8cm}",
           ">{\\raggedleft\\arraybackslash}m{0.8cm}",
           "*{5}{>{\\raggedleft\\arraybackslash}m{1.10cm}}}"),
    paste0("\\caption[Summary statistics for the predictors]{\\footnotesize ",
           "\\textbf{: Summary statistics for the predictors.} Monthly ",
           "observations of the market series, each variable measured over the ",
           "longest window it supports; `From' is the first year available. ",
           "Variance and quarticity measures are reported in basis points. ",
           "Definitions and sources are in ",
           "Table~\\ref{tab:vardefs}.} \\label{tab:sumcov} \\\\"),
    "\\hline",
    "Predictor & Obs. & From & Mean & S.D. & P10 & Median & P90 \\\\",
    "\\hline",
    "\\endfirsthead",
    "\\hline",
    "Predictor & Obs. & From & Mean & S.D. & P10 & Median & P90 \\\\",
    "\\hline",
    "\\endhead",
    "\\hline",
    "\\endfoot",
    sprintf("%s & %d & %s & %s & %s & %s & %s & %s \\\\",
            esc_tex(stats_tbl$label), stats_tbl$n, stats_tbl$start,
            fmt(stats_tbl$mean), fmt(stats_tbl$sd), fmt(stats_tbl$p10),
            fmt(stats_tbl$med), fmt(stats_tbl$p90)),
    "\\end{longtable}",
    "}"
  )
  writeLines(tbl_cov, file.path(tables_dir, "summary_covariates.tex"))

# Table: the forecasting targets------------------------------------------------

  ## The market series and the factor cross-section, in basis points per month.
  fac_target <- target_pan %>% dplyr::filter(.data$name != market_key)
  mkt_target <- target_pan %>% dplyr::filter(.data$name == market_key)

  per_key <- fac_target %>%
    dplyr::group_by(.data$name) %>%
    dplyr::summarize(months = dplyr::n(),
                     mean_rv = mean(.data$rv_month, na.rm = TRUE),
                     .groups = "drop")

  q <- function(x, p) stats::quantile(x, p, names = FALSE, na.rm = TRUE)

  target_rows <- tibble::tribble(
    ~grp, ~n_series, ~months, ~start, ~end, ~mean, ~sd, ~p10, ~med, ~p90,
    "Market", 1L, nrow(mkt_target),
      format(min(mkt_target$month_id), "%Y"), format(max(mkt_target$month_id), "%Y"),
      mean(mkt_target$rv_month) * 1e4, stats::sd(mkt_target$rv_month) * 1e4,
      q(mkt_target$rv_month, .10) * 1e4, stats::median(mkt_target$rv_month) * 1e4,
      q(mkt_target$rv_month, .90) * 1e4,
    "Factors, pooled", dplyr::n_distinct(fac_target$name), nrow(fac_target),
      format(min(fac_target$month_id), "%Y"), format(max(fac_target$month_id), "%Y"),
      mean(fac_target$rv_month) * 1e4, stats::sd(fac_target$rv_month) * 1e4,
      q(fac_target$rv_month, .10) * 1e4, stats::median(fac_target$rv_month) * 1e4,
      q(fac_target$rv_month, .90) * 1e4
  )

  tbl_tgt <- c(
    "\\begin{table}[pt]", "\\centering",
    "\\captionsetup{justification=raggedright,labelsep=none,singlelinecheck=false,labelformat=boldlabel}",
    "\\renewcommand{\\arraystretch}{1.15}",
    sprintf(paste0(
      "\\caption[The forecasting targets]{\\footnotesize \\textbf{: The ",
      "forecasting targets.} Monthly realized variance, defined as the sum of ",
      "squared daily returns within the month, in basis points. The market row ",
      "is the value-weighted market portfolio. The factor row pools the %d ",
      "long-short factors, whose histories begin at different dates, so the ",
      "panel is unbalanced and the pooled statistics weight each month by the ",
      "number of factors available. Per-factor detail is in the Internet ",
      "Appendix.}"), dplyr::n_distinct(fac_target$name)),
    "\\footnotesize",
    "\\setlength{\\tabcolsep}{4pt}",
    paste0("\\begin{tabularx}{\\linewidth}{",
           ">{\\raggedright\\arraybackslash}X",
           "*{2}{>{\\raggedleft\\arraybackslash}m{1.5cm}}",
           "*{2}{>{\\raggedleft\\arraybackslash}m{0.9cm}}",
           "*{5}{>{\\raggedleft\\arraybackslash}m{1.1cm}}}"),
    "\\hline",
    "Target & Series & Obs. & From & To & Mean & S.D. & P10 & Median & P90 \\\\",
    "\\hline",
    sprintf("%s & %d & %s & %s & %s & %s & %s & %s & %s & %s \\\\",
            target_rows$grp, target_rows$n_series,
            formatC(target_rows$months, format = "d", big.mark = ","),
            target_rows$start, target_rows$end,
            fmt(target_rows$mean), fmt(target_rows$sd), fmt(target_rows$p10),
            fmt(target_rows$med), fmt(target_rows$p90)),
    "\\hline",
    "\\end{tabularx}",
    "\\label{tab:sumtgt}",
    "\\end{table}"
  )
  writeLines(tbl_tgt, file.path(tables_dir, "summary_targets.tex"))

# Table: variable definitions and sources (Internet Appendix)-------------------

  tbl_def <- c(
    "{\\footnotesize",
    "\\captionsetup{justification=raggedright,labelsep=none,singlelinecheck=false,labelformat=boldlabel}",
    "\\setlength{\\tabcolsep}{3pt}",
    "\\renewcommand{\\arraystretch}{1.1}",
    ## Narrow ragged columns cannot always break a long source identifier;
    ## a little emergency stretch clears the residual overfull rows.
    "\\setlength{\\emergencystretch}{3em}",
    paste0("\\begin{longtable}{",
           ## As above, these widths are what fits the text block.
           ">{\\raggedright\\arraybackslash}m{3.3cm}",
           ">{\\raggedright\\arraybackslash}m{6.2cm}",
           ">{\\raggedright\\arraybackslash}m{2.9cm}}"),
    paste0("\\caption[Variable definitions and sources]{\\footnotesize ",
           "\\textbf{: Variable definitions and sources.} Every predictor ",
           "constructed for the models, with its definition and the series it ",
           "is built from. Predictors marked as available from 1927 enter the ",
           "long sample; the remainder are available only from 1986. Series ",
           "identifiers refer to the Federal Reserve Economic Data ",
           "repository.} \\label{tab:vardefs} \\\\"),
    "\\hline",
    "Predictor & Definition & Source \\\\",
    "\\hline",
    "\\endfirsthead",
    "\\hline",
    "Predictor & Definition & Source \\\\",
    "\\hline",
    "\\endhead",
    "\\hline",
    "\\endfoot",
    sprintf("%s & %s & %s \\\\",
            esc_tex(dict$label), dict$definition, dict$source),
    "\\end{longtable}",
    "}"
  )
  writeLines(tbl_def, file.path(tables_dir, "variable_definitions.tex"))

# Table: hyperparameter search spaces (Internet Appendix)-----------------------

  ## The values below are transcribed from the run specification. They are not
  ## read from it, because sourcing that file would launch the fitting run, so
  ## the guard checks that each literal is still present in the source. Editing
  ## a search space without editing this table therefore fails the build rather
  ## than publishing a stale one.
  spec_src <- paste(readLines(file.path(project_dir, "fit_models.R"),
                              warn = FALSE), collapse = " ")
  spec_src <- gsub("[[:space:]]+", " ", spec_src)
  expected <- c(
    "alpha = c(0, 0.25, 0.5, 0.75, 1)",
    "lambda.min.ratio = 1e-4",
    "bayes = list(alpha = c(0, 1))",
    "mtry = c(0.15, 0.35, 0.50), max.depth = c(2L, 5L, 8L)",
    "num.trees = 1000L, min.bucket = 5L",
    "mtry = c(0.15, 0.50), max.depth = c(2L, 8L)",
    "eta = c(0.01, 0.05, 0.10), max_depth = c(1L, 2L, 3L)",
    "min_child_weight = c(5L, 20L)",
    "nrounds = 1000L, subsample = 0.8, colsample_bytree = 0.8",
    "eta = c(-2, -1), max_depth = c(1L, 3L)",
    "lr = c(1e-4, 1e-3, 1e-2), droprate = c(0, 0.1, 0.25)",
    "regularizer_l1 = c(1e-5, 1e-4, 1e-3)",
    "lr = c(-4, -2), droprate = c(0, 0.25)",
    "regularizer_l1 = c(-5, -3)",
    "regularizer_l2 = 0, size_of_batch = 64L",
    "number_of_epochs = 100L"
  )
  absent <- expected[!vapply(expected, function(e)
    grepl(gsub("[[:space:]]+", " ", e), spec_src, fixed = TRUE), logical(1))]
  if (length(absent) > 0L) {
    stop("The run specification no longer matches the hyperparameter table. ",
         "Missing: ", paste(absent, collapse = " | "))
  }

  hp <- tibble::tribble(
    ~name, ~symbol, ~enet, ~rf, ~xgb, ~nn,
    "L1 share of the penalty", "$\\lambda^{\\rho}$", "0, 0.25, 0.5, 0.75, 1", "--", "--", "--",
    "Penalty path floor", "$\\lambda^{\\psi}$", "Fixed: $10^{-4}$", "--", "--", "--",
    "Trees / boosting rounds", "$\\lambda^{T}$ / $\\lambda^{F}$", "--", "Fixed: 1{,}000", "Fixed: 1{,}000", "--",
    "Maximum depth", "$\\lambda^{D}$", "--", "2, 5, 8", "1, 2, 3", "--",
    "Minimum terminal node size", "$\\lambda^{L}$", "--", "Fixed: 5", "--", "--",
    "Minimum child weight", "$\\lambda^{C}$", "--", "--", "5, 20", "--",
    "Feature subsampling fraction", "$\\lambda^{R}$", "--", "0.15, 0.35, 0.50", "Fixed: 0.8", "--",
    "Row subsampling fraction", "$\\lambda^{S}$", "--", "--", "Fixed: 0.8", "--",
    "Learning rate", "$\\lambda^{\\eta}$", "--", "--", "0.01, 0.05, 0.10", "$10^{-4}$, $10^{-3}$, $10^{-2}$",
    "L1 penalty", "$\\lambda^{\\alpha}$ / $\\lambda^{\\omega}$", "--", "--", "Fixed: 0", "$10^{-5}$, $10^{-4}$, $10^{-3}$",
    "L2 penalty", "$\\lambda^{\\delta}$", "--", "--", "--", "Fixed: 0",
    "Minimum split loss", "$\\lambda^{\\gamma}$", "--", "--", "Fixed: 0", "--",
    "Dropout rate", "$\\lambda^{p}$", "--", "--", "--", "0, 0.1, 0.25",
    "Batch size", "$\\lambda^{b}$", "--", "--", "--", "Fixed: 64",
    "Epochs", "$\\lambda^{e}$", "--", "--", "--", "Fixed: 100"
  )

  hpb <- tibble::tribble(
    ~name, ~symbol, ~enet, ~rf, ~xgb, ~nn,
    "L1 share of the penalty", "$\\lambda^{\\rho}$", "[0, 1]", "--", "--", "--",
    "Penalty path floor", "$\\lambda^{\\psi}$", "Fixed: $10^{-4}$", "--", "--", "--",
    "Trees / boosting rounds", "$\\lambda^{T}$ / $\\lambda^{F}$", "--", "Fixed: 1{,}000", "Fixed: 1{,}000", "--",
    "Maximum depth", "$\\lambda^{D}$", "--", "[2, 8]", "[1, 3]", "--",
    "Minimum terminal node size", "$\\lambda^{L}$", "--", "Fixed: 5", "--", "--",
    "Minimum child weight", "$\\lambda^{C}$", "--", "--", "[5, 20]", "--",
    "Feature subsampling fraction", "$\\lambda^{R}$", "--", "[0.15, 0.50]", "Fixed: 0.8", "--",
    "Row subsampling fraction", "$\\lambda^{S}$", "--", "--", "Fixed: 0.8", "--",
    "Learning rate", "$\\lambda^{\\eta}$", "--", "--", "[$10^{-2}$, $10^{-1}$]", "[$10^{-4}$, $10^{-2}$]",
    "L1 penalty", "$\\lambda^{\\alpha}$ / $\\lambda^{\\omega}$", "--", "--", "Fixed: 0", "[$10^{-5}$, $10^{-3}$]",
    "L2 penalty", "$\\lambda^{\\delta}$", "--", "--", "--", "Fixed: 0",
    "Minimum split loss", "$\\lambda^{\\gamma}$", "--", "--", "Fixed: 0", "--",
    "Dropout rate", "$\\lambda^{p}$", "--", "--", "--", "[0, 0.25]",
    "Batch size", "$\\lambda^{b}$", "--", "--", "--", "Fixed: 64",
    "Epochs", "$\\lambda^{e}$", "--", "--", "--", "Fixed: 100"
  )

  hp_rows <- function(d) sprintf("%s & %s & %s & %s & %s & %s \\\\",
                                 d$name, d$symbol, d$enet, d$rf, d$xgb, d$nn)

  tbl_hp <- c(
    "\\begin{table}[p]",
    "\\captionsetup{justification=raggedright,labelsep=none,singlelinecheck=false,labelformat=boldlabel}",
    "\\renewcommand{\\arraystretch}{0.85}",
    paste0("\\caption[Hyperparameter search spaces]{\\footnotesize \\textbf{: ",
           "Hyperparameter search spaces.} Reports the grid and the Bayesian ",
           "search domains for each tuned model, with the per-refit evaluation ",
           "budgets. Entries marked fixed are set once and excluded from the ",
           "search. The heterogeneous autoregressive specifications are untuned ",
           "and omitted.}"),
    "\\scriptsize",
    "\\setlength{\\tabcolsep}{3pt}",
    paste0("\\begin{tabularx}{\\linewidth}{",
           ">{\\raggedright\\arraybackslash}m{2.9cm}",
           ">{\\centering\\arraybackslash}m{1.35cm}",
           "*{4}{>{\\raggedleft\\arraybackslash}X}}"),
    "\\hline",
    "Hyperparameter & Symbol & ENET & RF & XGB & NN1--NN3 \\\\",
    "\\hline",
    "\\multicolumn{6}{@{}l}{\\textit{Panel A. Grid search}} \\\\",
    hp_rows(hp),
    "\\hline",
    "Grid combinations per refit & & 5 & 9 & 18 & 27 \\\\",
    "\\hline",
    "\\multicolumn{6}{@{}l}{\\textit{Panel B. Bayesian optimization}} \\\\",
    hp_rows(hpb),
    "\\hline",
    "Initial design points & & 6 & 8 & 10 & 10 \\\\",
    "Acquisition iterations & & 6 & 10 & 12 & 10 \\\\",
    "Total evaluations per refit & & 12 & 18 & 22 & 20 \\\\",
    "\\hline",
    paste0("\\multicolumn{6}{@{}p{0.98\\linewidth}@{}}{\\textit{Both arms ",
           "minimize validation root mean squared error and re-optimize at each ",
           "annual refit; grid search is the primary specification. Learning ",
           "rates and the L1 penalty are searched on a logarithmic scale and ",
           "reported here in engine units. Boosting rounds are a ceiling set by ",
           "early stopping, not a tuned quantity.}} \\\\"),
    "\\hline",
    "\\end{tabularx}",
    "\\label{tab:hyper}",
    "\\end{table}"
  )
  writeLines(tbl_hp, file.path(tables_dir, "hyperparameter_bounds.tex"))

# Figure: predictor time series-------------------------------------------------

  ## One panel per predictor, each over its own longest available window.
  long_from <- function(src, vars) {
    src %>%
      dplyr::select(dplyr::all_of(c("month_id", vars))) %>%
      dplyr::mutate(dplyr::across(dplyr::all_of(vars), as.numeric)) %>%
      tidyr::pivot_longer(-"month_id", names_to = "var", values_to = "value")
  }
  ## The calendar month is a repeating sawtooth and carries no information as a
  ## time series; dropping it also leaves 32 panels, which fill four columns.
  only_1986 <- setdiff(vars_1986, vars_1927)
  ts_long <- dplyr::bind_rows(long_from(spx_1927, setdiff(vars_1927, "month")),
                              long_from(spx_1986, setdiff(only_1986, "month"))) %>%
    dplyr::filter(is.finite(.data$value)) %>%
    dplyr::left_join(dict[c("var", "label")], by = "var")
  ts_long$label <- factor(ts_long$label, levels = setdiff(dict$label, "Calendar month"))

  p_grid <- ggplot2::ggplot(ts_long,
                            ggplot2::aes(x = .data$month_id, y = .data$value)) +
    ggplot2::geom_line(linewidth = 0.25, colour = "#1f4e79") +
    ggplot2::facet_wrap(~ label, ncol = 4, scales = "free_y") +
    ggplot2::labs(x = NULL, y = NULL) +
    ggplot2::theme_minimal(base_size = 7, base_family = "Helvetica") +
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank(),
                   strip.text = ggplot2::element_text(size = 6.2, face = "bold"))

  ggplot2::ggsave(file.path(figures_dir, "data_covariates_grid.png"), p_grid,
                  width = 9.2, height = 11.6, dpi = 300)

# Figure: the targets-----------------------------------------------------------

  fac_band <- fac_target %>%
    dplyr::group_by(.data$month_id) %>%
    dplyr::summarize(lo = q(.data$rv_month, .10), md = stats::median(.data$rv_month),
                     hi = q(.data$rv_month, .90), .groups = "drop")

  p_tgt <- ggplot2::ggplot() +
    ggplot2::geom_ribbon(data = fac_band,
                         ggplot2::aes(x = .data$month_id, ymin = .data$lo * 1e4,
                                      ymax = .data$hi * 1e4, fill = "Factors, 10th to 90th percentile"),
                         alpha = 0.45) +
    ggplot2::geom_line(data = fac_band,
                       ggplot2::aes(x = .data$month_id, y = .data$md * 1e4,
                                    colour = "Factors, median"), linewidth = 0.3) +
    ggplot2::geom_line(data = mkt_target,
                       ggplot2::aes(x = .data$month_id, y = .data$rv_month * 1e4,
                                    colour = "Market"), linewidth = 0.3) +
    ggplot2::scale_y_log10() +
    ggplot2::scale_fill_manual(values = c("Factors, 10th to 90th percentile" = "#bdc3c7"), name = NULL) +
    ggplot2::scale_colour_manual(values = c("Factors, median" = "#7f8c8d",
                                            "Market" = "#1f4e79"), name = NULL) +
    ggplot2::labs(x = NULL, y = "Monthly realized variance (bp, log scale)") +
    ggplot2::theme_minimal(base_size = 10, base_family = "Helvetica") +
    ggplot2::theme(legend.position = "top",
                   panel.grid.minor = ggplot2::element_blank())

  ggplot2::ggsave(file.path(figures_dir, "data_targets.png"), p_tgt,
                  width = 9, height = 4.6, dpi = 300)

# Figure: correlogram (Internet Appendix)---------------------------------------

  num_1986 <- spx_1986 %>%
    dplyr::select(dplyr::all_of(vars_1986)) %>%
    dplyr::mutate(dplyr::across(dplyr::everything(), as.numeric))
  num_1986 <- num_1986[, vapply(num_1986, function(z) stats::sd(z, na.rm = TRUE) > 0, logical(1))]
  cm <- stats::cor(num_1986, use = "pairwise.complete.obs")
  ## Order by hierarchical clustering so blocks of related predictors are visible.
  ord <- stats::hclust(stats::as.dist(1 - cm))$order
  cm  <- cm[ord, ord]
  lab <- dict$label[match(rownames(cm), dict$var)]

  cm_long <- data.frame(
    x = factor(rep(lab, times = ncol(cm)), levels = lab),
    y = factor(rep(lab, each  = nrow(cm)), levels = rev(lab)),
    r = as.vector(cm))

  p_cor <- ggplot2::ggplot(cm_long, ggplot2::aes(.data$x, .data$y, fill = .data$r)) +
    ggplot2::geom_tile() +
    ggplot2::scale_fill_gradient2(low = "#b2182b", mid = "white", high = "#1f4e79",
                                  midpoint = 0, limits = c(-1, 1), name = NULL) +
    ggplot2::labs(x = NULL, y = NULL) +
    ggplot2::theme_minimal(base_size = 7, base_family = "Helvetica") +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 60, hjust = 1),
                   panel.grid = ggplot2::element_blank(),
                   legend.key.height = ggplot2::unit(1.1, "cm"))

  ggplot2::ggsave(file.path(figures_dir, "data_correlogram.png"), p_cor,
                  width = 8.2, height = 7.2, dpi = 300)

# Figure: panel coverage (Internet Appendix)------------------------------------

  cov_long <- dplyr::bind_rows(
    panel_1927 %>% dplyr::count(.data$month_id, name = "n") %>%
      dplyr::mutate(sample = "Long sample, from 1927"),
    panel_1986 %>% dplyr::count(.data$month_id, name = "n") %>%
      dplyr::mutate(sample = "Short sample, from 1986"))

  p_cov <- ggplot2::ggplot(cov_long,
                           ggplot2::aes(.data$month_id, .data$n, colour = .data$sample)) +
    ggplot2::geom_step(linewidth = 0.4) +
    ggplot2::scale_colour_manual(values = c("Long sample, from 1927" = "#1f4e79",
                                            "Short sample, from 1986" = "#c0392b"),
                                 name = NULL) +
    ggplot2::labs(x = NULL, y = "Series available") +
    ggplot2::theme_minimal(base_size = 10, base_family = "Helvetica") +
    ggplot2::theme(legend.position = "top",
                   panel.grid.minor = ggplot2::element_blank())

  ggplot2::ggsave(file.path(figures_dir, "data_coverage.png"), p_cov,
                  width = 9, height = 3.8, dpi = 300)

# Figure wrappers---------------------------------------------------------------

  fig <- function(file, out, label, caption, width = "\\linewidth") {
    writeLines(c(
      "\\begin{figure}[htbp]", "\\centering",
      "\\captionsetup{justification=raggedright,labelsep=none,singlelinecheck=false,labelformat=boldlabel}",
      caption,
      sprintf("\\includegraphics[width=%s]{figures/%s}", width, file),
      sprintf("\\label{%s}", label),
      "\\end{figure}"), file.path(figures_dir, out))
  }

  fig("data_covariates_grid.png", "data_covariates_figure.tex", "fig:covgrid",
      paste0("\\caption[Predictors over time]{\\footnotesize \\textbf{: ",
             "Predictors over time.} Each panel plots one predictor for the ",
             "market series over the longest window it supports, so panels ",
             "start at different dates. Vertical scales are free. Variance and ",
             "quarticity measures are in decimal units. The calendar month is ",
             "omitted: it enters the models as twelve indicators and has no ",
             "time-series interpretation.}"),
      "\\linewidth")

  fig("data_targets.png", "data_targets_figure.tex", "fig:targets",
      paste0("\\caption[Monthly realized variance of the market and of the ",
             "factor cross-section]{\\footnotesize \\textbf{: Monthly realized ",
             "variance of the market and of the factor cross-section.} The dark ",
             "line is the value-weighted market portfolio. The band is the ",
             "10th to 90th percentile of realized variance across the factors ",
             "available in each month and the grey line is their median. The ",
             "vertical scale is logarithmic.}"))

  fig("data_correlogram.png", "data_correlogram_figure.tex", "fig:correlogram",
      paste0("\\caption[Correlations among the predictors]{\\footnotesize ",
             "\\textbf{: Correlations among the predictors.} Pairwise ",
             "correlations for the market series over the common window from ",
             "1986, before preprocessing. Rows and columns are ordered by ",
             "hierarchical clustering on correlation distance, so blocks of ",
             "related predictors appear together.}"), "0.92\\linewidth")

  fig("data_coverage.png", "data_coverage_figure.tex", "fig:coverage",
      paste0("\\caption[Availability of the factor panel]{\\footnotesize ",
             "\\textbf{: Availability of the factor panel.} Number of series, ",
             "the market included, carrying a complete set of predictors in ",
             "each month. A series enters as soon as it can support the full ",
             "lag structure and remains thereafter, so the panel is unbalanced ",
             "and widens over time.}"))

# Mirror into the LaTeX tree----------------------------------------------------

  latex_fig_dir <- file.path(project_dir, "latex_paper", "figures")
  dir.create(latex_fig_dir, recursive = TRUE, showWarnings = FALSE)
  pngs <- list.files(figures_dir, pattern = "\\.png$", full.names = TRUE)
  ok <- file.copy(pngs, latex_fig_dir, overwrite = TRUE)
  if (!all(ok)) warning("Failed to mirror: ", paste(basename(pngs[!ok]), collapse = ", "))

# Report------------------------------------------------------------------------

  message(sprintf("Predictors: %d in the long sample, %d in the short sample.",
                  length(vars_1927), length(vars_1986)))
  message(sprintf("Targets: market %d months (%s-%s); %d factors, %d observations (%s-%s).",
                  nrow(mkt_target), format(min(mkt_target$month_id), "%Y"),
                  format(max(mkt_target$month_id), "%Y"),
                  dplyr::n_distinct(fac_target$name), nrow(fac_target),
                  format(min(fac_target$month_id), "%Y"),
                  format(max(fac_target$month_id), "%Y")))
