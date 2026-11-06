# Inspect saved DeMiguel results, extract standalone managed-factor returns and
# assemble every number of the results e-mail (email_* objects, last section).
# Run in the project's existing R/renv environment. This script reads saved
# artifacts and creates objects in the R session; it does not write files,
# refit forecasting models or optimize portfolios. The only new computations
# are the standalone managed factors: their returns, stock-level costs and
# bootstrap p-values, using the saved bootstrap settings (about one minute).

project_dir <- "C:/Users/paulo.fonteles/OneDrive - Sicoob/Documentos/Doutorado/ML_RV"
data_dir <- file.path(project_dir, "data")
`%>%` <- dplyr::`%>%`

# Source function definitions, rather than the table runners, which write outputs.
# portfolio_inference_helpers: draw_portfolio_months() (bootstrap plan);
# demiguel_cost_helpers: stock_cost() (standalone net returns).
source(file.path(project_dir, "R", "portfolio_helpers.R"))
source(file.path(project_dir, "R", "portfolio_inference_helpers.R"))
source(file.path(project_dir, "R", "demiguel_cost_helpers.R"))
source(file.path(project_dir, "R", "demiguel_replication_helpers.R"))
source(file.path(project_dir, "R", "demiguel_table3_helpers.R"))

# Table I: original lagged-variance specification -------------------------------

# demiguel_table1_ac_v3.rds is a list with:
#   $table: nine factor rows. sr_unmanaged is buy-and-hold; sr_is and sr_oos
#     are Sharpe ratios of the managed/unmanaged MVO COMBINATION (Panels A/C).
#     a_is and b_is are its rescaled in-sample sleeve coefficients.
#   $returns: monthly unmanaged and managed_combo returns, coefficients a/b,
#     factor name and panel. Panel C additionally records train_end.
#     The standalone managed sleeve is not stored as its own return column.
#   $settings: estimation dates, initial M, risk aversion and bootstrap settings.
# Evaluation: February 1977-December 2020; initial estimation: 120 months.
table1_ac <- readRDS(file.path(data_dir, "demiguel_table1_ac_v3.rds"))
table1_ac_table <- table1_ac$table
table1_ac_vs_paper <- compare_table1(table1_ac_table)

# demiguel_table1_bde_v3.rds is a list with:
#   $table: nine factor rows for B (in-sample net), D (OOS net with cost-unaware
#     allocation, separate sleeve costs), E (OOS cost-aware with trade netting).
#     The table also records p-values, Panel B coefficients and unit sleeve
#     average monthly costs in basis points. These are combination portfolios.
#   $results: named factor lists, e.g. $results$umd. Each contains:
#     $in_sample: monthly unmanaged, unmanaged_net, gross_b and net_b returns;
#     $oos: monthly gross_c/gross_e, cost series, net_d/net_e, unmanaged and
#       unmanaged_net, and Panel E coefficients a_e/b_e;
#     $b_is: Panel B coefficients; $tc_sleeves: in-sample unit sleeve costs.
#   $settings: sample, initial M, risk aversion and bootstrap settings.
table1_bde <- readRDS(file.path(data_dir, "demiguel_table1_bde_v3.rds"))
table1_bde_table <- table1_bde$table
table1_bde_vs_paper <- compare_table1_costs(table1_bde_table)

# Table I: lagged signals and ML forecasts -------------------------------------

# demiguel_table1_ml_v3.rds is a list with $paper_window, $full_oos, $settings.
# Each window contains named signals: lagvar, lagss, har, harx, ew.
#   lagvar: preceding-month sample variance of daily factor returns.
#   lagss: preceding-month sum of squared daily returns (target persistence).
#   har/harx/ew: forecasts of monthly sum-of-squared returns; ew averages seven
#     model forecasts on the LOG scale, excluding HAR.
# Each signal result has $table, $ac and $bde. $ac has the baseline A/C schema;
# $bde has the baseline B/D/E schema, and is NULL for full_oos.
# $settings includes per-factor start dates and initial M, inference settings,
# model membership and recorded forecast-source paths/hashes.
#
# paper_window evaluates all factors February 1977-December 2020. ROE/IA start
# their portfolio-estimation sample in March 1970 (83 initial months); the
# other factors start in February 1967 (120). This also restricts lagvar/lagss
# to provide controls with the same estimation history as ML.
# full_oos uses each factor's forecast coverage through December 2024 and 120
# initial estimation months. It is a sample extension, not a published table.
# Its Panel A remains an in-sample portfolio fit, despite OOS volatility inputs.
table1_ml <- readRDS(file.path(data_dir, "demiguel_table1_ml_v3.rds"))
table1_ml_paper_window <- purrr::map_dfr(table1_ml$paper_window, "table")
table1_ml_full_oos <- purrr::map_dfr(table1_ml$full_oos, "table")

# Select one signal before comparing with paper: comparisons join by factor
# and statistic, without a signal key. ROE/IA have shorter estimation histories
# here than in the original baseline, although evaluation dates match.
table1_ml_ew_ac_vs_paper <- compare_table1(table1_ml$paper_window$ew$table)
table1_ml_ew_bde_vs_paper <- compare_table1_costs(table1_ml$paper_window$ew$table)

# Table III: original market-volatility specification --------------------------

# demiguel_table3_v3.rds is a list with $table, $returns, $weights, $settings.
#   $table: Panels A-D and four portfolio columns:
#     umv_notc / cmv_notc: coefficients optimized ignoring trading costs;
#     umv_tc / cmv_tc: coefficients optimized accounting for trading costs.
#     _notc does not mean cost-free evaluation: these portfolios pay costs
#     when evaluated in Panels C/D.
#     A = in-sample gross; B = OOS gross; C = OOS minus separate-sleeve costs;
#     D = OOS minus stock-level portfolio-netted costs. B/C/D reuse weights.
#     mean, sd, alpha and tc are annualized DECIMALS; sr and t_alpha are unitless.
#     Reported sr uses net mean / gross SD. p_value compares CMV with matching
#     UMV; it is not a test of one volatility signal against another.
#   $returns: months_is and gross_is (Panel A); months and gross (OOS);
#     tc_ntd and tc_td are MONTHLY decimal cost matrices. Matrix columns are
#     the same four portfolios. Panel D monthly net = gross - tc_td.
#   $weights: $in_sample coefficient matrix and $oos monthly coefficient lists.
#   $settings: initial M, gamma, refit frequency and bootstrap settings.
# Managed sleeves use inverse MARKET SD. CFW paper columns 2/5 are absent.
table3 <- readRDS(file.path(data_dir, "demiguel_table3_v3.rds"))
table3_table <- table3$table
table3_vs_paper <- compare_table3(table3_table)

# demiguel_table3_ml_v3.rds is a list with $table, $runs, $results, $settings.
#   $runs: nine configurations keyed by variant, signal, estimation start, M.
#   $results: a list aligned with $runs; each entry has the baseline Table III
#     $table/$returns/$weights/$settings schema.
#   $table: stacked run statistics with configuration keys attached.
#   $settings: risk aversion, refit/inference settings and forecast provenance.
# Signals: lagvar, lagss, harx, ew. CMV uses market SD; FCMV uses each factor's
# own SD. In FCMV rows, the cmv_* column labels therefore represent FCMV.
# All runs evaluate February 1977-December 2020. CMV has four 1967/M=120 runs
# plus a lagvar 1970/M=83 control; FCMV has four 1970/M=83 runs. Here 'start'
# describes ESTIMATION start, unlike the evaluation start in Table I $table.
table3_ml <- readRDS(file.path(data_dir, "demiguel_table3_ml_v3.rds"))
table3_ml_table <- table3_ml$table
table3_ml_panel_d <- dplyr::filter(table3_ml_table, panel == "D")
table3_ml_cmv_ew <- dplyr::filter(
  table3_ml_table, variant == "cmv", signal == "ew",
  start == as.Date("1967-02-01")
)
table3_ml_cmv_ew_vs_paper <- compare_table3(table3_ml_cmv_ew)

# Standalone managed portfolios: the nine DeMiguel factors ----------------------

# demiguel_returns_monthly_v3.rds is a long factor-month table:
# name, month_id, factor_exc and rf. Returns are decimal monthly values; the
# factor returns are already excess returns. These are native provider monthly
# returns, not monthly returns compounded here from the daily file.
monthly <- readRDS(file.path(data_dir, "demiguel_returns_monthly_v3.rds"))

# demiguel_returns_daily_v3.rds is a long name/date/ret table of decimal daily
# factor returns. It supplies volatility signals, not the earned monthly return.
daily <- readRDS(file.path(data_dir, "demiguel_returns_daily_v3.rds"))
variance <- prepare_factor_variance(daily)

# The ensemble's variance forecast, stamped to the month it scales: the saved
# EW ensemble (log scale) inverted as exp(pred) - 1e-8, as in the Table I/III
# ML runners. Some caches are S4 objects, others plain lists.
ensemble <- readRDS(file.path(project_dir, "models_v3", "demiguel", "1927", "ensembles",
                              "ew_demiguel_range_grid_1927.rds"))
ensemble <- if (isS4(ensemble)) ensemble@oos_outputs else ensemble$oos_outputs
ensemble_variance <- dplyr::transmute(ensemble, name, month_id, variance = exp(pred) - 1e-8)

# demiguel_stock_costs_v3.rds: the authors' stock weights, kappas and stock
# returns (R/load_demiguel_costs.R). Needed only for standalone NET returns.
stock <- readRDS(file.path(data_dir, "demiguel_stock_costs_v3.rds"))

# Evaluation months and the bootstrap plan of Table I: same months, draws,
# block length and seed as the saved runs, so p-values are comparable.
eval_months <- seq(as.Date("1977-02-01"), as.Date("2020-12-01"), by = "month")
calendar <- draw_portfolio_months(eval_months, draws = table1_ac$settings$draws,
                                  block_length = table1_ac$settings$block_length,
                                  seed = table1_ac$settings$seed)

# build_table1_sample() already constructs the standalone managed sleeve:
#   managed_t = factor_exc_t * c / variance signal for month t.
# No managed/unmanaged MVO combination is estimated in this block.
# c matches volatility over the estimation sample, as in the replication. It
# cancels from gross AND net Sharpe ratios (costs scale with the exposure too),
# but return levels/exposures retain it. This is not the manuscript's expanding c_t.
#
# Standalone NET returns: the sleeve's own stock-level trades (exposure c /
# variance on the factor's stock weights), entry from cash in February 1977,
# and, as in Table I, the AVERAGE realized monthly cost deducted from every
# month. No second sleeve, so no trading diversification is possible.
# p-values: one-sided bootstrap of SR(standalone) - SR(unmanaged), gross
# against the unmanaged factor, net against the OOS unmanaged factor net of
# its own costs (entry included), the benchmark of Table I Panels D and E.
#
# sample_start: estimation start per factor. Pure standalone results do not
# depend on it (it only changes c); the "matched" column uses the ML runs'
# starts (ROE/IA March 1970) to mirror the MVO comparison.
standalone_for <- function(signal, sample_start, unmanaged_net) {
  purrr::map_dfr(demiguel_table1_paper$name, function(k) {
    s <- build_table1_sample(monthly, signal, factor_name = k,
                             start = sample_start(k), end = as.Date("2020-12-01")) |>
      dplyr::filter(month_id >= as.Date("1977-02-01"))
    if (!identical(s$month_id, eval_months)) rlang::abort(paste("Evaluation months differ for", k))
    cost <- stock_cost(stock, tibble::tibble(month_id = s$month_id, sleeve = "managed",
                                             factor = k, w = s$scale / s$variance),
                       netting = "sleeve", charge_entry = TRUE)$cost
    net <- s$managed - mean(cost)
    tibble::tibble(
      name = k, n = nrow(s),
      sr_unmanaged = annual_sharpe(s$factor_exc),
      sr_managed = annual_sharpe(s$managed),
      p_managed = demiguel_sr_pvalue(s$factor_exc, s$managed, calendar),
      sr_managed_net = sqrt(12) * mean(net) / stats::sd(s$managed),
      p_managed_net = demiguel_sr_pvalue(unmanaged_net[[k]], net, calendar),
      cost_bp = 1e4 * mean(cost)
    )
  })
}

# OOS unmanaged factor net of its own costs (entry included), per factor, from
# a saved B/D/E result; identical across signals (no estimation involved).
oos_unmanaged_net <- function(bde) purrr::map(bde$results, ~ .x$oos$unmanaged_net)

baseline_start <- function(k) as.Date("1967-02-01")
ml_start <- function(k) factor_setting(table1_ml$paper_window$ew$bde$settings$start, k)

# Nine rows each, over the SAME 527 evaluation months as Table I. sr_managed
# is the pure managed factor, not sr_is/sr_oos.
standalone_table <- standalone_for(variance, baseline_start, oos_unmanaged_net(table1_bde))
standalone_matched <- standalone_for(variance, ml_start,
                                     oos_unmanaged_net(table1_ml$paper_window$lagvar$bde))
standalone_ensemble <- standalone_for(ensemble_variance, ml_start,
                                      oos_unmanaged_net(table1_ml$paper_window$ew$bde))

# Optional: standalone portfolios already used in the manuscript ---------------

# Set TRUE to load the larger JKP object. These portfolios use a different
# universe and methodology from the nine-factor standalone extraction above.
load_manuscript_mm <- FALSE
if (load_manuscript_mm) {
  # all_ports.rds is a named model list: har, harx, glmnet, rf, xgb, nn1/2/3, ew.
  # Each model contains $w_compiled_df (monthly paths), $calibration_data
  # (return/signal history, including warm-up), $strategy_summary (per-factor
  # performance), $coverage and $settings. The panel has 153 JKP factors plus
  # spx_vw, rather than the nine French/HXZ/AQR factors.
  # Methods inside every model: buyhold = unmanaged, rw = lagged monthly sum
  # of squared daily returns, vol_scaled = that model's variance forecast.
  # Expanding volatility normalization uses 60 prior pairs. Four saved return
  # versions distinguish raw/capped exposure and gross/net returns. The cap
  # is 1.5; costs are 10 bp per change in factor exposure, excluding stock costs.
  # Here ew labels the FORECAST ensemble; no MVO combination occurs.
  mm <- readRDS(file.path(project_dir, "ports", "v3", "1927", "panel",
                          "expanding_vol_10bps_60m", "all_ports.rds"))
  mm_ensemble_returns <- mm$ew$w_compiled_df
  mm_ensemble_table <- dplyr::select(
    mm$ew$strategy_summary, name, method, n, start, end,
    sharpe_ratio_raw, sharpe_ratio_capped,
    sharpe_ratio_net_raw, sharpe_ratio_net_capped
  )
  # Replace mm$ew with mm$har/mm$harx for their standalone forecast portfolios.
  # 'rw' rows are persistence controls, not forecasts from the selected model.
}

# Numbers of the results e-mail ------------------------------------------------

# Display helpers: three decimals without the leading zero (.412, -.233), and
# "Sharpe (p-value)". p-values are against the corresponding unmanaged factor.
fmt <- function(x, digits = 3) {
  out <- sub("^(-?)0\\.", "\\1.", formatC(x, format = "f", digits = digits))
  ifelse(is.na(x), "—", out)
}
fmt_sp <- function(sr, p) paste0(fmt(sr), " (", fmt(p), ")")
row_of <- function(tab, k) tab[tab$name == k, ]

# Table I in e-mail layout, one panel at a time. Columns:
#   Published: the paper (demiguel_table1_paper / _paper_costs);
#   Our replication: baseline lagvar, every factor from February 1967;
#   Matched lagvar: lagvar on the ML runs' estimation history (ROE/IA 1970);
#   ML Ensemble: the EW ensemble forecast. All evaluate Feb 1977-Dec 2020.
# Rows per factor: Unmanaged (Sharpe only), MVO combination, Standalone managed.
# Panel E's Unmanaged row is the OOS unmanaged factor net of its own costs,
# entry included: the benchmark behind every Panel E p-value. (Table I's
# Panel B column sr_unmanaged_net is the in-sample version and differs in the
# third decimal for some factors.)
email_table1_panel <- function(panel = c("C", "E")) {
  panel <- match.arg(panel)
  ml_lag <- table1_ml$paper_window$lagvar
  ml_ew <- table1_ml$paper_window$ew
  unm_net <- function(bde, k) {
    o <- bde$results[[k]]$oos
    sqrt(12) * mean(o$unmanaged_net) / stats::sd(o$unmanaged)
  }
  purrr::map_dfr(demiguel_table1_paper$name, function(k) {
    if (panel == "C") {
      unm <- c(row_of(demiguel_table1_paper, k)$sr_unmanaged, row_of(table1_ac_table, k)$sr_unmanaged,
               row_of(ml_lag$table, k)$sr_unmanaged, row_of(ml_ew$table, k)$sr_unmanaged)
      mvo <- c(fmt_sp(row_of(demiguel_table1_paper, k)$sr_oos, row_of(demiguel_table1_paper, k)$p_oos),
               fmt_sp(row_of(table1_ac_table, k)$sr_oos, row_of(table1_ac_table, k)$p_oos),
               fmt_sp(row_of(ml_lag$table, k)$sr_oos, row_of(ml_lag$table, k)$p_oos),
               fmt_sp(row_of(ml_ew$table, k)$sr_oos, row_of(ml_ew$table, k)$p_oos))
      sa <- c("—",
              fmt_sp(row_of(standalone_table, k)$sr_managed, row_of(standalone_table, k)$p_managed),
              fmt_sp(row_of(standalone_matched, k)$sr_managed, row_of(standalone_matched, k)$p_managed),
              fmt_sp(row_of(standalone_ensemble, k)$sr_managed, row_of(standalone_ensemble, k)$p_managed))
    } else {
      pc <- row_of(demiguel_table1_paper_costs, k)
      unm <- c(pc$sr_unmanaged_net, unm_net(table1_bde, k), unm_net(ml_lag$bde, k), unm_net(ml_ew$bde, k))
      mvo <- c(fmt_sp(pc$sr_oos_td, pc$p_oos_td),
               fmt_sp(row_of(table1_bde_table, k)$sr_oos_td, row_of(table1_bde_table, k)$p_oos_td),
               fmt_sp(row_of(ml_lag$table, k)$sr_oos_td, row_of(ml_lag$table, k)$p_oos_td),
               fmt_sp(row_of(ml_ew$table, k)$sr_oos_td, row_of(ml_ew$table, k)$p_oos_td))
      sa <- c("—",
              fmt_sp(row_of(standalone_table, k)$sr_managed_net, row_of(standalone_table, k)$p_managed_net),
              fmt_sp(row_of(standalone_matched, k)$sr_managed_net, row_of(standalone_matched, k)$p_managed_net),
              fmt_sp(row_of(standalone_ensemble, k)$sr_managed_net, row_of(standalone_ensemble, k)$p_managed_net))
    }
    tibble::tibble(Factor = toupper(k),
                   Portfolio = c("Unmanaged", "MVO combination", "Standalone managed"),
                   Published = c(fmt(unm[1]), mvo[1], sa[1]),
                   `Our replication` = c(fmt(unm[2]), mvo[2], sa[2]),
                   `Matched lagvar` = c(fmt(unm[3]), mvo[3], sa[3]),
                   `ML Ensemble` = c(fmt(unm[4]), mvo[4], sa[4]))
  })
}
email_table1_c <- email_table1_panel("C")
email_table1_e <- email_table1_panel("E")

# Text claims about the standalone extension: number of factors (of nine)
# whose standalone Sharpe the ensemble raises over the lagvar replication.
email_standalone_counts <- c(
  net_improved = sum(standalone_ensemble$sr_managed_net > standalone_table$sr_managed_net),
  gross_improved = sum(standalone_ensemble$sr_managed > standalone_table$sr_managed)
)

# Table III, Panel D, cost-aware columns (4) UMV and (6) CMV. Replication from
# demiguel_table3_v3.rds; ML Ensemble CMV from the table3_ml run with the EW
# market-volatility signal (1967 sample, M = 120). p-values with four decimals.
table3_stats <- function(r, p_digits = 4) {
  c(`Annual net mean` = fmt(r$mean), `Annual gross volatility` = fmt(r$sd),
    `Reported Sharpe ratio` = fmt(r$sr), `p-value versus UMV` = fmt(r$p_value, p_digits),
    `Annual alpha versus UMV` = fmt(r$alpha), `t-statistic of alpha` = fmt(r$t_alpha),
    `Annual trading costs` = fmt(r$tc))
}
pick3 <- function(tab, col) tab[tab$panel == "D" & tab$column == col, ]
ml_run <- function(variant, signal, start, col) {
  pick3(dplyr::filter(table3_ml_table, variant == !!variant, signal == !!signal,
                      start == as.Date(!!start)), col)
}
email_table3 <- tibble::tibble(
  Statistic = names(table3_stats(pick3(table3_table, "umv_tc"))),
  `Paper UMV` = table3_stats(pick3(demiguel_table3_paper, "umv_tc"), 3),
  `Paper CMV` = table3_stats(pick3(demiguel_table3_paper, "cmv_tc"), 3),
  `Replication UMV` = table3_stats(pick3(table3_table, "umv_tc")),
  `Replication CMV` = table3_stats(pick3(table3_table, "cmv_tc")),
  `ML Ensemble CMV` = table3_stats(ml_run("cmv", "ew", "1967-02-01", "cmv_tc"))
)

# FCMV table: every column is estimated from March 1970 (M = 83), because
# ROE/IA forecasts start then; evaluation is still Feb 1977-Dec 2020. Hence
# UMV and CMV lagvar differ from email_table3. In FCMV rows the cmv_* columns
# hold FCMV (each factor scaled by its own volatility).
email_fcmv <- tibble::tibble(
  Statistic = names(table3_stats(pick3(table3_table, "umv_tc"))),
  UMV = table3_stats(ml_run("cmv", "lagvar", "1970-03-01", "umv_tc")),
  `CMV lagvar` = table3_stats(ml_run("cmv", "lagvar", "1970-03-01", "cmv_tc")),
  `FCMV lagvar` = table3_stats(ml_run("fcmv", "lagvar", "1970-03-01", "cmv_tc")),
  `FCMV ensemble` = table3_stats(ml_run("fcmv", "ew", "1970-03-01", "cmv_tc"))
)

# Text claim: annual trading costs of CMV, lagvar replication vs ensemble.
email_cmv_costs_pct <- c(
  replication = 100 * pick3(table3_table, "cmv_tc")$tc,
  ensemble = 100 * ml_run("cmv", "ew", "1967-02-01", "cmv_tc")$tc
)

# The UMV of the ensemble run must equal the replication UMV (no signal used).
stopifnot(isTRUE(all.equal(ml_run("cmv", "ew", "1967-02-01", "umv_tc")$sr,
                           pick3(table3_table, "umv_tc")$sr)))

print(as.data.frame(email_table1_c), row.names = FALSE)
print(as.data.frame(email_table1_e), row.names = FALSE)
print(email_standalone_counts)
print(as.data.frame(email_table3), row.names = FALSE)
print(as.data.frame(email_fcmv), row.names = FALSE)
print(round(email_cmv_costs_pct, 2))

# Inspect, for example:
# table1_ac_table
# table1_bde_table
# table1_ml_paper_window
# table3_ml_panel_d
# standalone_table; standalone_ensemble
