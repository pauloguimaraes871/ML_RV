##################################
##  DeMiguel factor panel (v3)  ##
##################################
#
# Builds the nine-factor dataset of DeMiguel, Martin-Utrera and Uppal (2024)
# for the same V3 forecasting pipeline that load_data.R feeds with JKP factors.
# Only the factor universe changes:
#   * the per-factor HAR blocks come from generate_rv_feature_block(), the very
#     function load_data.R uses, so the feature definitions cannot drift;
#   * the shared (macro, implied-vol, calendar) covariates are the SAVED
#     covariates_<tier>v2.rds artifacts of load_data.R, checked here against
#     the JKP panel so both universes see byte-identical shared features;
#   * keys enter under the JKP rule (drop first two months, then require a
#     complete HAR block), so each factor keeps its own provider history.
#
# Provenance is kept apart from the JKP build on purpose: universe token
# `demiguel`, separate artifact names and a separate manifest. The market key is
# `mkt` (French Mkt-RF), never `spx_vw` (CRSP vwretd).
#
# Returns: monthly provider files are the returns portfolios earn. Daily files
# give realized variance. See docs/DEMIGUEL_REPLICATION.md.

  `%>%` <- dplyr::`%>%`

  source(file.path(here::here(), "R", "helpers.R"))
  source(file.path(here::here(), "R", "portfolio_helpers.R"))
  source(file.path(here::here(), "R", "demiguel_helpers.R"))

# Configuration-----------------------------------------------------------------

  data_dir <- file.path(here::here(), "data")
  raw_dir <- file.path(data_dir, "demiguel", "raw")

  ## Provider choices, fixed by the Table I check (see build_demiguel_returns()).
  ## q5 2025 is the newest Hou-Xue-Zhang vintage at download (2026-10-06).
  sources <- list(mkt_source = "ff3", smb_source = "ff3", hml_source = "ff3",
                  q5_vintage = "2025")

  ## Absolute Table I Sharpe difference accepted as a data match. Provider
  ## revisions since the paper make exact equality impossible.
  sharpe_tolerance <- .03

  ## Tiers mirror load_data.R: the long sample uses the 1927 covariate set, the
  ## short sample the 1986 one. Sample ids are kept so every runner switch works.
  tiers <- c("1927", "1986")

# Factor returns----------------------------------------------------------------

  ## Monthly provider returns: what portfolios earn, and the Table I check
  monthly <- do.call(build_demiguel_returns,
                     c(list(raw_dir = raw_dir, frequency = "monthly"), sources))
  ## Daily provider returns: realized variance and volatility signals only
  daily <- do.call(build_demiguel_returns,
                   c(list(raw_dir = raw_dir, frequency = "daily"), sources))

  sharpe_check <- check_demiguel_sharpe(monthly$returns, tolerance = sharpe_tolerance)
  cat("\nTable I, Panel A, SR(r_k), January 1977 - December 2020:\n")
  print(as.data.frame(dplyr::select(sharpe_check, name, sharpe, sharpe_paper,
                                    difference, within_tolerance)), digits = 3)
  if (!all(sharpe_check$within_tolerance)) {
    rlang::abort("Unmanaged factor Sharpe ratios do not match Table I; inspect the data.")
  }

  ## Monthly returns in the shape the portfolio code expects. Every DeMiguel
  ## factor, MKT included, is already an excess return: RF is never subtracted
  ## again. RF is kept as the collateral return.
  returns_monthly <- monthly$returns %>%
    dplyr::inner_join(monthly$rf, by = "month_id", relationship = "many-to-one") %>%
    dplyr::transmute(name, month_id, factor_exc = ret, rf) %>%
    dplyr::arrange(name, month_id)
  if (nrow(returns_monthly) != nrow(monthly$returns)) {
    rlang::abort("Missing RF for a monthly factor return.")
  }

# Per-factor lagged RV blocks and target----------------------------------------

  ## Strictly within factor, exactly as load_data.R does for JKP: group_modify()
  ## hands each factor its own rows, so no lag or window crosses a boundary.
  factor_blocks <- daily$returns %>%
    dplyr::group_by(name) %>%
    dplyr::group_modify(~ {
      purrr::reduce(
        generate_rv_feature_block(.x, date_col = "date", ret_col = "ret"),
        dplyr::full_join,
        by = "month_id"
      )
    }) %>%
    dplyr::ungroup()

  ## Monthly sum of squared daily returns: identical in definition to the JKP
  ## target. MKT is the daily EXCESS return here, not CRSP's total return.
  target <- daily$returns %>%
    dplyr::mutate(month_id = lubridate::floor_date(date, unit = "month")) %>%
    dplyr::group_by(name, month_id) %>%
    dplyr::summarize(rv_month = sum(ret ^ 2), .groups = "drop") %>%
    dplyr::arrange(month_id, name)

  ## JKP minimum-history rule, unchanged: first two months are structurally
  ## unusable, then the HAR block must be complete.
  har_block_cols <- setdiff(names(factor_blocks), c("name", "month_id"))

  factor_blocks <- factor_blocks %>%
    dplyr::group_by(name) %>%
    dplyr::arrange(month_id, .by_group = TRUE) %>%
    dplyr::filter(dplyr::row_number() > 2) %>%
    dplyr::ungroup()
  factor_blocks <- factor_blocks[stats::complete.cases(factor_blocks[, har_block_cols]), ]

# Shared covariates-------------------------------------------------------------

  ## Reuse the saved shared covariates of the JKP build, minus the market's own
  ## HAR columns (each key carries its own). Before joining, prove they are the
  ## same values the JKP panel was built on, so the two universes differ ONLY in
  ## their factors.
  shared_covariates <- purrr::map(stats::setNames(tiers, tiers), function(tier) {

    covariates <- readRDS(file.path(data_dir, sprintf("covariates_%sv2.rds", tier)))
    if (!all(har_block_cols %in% names(covariates))) {
      rlang::abort("Saved covariates lack the HAR block columns; rerun load_data.R.")
    }
    shared <- covariates[, setdiff(names(covariates), har_block_cols)]

    jkp <- readRDS(file.path(data_dir, sprintf("covariates_panel_%s_v3.rds", tier))) %>%
      dplyr::filter(name == "spx_vw") %>%
      dplyr::select(dplyr::all_of(names(shared))) %>%
      dplyr::arrange(month_id)
    reused <- shared %>%
      dplyr::filter(month_id %in% jkp$month_id) %>%
      dplyr::arrange(month_id)
    if (!isTRUE(all.equal(as.data.frame(jkp), as.data.frame(reused)))) {
      rlang::abort(paste("Shared covariates differ from the JKP panel for tier", tier))
    }
    shared
  })

  panel_raw <- purrr::map(shared_covariates, function(shared) {
    factor_blocks %>%
      dplyr::inner_join(shared, by = "month_id", relationship = "many-to-one") %>%
      dplyr::arrange(month_id, name)
  })

  purrr::iwalk(panel_raw, function(panel, tier) {
    cat("\nDeMiguel panel", tier, ":", nrow(panel), "rows,",
        dplyr::n_distinct(panel$name), "keys,",
        format(min(panel$month_id)), "to", format(max(panel$month_id)), "\n")
    print(panel %>% dplyr::group_by(name) %>%
            dplyr::summarise(first = min(month_id), n = dplyr::n(), .groups = "drop"))
  })

# Preprocessing-----------------------------------------------------------------

  ## Same calls and winsorization ranges as load_data.R
  panel_prepped <- purrr::imap(panel_raw, function(panel, tier) {
    list(
      zscore = prep_covariates(panel, scaling = "zscore", key_col = "name"),
      range  = prep_covariates(panel, scaling = "range",
                               winsorize_range = c(0.025, 0.975), key_col = "name")
    )
  })

# Save--------------------------------------------------------------------------

  ## Data artifacts match data/*_v3.rds and stay out of git; their hashes go
  ## into the tracked manifest below.
  outputs <- c(
    purrr::imap(panel_raw, ~ .x) %>%
      stats::setNames(sprintf("covariates_demiguel_%s_v3.rds", tiers)),
    purrr::imap(panel_prepped, ~ .x$zscore) %>%
      stats::setNames(sprintf("covariates_demiguel_%s_zscore_v3.rds", tiers)),
    purrr::imap(panel_prepped, ~ .x$range) %>%
      stats::setNames(sprintf("covariates_demiguel_%s_range_v3.rds", tiers)),
    list(
      "target_demiguel_v3.rds"          = target,
      "demiguel_returns_monthly_v3.rds" = returns_monthly,
      "demiguel_returns_daily_v3.rds"   = daily$returns,
      "demiguel_rf_daily_v3.rds"        = daily$rf
    )
  )

  purrr::iwalk(outputs, ~ saveRDS(.x, file.path(data_dir, .y)))
  utils::write.csv(sharpe_check, file.path(data_dir, "demiguel", "table1_sharpe_check.csv"),
                   row.names = FALSE)

# Manifest----------------------------------------------------------------------

  date_column <- function(x) if ("month_id" %in% names(x)) x$month_id else x$date

  manifest <- dplyr::bind_rows(
    tibble::tibble(
      role = "input",
      artifact = c(file.path("demiguel", "raw", list.files(raw_dir)),
                   sprintf("covariates_%sv2.rds", tiers),
                   sprintf("covariates_panel_%s_v3.rds", tiers)),
      sha256 = purrr::map_chr(file.path(data_dir, artifact), hash_artifact)
    ),
    purrr::imap_dfr(outputs, function(object, artifact) {
      tibble::tibble(
        role = "output", artifact = artifact,
        sha256 = hash_artifact(file.path(data_dir, artifact)),
        n_rows = nrow(object), n_cols = ncol(object),
        n_keys = if ("name" %in% names(object)) dplyr::n_distinct(object$name) else 1L,
        date_min = as.character(min(date_column(object))),
        date_max = as.character(max(date_column(object)))
      )
    }),
    tibble::tibble(
      role = "policy",
      artifact = c("universe", "sources", "returns", "target", "market_key",
                   "min_history", "shared_covariates", "preprocessing"),
      notes = c(
        "DeMiguel et al. (2024) nine factors: mkt smb hml rmw cma umd roe ia bab",
        paste(names(sources), unlist(sources), sep = "=", collapse = "; "),
        "native monthly provider returns are earned; daily files only build RV and volatility",
        "monthly sum of squared daily returns, as the JKP target; mkt uses daily Mkt-RF",
        "mkt = French Mkt-RF; CRSP spx_vw is NOT pooled into this panel",
        "JKP rule: each key drops its first 2 months, then requires a complete HAR block",
        "covariates_<tier>v2.rds minus HAR block columns; checked equal to the JKP panel",
        "expanding, within key; zscore winsorized 0.01/0.99, range 0.025/0.975, min_n=36"
      )
    )
  )

  utils::write.csv(manifest, file.path(data_dir, "manifest_demiguel_v3.csv"), row.names = FALSE)
  cat("\nWrote", length(outputs), "DeMiguel artifacts and data/manifest_demiguel_v3.csv\n")

# Tests-------------------------------------------------------------------------

  ## Like load_data.R: a build that fails its own checks is not a build.
  testthat::test_file(
    file.path(here::here(), "tests", "testthat", "test-demiguel-data.R"),
    reporter = "summary",
    stop_on_failure = TRUE
  )
