# Saved-return adapters for portfolio inference --------------------------------
# No function in this file estimates a forecast, scaler, or portfolio weight.
`%>%` <- dplyr::`%>%`

#' Extract comparable net returns from a completed portfolio
#'
#' @param data Saved MM weights or allocation weights/monthly returns.
#' @param family Either mm or allocation, selecting the saved column convention.
#' @param version Raw or capped exposure. Both versions deduct saved costs.
#' @return Original keys with excess_return and collateralized total_return.
#'   Units remain monthly decimals. Missing columns, nonfinite returns,
#'   inconsistent costs, or insolvency fail.
extract_portfolio_inference_returns <- function(data, family, version) {
  if (!is.character(family) || length(family) != 1L || !family %in% c("mm", "allocation") ||
      !is.character(version) || length(version) != 1L || !version %in% c("raw", "capped")) {
    rlang::abort("Select mm/allocation and raw/capped explicitly.")
  }
  net <- if (family == "mm") paste0("r_exc_fwd1_net_", version) else paste0("net_", version)
  gross <- if (family == "mm") paste0("r_exc_fwd1_", version) else paste0("gross_", version)
  cost <- if (family == "mm" && version == "capped") "tc" else paste0("tc_", version)
  required <- c("month_id", "rf", net, gross, cost)
  if (!is.data.frame(data) || !all(required %in% names(data)) || !nrow(data) ||
      !all(purrr::map_lgl(data[required[-1]], is.numeric)) ||
      any(!is.finite(as.matrix(data[required[-1]])))) {
    rlang::abort("Saved returns require finite numeric net, gross, cost, and rf columns.")
  }
  if (any(data[[cost]] < 0) || any(abs(data[[gross]] - data[[cost]] - data[[net]]) > 1e-12)) {
    rlang::abort("Saved net returns do not equal gross returns less nonnegative costs.")
  }
  result <- data %>% dplyr::mutate(excess_return = .data[[net]],
                                   total_return = .data[[net]] + rf)
  if (family == "mm") {
    total <- paste0("r_strat_fwd1_net_", version)
    if (!total %in% names(data) || anyNA(data[[total]]) ||
        any(abs(result$total_return - data[[total]]) > 1e-12)) {
      rlang::abort("Saved MM total returns do not equal net excess returns plus rf.")
    }
  }
  if (any(result$total_return <= -1)) rlang::abort("Insolvent strategy in saved returns.")
  result
}

#' Check copies of a saved benchmark before discarding duplicates
#'
#' @param reference,data Data frames with the same columns and keyed observations.
#' @param keys Explicit matching keys, normally method/name/month_id.
#' @return Invisible TRUE. Order may differ, values may not (tolerance 1e-12).
check_portfolio_benchmark_copy <- function(reference, data, keys) {
  if (!is.data.frame(reference) || !is.data.frame(data) ||
      !setequal(names(reference), names(data)) || !all(keys %in% names(data)) ||
      anyNA(reference[keys]) || anyNA(data[keys]) ||
      anyDuplicated(reference[keys]) || anyDuplicated(data[keys])) {
    rlang::abort("Benchmark copies require matching columns and unique complete keys.")
  }
  ordered <- function(x) x %>% dplyr::arrange(dplyr::across(dplyr::all_of(keys))) %>%
    dplyr::select(dplyr::all_of(names(reference))) %>% tibble::as_tibble()
  if (!isTRUE(all.equal(ordered(reference), ordered(data), tolerance = 1e-12))) {
    rlang::abort("Benchmark copies differ. They cannot be deduplicated.")
  }
  invisible(TRUE)
}

#' Verify completed-backtest inputs while retaining live-source drift
#'
#' Saved artifacts must remain byte-identical to their run manifests. Files in
#' the repository's live R source directory are also checked and reported, but
#' later source edits do not invalidate inference conditional on saved returns.
#' The executed run snapshot and every other saved input remain strict checks.
#'
#' @param manifest_paths Input-manifest CSV files from completed backtests.
#' @return One row per distinct input with current hashes and audit status.
check_portfolio_inference_provenance <- function(manifest_paths) {
  if (!is.character(manifest_paths) || !length(manifest_paths) ||
      anyNA(manifest_paths) || any(!file.exists(manifest_paths))) {
    rlang::abort("Portfolio inference requires existing input manifests.")
  }
  provenance <- purrr::map_dfr(manifest_paths, function(path) {
    manifest <- utils::read.csv(path)
    if (!all(c("file", "sha256") %in% names(manifest)) ||
        anyNA(manifest[c("file", "sha256")])) {
      rlang::abort(paste("Invalid saved input manifest:", path))
    }
    dplyr::select(manifest, file, sha256)
  }) %>% dplyr::distinct()
  if (anyDuplicated(provenance$file) || any(!file.exists(provenance$file))) {
    rlang::abort("Upstream manifests contain conflicting hashes or unavailable inputs.")
  }
  normalized_file <- stringr::str_replace_all(provenance$file, "\\\\", "/")
  provenance <- provenance %>%
    dplyr::mutate(
      input_type = dplyr::if_else(
        stringr::str_detect(.env$normalized_file, "(^|/)R/[^/]+[.]R$"),
        "live_source", "saved_input"
      ),
      match_required = input_type == "saved_input",
      current_sha256 = purrr::map_chr(file,
        ~ digest::digest(file = .x, algo = "sha256")),
      matches = sha256 == current_sha256
    )
  failed_inputs <- provenance %>% dplyr::filter(match_required, !matches)
  if (nrow(failed_inputs)) {
    rlang::abort(paste("A saved backtest input no longer matches its recorded hash:",
                       paste(basename(failed_inputs$file), collapse = ", ")))
  }
  changed_source <- provenance %>% dplyr::filter(!match_required, !matches)
  if (nrow(changed_source)) {
    rlang::warn(paste(
      "Live portfolio source differs from the completed-backtest version;",
      "inference remains conditional on the unchanged saved returns:",
      paste(basename(changed_source$file), collapse = ", ")
    ))
  }
  provenance
}

#' Read primary completed portfolios for the prespecified inference contrasts
#'
#' @param project_dir Repository root. Reads only primary 60-month-minimum runs.
#' @param sample_id Saved model vintage. The 1927 adapter retains the full
#'   market history and the balanced January 1980--December 2024 factor calendar.
#' @return List with monthly aggregate/market returns, individual factor returns,
#'   comparison definitions, input hashes, effective settings and scaler checks.
#'   Aggregates equal-weight 153 nonmarket factors within each holding month.
#'   The multifactor opportunity set instead includes all 154 factors.
read_primary_portfolio_inference <- function(project_dir = here::here(),
                                             sample_id = c("1986", "1927")) {
  sample_id <- match.arg(sample_id)
  root <- file.path(project_dir, "ports", "v3", sample_id)
  allocation_dir <- file.path(root, "allocations", "expanding_vol_10bps_60m_alloc_60_60_60m")
  first_month <- if (sample_id == "1986") "2011-07-01" else "1980-01-01"
  dates <- seq(as.Date(first_month), as.Date("2024-12-01"), by = "month")
  clusters <- utils::read.csv(file.path(project_dir, "data", "cluster_labels.csv")) %>%
    dplyr::transmute(name = characteristic, cluster)
  if (nrow(clusters) != 153L || anyNA(clusters) || anyDuplicated(clusters$name) ||
      dplyr::n_distinct(clusters$cluster) != 13L) rlang::abort("Unexpected JKP mapping.")
  paths <- character()
  settings <- list()
  read_saved <- function(path) {
    if (!file.exists(path)) rlang::abort(paste("Missing completed portfolio:", path))
    paths <<- unique(c(paths, path))
    readRDS(path)
  }
  record_settings <- function(path, value) settings[[path]] <<- value
  validate_settings <- function(value, expected) {
    purrr::iwalk(expected, function(target, field) {
      if (!isTRUE(all.equal(value[[field]], target, check.attributes = FALSE))) {
        rlang::abort(paste("Unexpected effective portfolio setting:", field))
      }
    })
  }
  validate_panel <- function(data, names_expected, native_market = FALSE) {
    if (native_market && sample_id == "1927") {
      # Validate market and nonmarket calendars separately. Their start dates
      # differ, but no factor is silently admitted to a changing aggregate.
      market <- dplyr::filter(data, name == "spx_vw")
      factors <- dplyr::filter(data, name != "spx_vw")
      market_dates <- seq(as.Date("1952-06-01"), as.Date("2024-12-01"), by = "month")
      counts <- market %>% dplyr::count(method, month_id)
      if (anyDuplicated(data[c("method", "name", "month_id")]) ||
          !setequal(data$name, names_expected) || any(counts$n != 1L) ||
          nrow(counts) != length(unique(market$method)) * length(market_dates) ||
          !identical(sort(unique(market$month_id)), market_dates)) {
        rlang::abort("Unexpected full-history market holdings.")
      }
      if (nrow(factors)) validate_panel(factors, setdiff(names_expected, "spx_vw"))
      if ("train_end" %in% names(market) &&
          (anyNA(market[c("train_end", "n_train")]) ||
           any(market$train_end >= market$month_id) || any(market$n_train < 60))) {
        rlang::abort("Market allocation violates training availability.")
      }
      return(invisible(TRUE))
    }
    if (anyDuplicated(data[c("method", "name", "month_id")]) ||
        !setequal(data$name, names_expected) || !identical(sort(unique(data$month_id)), dates)) {
      rlang::abort("Unexpected saved portfolio keys, universe, or evaluation dates.")
    }
    complete <- data %>% dplyr::count(method, month_id)
    if (any(complete$n != length(names_expected)) ||
        nrow(complete) != length(unique(data$method)) * length(dates)) {
      rlang::abort("Saved portfolios do not have balanced factor-month coverage.")
    }
    if ("train_end" %in% names(data) &&
        (anyNA(data[c("train_end", "n_train")]) ||
         any(data$train_end >= data$month_id) || any(data$n_train < 60))) {
      rlang::abort("Allocation training history violates primary timing or minimum length.")
    }
  }
  ## Saved MM objects contain the ensemble and linear benchmarks. RW is a
  ## benchmark method within every object, so retaining it once avoids treating
  ## byte-identical benchmark copies as distinct portfolio strategies.
  models <- c("ew", "har", "harx")
  versions <- c("raw", "capped")
  scaler_checks <- list()

  ## Implementation data retain gross returns, modeled costs, and exposures in
  ## addition to net returns. The descriptive Cederburg and MVO tables need
  ## these fields, whereas paired bootstrap inference only needs net returns.
  single_implementation <- list()
  single <- purrr::map_dfr(c("panel", "spx"), function(universe) {
    mm_path <- file.path(root, universe, "expanding_vol_10bps_60m", "all_ports.rds")
    mm <- read_saved(mm_path)
    # Preserve the native market history alongside the balanced factor panel.
    if (sample_id == "1927") mm <- purrr::map(mm, function(result) {
      result$w_compiled_df <- dplyr::filter(result$w_compiled_df,
        name == "spx_vw" | month_id %in% dates)
      result
    })
    expected_names <- if (universe == "panel") c(clusters$name, "spx_vw") else "spx_vw"
    purrr::iwalk(mm, function(result, model) {
      validate_settings(result$settings, list(normalization = "expanding_vol", min_history = 60L,
        weight_cap = 1.5, cost_per_unit = .001, initial_weight = 0))
      validate_panel(result$w_compiled_df, expected_names, native_market = TRUE)
      # Buyhold and RW copies must match before retaining the ensemble copy.
      benchmark <- function(x) x %>% dplyr::filter(method %in% c("buyhold", "rw"))
      check_portfolio_benchmark_copy(benchmark(mm$ew$w_compiled_df),
        benchmark(result$w_compiled_df), c("method", "name", "month_id"))
    })
    record_settings(mm_path, purrr::map(mm, "settings"))
    ## The unmanaged benchmark is common across forecast models. Extract it
    ## from the ensemble object only after the equality checks above establish
    ## that the duplicated copies agree on every factor-month key.
    buyhold <- mm$ew$w_compiled_df %>% dplyr::filter(method == "buyhold")
    single_implementation[[paste(universe, "buyhold")]] <<- purrr::map_dfr(
      versions, function(version) {
        gross <- paste0("r_exc_fwd1_", version)
        net <- paste0("r_exc_fwd1_net_", version)
        cost <- if (version == "raw") "tc_raw" else "tc"
        turnover <- if (version == "raw") "turnover_raw" else "turnover"
        exposure <- if (version == "raw") "w_raw" else "w_capped"
        buyhold %>% dplyr::transmute(family = "ced_ew", universe, version,
          model = "buyhold", name, month_id, gross_return = .data[[gross]],
          excess_return = .data[[net]], total_return = .data[[net]] + rf,
          turnover = .data[[turnover]], cost = .data[[cost]],
          exposure = .data[[exposure]])
      })
    ## MM rows are the standalone inverse-variance managed portfolios. These
    ## are kept separate from the Cederburg artifacts below because their 1.5
    ## cap and direct managed-return construction differ from the two-sleeve
    ## allocation with a total-exposure cap of five.
    mm_rows <- purrr::map_dfr(models, function(model) {
      x <- mm[[model]]$w_compiled_df
      if (model != "ew") x <- dplyr::filter(x, method == "vol_scaled")
      purrr::map_dfr(versions, function(version) {
        extract_portfolio_inference_returns(x, "mm", version) %>%
          dplyr::transmute(family = "mm", universe, version, name, month_id,
            model = dplyr::if_else(method == "vol_scaled", model, method), excess_return, total_return)
      })
    })
    ## Each Cederburg artifact contains a fixed 50/50 combination (`ew`), a
    ## signed two-sleeve MVO (`mvo`), and its unmanaged-only MVO comparator.
    ## The RW signal is stored in the ensemble artifact because it does not
    ## depend on fitted forecasting-model parameters.
    ced_rows <- purrr::map_dfr(c(models, "rw"), function(model) {
      signal <- if (model == "rw") "rw" else "vol_scaled"
      saved_model <- if (model == "rw") "ew" else model
      path <- file.path(allocation_dir, paste0("cederburg_", saved_model, "_", universe,
                                             "_", signal, "_training_vol.rds"))
      saved <- read_saved(path)
      record_settings(path, saved$settings)
      validate_settings(saved$settings, list(normalization = "training_vol", min_history = 60L,
        source_normalization = "expanding_vol", exposure_cap = 5, cost_per_unit = .001,
        initial_weight = 0, gamma = 5))
      x <- dplyr::filter(saved$weights, name == "spx_vw" | month_id %in% dates)
      validate_panel(x, expected_names, native_market = TRUE)
      ## The primary Cederburg design must use the same current MM scaler on
      ## each holding date. This keyed equality check rules out accidentally
      ## comparing a historical saved normalizer with the current one.
      ced_scaler <- x %>% dplyr::filter(method == "ew") %>%
        dplyr::select(name, month_id, scaler)
      mm_scaler <- mm[[saved_model]]$w_compiled_df %>%
        dplyr::filter(method == signal) %>% dplyr::select(name, month_id, w_raw)
      joined <- dplyr::inner_join(ced_scaler, mm_scaler, by = c("name", "month_id"),
                                 relationship = "one-to-one")
      error <- max(abs(joined$scaler - joined$w_raw))
      if (nrow(joined) != nrow(ced_scaler) || nrow(joined) != nrow(mm_scaler) || error > 1e-12) {
        rlang::abort("MM and Cederburg current scalers disagree on matched holding dates.")
      }
      scaler_checks[[paste(universe, model)]] <<- tibble::tibble(universe, model,
        n_pairs = nrow(joined), max_absolute_difference = error)
      ## Retain the implementation rows required for the two descriptive
      ## tables. The unmanaged-only MVO is taken only from the ensemble file;
      ## its return does not vary with the forecast model and would otherwise
      ## be needlessly duplicated.
      implementation_methods <- x %>% dplyr::filter(
        method %in% c("ew", "mvo") |
          (method == "unmanaged_mvo" & .env$model == "ew"))
      single_implementation[[paste(universe, model)]] <<- purrr::map_dfr(
        versions, function(version) {
          implementation_methods %>% dplyr::transmute(
            family = dplyr::if_else(method == "ew", "ced_ew", "ced_mvo"),
            universe, version, name, month_id,
            model = dplyr::if_else(method == "unmanaged_mvo",
              "unmanaged_mvo_ew", .env$model),
            gross_return = .data[[paste0("gross_", version)]],
            excess_return = .data[[paste0("net_", version)]],
            total_return = .data[[paste0("net_", version)]] + rf,
            turnover = .data[[paste0("turnover_", version)]],
            cost = .data[[paste0("tc_", version)]],
            exposure = .data[[paste0("w_", version)]])
        })
      ## Return-only rows feed paired comparisons. The implementation table
      ## above instead retains the factor-level costs and exposures needed for
      ## descriptive statistics.
      purrr::map_dfr(versions, function(version) {
        extract_portfolio_inference_returns(x, "allocation", version) %>%
          dplyr::transmute(family = dplyr::if_else(method == "ew", "ced_ew", "ced_mvo"),
            universe, version, name, month_id,
            model = dplyr::if_else(method == "unmanaged_mvo", paste0("unmanaged_mvo_", model), model),
            excess_return, total_return)
      })
    })
    # The 50/50 extension compares with buyhold. Signed MVO keeps each file's
    # own unmanaged MVO comparator because its available training rows can vary.
    dplyr::bind_rows(mm_rows, ced_rows,
      mm_rows %>% dplyr::filter(model == "buyhold") %>% dplyr::mutate(family = "ced_ew"))
  ## The panel artifact includes both nonmarket factors and `spx_vw`. Preserve
  ## the pooled market as its own scope before forming the equal-weighted
  ## nonmarket-factor aggregate; it is not part of the 153-factor portfolio.
  }) %>% dplyr::mutate(scope = dplyr::case_when(universe == "spx" ~ "market_separate",
    name == "spx_vw" ~ "market_pooled", TRUE ~ "aggregate_153"))
  individual <- single %>% dplyr::filter(scope == "aggregate_153") %>%
    dplyr::left_join(clusters, by = "name", relationship = "many-to-one")
  aggregated <- single %>% dplyr::group_by(family, scope, version, model, month_id) %>%
    dplyr::summarise(excess_return = mean(excess_return), total_return = mean(total_return),
                     n_factors = dplyr::n(), .groups = "drop")
  if (any(aggregated$n_factors != ifelse(aggregated$scope == "aggregate_153", 153L, 1L))) {
    rlang::abort("Unexpected single-factor aggregate membership.")
  }

  multifactor <- purrr::map_dfr(c("ew", "hew"), function(allocator) {
    reference_umv <- reference_lagged <- NULL
    all_models <- if (sample_id == "1986")
      c("ew", "har", "harx", "glmnet", "rf", "xgb", "nn1", "nn2", "nn3") else models
    purrr::map_dfr(all_models, function(model) {
      purrr::map_dfr(c("cmv_panel", "cmv_spx", "fcmv", "lagged_market"), function(specification) {
        path <- file.path(allocation_dir, paste0("multifactor_", model, "_", specification,
                                               "_", allocator, ".rds"))
        saved <- read_saved(path)
        record_settings(path, saved$settings)
        validate_settings(saved$settings, list(allocation = allocator, min_history = 60L,
          managed_normalization = "training_vol", exposure_cap = 1.5, cost_per_unit = .001,
          initial_weight = 0, gamma = 5))
        validate_panel(saved$weights, c(clusters$name, "spx_vw"))
        x <- saved$monthly_returns
        umv <- dplyr::filter(x, method == "umv")
        if (is.null(reference_umv)) reference_umv <<- umv else
          check_portfolio_benchmark_copy(reference_umv, umv, c("method", "month_id"))
        if (specification == "lagged_market") {
          lagged <- dplyr::filter(x, method != "umv")
          if (is.null(reference_lagged)) reference_lagged <<- lagged else
            check_portfolio_benchmark_copy(reference_lagged, lagged, c("method", "month_id"))
        }
        if (!model %in% models || (specification == "lagged_market" && model != "ew")) return(NULL)
        # UMV is retained once per allocator, after all-copy equality checks.
        if (!(model == "ew" && specification == "cmv_panel")) x <- dplyr::filter(x, method != "umv")
        purrr::map_dfr(versions, function(version) {
          extract_portfolio_inference_returns(x, "allocation", version) %>%
            dplyr::transmute(family = paste0("multi_", allocator), scope = "all_154", version,
              model = dplyr::case_when(method == "umv" ~ "umv",
                specification == "lagged_market" ~ "lagged_market",
                TRUE ~ paste(specification, model, sep = "_")),
              month_id, excess_return, total_return, n_factors = 154L)
        })
      })
    })
  })
  returns <- dplyr::bind_rows(aggregated, multifactor) %>%
    tidyr::unite("series_id", family, scope, version, model, remove = FALSE, sep = "/")
  # Compare strategies on a complete common calendar within each scope. The
  # long-sample market starts earlier than the balanced 153-factor aggregate.
  purrr::walk(split(returns, returns$scope), align_portfolio_inference)
  ## Aggregate factor-level implementations month by month before computing
  ## nonlinear performance statistics. This produces the return of an
  ## equal-weighted factor portfolio, rather than an average of individual
  ## factor Sharpe ratios, and maintains a consistent cost/exposure convention.
  implementation <- dplyr::bind_rows(single_implementation) %>%
    dplyr::mutate(scope = dplyr::case_when(
      universe == "spx" ~ "market_separate",
      name == "spx_vw" ~ "market_pooled",
      TRUE ~ "aggregate_153")) %>%
    dplyr::group_by(family, scope, version, model, month_id) %>%
    dplyr::summarise(
      dplyr::across(c(gross_return, excess_return, total_return,
                      turnover, cost, exposure), mean),
      n_factors = dplyr::n(), .groups = "drop")
  ## The accounting identity is checked after aggregation, because it is the
  ## exact return convention shown in the reported gross and net tables.
  expected_implementation_n <- ifelse(implementation$scope == "aggregate_153", 153L, 1L)
  if (any(implementation$n_factors != expected_implementation_n) ||
      anyDuplicated(implementation[c("family", "scope", "version", "model", "month_id")]) ||
      any(abs(implementation$gross_return - implementation$cost -
              implementation$excess_return) > 1e-12)) {
    rlang::abort("Unexpected single-factor implementation aggregation.")
  }
  contrasts <- returns %>% dplyr::distinct(family, scope, version, model) %>%
    dplyr::filter(model %in% c("ew", "cmv_panel_ew", "cmv_spx_ew", "fcmv_ew")) %>%
    purrr::pmap_dfr(function(family, scope, version, model) {
      benchmarks <- if (family == "ced_mvo") c("unmanaged_mvo_ew", "rw", "har", "harx") else
        if (startsWith(family, "multi_")) {
          prefix <- sub("_ew$", "", model)
          # No own-factor lagged-RV FCMV backtest exists. The historical CMV
          # comparator is included only for CMV, with its actual market signal.
          c("umv", if (prefix != "fcmv") "lagged_market", paste0(prefix, c("_har", "_harx")))
        } else c("buyhold", "rw", "har", "harx")
      scope_family <- if (scope == "market_separate" || model == "cmv_spx_ew") "standalone_market" else
        if (family == "ced_mvo") "signed_cederburg" else
          if (startsWith(family, "multi_")) "multifactor_primary" else "single_factor_primary"
      portfolio_family_id <- family
      tibble::tibble(comparison_id = paste(portfolio_family_id, scope, version, model, benchmarks, sep = "/"),
        family = scope_family, portfolio_family = portfolio_family_id, scope, version, forecast = model,
        comparator = benchmarks,
        strategy = paste(portfolio_family_id, scope, version, model, sep = "/"),
        benchmark = paste(portfolio_family_id, scope, version, benchmarks, sep = "/"))
    })
  ## Verify the upstream provenance chain without rebuilding cached forecasts.
  ## Current source files and recorded hashes are retained alongside results.
  manifest_paths <- c(file.path(allocation_dir, "input_manifest.csv"),
    file.path(root, c("panel", "spx"), "expanding_vol_10bps_60m", "input_manifest.csv"))
  provenance <- check_portfolio_inference_provenance(manifest_paths)
  ensemble_checks <- purrr::map_dfr(c("panel", "spx"), function(universe) {
    path <- file.path(project_dir, "models_v3", sample_id, "ensembles",
                      paste0("ew_", universe, "_range_grid_", sample_id, ".rds"))
    cached <- read_saved(path)
    # Standalone forecasts have one target and are keyed only by month. Attach
    # its known economic identifier explicitly for the common audit join.
    if (universe == "spx") cached$oos_outputs <- dplyr::mutate(cached$oos_outputs, name = "spx_vw")
    weights <- cached$backtest_meta$weights
    sources <- cached$backtest_meta$sources
    members <- c("harx", "glmnet", "rf", "xgb", "nn1", "nn2", "nn3")
    if (nrow(weights) != 7L || nrow(sources) != 7L || anyDuplicated(weights$model) ||
        anyDuplicated(sources$model) || !setequal(weights$model, members) ||
        !setequal(sources$model, members) || any(abs(weights$weight - 1 / 7) > 1e-15)) {
      rlang::abort("Unexpected cached ensemble membership or weights.")
    }
    predictions <- purrr::pmap_dfr(sources, function(model, file, sha256) {
      member_path <- file.path(project_dir, "models_v3", sample_id, file)
      if (!identical(digest::digest(file = member_path, algo = "sha256"), sha256)) {
        rlang::abort("Cached ensemble member hash mismatch.")
      }
      result <- readRDS(member_path)
      prediction <- methods::slot(result, "oos_outputs")
      if (universe == "spx") prediction <- dplyr::mutate(prediction, name = "spx_vw")
      if (anyDuplicated(prediction[c("name", "month_id")]) ||
          any(!is.finite(prediction$pred))) rlang::abort("Invalid cached ensemble member keys or forecasts.")
      dplyr::mutate(prediction, model = model)
    }) %>% dplyr::group_by(name, month_id) %>%
      dplyr::summarise(member_mean = mean(pred), n_members = dplyr::n(), .groups = "drop")
    joined <- dplyr::full_join(cached$oos_outputs, predictions,
      by = c("name", "month_id"), relationship = "one-to-one")
    if (anyNA(joined[c("pred", "member_mean", "n_members")]) || any(joined$n_members != 7L) ||
        any(abs(joined$pred - joined$member_mean) > 1e-12)) {
      rlang::abort("Cached ensemble is not the complete seven-member mean on the log scale.")
    }
    tibble::tibble(universe, n_predictions = nrow(joined), n_members = 7L,
      max_absolute_difference = max(abs(joined$pred - joined$member_mean)),
      convention = "Arithmetic average of log forecasts, HAR excluded")
  })
  paths <- unique(c(paths, manifest_paths, file.path(project_dir, "data", "cluster_labels.csv")))
  list(returns = returns, individual = individual, implementation = implementation,
    contrasts = contrasts,
    manifest = tibble::tibble(file = paths, sha256 = purrr::map_chr(paths,
      ~ digest::digest(file = .x, algo = "sha256"))),
    effective_settings = settings, scaler_checks = dplyr::bind_rows(scaler_checks),
    provenance_checks = provenance, ensemble_checks = ensemble_checks)
}
