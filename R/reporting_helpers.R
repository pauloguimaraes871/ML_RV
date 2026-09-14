# Forecast reporting helpers----------------------------------------------------

  `%>%` <- dplyr::`%>%`

#' Validate a forecast table before aggregation
#'
#' Requires finite numeric log forecasts and targets, first-of-month Date values,
#' and unique, nonmissing observation keys. Does not repair or discard rows.
validate_forecasts <- function(data, keys = "month_id") {
  required <- unique(c(keys, "month_id", "target", "pred"))
  if (!is.data.frame(data) || !all(required %in% names(data))) {
    rlang::abort(paste("Forecast table requires:", paste(required, collapse = ", ")))
  }
  if (nrow(data) == 0L) rlang::abort("Forecast table is empty.")
  if (!inherits(data$month_id, "Date") || anyNA(data$month_id) ||
      any(format(data$month_id, "%d") != "01")) {
    rlang::abort("month_id must contain nonmissing first-of-month Date values.")
  }
  if (anyNA(data[keys]) || any(vapply(data[keys], function(x)
    is.character(x) && any(!nzchar(x)), logical(1)))) {
    rlang::abort("Forecast keys must be nonmissing and nonempty.")
  }
  if (anyDuplicated(data[keys])) rlang::abort("Duplicate forecast keys.")
  if (!is.numeric(data$target) || !is.numeric(data$pred)) {
    rlang::abort("Forecasts and targets must be numeric.")
  }
  if (any(!is.finite(data$target)) || any(!is.finite(data$pred))) {
    rlang::abort("Forecasts and targets must be finite.")
  }
  invisible(data)
}

#' Apply the estimation engine's metrics to all rows or explicit groups
#'
#' No averaging of precomputed RMSEs or R-squared values. Units remain those of
#' the input forecasts. Constant/single-observation targets retain undefined R2.
summarise_forecasts <- function(data, groups = character(), meta = list()) {
  score <- function(x) tibble::as_tibble(calc_eval_metrics(
    pred = x$pred, target = x$target,
    huber_delta = if (is.null(meta$huber_delta)) 1 else meta$huber_delta,
    quantile_tau = if (is.null(meta$quantile_tau)) 0.5 else meta$quantile_tau,
    eval_metric = if (is.null(meta$eval_metric)) "rmse" else meta$eval_metric
  )) %>% dplyr::select(-dplyr::all_of("Score"))
  if (!all(groups %in% names(data))) rlang::abort("Missing scoring group columns.")
  if (nrow(data) == 0L || any(!is.finite(data$target)) || any(!is.finite(data$pred))) {
    rlang::abort("Scoring requires nonempty finite forecasts and targets.")
  }
  if (!length(groups)) return(score(data))
  data %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(groups))) %>%
    dplyr::group_modify(~ score(.x)) %>%
    dplyr::ungroup()
}

#' Read and audit a saved backtest without loading its estimation engines
#'
#' Returns a lightweight list: stored forecasts, metrics, tuning and importance
#' summaries, provenance, and final-surrogate diagnostics. Fitted objects are
#' discarded after extraction. The original RDS file is never modified.
read_model_result <- function(path, data_dir) {
  object <- readRDS(path)
  if (!methods::is(object, "wf_backtest_results")) rlang::abort("Expected wf_backtest_results.")
  meta <- methods::slot(object, "backtest_meta")
  provenance <- meta$provenance
  if (is.null(provenance$model_id) || is.null(provenance$universe) ||
      !identical(compute_run_signature(provenance), provenance$run_signature)) {
    rlang::abort(paste("Missing or inconsistent provenance:", basename(path)))
  }
  for (kind in c("covariates", "target")) {
    artifact <- provenance[[paste0(kind, "_artifact")]]
    if (is.null(artifact) || !identical(
      hash_artifact(file.path(data_dir, artifact)), provenance[[paste0(kind, "_sha256")]])) {
      rlang::abort(paste("Dataset hash mismatch:", basename(path), kind))
    }
  }
  fields <- c("oos_outputs", "eval_metrics", "hyper_choice", "eval_metric_val",
              "val_eval_metrics_hyper_choice", "feat_imp", "final_feat_imp")
  result <- stats::setNames(purrr::map(fields, ~ methods::slot(object, .x)), fields)
  keys <- c("month_id", meta$key_col)
  validate_forecasts(result$oos_outputs, keys)
  if (!identical(sort(unique(result$oos_outputs$month_id)), meta$dates_test)) {
    rlang::abort("Saved forecast dates disagree with metadata.")
  }
  observed <- result$oos_outputs
  target <- readRDS(file.path(data_dir, provenance$target_artifact))
  if (!identical(provenance$target_transform, "boxcox_apply(lambda=0, offset=1e-08)")) {
    rlang::abort("Reporting currently supports the recorded V3 log target only.")
  }
  if (anyDuplicated(target[keys])) rlang::abort("Duplicate keys in the target artifact.")
  checked <- observed %>%
    dplyr::left_join(target, by = keys, relationship = "one-to-one")
  if (anyNA(checked$rv_month) ||
      !isTRUE(all.equal(checked$target, boxcox_apply(checked$rv_month, 0, 1e-8),
                        tolerance = 1e-12))) rlang::abort("Stored targets disagree with target artifact.")
  recalculated <- summarise_forecasts(observed, meta = meta) %>%
    tidyr::pivot_longer(dplyr::everything(), names_to = "metric", values_to = "recalculated")
  if (anyDuplicated(result$eval_metrics$metric) ||
      !setequal(result$eval_metrics$metric, recalculated$metric)) {
    rlang::abort("Saved metric names are incomplete or duplicated.")
  }
  comparison <- result$eval_metrics %>%
    dplyr::left_join(recalculated, by = "metric", relationship = "one-to-one")
  if (!isTRUE(all.equal(comparison$cons_oos, comparison$recalculated, tolerance = 1e-12)) ||
      !isTRUE(all.equal(observed$error, observed$target - observed$pred, tolerance = 1e-12))) {
    rlang::abort(paste("Stored metrics/errors do not reproduce:", basename(path)))
  }
  # Register xts methods before dispatching zoo::index() on a deserialized slot.
  base::loadNamespace("xts")
  if (!is.null(result$hyper_choice)) {
    if (!identical(as.numeric(zoo::index(result$hyper_choice)), as.numeric(meta$rebal_dates)) ||
        !identical(as.numeric(zoo::index(result$val_eval_metrics_hyper_choice)), as.numeric(meta$rebal_dates))) {
      rlang::abort("Tuning dates disagree with metadata.")
    }
    val <- result$val_eval_metrics_hyper_choice
    if (any(!is.finite(val)) || any(!is.finite(result$hyper_choice))) {
      rlang::abort("Nonfinite selected validation scores or hyperparameters.")
    }
    minima <- purrr::map_dbl(result$eval_metric_val, function(grid) {
      if (any(!is.finite(grid$eval_metric))) rlang::abort("Nonfinite tuning score.")
      min(grid$eval_metric)
    })
    if (!identical(meta$eval_metric, "rmse") || length(minima) != nrow(val) ||
        !isTRUE(all.equal(as.numeric(val[, "rmse"]), minima, check.attributes = FALSE,
                          tolerance = 1e-12))) rlang::abort("Selected RMSE does not match grid minimum.")
    val_summary <- tibble::tibble(metric = colnames(val), checked_val = as.numeric(colMeans(val)))
    val_summary <- dplyr::left_join(result$eval_metrics, val_summary, by = "metric",
                                    relationship = "one-to-one")
    if (!isTRUE(all.equal(val_summary$avg_val, val_summary$checked_val, tolerance = 1e-12))) {
      rlang::abort("Stored average validation metrics do not reproduce.")
    }
  }
  gsm <- methods::slot(object, "final_gsm")
  coefficients <- stats::coef(gsm)
  result$final_aliases <- gsub("`", "", names(coefficients)[is.na(coefficients)])
  result$surrogate_r2 <- summary(gsm)$r.squared
  result$serialized_members <- NA_integer_
  if (meta$model == "nn") {
    blobs <- attr(methods::slot(methods::slot(object, "final_rv_model"), "fit_obj"), "serialized")
    result$serialized_members <- sum(purrr::map_lgl(blobs, is.raw))
  }
  result$backtest_meta <- meta
  result$source <- tibble::tibble(file = basename(path), sha256 = hash_artifact(path))
  result
}

#' Equal-weight HARX and ML log forecasts on identical target-month coverage
#'
#' Returns oos_outputs, eval_metrics and backtest_meta as a plain list; no fitted
#' model, hyperparameters or validation forecasts are invented. Each named model
#' receives one vote, regardless of its internal ensemble size. HAR is excluded.
build_ew_ensemble <- function(models,
                              members = c("harx", "glmnet", "rf", "xgb", "nn1", "nn2", "nn3")) {
  
  if (!is.character(members) || !length(members) || anyNA(members) || anyDuplicated(members)) {
    rlang::abort("Ensemble members must be nonempty and unique.")
  }
  if ("har" %in% members) rlang::abort("HAR must not enter this ensemble.")
  if (is.null(names(models)) || anyDuplicated(names(models))) rlang::abort("Models need unique names.")
  missing <- setdiff(members, names(models))
  if (length(missing)) rlang::abort(paste("Missing ensemble members:", paste(missing, collapse = ", ")))
  reference <- models[[members[1]]]
  meta <- reference$backtest_meta
  keys <- c("month_id", meta$key_col)
  validate_forecasts(reference$oos_outputs, keys)
  fields <- c("universe", "target_transform", "preprocessing")
  purrr::walk(models[members], function(model) {
    other <- model$backtest_meta
    if (!identical(meta$key_col, other$key_col) ||
        !identical(meta[c("eval_metric", "huber_delta", "quantile_tau")],
                   other[c("eval_metric", "huber_delta", "quantile_tau")]) ||
        !identical(meta$provenance[fields], other$provenance[fields])) {
      rlang::abort("Ensemble members have incompatible scoring or provenance.")
    }
    validate_forecasts(model$oos_outputs, keys)
    paired <- reference$oos_outputs %>%
      dplyr::select(dplyr::all_of(keys), target) %>%
      dplyr::full_join(dplyr::select(model$oos_outputs, dplyr::all_of(keys), target),
                       by = keys, suffix = c("_reference", "_member"), relationship = "one-to-one")
    if (anyNA(paired)) rlang::abort("Ensemble members have different forecast coverage.")
    if (any(paired$target_reference != paired$target_member)) {
      rlang::abort("Ensemble members have different target values.")
    }
  })
  oos <- purrr::map_dfr(models[members], "oos_outputs", .id = "model") %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(keys))) %>%
    dplyr::summarise(target = dplyr::first(target), pred = mean(pred), .groups = "drop") %>%
    dplyr::arrange(dplyr::across(dplyr::all_of(keys))) %>%
    dplyr::mutate(error = target - pred)
  evaluation <- summarise_forecasts(oos, meta = meta) %>%
    tidyr::pivot_longer(dplyr::everything(), names_to = "metric", values_to = "cons_oos")
  list(oos_outputs = oos, eval_metrics = evaluation,
       backtest_meta = list(
         model = "ew", key_col = meta$key_col, eval_metric = meta$eval_metric,
         huber_delta = meta$huber_delta, quantile_tau = meta$quantile_tau,
         provenance = c(meta$provenance[fields], list(model_id = "ew")),
         weights = tibble::tibble(model = members, weight = 1 / length(members)),
         dates_test = sort(unique(oos$month_id)),
         description = "Equal arithmetic mean of seven model log forecasts; HAR excluded."
       ))
}

#' Assign each test month to its most recent fitted model
assign_refit_dates <- function(data, rebal_dates) {
  if (!inherits(rebal_dates, "Date") || !length(rebal_dates) || anyNA(rebal_dates) ||
      is.unsorted(rebal_dates, strictly = TRUE)) rlang::abort("Refit dates must be unique and sorted.")
  if (any(data$month_id < min(rebal_dates))) rlang::abort("Forecast occurs before first refit.")
  data %>% dplyr::mutate(refit_date = rebal_dates[findInterval(month_id, rebal_dates)])
}

#' Trailing pooled RMSE over a complete calendar window
#'
#' A missing calendar month or incomplete starting window returns NA; no padding
#' or imputation. Pool target-month squared errors before taking the square root.
rolling_forecast_rmse <- function(data, groups, window = 12L) {
  if (length(window) != 1L || !is.finite(window) || window < 1 || window != as.integer(window)) {
    rlang::abort("Rolling window must be a positive integer.")
  }
  by_month <- data %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(c(groups, "month_id")))) %>%
    dplyr::summarise(sse = sum((target - pred)^2), n = dplyr::n(), .groups = "drop")
  roll <- function(x) {
    x <- dplyr::arrange(x, month_id)
    month_number <- as.integer(format(x$month_id, "%Y")) * 12L +
      as.integer(format(x$month_id, "%m"))
    x$rmse <- purrr::map_dbl(seq_len(nrow(x)), function(i) {
      positions <- which(month_number <= month_number[i] & month_number > month_number[i] - window)
      if (length(positions) != window) return(NA_real_)
      sqrt(sum(x$sse[positions]) / sum(x$n[positions]))
    })
    x
  }
  if (!length(groups)) return(roll(by_month))
  by_month %>% dplyr::group_by(dplyr::across(dplyr::all_of(groups))) %>%
    dplyr::group_modify(~ roll(.x)) %>% dplyr::ungroup()
}

#' Explicit themes for every predictor used by the V3 1986 specification
report_theme_map <- function() {
  families <- c("rv", "rq", "cont", "jump", "negret")
  tibble::tibble(
    feat = c(unlist(purrr::map(families, ~ paste0(.x, c("_month_lag1", "_tw_lag1", "_td_lag1")))),
             "rv_level_lag1", "log_epu_diff_lag1", "gpr_lag1", "log_cons_sent_diff_lag1",
             "ppi_vol", "ip_vol", "mbas_vol", "dy_lag1", "ey_lag1", "dfr_lag1",
             "dfy_lag1", "tms_lag1", "tbill_lag1", "vix_lag1",
             "oil_rv_month_lag1", "oil_rv_tw_lag1", "oil_rv_td_lag1", "sqrt_days", paste0("m_", 1:12)),
    theme = c(rep("Own-series RV terms", 15), "Long-run RV level", rep("Sentiment", 3),
              rep("Macroeconomic", 3), rep("Macrofinancial", 6), "Implied variance",
              rep("Oil RV", 3), rep("Calendar", 13))
  )
}

#' Descriptive signed and gross shares of stored OLS surrogate coefficients
#'
#' Excludes the intercept and ignores the stored norm_imp. Uses the reported
#' coefficient parameterization; zero/aliased terms contribute zero, not evidence
#' of irrelevance. Different predictor scales and aliases limit interpretation.
summarise_theme_loadings <- function(importance, theme_map) {
  if (anyDuplicated(theme_map$feat)) rlang::abort("Theme map requires unique features.")
  importance <- importance %>% dplyr::filter(feat != "(Intercept)")
  missing <- setdiff(importance$feat, theme_map$feat)
  if (length(missing)) rlang::abort(paste("Unmapped features:", paste(missing, collapse = ", ")))
  if (any(!is.finite(importance$imp))) rlang::abort("Surrogate coefficients must be finite.")
  importance %>%
    dplyr::left_join(theme_map, by = "feat", relationship = "many-to-one") %>%
    dplyr::group_by(universe, model, dates) %>%
    dplyr::mutate(denominator = sum(abs(imp))) %>%
    dplyr::group_by(universe, model, dates, theme) %>%
    dplyr::summarise(
      gross_share = if (dplyr::first(denominator) == 0) NA_real_ else sum(abs(imp)) / dplyr::first(denominator),
      net_share = if (dplyr::first(denominator) == 0) NA_real_ else sum(imp) / dplyr::first(denominator),
      .groups = "drop"
    )
}

# LaTeX output------------------------------------------------------------------

escape_report_tex <- function(x) {
  x <- as.character(x)
  # Labels are data, never executable LaTeX. Escape backslashes before specials.
  x <- gsub("\\", "\\textbackslash{}", x, fixed = TRUE)
  for (symbol in c("&", "%", "$", "#", "_", "{", "}")) {
    x <- gsub(symbol, paste0("\\", symbol), x, fixed = TRUE)
  }
  x <- gsub("~", "\\textasciitilde{}", x, fixed = TRUE)
  gsub("^", "\\textasciicircum{}", x, fixed = TRUE)
}

format_report_caption <- function(caption) {
  if (!is.character(caption) || length(caption) != 1L || is.na(caption) || !nzchar(trimws(caption))) {
    rlang::abort("A report caption must be one nonempty string.")
  }
  title_end <- regexpr(".", caption, fixed = TRUE)[1]
  if (title_end < 1L) {
    title <- trimws(caption)
    note <- ""
  } else {
    title <- trimws(substr(caption, 1L, title_end))
    note <- trimws(substr(caption, title_end + 1L, nchar(caption)))
  }
  title_tex <- escape_report_tex(title)
  note_tex <- escape_report_tex(note)
  list(
    short = title_tex,
    full = paste0("\\footnotesize \\textbf{: ", title_tex, "}",
                  if (nzchar(note_tex)) paste0(" ", note_tex) else "")
  )
}

write_report_table <- function(data, path, caption, label, long = FALSE, digits = 4) {
  printable <- purrr::map(data, function(x) {
    if (is.numeric(x)) ifelse(is.na(x), "--", formatC(x, format = "f", digits = digits))
    else ifelse(is.na(x), "--", escape_report_tex(x))
  }) %>% tibble::as_tibble()
  rows <- purrr::pmap_chr(printable, ~ paste0(paste(c(...), collapse = " & "), " \\\\"))
  heading <- paste0(paste(escape_report_tex(names(data)), collapse = " & "), " \\\\")
  spec <- paste0("l", paste(rep("r", ncol(data) - 1), collapse = ""))
  caption_tex <- format_report_caption(caption)
  caption_setup <- "\\captionsetup{justification=raggedright,labelsep=none,singlelinecheck=false,labelformat=boldlabel}"
  if (long) {
    lines <- c("\\begingroup\\scriptsize", caption_setup, "\\setlength{\\tabcolsep}{3pt}",
      paste0("\\begin{longtable}{", spec, "}"),
      paste0("\\caption[", caption_tex$short, "]{", caption_tex$full, "}\\label{", label, "}\\\\"),
      "\\hline", heading, "\\hline\\endfirsthead", "\\hline", heading,
      "\\hline\\endhead", "\\hline\\endfoot", rows, "\\end{longtable}", "\\endgroup")
  } else {
    lines <- c("\\begin{table}[htbp]", "\\centering\\footnotesize",
      caption_setup, "\\renewcommand{\\arraystretch}{1.15}", "\\setlength{\\tabcolsep}{4pt}",
      paste0("\\caption[", caption_tex$short, "]{", caption_tex$full, "}\\label{", label, "}"),
      paste0("\\begin{tabular}{", spec, "}"), "\\hline", heading, "\\hline", rows,
      "\\hline", "\\end{tabular}", "\\end{table}")
  }
  writeLines(lines, path, useBytes = TRUE)
}

write_report_panel_table <- function(panels, path, caption, label, digits = 4) {
  if (!is.list(panels) || !length(panels) || is.null(names(panels)) || any(!nzchar(names(panels)))) {
    rlang::abort("Panel tables require a named, nonempty list of data frames.")
  }
  if (any(!purrr::map_lgl(panels, is.data.frame))) rlang::abort("Every panel must be a data frame.")
  reference_names <- names(panels[[1]])
  if (any(!purrr::map_lgl(panels, ~ identical(names(.x), reference_names)))) {
    rlang::abort("Every panel must have the same columns in the same order.")
  }
  format_panel <- function(data, panel_name) {
    printable <- purrr::map(data, function(x) {
      if (is.numeric(x)) ifelse(is.na(x), "--", formatC(x, format = "f", digits = digits))
      else ifelse(is.na(x), "--", escape_report_tex(x))
    }) %>% tibble::as_tibble()
    rows <- purrr::pmap_chr(printable, ~ paste0(paste(c(...), collapse = " & "), " \\\\"))
    heading <- paste0(paste(escape_report_tex(names(data)), collapse = " & "), " \\\\")
    c(paste0("\\multicolumn{", ncol(data), "}{@{}l}{\\textit{", escape_report_tex(panel_name), "}} \\\\"),
      "\\hline", heading, "\\hline", rows, "\\hline")
  }
  caption_tex <- format_report_caption(caption)
  spec <- paste0("l", paste(rep("r", ncol(panels[[1]]) - 1L), collapse = ""))
  lines <- c("\\begin{table}[p]", "\\centering\\footnotesize",
    "\\captionsetup{justification=raggedright,labelsep=none,singlelinecheck=false,labelformat=boldlabel}",
    "\\renewcommand{\\arraystretch}{1.1}", "\\setlength{\\tabcolsep}{4pt}",
    paste0("\\caption[", caption_tex$short, "]{", caption_tex$full, "}\\label{", label, "}"),
    paste0("\\begin{tabular}{", spec, "}"),
    unlist(purrr::imap(panels, format_panel), use.names = FALSE),
    "\\end{tabular}", "\\end{table}")
  writeLines(lines, path, useBytes = TRUE)
}

write_cluster_metric_table <- function(data, path, caption, label,
                                       direction = c("min", "max"), digits = 4) {
  direction <- match.arg(direction)
  required <- c("cluster", "Factor", "row_type")
  if (!is.data.frame(data) || !all(required %in% names(data))) {
    rlang::abort("Cluster metric tables require cluster, Factor, and row_type columns.")
  }
  model_columns <- setdiff(names(data), required)
  if (!length(model_columns) || !"HAR" %in% model_columns ||
      any(!purrr::map_lgl(data[model_columns], is.numeric))) {
    rlang::abort("Cluster metric tables require numeric model columns including HAR.")
  }
  if (anyNA(data[c("cluster", "Factor", "row_type")]) || anyDuplicated(data[c("cluster", "Factor")])) {
    rlang::abort("Cluster metric table keys must be complete and unique.")
  }
  caption_tex <- format_report_caption(caption)
  heading <- paste0(paste(c("Factor", model_columns), collapse = " & "), " \\\\")
  clusters <- unique(data$cluster)
  panel_letters <- LETTERS[seq_along(clusters)]
  body <- purrr::map2(clusters, panel_letters, function(cluster_name, panel_letter) {
    panel <- dplyr::filter(data, cluster == cluster_name)
    rows <- purrr::map_chr(seq_len(nrow(panel)), function(index) {
      values <- as.numeric(panel[index, model_columns])
      benchmark <- values[match("HAR", model_columns)]
      best <- if (direction == "min") min(values, na.rm = TRUE) else max(values, na.rm = TRUE)
      cells <- purrr::map2_chr(values, model_columns, function(value, model) {
        if (!is.finite(value)) return("--")
        cell <- formatC(value, format = "f", digits = digits)
        beats_har <- if (direction == "min") value < benchmark else value > benchmark
        if (beats_har) cell <- paste0("\\cellcolor{blue!10}", cell)
        if (isTRUE(all.equal(value, best, tolerance = 1e-12))) cell <- paste0("\\textbf{", cell, "}")
        cell
      })
      factor_label <- escape_report_tex(panel$Factor[index])
      if (panel$row_type[index] == "aggregate") factor_label <- paste0("\\textbf{", factor_label, "}")
      paste0(paste(c(factor_label, cells), collapse = " & "), " \\\\")
    })
    c(paste0("\\multicolumn{", length(model_columns) + 1L,
             "}{@{}l}{\\textit{Panel ", panel_letter, ". ", escape_report_tex(cluster_name), "}} \\\\"),
      "\\hline", heading, "\\hline", rows, "\\hline")
  })
  spec <- paste0(">{\\raggedright\\arraybackslash}p{2.6cm}",
                 paste(rep("r", length(model_columns)), collapse = ""))
  lines <- c("\\begingroup\\scriptsize",
    "\\captionsetup{justification=raggedright,labelsep=none,singlelinecheck=false,labelformat=boldlabel}",
    "\\setlength{\\tabcolsep}{2.5pt}", paste0("\\begin{longtable}{", spec, "}"),
    paste0("\\caption[", caption_tex$short, "]{", caption_tex$full, "}\\label{", label, "}\\\\"),
    "\\hline", heading, "\\hline\\endfirsthead", "\\hline", heading,
    "\\hline\\endhead", "\\hline\\endfoot", unlist(body, use.names = FALSE),
    "\\end{longtable}", "\\endgroup")
  writeLines(lines, path, useBytes = TRUE)
}
