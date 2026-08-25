# V3 variable diagnostics-------------------------------------------------------

  ## This script is intentionally separate from fit_models.R. It reads raw V3
  ## artifacts, produces summaries and plots, and never fits or alters a model.
  `%>%` <- dplyr::`%>%`
  source(file.path(here::here(), "R", "helpers.R"))

  project_dir <- here::here()
  output_dir <- file.path(project_dir, "diagnostics_v3")
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  if (.Platform$OS.type == "windows") {
    grDevices::windowsFonts(Helvetica = grDevices::windowsFont("Arial"))
  }

  input_paths <- c(
    panel_1927 = file.path(project_dir, "data", "covariates_panel_1927_v3.rds"),
    panel_1986 = file.path(project_dir, "data", "covariates_panel_1986_v3.rds"),
    target_spx = file.path(project_dir, "data", "target_spx_v3.rds")
  )

  missing_inputs <- input_paths[!file.exists(input_paths)]
  if (length(missing_inputs) > 0L) {
    stop("Missing required V3 diagnostic input(s):\n",
         paste(missing_inputs, collapse = "\n"))
  }

  panel_1927 <- readRDS(input_paths[["panel_1927"]])
  panel_1986 <- readRDS(input_paths[["panel_1986"]])
  target_spx <- readRDS(input_paths[["target_spx"]])


# Validation and subsets--------------------------------------------------------

  validate_panel <- function(panel, label) {
    required <- c("name", "month_id")
    missing <- setdiff(required, names(panel))
    if (length(missing) > 0L) {
      stop(label, " is missing required column(s): ",
           paste(missing, collapse = ", "))
    }
    if (anyDuplicated(panel[c("name", "month_id")])) {
      stop(label, " contains duplicate (name, month_id) keys.")
    }
    if (!"spx_vw" %in% panel$name) {
      stop(label, " does not contain the spx_vw key.")
    }
    invisible(panel)
  }

  validate_panel(panel_1927, "panel_1927")
  validate_panel(panel_1986, "panel_1986")

  spx_from_panel <- function(panel) {
    panel %>%
      dplyr::filter(.data$name == "spx_vw") %>%
      dplyr::select(-dplyr::all_of("name")) %>%
      dplyr::arrange(.data$month_id)
  }

  spx_1927 <- spx_from_panel(panel_1927)
  spx_1986 <- spx_from_panel(panel_1986)


# Summary tables---------------------------------------------------------------

  summary_spx_1927 <- make_var_summary(spx_1927)
  summary_spx_1986 <- make_var_summary(spx_1986)
  summary_target_spx <- make_var_summary(target_spx)

  gt::gtsave(summary_spx_1927,
             file.path(output_dir, "summary_spx_covariates_1927_v3.html"))
  gt::gtsave(summary_spx_1986,
             file.path(output_dir, "summary_spx_covariates_1986_v3.html"))
  gt::gtsave(summary_target_spx,
             file.path(output_dir, "summary_spx_target_v3.html"))

  panel_feature_summary <- function(panel, sample_id) {
    feature_names <- names(panel)[stringr::str_detect(
      names(panel), "^(rv|rq|jump|cont|negret)_"
    )]

    purrr::map_dfr(feature_names, function(feature_name) {
      x <- panel[[feature_name]]
      ok <- is.finite(x)
      tibble::tibble(
        sample = sample_id,
        variable = feature_name,
        n = sum(ok),
        n_missing = sum(!ok),
        mean = mean(x[ok]),
        sd = stats::sd(x[ok]),
        p10 = stats::quantile(x[ok], 0.10, names = FALSE),
        median = stats::median(x[ok]),
        p90 = stats::quantile(x[ok], 0.90, names = FALSE),
        min = min(x[ok]),
        max = max(x[ok])
      )
    })
  }

  panel_features <- dplyr::bind_rows(
    panel_feature_summary(panel_1927, "1927"),
    panel_feature_summary(panel_1986, "1986")
  )

  panel_feature_table <- panel_features %>%
    gt::gt(groupname_col = "sample") %>%
    gt::tab_header(
      title = "V3 panel feature summary",
      subtitle = "Factor-specific HAR features; shared calendar covariates excluded"
    ) %>%
    gt::fmt_number(
      columns = c(mean, sd, p10, median, p90, min, max), decimals = 4
    ) %>%
    gt::fmt_number(columns = c(n, n_missing), decimals = 0, use_seps = TRUE)

  gt::gtsave(panel_feature_table,
             file.path(output_dir, "summary_panel_features_v3.html"))

  panel_coverage <- dplyr::bind_rows(
    panel_1927 %>%
      dplyr::count(.data$month_id, name = "n_keys") %>%
      dplyr::mutate(sample = "1927", .before = 1),
    panel_1986 %>%
      dplyr::count(.data$month_id, name = "n_keys") %>%
      dplyr::mutate(sample = "1986", .before = 1)
  )

  utils::write.csv(
    panel_coverage,
    file.path(output_dir, "panel_coverage_by_month_v3.csv"),
    row.names = FALSE
  )

  coverage_table <- panel_coverage %>%
    dplyr::group_by(.data$sample) %>%
    dplyr::summarize(
      start = min(.data$month_id),
      end = max(.data$month_id),
      months = dplyr::n(),
      min_keys = min(.data$n_keys),
      median_keys = stats::median(.data$n_keys),
      max_keys = max(.data$n_keys),
      .groups = "drop"
    ) %>%
    gt::gt() %>%
    gt::tab_header(
      title = "V3 panel coverage",
      subtitle = "Number of available series by month"
    ) %>%
    gt::fmt_date(columns = c(start, end), date_style = 3) %>%
    gt::fmt_number(columns = c(months, min_keys, median_keys, max_keys),
                   decimals = 0, use_seps = TRUE)

  gt::gtsave(coverage_table,
             file.path(output_dir, "summary_panel_coverage_v3.html"))


# Time-series plots-------------------------------------------------------------

  save_plot <- function(plot, filename, width = 9, height = 6) {
    ggplot2::ggsave(
      filename = file.path(output_dir, filename),
      plot = plot,
      width = width,
      height = height,
      dpi = 300
    )
  }

  plot_family <- function(data, variables, filename, title,
                          ncol = 3, width = 9, height = 6, smooth = TRUE) {
    variables <- intersect(variables, names(data))
    if (length(variables) == 0L) return(invisible(NULL))
    plot <- plot_ts_grid(data, variables, ncol = ncol, title = title,
                         smooth = smooth)
    save_plot(plot, filename, width = width, height = height)
    invisible(plot)
  }

  plot_family(
    target_spx, "rv_month", "fig_spx_target_v3.png",
    "Monthly S&P 500 realized variance", ncol = 1, width = 6, height = 4,
    smooth = FALSE
  )

  feature_families <- c("rv", "rq", "jump", "cont", "negret")
  for (family in feature_families) {
    variables <- stringr::str_subset(names(spx_1927), paste0("^", family, "_"))
    plot_family(
      spx_1927,
      variables,
      paste0("fig_spx_", family, "_1927_v3.png"),
      paste0("S&P 500 ", toupper(family), " measures: 1927-2024")
    )
  }

  plot_family(
    spx_1927, c("ppi_vol", "ip_vol"),
    "fig_spx_macro_1927_v3.png", "Macroeconomic volatility: 1927-2024",
    ncol = 2, width = 7
  )
  plot_family(
    spx_1927, c("log_epu_diff_lag1", "gpr_lag1"),
    "fig_spx_sentiment_1927_v3.png", "Sentiment variables: 1927-2024",
    ncol = 2, width = 7
  )
  plot_family(
    spx_1927, c("dy_lag1", "ey_lag1"),
    "fig_spx_macrofinancial_1927_v3.png", "Macrofinancial variables: 1927-2024",
    ncol = 2, width = 7
  )

  plot_family(
    spx_1986, c("ppi_vol", "ip_vol", "mbas_vol"),
    "fig_spx_macro_1986_v3.png", "Macroeconomic volatility: 1986-2024"
  )
  plot_family(
    spx_1986,
    c("log_epu_diff_lag1", "log_cons_sent_diff_lag1", "gpr_lag1"),
    "fig_spx_sentiment_1986_v3.png", "Sentiment variables: 1986-2024"
  )
  plot_family(
    spx_1986,
    c("dfr_lag1", "dfy_lag1", "tms_lag1", "dy_lag1", "ey_lag1",
      "tbill_lag1"),
    "fig_spx_macrofinancial_1986_v3.png", "Macrofinancial variables: 1986-2024"
  )
  plot_family(
    spx_1986, "vix_lag1", "fig_spx_implied_vol_1986_v3.png",
    "Implied volatility: 1986-2024", ncol = 1, width = 6, height = 4
  )
  plot_family(
    spx_1986,
    c("oil_rv_month_lag1", "oil_rv_tw_lag1", "oil_rv_td_lag1"),
    "fig_spx_oil_rv_1986_v3.png", "Crude-oil realized variance: 1986-2024"
  )

  coverage_plot <- ggplot2::ggplot(
    panel_coverage,
    ggplot2::aes(x = .data$month_id, y = .data$n_keys, colour = .data$sample)
  ) +
    ggplot2::geom_line(linewidth = 0.5) +
    ggplot2::labs(
      x = NULL,
      y = "Available series",
      colour = "Sample",
      title = "V3 panel coverage by month"
    ) +
    ggplot2::theme_minimal(base_family = "Helvetica")

  save_plot(coverage_plot, "fig_panel_coverage_v3.png", width = 8, height = 4)

  cat("Wrote V3 variable diagnostics to:", output_dir, "\n")
