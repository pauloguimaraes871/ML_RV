# Combine the two applications of report_results() for the manuscript ---------
# This file changes presentation only. Tables retain the cells produced by the
# shared report body. New comparison plots consume its machine-readable files.
# Run after report_results("1927") and the short-sample regression check.
compose_sample_reports <- function(project_dir = here::here()) {
  `%>%` <- dplyr::`%>%`
  source(file.path(project_dir, "R", "reporting_helpers.R"), local = TRUE)
  source(file.path(project_dir, "R", "report_portfolio_inference.R"), local = TRUE)
  root <- file.path(project_dir, "latex_paper", "results_v3")
  output <- file.path(root, "combined")
  # Render BH from the saved short-sample bootstrap. Keeping these wrappers in
  # staging preserves the original 1986 files and makes layout-only reruns
  # independent of whether a previous staging directory is still present.
  short_tables <- file.path(project_dir, "tmp", "report_short_inference_tables")
  purrr::walk(c("main_appendix", "internet_appendix"),
    ~ dir.create(file.path(short_tables, .x), recursive = TRUE, showWarnings = FALSE))
  write_portfolio_inference_tables(utils::read.csv(file.path(root, "1986", "data",
    "portfolio_inference_bootstrap.csv")), short_tables, project_dir)
  purrr::walk(c("main", "main_appendix", "internet_appendix", "data"),
    ~ dir.create(file.path(output, .x), recursive = TRUE, showWarnings = FALSE))
  samples <- c("1927", "1986")
  sample_names <- c(`1927` = "Long sample", `1986` = "Short sample")
  source_files <- character()
  inputs <- list(main = character(), main_appendix = character(), internet_appendix = character())
  register <- function(section, name) inputs[[section]] <<- c(inputs[[section]],
    paste0("\\input{results_v3/combined/", section, "/", name, ".tex}"))
  read_data <- function(name) purrr::map_dfr(samples, function(sample_id) {
    path <- file.path(root, sample_id, "data", paste0(name, ".csv"))
    source_files <<- c(source_files, path)
    utils::read.csv(path) %>%
      dplyr::mutate(sample = factor(sample_names[[sample_id]], levels = sample_names))
  })
  source_lines <- function(sample_id, section, name) {
    staging <- file.path(project_dir, "tmp", "report_short_inference_tables", section, paste0(name, ".tex"))
    path <- if (sample_id == "1986" && startsWith(name, "portfolio_inference_") && file.exists(staging))
      staging else file.path(root, sample_id, section, paste0(name, ".tex"))
    source_files <<- c(source_files, path)
    readLines(path, warn = FALSE)
  }
  caption_from <- function(lines) {
    x <- lines[grepl("\\caption[", lines, fixed = TRUE)][1]
    start <- regexpr("]{", x, fixed = TRUE)[1]
    end <- regexpr("}\\label", x, fixed = TRUE)[1]
    if (end < 0) end <- nchar(x)
    substr(x, start + 2L, end - 1L)
  }
  # Existing caption references to panel letters would be misleading after
  # adding the second vintage. Replace them with neutral descriptions below.
  titles <- c(performance = "Out-of-sample predictive performance.",
    factor_gain_summary = "Distribution of out-of-sample RMSE gains across the 153 equity factors.",
    portfolio_performance = "Economic performance of volatility-managed portfolios.",
    portfolio_performance_cederburg_ew = "Economic performance of fixed 50/50 unmanaged and volatility-managed combinations.",
    portfolio_performance_multifactor = "Economic performance of multifactor volatility-managed portfolios.",
    additional_scopes = "Out-of-sample predictive performance for the market portfolio estimated separately.",
    portfolio_performance_market_separate = "Economic performance of volatility-managed portfolios for the market portfolio estimated separately.",
    portfolio_implementation = "Implementation statistics for net capped volatility-managed portfolios.",
    portfolio_performance_cederburg_mvo = "Economic performance of signed two-sleeve mean-variance portfolios.",
    portfolio_spanning_single_factor = "Spanning regressions for net single-factor portfolios relative to unmanaged exposure.")
  notes <- c(performance = "Statistics pool errors across the stated target-month observations. MPHE is mean pseudo-Huber error and MAPE is mean absolute percentage error. Bias is realized minus predicted log variance. EW averages seven log forecasts, excluding HAR.",
    factor_gain_summary = "Positive percentage gains indicate lower RMSE than HAR. Quantiles describe the cross-sectional distribution across factors.",
    portfolio_performance = "Raw and capped columns report gross performance. Net columns deduct 10 basis points per unit change in factor exposure. The cap is 1.5. CER uses risk aversion five. The factor aggregate equal-weights 153 nonmarket factors within each month.",
    portfolio_performance_cederburg_ew = "Each managed row combines equal unmanaged and own-variance-managed sleeves. Total factor exposure is bounded by five. Returns, volatility, CER and costs are annualized percentages. Turnover is the monthly absolute exposure change. EW denotes the forecast ensemble.",
    portfolio_performance_multifactor = "UMV holds 154 unmanaged factors. CMV conditions on market volatility and FCMV on each factor's own volatility. EW and HEW identify allocation rules. EW in a forecast row denotes the forecast ensemble. Exposure is capped at 1.5. Returns, volatility, CER and costs are annualized percentages.",
    additional_scopes = "Each model is estimated only on the market time series. Forecast scores use monthly log realized variance.",
    portfolio_performance_market_separate = "The market forecast is estimated separately. Raw and capped columns are gross, and net columns deduct factor-exposure costs. Exposure is capped at 1.5 and CER uses risk aversion five.",
    portfolio_implementation = "Returns, volatility, CER, drawdowns and costs are percentages. Sharpe and CER use net excess returns. Factor-aggregate performance is computed from monthly aggregate returns. Exposure statistics summarize the underlying positions.",
    portfolio_performance_cederburg_mvo = "Signed two-sleeve MVO is compared with unmanaged-only MVO, which estimates the factor and cash allocation. Total exposure is bounded between minus five and five. Net results deduct factor-exposure costs. Returns, volatility, CER, drawdown and costs are annualized percentages except drawdown, which measures the full evaluation path.",
    portfolio_spanning_single_factor = "The dependent variable is the net managed excess return and the regressor is unmanaged excess return. Alpha is annualized. Newey-West statistics use lag one. Spanning is an ex-post opportunity-set diagnostic.")

  # Extract complete tabular bodies rather than recomputing their cells. The
  # short-sample numerical rows therefore remain exactly as human-reviewed.
  combine_table <- function(name, section, split_pages = FALSE, landscape = FALSE) {
    bodies <- list()
    k <- 0L
    for (sample_id in samples) {
      lines <- source_lines(sample_id, section, name)
      begin <- which(grepl("\\begin{tabular}", lines, fixed = TRUE))[1]
      end <- which(grepl("\\end{tabular}", lines, fixed = TRUE))[1]
      if (is.na(begin) || is.na(end)) rlang::abort(paste("Expected tabular:", name))
      body <- lines[seq.int(begin + 1L, end - 1L)]
      panel_rows <- which(grepl("\\textit{Panel ", body, fixed = TRUE))
      n_columns <- length(strsplit(body[grep(" & ", body)[1]], " & ", fixed = TRUE)[[1]])
      if (!length(panel_rows)) {
        k <- k + 1L
        body <- c(paste0("\\multicolumn{", n_columns, "}{@{}l}{\\textit{Panel ",
          LETTERS[k], ". ", sample_names[[sample_id]], "}} \\\\"), body)
      } else for (j in panel_rows) {
        k <- k + 1L
        body[j] <- sub("Panel [A-Z]\\. ", paste0("Panel ", LETTERS[k], ". ",
          sample_names[[sample_id]], ": "), body[j])
      }
      bodies[[sample_id]] <- list(spec = lines[begin], body = body)
    }
    title <- if (name %in% names(titles)) titles[[name]] else
      sub("\\.$", ".", name)
    note <- if (name %in% names(notes)) notes[[name]] else ""
    if (grepl("inference", name)) {
      title <- if (grepl("mean_ann", name)) "Paired mean excess return comparisons." else
        if (grepl("single_factor", name)) "Paired Sharpe comparisons for single-factor strategies." else
          "Paired Sharpe comparisons for multifactor strategies."
      note <- paste("Each row compares the EW forecast ensemble with the stated benchmark, using net returns.",
        "Intervals are pointwise 95 percent percentile intervals from 10,000 paired stationary-bootstrap draws with expected block length five months.",
        "One-sided p-values use centered bootstrap errors. Holm and Benjamini-Hochberg (BH) adjustments use the same prespecified family across Sharpe, mean and CER, separately within each sample.",
        "BH controls the false discovery rate under its dependence assumptions. Inference is conditional on fitted forecasts.")
    }
    dates <- if (name %in% c("performance", "factor_gain_summary", "additional_scopes"))
      "Forecast evaluation spans June 1947 to December 2024 in the long sample and July 2006 to December 2024 in the short sample." else
      if (name %in% c("portfolio_performance_multifactor", "portfolio_inference_multifactor_primary"))
        "Long-sample evaluation spans January 1980 to December 2024. Short-sample evaluation spans July 2011 to December 2024." else
        "Long-sample market strategies span June 1952 to December 2024 and factor aggregates January 1980 to December 2024. Short-sample strategies span July 2011 to December 2024."
    caption <- paste0("\\footnotesize \\textbf{: ", title, "} ", note, " ", dates)
    pages <- if (split_pages) lapply(samples, function(s) bodies[s]) else list(bodies)
    text <- unlist(purrr::imap(pages, function(page, index) {
      tabular <- page[[1]]$spec
      c(if (landscape) "\\begin{landscape}", "\\begin{table}[p]",
        if (index > 1L) "\\ContinuedFloat",
        if (name == "performance") "\\centering\\footnotesize" else "\\centering\\scriptsize",
        "\\captionsetup{justification=raggedright,labelsep=none,singlelinecheck=false,labelformat=boldlabel}",
        paste0("\\renewcommand{\\arraystretch}{", if (name == "performance") "0.88" else "0.91",
          "}\\setlength{\\tabcolsep}{4pt}"),
        paste0("\\caption[]{", if (index == 1L) caption else paste0("\\footnotesize \\textbf{: ", title,
          " Continued.} Short-sample results. Definitions and conventions follow the preceding panels."), "}"),
        if (index == 1L) paste0("\\label{tab:rv-", name, "}"),
        "\\resizebox{\\ifdim\\width>\\linewidth\\linewidth\\else\\width\\fi}{!}{%", tabular,
        unlist(purrr::map(page, "body"), use.names = FALSE),
        "\\end{tabular}}",
        "\\end{table}", if (landscape) "\\end{landscape}")
    }), use.names = FALSE)
    writeLines(text, file.path(output, section, paste0(name, ".tex")))
    register(section, name)
  }
  # Longtables span several pages within each sample. Decrement only the table
  # counter between samples, preserving one exhibit number and unique labels.
  combine_longtable <- function(name) {
    text <- purrr::imap(samples, function(sample_id, index) {
      lines <- source_lines(sample_id, "internet_appendix", name)
      lines <- gsub("\\textbf{: ", paste0("\\textbf{: ", sample_names[[sample_id]], ". "), lines, fixed = TRUE)
      if (index > 1L) lines <- gsub(paste0("\\label{tab:rv-", name, "}"),
        paste0("\\label{tab:rv-", name, "-short}"), lines, fixed = TRUE)
      c(if (index > 1L) "\\addtocounter{table}{-1}", lines)
    })
    writeLines(unlist(text), file.path(output, "internet_appendix", paste0(name, ".tex")))
    register("internet_appendix", name)
  }
  figure_tex <- function(name, section, caption, paths, split_pages = FALSE) {
    pages <- if (split_pages) as.list(seq_along(paths)) else list(seq_along(paths))
    text <- unlist(purrr::imap(pages, function(page, i) c("\\begin{figure}[p]",
      if (i > 1L) "\\ContinuedFloat", "\\centering",
      "\\captionsetup{justification=raggedright,labelsep=none,singlelinecheck=false,labelformat=boldlabel}",
      paste0("\\caption[]{\\footnotesize \\textbf{: ",
        sub("\\..*$", ".", caption), "} ", sub("^[^.]*\\. ?", "", caption),
        if (i > 1L) " Continued." else "", "}"),
      if (i == 1L) paste0("\\label{fig:rv-", name, "}"),
      unlist(purrr::map(page, function(j) c(
        if (length(paths) > 1L) paste0("\\par\\smallskip\\textbf{Panel ", LETTERS[j], ". ",
          sample_names[j], "}\\par\\smallskip"),
        paste0("\\includegraphics[width=\\linewidth,height=", if (length(page) > 1L) "0.32" else "0.70",
          "\\textheight,keepaspectratio]{", paths[j], "}")))), "\\end{figure}")), use.names = FALSE)
    writeLines(text, file.path(output, section, paste0(name, ".tex")))
    register(section, name)
  }
  pair_figure <- function(name, section, split_pages = FALSE) {
    lines <- source_lines("1986", section, name)
    caption <- caption_from(lines)
    caption <- sub("\\\\footnotesize \\\\textbf\\{: ", "", caption)
    caption <- sub("}", "", caption, fixed = TRUE)
    figure_tex(name, section, caption,
      paste0("results_v3/", samples, "/", section, "/", name, ".pdf"), split_pages)
  }
  palette <- c(Unmanaged = "#444444", `Random walk` = "#999999", HAR = "#D55E00",
    HARX = "#E69F00", EW = "#0072B2")
  paper_theme <- ggplot2::theme_bw(base_size = 10) + ggplot2::theme(
    legend.position = "bottom", panel.grid.minor = ggplot2::element_blank(),
    strip.background = ggplot2::element_rect(fill = "grey96"))
  save_plot <- function(plot, name, section, caption, width = 10, height = 6) {
    purrr::walk(c("pdf", "png"), ~ ggplot2::ggsave(file.path(output, section,
      paste0(name, ".", .x)), plot, width = width, height = height, dpi = 200, bg = "white"))
    figure_tex(name, section, caption, paste0("results_v3/combined/", section, "/", name, ".pdf"))
  }

  combine_table("performance", "main")
  combine_table("factor_gain_summary", "main")
  scores <- read_data("scope_metrics")
  gains <- scores %>% dplyr::left_join(scores %>% dplyr::filter(model == "har") %>%
    dplyr::select(sample, scope, har_rmse = rmse), by = c("sample", "scope"), relationship = "many-to-one") %>%
    dplyr::mutate(gain = 100 * (1 - rmse / har_rmse), model = factor(model,
      levels = c("har", "harx", "glmnet", "rf", "xgb", "nn1", "nn2", "nn3", "ew"),
      labels = c("HAR", "HARX", "ENET", "RF", "XGB", "NN1", "NN2", "NN3", "EW")))
  for (separate in c(FALSE, TRUE)) {
    data <- gains %>% dplyr::filter((scope == "Market portfolio, separate model") == separate)
    name <- if (separate) "har_relative_gains_market_separate" else "har_relative_gains"
    section <- if (separate) "internet_appendix" else "main"
    plot <- ggplot2::ggplot(data, ggplot2::aes(model, gain, colour = sample, shape = sample)) +
      ggplot2::geom_hline(yintercept = 0, colour = "grey60") +
      ggplot2::geom_point(size = 2.7, position = ggplot2::position_dodge(width = .35)) +
      ggplot2::facet_wrap(~ scope, nrow = 1) + ggplot2::scale_shape_manual(values = c(16, 4)) +
      ggplot2::scale_colour_manual(values = c("#0072B2", "#D55E00")) +
      ggplot2::labs(x = NULL, y = "RMSE reduction relative to HAR (%)", colour = NULL, shape = NULL) + paper_theme
    save_plot(plot, name, section, paste("Out-of-sample RMSE gains relative to HAR.",
      "Dots identify the long sample and crosses the short sample. Each comparison uses that sample's HAR.",
      "Panel statistics pool squared errors across target-month observations before taking the square root."), height = 4)
  }
  combine_table("portfolio_performance", "main", split_pages = TRUE)
  combine_table("portfolio_performance_cederburg_ew", "main")
  combine_table("portfolio_performance_multifactor", "main")
  # Preserve the original loading calculation and colour conventions. Two
  # universe columns avoid shrinking four vertically stacked heatmaps.
  final_themes <- read_data("final_theme_loadings") %>% dplyr::mutate(
    sample = factor(paste0(c("Panel A. ", "Panel B. ")[as.integer(sample)], sample)),
    universe = dplyr::recode(universe, panel = "Pooled panel", spx = "Separate market"),
    model = factor(model, levels = c("har", "harx", "glmnet", "rf", "xgb", "nn1", "nn2", "nn3"),
      labels = c("HAR", "HARX", "ENET", "RF", "XGB", "NN1", "NN2", "NN3")))
  for (measure in c("gross_share", "net_share")) {
    plot <- ggplot2::ggplot(final_themes, ggplot2::aes(model, theme, fill = .data[[measure]])) +
      ggplot2::geom_tile(colour = "white") +
      ggplot2::geom_text(ggplot2::aes(label = sprintf("%.2f", .data[[measure]])), size = 2.6) +
      ggplot2::facet_grid(sample ~ universe, scales = "free_y", space = "free_y") +
      ggplot2::labs(x = NULL, y = NULL, fill = "Share") + paper_theme
    plot <- if (measure == "net_share") plot + ggplot2::scale_fill_gradient2(
      low = "#D55E00", mid = "white", high = "#0072B2", midpoint = 0) else
      plot + ggplot2::scale_fill_gradient(low = "white", high = "#56B4E9", limits = c(0, 1))
    section <- if (measure == "gross_share") "main_appendix" else "internet_appendix"
    save_plot(plot, paste0("final_theme_", measure), section,
      paste("Predictor-theme", if (measure == "gross_share") "absolute" else "signed",
        "loading shares at the final model refit.",
        "Panel A reports the long sample and Panel B the short sample. Columns distinguish pooled and separate market estimation.",
        "Linear surrogate coefficients are normalized by the sum of absolute non-intercept coefficients before aggregation by theme.",
        "Blank cells indicate absent loadings. These descriptive associations are not causal importance measures."),
      width = 10, height = 7)
  }

  # These figures use the same annualized gross/net Sharpe summaries as the
  # economic-performance tables. Fixed caps are retained within each family.
  mm <- read_data("portfolio_summary") %>% dplyr::filter(version %in% c("capped", "net_capped")) %>%
    dplyr::transmute(sample, scope = universe, construction = "MM", model = strategy,
      basis = ifelse(version == "capped", "Gross", "Net"), sharpe = sharpe_ratio)
  ced <- read_data("portfolio_inference_single_factor_descriptive") %>%
    dplyr::filter(family == "ced_ew", version == "capped", model %in% c("buyhold", "rw", "har", "harx", "ew")) %>%
    dplyr::mutate(scope = dplyr::recode(scope, aggregate_153 = "factor_panel"),
      model = dplyr::recode(model, buyhold = "unmanaged"), construction = "50/50 combination") %>%
    tidyr::pivot_longer(c(gross_sharpe, net_sharpe), names_to = "basis", values_to = "sharpe") %>%
    dplyr::mutate(basis = ifelse(basis == "gross_sharpe", "Gross", "Net")) %>%
    dplyr::select(sample, scope, construction, model, basis, sharpe)
  multi <- read_data("portfolio_inference_multifactor_descriptive") %>% dplyr::filter(version == "capped")
  multifactor <- purrr::map_dfr(c("CMV", "FCMV"), function(spec) {
    prefix <- if (spec == "CMV") "cmv_panel_" else "fcmv_"
    multi %>% dplyr::filter(startsWith(model, prefix) | model == "umv" |
      (spec == "CMV" & model == "lagged_market")) %>%
      dplyr::mutate(scope = "factor_panel", construction = paste(spec,
        ifelse(family == "multi_ew", "EW allocation", "HEW allocation"), sep = " / "),
        model = dplyr::case_when(model == "umv" ~ "unmanaged", model == "lagged_market" ~ "rw",
          TRUE ~ sub(prefix, "", model, fixed = TRUE))) %>%
      tidyr::pivot_longer(c(gross_sharpe, net_sharpe), names_to = "basis", values_to = "sharpe") %>%
      dplyr::mutate(basis = ifelse(basis == "gross_sharpe", "Gross", "Net")) %>%
      dplyr::select(sample, scope, construction, model, basis, sharpe)
  })
  bar_data <- dplyr::bind_rows(mm, ced, multifactor) %>%
    dplyr::mutate(construction = factor(construction, levels = c("MM", "50/50 combination",
      "CMV / EW allocation", "FCMV / EW allocation", "CMV / HEW allocation", "FCMV / HEW allocation")),
      model = factor(model, levels = c("unmanaged", "rw", "har", "harx", "ew"),
      labels = names(palette)), basis = factor(basis, levels = c("Gross", "Net")),
      sample = factor(paste0(c("Panel A. ", "Panel B. ")[as.integer(sample)], sample)))
  utils::write.csv(bar_data, file.path(output, "data", "gross_net_sharpe.csv"), row.names = FALSE)
  for (scope_id in c("market_pooled", "factor_panel", "market_separate")) {
    data <- dplyr::filter(bar_data, scope == scope_id)
    filename <- paste0("portfolio_wealth_", scope_id)
    section <- if (scope_id == "market_separate") "internet_appendix" else "main_appendix"
    plot <- ggplot2::ggplot(data, ggplot2::aes(model, sharpe, fill = model, alpha = basis)) +
      ggplot2::geom_hline(yintercept = 0, colour = "grey60") +
      ggplot2::geom_col(position = ggplot2::position_dodge(width = .8), width = .72) +
      ggplot2::facet_grid(sample ~ construction, scales = "free_y") +
      ggplot2::scale_fill_manual(values = palette, drop = FALSE, guide = "none") +
      ggplot2::scale_alpha_manual(values = c(Gross = .35, Net = 1), drop = FALSE) +
      ggplot2::labs(x = NULL, y = "Annualized Sharpe ratio", alpha = NULL) + paper_theme +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 8),
        strip.text = ggplot2::element_text(size = 8))
    caption <- paste(if (scope_id == "factor_panel")
      "Gross and net Sharpe ratios for factor aggregates and multifactor allocations." else
      if (scope_id == "market_pooled") "Gross and net Sharpe ratios for the market portfolio using pooled forecasts." else
        "Gross and net Sharpe ratios for the market portfolio using separate forecasts.",
      "Panel A reports the long sample and Panel B the short sample. Pale bars are gross and solid bars are net of modeled factor-exposure costs.",
      "EW denotes the forecast ensemble. Each construction retains its stated exposure cap.",
      if (scope_id == "factor_panel") paste(
        "MM and 50/50 combinations equal-weight the 153 nonmarket factors. Multifactor allocations include the market.",
        "In CMV, Random walk denotes the lagged-market signal. FCMV comparisons use HAR, HARX and the ensemble.",
        "Unmanaged denotes the matching UMV in multifactor panels.") else "")
    if (scope_id == "factor_panel") {
      # Six constructions are legible as a 3-by-2 grid per sample, rather than
      # six narrow columns compressed across one manuscript page.
      purrr::iwalk(levels(data$sample), function(sample_label, index) {
        sample_plot <- plot + dplyr::filter(data, sample == sample_label) +
          ggplot2::facet_wrap(~ construction, ncol = 3, scales = "free_y")
        purrr::walk(c("pdf", "png"), ~ ggplot2::ggsave(file.path(output, section,
          paste0(filename, "_", index, ".", .x)), sample_plot,
          width = 9, height = 5.6, dpi = 200, bg = "white"))
      })
      figure_tex(filename, section, caption, paste0("results_v3/combined/", section,
        "/", filename, "_", 1:2, ".pdf"), split_pages = TRUE)
    } else save_plot(plot, filename, section, caption, width = 9, height = 6)
  }
  pair_figure("portfolio_cluster_sharpe", "main_appendix", split_pages = TRUE)
  for (name in c("portfolio_inference_single_factor_primary", "portfolio_inference_multifactor_primary"))
    combine_table(name, "main_appendix", split_pages = TRUE)
  combine_longtable("factor_rmse")
  combine_table("additional_scopes", "internet_appendix")
  pair_figure("sequential_waterfall", "internet_appendix", split_pages = TRUE)
  pair_figure("rolling_rmse", "internet_appendix")
  # Cluster curves retain pooled squared-error RMSE. A dedicated first facet
  # emphasizes overall pooled-panel accuracy, with a common sample legend.
  cluster <- read_data("cluster_rolling_rmse")
  overall <- read_data("rolling_rmse") %>% dplyr::filter(scope == "Pooled panel", model == "ew") %>%
    dplyr::mutate(cluster = "Overall pooled panel", p25 = NA_real_, p75 = NA_real_)
  cluster_levels <- c("Overall pooled panel", sort(unique(cluster$cluster)))
  curves <- dplyr::bind_rows(cluster, overall) %>% dplyr::mutate(month_id = as.Date(month_id),
    cluster = factor(cluster, levels = cluster_levels))
  plot <- ggplot2::ggplot(curves, ggplot2::aes(month_id, rmse, colour = sample, linetype = sample)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = p25, ymax = p75, fill = sample),
      colour = NA, alpha = .10, show.legend = FALSE, na.rm = TRUE) +
    ggplot2::geom_line(linewidth = .5, na.rm = TRUE) +
    ggplot2::facet_wrap(~ cluster, ncol = 3, scales = "free_y") +
    ggplot2::scale_colour_manual(values = c("#0072B2", "#D55E00")) +
    ggplot2::scale_fill_manual(values = c("#0072B2", "#D55E00")) +
    ggplot2::labs(x = NULL, y = "Trailing 12-month RMSE", colour = NULL, linetype = NULL) + paper_theme
  aggregate_plot <- plot + dplyr::filter(curves, cluster == "Overall pooled panel") +
    ggplot2::facet_wrap(~ cluster, ncol = 1) + ggplot2::theme(legend.position = "none")
  theme_plot <- plot + dplyr::filter(curves, cluster != "Overall pooled panel")
  cluster_figure <- grid::grobTree(
    grid::grobTree(ggplot2::ggplotGrob(aggregate_plot),
      vp = grid::viewport(x = .5, y = .90, width = 1, height = .20)),
    grid::grobTree(ggplot2::ggplotGrob(theme_plot),
      vp = grid::viewport(x = .5, y = .40, width = 1, height = .80)))
  save_plot(cluster_figure, "cluster_rolling_rmse", "internet_appendix",
    "Trailing 12-month RMSE of the EW forecast ensemble. The first panel pools all factor and market errors. The remaining panels pool errors within JKP clusters. Shading shows the interquartile range of factor-level RMSE. Both sample histories are shown on the same date axis.", height = 11)
  for (name in c("validation_test", "hyperparameters_all",
                 "theme_history_panel", "theme_history_spx", "portfolio_ew_sharpe_waterfall"))
    pair_figure(name, "internet_appendix", split_pages = name != "final_theme_net_share")
  # No long-sample full-sample-normalizer backtest exists. Include only the
  # observed sensitivity and make its short-sample status explicit.
  name <- "portfolio_performance_full_sample"
  lines <- source_lines("1986", "internet_appendix", name)
  lines <- gsub("\\textbf{: ", "\\textbf{: Short sample. ", lines, fixed = TRUE)
  lines <- gsub("{1.0}", "{0.98}", lines, fixed = TRUE)
  writeLines(lines, file.path(output, "internet_appendix", paste0(name, ".tex")))
  register("internet_appendix", name)
  combine_longtable("portfolio_factor_sharpe")
  combine_table("portfolio_performance_market_separate", "internet_appendix", split_pages = TRUE)
  combine_table("portfolio_implementation", "internet_appendix", split_pages = TRUE, landscape = TRUE)
  combine_table("portfolio_performance_cederburg_mvo", "internet_appendix", split_pages = TRUE, landscape = TRUE)
  combine_table("portfolio_spanning_single_factor", "internet_appendix")
  combine_table("portfolio_inference_single_factor_primary_mean_ann_1", "internet_appendix", split_pages = TRUE)
  # R-squared detail and the three requested mean/CER appendix exhibits are
  # excluded here, while their machine-readable statistics remain available.
  # Preserve the reviewed exhibit sequence, independently of computation order.
  old_order <- readLines(file.path(root, "1986", "internet_appendix.tex"), warn = FALSE)
  names_in_order <- sub(".*/([^/]+)\\.tex.*", "\\1", old_order)
  desired <- paste0("\\input{results_v3/combined/internet_appendix/", names_in_order, ".tex}")
  ordered <- desired[desired %in% inputs$internet_appendix]
  # A reporting-only run may omit inference files from the sample-specific
  # registry even though their reviewed artifacts remain available. Retain any
  # combined exhibits absent from that partial registry in generation order.
  inputs$internet_appendix <- c(ordered, setdiff(inputs$internet_appendix, ordered))
  purrr::iwalk(inputs, ~ writeLines(.x, file.path(output, paste0(.y, ".tex"))))
  inference <- read_data("portfolio_inference_bootstrap") %>%
    dplyr::group_by(sample, family, block_length) %>%
    dplyr::mutate(p_bh = stats::p.adjust(p_value, "BH")) %>% dplyr::ungroup()
  utils::write.csv(inference, file.path(output, "data", "portfolio_inference_bootstrap.csv"), row.names = FALSE)
  manifest <- tibble::tibble(file = unique(c(source_files, file.path(project_dir, "R",
    c("report_results.R", "report_portfolio_inference.R", "compose_sample_reports.R"))))) %>%
    dplyr::mutate(sha256 = purrr::map_chr(file, ~ digest::digest(file = .x, algo = "sha256")))
  utils::write.csv(manifest, file.path(output, "data", "sources.csv"), row.names = FALSE)
  invisible(inputs)
}
