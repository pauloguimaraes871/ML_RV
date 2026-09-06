# Literature review outputs (paper section 2)-----------------------------------

  ## This script produces every table and figure that Section 2 of the
  ## manuscript refers to. It reads three sources and writes nothing else:
  ##   - bibliometrics/scopus_final.csv     : the systematic review corpus
  ##   - bibliometrics/review_predictors.csv: the predictor inventory
  ##   - data/covariates_panel_*_v3.rds     : the covariate panels
  ## It never fits a model and never alters an artifact under data/.
  ##
  ## Outputs
  ##   tables/literature_families.tex    main paper, Table 1
  ##   tables/literature_predictors.tex  Internet Appendix
  ##   figures/literature_growth.png     main paper, Figure 1
  ##   figures/literature_growth_figure.tex
  ##   figures/literature_networks_figure.tex  Internet Appendix
  ##
  ## Nothing written here may name a file, a column, or a pipeline stage: the
  ## reader of the paper has none of that context. Keep internal vocabulary in
  ## comments, never in a caption.

  `%>%` <- dplyr::`%>%`

  project_dir <- here::here()
  tables_dir  <- file.path(project_dir, "tables")
  figures_dir <- file.path(project_dir, "figures")
  dir.create(tables_dir,  recursive = TRUE, showWarnings = FALSE)
  dir.create(figures_dir, recursive = TRUE, showWarnings = FALSE)

  if (.Platform$OS.type == "windows") {
    grDevices::windowsFonts(Helvetica = grDevices::windowsFont("Arial"))
  }

# Helpers-----------------------------------------------------------------------

  ## LaTeX escaping for anything that comes out of the bibliographic export.
  ## Journal names carry ampersands and titles carry percent signs; leaving
  ## either unescaped produces a silent compilation failure much later.
  esc_tex <- function(x) {
    x <- as.character(x)
    x <- gsub("\\", "\\textbackslash{}", x, fixed = TRUE)
    x <- gsub("&", "\\&", x, fixed = TRUE)
    x <- gsub("%", "\\%", x, fixed = TRUE)
    x <- gsub("_", "\\_", x, fixed = TRUE)
    x <- gsub("#", "\\#", x, fixed = TRUE)
    x
  }

  cite_of <- function(x) sprintf("\\citet{%s}", gsub("; ", ",", x, fixed = TRUE))

# Corpus--------------------------------------------------------------------

  scopus_path <- file.path(project_dir, "bibliometrics", "scopus_final.csv")
  if (!file.exists(scopus_path)) {
    stop("Missing systematic review corpus: ", scopus_path)
  }

  corpus <- utils::read.csv(scopus_path, check.names = FALSE,
                            stringsAsFactors = FALSE)

  corpus$py <- suppressWarnings(as.integer(corpus[["Year"]]))
  corpus$tc <- suppressWarnings(as.integer(corpus[["Cited by"]]))
  corpus$tc[is.na(corpus$tc)] <- 0L

  corpus_stats <- list(
    n_docs    = nrow(corpus),
    year_min  = min(corpus$py, na.rm = TRUE),
    year_max  = max(corpus$py, na.rm = TRUE),
    n_sources = length(unique(corpus[["Source title"]]))
  )

  ## Machine-learning subset, flagged on the abstract and author keywords with
  ## the term list used in the exploratory bibliometric report. Kept identical
  ## so the two documents cannot diverge.
  ml_keywords <- c("machine learning", "neural network", "deep learning",
                   "lstm", "gru", "svm", "lasso", "random forest", "boosting",
                   "ensemble", "recurrent", "transformer", "gradient boosting",
                   "reinforcement learning", "support vector")
  abstracts <- tolower(paste(corpus[["Abstract"]], corpus[["Author Keywords"]]))
  corpus$is_ml <- Reduce(`|`, lapply(ml_keywords, function(k)
    grepl(k, abstracts, fixed = TRUE)))

  ## Local citations: how many of the corpus articles cite a given source. This
  ## is the metric that ties a predictor family back to the review, and it is
  ## defined identically for sources inside the corpus and for the foundational
  ## work that the corpus cites without containing.
  corpus_refs <- tolower(corpus[["References"]])
  local_cites <- function(pattern) {
    sum(grepl(tolower(pattern), corpus_refs, fixed = TRUE))
  }

# Table 1: predictor families identified by the review----------------------

  ## One row per predictor family. `pattern` is a distinctive fragment of the
  ## representative source's reference string, used to count local citations.
  ## `ids` cross-references the predictor inventory in the Internet Appendix.
  families <- tibble::tribble(
    ~family, ~bibkey, ~pattern, ~implemented, ~ids,

    "Daily, weekly and monthly lagged realized variance",
      "corsi2009", "simple approximate long-memory", TRUE, "F01",
    "Realized quarticity",
      "bollerslevpattonquaedvlieg2016", "exploiting the errors", TRUE, "F02",
    "Jump and continuous components of realized variance",
      "andersenbollerslevdiebold2007", "roughing it up", TRUE, "F03, F04",
    "Leverage effect from non-positive returns",
      "campbellhentschel1992", "no news is good news", TRUE, "F71",
    "Option-implied volatility",
      "christensenprabhala1998", "relation between implied and realized", TRUE, "F06",
    "Economic policy uncertainty",
      "liuzhang2015", "economic policy uncertainty and stock market", TRUE, "F09",
    "Geopolitical risk",
      "caldaraiacoviello2022", "measuring geopolitical risk", TRUE, "F41",
    "Volatility of producer price and industrial production growth",
      "schwert1989", "why does stock market volatility change", TRUE, "F18, F21, F22",
    "Volatility of the monetary base",
      "schwert1989", "why does stock market volatility change", TRUE, "F20",
    "Credit spreads",
      "paye2012", "predictive regressions for aggregate stock market", TRUE, "F13",
    "Term structure and the short rate",
      "welchgoyal2008", "comprehensive look", TRUE, "F14, F32",
    "Equity valuation ratios",
      "welchgoyal2008", "comprehensive look", TRUE, "F30",
    "Survey-based consumer sentiment",
      "audrinosigristballinari2020", "sentiment and attention measures", TRUE, "F40",
    "Realized variance of crude oil",
      "degiannakisfilis2017", "information channels from other asset", TRUE, "F42",
    ## The trading-day count and the month dummies are both Schwert's, not
    ## McAleer and Medeiros's. The latter contribute the announcement and
    ## day-of-week dummies, which form their own family below.
    "Calendar effects and trading-day count",
      "schwert1989", "why does stock market volatility change", TRUE, "F25, F27",

    "Realized semivariances and signed jumps",
      "pattonsheppard2015", "good volatility, bad volatility", FALSE, "F05, F67, F82",
    "Scheduled macroeconomic announcements and day-of-week effects",
      "mcaleermedeiros2008", "multiple regime smooth transition", FALSE, "F69, F70",
    "Implied volatility of other markets and asset classes",
      "liangweizhang2020", "implied volatility more informative", FALSE, "F07, F08",
    "Realized quarticity interacted with lagged variance",
      "bollerslevpattonquaedvlieg2016", "exploiting the errors", FALSE, "F68",
    "News, social media sentiment and text-based uncertainty",
      "audrinosigristballinari2020", "sentiment and attention measures", FALSE, "F28, F66",
    "Search-engine attention",
      "dimpfljank2016", "internet search queries", FALSE, "F29, F43",
    "International volatility spillovers, jump factors and capital flows",
      "zhangmaliao2020", "forecasting global equity market", FALSE, "F58, F59, F60, F61, F62, F83",
    "Realized variance of other asset classes",
      "degiannakisfilis2017", "information channels from other asset", FALSE, "F72, F73, F74",
    "Foreign exchange returns, carry and liquidity",
      "audrinosigristballinari2020", "sentiment and attention measures", FALSE, "F55, F56, F57",
    "Trading volume, turnover and order book measures",
      "rahimikiapoon2026", "machine learning for realised", FALSE, "F26, F35, F46, F53",
    "Technical and price-range indicators",
      "songtangwangma2023", "garch-midas and deep learning", FALSE,
      "F45, F47, F48, F49, F50, F51, F52",
    "Option prices across moneyness and maturity",
      "carrwuzhang2020", "using machine learning to predict realized", FALSE, "F65",
    "Macroeconomic factors from large panels",
      "juradoludvigsonng2015", "measuring uncertainty", FALSE, "F44, F79",
    "Business-cycle, leverage and funding conditions",
      "paye2012", "predictive regressions for aggregate stock market", FALSE,
      "F10, F11, F12, F15, F16, F17, F24",
    "Interest rate and bond market volatility and risk premia",
      "schwert1989", "why does stock market volatility change", FALSE,
      "F19, F23, F33, F34, F80",
    "Macroeconomic activity and price indicators",
      "audrinosigristballinari2020", "sentiment and attention measures", FALSE,
      "F36, F37, F38, F39, F75, F76, F77, F78",
    "Equity factor and lagged equity returns",
      "nonejad2017", "which models forecast best", FALSE, "F31, F54, F81",
    "Topological and signal-decomposition features",
      "souto2023", "topological tail dependence", FALSE, "F63, F64"
  )

  families$cites <- vapply(families$pattern, local_cites, integer(1))

  ## Guard: the families marked as implemented must correspond to the
  ## predictors flagged as implemented in the inventory, and to variables that
  ## actually reach the models. A mismatch means the paper would misdescribe
  ## the estimation sample, so fail rather than emit a wrong table.
  predictors_path <- file.path(project_dir, "bibliometrics",
                               "review_predictors.csv")
  if (!file.exists(predictors_path)) {
    stop("Missing predictor inventory: ", predictors_path)
  }
  predictors <- utils::read.csv(predictors_path, check.names = FALSE,
                                stringsAsFactors = FALSE)

  ids_of <- function(s) trimws(unlist(strsplit(s, ",", fixed = TRUE)))

  ## Guard: the families must partition the inventory. Without this the table
  ## silently classifies part of the review and reads as though it covered all
  ## of it, which is how the crude oil index and the international implied
  ## volatility measures went missing from an earlier version.
  all_claimed <- unlist(lapply(families$ids, ids_of))
  duplicated_ids <- unique(all_claimed[duplicated(all_claimed)])
  if (length(duplicated_ids) > 0L) {
    stop("Predictors claimed by more than one family: ",
         paste(duplicated_ids, collapse = ", "))
  }
  unclaimed <- setdiff(predictors$id, all_claimed)
  if (length(unclaimed) > 0L) {
    stop("Predictors claimed by no family: ", paste(unclaimed, collapse = ", "))
  }
  unknown_ids <- setdiff(all_claimed, predictors$id)
  if (length(unknown_ids) > 0L) {
    stop("Families refer to predictors absent from the inventory: ",
         paste(unknown_ids, collapse = ", "))
  }

  ids_implemented_families <- sort(unique(unlist(
    lapply(families$ids[families$implemented], ids_of))))
  ids_implemented_inventory <- sort(predictors$id[predictors$implemented == "Yes"])

  only_in_families <- setdiff(ids_implemented_families, ids_implemented_inventory)
  if (length(only_in_families) > 0L) {
    stop("Predictors marked implemented in Table 1 but not in the inventory: ",
         paste(only_in_families, collapse = ", "))
  }
  only_in_inventory <- setdiff(ids_implemented_inventory, ids_implemented_families)
  if (length(only_in_inventory) > 0L) {
    stop("Predictors marked implemented in the inventory but in no Table 1 family: ",
         paste(only_in_inventory, collapse = ", "))
  }

  ## Guard: every covariate the models receive must belong to an implemented
  ## family. The panels are read only to check this; nothing from them is
  ## reported in the paper by this script.
  panel_paths <- c(
    file.path(project_dir, "data", "covariates_panel_1927_v3.rds"),
    file.path(project_dir, "data", "covariates_panel_1986_v3.rds")
  )
  if (all(file.exists(panel_paths))) {
    key_cols <- c("name", "month_id")
    model_vars <- sort(unique(unlist(lapply(panel_paths, function(p)
      setdiff(names(readRDS(p)), key_cols)))))
    ## Prefix of each covariate block, mapped to the family it belongs to.
    var_family <- c(
      rv = "lagged realized variance", rq = "realized quarticity",
      jump = "jump component", cont = "continuous component",
      negret = "leverage", log_epu = "policy uncertainty",
      ppi = "producer prices", ip = "industrial production",
      mbas = "monetary base", dy = "valuation ratios", ey = "valuation ratios",
      dfr = "credit spreads", dfy = "credit spreads", tms = "term structure",
      tbill = "term structure", log_cons = "survey sentiment",
      gpr = "geopolitical risk", oil = "crude oil variance",
      month = "calendar", sqrt = "calendar", vix = "implied volatility"
    )
    unmatched <- model_vars[!vapply(model_vars, function(v)
      any(startsWith(v, names(var_family))), logical(1))]
    if (length(unmatched) > 0L) {
      stop("Covariates that belong to no family in Table 1: ",
           paste(unmatched, collapse = ", "))
    }
    n_model_vars <- length(model_vars)
  } else {
    warning("Covariate panels not found; the coverage guard did not run.")
    n_model_vars <- NA_integer_
  }

  fam_tbl <- families %>%
    dplyr::arrange(dplyr::desc(.data$implemented), dplyr::desc(.data$cites))

  n_impl <- sum(families$implemented)

  ## The taxonomy partitions the whole inventory, so the table no longer fits
  ## on one page. A longtable breaks across pages instead of overrunning the
  ## bottom margin, which a table environment does without any warning.
  tbl1 <- c(
    "{\\scriptsize",
    "\\captionsetup{justification=raggedright,labelsep=none,singlelinecheck=false,labelformat=boldlabel}",
    "\\setlength{\\tabcolsep}{4pt}",
    "\\renewcommand{\\arraystretch}{1.05}",
    paste0("\\begin{longtable}{",
           ">{\\raggedright\\arraybackslash}m{6.0cm}",
           ">{\\raggedright\\arraybackslash}m{3.9cm}",
           ">{\\raggedleft\\arraybackslash}m{1.0cm}",
           ">{\\raggedleft\\arraybackslash}m{1.4cm}}"),
    sprintf(paste0(
      "\\caption[Predictor families identified by the systematic review]{\\footnotesize ",
      "\\textbf{: Predictor families identified by the systematic review.} The ",
      "%d predictors of realized variance proposed in the reviewed literature ",
      "and in the foundational work it builds on, grouped into %d families. ",
      "Each family is shown with the study taken as its representative source. ",
      "`Cited by' counts how many of the %d reviewed articles cite that source, ",
      "which places foundational work the reviewed articles build on, such as ",
      "\\citet{corsi2009} and \\citet{schwert1989}, on the same scale as work ",
      "inside the review set. The last column records whether the family enters ",
      "the models estimated here; %d of the %d families do. Families are ordered ",
      "by that column and then by citations. Table~\\ref{tab:litpredictors} ",
      "lists the individual predictors and repeats this classification at that ",
      "level.} \\label{tab:litfamilies} \\\\"),
      nrow(predictors), nrow(families), corpus_stats$n_docs, n_impl,
      nrow(families)),
    "\\hline",
    "Predictor family & Representative source & Cited by & In this study \\\\",
    "\\hline",
    "\\endfirsthead",
    "\\hline",
    "Predictor family & Representative source & Cited by & In this study \\\\",
    "\\hline",
    "\\endhead",
    "\\hline",
    "\\endfoot",
    sprintf("%s & %s & %d & %s \\\\",
            esc_tex(fam_tbl$family), cite_of(fam_tbl$bibkey),
            fam_tbl$cites, ifelse(fam_tbl$implemented, "Yes", "No")),
    "\\end{longtable}",
    "}"
  )
  writeLines(tbl1, file.path(tables_dir, "literature_families.tex"))

# Figure 1: corpus growth and the machine-learning share-------------------

  growth <- corpus %>%
    dplyr::filter(!is.na(.data$py)) %>%
    dplyr::count(.data$py, .data$is_ml, name = "n") %>%
    dplyr::rename(year = "py")

  p_growth <- ggplot2::ggplot(
      growth,
      ggplot2::aes(x = .data$year, y = .data$n,
                   fill = ifelse(.data$is_ml, "Machine learning", "Other"))) +
    ggplot2::geom_col(width = 0.8) +
    ggplot2::scale_fill_manual(values = c("Machine learning" = "#1f4e79",
                                          "Other" = "#bdc3c7"),
                               name = NULL) +
    ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(n = 8)) +
    ggplot2::labs(x = NULL, y = "Articles") +
    ggplot2::theme_minimal(base_size = 11, base_family = "Helvetica") +
    ggplot2::theme(
      legend.position = "top",
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank()
    )

  ggplot2::ggsave(file.path(figures_dir, "literature_growth.png"),
                  p_growth, width = 8, height = 4, dpi = 300)

  ml_share <- round(100 * mean(corpus$is_ml), 1)
  ml_share_recent <- round(
    100 * mean(corpus$is_ml[!is.na(corpus$py) & corpus$py >= 2020]), 1)

  fig1 <- c(
    "\\begin{figure}[htbp]",
    "\\centering",
    "\\captionsetup{justification=raggedright,labelsep=none,singlelinecheck=false,labelformat=boldlabel}",
    sprintf(paste0(
      "\\caption[Annual production of the reviewed literature]{\\footnotesize ",
      "\\textbf{: Annual production of the reviewed literature.} Number of ",
      "articles per publication year among the %d reviewed studies, split by ",
      "whether the abstract or author keywords contain a machine-learning term. ",
      "The term list is reported in the Internet Appendix. Machine-learning ",
      "studies are %.1f\\%% of the reviewed literature overall and %.1f\\%% of ",
      "the articles published from 2020 onwards.}"),
      corpus_stats$n_docs, ml_share, ml_share_recent),
    "\\includegraphics[width=\\linewidth]{figures/literature_growth.png}",
    "\\label{fig:litgrowth}",
    "\\end{figure}"
  )
  writeLines(fig1, file.path(figures_dir, "literature_growth_figure.tex"))

# Internet Appendix: full predictor inventory------------------------------

  ## The predictors catalogued during the review, with the study each was taken
  ## from and whether it enters the models. Sources are author-year labels
  ## rather than citation keys, because most of these studies are not otherwise
  ## cited in the paper and adding them would leave unused bibliography entries.
  tbl_ia <- c(
    "{\\footnotesize",
    ## longtable is not a float and so does not inherit the caption setup the
    ## table environments carry. Without this the label prints its own colon
    ## and the caption opens with a doubled separator.
    "\\captionsetup{justification=raggedright,labelsep=none,singlelinecheck=false,labelformat=boldlabel}",
    "\\setlength{\\tabcolsep}{3pt}",
    "\\renewcommand{\\arraystretch}{1.1}",
    ## Column widths sum to 12.8cm which, with eight inter-column gaps of 3pt,
    ## fits the text block. Widening any column overfulls every row.
    paste0("\\begin{longtable}{",
           ">{\\raggedright\\arraybackslash}m{0.7cm}",
           ">{\\raggedright\\arraybackslash}m{5.5cm}",
           ">{\\raggedright\\arraybackslash}m{5.1cm}",
           ">{\\raggedleft\\arraybackslash}m{1.5cm}}"),
    paste0("\\caption[Predictors of realized variance identified by the review]",
           "{\\footnotesize \\textbf{: Predictors of realized variance ",
           "identified by the review.} Every distinct predictor proposed in the ",
           sprintf("%d reviewed articles ", corpus_stats$n_docs),
           "and in the foundational work they build on, with the study it is ",
           "taken from and whether it enters the models estimated here. ",
           "Predictors are grouped into families in ",
           "Table~\\ref{tab:litfamilies}. Several entries map onto a single ",
           "constructed series, and the predictors marked absent are either ",
           "unavailable over the sample period or require intraday data.}",
           " \\label{tab:litpredictors} \\\\"),
    "\\hline",
    "ID & Predictor & Source & In this study \\\\",
    "\\hline",
    "\\endfirsthead",
    "\\hline",
    "ID & Predictor & Source & In this study \\\\",
    "\\hline",
    "\\endhead",
    "\\hline",
    "\\endfoot",
    sprintf("%s & %s & %s & %s \\\\",
            esc_tex(predictors$id), esc_tex(predictors$predictor),
            esc_tex(predictors$source), esc_tex(predictors$implemented)),
    "\\end{longtable}",
    "}"
  )
  writeLines(tbl_ia, file.path(tables_dir, "literature_predictors.tex"))

# Internet Appendix: bibliometric description------------------------------

  ## The standard descriptive apparatus of a bibliometric study: productivity by
  ## source and by country, the knowledge base measured by local citations, the
  ## co-citation and co-word networks, the conceptual structure, and the
  ## historical citation network. bibliometrix is heavy and is not part of the
  ## modelling dependency set, so when it is absent the script still writes
  ## valid .tex files and reports what could not be produced.
  bib_pngs <- c(
    sources    = file.path(figures_dir, "lit_sources.png"),
    countries  = file.path(figures_dir, "lit_countries.png"),
    localcited = file.path(figures_dir, "lit_localcited.png"),
    conceptual = file.path(figures_dir, "lit_conceptual.png"),
    cocitation = file.path(figures_dir, "lit_cocitation.png"),
    keywords   = file.path(figures_dir, "lit_keywords.png"),
    histnet    = file.path(figures_dir, "lit_histnetwork.png")
  )

  ## Shared look for the bar charts, so the appendix figures match Figure 1.
  bar_theme <- ggplot2::theme_minimal(base_size = 10, base_family = "Helvetica") +
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank(),
                   panel.grid.major.y = ggplot2::element_blank())

  ## Truncate long labels so the horizontal bars stay readable.
  clip <- function(x, n = 44) ifelse(nchar(x) > n, paste0(substr(x, 1, n - 1), "."), x)

  bar_plot <- function(labels, values, xlab) {
    ## Truncation can collapse two long labels onto the same string, and a
    ## factor cannot carry a duplicated level, so disambiguate before ordering.
    d <- data.frame(lab = make.unique(clip(labels), sep = " "), val = values)
    d$lab <- factor(d$lab, levels = rev(d$lab))
    ggplot2::ggplot(d, ggplot2::aes(x = .data$val, y = .data$lab)) +
      ggplot2::geom_col(fill = "#1f4e79", width = 0.75) +
      ggplot2::labs(x = xlab, y = NULL) +
      bar_theme
  }

  ## Any single panel can fail on a re-export without taking the rest with it.
  try_panel <- function(label, expr) {
    tryCatch(force(expr), error = function(e) {
      warning("Bibliometric panel '", label, "' failed: ", conditionMessage(e))
      NULL
    })
  }

  ## Most productive sources: a plain count, so no parsing package is needed.
  src <- sort(table(corpus[["Source title"]]), decreasing = TRUE)[1:15]
  ggplot2::ggsave(bib_pngs[["sources"]],
                  bar_plot(names(src), as.integer(src), "Articles"),
                  width = 7.2, height = 4.4, dpi = 300)

  if (requireNamespace("bibliometrix", quietly = TRUE)) {
    ## Attach rather than call through `::`. Several bibliometrix routines load
    ## their own reference tables with data(countries), data(stopwords) and
    ## data(logo), which resolve only when the package is on the search path.
    suppressPackageStartupMessages(library("bibliometrix"))

    M <- bibliometrix::convert2df(scopus_path, dbsource = "scopus", format = "csv")

    ## Corresponding-author country.
    try_panel("countries", {
      Mc <- bibliometrix::metaTagExtraction(M, Field = "AU1_CO", sep = ";")
      cty <- sort(table(trimws(Mc$AU1_CO)), decreasing = TRUE)
      cty <- cty[!is.na(names(cty)) & names(cty) != "NA"][1:15]
      cty_lab <- tools::toTitleCase(tolower(names(cty)))
      ## Title case mangles the country codes that are acronyms.
      cty_lab <- sub("^Usa$", "USA", cty_lab)
      cty_lab <- sub("^United Kingdom$", "United Kingdom", cty_lab)
      ggplot2::ggsave(bib_pngs[["countries"]],
                      bar_plot(cty_lab, as.integer(cty), "Articles"),
                      width = 7.2, height = 4.4, dpi = 300)
    })

    ## Knowledge base: references most cited by the reviewed articles. Scopus
    ## stores the same reference with small string variations, so the raw counts
    ## split one work across several rows. Collapse on a normalized key built
    ## from the leading authors and title words before ranking, otherwise the
    ## figure reports Corsi (2009) twice and understates every entry.
    try_panel("local citations", {
      cr  <- as.data.frame(bibliometrix::citations(M, field = "article", sep = ";"))
      ref <- as.character(cr[[1]])
      cnt <- as.integer(cr[[2]])
      key <- substr(gsub("[^A-Z]", "", toupper(ref)), 1, 38)

      agg <- stats::aggregate(list(n = cnt), by = list(key = key), FUN = sum)
      agg <- agg[order(-agg$n), ][1:15, ]
      ## Show the shortest surviving variant of each collapsed group.
      agg$label <- vapply(agg$key, function(k) {
        v <- ref[key == k]
        v[which.min(nchar(v))]
      }, character(1))
      agg$label <- sub(",\\s*\\(\\d{4}\\).*$", "", agg$label)   # drop year and outlet
      agg$label <- tools::toTitleCase(tolower(sub("\\.$", "", agg$label)))

      ggplot2::ggsave(bib_pngs[["localcited"]],
                      bar_plot(agg$label, agg$n,
                               "Citations from within the review set"),
                      width = 7.2, height = 4.6, dpi = 300)
    })

    ## Conceptual structure of the abstracts.
    try_panel("conceptual structure", {
      syns <- c("realized volatility;realised volatility;rv",
                "garch;egarch;arch",
                "implied volatility;iv;vix",
                "jump;cojump;signed jump",
                "machine learning;ml")
      cs <- bibliometrix::conceptualStructure(
        M, field = "AB", method = "MCA", ngrams = 2, minDegree = 2,
        clust = "auto", stemming = TRUE, documents = 100,
        remove.terms = c("model", "models", "analysis", "approach", "evidence"),
        synonyms = syns, graph = FALSE)
      ## conceptualStructure stamps the package logo onto the plot with a
      ## custom annotation. Strip it: a manuscript figure carries no software
      ## branding.
      g <- cs$graph_terms
      g$layers <- Filter(function(l) !inherits(l$geom, "GeomCustomAnn"), g$layers)
      ggplot2::ggsave(bib_pngs[["conceptual"]], g,
                      width = 7.6, height = 5.6, dpi = 300)
    })

    ## Co-citation and co-word networks.
    try_panel("co-citation network", {
      grDevices::png(bib_pngs[["cocitation"]], width = 2400, height = 1800, res = 300)
      cocit <- bibliometrix::biblioNetwork(M, analysis = "co-citation",
                                           network = "references", sep = ";")
      bibliometrix::networkPlot(cocit, n = 25, Title = "", type = "fruchterman",
                                size = TRUE, remove.multiple = FALSE,
                                normalize = "salton", labelsize = 0.7, edgesize = 5)
      grDevices::dev.off()
    })

    try_panel("keyword network", {
      grDevices::png(bib_pngs[["keywords"]], width = 2400, height = 1800, res = 300)
      kw <- bibliometrix::biblioNetwork(M, analysis = "co-occurrences",
                                        network = "keywords", sep = ";")
      bibliometrix::networkPlot(kw, weighted = TRUE, n = 25, normalize = "salton",
                                Title = "", type = "fruchterman", size = TRUE,
                                edgesize = 5, labelsize = 0.7)
      grDevices::dev.off()
    })

    ## Historical direct citation network: how the field developed in time.
    ## histPlot returns a ggplot object rather than drawing to the open device,
    ## so it has to be saved explicitly. Writing it to a png device produces an
    ## almost empty file.
    try_panel("historical citation network", {
      hs <- bibliometrix::histNetwork(M, sep = ";", verbose = FALSE)
      hp <- bibliometrix::histPlot(hs, n = 15, size = 6, labelsize = 3,
                                   verbose = FALSE)
      g  <- if (inherits(hp, "ggplot")) hp else hp$g
      ggplot2::ggsave(bib_pngs[["histnet"]], g, width = 9.5, height = 6.4,
                      dpi = 300)
    })
  } else {
    message("bibliometrix not installed: appendix network figures were not regenerated.")
  }

  ## Figure builders. Each writes a valid .tex whether or not its images were
  ## produced, so the manuscript always compiles.
  cap_head <- c(
    "\\begin{figure}[htbp]",
    "\\centering",
    "\\captionsetup{justification=raggedright,labelsep=none,singlelinecheck=false,labelformat=boldlabel}")

  panel <- function(file, caption, width = "0.86") c(
    "\\begin{subfigure}{\\linewidth}\\centering",
    sprintf("\\includegraphics[width=%s\\linewidth]{figures/%s}", width, file),
    sprintf("\\caption{%s}", caption),
    "\\end{subfigure}")

  write_fig <- function(out, keys, caption, label, panels) {
    if (all(file.exists(bib_pngs[keys]))) {
      writeLines(c(cap_head, caption, panels, sprintf("\\label{%s}", label),
                   "\\end{figure}"), file.path(figures_dir, out))
    } else {
      writeLines(c(sprintf("%%%% %s not generated: install bibliometrix and re-run", out),
                   "%% R/lit_review_outputs.R."), file.path(figures_dir, out))
    }
  }

  write_fig(
    "literature_production_figure.tex", c("sources", "countries"),
    paste0("\\caption[Productivity of the reviewed literature]{\\footnotesize ",
           "\\textbf{: Productivity of the reviewed literature.} Panel (a) reports the ",
           "fifteen sources publishing the most articles in the review set. Panel (b) ",
           "reports the fifteen most frequent corresponding-author countries. Long ",
           "source names are truncated.}"),
    "fig:litproduction",
    c(panel("lit_sources.png", "Most productive sources"),
      panel("lit_countries.png", "Corresponding-author country")))

  write_fig(
    "literature_networks_figure.tex", c("cocitation", "keywords"),
    paste0("\\caption[Bibliographic networks of the reviewed literature]{\\footnotesize ",
           "\\textbf{: Bibliographic networks of the reviewed literature.} Panel (a) is ",
           "the co-citation network of the 25 most co-cited references, which ",
           "identifies the knowledge base the reviewed articles draw on. Panel (b) is ",
           "the keyword co-occurrence network of the 25 most frequent terms. Both use ",
           "the Fruchterman-Reingold layout with Salton normalization; node size ",
           "is weighted degree and edge width is tie strength.}"),
    "fig:litnetworks",
    c(panel("lit_cocitation.png", "Co-citation network"),
      panel("lit_keywords.png", "Keyword co-occurrence network")))

  write_fig(
    "literature_structure_figure.tex", c("localcited", "conceptual"),
    paste0("\\caption[Intellectual and conceptual structure of the reviewed literature]",
           "{\\footnotesize \\textbf{: Intellectual and conceptual structure of the ",
           "reviewed literature.} Panel (a) reports the fifteen references most often ",
           "cited by the articles in the review set, which is the measure used in ",
           "Table~\\ref{tab:litfamilies}. The bibliographic database records the same ",
           "reference with small variations in spelling, so entries are merged on a ",
           "normalized key before counting; residual duplication across very similar ",
           "titles by the same authors cannot be ruled out. Panel (b) is the ",
           "conceptual structure map ",
           "obtained by multiple correspondence analysis of the abstracts, with terms ",
           "stemmed and grouped into two-word phrases and clusters assigned by ",
           "$k$-means.}"),
    "fig:litstructure",
    c(panel("lit_localcited.png", "Most locally cited references"),
      panel("lit_conceptual.png", "Conceptual structure map", "0.95")))

  write_fig(
    "literature_histnetwork_figure.tex", "histnet",
    paste0("\\caption[Historical direct citation network]{\\footnotesize ",
           "\\textbf{: Historical direct citation network.} Chronological map of the ",
           "direct citations among the fifteen most locally cited articles in the ",
           "review set. Arrows run from the citing article to the cited one, so the ",
           "figure traces how the field developed in time.}"),
    "fig:lithistnet",
    ## Single panel, so no subfigure wrapper and no stray "(a)" label.
    "\\includegraphics[width=0.98\\linewidth]{figures/lit_histnetwork.png}")

# Mirror the figures into the LaTeX tree-----------------------------------

  ## manuscript.tex resolves \includegraphics relative to latex_paper/, so the
  ## rendered images are copied there. figures/ remains the single place they
  ## are produced; latex_paper/figures is a build artifact.
  latex_fig_dir <- file.path(project_dir, "latex_paper", "figures")
  dir.create(latex_fig_dir, recursive = TRUE, showWarnings = FALSE)
  pngs <- list.files(figures_dir, pattern = "\\.png$", full.names = TRUE)
  ok <- file.copy(pngs, latex_fig_dir, overwrite = TRUE)
  if (!all(ok)) {
    warning("Failed to mirror figure(s) into latex_paper/figures: ",
            paste(basename(pngs[!ok]), collapse = ", "))
  }

# Report--------------------------------------------------------------------

  message(sprintf("Reviewed literature: %d articles, %d-%d, %d sources, ML share %.1f%%.",
                  corpus_stats$n_docs, corpus_stats$year_min,
                  corpus_stats$year_max, corpus_stats$n_sources, ml_share))
  message(sprintf("Families: %d of %d implemented. Predictors: %d of %d implemented.",
                  n_impl, nrow(families),
                  sum(predictors$implemented == "Yes"), nrow(predictors)))
  message(sprintf("Covariates reaching the models: %d, all matched to a family.",
                  n_model_vars))
