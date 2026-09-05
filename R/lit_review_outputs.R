# Literature review outputs (paper section 2)-----------------------------------

  ## This script produces every table and figure that Section 2 of the
  ## manuscript refers to. It reads two sources and writes nothing else:
  ##   - bibliometrics/scopus_final.csv : the systematic review corpus
  ##   - data/*_v3.rds, data/manifest_v3.csv : the V3 feature artifacts
  ## It never fits a model and never alters an artifact under data/.
  ##
  ## Outputs
  ##   tables/literature_corpus.tex      main paper, Table 1
  ##   tables/feature_map.tex            main paper, Table 2
  ##   tables/literature_predictors.tex  Internet Appendix
  ##   figures/literature_growth.png     main paper, Figure 1
  ##   figures/literature_growth_figure.tex
  ##   figures/literature_networks_figure.tex  Internet Appendix

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

  ## Scopus stores authors as "Surname A.B.; Surname C." Reduce to the
  ## author-year label used in the manuscript prose.
  short_cite <- function(authors, year) {
    parts <- strsplit(authors, ";", fixed = TRUE)
    vapply(seq_along(parts), function(i) {
      a <- trimws(parts[[i]])
      ## Scopus writes "Surname A.B." and "Trierweiler Ribeiro G.". The final
      ## whitespace-separated token is always the initials, so dropping it
      ## recovers compound surnames as well as simple ones.
      surname <- function(s) {
        tok <- strsplit(trimws(s), "\\s+")[[1]]
        if (length(tok) <= 1L) trimws(s) else paste(tok[-length(tok)], collapse = " ")
      }
      lab <- if (length(a) == 1L) {
        surname(a[1])
      } else if (length(a) == 2L) {
        paste(surname(a[1]), "and", surname(a[2]))
      } else {
        paste0(surname(a[1]), " et al.")
      }
      sprintf("%s (%s)", lab, year[i])
    }, character(1))
  }

# Corpus--------------------------------------------------------------------

  scopus_path <- file.path(project_dir, "bibliometrics", "scopus_final.csv")
  if (!file.exists(scopus_path)) {
    stop("Missing systematic review corpus: ", scopus_path)
  }

  corpus <- utils::read.csv(scopus_path, check.names = FALSE,
                            stringsAsFactors = FALSE)

  ## Scopus writes the citation count as "Cited by" and leaves it empty for
  ## uncited documents. Treat empty as zero rather than dropping the row.
  corpus$tc <- suppressWarnings(as.integer(corpus[["Cited by"]]))
  corpus$tc[is.na(corpus$tc)] <- 0L
  corpus$py <- suppressWarnings(as.integer(corpus[["Year"]]))

  corpus_stats <- list(
    n_docs    = nrow(corpus),
    year_min  = min(corpus$py, na.rm = TRUE),
    year_max  = max(corpus$py, na.rm = TRUE),
    n_sources = length(unique(corpus[["Source title"]])),
    mean_tc   = mean(corpus$tc)
  )

  ## Machine-learning subset, flagged on the abstract with the keyword list
  ## used in the exploratory bibliometric report. Kept identical so the two
  ## documents cannot diverge.
  ml_keywords <- c("machine learning", "neural network", "deep learning",
                   "lstm", "gru", "svm", "lasso", "random forest", "boosting",
                   "ensemble", "recurrent", "transformer", "gradient boosting",
                   "reinforcement learning", "support vector")
  abstracts <- tolower(paste(corpus[["Abstract"]], corpus[["Author Keywords"]]))
  corpus$is_ml <- Reduce(`|`, lapply(ml_keywords, function(k)
    grepl(k, abstracts, fixed = TRUE)))

# Table 1: most cited studies in the corpus--------------------------------

  ## The predictor family each study contributes to the feature universe.
  ## Keyed on DOI so that a re-export of the corpus cannot silently reassign
  ## a label to the wrong row.
  family_by_doi <- c(
    "10.1016/s0304-405x(98)00034-8"   = "Option-implied volatility",
    "10.1016/j.jeconom.2014.05.008"   = "Implied volatility, variance premium",
    "10.1016/j.frl.2015.08.009"       = "Policy uncertainty",
    "10.1016/S0304-4076(01)00068-9"   = "Option-implied volatility",
    "10.1016/j.jfineco.2012.06.005"   = "Macroeconomic and credit conditions",
    "10.1016/j.jeconom.2015.10.007"   = "Realized quarticity",
    "10.1111/eufm.12058"              = "Search-engine attention",
    "10.1016/j.ijforecast.2019.05.010" = "Sentiment and attention",
    "10.1093/jjfinec/nbi028"          = "Realized variance measurement",
    "10.1016/j.jeconom.2008.09.032"   = "Regimes, announcements, calendar",
    "10.1016/j.ijforecast.2020.02.007" = "Global volatility factors",
    "10.1002/for.2686"                = "International implied volatility",
    "10.1093/jjfinec/nbaa008"         = "Macro-financial predictors (ML)",
    "10.1016/j.jempfin.2017.03.003"   = "Macro-financial predictors",
    "10.1016/j.irfa.2020.101552"      = "Sentiment indices"
  )

  top_n <- 12L
  top_tbl <- corpus %>%
    dplyr::arrange(dplyr::desc(.data$tc)) %>%
    dplyr::slice_head(n = top_n) %>%
    dplyr::transmute(
      study   = short_cite(.data$Authors, .data$py),
      journal = .data[["Source title"]],
      tc      = .data$tc,
      family  = unname(family_by_doi[.data$DOI])
    )
  top_tbl$family[is.na(top_tbl$family)] <- "Not used in the feature universe"

  tbl1 <- c(
    "\\begin{table}[pt]",
    "\\centering",
    "\\captionsetup{justification=raggedright,labelsep=none,singlelinecheck=false,labelformat=boldlabel}",
    "\\renewcommand{\\arraystretch}{1.15}",
    sprintf(paste0(
      "\\caption[Most cited studies in the systematic review corpus]{\\footnotesize ",
      "\\textbf{: Most cited studies in the systematic review corpus.} The corpus ",
      "contains %d articles published between %d and %d across %d sources, ",
      "retrieved from Scopus under the protocol described in Section~\\ref{sec:literature} ",
      "and detailed in the Internet Appendix. Studies are ordered by total ",
      "citations recorded in Scopus at the export date. The last column reports ",
      "the predictor family the study contributes to the feature universe of this ",
      "paper, and Table~\\ref{tab:featuremap} maps those families onto the ",
      "variables actually constructed.}"),
      corpus_stats$n_docs, corpus_stats$year_min, corpus_stats$year_max,
      corpus_stats$n_sources),
    "\\footnotesize",
    "\\setlength{\\tabcolsep}{3pt}",
    paste0("\\begin{tabularx}{\\linewidth}{",
           ">{\\raggedright\\arraybackslash}m{3.6cm}",
           ">{\\raggedright\\arraybackslash}X",
           ">{\\raggedleft\\arraybackslash}m{1.1cm}",
           ">{\\raggedright\\arraybackslash}m{4.1cm}}"),
    "\\hline",
    "Study & Journal & Cites & Predictor family contributed \\\\",
    "\\hline",
    sprintf("%s & %s & %d & %s \\\\",
            esc_tex(top_tbl$study), esc_tex(top_tbl$journal),
            top_tbl$tc, esc_tex(top_tbl$family)),
    "\\hline",
    "\\end{tabularx}",
    "\\label{tab:litcorpus}",
    "\\end{table}"
  )
  writeLines(tbl1, file.path(tables_dir, "literature_corpus.tex"))

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
      "\\caption[Annual production of the systematic review corpus]{\\footnotesize ",
      "\\textbf{: Annual production of the systematic review corpus.} Number of ",
      "articles per publication year in the %d-document corpus, split by whether ",
      "the abstract or author keywords contain a machine-learning term. The ",
      "keyword list is reported in the Internet Appendix. Machine-learning ",
      "studies represent %.1f\\%% of the corpus overall and %.1f\\%% of the ",
      "articles published from 2020 onwards.}"),
      corpus_stats$n_docs, ml_share, ml_share_recent),
    "\\includegraphics[width=\\linewidth]{figures/literature_growth.png}",
    "\\label{fig:litgrowth}",
    "\\end{figure}"
  )
  writeLines(fig1, file.path(figures_dir, "literature_growth_figure.tex"))

# Table 2: from the review to the feature universe-------------------------

  ## Availability tiers are read from the V3 artifacts rather than asserted,
  ## so the table cannot drift away from what the models actually receive.
  panel_paths <- c(
    "1927" = file.path(project_dir, "data", "covariates_panel_1927_v3.rds"),
    "1986" = file.path(project_dir, "data", "covariates_panel_1986_v3.rds")
  )
  missing_panels <- panel_paths[!file.exists(panel_paths)]
  if (length(missing_panels) > 0L) {
    stop("Missing V3 covariate panel(s):\n", paste(missing_panels, collapse = "\n"))
  }

  key_cols <- c("name", "month_id")
  feat_1927 <- setdiff(names(readRDS(panel_paths[["1927"]])), key_cols)
  feat_1986 <- setdiff(names(readRDS(panel_paths[["1986"]])), key_cols)

  ## Curated mapping from the predictor families identified in the review to
  ## the variables constructed in this paper. Families the review identified
  ## but that this paper does not implement are retained with an explicit
  ## reason, because their absence is itself a design choice a referee will
  ## want to see justified.
  feature_map <- tibble::tribble(
    ~family,                              ~sources,                                          ~vars,
    "Lagged realized variance (HAR)",     "corsi2009; andersenbollerslev1998",               "rv_td_lag1, rv_tw_lag1, rv_month_lag1",
    "Realized quarticity",                "bollerslevpattonquaedvlieg2016",                  "rq_td_lag1, rq_tw_lag1, rq_month_lag1",
    "Jump and continuous components",     "andersenbollerslevdiebold2007",                   "jump_td_lag1, jump_tw_lag1, jump_month_lag1, cont_td_lag1, cont_tw_lag1, cont_month_lag1",
    "Leverage effect",                    "campbellhentschel1992; corsireno2012",            "negret_td_lag1, negret_tw_lag1, negret_month_lag1",
    "Policy uncertainty",                 "liuzhang2015",                                    "log_epu_diff_lag1",
    "Producer price volatility",          "schwert1989; paye2012",                           "ppi_vol",
    "Industrial production volatility",   "schwert1989",                                     "ip_vol",
    "Equity valuation ratios",            "welchgoyal2008; audrinosigristballinari2020",     "dy_lag1, ey_lag1",
    "Geopolitical risk",                  "caldaraiacoviello2022",                           "gpr_lag1",
    "Calendar effects",                   "schwert1989; mcaleermedeiros2008",                "month, sqrt_days",
    "Credit spreads",                     "schwert1989; paye2012; welchgoyal2008",           "dfr_lag1, dfy_lag1",
    "Term structure",                     "paye2012; welchgoyal2008",                        "tms_lag1, tbill_lag1",
    "Monetary base volatility",           "schwert1989",                                     "mbas_vol",
    "Survey sentiment",                   "audrinosigristballinari2020",                     "log_cons_sent_diff_lag1",
    "Cross-asset realized variance",      "degiannakisfilis2017",                            "oil_rv_td_lag1, oil_rv_tw_lag1, oil_rv_month_lag1",
    "Option-implied volatility",          "christensenprabhala1998; blairpoontaylor2001; bekaerthoerova2014", "vix_lag1"
  )

  ## Assign each family to the earliest panel that carries all of its
  ## variables, and fail loudly if a mapped variable exists in neither.
  classify_tier <- function(vars_string) {
    v <- trimws(strsplit(vars_string, ",", fixed = TRUE)[[1]])
    unknown <- setdiff(v, union(feat_1927, feat_1986))
    if (length(unknown) > 0L) {
      stop("feature_map refers to variables absent from every V3 panel: ",
           paste(unknown, collapse = ", "))
    }
    if (all(v %in% feat_1927)) "1927" else "1986"
  }
  feature_map$tier <- vapply(feature_map$vars, classify_tier, character(1))

  ## Families present in the review but deliberately not implemented.
  not_implemented <- tibble::tribble(
    ~family,                            ~sources,                          ~reason,
    "Limit order book measures",        "rahimikiapoon2026",               "Not available at monthly frequency over the sample",
    "News and social media sentiment",  "audrinosigristballinari2020",     "Series begin in the 2010s",
    "Search-engine attention",          "dimpfljank2016",                  "Series begin in 2004",
    "International volatility spillovers", "zhangmaliao2020; liangweizhang2020", "Reserved for the factor panel rather than the market series",
    "Topological and decomposition features", "souto2023",                 "Requires intraday cross-sections unavailable before 1993"
  )

  ## Every mapped variable must be accounted for, otherwise the table
  ## understates the feature universe.
  mapped_vars <- unlist(lapply(feature_map$vars, function(s)
    trimws(strsplit(s, ",", fixed = TRUE)[[1]])), use.names = FALSE)
  unmapped <- setdiff(union(feat_1927, feat_1986), mapped_vars)
  if (length(unmapped) > 0L) {
    warning("V3 variables absent from feature_map: ", paste(unmapped, collapse = ", "))
  }

  cite_of <- function(x) {
    sprintf("\\citet{%s}", gsub("; ", ",", x, fixed = TRUE))
  }

  tbl2 <- c(
    "\\begin{table}[pt]",
    "\\centering",
    "\\captionsetup{justification=raggedright,labelsep=none,singlelinecheck=false,labelformat=boldlabel}",
    "\\renewcommand{\\arraystretch}{1.15}",
    sprintf(paste0(
      "\\caption[From the systematic review to the feature universe]{\\footnotesize ",
      "\\textbf{: From the systematic review to the feature universe.} Each row is ",
      "a predictor family identified in the %d-document review corpus. `Variables' ",
      "lists the names of the series constructed in this paper, and `From' reports ",
      "the earliest monthly panel that carries the whole family, read directly from ",
      "the V3 covariate artifacts rather than asserted. Panel~B lists families the ",
      "review identified that this paper does not implement, with the reason. The ",
      "complete inventory of %d predictors catalogued in the review is reported in ",
      "Table~\\ref{tab:litpredictors}.}"),
      corpus_stats$n_docs, 83L),
    "\\footnotesize",
    "\\setlength{\\tabcolsep}{3pt}",
    paste0("\\begin{tabularx}{\\linewidth}{",
           ">{\\raggedright\\arraybackslash}m{3.5cm}",
           ">{\\raggedright\\arraybackslash}m{3.9cm}",
           ">{\\raggedright\\arraybackslash}X",
           ">{\\raggedleft\\arraybackslash}m{0.9cm}}"),
    "\\hline",
    "\\multicolumn{4}{l}{\\textit{Panel A: families implemented}} \\\\",
    "\\hline",
    "Predictor family & Representative sources & Variables & From \\\\",
    "\\hline",
    sprintf("%s & %s & \\texttt{%s} & %s \\\\",
            esc_tex(feature_map$family),
            cite_of(feature_map$sources),
            esc_tex(feature_map$vars),
            feature_map$tier),
    "\\hline",
    "\\multicolumn{4}{l}{\\textit{Panel B: families identified but not implemented}} \\\\",
    "\\hline",
    "Predictor family & Representative sources & \\multicolumn{2}{l}{Reason} \\\\",
    "\\hline",
    sprintf("%s & %s & \\multicolumn{2}{p{5.2cm}}{%s} \\\\",
            esc_tex(not_implemented$family),
            cite_of(not_implemented$sources),
            esc_tex(not_implemented$reason)),
    "\\hline",
    "\\end{tabularx}",
    "\\label{tab:featuremap}",
    "\\end{table}"
  )
  writeLines(tbl2, file.path(tables_dir, "feature_map.tex"))

# Internet Appendix: full predictor inventory------------------------------

  ## The 83 predictors catalogued during the review, with the study each was
  ## taken from. Sources are given as author-year labels rather than \citet
  ## keys, because most of these studies are not otherwise cited in the paper
  ## and adding them to references.bib would leave unused entries.
  predictors_path <- file.path(project_dir, "bibliometrics",
                               "review_predictors.csv")
  if (!file.exists(predictors_path)) {
    stop("Missing predictor inventory: ", predictors_path,
         "\nGenerate it from the bibliometric report before running this script.")
  }
  predictors <- utils::read.csv(predictors_path, check.names = FALSE,
                                stringsAsFactors = FALSE)

  tbl_ia <- c(
    "{\\footnotesize",
    "\\setlength{\\tabcolsep}{3pt}",
    "\\renewcommand{\\arraystretch}{1.1}",
    ## Column widths sum to 13.1cm, which keeps the longtable inside the
    ## elsarticle preprint text block. Widening any column overfulls the row.
    paste0("\\begin{longtable}{",
           ">{\\raggedright\\arraybackslash}m{0.8cm}",
           ">{\\raggedright\\arraybackslash}m{6.7cm}",
           ">{\\raggedright\\arraybackslash}m{5.55cm}}"),
    paste0("\\caption[Predictors of realized variance identified in the review]",
           "{\\footnotesize \\textbf{: Predictors of realized variance identified ",
           "in the review.} Every distinct predictor proposed in the ",
           sprintf("%d-document ", corpus_stats$n_docs),
           "corpus, with the study it is taken from. Families implemented in this ",
           "paper are mapped in Table~\\ref{tab:featuremap}.}",
           " \\label{tab:litpredictors} \\\\"),
    "\\hline",
    "ID & Predictor & Source \\\\",
    "\\hline",
    "\\endfirsthead",
    "\\hline",
    "ID & Predictor & Source \\\\",
    "\\hline",
    "\\endhead",
    "\\hline",
    "\\endfoot",
    sprintf("%s & %s & %s \\\\",
            esc_tex(predictors[[1]]), esc_tex(predictors[[2]]),
            esc_tex(predictors[[3]])),
    "\\end{longtable}",
    "}"
  )
  writeLines(tbl_ia, file.path(tables_dir, "literature_predictors.tex"))

# Internet Appendix: bibliometric networks---------------------------------

  ## bibliometrix is heavy and is not part of the modelling dependency set.
  ## When it is absent the script still writes a valid .tex so the manuscript
  ## compiles, and reports what could not be produced.
  network_pngs <- c(
    cocitation = file.path(figures_dir, "lit_cocitation.png"),
    keywords   = file.path(figures_dir, "lit_keywords.png")
  )

  if (requireNamespace("bibliometrix", quietly = TRUE)) {
    M <- bibliometrix::convert2df(scopus_path, dbsource = "scopus", format = "csv")

    grDevices::png(network_pngs[["cocitation"]], width = 2400, height = 1800, res = 300)
    cocit <- bibliometrix::biblioNetwork(M, analysis = "co-citation",
                                         network = "references", sep = ";")
    bibliometrix::networkPlot(cocit, n = 25, Title = "", type = "fruchterman",
                              size = TRUE, remove.multiple = FALSE,
                              normalize = "salton", labelsize = 0.7, edgesize = 5)
    grDevices::dev.off()

    grDevices::png(network_pngs[["keywords"]], width = 2400, height = 1800, res = 300)
    kw <- bibliometrix::biblioNetwork(M, analysis = "co-occurrences",
                                      network = "keywords", sep = ";")
    bibliometrix::networkPlot(kw, weighted = TRUE, n = 25, normalize = "salton",
                              Title = "", type = "fruchterman", size = TRUE,
                              edgesize = 5, labelsize = 0.7)
    grDevices::dev.off()
  } else {
    message("bibliometrix not installed: network figures were not regenerated.")
  }

  if (all(file.exists(network_pngs))) {
    fig_net <- c(
      "\\begin{figure}[htbp]",
      "\\centering",
      "\\captionsetup{justification=raggedright,labelsep=none,singlelinecheck=false,labelformat=boldlabel}",
      paste0("\\caption[Bibliographic networks of the review corpus]{\\footnotesize ",
             "\\textbf{: Bibliographic networks of the review corpus.} Panel (a) is ",
             "the co-citation network of the 25 most co-cited references, which ",
             "identifies the knowledge base the corpus draws on. Panel (b) is the ",
             "keyword co-occurrence network of the 25 most frequent terms. Both use ",
             "the Fruchterman-Reingold layout with Salton normalization; node size ",
             "is weighted degree and edge width is tie strength.}"),
      "\\begin{subfigure}{\\linewidth}\\centering",
      "\\includegraphics[width=0.86\\linewidth]{figures/lit_cocitation.png}",
      "\\caption{Co-citation network}",
      "\\end{subfigure}",
      "\\begin{subfigure}{\\linewidth}\\centering",
      "\\includegraphics[width=0.86\\linewidth]{figures/lit_keywords.png}",
      "\\caption{Keyword co-occurrence network}",
      "\\end{subfigure}",
      "\\label{fig:litnetworks}",
      "\\end{figure}"
    )
  } else {
    fig_net <- c(
      "%% Network figures not yet generated: install bibliometrix and re-run",
      "%% R/lit_review_outputs.R."
    )
  }
  writeLines(fig_net, file.path(figures_dir, "literature_networks_figure.tex"))

# Mirror the figures into the LaTeX tree-----------------------------------

  ## manuscript.tex resolves \includegraphics relative to latex_paper/, so the
  ## rendered PNGs are copied there. figures/ remains the single place they are
  ## produced; latex_paper/figures is a build artifact.
  latex_fig_dir <- file.path(project_dir, "latex_paper", "figures")
  dir.create(latex_fig_dir, recursive = TRUE, showWarnings = FALSE)
  pngs <- list.files(figures_dir, pattern = "\\.png$", full.names = TRUE)
  ok <- file.copy(pngs, latex_fig_dir, overwrite = TRUE)
  if (!all(ok)) {
    warning("Failed to mirror figure(s) into latex_paper/figures: ",
            paste(basename(pngs[!ok]), collapse = ", "))
  }

# Report--------------------------------------------------------------------

  message(sprintf(
    "Corpus: %d documents, %d-%d, %d sources, mean citations %.1f, ML share %.1f%%.",
    corpus_stats$n_docs, corpus_stats$year_min, corpus_stats$year_max,
    corpus_stats$n_sources, corpus_stats$mean_tc, ml_share))
  message(sprintf("Feature universe: %d variables from 1927, %d from 1986.",
                  length(feat_1927), length(feat_1986)))
