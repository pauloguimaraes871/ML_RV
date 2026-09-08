## Integration tests. These read the artifacts load_data.R has just written and
## check the properties the fitting stage relies on. They are run at the end of
## load_data.R, and can also be run on their own against whatever is in data/.
##
## A failure here usually means the artifacts on disk predate a preprocessing
## change and load_data.R needs to be re-run.
library("testthat")
`%>%` <- dplyr::`%>%`
source(file.path(here::here(), "R", "helpers.R"))

DATA  <- file.path(here::here(), "data")
MIN_N <- 36L

art <- function(f) file.path(DATA, f)
have <- function(f) file.exists(art(f))
read_art <- function(f) readRDS(art(f))

## Artifacts written before a preprocessing change cannot satisfy the tests for
## it. Exactly one test asserts the column is there, so it fails and says so;
## everything that depends on it skips rather than raising a subscript error.
has_level <- function(f) have(f) && "rv_level_lag1" %in% names(read_art(f))
skip_unless_rebuilt <- function(f) {
  testthat::skip_if_not(
    has_level(f),
    paste0(f, " predates the preprocessing fixes; re-run R/load_data.R")
  )
}

PANELS <- list(
  list(tag = "1986", raw = "covariates_panel_1986_v3.rds",
       range = "covariates_panel_1986_range_v3.rds",
       zscore = "covariates_panel_1986_zscore_v3.rds",
       spx_range = "covariates_spx_1986_range_v3.rds"),
  list(tag = "1927", raw = "covariates_panel_1927_v3.rds",
       range = "covariates_panel_1927_range_v3.rds",
       zscore = "covariates_panel_1927_zscore_v3.rds",
       spx_range = "covariates_spx_1927_range_v3.rds")
)

ALL_ARTIFACTS <- c(
  "covariates_panel_1986_v3.rds", "covariates_panel_1986_range_v3.rds",
  "covariates_panel_1986_zscore_v3.rds", "covariates_panel_1927_v3.rds",
  "covariates_panel_1927_range_v3.rds", "covariates_panel_1927_zscore_v3.rds",
  "covariates_spx_1986_range_v3.rds", "covariates_spx_1986_zscore_v3.rds",
  "covariates_spx_1927_range_v3.rds", "covariates_spx_1927_zscore_v3.rds",
  "target_panel_v3.rds", "target_spx_v3.rds"
)


# The artifacts exist ------------------------------------------------------

testthat::test_that("every V3 artifact load_data.R promises is on disk", {
  missing <- ALL_ARTIFACTS[!vapply(ALL_ARTIFACTS, have, logical(1))]
  testthat::expect_equal(missing, character(0))
})


# The warm-up is gone ------------------------------------------------------

for (p in PANELS) local({
  pp <- p

  testthat::test_that(paste0("panel ", pp$tag,
                             ": each key is shorter by exactly the warm-up"), {
    testthat::skip_if_not(have(pp$raw))
    skip_unless_rebuilt(pp$range)

    raw <- read_art(pp$raw)
    prp <- read_art(pp$range)

    n_raw <- table(raw$name)
    n_prp <- table(prp$name)

    ## Keys that had no more than the warm-up are dropped entirely.
    survivors <- names(n_raw)[n_raw > MIN_N]
    testthat::expect_setequal(names(n_prp), survivors)
    testthat::expect_equal(as.integer(n_prp[survivors]),
                           as.integer(n_raw[survivors]) - MIN_N)
  })

  testthat::test_that(paste0("panel ", pp$tag,
                             ": the first retained month is the one after the warm-up"), {
    testthat::skip_if_not(have(pp$raw))
    skip_unless_rebuilt(pp$range)

    raw <- read_art(pp$raw); prp <- read_art(pp$range)
    first_raw <- tapply(raw$month_id, raw$name, function(d) sort(d)[MIN_N + 1L])
    first_prp <- tapply(prp$month_id, prp$name, min)
    k <- intersect(names(first_raw), names(first_prp))

    testthat::expect_equal(unname(first_prp[k]), unname(first_raw[k]))
  })

  testthat::test_that(paste0("panel ", pp$tag,
                             ": no row is still on the natural scale"), {
    skip_unless_rebuilt(pp$range)

    prp <- read_art(pp$range)
    num <- setdiff(names(prp)[vapply(prp, is.numeric, logical(1))],
                   c("sqrt_days", "rv_level_lag1"))
    num <- num[!grepl("^m_[0-9]+$", num)]

    ## Before the fix the warm-up rows of rv_month_lag1 sat near 1e-03 while
    ## the scaled rows sat near -1. Nothing that wide should survive: an
    ## out-of-sample extreme can leave [-1, 1], but not by orders of magnitude.
    worst <- vapply(prp[num], function(x) max(abs(x), na.rm = TRUE), numeric(1))
    testthat::expect_true(all(worst < 50),
                          info = paste("widest columns:",
                                       paste(names(sort(worst, decreasing = TRUE))[1:3],
                                             collapse = ", ")))
  })
})


# The level column ---------------------------------------------------------

for (p in PANELS) local({
  pp <- p

  testthat::test_that(paste0("panel ", pp$tag,
                             ": rv_level_lag1 is present and unscaled"), {
    testthat::skip_if_not(have(pp$range))

    prp <- read_art(pp$range)
    testthat::expect_true("rv_level_lag1" %in% names(prp))
    skip_unless_rebuilt(pp$range)

    ## It carries log realized variance, so it must sit well below the scaled
    ## band. If it were inside [-1, 1] it would have been scaled by mistake.
    testthat::expect_lt(max(prp$rv_level_lag1), -1)
    testthat::expect_true(all(is.finite(prp$rv_level_lag1)))
  })

  testthat::test_that(paste0("panel ", pp$tag,
                             ": rv_level_lag1 equals the expanding mean of the logged term"), {
    testthat::skip_if_not(have(pp$raw))
    skip_unless_rebuilt(pp$range)

    raw <- read_art(pp$raw)[, c("month_id", "name", "rv_month_lag1")]
    raw <- raw[order(raw$name, raw$month_id), ]
    raw$expected <- ave(log(raw$rv_month_lag1), raw$name,
                        FUN = function(v) cumsum(v) / seq_along(v))
    raw <- raw[ave(seq_len(nrow(raw)), raw$name, FUN = seq_along) > MIN_N, ]

    prp <- read_art(pp$range)[, c("month_id", "name", "rv_level_lag1")]
    both <- merge(raw, prp, by = c("month_id", "name"))

    testthat::expect_equal(nrow(both), nrow(prp))
    testthat::expect_equal(both$rv_level_lag1, both$expected)
  })

  testthat::test_that(paste0("panel ", pp$tag,
                             ": rv_level_lag1 tracks how volatile each series really is"), {
    testthat::skip_if_not(have("target_panel_v3.rds"))
    skip_unless_rebuilt(pp$range)

    prp <- read_art(pp$range)
    tgt <- read_art("target_panel_v3.rds")
    tgt <- tgt[tgt$month_id >= min(prp$month_id), ]

    truth <- tapply(log(tgt$rv_month + 1e-08), tgt$name, mean)
    level <- tapply(prp$rv_level_lag1, prp$name, mean)
    k <- intersect(names(truth), names(level))

    ## Before the level column existed the pooled forecast correlated -0.244
    ## with this. The column itself must do far better than that.
    testthat::expect_gt(stats::cor(level[k], truth[k]), 0.85)

    ## And it must spread the series out rather than flatten them.
    testthat::expect_gt(stats::sd(level[k]), 0.5 * stats::sd(truth[k]))
  })

  testthat::test_that(paste0("panel ", pp$tag,
                             ": the market key reads as more volatile than a typical factor"), {
    skip_unless_rebuilt(pp$range)

    prp <- read_art(pp$range)
    testthat::skip_if_not("spx_vw" %in% prp$name)

    level <- tapply(prp$rv_level_lag1, prp$name, mean)
    testthat::expect_gt(level[["spx_vw"]], stats::median(level))
  })
})


# The log transform --------------------------------------------------------

testthat::test_that("the variance-family columns are logged, not left skewed", {
  skip_unless_rebuilt("covariates_panel_1986_range_v3.rds")

  prp <- read_art("covariates_panel_1986_range_v3.rds")
  check <- intersect(c("rv_month_lag1", "rv_tw_lag1", "rq_month_lag1",
                       "cont_month_lag1", "vix_lag1", "oil_rv_month_lag1"),
                     names(prp))

  for (v in check) {
    at_floor <- mean(prp[[v]] <= min(prp[[v]]) + 0.02)
    testthat::expect_lt(at_floor, 0.10)
    testthat::expect_gt(stats::median(prp[[v]]), -0.80)
  }
})

testthat::test_that("the jump daily term keeps its zeros rather than gaining -Inf", {
  skip_unless_rebuilt("covariates_panel_1986_range_v3.rds")

  prp <- read_art("covariates_panel_1986_range_v3.rds")
  testthat::skip_if_not("jump_td_lag1" %in% names(prp))

  ## It is exactly zero most months, so a large floor share is the data and not
  ## a defect. What matters is that nothing became non-finite.
  testthat::expect_true(all(is.finite(prp$jump_td_lag1)))
})


# Nothing broke ------------------------------------------------------------

testthat::test_that("no PREPROCESSED artifact carries an NA or a non-finite value", {
  ## The raw panel artifacts are excluded on purpose: they are the input to
  ## prep_covariates and still hold the gaps that impute_locf fills.
  prepped <- grep("_(range|zscore)_v3[.]rds$", ALL_ARTIFACTS, value = TRUE)
  for (f in prepped) {
    if (!have(f)) next
    d <- read_art(f)
    num <- d[vapply(d, is.numeric, logical(1))]
    testthat::expect_true(all(vapply(num, function(x) all(is.finite(x)), logical(1))),
                          info = f)
  }
})

testthat::test_that("the range and z-score builds cover the same rows", {
  for (p in PANELS) {
    if (!have(p$range) || !have(p$zscore)) next
    a <- read_art(p$range); b <- read_art(p$zscore)
    testthat::expect_equal(nrow(a), nrow(b), info = p$tag)
    testthat::expect_setequal(names(a), names(b))
    testthat::expect_equal(a$month_id, b$month_id, info = p$tag)
    testthat::expect_equal(a$name, b$name, info = p$tag)
  }
})

testthat::test_that("the market key inside the panel is untouched by the factors", {
  for (p in PANELS) {
    if (!have(p$range) || !have(p$spx_range)) next
    panel <- read_art(p$range); spx <- read_art(p$spx_range)

    shared <- intersect(names(panel), names(spx))
    lhs <- as.data.frame(panel %>% dplyr::filter(name == "spx_vw") %>%
                           dplyr::arrange(month_id) %>%
                           dplyr::select(dplyr::all_of(shared)))
    rhs <- as.data.frame(spx %>% dplyr::arrange(month_id) %>%
                           dplyr::select(dplyr::all_of(shared)))
    row.names(lhs) <- NULL; row.names(rhs) <- NULL

    testthat::expect_equal(lhs, rhs, info = p$tag)
  }
})

testthat::test_that("`name` is a key, never a feature", {
  for (f in ALL_ARTIFACTS) {
    if (!have(f) || !grepl("^covariates_", f)) next
    d <- read_art(f)
    ## The S&P frames carry no key at all, which satisfies this trivially.
    if (!"name" %in% names(d)) next
    testthat::expect_false(is.numeric(d$name), info = f)
  }
})

testthat::test_that("every covariate row has a target to match", {
  for (p in PANELS) {
    if (!have(p$range) || !have("target_panel_v3.rds")) next
    cov <- read_art(p$range); tgt <- read_art("target_panel_v3.rds")
    joined <- merge(cov[, c("month_id", "name")], tgt, by = c("month_id", "name"))
    testthat::expect_equal(nrow(joined), nrow(cov), info = p$tag)
  }
})
