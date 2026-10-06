## Tests for the DeMiguel factor dataset (R/demiguel_helpers.R and the
## artifacts written by R/load_demiguel_data.R).
##
##   1. readers and checks on synthetic inputs: units, keys, malformed files
##   2. the provider files on disk: coverage, Table I, monthly/daily agreement
##   3. the built artifacts: target definition, entry rule, key isolation,
##      shared covariates identical to the JKP panel, manifest hashes
##
## load_demiguel_data.R runs this file after writing and refuses to finish if
## it fails. Sections 2 and 3 skip when their inputs are absent.

library("testthat")
`%>%` <- dplyr::`%>%`
source(file.path(here::here(), "R", "helpers.R"))
source(file.path(here::here(), "R", "portfolio_helpers.R"))
source(file.path(here::here(), "R", "demiguel_helpers.R"))

data_dir <- file.path(here::here(), "data")
raw_dir <- file.path(data_dir, "demiguel", "raw")


# 1. Readers and checks ========================================================

french_fixture <- c(
  "This file was created using the 202608 CRSP database.",
  "",
  ",Mkt-RF,SMB,RF",
  "196307,   -0.39,   -0.48,    0.27",
  "196308,    5.08,   -0.80,    0.25",
  "",
  " Annual Factors: January-December ",
  ",Mkt-RF,SMB,RF",
  "1964,   10.00,    1.00,    3.00"
)

testthat::test_that("French lines: monthly block only, percent to decimal, clean names", {
  x <- parse_french_lines(french_fixture, "monthly")
  testthat::expect_equal(names(x), c("month_id", "mkt_rf", "smb", "rf"))
  testthat::expect_equal(x$month_id, as.Date(c("1963-07-01", "1963-08-01")))
  testthat::expect_equal(x$mkt_rf, c(-.0039, .0508))
  testthat::expect_equal(x$rf, c(.0027, .0025))
})

testthat::test_that("French lines: daily stamps and malformed files fail", {
  daily <- c(",Mom", "19261103,  0.56", "19261104, -0.50")
  x <- parse_french_lines(daily, "daily")
  testthat::expect_equal(x$date, as.Date(c("1926-11-03", "1926-11-04")))
  testthat::expect_equal(x$mom, c(.0056, -.005))
  testthat::expect_error(parse_french_lines(daily, "monthly"), "No monthly records")
  testthat::expect_error(parse_french_lines(c(",A", "196301, -99.99"), "monthly"), "Missing-value")
  testthat::expect_error(parse_french_lines(c(",A,B", "196301, 1"), "monthly"), "Ragged")
  testthat::expect_error(parse_french_lines(c(",A", "196301, 1", "x", "196302, 1"), "monthly"),
                         "Non-contiguous")
  testthat::expect_error(parse_french_lines(c(",A", "196301, 1", "196303, 1"), "monthly"),
                         "consecutive")
})

testthat::test_that("q5 reader converts percent and builds first-of-month keys", {
  path <- tempfile(fileext = ".csv")
  utils::write.csv(data.frame(year = c(1967, 1967), month = c(1, 2), R_F = .39,
    R_MKT = 8.19, R_ME = 6.8, R_IA = -2.92, R_ROE = 1.89, R_EG = -2.55),
    path, row.names = FALSE)
  x <- read_hxz_q5(path, "monthly")
  testthat::expect_equal(x$month_id, as.Date(c("1967-01-01", "1967-02-01")))
  testthat::expect_equal(x$r_roe, c(.0189, .0189))
  testthat::expect_equal(x$r_ia, c(-.0292, -.0292))
})

testthat::test_that("Sharpe check annualizes with sqrt(12) and needs a full window", {
  months <- seq(as.Date("2000-01-01"), by = "month", length.out = 24)
  i <- seq_along(months)
  data <- tidyr::expand_grid(name = demiguel_factor_names, month_id = months) %>%
    dplyr::mutate(ret = .01 + .02 * sin(as.integer(month_id) / 50 + nchar(name)))
  check <- check_demiguel_sharpe(data, months[1], months[24], tolerance = 10)
  manual <- data %>% dplyr::filter(name == "umd")
  testthat::expect_equal(check$sharpe[check$name == "umd"],
                         sqrt(12) * mean(manual$ret) / stats::sd(manual$ret))
  testthat::expect_equal(check$name, demiguel_factor_names)
  testthat::expect_error(check_demiguel_sharpe(data, months[1], as.Date("2002-06-01")),
                         "every month")
})


# 2. Provider files on disk ====================================================

have_raw <- dir.exists(raw_dir) &&
  length(list.files(raw_dir, pattern = "\\.(zip|csv|xlsx)$")) >= 10L

if (have_raw) {

  monthly <- build_demiguel_returns(raw_dir, "monthly")
  daily <- build_demiguel_returns(raw_dir, "daily")

  testthat::test_that("each factor keeps its own provider history", {
    starts <- monthly$returns %>% dplyr::group_by(name) %>%
      dplyr::summarise(start = min(month_id), .groups = "drop")
    expected <- c(mkt = "1926-07-01", smb = "1926-07-01", hml = "1926-07-01",
                  umd = "1927-01-01", bab = "1930-12-01", rmw = "1963-07-01",
                  cma = "1963-07-01", roe = "1967-01-01", ia = "1967-01-01")
    testthat::expect_equal(starts$start[match(names(expected), starts$name)],
                           as.Date(unname(expected)))
    testthat::expect_setequal(unique(daily$returns$name), demiguel_factor_names)
    testthat::expect_false("spx_vw" %in% daily$returns$name)
  })

  testthat::test_that("unmanaged Sharpe ratios match Table I within 0.03", {
    check <- check_demiguel_sharpe(monthly$returns)
    testthat::expect_true(all(check$within_tolerance))
    testthat::expect_true(all(check$n == 528L))
  })

  testthat::test_that("monthly provider returns agree with compounded daily returns", {
    ## Not identical by construction, but a unit or alignment error would show
    ## up as a collapse in correlation. French's daily momentum is rebalanced
    ## DAILY, so UMD gets a looser bound (observed 0.93 at vintage 202608).
    agreement <- daily$returns %>%
      dplyr::mutate(month_id = lubridate::floor_date(date, "month")) %>%
      dplyr::group_by(name, month_id) %>%
      dplyr::summarise(compounded = prod(1 + ret) - 1, .groups = "drop") %>%
      dplyr::inner_join(monthly$returns, by = c("name", "month_id")) %>%
      dplyr::group_by(name) %>%
      dplyr::summarise(rho = stats::cor(compounded, ret), .groups = "drop")
    testthat::expect_true(all(agreement$rho[agreement$name != "umd"] > .99))
    testthat::expect_gt(agreement$rho[agreement$name == "umd"], .9)
  })

  testthat::test_that("French files share one vintage; the source switch matters only for SMB", {
    alternative <- build_demiguel_returns(raw_dir, "daily", smb_source = "ff5", hml_source = "ff5",
                                          mkt_source = "ff5")
    joined <- dplyr::inner_join(daily$returns, alternative$returns,
                                by = c("name", "date"), suffix = c("_ff3", "_ff5"))
    differs <- joined %>% dplyr::group_by(name) %>%
      dplyr::summarise(max_gap = max(abs(ret_ff3 - ret_ff5)), .groups = "drop")
    testthat::expect_equal(differs$max_gap[differs$name != "smb"],
                           rep(0, length(demiguel_factor_names) - 1L))
    testthat::expect_gt(differs$max_gap[differs$name == "smb"], 0)
  })
}


# 3. Built artifacts ===========================================================

artifact <- function(name) file.path(data_dir, name)
have_artifacts <- file.exists(artifact("target_demiguel_v3.rds")) &&
  file.exists(artifact("manifest_demiguel_v3.csv"))

if (have_raw && have_artifacts) {

  target <- readRDS(artifact("target_demiguel_v3.rds"))

  testthat::test_that("target is the monthly sum of squared daily returns", {
    rebuilt <- daily$returns %>%
      dplyr::mutate(month_id = lubridate::floor_date(date, "month")) %>%
      dplyr::group_by(name, month_id) %>%
      dplyr::summarise(rv = sum(ret ^ 2), .groups = "drop")
    joined <- dplyr::inner_join(target, rebuilt, by = c("name", "month_id"),
                                relationship = "one-to-one")
    testthat::expect_equal(nrow(joined), nrow(target))
    testthat::expect_equal(joined$rv_month, joined$rv)
    testthat::expect_true(all(target$rv_month > 0))
  })

  testthat::test_that("saved monthly returns are provider returns with RF as collateral", {
    saved <- readRDS(artifact("demiguel_returns_monthly_v3.rds"))
    joined <- dplyr::inner_join(saved, monthly$returns, by = c("name", "month_id"),
                                relationship = "one-to-one")
    testthat::expect_equal(nrow(joined), nrow(saved))
    testthat::expect_equal(joined$factor_exc, joined$ret)
    testthat::expect_equal(dplyr::n_distinct(dplyr::distinct(saved, month_id, rf)$month_id),
                           dplyr::n_distinct(saved$month_id))
  })

  for (tier in c("1927", "1986")) {

    raw_panel <- readRDS(artifact(sprintf("covariates_demiguel_%s_v3.rds", tier)))

    testthat::test_that(paste("entry rule and keys,", tier), {
      testthat::expect_setequal(unique(raw_panel$name), demiguel_factor_names)
      testthat::expect_false(anyDuplicated(raw_panel[c("name", "month_id")]) > 0)
      testthat::expect_false(is.unsorted(raw_panel$month_id))
      ## A key's first panel month is at least its third month of daily data
      first_daily <- daily$returns %>% dplyr::group_by(name) %>%
        dplyr::summarise(first = lubridate::floor_date(min(date), "month"), .groups = "drop")
      first_panel <- raw_panel %>% dplyr::group_by(name) %>%
        dplyr::summarise(entry = min(month_id), .groups = "drop") %>%
        dplyr::inner_join(first_daily, by = "name")
      testthat::expect_true(all(first_panel$entry >=
        lubridate::add_with_rollback(first_panel$first, months(2))))
      ## Within key the monthly calendar is gap-free after entry
      validate_port_data(raw_panel, c("name", "month_id"), "rv_month_lag1")
    })

    testthat::test_that(paste("shared covariates equal the JKP panel,", tier), {
      jkp <- readRDS(artifact(sprintf("covariates_panel_%s_v3.rds", tier))) %>%
        dplyr::filter(name == "spx_vw")
      shared <- setdiff(intersect(names(raw_panel), names(jkp)),
                        c("name", grep("^(rv|rq|jump|cont|negret)_", names(jkp), value = TRUE)))
      lhs <- raw_panel %>% dplyr::filter(name == "mkt", month_id %in% jkp$month_id) %>%
        dplyr::select(dplyr::all_of(shared)) %>% dplyr::arrange(month_id)
      rhs <- jkp %>% dplyr::filter(month_id %in% lhs$month_id) %>%
        dplyr::select(dplyr::all_of(shared)) %>% dplyr::arrange(month_id)
      testthat::expect_gt(nrow(lhs), 100)
      testthat::expect_equal(as.data.frame(lhs), as.data.frame(rhs))
    })

    testthat::test_that(paste("preprocessing never crosses keys,", tier), {
      for (scaling in c("zscore", "range")) {
        prepped <- readRDS(artifact(sprintf("covariates_demiguel_%s_%s_v3.rds", tier, scaling)))
        winsor <- if (scaling == "range") c(.025, .975) else c(.01, .99)
        for (key in c("mkt", "roe")) {
          alone <- prep_covariates(dplyr::filter(raw_panel, name == key), scaling = scaling,
                                   winsorize_range = winsor, key_col = "name")
          pooled <- dplyr::filter(prepped, name == key)
          testthat::expect_equal(as.data.frame(pooled), as.data.frame(alone))
        }
      }
    })
  }

  testthat::test_that("manifest hashes describe the files on disk", {
    manifest <- utils::read.csv(artifact("manifest_demiguel_v3.csv"))
    hashed <- dplyr::filter(manifest, role %in% c("input", "output"))
    testthat::expect_true(all(file.exists(artifact(hashed$artifact))))
    testthat::expect_equal(purrr::map_chr(artifact(hashed$artifact), hash_artifact),
                           hashed$sha256)
  })
}
