## Tests for the stock-level cost engine (R/demiguel_cost_helpers.R)
##
##   1. synthetic: trade arithmetic, NA convention, entry, netting levels
##   2. the compacted package data, when R/load_demiguel_costs.R has been run:
##      reproduces an independent replica of the authors' Figure 2B code

library("testthat")
`%>%` <- dplyr::`%>%`
source(file.path(here::here(), "R", "portfolio_helpers.R"))
source(file.path(here::here(), "R", "demiguel_cost_helpers.R"))

## Two months, three stocks, two factors. Stock 3 is outside CRSP in month 1
## (NA weight), so its trade in month 2 is not charged; stock 2 leaves
## factor "a" while still listed (weight 0), which is charged.
toy_stock <- function() {
  list(
    months = as.Date(c("2000-01-01", "2000-02-01")), factors = c("a", "b"),
    panels = list(
      list(id = 1:3, kappa = c(.01, .02, 0), ret = c(.10, -.50, NA),
           x = cbind(a = c(.6, .4, NA), b = c(1, -1, NA))),
      list(id = 1:3, kappa = c(.01, .02, .03), ret = c(0, 0, 0),
           x = cbind(a = c(1, 0, 0), b = c(.5, -1, .5)))
    )
  )
}

exposures_for <- function(w_a, w_b) {
  tidyr::expand_grid(month_id = as.Date(c("2000-01-01", "2000-02-01")),
                     sleeve = c("A", "B")) %>%
    dplyr::mutate(factor = tolower(sleeve),
                  w = dplyr::if_else(sleeve == "A", w_a[as.integer(factor(month_id))],
                                     w_b[as.integer(factor(month_id))]))
}


# 1. Synthetic =================================================================

testthat::test_that("trades follow x_t w_t - x_{t-1} w_{t-1} (1 + r_{t-1}) and skip NA", {
  s <- toy_stock()
  out <- stock_cost(s, exposures_for(c(1, 2), c(1, 1)), "sleeve", charge_entry = FALSE, by_sleeve = TRUE)
  ## Month 2, sleeve A: stock 1: |1*2 - .6*1*1.1| * .01; stock 2: |0 - .4*.5| * .02;
  ## stock 3: previous NA, not charged
  a <- abs(2 - .6 * 1.1) * .01 + abs(0 - .4 * .5) * .02
  b <- abs(.5 - 1 * 1.1) * .01 + abs(-1 - (-1) * .5) * .02
  testthat::expect_equal(out$cost[out$month_id == as.Date("2000-02-01") & out$sleeve == "A"], a)
  testthat::expect_equal(out$cost[out$month_id == as.Date("2000-02-01") & out$sleeve == "B"], b)
  testthat::expect_true(all(is.na(out$cost[out$month_id == as.Date("2000-01-01")])))
})

testthat::test_that("entry from cash charges the first month's full position", {
  s <- toy_stock()
  out <- stock_cost(s, exposures_for(c(2, 2), c(1, 1)), "sleeve", charge_entry = TRUE)
  first <- 2 * (.6 * .01 + .4 * .02) + (1 * .01 + 1 * .02)
  testthat::expect_equal(out$cost[1], first)
})

testthat::test_that("netting levels: portfolio <= factor <= sleeve, equal without overlap", {
  s <- toy_stock()
  e <- exposures_for(c(1, 2), c(1, 1))
  sl <- stock_cost(s, e, "sleeve")$cost
  fa <- stock_cost(s, e, "factor")$cost
  po <- stock_cost(s, e, "portfolio")$cost
  testthat::expect_true(all(po <= fa + 1e-15) && all(fa <= sl + 1e-15))
  ## One sleeve per factor: sleeve and factor netting coincide
  testthat::expect_equal(fa, sl)
  ## Portfolio netting: month 2, stock 1 trades +1.34 in A and -0.6 in B
  testthat::expect_lt(po[2], sl[2])
})

testthat::test_that("costs are homogeneous of degree one in the exposures", {
  s <- toy_stock()
  base <- stock_cost(s, exposures_for(c(1, 2), c(1, -1)), "portfolio")$cost
  doubled <- stock_cost(s, exposures_for(c(2, 4), c(2, -2)), "portfolio")$cost
  testthat::expect_equal(doubled, 2 * base)
})

testthat::test_that("invalid exposures abort", {
  s <- toy_stock()
  e <- exposures_for(c(1, 2), c(1, 1))
  testthat::expect_error(stock_cost(s, dplyr::mutate(e, factor = "z"), "sleeve"), "Unknown factor")
  testthat::expect_error(stock_cost(s, e[-1, ], "sleeve"), "every sleeve")
  testthat::expect_error(stock_cost(s, dplyr::mutate(e, month_id = month_id + 31), "sleeve"),
                         "first-of-month|outside")
  testthat::expect_error(stock_cost(s, e, "factor", by_sleeve = TRUE), "by_sleeve")
})


# 2. Package data ==============================================================

stock_path <- file.path(here::here(), "data", "demiguel_stock_costs_v3.rds")
if (file.exists(stock_path)) {
  stock <- readRDS(stock_path)

  testthat::test_that("the compacted data keep the package's shape and timing", {
    testthat::expect_equal(length(stock$months), 647L)
    testthat::expect_equal(range(stock$months), as.Date(c("1967-02-01", "2020-12-01")))
    testthat::expect_equal(stock$factors, c("mkt", "smb", "hml", "rmw", "cma", "umd", "roe", "ia", "bab"))
    p <- stock$panels[[which(stock$months == as.Date("2000-06-01"))]]
    long <- colSums(pmax(p$x, 0), na.rm = TRUE)
    short <- colSums(pmin(p$x, 0), na.rm = TRUE)
    testthat::expect_equal(unname(long[1:8]), rep(1, 8), tolerance = 1e-6)
    testthat::expect_equal(unname(short[2:8]), rep(-1, 7), tolerance = 1e-6)
  })

  testthat::test_that("unit factor costs reproduce the authors' Figure 2B code", {
    ## Reference values from an independent replica of
    ## ConstructReturnVarianceTCfigures.m (Python/h5py on the same .mat):
    ## mean over trades March 1977 - December 2020, in bp per month.
    reference <- c(mkt = 1.5354249888, smb = 7.1691919234, hml = 9.9520574185,
                   rmw = 9.8550934672, cma = 13.4398276082, umd = 45.8441650690,
                   roe = 30.7406943274, ia = 21.6600047594, bab = 28.1688142261)
    costs <- factor_rebalancing_costs(stock, as.Date("1977-02-01"), as.Date("2020-12-01")) %>%
      dplyr::filter(!is.na(cost)) %>%
      dplyr::group_by(name) %>%
      dplyr::summarise(n = dplyr::n(), bp = 1e4 * mean(cost), .groups = "drop")
    testthat::expect_true(all(costs$n == 526L))
    testthat::expect_equal(costs$bp[match(names(reference), costs$name)], unname(reference),
                           tolerance = 1e-8)
  })

  testthat::test_that("rebuilt factor returns track the package's provider returns", {
    ## sum_i x_{k,i,t} r_{i,t} vs Rchar_t (paper footnote 5: correlation > 0.9)
    rebuilt <- purrr::map_dfr(seq_along(stock$months), function(t) {
      p <- stock$panels[[t]]
      tibble::tibble(month_id = stock$months[t], name = stock$factors,
                     rebuilt = colSums(p$x * p$ret, na.rm = TRUE))
    }) %>% dplyr::inner_join(stock$factor_returns, by = c("month_id", "name"))
    rho <- rebuilt %>% dplyr::group_by(name) %>%
      dplyr::summarise(rho = stats::cor(rebuilt, factor_exc), .groups = "drop")
    testthat::expect_true(all(rho$rho > .9))
  })
}
