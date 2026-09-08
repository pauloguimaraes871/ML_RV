# Test runner.
#
# This project is a set of scripts, not an R package, so the usual
#   library(ML_RV); test_check("ML_RV")
# form that usethis::use_testthat() writes cannot work here: there is no
# DESCRIPTION and no package to attach. The suite is run over the directory
# instead, and each test file sources the helpers it needs.
#
# Run everything:
#   Rscript tests/testthat.R
#
# Run one file:
#   testthat::test_file("tests/testthat/test-v3-artifacts.R")
#
# The dataset build in R/load_data.R runs the preprocessing and artifact files
# itself, and refuses to finish if they fail.

library("testthat")

testthat::test_dir(
  file.path(here::here(), "tests", "testthat"),
  reporter        = "summary",
  stop_on_failure = TRUE
)
