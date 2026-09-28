# Reapply the reviewed reporting body to the long sample ------------------------
# Stored artifacts use 1927, the first predictor year, although daily returns
# begin in 1926. The short-sample source tables and data remain untouched.
source(here::here("R", "report_results.R"))
source(here::here("R", "compose_sample_reports.R"))
report_results("1927")

# Composition adds BH from the saved short-sample bootstrap without resampling
# its returns or changing the original estimates and Holm values.
compose_sample_reports()
