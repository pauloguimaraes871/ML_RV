# Example: Using Grid Search for Hyperparameter Tuning
# This script demonstrates how to use the new grid search functionality

# Load required packages and helpers ------------------------------------------
source(file.path(here::here(), "R", "helpers.R"))
source(file.path(here::here(), "R", "classes.R"))

# Load data -------------------------------------------------------------------
covariates_1927_df <- readRDS(file.path(here::here(), "data", "covariates_1927.rds"))
target <- readRDS(file.path(here::here(), "data", "crsp_monthly.rds"))

# Prepare data for gold period
target_gold <- target %>% 
  dplyr::filter(month_id >= "1962-01-31")

covariates_1927_df_gold <- covariates_1927_df %>% 
  dplyr::filter(month_id >= "1962-01-31")

# Example 1: GLMNET with Grid Search -----------------------------------------
cat(strrep("=", 80), "\n")
cat("Example 1: GLMNET with Grid Search\n")
cat(strrep("=", 80), "\n\n")

# Setup parallel processing
future::plan("multisession")
doFuture::registerDoFuture()

# Run walk forward validation with grid search
glmnet_grid_search_res <- run_walk_forward_validation(
  target = target_gold, 
  covariates = covariates_1927_df_gold,
  model = "glmnet",
  
  # Hyperparameter grid - same format as Bayesian optimization
  hyper_grid_domain_list = list(
    alpha = c(0, 1),
    lambda.min.ratio = c(1e-4, 1e-2)
  ),
  
  # Grid search specific parameters
  tuning_method = "grid_search",     # Specify grid search instead of Bayesian
  n_grid_points = 5L,                # 5 points per dimension = 5^2 = 25 evaluations
  
  # Standard parameters
  obj_fun = "squared_error", 
  eval_metric = "rmse",
  huber_delta = 1, 
  quantile_tau = 0.5,
  train_n = 120L, 
  val_n = 60L,
  rebal_months = c(6),
  early_stop = NULL,
  gsm_algo = "ols",
  upper_quant_wins = 0.95,
  lower_quant_wins = 0.05,
  parallel = TRUE,
  verbose = TRUE,
  .test_seed = 123
)

# Save results
saveRDS(glmnet_grid_search_res, "models/glmnet_grid_search_res.rds")

cat("\n")
cat("GLMNET Grid Search completed!\n")
cat("Results saved to: models/glmnet_grid_search_res.rds\n\n")

# Example 2: Random Forest with Grid Search ----------------------------------
cat(strrep("=", 80), "\n")
cat("Example 2: Random Forest with Grid Search\n")
cat(strrep("=", 80), "\n\n")

# Setup parallel processing
future::plan("multisession")
doFuture::registerDoFuture()

rf_grid_search_res <- run_walk_forward_validation(
  target = target_gold, 
  covariates = covariates_1927_df_gold,
  model = "rf",
  
  # Hyperparameter grid
  hyper_grid_domain_list = list(
    mtry = c(0.1, 0.9),
    num.trees = c(300L, 1500L),
    max.depth = c(3L, 16L),
    min.bucket = c(1L, 6L)
  ),
  
  # Grid search specific - smaller grid due to 4 dimensions
  tuning_method = "grid_search",
  n_grid_points = 3L,  # 3^4 = 81 evaluations
  
  # Standard parameters
  obj_fun = "squared_error", 
  eval_metric = "rmse",
  huber_delta = 1, 
  quantile_tau = 0.5,
  train_n = 120L, 
  val_n = 60L,
  rebal_months = c(6),
  early_stop = NULL,
  gsm_algo = "ols",
  upper_quant_wins = 0.95,
  lower_quant_wins = 0.05,
  parallel = TRUE,
  verbose = TRUE,
  .test_seed = 123
)

# Save results
saveRDS(rf_grid_search_res, "models/rf_grid_search_res.rds")

cat("\n")
cat("Random Forest Grid Search completed!\n")
cat("Results saved to: models/rf_grid_search_res.rds\n\n")

# Example 3: Comparison with Bayesian Optimization ---------------------------
cat(strrep("=", 80), "\n")
cat("Example 3: Comparing Grid Search vs Bayesian Optimization\n")
cat(strrep("=", 80), "\n\n")

# Run with Bayesian Optimization for comparison
cat("Running with Bayesian Optimization...\n")
glmnet_bayesian_res <- run_walk_forward_validation(
  target = target_gold, 
  covariates = covariates_1927_df_gold,
  model = "glmnet",
  
  hyper_grid_domain_list = list(
    alpha = c(0, 1),
    lambda.min.ratio = c(1e-4, 1e-2)
  ),
  
  # Bayesian optimization parameters
  tuning_method = "bayesian",  # Default
  n_iter = 10L,                # 10 iterations
  init_points = 5L,            # 5 initial random points
  k_iter = 2L,
  acq = "ucb",
  
  obj_fun = "squared_error", 
  eval_metric = "rmse",
  train_n = 120L, 
  val_n = 60L,
  rebal_months = c(6),
  parallel = TRUE,
  verbose = TRUE,
  .test_seed = 123
)

# Compare results
cat("\n")
cat(strrep("=", 80), "\n")
cat("Comparison Results\n")
cat(strrep("=", 80), "\n\n")

cat("Grid Search Performance:\n")
print(summary(glmnet_grid_search_res$test_eval_metrics))

cat("\nBayesian Optimization Performance:\n")
print(summary(glmnet_bayesian_res$test_eval_metrics))

cat("\n")
cat("Comparison completed!\n")
cat("Grid Search tested 25 fixed combinations\n")
cat("Bayesian Optimization tested 15 adaptive combinations (5 init + 10 iter)\n")

# Clean up
future::plan("sequential")
