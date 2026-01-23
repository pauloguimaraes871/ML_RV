# Grid Search Implementation for ML_RV

This document explains how to use the new grid search algorithm for hyperparameter tuning in the walk forward pipeline.

## Overview

The grid search algorithm provides an alternative to Bayesian optimization for hyperparameter tuning. It systematically searches through a grid of hyperparameter combinations to find the optimal set.

## Key Features

- **Exhaustive Search**: Tests all combinations of hyperparameters within specified ranges
- **Parallel Execution**: Supports parallel processing for faster computation
- **Consistent Interface**: Uses the same interface as Bayesian optimization
- **Flexible Grid Size**: Control the number of points per dimension with `n_grid_points`

## Usage

### Basic Example with GLMNET

```r
# Load packages and helpers
source(file.path(here::here(), "R", "helpers.R"))
source(file.path(here::here(), "R", "classes.R"))

# Load data
covariates_1927_df <- readRDS(file.path(here::here(), "data", "covariates_1927.rds"))
target <- readRDS(file.path(here::here(), "data", "crsp_monthly.rds"))

# Run walk forward validation with grid search
glmnet_grid_search_res <- run_walk_forward_validation(
  target = target, 
  covariates = covariates_1927_df,
  model = "glmnet",
  
  # Hyperparameter grid - same format as Bayesian optimization
  hyper_grid_domain_list = list(
    alpha = c(0, 1),                # Min and max values
    lambda.min.ratio = c(1e-4, 1e-2)
  ),
  
  # Grid search specific parameters
  tuning_method = "grid_search",     # NEW: Specify grid search
  n_grid_points = 5L,                # NEW: Number of points per dimension
  
  # Standard parameters
  obj_fun = "squared_error", 
  eval_metric = "rmse",
  train_n = 120L, 
  val_n = 60L,
  rebal_months = c(6),
  parallel = TRUE,
  verbose = TRUE,
  .test_seed = 123
)
```

### Example with Random Forest

```r
rf_grid_search_res <- run_walk_forward_validation(
  target = target, 
  covariates = covariates_1927_df,
  model = "rf",
  
  # Hyperparameter grid
  hyper_grid_domain_list = list(
    mtry = c(0.1, 0.9),
    num.trees = c(300L, 1500L),
    max.depth = c(3L, 16L),
    min.bucket = c(1L, 6L)
  ),
  
  # Grid search specific
  tuning_method = "grid_search",
  n_grid_points = 3L,  # 3^4 = 81 combinations
  
  # Standard parameters
  obj_fun = "squared_error", 
  eval_metric = "rmse",
  train_n = 120L, 
  val_n = 60L,
  rebal_months = c(6),
  parallel = TRUE,
  verbose = TRUE,
  .test_seed = 123
)
```

### Example with XGBoost

```r
xgb_grid_search_res <- run_walk_forward_validation(
  target = target, 
  covariates = covariates_1927_df,
  model = "xgb",
  
  # Hyperparameter grid
  hyper_grid_domain_list = list(
    min_child_weight = c(2L, 10L),
    max_depth        = c(2L, 5L),
    subsample        = c(0.60, 0.95),
    colsample_bytree = c(0.60, 0.95),
    eta              = c(0.05, 0.20),
    alpha            = c(0.00, 2.00)
  ),
  
  # Grid search specific
  tuning_method = "grid_search",
  n_grid_points = 3L,  # 3^6 = 729 combinations
  
  # Standard parameters
  obj_fun = "squared_error", 
  eval_metric = "rmse",
  train_n = 120L, 
  val_n = 60L,
  rebal_months = c(6),
  parallel = TRUE,
  verbose = TRUE,
  .test_seed = 123
)
```

## Comparison: Grid Search vs Bayesian Optimization

### Grid Search
- **Pros**: 
  - Exhaustive search guarantees finding the best combination in the grid
  - Predictable computation time (depends on grid size)
  - Simple to understand and implement
- **Cons**: 
  - Computationally expensive for high-dimensional problems
  - Number of evaluations grows exponentially with dimensions
  - May waste evaluations on suboptimal regions

### Bayesian Optimization
- **Pros**: 
  - More efficient for high-dimensional problems
  - Intelligently samples promising regions
  - Fewer evaluations needed
- **Cons**: 
  - Stochastic - results may vary between runs
  - May miss global optimum
  - More complex algorithm

## Parameters

### New Parameters in `run_walk_forward_validation`

- `tuning_method`: Character string specifying tuning method
  - `"bayesian"` (default): Use Bayesian optimization
  - `"grid_search"`: Use grid search
  
- `n_grid_points`: Integer specifying number of points per dimension in grid search (default: 5)
  - Total evaluations = `n_grid_points ^ number_of_hyperparameters`
  - Example: 5 points with 3 hyperparameters = 5^3 = 125 evaluations

### Existing Parameters (Unchanged)

All other parameters work exactly the same way:
- `hyper_grid_domain_list`: List with min/max bounds for each hyperparameter
- `obj_fun`, `eval_metric`: Objective function and evaluation metric
- `train_n`, `val_n`: Training and validation window sizes
- `rebal_months`: Rebalancing schedule
- `parallel`: Enable parallel processing
- And all other standard parameters...

## Tips for Using Grid Search

1. **Start Small**: Begin with `n_grid_points = 3` to get quick results
2. **Consider Grid Size**: Total evaluations = n_grid_points ^ number_of_hyperparameters
3. **Use Parallel Processing**: Enable `parallel = TRUE` for faster computation
4. **Compare Methods**: Try both grid search and Bayesian optimization to see which works better
5. **Adjust Grid Size**: Increase `n_grid_points` for more thorough search if computation time allows

## Technical Details

### Grid Generation

The grid is generated using the following logic:
- For continuous parameters (default): `seq(min, max, length.out = n_grid_points)`
- For integer parameters (marked with `L` suffix, e.g., `c(1L, 10L)`): Integer sequence respecting bounds and avoiding redundant points
- Full grid created using `expand.grid()`

**Important**: To specify integer parameters, use the `L` suffix in R (e.g., `c(300L, 1500L)`). Parameters without the `L` suffix are treated as continuous, even if the bounds are whole numbers (e.g., `c(0, 1)` will generate points like 0, 0.25, 0.5, 0.75, 1 with `n_grid_points = 5`).

### Evaluation

- Each hyperparameter combination is evaluated on the validation set
- Parallel evaluation uses `foreach` with `doFuture` backend
- Best combination selected based on maximum Score (equivalent to minimum loss)

### Output Structure

The output structure is identical to Bayesian optimization:
- `eval_metric_val`: All evaluated combinations and their metrics
- `optimal_hyper`: Best hyperparameters found
- `val_eval_metrics_hyper_choice_current_date`: Performance metrics for best combination

## Example Output

```
=============================
Model: glmnet
Objective function: squared_error
Evaluation metric: rmse
=============================

Current date: 1986-01-31
Starting model rebal at: 1986-01-31

Hyper tuning (Grid Search) at: 1986-01-31...
Grid Search: Testing 25 hyperparameter combinations
Progress: 20%
Progress: 40%
Progress: 60%
Progress: 80%
Progress: 100%
Chosen hyperparameters were: alpha: 0.5 lambda.min.ratio: 0.0025 best_lam: 0.0123
Validation eval_metrics for hyperparameters chosen were: Score: -0.0045 rmse: 0.0045 ...
```

## Integration with Existing Code

The grid search implementation:
- Is fully integrated with the existing walk forward validation framework
- Uses the same evaluation functions as Bayesian optimization
- Supports all models: HAR, GLMNET, RF, XGB, NN1-5, LSTM
- Works with all objective functions and evaluation metrics
- Maintains backward compatibility (Bayesian optimization is still the default)

## Notes

- Grid search is particularly useful when:
  - You have a small number of hyperparameters (≤4)
  - You want guaranteed coverage of the search space
  - You want reproducible results
  - You have sufficient computational resources

- Consider Bayesian optimization when:
  - You have many hyperparameters (>4)
  - Computational budget is limited
  - You want adaptive sampling of the search space
