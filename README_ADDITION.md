# Hyperparameter Tuning Methods

This section should be added to the main README to document the new grid search functionality.

---

## Hyperparameter Tuning

The walk forward validation pipeline supports two methods for hyperparameter tuning:

### 1. Bayesian Optimization (Default)

Uses intelligent sampling to efficiently explore the hyperparameter space.

```r
result <- run_walk_forward_validation(
  target = target,
  covariates = covariates_df,
  model = "glmnet",
  hyper_grid_domain_list = list(
    alpha = c(0, 1),
    lambda.min.ratio = c(1e-4, 1e-2)
  ),
  tuning_method = "bayesian",  # Default
  n_iter = 10L,                # Number of BO iterations
  init_points = 5L,            # Initial random points
  # ... other parameters
)
```

**Pros:**
- Efficient for high-dimensional problems
- Adaptive sampling focuses on promising regions
- Requires fewer evaluations

**Cons:**
- Stochastic - results may vary
- May miss global optimum

### 2. Grid Search (New)

Systematically evaluates all combinations in a predefined grid.

```r
result <- run_walk_forward_validation(
  target = target,
  covariates = covariates_df,
  model = "glmnet",
  hyper_grid_domain_list = list(
    alpha = c(0, 1),
    lambda.min.ratio = c(1e-4, 1e-2)
  ),
  tuning_method = "grid_search",  # Use grid search
  n_grid_points = 5L,             # Points per dimension
  # ... other parameters
)
```

**Pros:**
- Deterministic - reproducible results
- Exhaustive search guarantees coverage
- Simple to understand

**Cons:**
- Computationally expensive for high dimensions
- Evaluations grow exponentially: n_grid_points^n_parameters

### Choosing a Method

| Scenario | Recommended Method | Reasoning |
|----------|-------------------|-----------|
| ≤ 4 hyperparameters | Grid Search | Manageable computation, guaranteed coverage |
| > 4 hyperparameters | Bayesian Optimization | Exponential growth makes grid search impractical |
| Need reproducibility | Grid Search | Same inputs always give same outputs |
| Limited computation | Bayesian Optimization | Fewer evaluations needed |
| Exploratory analysis | Bayesian Optimization | Adaptive sampling finds good regions faster |
| Final tuning | Grid Search | Exhaustive search around known good region |

### Parameter Specification

Both methods use the same parameter format:

```r
# Continuous parameters (default)
hyper_grid_domain_list = list(
  alpha = c(0, 1),              # Generates 0, 0.25, 0.5, 0.75, 1
  learning_rate = c(0.01, 0.1)  # Generates 0.01, 0.0325, 0.055, ...
)

# Integer parameters (use L suffix)
hyper_grid_domain_list = list(
  num_trees = c(300L, 1500L),   # Generates 300, 600, 900, 1200, 1500
  max_depth = c(3L, 16L)        # Generates 3, 6, 10, 13, 16
)
```

**Important:** Parameters are treated as continuous by default. Use the `L` suffix for integers.

### Examples

See the following files for complete examples:
- `example_grid_search.R` - Demonstrations with different models
- `GRID_SEARCH_USAGE.md` - Comprehensive usage guide
- `IMPLEMENTATION_SUMMARY.md` - Technical details

### Grid Size Calculator

Calculate total evaluations for grid search:

```r
# Example: 3 parameters with 5 points each
n_parameters <- 3
n_grid_points <- 5
total_evaluations <- n_grid_points ^ n_parameters
# Result: 5^3 = 125 evaluations
```

Quick reference:
- 2 params, 5 points: 25 evaluations
- 3 params, 5 points: 125 evaluations
- 4 params, 5 points: 625 evaluations
- 5 params, 5 points: 3,125 evaluations

---

This content can be inserted into the main README file of the project.
