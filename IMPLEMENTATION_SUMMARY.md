# Grid Search Implementation Summary

## Overview

This implementation adds a grid search algorithm for hyperparameter tuning in the walk forward pipeline as an alternative to Bayesian optimization. The grid search systematically evaluates all combinations of hyperparameters within a specified grid.

## Implementation Details

### Files Modified

1. **R/helpers.R**
   - Added `grid_search_tune()` function (lines 2497-2692)
   - Modified `run_walk_forward_validation()` to support tuning method selection (lines 3016-3305)

### Files Added

1. **GRID_SEARCH_USAGE.md** - Comprehensive user guide with examples
2. **example_grid_search.R** - Practical examples demonstrating usage
3. **validate_grid_search.R** - Validation tests for grid generation logic
4. **IMPLEMENTATION_SUMMARY.md** - This file

## Key Features

### 1. Grid Search Function (`grid_search_tune`)

The function follows the same signature and output structure as `hyper_tune()` for seamless integration:

```r
grid_search_tune <- function(
    model, full_data_train_clean, covariates_val, target_val,
    eval_fun, obj_fun_trans,
    eval_metric_trans, early_stop,
    eval_metric, huber_delta, quantile_tau,
    hyper_grid_domain_list, n_grid_points = 5L,
    keras_architecture_pars,
    parallel,
    verbose)
```

**Key Design Decisions:**

1. **Continuous by Default**: Parameters are treated as continuous unless explicitly marked with `L` suffix
   - `c(0, 1)` → generates [0, 0.25, 0.5, 0.75, 1] with n_grid_points=5
   - `c(0L, 1L)` → generates [0, 1] (only min and max)

2. **Parallel Processing**: Uses `foreach` with `doFuture::withDoRNG` for reproducible parallel execution
   - Disabled for neural networks to avoid conflicts
   - Matches the pattern used in Bayesian optimization

3. **Integer Handling**: For integer parameters with small ranges, respects actual range
   - `c(2L, 4L)` with n_grid_points=10 → generates [2, 3, 4] (not 10 points)

### 2. Integration with Walk Forward Validation

Modified `run_walk_forward_validation()` to accept:

```r
tuning_method = "bayesian"  # or "grid_search"
n_grid_points = 5L          # controls grid density
```

The function now:
1. Checks `tuning_method` parameter
2. Calls either `hyper_tune()` or `grid_search_tune()`
3. Processes results identically regardless of method
4. Maintains full backward compatibility (default is "bayesian")

## Usage Examples

### Basic Usage

```r
result <- run_walk_forward_validation(
  target = target,
  covariates = covariates_df,
  model = "glmnet",
  hyper_grid_domain_list = list(
    alpha = c(0, 1),
    lambda.min.ratio = c(1e-4, 1e-2)
  ),
  tuning_method = "grid_search",  # NEW
  n_grid_points = 5L,             # NEW
  obj_fun = "squared_error",
  eval_metric = "rmse",
  # ... other parameters
)
```

### Comparison with Bayesian Optimization

Grid Search:
- Deterministic results
- Exhaustive coverage
- 5^2 = 25 evaluations for 2 parameters

Bayesian Optimization:
- Adaptive sampling
- 5 initial + 10 iterations = 15 evaluations
- Focuses on promising regions

## Grid Size Considerations

Total evaluations = `n_grid_points ^ number_of_parameters`

Examples:
- 2 parameters, n_grid_points=5: 5^2 = 25 evaluations
- 3 parameters, n_grid_points=5: 5^3 = 125 evaluations
- 4 parameters, n_grid_points=5: 5^4 = 625 evaluations
- 6 parameters, n_grid_points=5: 5^6 = 15,625 evaluations

**Recommendation**: Use grid search for ≤4 parameters, Bayesian optimization for >4

## Validation

The implementation has been validated for:

1. **Grid Generation**
   - Continuous parameters generate evenly spaced points
   - Integer parameters respect range constraints
   - Mixed parameter types work correctly

2. **Result Structure**
   - Best hyperparameters correctly identified
   - Output format matches `hyper_tune()`
   - All evaluation metrics properly computed

3. **Edge Cases**
   - Single parameter grids
   - High-dimensional grids
   - Small integer ranges
   - Minimal grids (n_grid_points=2)

## Performance Characteristics

### Computational Complexity

- **Grid Search**: O(n^d) where n=n_grid_points, d=number of parameters
- **Bayesian Optimization**: O(k) where k=init_points + n_iter

### Memory Usage

Grid search stores all evaluations in memory:
- Modest for typical use cases (≤100 combinations)
- Can be large for high-dimensional problems

### Parallel Scaling

- Linear speedup with number of cores for models that support parallelization
- Neural networks run sequentially to avoid conflicts

## Compatibility

The implementation is compatible with:

- **All Models**: HAR, GLMNET, RF, XGB, NN1-5, LSTM
- **All Objective Functions**: squared_error, absolute_error, huber, pinball
- **All Evaluation Metrics**: rmse, mae, mphe, mpe, mape, hr, mb
- **All Existing Features**: Early stopping, custom losses, validation windows

## Testing Recommendations

To test the implementation:

1. **Small Grid Test**: Start with n_grid_points=3 and 2 parameters (9 evaluations)
2. **Compare Methods**: Run same configuration with both methods
3. **Verify Reproducibility**: Grid search should give identical results with same seed
4. **Check Performance**: Monitor computation time vs accuracy trade-off

## Future Enhancements

Potential improvements for future versions:

1. **Random Grid Search**: Sample random points from grid for efficiency
2. **Adaptive Grid Refinement**: Start coarse, refine around best regions
3. **Grid Visualization**: Plot grid points and evaluation results
4. **Resume Capability**: Save/load partial results for long-running searches

## Maintenance Notes

When maintaining this code:

1. **Consistency**: Keep grid_search_tune signature aligned with hyper_tune
2. **Testing**: Run validation script after any changes to grid generation logic
3. **Documentation**: Update GRID_SEARCH_USAGE.md when adding features
4. **Examples**: Keep example_grid_search.R working with latest data

## References

- Original Bayesian optimization: `hyper_tune()` function
- Walk forward validation: `run_walk_forward_validation()` function
- Evaluation functions: `set_eval_function()` and `calc_eval_metrics()`

## Contact

For questions or issues with the grid search implementation, refer to:
- GRID_SEARCH_USAGE.md for usage documentation
- validate_grid_search.R for validation tests
- example_grid_search.R for working examples
