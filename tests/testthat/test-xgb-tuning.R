base::source(base::file.path(here::here(), "R", "helpers.R"))
base::source(base::file.path(here::here(), "R", "classes.R"))


make_xgb_early_stop_fixture <- function() {
  base::set.seed(7)
  x <- base::matrix(stats::rnorm(400), nrow = 100, ncol = 4)
  base::colnames(x) <- base::paste0("x", base::seq_len(base::ncol(x)))

  train_x <- x[1:70, , drop = FALSE]
  val_x   <- x[71:100, , drop = FALSE]

  list(
    full_data_train_clean = base::transform(
      base::as.data.frame(train_x),
      rv_month = 10 * train_x[, 1]
    ),
    covariates_val = base::data.frame(
      month_id = base::seq(as.Date("2000-01-01"), by = "month", length.out = 30),
      base::as.data.frame(val_x),
      check.names = FALSE
    ),
    target_val = -10 * val_x[, 1]
  )
}


make_xgb_evaluator <- function(fixture, early_stop = 3L) {
  set_eval_function("xgb")(
    full_data_train_clean = fixture$full_data_train_clean,
    covariates_val        = fixture$covariates_val,
    target_val            = fixture$target_val,
    model                 = "xgb",
    tuning_method         = "grid_search",
    eval_metric           = "rmse",
    eval_metric_trans     = "rmse",
    huber_delta           = 1,
    quantile_tau          = 0.5,
    early_stop            = early_stop,
    obj_fun_trans         = "reg:squarederror",
    keras_architecture_pars = NULL,
    verbose               = FALSE
  )
}


testthat::test_that("XGBoost early stopping returns a one-based boosting count", {
  testthat::skip_if_not_installed("xgboost")

  fixture <- make_xgb_early_stop_fixture()
  evaluator <- make_xgb_evaluator(fixture)

  result <- evaluator(
    min_child_weight = 1L,
    max_depth        = 5L,
    subsample        = 1,
    colsample_bytree = 1,
    eta              = 1,
    alpha            = 0,
    gamma            = 0,
    nrounds          = 20L
  )

  train_x <- fixture$full_data_train_clean[, base::setdiff(
    base::names(fixture$full_data_train_clean), "rv_month"
  ), drop = FALSE]
  val_x <- fixture$covariates_val[, base::setdiff(
    base::names(fixture$covariates_val), "month_id"
  ), drop = FALSE]

  direct_fit <- xgboost::xgb.train(
    params = list(
      objective        = "reg:squarederror",
      eval_metric      = "rmse",
      eta              = 1,
      min_child_weight = 1L,
      max_depth        = 5L,
      subsample        = 1,
      colsample_bytree = 1,
      alpha            = 0,
      gamma            = 0,
      huber_slope      = 1
    ),
    data = xgboost::xgb.DMatrix(
      data = base::as.matrix(train_x),
      label = fixture$full_data_train_clean$rv_month
    ),
    nrounds = 20L,
    watchlist = list(
      train = xgboost::xgb.DMatrix(
        data = base::as.matrix(train_x),
        label = fixture$full_data_train_clean$rv_month
      ),
      validation = xgboost::xgb.DMatrix(
        data = base::as.matrix(val_x),
        label = fixture$target_val
      )
    ),
    early_stopping_rounds = 3L,
    verbose = 0
  )

  expected_best_iter <- base::as.integer(base::which.min(
    direct_fit$evaluation_log$validation_rmse
  ))

  # This deliberately makes the first validation round optimal. The XGBoost
  # attribute is zero-based here, while nrounds requires a positive count.
  testthat::expect_identical(expected_best_iter, 1L)
  testthat::expect_identical(result$best_iter, expected_best_iter)
  testthat::expect_gte(result$best_iter, 1L)
})


testthat::test_that("Bayesian XGBoost objectives inject fixed engine parameters", {
  testthat::skip_if_not_installed("xgboost")

  fixture <- make_xgb_early_stop_fixture()
  parts <- partition_hyper_domain(
    hyper_grid_domain_list = list(
      eta = c(-1, 0), max_depth = c(1L, 5L),
      min_child_weight = c(1L, 10L)
    ),
    fixed_params = list(
      nrounds = 20L, subsample = 1, colsample_bytree = 1,
      alpha = 0, gamma = 0
    ),
    int_params = c("max_depth", "min_child_weight", "nrounds"),
    log10_params = "eta",
    tuning_method = "bayesian_optimization"
  )

  objective <- make_hyper_objective(
    inner = make_xgb_evaluator(fixture),
    tunable_names = base::names(parts$tunable),
    fixed = parts$fixed,
    int_params = parts$int_params,
    log10_params = parts$log10_params
  )

  result <- objective(eta = 0, max_depth = 5, min_child_weight = 1)

  testthat::expect_setequal(
    base::names(base::formals(objective)),
    c("eta", "max_depth", "min_child_weight")
  )
  testthat::expect_true(base::is.finite(result$Score))
  testthat::expect_gte(result$best_iter, 1L)
})


testthat::test_that("grid tuning fixes XGBoost constants in the final booster", {
  testthat::skip_if_not_installed("doRNG")
  testthat::skip_if_not_installed("jsonlite")
  testthat::skip_if_not_installed("xgboost")

  fixture <- make_xgb_early_stop_fixture()
  fixed <- list(
    nrounds = 20L,
    subsample = 0.8,
    colsample_bytree = 0.7,
    alpha = 0.2,
    gamma = 0.1
  )

  tuned <- hyper_tune(
    model = "xgb",
    tuning_method = "grid_search",
    full_data_train_clean = fixture$full_data_train_clean,
    covariates_val = fixture$covariates_val,
    target_val = fixture$target_val,
    eval_fun = set_eval_function("xgb"),
    obj_fun_trans = "reg:squarederror",
    eval_metric_trans = "rmse",
    early_stop = 3L,
    eval_metric = "rmse",
    huber_delta = 1,
    quantile_tau = 0.5,
    hyper_grid_domain_list = list(
      eta = c(0.5, 1), max_depth = 5L, min_child_weight = 1L
    ),
    n_iter = 2L,
    fixed_params = fixed,
    int_params = c("max_depth", "min_child_weight", "nrounds"),
    log10_params = "eta",
    init_points = 1L,
    k_iter = 1L,
    acq = "ucb",
    keras_architecture_pars = NULL,
    parallel = FALSE,
    verbose = FALSE
  )

  for (nm in base::names(fixed)) {
    testthat::expect_equal(
      base::unname(tuned$optimal_hyper[[nm]]),
      fixed[[nm]],
      info = nm
    )
    testthat::expect_true(base::all(
      tuned$eval_metric_val_current_date[[nm]] == fixed[[nm]]
    ), info = nm)
  }
  testthat::expect_setequal(
    base::names(tuned$optimal_hyper),
    c("eta", "max_depth", "min_child_weight", base::names(fixed), "best_iter")
  )
  testthat::expect_gte(base::unname(tuned$optimal_hyper[["best_iter"]]), 1)

  covariates_refit <- base::rbind(
    base::data.frame(
      month_id = base::seq(as.Date("1994-01-01"), by = "month", length.out = 70),
      fixture$full_data_train_clean[, base::setdiff(
        base::names(fixture$full_data_train_clean), "rv_month"
      ), drop = FALSE],
      check.names = FALSE
    ),
    fixture$covariates_val
  )
  target_refit <- c(
    fixture$full_data_train_clean$rv_month,
    fixture$target_val
  )

  fitted <- fit_rv_model(
    model = "xgb",
    covariates_refit = covariates_refit,
    target_refit = target_refit,
    full_data_refit_clean = NULL,
    obj_fun_trans = "reg:squarederror",
    huber_delta = 1,
    quantile_tau = 0.5,
    early_stop = 3L,
    keras_architecture_pars = NULL,
    optimal_hyper = tuned$optimal_hyper,
    eval_metric_trans = "rmse",
    n_ensembles = 1L,
    upper_quant_wins = 0.95,
    lower_quant_wins = 0.05,
    verbose = FALSE
  )

  testthat::expect_s4_class(fitted, "rv_model")
  testthat::expect_setequal(
    base::names(fitted@best_hyperparameters),
    base::names(tuned$optimal_hyper)
  )
  for (nm in base::names(fixed)) {
    testthat::expect_equal(
      base::unname(fitted@best_hyperparameters[[nm]]),
      fixed[[nm]],
      info = nm
    )
  }

  config <- jsonlite::fromJSON(xgboost::xgb.config(fitted@fit_obj))
  config_values <- base::unlist(config)
  values_for <- function(parameter) {
    base::as.numeric(config_values[
      base::endsWith(base::names(config_values), base::paste0(".", parameter))
    ])
  }

  expected_engine_params <- c(
    subsample = fixed$subsample,
    colsample_bytree = fixed$colsample_bytree,
    alpha = fixed$alpha,
    gamma = fixed$gamma
  )
  for (nm in base::names(expected_engine_params)) {
    observed <- values_for(nm)
    testthat::expect_gt(base::length(observed), 0L)
    testthat::expect_equal(
      base::unique(observed), expected_engine_params[[nm]],
      tolerance = 1e-6, info = nm
    )
  }

  testthat::expect_equal(
    base::unique(values_for("num_trees")),
    base::unname(tuned$optimal_hyper[["best_iter"]])
  )
})
