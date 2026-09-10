# V3 model fitting---------------------------------------------------------------

  ## Package and project code
  `%>%` <- dplyr::`%>%`
  source(file.path(here::here(), "R", "helpers.R"))
  source(file.path(here::here(), "R", "classes.R"))

# Run configuration-------------------------------------------------------------

  ## The only switches that need editing. Nothing further down has to be touched
  ## to change sample, universe, preprocessing or tuning method.
  run_1986     <- TRUE   # Initial sample
  run_1927     <- FALSE  # Enable later as a longer-sample robustness check
  run_panel    <- TRUE   # Pooled factor panel, including the spx_vw key
  run_spx_only <- TRUE   # Standalone S&P 500 comparison
  models_to_run <- c("har", "harx", "glmnet", "rf", "xgb", "nn1", "nn2", "nn3")
  preprocessing_to_run <- "range"
  tuning_methods_to_run <- "grid_search"

  ## TRUE reports the run matrix with cache hits and misses and fits NOTHING.
  ## Set to FALSE to actually fit.
  dry_run <- FALSE

  ## Where fitted objects are looked for and written
  model_dirs_v3 <- file.path(here::here(), "models_v3")
  cache_dirs    <- model_dirs_v3

  ## Shared experimental design
  design <- list(
    obj_fun          = "squared_error",
    eval_metric      = "rmse",
    huber_delta      = 1,
    quantile_tau     = 0.5,
    rebal_months     = c(6),
    gsm_algo         = "ols",
    upper_quant_wins = 0.95,
    lower_quant_wins = 0.05,
    seed             = 123
  )

  ## Target transform.
  ## Applied here rather than baked into the dataset, so that the fitted object
  ## records which transform produced it and the raw target stays reusable.
  target_transform_fun <- function(x) boxcox_apply(x, 0, 1e-08)
  target_transform_id  <- "boxcox_apply(lambda=0, offset=1e-08)"
  target_inverse_id    <- "boxcox_inv(lambda=0, offset=1e-08)"


# Search spaces-----------------------------------------------------------------

  ## Deliberately small. This is a monthly RV problem with a few hundred
  ## observations per key, not a cross-sectional stock panel: the budgets in Gu,
  ## Kelly and Xiu (2020) and Hanauer and Kalsbach (2023) are references, not
  ## templates.
  ##
  ## Constants live in `fixed` so they never appear as a degenerate Bayesian
  ## bound or as a grid dimension of length one. The union of `fixed` and the
  ## searched domain must cover every argument the engine needs, which validator()
  ## enforces.
  ##
  ## Bayesian bounds for eta, lr and regularizer_l1 are stated on a LOG10 scale
  ## (declared in `log10`) because those parameters act multiplicatively; a
  ## uniform prior over the level would spend most of its budget on large values.
  ## Grid values are always written in engine units.
  hyper_spec <- list(

    glmnet = list(
      ### lambda is chosen along the fitted path by get_best_lambda(), so the
      ### only thing left worth searching is the elastic-net mixing parameter
      bayes  = list(alpha = c(0, 1)),
      grid   = list(alpha = c(0, 0.25, 0.5, 0.75, 1)),
      fixed  = list(lambda.min.ratio = 1e-4),
      int    = character(),
      log10  = character(),
      budget = list(n_iter = 6L, init_points = 6L, k_iter = 2L, acq = "ucb")
    ),

    rf = list(
      ### Predictor fraction and depth. Tree count and leaf size are fixed:
      ### neither repays a search once the forest is large enough.
      ###
      ### The first pass over the 1986 panel saturated both boundaries: the
      ### search took max.depth = 8 at 19 of 19 refits and mtry = 0.50 at 16 of
      ### 19, which is a search asking for capacity the grid would not give it.
      ### Depth matters most here. A pooled forest has to isolate one key out of
      ### 154 to place it correctly, and at depth 8 the market shared leaves with
      ### calmer series and was pulled toward them.
      bayes  = list(mtry = c(0.25, 0.85), max.depth = c(4L, 16L)),
      grid   = list(mtry = c(0.35, 0.50, 0.75), max.depth = c(5L, 8L, 12L)),
      fixed  = list(num.trees = 1000L, min.bucket = 5L),
      int    = c("max.depth", "num.trees", "min.bucket"),
      log10  = character(),
      budget = list(n_iter = 10L, init_points = 8L, k_iter = 2L, acq = "ucb")
    ),

    xgb = list(
      ### Learning rate, shallow depth and one observation-complexity control.
      ### Regularization is held fixed and early stopping decides the number of
      ### rounds, so nrounds is a ceiling rather than a tuned quantity.
      bayes  = list(eta = c(-2, -1), max_depth = c(1L, 3L),
                    min_child_weight = c(5L, 20L)),
      grid   = list(eta = c(0.01, 0.05, 0.10), max_depth = c(1L, 2L, 3L),
                    min_child_weight = c(5L, 20L)),
      fixed  = list(nrounds = 1000L, subsample = 0.8, colsample_bytree = 0.8,
                    alpha = 0, gamma = 0),
      int    = c("max_depth", "min_child_weight", "nrounds"),
      log10  = "eta",
      budget = list(n_iter = 12L, init_points = 10L, k_iter = 2L, acq = "ucb")
    ),

    nn = list(
      ### Learning rate, dropout and one regularization term. Batch size, epochs
      ### and the l2 penalty are fixed; the architectures are the established
      ### 32, 32/16 and 32/16/8 stacks.
      bayes  = list(lr = c(-4, -2), droprate = c(0, 0.25),
                    regularizer_l1 = c(-5, -3)),
      grid   = list(lr = c(1e-4, 1e-3, 1e-2), droprate = c(0, 0.1, 0.25),
                    regularizer_l1 = c(1e-5, 1e-4, 1e-3)),
      fixed  = list(regularizer_l2 = 0, size_of_batch = 64L,
                    number_of_epochs = 100L),
      int    = c("size_of_batch", "number_of_epochs"),
      log10  = c("lr", "regularizer_l1"),
      budget = list(n_iter = 10L, init_points = 10L, k_iter = 2L, acq = "ei")
    )
  )

  ## Per-model engine settings.
  ## `har` and `harx` are the same estimator on different covariate sets, which
  ## is why they are two model_ids over one `model`.
  nn_arch <- function(units, n_layers) {
    list(
      units = units,
      n_layers = n_layers,
      activation = rep("relu", n_layers),
      nn_optimizer = "Adam",
      batch_norm_option = rep(TRUE, n_layers)
    )
  }

  model_spec <- list(
    har    = list(model = "har",    hyper = NULL,     feature_set = "har",
                  train_n = 210L, val_n = 0L,  early_stop = NULL,
                  parallel = TRUE,  n_ensembles = 1L,  arch = NULL),
    harx   = list(model = "har",    hyper = NULL,     feature_set = "harx",
                  train_n = 210L, val_n = 0L,  early_stop = NULL,
                  parallel = TRUE,  n_ensembles = 1L,  arch = NULL),
    glmnet = list(model = "glmnet", hyper = "glmnet", feature_set = "harx",
                  train_n = 120L, val_n = 90L, early_stop = NULL,
                  parallel = TRUE,  n_ensembles = 1L,  arch = NULL),
    rf     = list(model = "rf",     hyper = "rf",     feature_set = "harx",
                  train_n = 120L, val_n = 90L, early_stop = NULL,
                  parallel = TRUE,  n_ensembles = 1L,  arch = NULL),
    xgb    = list(model = "xgb",    hyper = "xgb",    feature_set = "harx",
                  train_n = 120L, val_n = 90L, early_stop = 30L,
                  parallel = FALSE, n_ensembles = 1L,  arch = NULL),
    nn1    = list(model = "nn",     hyper = "nn",     feature_set = "harx",
                  train_n = 120L, val_n = 90L, early_stop = 10L,
                  parallel = FALSE, n_ensembles = 10L,
                  arch = nn_arch(32, 1)),
    nn2    = list(model = "nn",     hyper = "nn",     feature_set = "harx",
                  train_n = 120L, val_n = 90L, early_stop = 10L,
                  parallel = FALSE, n_ensembles = 10L,
                  arch = nn_arch(c(32, 16), 2)),
    nn3    = list(model = "nn",     hyper = "nn",     feature_set = "harx",
                  train_n = 120L, val_n = 90L, early_stop = 10L,
                  parallel = FALSE, n_ensembles = 10L,
                  arch = nn_arch(c(32, 16, 8), 3))
  )


# Dataset resolution------------------------------------------------------------

  ## Artifacts are produced by load_data.R. The run records the path AND the
  ## hash, so a result can always be traced back to the exact bytes it read.
  dataset_path <- function(universe, sample_id, prep){
    file.path(here::here(), "data",
              sprintf("covariates_%s_%s_%s_v3.rds", universe, sample_id, prep))
  }

  target_path <- function(universe){
    file.path(here::here(), "data",
              if (universe == "panel") "target_panel_v3.rds" else "target_spx_v3.rds")
  }

  ## HAR uses only the three lagged RV terms; HARX uses every covariate.
  ##
  ## In the pooled panel HAR also carries rv_level_lag1. Preprocessing runs
  ## within key, so each series is normalised against its own extremes and the
  ## design matrix says nothing about which series a row belongs to, while the
  ## target still spans three log units across series. Without the level term a
  ## pooled HAR cannot tell the market from a long-short factor and predicts
  ## close to one number for all of them, which makes it a broken benchmark
  ## rather than a weak one. The term is the panel analogue of a fixed effect,
  ## not an extra predictor.
  ##
  ## On a single series there is no cross-sectional level to carry, so the S&P
  ## HAR stays exactly the three terms of Corsi (2009).
  select_features <- function(covariates, feature_set, key_col){

    ### prep_covariates() emits rv_level_lag1 for every series, but it only
    ### means something in a pooled fit. On a single series it is redundant at
    ### best and harmful at worst: a linear model already carries the level in
    ### its intercept, which is re-estimated at every refit, and for a tree an
    ### expanding mean drifts monotonically enough to serve as a calendar
    ### index, so splits on it partition the sample by date rather than by
    ### state and do not extrapolate. It is dropped before anything else.
    if (is.null(key_col)) {
      covariates <- covariates %>%
        dplyr::select(-dplyr::any_of("rv_level_lag1"))
    }

    if (feature_set == "harx") return(covariates)

    covariates %>%
      dplyr::select(dplyr::all_of(c("month_id", key_col)),
                    dplyr::matches("^rv_(month|tw|td)_lag1$"),
                    dplyr::any_of("rv_level_lag1"))
  }

  ## The feature vector a run will actually estimate on, RESOLVED from the
  ## dataset rather than declared.
  ##
  ## feature_set is signed as the label "har" or "harx", so a change to
  ## select_features() -- which decides what those labels mean -- would leave
  ## every signature untouched and let a cached fit be reused for a different
  ## design matrix. Signing the resolved vector closes that.
  ##
  ## Column names only, so the read is cached per artifact rather than repeated
  ## once per run.
  resolve_feature_names <- local({
    seen <- new.env(parent = emptyenv())
    function(cov_path, feature_set, key_col){
      if (is.null(seen[[cov_path]])) seen[[cov_path]] <- readRDS(cov_path)[0, ]
      setdiff(names(select_features(seen[[cov_path]], feature_set, key_col)),
              c("month_id", key_col))
    }
  })

  ## Covariates and target must describe exactly the same observations, in the
  ## same order, keyed on (month_id[, key]). validator() rejects anything else.
  align_covariates_target <- function(covariates, target, key_col){

    id_cols <- c("month_id", key_col)

    paired <- covariates %>%
      dplyr::select(dplyr::all_of(id_cols)) %>%
      dplyr::inner_join(target, by = id_cols)

    list(
      covariates = covariates %>%
        dplyr::semi_join(paired, by = id_cols) %>%
        dplyr::arrange(dplyr::across(dplyr::all_of(id_cols))),
      target = paired %>%
        dplyr::arrange(dplyr::across(dplyr::all_of(id_cols)))
    )
  }


# Run matrix--------------------------------------------------------------------

  run_matrix <- build_run_matrix(
    run_1986     = run_1986,
    run_1927     = run_1927,
    run_panel    = run_panel,
    run_spx_only = run_spx_only,
    models       = models_to_run,
    preprocessing = preprocessing_to_run,
    tuning_methods = tuning_methods_to_run
  )

  ## Output path. Encodes the run, but is never used to identify it: only the
  ## recorded provenance does that.
  output_path_for <- function(row){
    method_tag <- if (is.na(row$tuning_method)) "none" else
      if (row$tuning_method == "grid_search") "grid" else "bayes"
    file.path(model_dirs_v3, row$sample,
              paste0(paste(row$model_id, row$universe, row$preprocessing,
                           method_tag, row$sample, sep = "_"), ".rds"))
  }

  ## Everything needed to fit one row, assembled from the specification above.
  ## The SAME object supplies the call arguments and the cache key, so the two
  ## cannot describe different runs.
  build_run <- function(row){

    spec     <- model_spec[[row$model_id]]
    key_col  <- if (row$universe == "panel") "name" else NULL
    cov_path <- dataset_path(row$universe, row$sample, row$preprocessing)
    tgt_path <- target_path(row$universe)

    feature_names_requested <- resolve_feature_names(cov_path,
                                                     spec$feature_set, key_col)

    ### Untuned models carry an empty domain and no budget
    hs <- if (is.null(spec$hyper)) NULL else hyper_spec[[spec$hyper]]

    domain <- if (is.null(hs)) list() else
      if (identical(row$tuning_method, "grid_search")) hs$grid else hs$bayes

    budget <- if (is.null(hs)){
      list(n_iter = 10L, init_points = 5L, k_iter = 2L, acq = "ucb")
    } else hs$budget

    ### HAR is fitted once per preprocessing specification. It has no
    ### algorithmic hyperparameter search, so a grid copy and a Bayesian copy
    ### would be two names for one number. tuning_method is still required by
    ### the engine signature, hence the placeholder.
    tuning_method <- if (is.na(row$tuning_method)) "grid_search" else
      row$tuning_method

    wf_args <- list(
      model                   = spec$model,
      hyper_grid_domain_list  = domain,
      fixed_params            = if (is.null(hs)) list()      else hs$fixed,
      int_params              = if (is.null(hs)) character() else hs$int,
      log10_params            = if (is.null(hs)) character() else
                                  if (identical(row$tuning_method, "grid_search"))
                                    character() else hs$log10,
      tuning_method           = tuning_method,
      obj_fun                 = design$obj_fun,
      eval_metric             = design$eval_metric,
      huber_delta             = design$huber_delta,
      quantile_tau            = design$quantile_tau,
      train_n                 = spec$train_n,
      val_n                   = spec$val_n,
      rebal_months            = design$rebal_months,
      early_stop              = spec$early_stop,
      gsm_algo                = design$gsm_algo,
      upper_quant_wins        = design$upper_quant_wins,
      lower_quant_wins        = design$lower_quant_wins,
      n_iter                  = budget$n_iter,
      init_points             = budget$init_points,
      k_iter                  = budget$k_iter,
      acq                     = budget$acq,
      n_ensembles             = spec$n_ensembles,
      keras_architecture_pars = spec$arch,
      parallel                = spec$parallel,
      key_col                 = key_col,
      ### NN seeds are derived from the rebalance date; everything else is fixed
      .test_seed              = if (spec$model == "nn") NA else design$seed
    )

    run_spec <- list(
      dataset_version          = "v3",
      universe                 = paste(row$universe, row$sample, sep = "_"),
      inclusion_policy         = paste(
        "usa JKP vw_cap factors, industries excluded, market series pooled in;",
        "unbalanced, each key drops its first 2 months then requires a",
        "complete HAR block"
      ),
      covariates_artifact      = basename(cov_path),
      covariates_sha256        = hash_artifact(cov_path),
      target_artifact          = basename(tgt_path),
      target_sha256            = hash_artifact(tgt_path),
      preprocessing            = row$preprocessing,
      winsorize_range          = if (row$preprocessing == "range"){
        c(0.025, 0.975)
      } else c(0.01, 0.99),
      scaling_min_n            = 36L,
      target_transform         = target_transform_id,
      target_inverse_transform = target_inverse_id,
      feature_set              = spec$feature_set,
      model_id                 = row$model_id,
      ### Columns the forest must be allowed to split on at every node, on top
      ### of whatever mtry samples. rf_always_split() in helpers.R decides this
      ### from the data, but the decision changes the estimator, so it is
      ### recorded here and signs the run rather than living only in code.
      ###
      ### Only ranger consumes it. The other engines have no equivalent
      ### mechanism, so the field is NULL for them and says so explicitly.
      ###
      ### The design matrix this run will actually estimate on, resolved from
      ### the artifact. Signing it means a change to select_features() cannot
      ### slip past the cache behind an unchanged "har"/"harx" label.
      feature_names_requested  = feature_names_requested,
      ### Derived by calling the SAME helper the two ranger call sites call, on
      ### the resolved feature vector, rather than restating its rule here.
      ### The previous version hardcoded "rv_level_lag1" under a condition that
      ### mirrored select_features(): editing rf_always_split() to return NULL
      ### then changed the estimator while leaving this field, and therefore
      ### the signature, untouched -- a false cache hit. Routing both through
      ### one function means they cannot disagree about the rule.
      forced_split_cols        = if (identical(spec$model, "rf")){
        rf_always_split(feature_names_requested)
      } else NULL
    )

    ### The cache key. Built from the same wf_args that drive the fit, so a
    ### changed argument necessarily changes the key.
    requested_provenance <- c(
      run_spec,
      wf_args[c("model", "key_col", "train_n", "val_n", "rebal_months",
                "obj_fun", "eval_metric", "huber_delta", "quantile_tau",
                "tuning_method", "fixed_params", "int_params", "log10_params",
                "n_iter", "init_points", "k_iter", "acq",
                "early_stop", "n_ensembles", "keras_architecture_pars")],
      list(
        target_name    = "rv_month",
        tunable_domain = wf_args$hyper_grid_domain_list,
        seed           = wf_args$.test_seed
      )
    )

    list(
      row                  = row,
      wf_args              = wf_args,
      run_spec             = run_spec,
      requested_provenance = requested_provenance,
      cov_path             = cov_path,
      tgt_path             = tgt_path,
      key_col              = key_col,
      output_path          = output_path_for(row)
    )
  }


# Cache scan--------------------------------------------------------------------

  ## Reads every candidate object and compares RECORDED PROVENANCE.
  ## The filename is never evidence. Only V3 objects carrying complete matching
  ## provenance may be reused; an object with no record of what it is must not
  ## be treated as a cache hit.
  ## Each candidate is deserialized ONCE and reduced to its signature. Scanning
  ## per run instead would re-read every cached object dozens of times, and
  ## these objects carry fitted models.
  build_cache_index <- function(dirs){

    candidates <- unlist(lapply(
      dirs[dir.exists(dirs)],
      function(d) list.files(d, pattern = "\\.rds$",
                             full.names = TRUE, recursive = TRUE)
    ))

    index <- list()

    for (candidate in candidates){

      obj <- tryCatch(readRDS(candidate), error = function(e) NULL)

      ### Anything that is not a backtest, or that carries no provenance, is
      ### skipped rather than guessed at. Every pre-v3 object falls here: an
      ### artifact with no record of what it is must never be reused.
      if (!is.null(obj) && methods::is(obj, "wf_backtest_results")){

        stored <- obj@backtest_meta$provenance

        if (!is.null(stored)){
          index[[length(index) + 1L]] <- list(
            file      = candidate,
            signature = tryCatch(compute_run_signature(stored),
                                 error = function(e) NA_character_)
          )
        }
      }

      rm(obj)
      gc()
    }

    index
  }

  find_cached_run <- function(requested_provenance, index){

    if (length(index) == 0L) return(NULL)

    wanted <- compute_run_signature(requested_provenance)

    for (entry in index){
      if (identical(entry$signature, wanted)) return(entry$file)
    }

    NULL
  }

  ## Build every run, then report before doing anything irreversible
  runs        <- lapply(seq_len(nrow(run_matrix)),
                        function(i) build_run(run_matrix[i, ]))
  cache_index <- build_cache_index(cache_dirs)

  cat("\nCached objects carrying provenance:", length(cache_index), "\n")

  cache_report <- dplyr::bind_rows(lapply(runs, function(run){

    hit <- find_cached_run(run$requested_provenance, cache_index)

    tibble::tibble(
      universe      = run$row$universe,
      sample        = run$row$sample,
      model_id      = run$row$model_id,
      preprocessing = run$row$preprocessing,
      tuning        = dplyr::coalesce(run$row$tuning_method, "none"),
      dataset_ok    = file.exists(run$cov_path) && file.exists(run$tgt_path),
      cache         = if (is.null(hit)) "MISS" else "HIT",
      cached_file   = if (is.null(hit)) NA_character_ else basename(hit),
      output        = basename(run$output_path)
    )
  }))

  cat("\n=== V3 run matrix ===\n")
  print(as.data.frame(cache_report), row.names = FALSE)
  cat("\nRuns:", nrow(cache_report),
      "| cache hits:", sum(cache_report$cache == "HIT"),
      "| to fit:",     sum(cache_report$cache == "MISS"),
      "| missing datasets:", sum(!cache_report$dataset_ok), "\n")

  if (isTRUE(dry_run)){
    cat("\ndry_run is TRUE: nothing was fitted. Set dry_run <- FALSE to run.\n")
  }


# Fit loop----------------------------------------------------------------------

  if (!isTRUE(dry_run)){

    for (run in runs){

      run_label <- paste(run$row$model_id, run$row$universe, run$row$sample,
                         run$row$preprocessing,
                         dplyr::coalesce(run$row$tuning_method, "none"),
                         sep = " | ")

      ### Reuse only on a complete provenance match
      hit <- find_cached_run(run$requested_provenance, cache_index)
      if (!is.null(hit)){
        cat("\n[CACHE HIT]", run_label, "->", basename(hit), "\n")
        next
      }

      if (!file.exists(run$cov_path) || !file.exists(run$tgt_path)){
        cat("\n[SKIP - dataset missing]", run_label, "\n")
        next
      }

      cat("\n[FIT]", run_label, "\n")

      #### Load, subset to the feature set, transform the target and align
      covariates_run <- readRDS(run$cov_path) %>%
        select_features(feature_set = run$run_spec$feature_set,
                        key_col     = run$key_col)

      target_run <- readRDS(run$tgt_path) %>%
        dplyr::mutate(rv_month = target_transform_fun(rv_month))

      aligned <- align_covariates_target(covariates_run, target_run,
                                         key_col = run$key_col)

      #### Parallel backend, per model. Set here rather than inside a helper so
      #### the plan is never changed as a side effect of a low-level call.
      if (isTRUE(run$wf_args$parallel)){
        future::plan("multisession")
        doFuture::registerDoFuture()
      } else {
        future::plan("sequential")
      }

      #### Fit. run_spec travels with the call, so provenance is attached inside
      #### run_walk_forward_validation() rather than appended afterwards.
      backtest_res <- do.call(
        run_walk_forward_validation,
        c(run$wf_args,
          list(
            target     = aligned$target,
            covariates = aligned$covariates,
            run_spec   = run$run_spec,
            verbose    = TRUE
          ))
      )

      #### Save
      dir.create(dirname(run$output_path), recursive = TRUE, showWarnings = FALSE)
      saveRDS(backtest_res, run$output_path)

      cat("[SAVED]", basename(run$output_path),
          "| signature:", substr(backtest_res@backtest_meta$run_signature, 1, 12),
          "\n")
    }

    future::plan("sequential")
  }

# End of V3 model runner--------------------------------------------------------

  ## Result aggregation and reporting belong in separate V3 analysis scripts.
