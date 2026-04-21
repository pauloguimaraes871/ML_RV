# Load packages and datasets-----------------------------------------------------

  ## Packages
  renv::install("gt")
  renv::install("slider")
  renv::install("xts")
  library("dplyr")

  ## Helpers and classes
  source(file.path(here::here(), "R", "helpers.R"))
  source(file.path(here::here(), "R", "classes.R"))
  
  ## For plots
  grDevices::windowsFonts(Helvetica = grDevices::windowsFont("Arial"))

  ## Covariates
  covariates_1927_df <- readRDS(file.path(here::here(), 
                                          "data", "covariates_1927.rds"))
  covariates_1962_df <- readRDS(file.path(here::here(),
                                          "data", "covariates_1962.rds"))
  covariates_1986_df <- readRDS(file.path(here::here(),
                                          "data", "covariates_1986.rds"))
  
  ## Target
  target            <- readRDS(file.path(here::here(), "data", "crsp_monthly.rds"))

# Summarize---------------------------------------------------------------------
  
  ## Summary tables
   
    ### Apply!
    summary_1927   <- make_var_summary(covariates_1927_df)
    summary_1962   <- make_var_summary(covariates_1962_df)
    summary_1986   <- make_var_summary(covariates_1986_df)
    summary_target <- make_var_summary(target)
    
    ### Save
    gt::gtsave(summary_1927,   "summary_1927.html")  
    gt::gtsave(summary_1962,   "summary_1962.html")
    gt::gtsave(summary_1986,   "summary_1986.html")
    gt::gtsave(summary_target, "summary_target.html")
  
  
  ##  Faceted plots
  
    ### Apply!
      #### 1927 plots
      rv_vars        <- stringr::str_subset(names(covariates_1927_df), "^rv_")
      rq_vars        <- stringr::str_subset(names(covariates_1927_df), "^rq_")
      jump_vars      <- stringr::str_subset(names(covariates_1927_df), "^jump_")
      cont_vars      <- stringr::str_subset(names(covariates_1927_df), "^cont_")
      negret_vars    <- stringr::str_subset(names(covariates_1927_df), "^negret_")
      past_rv_vars   <- c(rv_vars, rq_vars, jump_vars, cont_vars, negret_vars)
      macro_vars     <- c("ppi_vol", "ip_vol")
      sent_vars      <- c("gpr_lag1")
      macro_fin_vars <- c("dy_lag1", "ey_lag1")
      
      
      plot_target <- plot_ts_grid(
        target, "rv_month", ncol = 1,
        title = "Monthly S&P 500 Realized Variance (RV): 1927-2024",
        smooth = FALSE
      )
      plot_target
      ggplot2::ggsave("fig_target.png", plot = plot_target,
                      width = 6, height = 4, dpi = 300)
      plot_rv_1927 <- plot_ts_grid(
        covariates_1927_df, rv_vars, ncol = 3,
        title = "Realized Variance (RV) Measures: 1927-2024"
        )
      plot_rv_1927
      ggplot2::ggsave("fig_rv_grid_1927.png", plot = plot_rv_1927,
                      width = 9, height = 6, dpi = 300)
      
      plot_rq_1927 <- plot_ts_grid(
        covariates_1927_df, rq_vars, ncol = 3,
        title = "Realized Quarticity (RQ) Measures: 1927-2024"
      )
      plot_rq_1927
      ggplot2::ggsave("fig_rq_grid_1927.png", plot = plot_rq_1927,
                      width = 9, height = 6, dpi = 300)
      
      plot_jump_1927 <- plot_ts_grid(
        covariates_1927_df, jump_vars, ncol = 3,
        title = "Jump Variation Measures: 1927-2024"
      )
      plot_jump_1927
      ggplot2::ggsave("fig_jump_grid_1927.png", plot = plot_jump_1927,
                      width = 9, height = 6, dpi = 300)
      plot_cont_1927 <- plot_ts_grid(
        covariates_1927_df, cont_vars, ncol = 3,
        title = "Continuous Variation Measures: 1927-2024"
      )
      plot_cont_1927
      ggplot2::ggsave("fig_cont_grid_1927.png", plot = plot_cont_1927,
                      width = 9, height = 6, dpi = 300)
      plot_negret_1927 <- plot_ts_grid(
        covariates_1927_df, negret_vars, ncol = 3,
        title = "Negative Return Measures: 1927-2024"
      )
      plot_negret_1927
      ggplot2::ggsave("fig_negret_grid_1927.png", plot = plot_negret_1927,
                      width = 9, height = 6, dpi = 300)
      plot_past_rv_1927 <- plot_ts_grid(
        covariates_1927_df, past_rv_vars, ncol = 3,
        title = "Past S&P 500 Realized Variance (RV) Measures: 1927-2024"
      )
      plot_past_rv_1927
      plot_macro_1927 <- plot_ts_grid(
        covariates_1927_df, macro_vars, ncol = 3,
        title = "Macroeconomic Volatility Variables: 1927-2024"
      )
      plot_macro_1927
      ggplot2::ggsave("fig_macro_grid_1927.png", plot = plot_macro_1927,
                      width = 6, height = 6, dpi = 300)
      plot_sent_1927 <- plot_ts_grid(
        covariates_1927_df, sent_vars, ncol = 1,
        title = "Lagged Log Differences of Sentiment Variables: 1927-2024"
      )
      plot_sent_1927
      plot_macro_fin_1927 <- plot_ts_grid(
        covariates_1927_df, macro_fin_vars, ncol = 2,
        title = "Macrofinancial Variables: 1927-2024"
      )
      plot_macro_fin_1927
      ggplot2::ggsave("fig_macro_fin_grid_1927.png", plot = plot_macro_fin_1927,
                      width = 6, height = 6, dpi = 300)
      
      #### 1986 plots
      macro_vars     <- c("ppi_vol", "ip_vol",  "mbas_vol")
      sent_vars      <- c("gpr_lag1", "log_cons_sent_diff_lag1", "epu_lag1")
      macro_fin_vars <- c("dy_lag1", "ey_lag1", "dfr_lag1", "dfy_lag1",
                          "tms_lag1", "tbill_lag1")
      implied_vol    <- c("vix_lag1")
      rv_vars_oil   <- c("oil_rv_month_lag1", "oil_rv_tw_lag1", "oil_rv_td_lag1")
                          

      plot_macro_1986 <- plot_ts_grid(
        covariates_1986_df, macro_vars, ncol = 2,
        title = "Macroeconomic Volatility Variables: 1986-2024"
      )
      plot_macro_1986
      ggplot2::ggsave("fig_macro_grid_1986.png", plot = plot_macro_1986,
                      width = 6, height = 6, dpi = 300)
      plot_macro_fin_1986 <- plot_ts_grid(
        covariates_1986_df, macro_fin_vars, ncol = 2,
        title = "Macrofinancial Variables: 1986-2024"
      )
      plot_macro_fin_1986
      ggplot2::ggsave("fig_macro_fin_grid_1986.png", plot = plot_macro_fin_1986,
                      width = 6, height = 6, dpi = 300)
      plot_implied_vol_1986 <- plot_ts_grid(
        covariates_1986_df, implied_vol, ncol = 1,
        title = "Implied Volatility Measure (VIX Index): 1986-2024"
      )
      plot_implied_vol_1986
      ggplot2::ggsave("fig_implied_vol_grid_1986.png", plot = plot_implied_vol_1986,
                      width = 6, height = 4, dpi = 300)
      plot_sent_1986 <- plot_ts_grid(
        covariates_1986_df, sent_vars, ncol = 1,
        title = "Lagged Log Differences of Sentiment Variables: 1986-2024"
      )

    
# Preprocessing-----------------------------------------------------------------

  ## Covariates
        ### Helper  
        prep_covariates <- function(df,
                                    scaling = c("zscore", "range"),
                                    winsorize_range = c(0.01, 0.99),
                                    min_n = 36L) {
          
          scaling <- match.arg(scaling)
          
          df %>%
          ### Use LOCF to fill NAs
          impute_locf() %>%
          dplyr::arrange(month_id) %>%
          ### Add month dummies
          { mm <- stats::model.matrix(~ month - 1, data = .)
          colnames(mm) <- sub("^month", "m_", colnames(mm))
          dplyr::bind_cols(., as.data.frame(mm))
          } %>%
          dplyr::select(-month) %>%
          ### Winsorize
          dplyr::mutate(
            dplyr::across(
              dplyr::where(is.numeric) & 
                !dplyr::any_of("sqrt_days") & !dplyr::starts_with("m_"),
              ~ winsorize_expanding(.x, 
                                    p_lo = min(winsorize_range),
                                    p_hi = max(winsorize_range),
                                    min_n = min_n)
              )
            ) %>%
          ### Scale
          dplyr::mutate(
            dplyr::across(
              dplyr::where(is.numeric) &
                !dplyr::any_of("sqrt_days") & !dplyr::starts_with("m_"),
              ~ if (scaling == "zscore") {
                scale_expanding(.x, min_n = min_n)
              } else {
                rescale_expanding_to_unit(.x, min_n = min_n)
              }
            )
          )
      }
  
        ### Apply    
        covariates_1927_df_gold <- prep_covariates(covariates_1927_df, scaling = "zscore")
        covariates_1962_df_gold <- prep_covariates(covariates_1962_df, scaling = "zscore")
        covariates_1986_df_gold <- prep_covariates(covariates_1986_df, scaling = "zscore")
        
        covariates_1927_df_gold2 <- prep_covariates(
          covariates_1927_df, scaling = "range", winsorize_range = c(0.025,0.975)
          )
        covariates_1962_df_gold2 <- prep_covariates(
          covariates_1962_df, scaling = "range", winsorize_range = c(0.025,0.975)
        )
        covariates_1986_df_gold2 <- prep_covariates(
          covariates_1986_df, scaling = "range", winsorize_range = c(0.025,0.975)
        )
        
  ## Target     
  target_gold <- target %>% 
    dplyr::arrange(month_id) %>%
    dplyr::mutate(
      rv_month = boxcox_apply(rv_month, 0, 1e-08)
    )
  
  ## Save covariates_gold and target_gold to data folder
  saveRDS(covariates_1927_df_gold, here::here("data", "covariates_1927_df_gold.rds"))
  saveRDS(covariates_1962_df_gold, here::here("data", "covariates_1962_df_gold.rds"))
  saveRDS(covariates_1986_df_gold, here::here("data", "covariates_1986_df_gold.rds"))
  saveRDS(covariates_1927_df_gold2, here::here("data", "covariates_1927_df_gold2.rds"))
  saveRDS(covariates_1962_df_gold2, here::here("data", "covariates_1962_df_gold2.rds"))
  saveRDS(covariates_1986_df_gold2, here::here("data", "covariates_1986_df_gold2.rds"))
  saveRDS(target_gold, here::here("data", "target_gold.rds"))
  
# Run models for 1927--------------------------------------------------------
    
  ### Make sure target and covariates share same dates
    shared_dates <- dplyr::intersect(
      covariates_1927_df_gold %>% dplyr::pull(month_id),
      target_gold %>% dplyr::pull(month_id)
    ) %>% as.Date()
    
    target_gold <- target_gold %>%
      dplyr::filter(month_id %in% shared_dates) %>%
      dplyr::arrange(month_id)
    
    covariates_1927_df_gold <- covariates_1927_df_gold %>%
      dplyr::filter(month_id %in% shared_dates) %>%
      dplyr::arrange(month_id)
    
    covariates_1927_df_gold2 <- covariates_1927_df_gold2 %>%
      dplyr::filter(month_id %in% shared_dates) %>%
      dplyr::arrange(month_id)
  
    ### HAR
    harx_backtest_res <- run_walk_forward_validation(
      target = target_gold, 
      covariates = covariates_1927_df_gold2,
      model = "har",
      hyper_grid_domain_list = list(),
      obj_fun = "squared_error", eval_metric = "rmse",
      huber_delta = 1, quantile_tau = 0.5,
      train_n = 210L, val_n = 0L,
      rebal_months = c(6),
      early_stop = NULL,
      gsm_algo = "ols",
      upper_quant_wins = 0.95,
      lower_quant_wins = 0.05,
      n_iter = 10L, init_points = 5L,
      k_iter = 2L, acq = "ucb",
      keras_architecture_pars = NULL,
      parallel = TRUE,
      verbose = TRUE
    )
    harx_backtest_res@backtest_meta$dataset <- "covariates_1927_df_gold2"
    
      #### Save RDS
      saveRDS(harx_backtest_res,    "harx_backtest_res_rescaled.rds")
      
    ### GLMNET
      future::plan("multisession")
      doFuture::registerDoFuture()
      
      glmnet_backtest_grid_res <- run_walk_forward_validation(
        target = target_gold, 
        covariates = covariates_1927_df_gold2,
        model = "glmnet",
        hyper_grid_domain_list <- list(
          alpha            = c(0, 0.25, 0.5, 0.75, 1),
          lambda.min.ratio = c(1e-4, 3e-4, 1e-3, 3e-3, 1e-2)
        ),
        obj_fun = "squared_error", eval_metric = "rmse",
        huber_delta = 1, quantile_tau = 0.5,
        train_n = 120L, val_n = 90L,
        rebal_months = c(6),
        early_stop = NULL,
        gsm_algo = "ols",
        tuning_method = "grid_search",
        upper_quant_wins = 0.95,
        lower_quant_wins = 0.05,
        n_iter = 10L, init_points = 5L,
        k_iter = 2L, acq = "ucb",
        keras_architecture_pars = NULL,
        parallel = FALSE,
        verbose = TRUE,
        .test_seed = 123
      )
      glmnet_backtest_grid_res@backtest_meta$dataset <- "covariates_1927_df_gold2"
      
    
      #### Save RDS
      saveRDS(glmnet_backtest_grid_res, "glmnet_backtest_grid_res.rds")
      
      ### RF
      #future::plan("multisession")
      #doFuture::registerDoFuture()
      
      rf_backtest_grid_res <- run_walk_forward_validation(
        target = target_gold, 
        covariates = covariates_1927_df_gold2,
        model = "rf",
        hyper_grid_domain_list = list(
          mtry       = c(0.1, 0.3, 0.5, 0.7, 0.9),
          num.trees  = c(300L, 600L, 1000L, 1500L),
          max.depth  = c(3L, 6L, 10L, 16L),
          min.bucket = c(1L, 3L, 6L)
        ),
        obj_fun = "squared_error", eval_metric = "rmse",
        huber_delta = 1, quantile_tau = 0.5,
        train_n = 120L, val_n = 90L,
        rebal_months = c(6),
        early_stop = NULL,
        gsm_algo = "ols",
        upper_quant_wins = 0.95,
        lower_quant_wins = 0.05,
        tuning_method = "grid_search",
        n_iter = 25L, init_points = 10L,
        k_iter = 20L, acq = "ucb",
        keras_architecture_pars = NULL,
        parallel = FALSE,
        verbose = TRUE,
        .test_seed = 123
      )
      rf_backtest_grid_res@backtest_meta$dataset <- "covariates_1927_df_gold2"
      
      #### Save RDS
      saveRDS(rf_backtest_grid_res, "rf_backtest_grid_res.rds")
    
      
      ### XGB
      future::plan("multisession")
      doFuture::registerDoFuture()
      xgb_backtest_grid_res <- run_walk_forward_validation(
        target = target_gold, 
        covariates = covariates_1927_df_gold2,
        model = "xgb",
        hyper_grid_domain_list = list(
          min_child_weight = c(2L, 5L, 10L),
          max_depth        = c(2L, 3L, 4L, 5L),
          subsample        = c(0.6, 0.8, 0.95),
          colsample_bytree = c(0.6, 0.8, 0.95),
          eta              = c(0.05, 0.1, 0.2),
          alpha            = c(0, 0.5, 1, 2),
          gamma            = c(0, 1, 3, 5),
          nrounds          = c(150L, 300L, 500L, 800L)
        ),
        obj_fun = "squared_error", eval_metric = "rmse",
        huber_delta = 1, quantile_tau = 0.5,
        tuning_method = "grid_search",
        train_n = 120L, val_n = 90L,
        rebal_months = c(6),
        early_stop = 30L,
        gsm_algo = "ols",
        upper_quant_wins = 0.95,
        lower_quant_wins = 0.05,
        n_iter = 12L, init_points = 12L,
        k_iter = 8L, acq = "ucb",
        keras_architecture_pars = NULL,
        parallel = TRUE,
        verbose = TRUE,
        .test_seed = 123
      )
      xgb_backtest_grid_res@backtest_meta$dataset <- "covariates_1927_df_gold2"
      
      
      #### Save RDS
      saveRDS(xgb_backtest_grid_res, "xgb_backtest_grid_res.rds")
    
      ##NN
      nn_hyper_grid <- list(
        regularizer_l1   = c(1e-5, 1e-4, 1e-3, 1e-2),
        regularizer_l2   = c(1e-5, 1e-4, 1e-3, 1e-2),
        droprate         = c(0.25, 0.5, 0.75),
        lr               = c(1e-4, 5e-4, 1e-3, 5e-3, 1e-2),
        size_of_batch    = c(8L, 16L, 32L),
        number_of_epochs = c(50L, 100L, 150L)
      )
      
    
      ### NN1
      future::plan("sequential")
      
      nn1_backtest_grid_res <- run_walk_forward_validation(
        target = target_gold, 
        covariates = covariates_1927_df_gold2,
        model = "nn",
        keras_architecture_pars = list(
          units             = 32,
          n_layers          = 1,
          activation        = "relu",
          nn_optimizer      = "Adam",
          batch_norm_option = TRUE
        ),
        hyper_grid_domain_list = nn_hyper_grid,
        obj_fun = "squared_error", eval_metric = "rmse",
        tuning_method = "grid_search",
        huber_delta = 1, quantile_tau = 0.5,
        train_n = 120L, val_n = 90L,
        rebal_months = c(6),
        early_stop = 20L,
        gsm_algo = "ols",
        upper_quant_wins = 0.95,
        lower_quant_wins = 0.05,
        n_ensembles = 5,
        n_iter = 16L, init_points = 12L,
        k_iter = 2L, acq = "ucb",
        parallel = FALSE,
        verbose = TRUE,
        .test_seed = 123
      )
      nn1_backtest_grid_res@backtest_meta$dataset <- "covariates_1927_df_gold2"
      
      
      #### Save RDS
      saveRDS(nn1_backtest_grid_res, "nn1_backtest_grid_res.rds")
      
      ### NN2
      future::plan("sequential")
      
      nn2_backtest_grid_res <- run_walk_forward_validation(
        target = target_gold, 
        covariates = covariates_1927_df_gold2,
        model = "nn",
        keras_architecture_pars = list(
          units             = c(32, 16),
          n_layers          = 2,
          activation        = rep("relu", 2),
          nn_optimizer      = "Adam",
          batch_norm_option = rep(TRUE,2)
        ),
        hyper_grid_domain_list = nn_hyper_grid,
        obj_fun = "squared_error", eval_metric = "rmse",
        huber_delta = 1, quantile_tau = 0.5,
        train_n = 120L, val_n = 90L,
        rebal_months = c(6),
        tuning_method = "grid_search",
        early_stop = 20L,
        gsm_algo = "ols",
        upper_quant_wins = 0.95,
        lower_quant_wins = 0.05,
        n_ensembles = 5,
        n_iter = 16L, init_points = 12L,
        k_iter = 2L, acq = "ucb",
        parallel = FALSE,
        verbose = TRUE,
        .test_seed = 123
      )
      nn2_backtest_grid_res@backtest_meta$dataset <- "covariates_1927_df_gold2"
      
      #### Save RDS
      saveRDS(nn2_backtest_grid_res, "nn2_backtest_grid_res_ens5.rds") 
    
      
      ### NN3
      future::plan("sequential")
      
      nn3_backtest_grid_res <- run_walk_forward_validation(
        target = target_gold, 
        covariates = covariates_1927_df_gold2,
        model = "nn",
        keras_architecture_pars = list(
          units             = c(32, 16, 8),
          n_layers          = 3,
          activation        = rep("relu", 3),
          nn_optimizer      = "Adam",
          batch_norm_option = rep(TRUE,3)
        ),
        hyper_grid_domain_list = nn_hyper_grid,
        obj_fun = "squared_error", eval_metric = "rmse",
        tuning_method = "grid_search",
        huber_delta = 1, quantile_tau = 0.5,
        train_n = 120L, val_n = 90L,
        rebal_months = c(6),
        early_stop = 20L,
        gsm_algo = "ols",
        n_ensembles = 5,
        upper_quant_wins = 0.95,
        lower_quant_wins = 0.05,
        n_iter = 16L, init_points = 12L,
        k_iter = 2L, acq = "ucb",
        parallel = FALSE,
        verbose = TRUE,
        .test_seed = 123
      )
      nn3_backtest_grid_res@backtest_meta$dataset <- "covariates_1927_df_gold2"
      
      #### Save RDS
      saveRDS(nn3_backtest_grid_res, "nn3_backtest_grid_res_rescaled_ens5.rds") 
      
      ### NN4
      future::plan("sequential")
      
      nn4_backtest_grid_res <- run_walk_forward_validation(
        target = target_gold, 
        covariates = covariates_1927_df_gold2,
        model = "nn",
        keras_architecture_pars = list(
          units             = c(32, 16, 8, 4),
          n_layers          = 4,
          activation        = rep("relu", 4),
          nn_optimizer      = "Adam",
          batch_norm_option = rep(TRUE,4)
        ),
        hyper_grid_domain_list = nn_hyper_grid,
        obj_fun = "squared_error", eval_metric = "rmse",
        tuning_method = "grid_search",
        huber_delta = 1, quantile_tau = 0.5,
        train_n = 120L, val_n = 90L,
        rebal_months = c(6),
        early_stop = 20L,
        gsm_algo = "ols",
        upper_quant_wins = 0.95,
        n_ensembles = 5,
        lower_quant_wins = 0.05,
        n_iter = 16L, init_points = 12L,
        k_iter = 2L, acq = "ucb",
        parallel = FALSE,
        verbose = TRUE,
        .test_seed = 123
      )
      nn4_backtest_grid_res@backtest_meta$dataset <- "covariates_1927_df_gold2"
      
      #### Save RDS
      saveRDS(nn4_backtest_grid_res, "nn4_backtest_grid_res_rescaled_ens5.rds") 
      
      ### NN5
      future::plan("sequential")
      
      nn5_backtest_grid_res <- run_walk_forward_validation(
        target = target_gold, 
        covariates = covariates_1927_df_gold2,
        model = "nn",
        keras_architecture_pars = list(
          units             = c(32, 16, 8, 4, 2),
          n_layers          = 5,
          activation        = rep("relu", 5),
          nn_optimizer      = "Adam",
          batch_norm_option = rep(TRUE,5)
        ),
        hyper_grid_domain_list = nn_hyper_grid,
        obj_fun = "squared_error", eval_metric = "rmse",
        tuning_method = "grid_search",
        huber_delta = 1, quantile_tau = 0.5,
        train_n = 120L, val_n = 90L,
        rebal_months = c(6),
        early_stop = 20L,
        gsm_algo = "ols",
        upper_quant_wins = 0.95,
        lower_quant_wins = 0.05,
        n_iter = 16L, init_points = 12L,
        n_ensembles = 5,
        k_iter = 2L, acq = "ucb",
        parallel = FALSE,
        verbose = TRUE,
        .test_seed = 123
      )
      
      #### Save RDS
      saveRDS(nn5_backtest_grid_res, "nn5_backtest_grid_res_ens5.rds") 
      
      #Join tables
      har_backtest_res <- readRDS(file.path(here::here(), "models", "har_backtest_res_rescaled.rds"))
      glmnet_backtest_res <- readRDS(file.path(here::here(), "models", "glmnet_backtest_res_rescaled.rds"))
      rf_backtest_res    <- readRDS(file.path(here::here(), "models", "rf_backtest_res_rescaled.rds"))
      xgb_backtest_res   <- readRDS(file.path(here::here(), "models", "xgb_backtest_res_rescaled.rds"))
      nn1_backtest_res <- readRDS(file.path(here::here(), "models", "nn1_backtest_res_rescaled.rds.rds"))
      nn1_backtest_res_2 <- readRDS(file.path(here::here(), "models", "nn1_backtest_res_2.rds"))
      nn1_backtest_res_3 <- readRDS(file.path(here::here(), "models", "nn1_backtest_res_3.rds"))
      nn2_backtest_res <- readRDS(file.path(here::here(), "models", "nn2_backtest_res.rds"))
      nn3_backtest_res <- readRDS(file.path(here::here(), "models", "nn3_backtest_res.rds"))
      nn4_backtest_res <- readRDS(file.path(here::here(), "models", "nn4_backtest_res.rds"))
      nn5_backtest_res <- readRDS(file.path(here::here(), "models", "nn5_backtest_res.rds"))
      
      #Join all with purrr::reduce
      theme_map <- data.frame(
        feat = c("rv_month_lag1", "rv_tw_lag1", "rv_td_lag1",
                 "rq_month_lag1", "rq_tw_lag1", "rq_td_lag1",
                 "cont_month_lag1", "cont_tw_lag1", "cont_td_lag1",
                 "jump_month_lag1", "jump_tw_lag1", "jump_td_lag1",
                 "negret_month_lag1", "negret_tw_lag1", "negret_td_lag1",
                 "log_epu_diff_lag1", "gpr_lag1", 
                 "ppi_vol", "ip_vol", 
                 "dy_lag1", "ey_lag1",
                 "month", "sqrt_days"),
        theme = c(rep("Past S&P 500 RV Terms", 15),
                  rep("Sentiment Variables", 2),
                  rep("Macroeconomic Variables", 2),
                  rep("Macrofinancial Variables", 2),
                  rep("Calendar Variables", 2)
        )
      )
      
      list_backtest_res <- list(har_backtest_res, glmnet_backtest_res, rf_backtest_res,
                                xgb_backtest_res, nn1_backtest_res_3, nn2_backtest_res,
                                nn3_backtest_res, nn4_backtest_res, nn5_backtest_res)
      names(list_backtest_res) <- c("har", "glmnet", "rf", "xgb", "nn1", "nn2", "nn3", "nn4", "nn5")
      
      purrr::map(seq_along(list_backtest_res), function(table){
        
        list_backtest_res[[table]]@eval_metrics %>%
          dplyr::select(metric, cons_oos) %>%
          ### Rename
          dplyr::rename(!!names(list_backtest_res)[table] := cons_oos) %>%
          dplyr::filter(metric %in% c("rss", "rmse", "mae", "mphe", "mpe", "mape"))
        
      }) %>% 
        purrr::reduce(dplyr::left_join, by = "metric") 
      
# Redo analysis for 1986--------------------------------------------------------
      
      ### Make sure target and covariates share same dates
      shared_dates <- dplyr::intersect(
        covariates_1986_df_gold %>% dplyr::pull(month_id),
        target_gold %>% dplyr::pull(month_id)
      ) %>% as.Date()
      
      target_gold <- target_gold %>%
        dplyr::filter(month_id %in% shared_dates) %>%
        dplyr::arrange(month_id)
      
      covariates_1986_df_gold <- covariates_1986_df_gold %>%
        dplyr::filter(month_id %in% shared_dates) %>%
        dplyr::arrange(month_id)
      
      covariates_1986_df_gold2 <- covariates_1986_df_gold2 %>%
        dplyr::filter(month_id %in% shared_dates) %>%
        dplyr::arrange(month_id)
      
      
      ### HAR
      harx_backtest_res_1986 <- run_walk_forward_validation(
        target = target_gold, 
        covariates = covariates_1986_df_gold2,
        model = "har",
        hyper_grid_domain_list = list(),
        obj_fun = "squared_error", eval_metric = "rmse",
        huber_delta = 1, quantile_tau = 0.5,
        train_n = 210L, val_n = 0L,
        rebal_months = c(6),
        early_stop = NULL,
        tuning_method = "grid_search",
        gsm_algo = "ols",
        upper_quant_wins = 0.95,
        lower_quant_wins = 0.05,
        n_iter = 10L, init_points = 5L,
        k_iter = 2L, acq = "ucb",
        keras_architecture_pars = NULL,
        parallel = TRUE,
        verbose = TRUE
      )
      
      harx_backtest_res_1986@backtest_meta$dataset <- "covariates_1986_df_gold"
      
      
      #### Save RDS
      saveRDS(harx_backtest_res_1986, "harx_backtest_res_1986.rds")
      
      ### GLMNET
      future::plan("multisession")
      doFuture::registerDoFuture()
      
      glmnet_backtest_res_1986 <- run_walk_forward_validation(
        target = target_gold, 
        covariates = covariates_1986_df_gold2,
        model = "glmnet",
        hyper_grid_domain_list = list(
          alpha = c(0, 1),
          lambda.min.ratio = c(1e-4, 0.01)
        ),
        obj_fun = "squared_error", eval_metric = "rmse",
        huber_delta = 1, quantile_tau = 0.5,
        train_n = 150L, val_n = 60L,
        rebal_months = c(6),
        early_stop = NULL,
        gsm_algo = "ols",
        upper_quant_wins = 0.95,
        lower_quant_wins = 0.05,
        n_iter = 15L, init_points = 20L,
        k_iter = 4L, acq = "ucb",
        keras_architecture_pars = NULL,
        parallel = FALSE,
        verbose = TRUE,
        .test_seed = 123
      )
      
      glmnet_backtest_res_1986@backtest_meta$dataset <- "covariates_1986_df_gold2"
      
      #### Save RDS
      saveRDS(glmnet_backtest_res_1986, "glmnet_backtest_res_1986_rescaled.rds")
      
      ### RF
      future::plan("multisession")
      doFuture::registerDoFuture()
      
      rf_backtest_res_1986 <- run_walk_forward_validation(
        target = target_gold, 
        covariates = covariates_1986_df_gold2,
        model = "rf",
        hyper_grid_domain_list = list(
          mtry = c(0.1, 0.9),
          num.trees = c(300L, 1500L),
          max.depth = c(3L, 16L),
          min.bucket = c(1L, 6L)
        ),
        obj_fun = "squared_error", eval_metric = "rmse",
        huber_delta = 1, quantile_tau = 0.5,
        train_n = 120L, val_n = 90L,
        rebal_months = c(6),
        early_stop = NULL,
        gsm_algo = "ols",
        upper_quant_wins = 0.95,
        lower_quant_wins = 0.05,
        n_iter = 25L, init_points = 10L,
        k_iter = 20L, acq = "ucb",
        keras_architecture_pars = NULL,
        parallel = TRUE,
        verbose = TRUE,
        .test_seed = 123
      )
      
      rf_backtest_res_1986@backtest_meta$dataset <- "covariates_1986_df_gold2"
      
      #### Save RDS
      saveRDS(rf_backtest_res_1986, "rf_backtest_res_1986_rescaled.rds")
      
      
      ### XGB
      future::plan("multisession")
      doFuture::registerDoFuture()
      
      xgb_backtest_res_1986 <- run_walk_forward_validation(
        target = target_gold, 
        covariates = covariates_1986_df_gold2,
        model = "xgb",
        hyper_grid_domain_list = list(
          min_child_weight = c(2L, 10L),      
          max_depth        = c(2L, 5L),  
          subsample        = c(0.60, 0.95),
          colsample_bytree = c(0.60, 0.95),
          eta              = c(0.05, 0.20),
          alpha            = c(0.00, 2.00),   
          gamma            = c(0.00, 5.00),  
          nrounds          = c(150L, 800L)  
        ),
        obj_fun = "squared_error", eval_metric = "rmse",
        huber_delta = 1, quantile_tau = 0.5,
        train_n = 120L, val_n = 90L,
        rebal_months = c(6),
        early_stop = 30L,
        gsm_algo = "ols",
        upper_quant_wins = 0.95,
        lower_quant_wins = 0.05,
        n_iter = 12L, init_points = 12L,
        k_iter = 8L, acq = "ucb",
        keras_architecture_pars = NULL,
        parallel = TRUE,
        verbose = TRUE,
        .test_seed = 123
      )
      
      xgb_backtest_res_1986@backtest_meta$dataset <- "covariates_1986_df_gold2"
      
      #### Save RDS
      saveRDS(xgb_backtest_res_1986, "xgb_backtest_res_1986_rescaled.rds")
      
      ##NN
      nn_hyper_grid <- list(
        regularizer_l1   = c(1e-5, 1e-4, 1e-3),
        regularizer_l2   = c(1e-5, 1e-4),
        droprate         = c(0.0, 0.25, 0.5),
        lr               = c(0.001, 0.01, 0.1),
        size_of_batch    = 32L,
        number_of_epochs = 100L
      )
      
      ### NN1
      future::plan(future::sequential)
      
      nn1_backtest_grid_res_1986 <- run_walk_forward_validation(
        target = target_gold, 
        covariates = covariates_1986_df_gold2,
        model = "nn",
        keras_architecture_pars = list(
          units             = 32,
          n_layers          = 1,
          activation        = "relu",
          nn_optimizer      = "Adam",
          batch_norm_option = TRUE
        ),
        hyper_grid_domain_list = nn_hyper_grid,
        obj_fun = "squared_error", eval_metric = "rmse",
        huber_delta = 1, quantile_tau = 0.5,
        train_n = 120L, val_n = 90L,
        rebal_months = c(6),
        early_stop = 20L,
        tuning_method = "grid_search",
        gsm_algo = "ols",
        upper_quant_wins = 0.95,
        lower_quant_wins = 0.05,
        n_ensembles = 5,
        n_iter = 24L, init_points = 12L,
        k_iter = 2L, acq = "ucb",
        parallel = TRUE,
        verbose = TRUE,
        .test_seed = NA
      )
      nn1_backtest_grid_res_1986@backtest_meta$dataset <- "covariates_1986_df_gold2"
      
      #### Save RDS
      saveRDS(nn1_backtest_grid_res_1986, "nn1_backtest_grid_res_1986_resc_ens5.rds")
      
      ### NN2
      future::plan(future::sequential)
      
      nn2_backtest_grid_res_1986 <- run_walk_forward_validation(
        target = target_gold, 
        covariates = covariates_1986_df_gold2,
        model = "nn",
        keras_architecture_pars = list(
          units             = c(32, 16),
          n_layers          = 2,
          activation        = rep("relu", 2),
          nn_optimizer      = "Adam",
          batch_norm_option = rep(TRUE,2)
        ),
        hyper_grid_domain_list = nn_hyper_grid,
        obj_fun = "squared_error", eval_metric = "rmse",
        huber_delta = 1, quantile_tau = 0.5,
        train_n = 120L, val_n = 90L,
        rebal_months = c(6),
        early_stop = 20L,
        gsm_algo = "ols",
        upper_quant_wins = 0.95,
        lower_quant_wins = 0.05,
        n_ensembles = 5,
        tuning_method = "grid_search",
        n_iter = 24L, init_points = 12L,
        k_iter = 2L, acq = "ucb",
        parallel = FALSE,
        verbose = TRUE,
        .test_seed = 123
      )
      nn2_backtest_grid_res_1986@backtest_meta$dataset <- "covariates_1986_df_gold2"
      
      #### Save RDS
      saveRDS(nn2_backtest_grid_res_1986, "nn2_backtest_grid_res_1986_resc_ens5_3.rds") 
      
      
      ### NN3
      future::plan(future::sequential)
      
      nn3_backtest_grid_res_1986 <- run_walk_forward_validation(
        target = target_gold, 
        covariates = covariates_1986_df_gold2,
        model = "nn",
        keras_architecture_pars = list(
          units             = c(32, 16, 8),
          n_layers          = 3,
          activation        = rep("relu", 3),
          nn_optimizer      = "Adam",
          batch_norm_option = rep(TRUE,3)
        ),
        hyper_grid_domain_list = nn_hyper_grid,
        obj_fun = "squared_error", eval_metric = "rmse",
        huber_delta = 1, quantile_tau = 0.5,
        train_n = 120L, val_n = 90L,
        rebal_months = c(6),
        early_stop = 20L,
        gsm_algo = "ols",
        upper_quant_wins = 0.95,
        lower_quant_wins = 0.05,
        n_ensembles = 5,
        tuning_method = "grid_search",
        n_iter = 24L, init_points = 12L,
        k_iter = 2L, acq = "ucb",
        parallel = TRUE,
        verbose = TRUE,
        .test_seed = NA
      )
      nn3_backtest_grid_res_1986@backtest_meta$dataset <- "covariates_1986_df_gold2"
      
      
      #### Save RDS
      saveRDS(nn3_backtest_grid_res_1986, "nn3_backtest_grid_res_1986_resc_ens5_2.rds") 
      
      ### NN4
      future::plan("sequential")
      
      nn4_backtest_grid_res_1986 <- run_walk_forward_validation(
        target = target_gold, 
        covariates = covariates_1986_df_gold2,
        model = "nn",
        keras_architecture_pars = list(
          units             = c(32, 16, 8, 4),
          n_layers          = 4,
          activation        = rep("relu", 4),
          nn_optimizer      = "Adam",
          batch_norm_option = rep(TRUE,4)
        ),
        hyper_grid_domain_list = nn_hyper_grid,
        obj_fun = "squared_error", eval_metric = "rmse",
        huber_delta = 1, quantile_tau = 0.5,
        train_n = 120L, val_n = 90L,
        rebal_months = c(6),
        early_stop = 20L,
        gsm_algo = "ols",
        upper_quant_wins = 0.95,
        lower_quant_wins = 0.05,
        n_ensembles = 5,
        tuning_method = "grid_search",
        n_iter = 24L, init_points = 12L,
        k_iter = 2L, acq = "ucb",
        parallel = FALSE,
        verbose = TRUE,
        .test_seed = NA
      )
      nn4_backtest_grid_res_1986@backtest_meta$dataset <- "covariates_1986_df_gold2"
      
      
      #### Save RDS
      saveRDS(nn4_backtest_grid_res_1986, "nn4_backtest_grid_res_1986_resc_ens5_3.rds") 
      
      ### NN5
      future::plan("sequential")
      
      nn5_backtest_grid_res_1986 <- run_walk_forward_validation(
        target = target_gold, 
        covariates = covariates_1986_df_gold2,
        model = "nn",
        keras_architecture_pars = list(
          units             = c(32, 16, 8, 4, 2),
          n_layers          = 5,
          activation        = rep("relu", 5),
          nn_optimizer      = "Adam",
          batch_norm_option = rep(TRUE,5)
        ),
        hyper_grid_domain_list = nn_hyper_grid,
        obj_fun = "squared_error", eval_metric = "rmse",
        huber_delta = 1, quantile_tau = 0.5,
        train_n = 120L, val_n = 90L,
        rebal_months = c(6),
        early_stop = 20L,
        gsm_algo = "ols",
        upper_quant_wins = 0.95,
        lower_quant_wins = 0.05,
        n_ensembles = 5,
        n_iter = 24L, init_points = 12L,
        k_iter = 2L, acq = "ucb",
        tuning_method = "grid_search",
        parallel = FALSE,
        verbose = TRUE,
        .test_seed = 123
      )
      nn5_backtest_grid_res_1986@backtest_meta$dataset <- "covariates_1986_df_gold2"
      
      #### Save RDS
      saveRDS(nn5_backtest_grid_res_1986, "nn5_backtest_grid_res_1986_resc_ens5.rds") 
      
      ##LSTM
      lstm_backtest_res_1986 <- run_walk_forward_validation(
        target     = target_gold,
        covariates = covariates_1986_df_gold2,
        model      = "lstm",
        
        # LSTM architecture: things that do NOT change during BO
        keras_architecture_pars = list(
          sequence_length = 12L,  # T: how many past months the LSTM "sees"
          units           = 32L,   # number of LSTM units (hidden size)
          nn_optimizer    = "Adam",
          padding         = FALSE
        ),
        
        # Hyperparameters to be tuned by ParBayesianOptimization
        hyper_grid_domain_list = list(
          regularizer_l2   = c(1e-3, 1e-1),      # L2 penalty
          droprate         = c(0.25, 0.75),     # dropout on inputs
          rec_droprate     = c(0.25, 0.75),     # recurrent dropout
          lr               = c(1e-4, 1e-3),     # learning rate
          size_of_batch    = c(8L, 32L),       # batch size
          number_of_epochs = c(40L, 120L)      # max epochs (ES will stop earlier)
          
        ),
        
        obj_fun      = "squared_error",
        eval_metric  = "rmse",
        huber_delta  = 1,
        quantile_tau = 0.5,
        
        train_n      = 120L,
        val_n        = 90L,
        rebal_months = c(6),
        early_stop   = 20L,
        
        gsm_algo          = "ols",
        upper_quant_wins  = 0.95,
        lower_quant_wins  = 0.05,
        
        n_iter      = 16L,
        init_points = 12L,
        k_iter      = 2L,
        acq         = "ucb",
        
        parallel = FALSE,  
        verbose  = TRUE,
        .test_seed = 123
      )
      
      lstm_backtest_res_1986@backtest_meta$dataset <- "covariates_1986_df_gold2"
      
      #### Save RDS
      saveRDS(lstm_backtest_res_1986, "lstm_backtest_res_1986_rescaled_5.rds") 
      

# Collect results---------------------------------------------------------------    
    
    #1927
      #Join tables
      har_backtest         <- readRDS(file.path(here::here(), "models", "1927", "har_backtest_res.rds"))
      har_backtest_resc    <- readRDS(file.path(here::here(), "models", "1927", "har_backtest_res_rescaled.rds"))
      harx_backtest_resc   <- readRDS(file.path(here::here(), "models", "1927", "harx_backtest_res_rescaled.rds"))
      
      glmnet_backtest      <- readRDS(file.path(here::here(), "models", "1927", "glmnet_backtest_res.rds"))
      glmnet_backtest_resc <- readRDS(file.path(here::here(), "models", "1927", "glmnet_backtest_res_rescaled.rds"))
      
      rf_backtest          <- readRDS(file.path(here::here(), "models", "1927", "rf_backtest_res.rds"))
      rf_backtest_resc     <- readRDS(file.path(here::here(), "models", "1927", "rf_backtest_res_rescaled.rds"))
      
      xgb_backtest         <- readRDS(file.path(here::here(), "models", "1927", "xgb_backtest_res.rds"))
      xgb_backtest_resc    <- readRDS(file.path(here::here(), "models", "1927", "xgb_backtest_res_rescaled.rds"))
      
      nn1_backtest             <- readRDS(file.path(here::here(), "models", "1927", "nn1_backtest_res.rds"))
      nn1_backtest_2           <- readRDS(file.path(here::here(), "models", "1927", "nn1_backtest_res_2.rds"))
      nn1_backtest_3           <- readRDS(file.path(here::here(), "models", "1927", "nn1_backtest_res_3.rds"))
      nn1_backtest_resc        <- readRDS(file.path(here::here(), "models", "1927", "nn1_backtest_res_rescaled.rds"))
      nn1_backtest_resc_ens5   <- readRDS(file.path(here::here(), "models", "1927", "nn1_backtest_res_rescaled_ens5.rds"))
      
      nn2_backtest             <- readRDS(file.path(here::here(), "models", "1927", "nn2_backtest_res.rds"))
      nn2_backtest_resc        <- readRDS(file.path(here::here(), "models", "1927", "nn2_backtest_res_rescaled.rds"))
      nn2_backtest_resc_ens5   <- readRDS(file.path(here::here(), "models", "1927", "nn2_backtest_res_rescaled_ens5.rds"))
      
      nn3_backtest             <- readRDS(file.path(here::here(), "models", "1927", "nn3_backtest_res.rds"))
      nn3_backtest_resc        <- readRDS(file.path(here::here(), "models", "1927", "nn3_backtest_res_rescaled.rds"))
      nn3_backtest_resc_ens5   <- readRDS(file.path(here::here(), "models", "1927", "nn3_backtest_res_rescaled_ens5.rds"))
      
      nn4_backtest             <- readRDS(file.path(here::here(), "models", "1927", "nn4_backtest_res.rds"))
      nn4_backtest_resc        <- readRDS(file.path(here::here(), "models", "1927", "nn4_backtest_res_rescaled.rds"))
      nn4_backtest_resc_ens5   <- readRDS(file.path(here::here(), "models", "1927", "nn4_backtest_res_rescaled_ens5.rds"))
      
      nn5_backtest             <- readRDS(file.path(here::here(), "models", "1927", "nn5_backtest_res.rds"))
      
        
      list_backtest_res_1927 <- list(
        ## Baselines
        har                  = har_backtest,
        har_resc             = har_backtest_resc,
        harx_resc            = harx_backtest_resc,
        
        glmnet               = glmnet_backtest,
        glmnet_resc          = glmnet_backtest_resc,
        
        rf                   = rf_backtest,
        rf_resc              = rf_backtest_resc,
        
        xgb                  = xgb_backtest,
        xgb_resc             = xgb_backtest_resc,
        
        ## Neural nets – NN1
        nn1                  = nn1_backtest,
        nn1_2                = nn1_backtest_2, 
        nn1_3                = nn1_backtest_3, 
        nn1_resc_1           = nn1_backtest_resc,
        nn1_resc_1_ens5      = nn1_backtest_resc_ens5,
        
        ## Neural nets – NN2
        nn2                  = nn2_backtest,
        nn2_resc_1           = nn2_backtest_resc,
        nn2_resc_1_ens5      = nn2_backtest_resc_ens5,
        
        ## Neural nets – NN3
        nn3                  = nn3_backtest,
        nn3_resc_1           = nn3_backtest_resc,
        nn3_resc_1_ens5      = nn3_backtest_resc_ens5,
        
        ## Neural nets – NN4
        nn4                  = nn4_backtest,
        nn4_resc_1           = nn4_backtest_resc,
        nn4_resc_1_ens5      = nn4_backtest_resc_ens5,
        
        ## Neural nets – NN5
        nn5                  = nn5_backtest
      )
      
      
      ##Ensemble
      compute_ensemble <- function(backtests, list_backtest, ref_backtest){
        ensemble <- purrr::imap(
          list_backtest[
            backtests
          ],
          function(backtest, name){
            
            df <- backtest@oos_outputs %>%
              dplyr::select(month_id, pred)
            
            ## Add target only once
            if (name == ref_backtest) {
              df <- backtest@oos_outputs %>%
                dplyr::select(month_id, target, pred)
            }
            
            dplyr::rename(df, !!paste0("pred_", name) := pred)
          }
        ) %>%
          purrr::reduce(dplyr::left_join, by = "month_id") %>%
          dplyr::mutate(
            ensemble_pred = base::rowMeans(
              dplyr::select(., dplyr::starts_with("pred_")),
              na.rm = TRUE
            )
          )
        
        ensemble
      }
     
      ensemble_all_1927 <- compute_ensemble(
        backtests = c("glmnet_resc", "rf_resc", "xgb_resc",
                      "nn1_resc_1_ens5", "nn2_resc_1_ens5",
                      "nn3_resc_1_ens5", "nn4_resc_1_ens5"),
        list_backtest = list_backtest_res_1927,
        ref_backtest = "glmnet_resc"
      )
      ensemble_nn_1927 <- compute_ensemble(
        backtests = c("nn1_resc_1_ens5", "nn2_resc_1_ens5",
                      "nn3_resc_1_ens5", "nn4_resc_1_ens5"),
        list_backtest = list_backtest_res_1927,
        ref_backtest = "nn1_resc_1_ens5"
      )
      ensemble_all_but_nn_1927 <- compute_ensemble(
        backtests = c("glmnet_resc", "rf_resc", "xgb_resc"),
        list_backtest = list_backtest_res_1927,
        ref_backtest = "glmnet_resc"
      )
    
        ### Calculate eval metrics and error
        ensemble_all_testing_metrics <- calc_eval_metrics(
          pred   = ensemble_all_1927$ensemble_pred,
          target = ensemble_all_1927$target,
          huber_delta = 1,
          quantile_tau = 0.5,
          eval_metric = "rmse",
          return_error = FALSE
        )
        ensemble_nn_testing_metrics <- calc_eval_metrics(
          pred   = ensemble_nn_1927$ensemble_pred,
          target = ensemble_nn_1927$target,
          huber_delta = 1,
          quantile_tau = 0.5,
          eval_metric = "rmse",
          return_error = FALSE
        )
        ensemble_all_but_nn_testing_metrics <- calc_eval_metrics(
          pred   = ensemble_all_but_nn_1927$ensemble_pred,
          target = ensemble_all_but_nn_1927$target,
          huber_delta = 1,
          quantile_tau = 0.5,
          eval_metric = "rmse",
          return_error = FALSE
        )
        ensemble_all_nn_ensemble_testing_metrics <- calc_eval_metrics(
          pred   = ensemble_all_but_nn_1927 %>% dplyr::left_join(
            ensemble_nn_1927 %>% dplyr::select(month_id, pred_ensemble_nn = ensemble_pred),
            by = "month_id"
          ) %>%
            dplyr::mutate(ensemble_pred = base::rowMeans(
              dplyr::select(., dplyr::starts_with("pred_")),
              na.rm = TRUE
            )) %>%
            dplyr::pull(ensemble_pred),
          target = ensemble_all_1927$target,
          huber_delta = 1,
          quantile_tau = 0.5,
          eval_metric = "rmse",
          return_error = FALSE
        )
        
      # Add ensembles to list_backtest_res_1927 
      list_backtest_res_1927$ensemble_all <- list(
        oos_outputs  = ensemble_all_1927,
        eval_metrics = ensemble_all_testing_metrics
      ) 
      list_backtest_res_1927$ensemble_nn <- list(
        oos_outputs  = ensemble_nn_1927,
        eval_metrics = ensemble_nn_testing_metrics
      )
      list_backtest_res_1927$ensemble_all_but_nn <- list(
        oos_outputs  = ensemble_all_but_nn_1927,
        eval_metrics = ensemble_all_but_nn_testing_metrics
      )
      
      # Save
      saveRDS(list_backtest_res_1927, here::here("models", "1927", "list_backtest_res_1927.rds"))
      

      #1986
      #Join tables
      har_backtest         <- readRDS(file.path(here::here(), "models", "1986", "har_backtest_res_1986.rds"))
      har_backtest_resc    <- readRDS(file.path(here::here(), "models", "1986", "har_backtest_res_1986_rescaled.rds"))
      harx_backtest        <- readRDS(file.path(here::here(), "models", "1986", "harx_backtest_res_1986.rds"))
      harx_backtest_resc   <- readRDS(file.path(here::here(), "models", "1986", "harx_backtest_res_1986_rescaled.rds"))
      
      glmnet_backtest      <- readRDS(file.path(here::here(), "models", "1986", "glmnet_backtest_res_1986.rds"))
      glmnet_backtest_resc <- readRDS(file.path(here::here(), "models", "1986", "glmnet_backtest_res_1986_rescaled.rds"))
      glmnet_backtest_grid <- readRDS(file.path(here::here(), "models", "1986", "glmnet_backtest_grid_res_1986.rds"))
      
      rf_backtest          <- readRDS(file.path(here::here(), "models", "1986", "rf_backtest_res_1986.rds"))
      rf_backtest_resc     <- readRDS(file.path(here::here(), "models", "1986", "rf_backtest_res_1986_rescaled.rds"))
      rf_backtest_grid     <- readRDS(file.path(here::here(), "models", "1986", "rf_backtest_grid_res_1986_rescaled.rds"))
      
      xgb_backtest         <- readRDS(file.path(here::here(), "models", "1986", "xgb_backtest_res_1986.rds"))
      xgb_backtest_resc    <- readRDS(file.path(here::here(), "models", "1986", "xgb_backtest_res_1986_rescaled.rds"))
      xgb_backtest_grid    <- readRDS(file.path(here::here(), "models", "1986", "xgb_backtest_grid_res_1986_rescaled.rds"))
      
      nn1_backtest        <- readRDS(file.path(here::here(), "models", "1986", "nn1_backtest_res_1986.rds"))
      nn1_backtest_2      <- readRDS(file.path(here::here(), "models", "1986", "nn1_backtest_res_1986_2.rds"))
      
      nn1_backtest_resc        <- readRDS(file.path(here::here(), "models", "1986", "nn1_backtest_res_1986_rescaled.rds"))
      nn1_backtest_resc2       <- readRDS(file.path(here::here(), "models", "1986", "nn1_backtest_res_1986_rescaled_2.rds"))
      nn1_backtest_resc3       <- readRDS(file.path(here::here(), "models", "1986", "nn1_backtest_res_1986_rescaled_3.rds"))
      nn1_backtest_resc4       <- readRDS(file.path(here::here(), "models", "1986", "nn1_backtest_res_1986_rescaled_4.rds"))
      nn1_backtest_resc5       <- readRDS(file.path(here::here(), "models", "1986", "nn1_backtest_res_1986_rescaled_5.rds"))
      nn1_backtest_resc6       <- readRDS(file.path(here::here(), "models", "1986", "nn1_backtest_res_1986_rescaled_6.rds"))
      nn1_backtest_resc4_ens5  <- readRDS(file.path(here::here(), "models", "1986", "nn1_backtest_res_1986_rescaled_4_ens5.rds"))
      nn1_backtest_grid_ens5   <- readRDS(file.path(here::here(), "models", "1986", "nn1_backtest_grid_res_1986_resc_ens5.rds"))
      
      nn2_backtest            <- readRDS(file.path(here::here(), "models", "1986", "nn2_backtest_res_1986.rds"))
      nn2_backtest_resc       <- readRDS(file.path(here::here(), "models", "1986", "nn2_backtest_res_1986_rescaled.rds"))
      nn2_backtest_resc2      <- readRDS(file.path(here::here(), "models", "1986", "nn2_backtest_res_1986_rescaled_2.rds"))
      nn2_backtest_resc4      <- readRDS(file.path(here::here(), "models", "1986", "nn2_backtest_res_1986_rescaled_4.rds"))
      nn2_backtest_resc4_ens5 <- readRDS(file.path(here::here(), "models", "1986", "nn2_backtest_res_1986_rescaled_4_ens5.rds"))
      nn2_backtest_grid_ens5   <- readRDS(file.path(here::here(), "models", "1986", "nn2_backtest_grid_res_1986_resc_ens5_3.rds"))
      
      nn3_backtest             <- readRDS(file.path(here::here(), "models", "1986", "nn3_backtest_res_1986.rds"))
      nn3_backtest_resc        <- readRDS(file.path(here::here(), "models", "1986", "nn3_backtest_res_1986_rescaled.rds"))
      nn3_backtest_resc2       <- readRDS(file.path(here::here(), "models", "1986", "nn3_backtest_res_1986_rescaled_2.rds"))
      nn3_backtest_resc4       <- readRDS(file.path(here::here(), "models", "1986", "nn3_backtest_res_1986_rescaled_4.rds"))
      nn3_backtest_resc4_ens5  <- readRDS(file.path(here::here(), "models", "1986", "nn3_backtest_res_1986_rescaled_4_ens5.rds"))
      nn3_backtest_grid_ens5   <- readRDS(file.path(here::here(), "models", "1986", "nn3_backtest_grid_res_1986_resc_ens5_2.rds"))
      
      
      nn4_backtest            <- readRDS(file.path(here::here(), "models", "1986", "nn4_backtest_res_1986.rds"))
      nn4_backtest_resc       <- readRDS(file.path(here::here(), "models", "1986", "nn4_backtest_res_1986_rescaled.rds"))
      nn4_backtest_resc2      <- readRDS(file.path(here::here(), "models", "1986", "nn4_backtest_res_1986_rescaled_2.rds"))
      nn4_backtest_resc4      <- readRDS(file.path(here::here(), "models", "1986", "nn4_backtest_res_1986_rescaled_4.rds"))
      nn4_backtest_resc4_ens5 <- readRDS(file.path(here::here(), "models", "1986", "nn4_backtest_res_1986_rescaled_4_ens5.rds"))
      nn4_backtest_grid_ens5   <- readRDS(file.path(here::here(), "models", "1986", "nn4_backtest_grid_res_1986_resc_ens5.rds"))
      
      
      
      nn5_backtest            <- readRDS(file.path(here::here(), "models", "1986", "nn5_backtest_res_1986.rds"))
      nn5_backtest_resc       <- readRDS(file.path(here::here(), "models", "1986", "nn5_backtest_res_1986_rescaled.rds"))
      nn5_backtest_resc2      <- readRDS(file.path(here::here(), "models", "1986", "nn5_backtest_res_1986_rescaled_2.rds"))
      nn5_backtest_resc4      <- readRDS(file.path(here::here(), "models", "1986", "nn5_backtest_res_1986_rescaled_4.rds"))
      nn5_backtest_resc4_ens5 <- readRDS(file.path(here::here(), "models", "1986", "nn5_backtest_res_1986_rescaled_4_ens5.rds"))
      nn5_backtest_grid_ens5   <- readRDS(file.path(here::here(), "models", "1986", "nn5_backtest_grid_res_1986_resc_ens5.rds"))
      
      
      lstm_backtest_res   <- readRDS(file.path(here::here(), "models",  "1986", "lstm_backtest_res_1986.rds"))
      lstm_backtest_resc4 <- readRDS(file.path(here::here(), "models",  "1986", "lstm_backtest_res_1986_rescaled_4.rds"))
      lstm_backtest_resc4_pad <- readRDS(file.path(here::here(), "models",  "1986", "lstm_backtest_res_1986_rescaled_4_padding.rds"))
      lstm_backtest_resc5 <- readRDS(file.path(here::here(), "models",  "1986", "lstm_backtest_res_1986_rescaled_5.rds"))
      
      list_backtest_res_1986 <- list(
        ## Baselines
        har                  = har_backtest,
        har_resc             = har_backtest_resc,
        harx                 = harx_backtest,
        harx_resc            = harx_backtest_resc,
        
        glmnet               = glmnet_backtest,
        glmnet_resc          = glmnet_backtest_resc,
        glmnet_resc_grid     = glmnet_backtest_grid,
        
        rf                   = rf_backtest,
        rf_resc              = rf_backtest_resc,
        rf_resc_grid         = rf_backtest_grid,
        
        xgb                  = xgb_backtest,
        xgb_resc             = xgb_backtest_resc,
        xgb_resc_grid        = xgb_backtest_grid,
        
        ## Neural nets – NN1
        nn1                  = nn1_backtest,
        #nn1_v2               = nn1_backtest_2, #This had a bug
        nn1_resc_1           = nn1_backtest_resc,
        nn1_resc_2           = nn1_backtest_resc2,
        nn1_resc_3           = nn1_backtest_resc3,
        nn1_resc_4           = nn1_backtest_resc4,
        nn1_resc_5           = nn1_backtest_resc5,
        nn1_resc_6           = nn1_backtest_resc6,
        nn1_resc_4_ens5      = nn1_backtest_resc4_ens5,
        nn1_resc_grid        = nn1_backtest_grid_ens5,
        
        ## Neural nets – NN2
        nn2                  = nn2_backtest,
        nn2_resc_1           = nn2_backtest_resc,
        nn2_resc_2           = nn2_backtest_resc2,
        nn2_resc_4           = nn2_backtest_resc4,
        nn2_resc_4_ens5      = nn2_backtest_resc4_ens5,
        nn2_resc_grid        = nn2_backtest_grid_ens5,
        
        ## Neural nets – NN3
        nn3                  = nn3_backtest,
        nn3_resc_1           = nn3_backtest_resc,
        nn3_resc_2           = nn3_backtest_resc2,
        nn3_resc_4           = nn3_backtest_resc4,
        nn3_resc_4_ens5      = nn3_backtest_resc4_ens5,
        nn3_resc_grid        = nn3_backtest_grid_ens5,
        
        ## Neural nets – NN4
        nn4                  = nn4_backtest,
        nn4_resc_1           = nn4_backtest_resc,
        nn4_resc_2           = nn4_backtest_resc2,
        nn4_resc_4           = nn4_backtest_resc4,
        nn4_resc_4_ens5      = nn4_backtest_resc4_ens5,
        nn4_resc_grid        = nn4_backtest_grid_ens5,
        
        ## Neural nets – NN5
        nn5                  = nn5_backtest,
        nn5_resc_1           = nn5_backtest_resc,
        nn5_resc_2           = nn5_backtest_resc2,
        nn5_resc_4           = nn5_backtest_resc4,
        nn5_resc_4_ens5      = nn5_backtest_resc4_ens5,
        nn5_resc_grid        = nn5_backtest_grid_ens5,
        
        ## LSTM
        lstm                 = lstm_backtest_res,
        lstm_resc_4          = lstm_backtest_resc4,
        lstm_resc5           = lstm_backtest_resc5
      )
      
      
      ensemble_all_1986 <- compute_ensemble(
        backtests = c("glmnet_resc", "rf_resc", "xgb_resc",
                      "nn1_resc_4_ens5", "nn2_resc_4_ens5",
                      "nn3_resc_4_ens5", "nn4_resc_4_ens5",
                      "nn5_resc_4_ens5"),
        list_backtest = list_backtest_res_1986,
        ref_backtest = "glmnet_resc"
      )
      ensemble_nn_1986 <- compute_ensemble(
        backtests = c("nn1_resc_4_ens5", "nn2_resc_4_ens5",
                      "nn3_resc_4_ens5", "nn4_resc_4_ens5",
                      "nn5_resc_4_ens5"
                      ),
        list_backtest = list_backtest_res_1986,
        ref_backtest = "nn1_resc_4_ens5"
      )
      ensemble_all_but_nn_1986 <- compute_ensemble(
        backtests = c("glmnet_resc", "rf_resc", "xgb_resc"),
        list_backtest = list_backtest_res_1986,
        ref_backtest = "glmnet_resc"
      )
      
      ### Calculate eval metrics and error
      ensemble_all_testing_metrics <- calc_eval_metrics(
        pred   = ensemble_all_1986$ensemble_pred,
        target = ensemble_all_1986$target,
        huber_delta = 1,
        quantile_tau = 0.5,
        eval_metric = "rmse",
        return_error = FALSE
      )
      ensemble_nn_testing_metrics <- calc_eval_metrics(
        pred   = ensemble_nn_1986$ensemble_pred,
        target = ensemble_nn_1986$target,
        huber_delta = 1,
        quantile_tau = 0.5,
        eval_metric = "rmse",
        return_error = FALSE
      )
      ensemble_all_but_nn_testing_metrics <- calc_eval_metrics(
        pred   = ensemble_all_but_nn_1986$ensemble_pred,
        target = ensemble_all_but_nn_1986$target,
        huber_delta = 1,
        quantile_tau = 0.5,
        eval_metric = "rmse",
        return_error = FALSE
      )
      ensemble_all_nn_ensemble_testing_metrics <- calc_eval_metrics(
        pred   = ensemble_all_but_nn_1986 %>% dplyr::left_join(
          ensemble_nn_1986 %>% dplyr::select(month_id, pred_ensemble_nn = ensemble_pred),
          by = "month_id"
        ) %>%
          dplyr::mutate(ensemble_pred = base::rowMeans(
            dplyr::select(., dplyr::starts_with("pred_")),
            na.rm = TRUE
          )) %>%
          dplyr::pull(ensemble_pred),
        target = ensemble_all_but_nn_1986$target,
        huber_delta = 1,
        quantile_tau = 0.5,
        eval_metric = "rmse",
        return_error = FALSE
      )
      
      # Add ensembles to list_backtest_res_1986 
      list_backtest_res_1986$ensemble_all <- list(
        oos_outputs  = ensemble_all_1986,
        eval_metrics = ensemble_all_testing_metrics
      ) 
      list_backtest_res_1986$ensemble_nn <- list(
        oos_outputs  = ensemble_nn_1986,
        eval_metrics = ensemble_nn_testing_metrics
      )
      list_backtest_res_1986$ensemble_all_but_nn <- list(
        oos_outputs  = ensemble_all_but_nn_1986,
        eval_metrics = ensemble_all_but_nn_testing_metrics
      )
      
      # Save
      saveRDS(list_backtest_res_1986, here::here("models", "1986", "list_backtest_res_1986.rds"))
      
 
# Compile into tables----------------------------------------------------------           
      ##Function to compile results
      compile_results <- function(list_backtest_res){
        results_summary <- purrr::map(seq_along(list_backtest_res), function(table){
          
          list_backtest_res[[table]]@eval_metrics %>%
            dplyr::select(metric, cons_oos) %>%
            ### Rename
            dplyr::rename(!!names(list_backtest_res)[table] := cons_oos) %>%
            dplyr::filter(metric %in% c("rss", "rmse", "mae", "mphe", "mpe", "mape"))
          
        }) %>% 
          purrr::reduce(dplyr::left_join, by = "metric") %>%
          tidyr::pivot_longer(
            cols = -metric,
            names_to = "model",
            values_to = "value"
          )
        
        ### Add hyperparameters
        results_summary_complete <- purrr::map(seq_along(list_backtest_res), function(table){
          
          hyper_grid <- list_backtest_res[[table]]@backtest_meta$hyper_grid_domain_list
          
          search_type <- if (is.null(hyper_grid) || length(hyper_grid) == 0) {
            "no_tuning"
          } else {
            lengths_vec <- purrr::map_int(hyper_grid, length)
            if (max(lengths_vec) > 2) "grid_search" else "bayesian_optimization"
          }
         
          hyperparams <- list_backtest_res[[table]]@backtest_meta$hyper_grid_domain_list %>%
            purrr::map_chr(function(x){
              
              if(length(x) == 2){
                paste0(min(x), " - ", max(x))
              } else {
                paste0(x, collapse = ", ")
              }
              
            }) %>%
            as.list() %>%
            as.data.frame(stringsAsFactors = FALSE)
          
          
          hyperparams <- c(
            hyperparams,
            early_stop = list_backtest_res[[table]]@backtest_meta$early_stop,
            val_n = list_backtest_res[[table]]@backtest_meta$val_n,
            train_n = list_backtest_res[[table]]@backtest_meta$train_n,
            n_ensembles = if (is.null(list_backtest_res[[table]]@backtest_meta$n_ensembles)){
              0
            } else list_backtest_res[[table]]@backtest_meta$n_ensembles,
            search_type = search_type
          ) %>%
            as.data.frame()
          
          ##Add
          results_summary %>%
            dplyr::filter(model == names(list_backtest_res)[table]) %>%
            dplyr::cross_join(hyperparams)
          
        }) %>%
          purrr::reduce(dplyr::bind_rows) %>%
          dplyr::mutate(
            preprocessing = dplyr::case_when(
              grepl("resc", model) ~ "minmax_scaling",
              TRUE                 ~ "zscore_standardization"
            ),
            .after = model
          ) %>%
          dplyr::mutate(
            model = stringr::str_remove(model, "_resc.*$")
          ) 
        
      }

      results_summary_complete_1927 <- compile_results(list_backtest_res_1927)
      results_summary_complete_1927 %>% write.csv("results_summary_1927.csv", row.names = FALSE)
      
      results_summary_complete_1986 <- compile_results(list_backtest_res_1986)
      
      ### Add ensembles
      results_summary_complete_1986 <- results_summary_complete_1986 %>%
        dplyr::bind_rows(
          data.frame(
            model = "ensemble_all",
            metric = names(ensemble_all_testing_metrics)[-1],
            value  = as.numeric(unname(ensemble_all_testing_metrics)[-1])
        )
      ) %>%
        dplyr::bind_rows(
          data.frame(
            model = "ensemble_nn",
            metric = names(ensemble_nn_testing_metrics)[-1],
            value  = as.numeric(unname(ensemble_nn_testing_metrics)[-1])
          )
        ) %>%
        dplyr::bind_rows(
          data.frame(
            model = "ensemble_all_but_nn",
            metric = names(ensemble_all_nn_ensemble_testing_metrics)[-1],
            value  = as.numeric(unname(ensemble_all_nn_ensemble_testing_metrics)[-1])
          )
        )
      
      results_summary_complete_1986 %>% write.csv("results_summary_1986.csv", row.names = FALSE)
        
      library("ggplot2")
      results_summary_complete_1986 %>%
        dplyr::filter(
          metric == "rmse",
          search_type %in% c("grid_search", "bayesian_optimization")
        ) %>%
        ggplot(aes(x = model, y = value, fill = search_type)) +
        geom_boxplot(alpha = 0.7) +
        theme_minimal() +
        labs(
          title = "Grid Search vs Bayesian Optimization (RMSE)",
          x = "Algorithm",
          y = "RMSE",
          fill = "Search type"
        )
      
      
      
## Feat Imp-----------------------------------------------------------------
      #Join all with purrr::reduce
      theme_map <- data.frame(
        feat = c("(Intercept)",
                 "rv_month_lag1", "rv_tw_lag1", "rv_td_lag1",
                 "rq_month_lag1", "rq_tw_lag1", "rq_td_lag1",
                 "cont_month_lag1", "cont_tw_lag1", "cont_td_lag1",
                 "jump_month_lag1", "jump_tw_lag1", "jump_td_lag1",
                 "negret_month_lag1", "negret_tw_lag1", "negret_td_lag1",
                 "log_epu_diff_lag1", "gpr_lag1", "log_cons_sent_diff_lag1", 
                 "ppi_vol", "ip_vol", "mbas_vol",
                 "dy_lag1", "dfr_lag1", "dfy_lag1", "ey_lag1", "tbill_lag1",
                 "tms_lag1", "oil_rv_month_lag1", "oil_rv_td_lag1", "oil_rv_tw_lag1", 
                 "vix_lag1",
                 "m_1", "m_2", "m_3", "m_4", "m_5", "m_6", "m_7", "m_8",
                 "m_9", "m_10", "m_11", "m_12",
                 "sqrt_days"
        ),
        theme = c(
          "(Intercept)",
          rep("Past S&P 500 RV Terms", 15),
          rep("Sentiment Variables", 3),
          rep("Macroeconomic Variables", 3),
          rep("Macrofinancial Variables", 9),
          rep("Implied Vol", 1),
          rep("Calendar Variables", 13)
        )
      )
      
      plot(glmnet_backtest_resc, theme_map = theme_map, plot_id = 7)
      plot(rf_backtest_res,  theme_map = theme_map, plot_id = 7)
      plot(xgb_backtest_res, theme_map = theme_map, plot_id = 7)
      plot(nn1_backtest_res, theme_map = theme_map, plot_id = 7)
      plot(nn2_backtest_res, theme_map = theme_map, plot_id = 7)
      plot(nn3_backtest_grid_ens5, theme_map = theme_map, plot_id = 7)
      plot(nn4_backtest_res, theme_map = theme_map, plot_id = 7)
      plot(nn5_backtest_res, theme_map = theme_map, plot_id = 7)
      
      
      
      
      
      
      