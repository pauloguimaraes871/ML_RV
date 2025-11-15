# Load packages and datasets-----------------------------------------------------

  ## Packages
  renv::install("gt")
  renv::install("slider")
  renv::install("xts")

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
        prep_covariates <- function(df) {
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
              ~ winsorize_expanding(.x, p_lo = 0.01, p_hi = 0.99, min_n = 36L)
              )
            ) %>%
          ### Scale
          dplyr::mutate(
            dplyr::across(
              dplyr::where(is.numeric) &
                !dplyr::any_of("sqrt_days") & !dplyr::starts_with("m_"),
              ~ scale_expanding(.x, min_n = 36L, eps = sqrt(.Machine$double.eps))
              )
          )
      }
  
        ### Apply    
        covariates_1927_df_gold <- prep_covariates(covariates_1927_df)
        covariates_1962_df_gold <- prep_covariates(covariates_1962_df)
        covariates_1986_df_gold <- prep_covariates(covariates_1986_df)
        
  ## Target     
  target_gold <- target %>% 
    dplyr::arrange(month_id) %>%
    dplyr::mutate(
      rv_month = boxcox_apply(rv_month, 0, 1e-08)
    )
    
          
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
  
    ### HAR
    har_backtest_res <- run_walk_forward_validation(
      target = target_gold, 
      covariates = covariates_1927_df_gold %>%
        dplyr::select(month_id, dplyr::contains("rv_")),
      model = "har",
      hyper_grid_domain_list = list(),
      obj_fun = "squared_error", eval_metric = "rmse",
      huber_delta = 1, quantile_tau = 0.5,
      train_n = 180L, val_n = 0L,
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
    
      #### Save RDS
      saveRDS(har_backtest_res,    "har_backtest_res.rds")
      
    ### GLMNET
    future::plan("multisession")
    doFuture::registerDoFuture()
    
    glmnet_backtest_res <- run_walk_forward_validation(
      target = target_gold, 
      covariates = covariates_1927_df_gold,
      model = "glmnet",
      hyper_grid_domain_list = list(
        alpha = c(0, 1),
        lambda.min.ratio = c(1e-4, 1e-2)
      ),
      obj_fun = "squared_error", eval_metric = "rmse",
      huber_delta = 1, quantile_tau = 0.5,
      train_n = 120L, val_n = 60L,
      rebal_months = c(6),
      early_stop = NULL,
      gsm_algo = "ols",
      upper_quant_wins = 0.95,
      lower_quant_wins = 0.05,
      n_iter = 10L, init_points = 5L,
      k_iter = 2L, acq = "ucb",
      keras_architecture_pars = NULL,
      parallel = FALSE,
      verbose = TRUE,
      .test_seed = 123
    )
    
      #### Save RDS
      saveRDS(glmnet_backtest_res, "glmnet_backtest_res.rds")
      
      ### RF
      future::plan("multisession")
      doFuture::registerDoFuture()
      
      rf_backtest_res <- run_walk_forward_validation(
        target = target_gold, 
        covariates = covariates_1927_df_gold,
        model = "rf",
        hyper_grid_domain_list = list(
          mtry = c(0.1, 0.9),
          num.trees = c(300L, 1500L),
          max.depth = c(3L, 16L),
          min.bucket = c(1L, 6L)
        ),
        obj_fun = "squared_error", eval_metric = "rmse",
        huber_delta = 1, quantile_tau = 0.5,
        train_n = 120L, val_n = 60L,
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
      
      #### Save RDS
      saveRDS(rf_backtest_res, "rf_backtest_res.rds")
    
      
      ### XGB
      future::plan("multisession")
      doFuture::registerDoFuture()
      
      xgb_backtest_res <- run_walk_forward_validation(
        target = target_gold, 
        covariates = covariates_1927_df_gold,
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
        train_n = 120L, val_n = 60L,
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
      
      #### Save RDS
      saveRDS(xgb_backtest_res, "xgb_backtest_res.rds")
    
    
      ### NN1
      future::plan("sequential")
      
      nn1_backtest_res <- run_walk_forward_validation(
        target = target_gold, 
        covariates = covariates_1927_df_gold,
        model = "nn",
        keras_architecture_pars = list(
          units             = 32,
          n_layers          = 1,
          activation        = "relu",
          nn_optimizer      = "Adam",
          batch_norm_option = TRUE
        ),
        hyper_grid_domain_list = list(
          regularizer_l1   = c(0, 1e-6),      
          regularizer_l2   = c(2e-4, 2e-3),  
          droprate         = c(0.10, 0.25),
          lr               = c(8e-4, 2e-3),
          size_of_batch    = c(32L, 32L),
          number_of_epochs = c(50L, 150L)  
        ),
        obj_fun = "squared_error", eval_metric = "rmse",
        huber_delta = 1, quantile_tau = 0.5,
        train_n = 120L, val_n = 60L,
        rebal_months = c(6),
        early_stop = 20L,
        gsm_algo = "ols",
        upper_quant_wins = 0.95,
        lower_quant_wins = 0.05,
        n_iter = 16L, init_points = 12L,
        k_iter = 2L, acq = "ucb",
        parallel = FALSE,
        verbose = TRUE,
        .test_seed = 123
      )
      
      #### Save RDS
      saveRDS(nn1_backtest_res, "nn1_backtest_res_3.rds")
      
      ### NN2
      future::plan("sequential")
      
      nn2_backtest_res <- run_walk_forward_validation(
        target = target_gold, 
        covariates = covariates_1927_df_gold,
        model = "nn",
        keras_architecture_pars = list(
          units             = c(32, 16),
          n_layers          = 2,
          activation        = rep("relu", 2),
          nn_optimizer      = "Adam",
          batch_norm_option = rep(TRUE,2)
        ),
        hyper_grid_domain_list = list(
          regularizer_l1   = c(0, 1e-6),      
          regularizer_l2   = c(2e-4, 2e-3),  
          droprate         = c(0.10, 0.25),
          lr               = c(8e-4, 2e-3),
          size_of_batch    = c(32L, 32L),
          number_of_epochs = c(50L, 150L)  
        ),
        obj_fun = "squared_error", eval_metric = "rmse",
        huber_delta = 1, quantile_tau = 0.5,
        train_n = 120L, val_n = 60L,
        rebal_months = c(6),
        early_stop = 20L,
        gsm_algo = "ols",
        upper_quant_wins = 0.95,
        lower_quant_wins = 0.05,
        n_iter = 16L, init_points = 12L,
        k_iter = 2L, acq = "ucb",
        parallel = FALSE,
        verbose = TRUE,
        .test_seed = 123
      )
      
      #### Save RDS
      saveRDS(nn2_backtest_res, "nn2_backtest_res.rds") 
    
      
      ### NN3
      future::plan("sequential")
      
      nn3_backtest_res <- run_walk_forward_validation(
        target = target_gold, 
        covariates = covariates_1927_df_gold,
        model = "nn",
        keras_architecture_pars = list(
          units             = c(32, 16, 8),
          n_layers          = 3,
          activation        = rep("relu", 3),
          nn_optimizer      = "Adam",
          batch_norm_option = rep(TRUE,3)
        ),
        hyper_grid_domain_list = list(
          regularizer_l1   = c(0, 1e-6),      
          regularizer_l2   = c(2e-4, 2e-3),  
          droprate         = c(0.10, 0.25),
          lr               = c(8e-4, 2e-3),
          size_of_batch    = c(32L, 32L),
          number_of_epochs = c(50L, 150L)  
        ),
        obj_fun = "squared_error", eval_metric = "rmse",
        huber_delta = 1, quantile_tau = 0.5,
        train_n = 120L, val_n = 60L,
        rebal_months = c(6),
        early_stop = 20L,
        gsm_algo = "ols",
        upper_quant_wins = 0.95,
        lower_quant_wins = 0.05,
        n_iter = 16L, init_points = 12L,
        k_iter = 2L, acq = "ucb",
        parallel = FALSE,
        verbose = TRUE,
        .test_seed = 123
      )
      
      #### Save RDS
      saveRDS(nn3_backtest_res, "nn3_backtest_res.rds") 
      
      ### NN4
      future::plan("sequential")
      
      nn4_backtest_res <- run_walk_forward_validation(
        target = target_gold, 
        covariates = covariates_1927_df_gold,
        model = "nn",
        keras_architecture_pars = list(
          units             = c(32, 16, 8, 4),
          n_layers          = 4,
          activation        = rep("relu", 4),
          nn_optimizer      = "Adam",
          batch_norm_option = rep(TRUE,4)
        ),
        hyper_grid_domain_list = list(
          regularizer_l1   = c(0, 1e-6),      
          regularizer_l2   = c(2e-4, 2e-3),  
          droprate         = c(0.10, 0.25),
          lr               = c(8e-4, 2e-3),
          size_of_batch    = c(32L, 32L),
          number_of_epochs = c(50L, 150L)  
        ),
        obj_fun = "squared_error", eval_metric = "rmse",
        huber_delta = 1, quantile_tau = 0.5,
        train_n = 120L, val_n = 60L,
        rebal_months = c(6),
        early_stop = 20L,
        gsm_algo = "ols",
        upper_quant_wins = 0.95,
        lower_quant_wins = 0.05,
        n_iter = 16L, init_points = 12L,
        k_iter = 2L, acq = "ucb",
        parallel = FALSE,
        verbose = TRUE,
        .test_seed = 123
      )
      
      #### Save RDS
      saveRDS(nn4_backtest_res, "nn4_backtest_res.rds") 
      
      ### NN5
      future::plan("sequential")
      
      nn5_backtest_res <- run_walk_forward_validation(
        target = target_gold, 
        covariates = covariates_1927_df_gold,
        model = "nn",
        keras_architecture_pars = list(
          units             = c(32, 16, 8, 4, 2),
          n_layers          = 5,
          activation        = rep("relu", 5),
          nn_optimizer      = "Adam",
          batch_norm_option = rep(TRUE,5)
        ),
        hyper_grid_domain_list = list(
          regularizer_l1   = c(0, 1e-6),      
          regularizer_l2   = c(2e-4, 2e-3),  
          droprate         = c(0.10, 0.25),
          lr               = c(8e-4, 2e-3),
          size_of_batch    = c(32L, 32L),
          number_of_epochs = c(50L, 150L)  
        ),
        obj_fun = "squared_error", eval_metric = "rmse",
        huber_delta = 1, quantile_tau = 0.5,
        train_n = 120L, val_n = 60L,
        rebal_months = c(6),
        early_stop = 20L,
        gsm_algo = "ols",
        upper_quant_wins = 0.95,
        lower_quant_wins = 0.05,
        n_iter = 16L, init_points = 12L,
        k_iter = 2L, acq = "ucb",
        parallel = FALSE,
        verbose = TRUE,
        .test_seed = 123
      )
      
      #### Save RDS
      saveRDS(nn5_backtest_res, "nn5_backtest_res.rds") 
      
      #Join tables
      
      har_backtest_res <- readRDS(file.path(here::here(), "models", "har_backtest_res.rds"))
      glmnet_backtest_res <- readRDS(file.path(here::here(), "models", "glmnet_backtest_res.rds"))
      rf_backtest_res    <- readRDS(file.path(here::here(), "models", "rf_backtest_res.rds"))
      xgb_backtest_res   <- readRDS(file.path(here::here(), "models", "xgb_backtest_res.rds"))
      nn1_backtest_res <- readRDS(file.path(here::here(), "models", "nn1_backtest_res.rds"))
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
      
      ### HAR
      har_backtest_res_1986 <- run_walk_forward_validation(
        target = target_gold, 
        covariates = covariates_1986_df_gold %>%
          dplyr::select(month_id, dplyr::contains("rv_")),
        model = "har",
        hyper_grid_domain_list = list(),
        obj_fun = "squared_error", eval_metric = "rmse",
        huber_delta = 1, quantile_tau = 0.5,
        train_n = 180L, val_n = 0L,
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
      
      #### Save RDS
      saveRDS(har_backtest_res_1986,    "har_backtest_res_1986.rds")
      
      ### GLMNET
      future::plan("multisession")
      doFuture::registerDoFuture()
      
      glmnet_backtest_res_1986 <- run_walk_forward_validation(
        target = target_gold, 
        covariates = covariates_1986_df_gold,
        model = "glmnet",
        hyper_grid_domain_list = list(
          alpha = c(0, 1),
          lambda.min.ratio = c(1e-4, 1e-2)
        ),
        obj_fun = "squared_error", eval_metric = "rmse",
        huber_delta = 1, quantile_tau = 0.5,
        train_n = 120L, val_n = 60L,
        rebal_months = c(6),
        early_stop = NULL,
        gsm_algo = "ols",
        upper_quant_wins = 0.95,
        lower_quant_wins = 0.05,
        n_iter = 10L, init_points = 5L,
        k_iter = 2L, acq = "ucb",
        keras_architecture_pars = NULL,
        parallel = FALSE,
        verbose = TRUE,
        .test_seed = 123
      )
      
      #### Save RDS
      saveRDS(glmnet_backtest_res_1986, "glmnet_backtest_res_1986.rds")
      
      ### RF
      future::plan("multisession")
      doFuture::registerDoFuture()
      
      rf_backtest_res_1986 <- run_walk_forward_validation(
        target = target_gold, 
        covariates = covariates_1986_df_gold,
        model = "rf",
        hyper_grid_domain_list = list(
          mtry = c(0.1, 0.9),
          num.trees = c(300L, 1500L),
          max.depth = c(3L, 16L),
          min.bucket = c(1L, 6L)
        ),
        obj_fun = "squared_error", eval_metric = "rmse",
        huber_delta = 1, quantile_tau = 0.5,
        train_n = 120L, val_n = 60L,
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
      
      #### Save RDS
      saveRDS(rf_backtest_res_1986, "rf_backtest_res_1986.rds")
      
      
      ### XGB
      future::plan("multisession")
      doFuture::registerDoFuture()
      
      xgb_backtest_res_1986 <- run_walk_forward_validation(
        target = target_gold, 
        covariates = covariates_1986_df_gold,
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
        train_n = 120L, val_n = 60L,
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
      
      #### Save RDS
      saveRDS(xgb_backtest_res_1986, "xgb_backtest_res_1986.rds")
      
      
      ### NN1
      future::plan("sequential")
      
      nn1_backtest_res_1986 <- run_walk_forward_validation(
        target = target_gold, 
        covariates = covariates_1986_df_gold,
        model = "nn",
        keras_architecture_pars = list(
          units             = 32,
          n_layers          = 1,
          activation        = "relu",
          nn_optimizer      = "Adam",
          batch_norm_option = FALSE
        ),
        hyper_grid_domain_list = list(
          regularizer_l1   = c(0, 1e-5),      
          regularizer_l2   = c(1e-3, 5e-2),  
          droprate         = c(0.20, 0.50),
          lr               = c(5e-4, 2e-3),
          size_of_batch    = c(32L, 64L),
          number_of_epochs = c(40L, 120L)  
        ),
        obj_fun = "squared_error", eval_metric = "rmse",
        huber_delta = 1, quantile_tau = 0.5,
        train_n = 120L, val_n = 60L,
        rebal_months = c(6),
        early_stop = 20L,
        gsm_algo = "ols",
        upper_quant_wins = 0.95,
        lower_quant_wins = 0.05,
        n_iter = 16L, init_points = 12L,
        k_iter = 2L, acq = "ucb",
        parallel = FALSE,
        verbose = TRUE,
        .test_seed = 123
      )
      
      #### Save RDS
      saveRDS(nn1_backtest_res_1986, "nn1_backtest_res_1986_2.rds")
      
      ### NN2
      future::plan("sequential")
      
      nn2_backtest_res_1986 <- run_walk_forward_validation(
        target = target_gold, 
        covariates = covariates_1986_df_gold,
        model = "nn",
        keras_architecture_pars = list(
          units             = c(32, 16),
          n_layers          = 2,
          activation        = rep("relu", 2),
          nn_optimizer      = "Adam",
          batch_norm_option = rep(TRUE,2)
        ),
        hyper_grid_domain_list = list(
          regularizer_l1   = c(0, 1e-6),      
          regularizer_l2   = c(2e-4, 2e-3),  
          droprate         = c(0.10, 0.25),
          lr               = c(8e-4, 2e-3),
          size_of_batch    = c(32L, 32L),
          number_of_epochs = c(50L, 150L)  
        ),
        obj_fun = "squared_error", eval_metric = "rmse",
        huber_delta = 1, quantile_tau = 0.5,
        train_n = 120L, val_n = 60L,
        rebal_months = c(6),
        early_stop = 20L,
        gsm_algo = "ols",
        upper_quant_wins = 0.95,
        lower_quant_wins = 0.05,
        n_iter = 16L, init_points = 12L,
        k_iter = 2L, acq = "ucb",
        parallel = FALSE,
        verbose = TRUE,
        .test_seed = 123
      )
      
      #### Save RDS
      saveRDS(nn2_backtest_res_1986, "nn2_backtest_res_1986_2.rds") 
      
      
      ### NN3
      future::plan("sequential")
      
      nn3_backtest_res_1986 <- run_walk_forward_validation(
        target = target_gold, 
        covariates = covariates_1986_df_gold,
        model = "nn",
        keras_architecture_pars = list(
          units             = c(32, 16, 8),
          n_layers          = 3,
          activation        = rep("relu", 3),
          nn_optimizer      = "Adam",
          batch_norm_option = rep(TRUE,3)
        ),
        hyper_grid_domain_list = list(
          regularizer_l1   = c(0, 1e-6),      
          regularizer_l2   = c(2e-4, 2e-3),  
          droprate         = c(0.10, 0.25),
          lr               = c(8e-4, 2e-3),
          size_of_batch    = c(32L, 32L),
          number_of_epochs = c(50L, 150L)  
        ),
        obj_fun = "squared_error", eval_metric = "rmse",
        huber_delta = 1, quantile_tau = 0.5,
        train_n = 120L, val_n = 60L,
        rebal_months = c(6),
        early_stop = 20L,
        gsm_algo = "ols",
        upper_quant_wins = 0.95,
        lower_quant_wins = 0.05,
        n_iter = 16L, init_points = 12L,
        k_iter = 2L, acq = "ucb",
        parallel = FALSE,
        verbose = TRUE,
        .test_seed = 123
      )
      
      #### Save RDS
      saveRDS(nn3_backtest_res_1986, "nn3_backtest_res_1986_2.rds") 
      
      ### NN4
      future::plan("sequential")
      
      nn4_backtest_res_1986 <- run_walk_forward_validation(
        target = target_gold, 
        covariates = covariates_1986_df_gold,
        model = "nn",
        keras_architecture_pars = list(
          units             = c(32, 16, 8, 4),
          n_layers          = 4,
          activation        = rep("relu", 4),
          nn_optimizer      = "Adam",
          batch_norm_option = rep(TRUE,4)
        ),
        hyper_grid_domain_list = list(
          regularizer_l1   = c(0, 1e-6),      
          regularizer_l2   = c(2e-4, 2e-3),  
          droprate         = c(0.10, 0.25),
          lr               = c(8e-4, 2e-3),
          size_of_batch    = c(32L, 32L),
          number_of_epochs = c(50L, 150L)  
        ),
        obj_fun = "squared_error", eval_metric = "rmse",
        huber_delta = 1, quantile_tau = 0.5,
        train_n = 120L, val_n = 60L,
        rebal_months = c(6),
        early_stop = 20L,
        gsm_algo = "ols",
        upper_quant_wins = 0.95,
        lower_quant_wins = 0.05,
        n_iter = 16L, init_points = 12L,
        k_iter = 2L, acq = "ucb",
        parallel = FALSE,
        verbose = TRUE,
        .test_seed = 123
      )
      
      #### Save RDS
      saveRDS(nn4_backtest_res_1986, "nn4_backtest_res_1986_2.rds") 
      
      ### NN5
      future::plan("sequential")
      
      nn5_backtest_res_1986 <- run_walk_forward_validation(
        target = target_gold, 
        covariates = covariates_1986_df_gold,
        model = "nn",
        keras_architecture_pars = list(
          units             = c(32, 16, 8, 4, 2),
          n_layers          = 5,
          activation        = rep("relu", 5),
          nn_optimizer      = "Adam",
          batch_norm_option = rep(TRUE,5)
        ),
        hyper_grid_domain_list = list(
          regularizer_l1   = c(0, 1e-6),      
          regularizer_l2   = c(2e-4, 2e-3),  
          droprate         = c(0.10, 0.25),
          lr               = c(8e-4, 2e-3),
          size_of_batch    = c(32L, 32L),
          number_of_epochs = c(50L, 150L)  
        ),
        obj_fun = "squared_error", eval_metric = "rmse",
        huber_delta = 1, quantile_tau = 0.5,
        train_n = 120L, val_n = 60L,
        rebal_months = c(6),
        early_stop = 20L,
        gsm_algo = "ols",
        upper_quant_wins = 0.95,
        lower_quant_wins = 0.05,
        n_iter = 16L, init_points = 12L,
        k_iter = 2L, acq = "ucb",
        parallel = FALSE,
        verbose = TRUE,
        .test_seed = 123
      )
      
      #### Save RDS
      saveRDS(nn5_backtest_res_1986, "nn5_backtest_res_1986_2.rds") 
      
      #Join tables
      har_backtest_res    <- readRDS(file.path(here::here(), "models", "har_backtest_res_1986.rds"))
      glmnet_backtest_res <- readRDS(file.path(here::here(), "models", "glmnet_backtest_res_1986.rds"))
      rf_backtest_res     <- readRDS(file.path(here::here(), "models", "rf_backtest_res_1986.rds"))
      xgb_backtest_res    <- readRDS(file.path(here::here(), "models", "xgb_backtest_res_1986.rds"))
      nn1_backtest_res    <- readRDS(file.path(here::here(), "models", "nn1_backtest_res_1986.rds"))
      nn1_backtest_res    <- readRDS(file.path(here::here(), "models", "nn1_backtest_res_1986.rds"))
      nn2_backtest_res    <- readRDS(file.path(here::here(), "models", "nn2_backtest_res_1986.rds"))
      nn3_backtest_res    <- readRDS(file.path(here::here(), "models", "nn3_backtest_res_1986.rds"))
      nn4_backtest_res    <- readRDS(file.path(here::here(), "models", "nn4_backtest_res_1986.rds"))
      nn5_backtest_res    <- readRDS(file.path(here::here(), "models", "nn5_backtest_res_1986.rds"))
      
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
      
      list_backtest_res <- list(har_backtest_res, glmnet_backtest_res, rf_backtest_res,
                                xgb_backtest_res, nn1_backtest_res, nn2_backtest_res,
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
      
      plot(glmnet_backtest_res, theme_map = theme_map, plot_id = 7)
      plot(rf_backtest_res,  theme_map = theme_map, plot_id = 7)
      plot(xgb_backtest_res, theme_map = theme_map, plot_id = 7)
      plot(nn1_backtest_res, theme_map = theme_map, plot_id = 7)
      plot(nn2_backtest_res, theme_map = theme_map, plot_id = 7)
      plot(nn3_backtest_res, theme_map = theme_map, plot_id = 7)
      plot(nn4_backtest_res, theme_map = theme_map, plot_id = 7)
      plot(nn5_backtest_res, theme_map = theme_map, plot_id = 7)
      
      
      
      
      
      
      