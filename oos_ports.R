##Measure OOS Economic Gains from Vol Timing Strategy

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
  
  ## Risk-Free Return
  tbill <- readxl::read_excel(file.path(here::here(), "data", "TBILL.xlsx"),
                              sheet = "Monthly", range = "A1:B1101",
                              col_names = TRUE) %>%
    dplyr::rename(tbill = 2, dates = 1) %>%
    dplyr::mutate(
      month_id   = as.Date(paste(lubridate::year(dates),
                                 lubridate::month(dates), "01", sep = "-"))
    ) %>%
    dplyr::arrange(month_id) %>%
    dplyr::mutate(risk_free = (1 + tbill/100)^(1/12) - 1)

  ## S&P Returns
  crsp <- read.csv(file = file.path(here::here(), "data", "crsp.csv")) %>%
    as_tibble()
  
    ### Paste caldt into date
    crsp <-  crsp %>%
      mutate(
        ### Parse as dates
        caldt = as.Date(caldt, format = "%m/%d/%y"),
        ### If parsed year > current year, subtract 100 years
        caldt = if_else(caldt > as.Date("2025-01-01"),
                        lubridate::add_with_rollback(caldt,
                                                     lubridate::years(-100)),
                        caldt)
      )
    
    ### Compute monthly returns
    crsp_monthly <- crsp %>%
      dplyr::mutate(year  = lubridate::year(caldt),
                    month = lubridate::month(caldt),
                    month_id = as.Date(paste(year, month, "01", sep = "-"))) %>% 
      dplyr::group_by(month_id) %>%
      dplyr::summarise(
        date    = dplyr::last(caldt),                # last trading day of the month
        vwretd  = prod(1 + vwretd) - 1,              # compound gross returns
        .groups = "drop"
      ) %>%
      dplyr::arrange(date) 
    
  ## Merge them
  fwd_returns <- crsp_monthly %>%
    dplyr::left_join(tbill, by = "month_id") %>% 
    dplyr::select(date, vwretd, risk_free) %>%
    dplyr::rename(sp500 = vwretd, rf = risk_free) %>%
    ### Compute fwd returns
    dplyr::mutate(
      month_id   = as.Date(paste(lubridate::year(date),
                                 lubridate::month(date), "01", sep = "-")),
      sp500_fwd1 = sp500, #month_id is floor_date, so sp500 is fwd1 given day 1
      sp500      = dplyr::lag(sp500), #sp500 is return from month_id to month_id + 1, so lag it to get return from month_id - 1 to month_id
      rf_fwd1    = rf,     #month_id is floor_date, so rf is fwd1 given day 1 
      rf         = dplyr::lag(rf)     #rf is return from month_id to month_id + 1, so lag it to get return from month_id - 1 to month_id    
    ) %>%
    tidyr::drop_na() %>%
    dplyr::select(month_id, sp500, sp500_fwd1, rf, rf_fwd1) 
      
  ## Load models and target
  list_backtest_res_1927 <- readRDS(file.path(here::here(), "models", "1927", "list_backtest_res_1927.rds"))
  list_backtest_res_1986 <- readRDS(file.path(here::here(), "models", "1986", "list_backtest_res_1986.rds"))
  target_gold            <- readRDS(file.path(here::here(), "data", "target_gold.rds"))
  
  ## Backtest
  all_ports_1927 <- purrr::imap(
    list_backtest_res_1927,
    function(obj, model_name) {
      
      message("Running portfolio backtest for: ", model_name)
      
      # Extract oos_outputs depending on object type --------------------------
      oos_raw <- tryCatch(
        obj@oos_outputs,          # S4 slot
        error = function(e) obj$oos_outputs   # fallback to list element
      )
      
      # Standardise to month_id / target / pred / error ----------------------
      # Ensemble objects carry ensemble_pred instead of pred
      if (!"pred" %in% colnames(oos_raw)) {
        if (!"ensemble_pred" %in% colnames(oos_raw)) {
          stop("Cannot find pred or ensemble_pred in oos_outputs for model: ", model_name)
        }
        oos_raw <- oos_raw %>%
          dplyr::transmute(
            month_id,
            target,
            pred  = ensemble_pred,
            error = target - ensemble_pred
          )
      } else {
        oos_raw <- oos_raw %>%
          dplyr::select(month_id, target, pred, error)
      }
      
      # Run portfolio backtest ------------------------------------------------
      run_oos_ports(
        oos_outputs  = oos_raw,
        fwd_returns  = fwd_returns,
        target_gold  = target_gold,
        risk_aversion = c(1, 2, 5, 10)
      )
    }
  )
  
  ## Backtest
  all_ports_1986 <- purrr::imap(
    list_backtest_res_1986,
    function(obj, model_name) {
      
      message("Running portfolio backtest for: ", model_name)
      
      # Extract oos_outputs depending on object type --------------------------
      oos_raw <- tryCatch(
        obj@oos_outputs,          # S4 slot
        error = function(e) obj$oos_outputs   # fallback to list element
      )
      
      # Standardise to month_id / target / pred / error ----------------------
      # Ensemble objects carry ensemble_pred instead of pred
      if (!"pred" %in% colnames(oos_raw)) {
        if (!"ensemble_pred" %in% colnames(oos_raw)) {
          stop("Cannot find pred or ensemble_pred in oos_outputs for model: ", model_name)
        }
        oos_raw <- oos_raw %>%
          dplyr::transmute(
            month_id,
            target,
            pred  = ensemble_pred,
            error = target - ensemble_pred
          )
      } else {
        oos_raw <- oos_raw %>%
          dplyr::select(month_id, target, pred, error)
      }
      
      # Run portfolio backtest ------------------------------------------------
      run_oos_ports(
        oos_outputs  = oos_raw,
        fwd_returns  = fwd_returns,
        target_gold  = target_gold,
        risk_aversion = c(1, 2, 5, 10)
      )
    }
  )
  
    ### Compile all strategy_summary into one df
    strategy_summary_1986 <- purrr::imap_dfr(
      all_ports_1986,
      function(port_res, model_name) {
        port_res$strategy_summary %>%
          dplyr::mutate(model = model_name) %>%
          dplyr::select(model, dplyr::everything())
      }
    )
    
  
    
    # Define scatterplot
    plot_risk_return <- function(strategy_summary, ret_col, sd_col, title) {
      
      # Define benchmark methods
      benchmark_methods <- c("rw", "6040", "buyhold", "oracle")
      
      # Keep only one row per benchmark (since they repeat across models)
      data <- strategy_summary %>%
        dplyr::group_by(method) %>%
        dplyr::mutate(
          is_benchmark = method %in% benchmark_methods
        ) %>%
        dplyr::filter(!is_benchmark) %>%
        dplyr::ungroup() %>%
        dplyr::select(-is_benchmark)
      
      ggplot2::ggplot(data, ggplot2::aes_string(x = sd_col, y = ret_col,
                                                color = "model", shape = "method")) +
        ggplot2::geom_point(size = 3, alpha = 0.8) +
        ggplot2::labs(
          title = title,
          x = "Volatility (Std. Dev.)",
          y = "Average Excess Return"
        ) +
        ggplot2::theme_minimal()
    }
    
    p_capped_1986 <- plot_risk_return(
      strategy_summary_1986 %>%
        dplyr::filter(model %in% c("har", "har_resc", "harx", "harx_resc",
                                   "glmnet_resc",  
                                   "rf_resc", 
                                   "xgb_resc", 
                                   "nn1_resc_4_ens5", 
                                   "nn2_resc_4_ens5",
                                   "nn3_resc_4_ens5",
                                   "nn4_resc_4_ens5", 
                                   "nn5_resc_4_ens5",
                                   "lstm_resc5", "ensemble_all", "ensemble_nn")) %>%
        dplyr::mutate(model = dplyr::case_when(
          model == "har" ~ "HAR",
          model == "har_resc" ~ "HAR Range",
          model == "harx" ~ "HAR-X",
          model == "harx_resc" ~ "HAR-X Range",
          model == "glmnet_resc" ~ "ENET Range",
          model == "rf_resc" ~ "RF Range",
          model == "rf_resc_grid" ~ "RF Range Grid",
          model == "xgb_resc" ~ "XGB Range",
          model == "xgb_resc_grid" ~ "XGB Range Grid",
          model == "nn1_resc_4_ens5" ~ "NN1 Range ENS5",
          model == "nn1_resc_grid" ~ "NN1 Range Grid",
          model == "nn2_resc_4_ens5" ~ "NN2 Range ENS5",
          model == "nn2_resc_grid" ~ "NN2 Range Grid",
          model == "nn3_resc_4_ens5" ~ "NN3 Range ENS5",
          model == "nn3_resc_grid" ~ "NN3 Range Grid",
          model == "nn4_resc_4_ens5" ~ "NN4 Range ENS5",
          model == "nn4_resc_grid" ~ "NN4 Range Grid",
          model == "nn5_resc_4_ens5" ~ "NN5 Range ENS5",
          model == "nn5_resc_grid" ~ "NN5 Range Grid",
          model == "lstm_resc5" ~ "LSTM Range",
          model == "ensemble_all" ~ "Ensemble All",
          model == "ensemble_nn" ~ "Ensemble NN",
          TRUE ~ model)),
      ret_col = "avg_exc_ret_net_capped",
      sd_col  = "sd_return_net_capped",
      title   = "Risk-Return (Net, Capped)"
    )
    
    p_capped_1986
    
    
    # Bar plot of excess returns
    plot_bar_sharpe <- function(strategy_summary, sharpe_col, title) {
      
      # Define benchmark methods
      benchmark_methods <- c("rw", "6040", "buyhold", "oracle")
      
      # Keep only one row per benchmark (since they repeat across models)
      data <- strategy_summary %>%
        dplyr::group_by(method) %>%
        dplyr::mutate(
          is_benchmark = method %in% benchmark_methods,
          keep_row = ifelse(is_benchmark, dplyr::row_number() == 1, TRUE)
        ) %>%
        dplyr::filter(keep_row) %>%
        dplyr::ungroup()
      
      ggplot2::ggplot(
        data,
        ggplot2::aes(
          x = reorder(method, .data[[sharpe_col]]),
          y = .data[[sharpe_col]],
          fill = model
        )
      ) +
        ggplot2::geom_col(position = "dodge") +
        ggplot2::coord_flip() +
        ggplot2::labs(
          title = title,
          x = "Method",
          y = "Sharpe Ratio"
        ) +
        ggplot2::theme_minimal()
    }
    
    p_raw_1986 <- plot_bar_sharpe(
      strategy_summary_1986 %>%
        dplyr::filter(model %in% c("har", "har_resc", "harx", "harx_resc",
                                   "glmnet_resc",  
                                   "rf_resc", 
                                   "xgb_resc", 
                                   "nn1_resc_4_ens5", 
                                   "nn2_resc_4_ens5",
                                   "nn3_resc_4_ens5",
                                   "nn4_resc_4_ens5", 
                                   "nn5_resc_4_ens5",
                                   "lstm_resc5",
                                   "ensemble_all", "ensemble_nn", "ensemble_all_but_nn")) %>%
        dplyr::filter(method != "oracle") %>%
        dplyr::mutate(model = dplyr::case_when(
          model == "har" ~ "HAR",
          model == "har_resc" ~ "HAR Range",
          model == "harx" ~ "HAR-X",
          model == "harx_resc" ~ "HAR-X Range",
          model == "glmnet_resc" ~ "ENET Range",
          model == "rf_resc" ~ "RF Range",
          model == "rf_resc_grid" ~ "RF Range Grid",
          model == "xgb_resc" ~ "XGB Range",
          model == "xgb_resc_grid" ~ "XGB Range Grid",
          model == "nn1_resc_4_ens5" ~ "NN1 Range ENS5",
          model == "nn1_resc_grid" ~ "NN1 Range Grid",
          model == "nn2_resc_4_ens5" ~ "NN2 Range ENS5",
          model == "nn2_resc_grid" ~ "NN2 Range Grid",
          model == "nn3_resc_4_ens5" ~ "NN3 Range ENS5",
          model == "nn3_resc_grid" ~ "NN3 Range Grid",
          model == "nn4_resc_4_ens5" ~ "NN4 Range ENS5",
          model == "nn4_resc_grid" ~ "NN4 Range Grid",
          model == "nn5_resc_4_ens5" ~ "NN5 Range ENS5",
          model == "nn5_resc_grid" ~ "NN5 Range Grid",
          model == "lstm_resc5" ~ "LSTM Range",
          model == "ensemble_all" ~ "Ensemble All",
          model == "ensemble_nn" ~ "Ensemble NN",
          model == "ensemble_all_but_nn" ~ "Ensemble All but NN",
          TRUE ~ model)),
      sharpe_col = "sharpe_ratio_raw",
      title      = "Out-of-Sample Sharpe Ratio (Raw, Before Costs)"
    )
    
    p_raw_1986
    
    p_capped_1986 <- plot_bar_sharpe(
      strategy_summary_1986 %>%
        dplyr::filter(model %in% c("har", "har_resc", "harx", "harx_resc",
                                   "glmnet_resc",  
                                   "rf_resc", 
                                   "xgb_resc", 
                                   "nn1_resc_4_ens5", 
                                   "nn2_resc_4_ens5",
                                   "nn3_resc_4_ens5",
                                   "nn4_resc_4_ens5", 
                                   "nn5_resc_4_ens5",
                                   "lstm_resc5",
                                   "ensemble_all", "ensemble_nn", "ensemble_all_but_nn")) %>%
        dplyr::filter(method != "oracle") %>%
        dplyr::mutate(model = dplyr::case_when(
          model == "har" ~ "HAR",
          model == "har_resc" ~ "HAR Range",
          model == "harx" ~ "HAR-X",
          model == "harx_resc" ~ "HAR-X Range",
          model == "glmnet_resc" ~ "ENET Range",
          model == "rf_resc" ~ "RF Range",
          model == "rf_resc_grid" ~ "RF Range Grid",
          model == "xgb_resc" ~ "XGB Range",
          model == "xgb_resc_grid" ~ "XGB Range Grid",
          model == "nn1_resc_4_ens5" ~ "NN1 Range ENS5",
          model == "nn1_resc_grid" ~ "NN1 Range Grid",
          model == "nn2_resc_4_ens5" ~ "NN2 Range ENS5",
          model == "nn2_resc_grid" ~ "NN2 Range Grid",
          model == "nn3_resc_4_ens5" ~ "NN3 Range ENS5",
          model == "nn3_resc_grid" ~ "NN3 Range Grid",
          model == "nn4_resc_4_ens5" ~ "NN4 Range ENS5",
          model == "nn4_resc_grid" ~ "NN4 Range Grid",
          model == "nn5_resc_4_ens5" ~ "NN5 Range ENS5",
          model == "nn5_resc_grid" ~ "NN5 Range Grid",
          model == "lstm_resc5" ~ "LSTM Range",
          model == "ensemble_all" ~ "Ensemble All",
          model == "ensemble_nn" ~ "Ensemble NN",
          model == "ensemble_all_but_nn" ~ "Ensemble All but NN",
          TRUE ~ model)),
      sharpe_col = "sharpe_ratio_net_capped",
      title      = "Out-of-Sample Sharpe Ratio (Capped, After Costs)"
    )
    
    p_capped_1986
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    

    
    
  
  
  
  