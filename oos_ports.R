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
  
  

    
    
  
  
  
  