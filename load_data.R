######################
##  RV Predictions  ##
######################


# Initial Setup------------------------------------------------------------------
#renv::install("dplyr")
#renv::install("lubridate")
library("dplyr")

project_dir <- here::here()
helpers_path <- file.path(project_dir, "R", "helpers.R")

if (!file.exists(helpers_path)) {
  stop("Required helper file is missing: ", helpers_path)
}

source(helpers_path)

required_inputs <- file.path(
  project_dir,
  "data",
  c(
    "crsp.csv", "EPU.xlsx", "DFR.xlsx", "DFY.xlsx", "TMS.xlsx",
    "PPI.xlsx", "IPB50001N.xlsx", "MBAS.xlsx", "DY.xlsx", "EY.xlsx",
    "TBILL.xlsx", "CSENT.csv", "GPR.xls", "WTI.xlsx", "VIX.csv",
    "VXO.xls", "[usa]_[all_factors]_[daily]_[vw_cap].csv"
  )
)
missing_inputs <- required_inputs[!file.exists(required_inputs)]

if (length(missing_inputs) > 0) {
  stop("Missing required input file(s):\n", paste(missing_inputs, collapse = "\n"))
}

  ## Load
  crsp <- utils::read.csv(file = file.path(here::here(), "data", "crsp.csv")) %>%
    tibble::as_tibble()

  required_crsp_cols <- c("caldt", "vwretd")
  missing_crsp_cols <- setdiff(required_crsp_cols, names(crsp))

  if (length(missing_crsp_cols) > 0) {
    stop("crsp.csv is missing required column(s): ",
         paste(missing_crsp_cols, collapse = ", "))
  }
  
  ## Paste caldt into date
  crsp <-  crsp %>%
    mutate(
      ### Parse as dates
      caldt = as.Date(caldt, format = "%m/%d/%y"),
      ### If parsed year > current year, subtract 100 years
      caldt = if_else(caldt >= as.Date("2025-01-01"),
                       lubridate::add_with_rollback(caldt,
                                                    lubridate::years(-100)),
                       caldt)
    ) %>%
    dplyr::arrange(caldt)

  if (anyNA(crsp$caldt)) {
    stop("crsp.csv contains caldt values that cannot be parsed as %m/%d/%y.")
  }
  if (anyDuplicated(crsp$caldt)) {
    stop("crsp.csv must contain one unique observation per caldt.")
  }
  if (!is.numeric(crsp$vwretd)) {
    stop("crsp.csv column vwretd must be numeric.")
  }
  if (any(!is.finite(crsp$vwretd))) {
    stop("crsp.csv contains missing or non-finite vwretd values.")
  }
  
  ## Construct monthly and weekly RV for target (vwretd)
  crsp_monthly <- crsp %>%
    ### Calculate daily RV
    dplyr::mutate(rv_daily = vwretd ^ 2) %>%
    ### Get month ids
    dplyr::mutate(month_id = lubridate::floor_date(caldt, unit = "month")) %>%
    dplyr::group_by(month_id) %>%
    dplyr::summarize(rv_month = sum(rv_daily, na.rm = TRUE), .groups = "drop")
  
  crsp_weekly <- crsp %>%
    ### Calculate daily RV
    dplyr::mutate(rv_daily = vwretd ^ 2) %>%
    ### Get week ids
    dplyr::mutate(week_id = lubridate::floor_date(caldt, unit = "week",
                                                  week_start = 1)) %>%
    dplyr::group_by(week_id) %>%
    dplyr::summarize(rv_week = sum(rv_daily, na.rm = TRUE), .groups = "drop")
  

    
# Lagged terms covariates-------------------------------------------------------
  
    ### RV, RQ, the jump/continuous decomposition and the leverage term for the
    ### daily S&P 500 series. The body of these blocks now lives in
    ### generate_rv_feature_block() (R/helpers.R); the factor panel below calls
    ### the very same function per factor, which is what keeps the panel on the
    ### definitions established here instead of a copy that can drift.
    sp500_blocks <- generate_rv_feature_block(
      data     = crsp,
      date_col = "caldt",
      ret_col  = "vwretd",
      week_len = 5,
      month_trading_days = 22
    )

    rv     <- sp500_blocks$rv
    rq     <- sp500_blocks$rq
    jump   <- sp500_blocks$jump
    cont   <- sp500_blocks$cont
    negret <- sp500_blocks$negret
    
# Macro covariates--------------------------------------------------------------
    

  ## EPU (Liu and Zhang 2015) https://www.policyuncertainty.com/us_monthly
  epu <- readxl::read_excel(file.path(here::here(), "data", "EPU.xlsx"),
                            sheet = "Main News Index", range = "A1:C1509", 
                            col_names = TRUE) %>%
    ### Rename 3rd col as epu
    dplyr::rename(epu = 3) %>%
    ### Create month id by concatenating Year and Month cols
    dplyr::mutate(
      month_id = as.Date(paste(Year, Month, "01", sep = "-"))
    ) %>%
    dplyr::arrange(month_id) %>%
    ### Create log_epu_diff_lag1
    dplyr::mutate(
      log_epu_diff      = dplyr::if_else(dplyr::row_number() == 1, NA_real_, 
                                          log(epu) - log(dplyr::lag(epu))),
      log_epu_diff_lag1 = dplyr::lag(log_epu_diff, 1)
    ) %>%
    dplyr::select(month_id, log_epu_diff_lag1)
    
  ## Default Return Spread (Schwert (1989), Paye (2012))
  ## https://fred.stlouisfed.org/series/AAA10YM
  dfr <- readxl::read_excel(file.path(here::here(), "data", "DFR.xlsx"),
                             sheet = "Monthly", range = "A1:B870", 
                             col_names = TRUE) %>%
    dplyr::rename(dfr = 2, dates = 1) %>%
    dplyr::mutate(
      month_id = as.Date(paste(lubridate::year(dates),
                               lubridate::month(dates), "01", sep = "-"))
    ) %>%
    dplyr::arrange(month_id) %>%
    dplyr::mutate(dfr_lag1 = dplyr::lag(dfr, 1)) %>%
    dplyr::select(month_id, dfr_lag1)
  
  ## Default Spread (Paye 2012) - Not Significant
  ## https://fred.stlouisfed.org/series/BAA10YM
  dfy <- readxl::read_excel(file.path(here::here(), "data", "DFY.xlsx"),
                            sheet = "Monthly", range = "A1:B870", 
                            col_names = TRUE) %>%
    dplyr::rename(dfy = 2, dates = 1) %>%
    dplyr::mutate(
      month_id = as.Date(paste(lubridate::year(dates),
                               lubridate::month(dates), "01", sep = "-"))
    ) %>%
    dplyr::arrange(month_id) %>%
    dplyr::mutate(dfy_lag1 = dplyr::lag(dfy, 1)) %>%
    dplyr::select(month_id, dfy_lag1)
    
  ## Term Spread (Paye 2012) - Not Significant
  ## https://fred.stlouisfed.org/series/T10YFFM
  tms <- readxl::read_excel(file.path(here::here(), "data", "TMS.xlsx"),
                            sheet = "Monthly", range = "A1:B855", 
                            col_names = TRUE) %>%
    dplyr::rename(tms = 2, dates = 1) %>%
    dplyr::mutate(
      month_id = as.Date(paste(lubridate::year(dates),
                               lubridate::month(dates), "01", sep = "-"))
    ) %>%
    dplyr::arrange(month_id) %>%
    dplyr::mutate(tms_lag1 = dplyr::lag(tms, 1)) %>%
    dplyr::select(month_id, tms_lag1)
    
  ## PPI Vol (Schwert (1989), Paye (2012))
  ## https://fred.stlouisfed.org/series/PPIACO (Non-Seas Adj)
  ppi <- readxl::read_excel(file.path(here::here(), "data", "PPI.xlsx"),
                           sheet = "Monthly", range = "A1:B1353", 
                           col_names = TRUE) %>%
    dplyr::rename(ppi = 2, dates = 1) %>%
    dplyr::mutate(
      month_id   = as.Date(paste(lubridate::year(dates),
                                 lubridate::month(dates), "01", sep = "-"))
    ) %>%
    dplyr::arrange(month_id) %>%
    dplyr::mutate(ppi_growth = 100 * (log(ppi) - log(dplyr::lag(ppi))))
    
    ### To calculate ppi_vol, we should use Schwert (1989) formulation.
    ### However, this will introduce forward-looking bias if coefficients
    ### are estimated using the whole dataset.
    ppi <- schwert_vol_oos(
      data = ppi %>% dplyr::select(dates, ppi_growth),
      date_col = "dates",
      value_col = "ppi_growth",
      p = 12,
      start_year = min(lubridate::year(crsp$caldt)),
      annualize = TRUE
    ) %>%
      #### Get vars
      dplyr::mutate(month_id     = as.Date(paste(lubridate::year(dates),
                                                 lubridate::month(dates), "01",
                                                 sep = "-")),
                    ### No lag needed, even though ppi from t is released t+1
                    ### Bco function will use data up to t-1 to gen sigma_t
                    ppi_vol      = sigma_ann
                    ) %>%
      dplyr::arrange(month_id) %>%
      dplyr::select(month_id, ppi_vol)
    
    ## Industrial Production Vol (Non-Seas Adj)
    ## https://fred.stlouisfed.org/series/IPB50001N
    indpro <- readxl::read_excel(file.path(here::here(), "data", "IPB50001N.xlsx"),
                             sheet = "Monthly", range = "A1:B1280", 
                             col_names = TRUE) %>%
      dplyr::rename(indpro = 2, dates = 1) %>%
      dplyr::mutate(
        month_id = as.Date(paste(lubridate::year(dates),
                                 lubridate::month(dates), "01", sep = "-"))
      ) %>%
      dplyr::arrange(month_id) %>%
      dplyr::mutate(ip_growth = 100 * (log(indpro) - log(dplyr::lag(indpro))))
    
      ### Calculate ind_pro_vol
      indpro <- schwert_vol_oos(
        data = indpro %>% dplyr::select(dates, ip_growth),
        date_col = "dates",
        value_col = "ip_growth",
        p = 12,
        start_year = min(lubridate::year(crsp$caldt)),
        annualize = TRUE
      ) %>%
        #### Get vars
        dplyr::mutate(month_id     = as.Date(paste(lubridate::year(dates),
                                                   lubridate::month(dates), "01",
                                                   sep = "-")),
                      ### No lag needed, even though ip from t is released t+1
                      ### Bco function will use data up to t-1 to gen sigma_t
                      ip_vol       = sigma_ann
        ) %>%
        dplyr::arrange(month_id) %>%
        dplyr::select(month_id, ip_vol)
      
    
    ## Monetary Base Vol (Schwert (1989))
    ## https://fred.stlouisfed.org/series/BOGMBASE
    mbas <- readxl::read_excel(file.path(here::here(), "data", "MBAS.xlsx"),
                               sheet = "Monthly", range = "A1:B800", 
                               col_names = TRUE) %>%
      dplyr::rename(mbas = 2, dates = 1) %>%
      dplyr::mutate(
        month_id   = as.Date(paste(lubridate::year(dates),
                                   lubridate::month(dates), "01", sep = "-"))
      ) %>%
      dplyr::arrange(month_id) %>%
      dplyr::mutate(mbas_growth = 100 * (log(mbas) - log(dplyr::lag(mbas))))
    
      ### Calculate mbas_vol
      mbas <- schwert_vol_oos(
        data = mbas %>% dplyr::select(dates, mbas_growth),
        date_col = "dates",
        value_col = "mbas_growth",
        p = 12,
        start_year = min(lubridate::year(crsp$caldt)),
        annualize = TRUE
      ) %>%
        #### Get vars
        dplyr::mutate(month_id     = as.Date(paste(lubridate::year(dates),
                                                   lubridate::month(dates), "01",
                                                   sep = "-")),
                      ### No lag needed, even though mbas from t is released t+1
                      ### Bco function will use data up to t-1 to gen sigma_t
                      mbas_vol     = sigma_ann
        ) %>%
        dplyr::arrange(month_id) %>%
        dplyr::select(month_id, mbas_vol)
    
    
  ## DY (Audrino et al. (2020))
  ## Multpl
  dy <- readxl::read_excel(file.path(here::here(), "data", "DY.xlsx"),
                           sheet = "Sheet1", range = "A1:B1858", 
                           col_names = TRUE) %>%
    dplyr::rename(dy = 2, dates = 1) %>%
    dplyr::mutate(
      dates    = lubridate::mdy(dates),
      dy       = readr::parse_number(dy) / 
                 ifelse(stringr::str_detect(dy, "%"), 100, 1),
      month_id = as.Date(paste(lubridate::year(dates),
                               lubridate::month(dates), "01", sep = "-"))
    ) %>%
    dplyr::arrange(month_id) %>%
    dplyr::mutate(dy_lag1 = dplyr::lag(dy, 1)) %>%
    dplyr::select(month_id, dy_lag1)
  
  ## EY (Audrino et al. (2020))
  ## Multpl
  ey <- readxl::read_excel(file.path(here::here(), "data", "EY.xlsx"),
                           sheet = "Sheet1", range = "A1:B1858", 
                           col_names = TRUE) %>%
    dplyr::rename(ey = 2, dates = 1) %>%
    dplyr::mutate(
      dates    = lubridate::mdy(dates),
      ey       = readr::parse_number(ey) / 
                 ifelse(stringr::str_detect(ey, "%"), 100, 1),
      month_id = as.Date(paste(lubridate::year(dates),
                               lubridate::month(dates), "01", sep = "-"))
    ) %>%
    dplyr::arrange(month_id) %>%
    dplyr::mutate(ey_lag1 = dplyr::lag(ey, 1)) %>%
    dplyr::select(month_id, ey_lag1)
  
  ## T-bill (Audrino et al. (2020))
  ## https://fred.stlouisfed.org/series/TB3MS
  tbill <- readxl::read_excel(file.path(here::here(), "data", "TBILL.xlsx"),
                              sheet = "Monthly", range = "A1:B1101",
                              col_names = TRUE) %>%
    dplyr::rename(tbill = 2, dates = 1) %>%
    dplyr::mutate(
      month_id   = as.Date(paste(lubridate::year(dates),
                                 lubridate::month(dates), "01", sep = "-"))
    ) %>%
    dplyr::arrange(month_id) %>%
    dplyr::mutate(tbill_lag1 = dplyr::lag(tbill, 1)) %>%
    dplyr::select(month_id, tbill_lag1)
  
  ## Consumer Sentiment (Audrino et al. (2020))
  ## https://data.sca.isr.umich.edu/data-archive/mine.php
  cons_sent <- utils::read.csv(file.path(here::here(), "data", "CSENT.csv"),
                               skip = 1 ) %>%
    tibble::as_tibble() %>%
    dplyr::rename(cons_sent = 3, month = 1, year = 2) %>%
    dplyr::mutate(
      month_id           = as.Date(paste(year, month, "01", sep = "-"))
    ) %>%
    dplyr::arrange(month_id) %>%
    dplyr::mutate(
      log_cons_sent_diff = dplyr::if_else(dplyr::row_number() == 1, NA_real_,
                                          log(cons_sent) -
                                            log(dplyr::lag(cons_sent))),
      log_cons_sent_diff_lag1 = dplyr::lag(log_cons_sent_diff, 1)
    ) %>%
    dplyr::select(month_id, log_cons_sent_diff_lag1)
  
  ## Geopolitical Risk (Niu et al. (2023))
  ## https://www.matteoiacoviello.com/gpr.htm
  gpr <- readxl::read_excel(file.path(here::here(), "data", "GPR.xls"),
                           sheet = "Sheet1", range = "A1:E1509", 
                           col_names = TRUE,
                           col_types = c("date", "numeric", "numeric",
                                         "numeric", "numeric")) %>%
    dplyr::rename(gpr = 2, dates = 1, gpr_h = 5) %>%
    ### GPR starts in 1985. For dates before that, use gpr_h, merging into
    ### one only series
    dplyr::mutate(
      dates        = as.Date(dates),
      month_id     = as.Date(paste(lubridate::year(dates),
                               lubridate::month(dates), "01", sep = "-")),
      gpr          = dplyr::if_else(dates < as.Date("1985-01-01"), 
                                     gpr_h,
                                     gpr)
    ) %>%
    dplyr::arrange(month_id) %>%
    dplyr::mutate(
      log_gpr_diff = dplyr::if_else(dplyr::row_number() == 1, NA_real_,
                                          log(gpr) - log(dplyr::lag(gpr))),
      ### Lagged
      gpr_lag1 = dplyr::lag(log_gpr_diff, 1)
    ) %>%
    dplyr::select(month_id, gpr_lag1) 

  ## Crude Oil RV Fameliti & Skintzi (2024)
  ## https://fred.stlouisfed.org/series/DCOILWTICO
  oil <- readxl::read_excel(file.path(here::here(), "data", "WTI.xlsx"),
                            sheet = "Daily", range = "A1:B10354", 
                            col_names = TRUE) %>%
    dplyr::rename(oil = 2, dates = 1) %>%
    dplyr::mutate(
      dates        = as.Date(dates),
      month_id     = as.Date(paste(lubridate::year(dates),
                                   lubridate::month(dates), "01", sep = "-")),
      #### Guard for negative or zero prices
      oil     = dplyr::if_else(oil <= 0, NA_real_, oil)
    ) %>%
    dplyr::arrange(dates) %>%
    dplyr::mutate(
      oil_ret = dplyr::if_else(dplyr::row_number() == 1, NA_real_,
                                     log(oil) - log(dplyr::lag(oil))),
      oil_rv  = oil_ret ^ 2
    ) %>%
    generate_HAR_terms(
      date_col = dates,
      value_col = "oil_rv",
      prefix = NULL,
      week_len = 5,
      month_trading_days = 22,
      scale_to_month = TRUE,
      month_agg = "sum"
    )


# Implied Volatility Covariates-------------------------------------------------
  
  ## VIX (Blair et al (2001), Bekaert & Hoerova (2014), Christensen and
  ## Prabhala (1998): 
  ### https://www.cboe.com/tradable_products/vix/vix_historical_data/
  vix <- utils::read.csv(file = file.path(here::here(), "data", "VIX.csv")) %>%
    tibble::as_tibble() %>%
    dplyr::rename(vix = 5, dates = 1) %>%
    dplyr::select(dates, vix) %>% 
    dplyr::mutate(
      dates    = lubridate::mdy(dates),
      month_id = as.Date(paste(lubridate::year(dates),
                               lubridate::month(dates), "01", sep = "-")),
      vix      = ( (vix/100)^2 ) / 12 #Follows Bekaert & Hoerova (2014)
    ) %>%
    ### Agg to month
    dplyr::group_by(month_id) %>%
    dplyr::filter(dates == max(dates)) %>% 
    dplyr::arrange(month_id) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(vix_lag1 = dplyr::lag(vix, 1)) 

  
    #### Optional: Expand with VXO (for more 4 years of data)
    vxo <- readxl::read_excel(file.path(here::here(),"data", "VXO.xls"),
                              sheet = "Sheet1", range = "A3:E3535") %>%
      tibble::as_tibble() %>%
      dplyr::rename(vxo = 5, dates = 1) %>%
      dplyr::select(dates, vxo) %>% 
      dplyr::mutate(
        month_id = as.Date(paste(lubridate::year(dates),
                                 lubridate::month(dates), "01", sep = "-")),
        vxo      = ( (vxo/100)^2 ) / 12 #Follows Bekaert & Hoerova (2014)
      ) %>%
      ### Agg to month
      dplyr::group_by(month_id) %>%
      dplyr::filter(dates == max(dates)) %>% 
      dplyr::arrange(month_id) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(vxo_lag1 = dplyr::lag(vxo, 1)) 
    
    #### Bind rows for vix, considering dates before first date in vix
    vix <- dplyr::bind_rows(
      vxo %>%
        dplyr::filter(dates < min(vix$dates)) %>%
        dplyr::select(dates, month_id, vxo_lag1) %>%
        dplyr::rename(vix_lag1 = vxo_lag1),
      vix %>% dplyr::select(-vix)
    ) %>%
      dplyr::select(month_id, vix_lag1)
  

# Calendar Covariates-----------------------------------------------------------
    
  ## Month dummies
  month_dummies <- crsp %>% 
    dplyr::mutate(
      dates    = as.Date(caldt),
      month_id = lubridate::floor_date(dates, unit = "month"),
      month    = factor(lubridate::month(dates), levels = 1:12)
    ) %>% 
      dplyr::arrange(month_id) %>% 
      ### Get unique month ids
      dplyr::distinct(month_id, .keep_all = TRUE) %>%
      dplyr::select(month_id, month)
    
  ## Sqrt of number of days in month
  days <- crsp %>% 
    dplyr::mutate(
      dates    = as.Date(caldt),
      month_id = as.Date(paste(lubridate::year(dates),
                               lubridate::month(dates), "01", sep = "-"))
    ) %>%
    dplyr::group_by(month_id) %>%
    dplyr::summarize(
      n_days = dplyr::n(),
      sqrt_days = sqrt(n_days),
      .groups = "drop"
    ) %>%
    dplyr::arrange(month_id) %>%
    dplyr::select(month_id, sqrt_days)

   
    
# Merge all covariates-----------------------------------------------------------
  
  ## Get a list of all covariates
  covariates_list <- list(
    ### HAR terms
    rv, rq, jump, cont, negret,
    ### Macro vars
    epu, dfr, dfy, tms, ppi, indpro, mbas,
    dy, ey, tbill, cons_sent, gpr, oil,
    ### Implied
    vix,
    ### Calendar
    month_dummies, days
  ) %>%
    setNames(
      c(
        "rv", "rq", "jump", "cont", "negret",
        "epu", "dfr", "dfy", "tms", "ppi", "indpro", "mbas",
        "dy", "ey", "tbill", "cons_sent", "gpr", "oil",
        "vix",
        "month_dummies", "days"
      )
    )
  
  ## Get starting date for each tibble inside covariates_list
  starting_dates <- purrr::map_dbl(
    covariates_list,
    ~ min(.$month_id, na.rm = TRUE) 
  ) %>% as.Date()
  
    ### There are three possible subsets:
     #### >1927 onwards
     covariates_1927 <- which(starting_dates <= as.Date("1927-01-01"))
     cat("Covariates from 1927 onwards:", names(covariates_list)[covariates_1927], "\n")
     
     #### >1962 onwards
     covariates_1962 <- which(starting_dates <= as.Date("1962-01-01"))
     cat("Covariates from 1962 onwards:", names(covariates_list)[covariates_1962], "\n")
     cat("This adds:", setdiff(names(covariates_list)[covariates_1962],
                                        names(covariates_list)[covariates_1927]), "\n")
     
     #### >1986 onwards
     covariates_1986 <- which(starting_dates <= as.Date("1986-01-01"))
     cat("Covariates from 1986 onwards:", names(covariates_list)[covariates_1986], "\n")
     cat("This adds:", setdiff(names(covariates_list)[covariates_1986],
                                        names(covariates_list)[covariates_1962]), "\n")
     
    ### Create the three possible features matrix using purrr::reduce
    covariates_1927_df <- purrr::reduce(
      covariates_list[covariates_1927],
      dplyr::full_join,
      by = "month_id"
    ) %>%
      dplyr::arrange(month_id) %>%
      dplyr::filter(month_id >= as.Date("1927-01-01") &
                      month_id <= as.Date("2024-12-01")
                    ) 
    
    covariates_1962_df <- purrr::reduce(
      covariates_list[covariates_1962],
      dplyr::full_join,
      by = "month_id"
    ) %>%
      dplyr::arrange(month_id) %>%
      dplyr::filter(month_id >= as.Date("1962-01-01") &
                      month_id <= as.Date("2024-12-01")
                    ) 
    
    covariates_1986_df <- purrr::reduce(
      covariates_list[covariates_1986],
      dplyr::full_join,
      by = "month_id"
    ) %>%
      dplyr::arrange(month_id) %>%
      dplyr::filter(month_id >= as.Date("1986-02-01") &
                      month_id <= as.Date("2024-12-01")
                    )
    
    
# Factor panel (v3)-------------------------------------------------------------

  ## Read the JKP daily factor returns

    ### The raw file is deliberately untracked (see .gitignore); its SHA-256 is
    ### written to data/manifest_v3.csv so the build stays reproducible.
    jkp_path <- file.path(here::here(), "data",
                          "[usa]_[all_factors]_[daily]_[vw_cap].csv")

    jkp_raw <- data.table::fread(
      jkp_path,
      select = c("location", "name", "date", "ret", "direction", "n_stocks"),
      showProgress = FALSE
    ) %>%
      tibble::as_tibble() %>%
      ### US only. Industries are deliberately excluded here; they are a later
      ### universe-robustness exercise, not part of this dataset.
      dplyr::filter(location == "usa") %>%
      dplyr::mutate(date = as.Date(date)) %>%
      dplyr::arrange(name, date)

    if (nrow(jkp_raw) == 0L) {
      stop("The JKP input contains no USA factor observations.")
    }
    if (anyNA(jkp_raw$date) || anyNA(jkp_raw$name) || any(jkp_raw$name == "")) {
      stop("The JKP input contains missing or invalid factor identifiers or dates.")
    }
    if (!is.numeric(jkp_raw$ret)) {
      stop("The JKP input column ret must be numeric.")
    }
    if (any(!is.finite(jkp_raw$ret))) {
      stop("The JKP input contains missing or non-finite returns.")
    }
    if (anyDuplicated(jkp_raw[c("name", "date")])) {
      stop("The JKP input must contain one observation per name and date.")
    }

    ### `ret` is a daily return in DECIMAL units, on the same footing as
    ### crsp$vwretd, so no rescaling is applied.
    ### `direction` is documentation only and must NEVER be multiplied in: JKP
    ### already applies the sign upstream. The diagnostic below is the evidence.
    ### Of the factors carrying direction == -1, the large majority have a
    ### POSITIVE full-sample mean return, which is only possible if the sign has
    ### already been applied. Re-applying it here would flip them back and
    ### silently invert half the panel.
    direction_check <- jkp_raw %>%
      dplyr::group_by(name, direction) %>%
      dplyr::summarize(mean_ret = mean(ret, na.rm = TRUE), .groups = "drop") %>%
      dplyr::count(direction, positive_mean = mean_ret > 0)

    cat("\nJKP direction diagnostic (ret is already sign-adjusted):\n")
    print(direction_check)

    ### Breadth is recorded but NOT used as a filter: the inclusion rule below
    ### is about available history, not about cross-sectional breadth.
    cat("\nFactors:", dplyr::n_distinct(jkp_raw$name),
        "| min n_stocks observed:", min(jkp_raw$n_stocks, na.rm = TRUE), "\n")


  ## Per-factor lagged RV blocks

    ### Same function as the S&P 500 series above, called strictly within
    ### factor. group_modify() hands each factor its own rows only, so no lag,
    ### rolling window or month aggregation can reach across a factor boundary.
    cat("\nBuilding per-factor HAR blocks (this is the slow step)...\n")

    factor_blocks <- jkp_raw %>%
      dplyr::group_by(name) %>%
      dplyr::group_modify(~ {
        purrr::reduce(
          generate_rv_feature_block(.x, date_col = "date", ret_col = "ret"),
          dplyr::full_join,
          by = "month_id"
        )
      }) %>%
      dplyr::ungroup()

    ### Per-factor monthly RV target, identical in definition to crsp_monthly
    factor_target <- jkp_raw %>%
      dplyr::mutate(
        rv_daily = ret ^ 2,
        month_id = lubridate::floor_date(date, unit = "month")
      ) %>%
      dplyr::group_by(name, month_id) %>%
      dplyr::summarize(rv_month = sum(rv_daily, na.rm = TRUE), .groups = "drop")


  ## Add the market series as one more key

    ### The model is trained on the pooled panel WITH the market series present,
    ### so its out-of-sample predictions fall out of the ordinary walk-forward
    ### loop. This is pooled learning that is still strictly time-OOS, not
    ### transfer from a factors-only model.
    ### spx_vw is CRSP vwretd, the series this repository labels S&P 500.
    spx_blocks <- purrr::reduce(sp500_blocks, dplyr::full_join,
                                by = "month_id") %>%
      dplyr::mutate(name = "spx_vw", .before = 1)

    spx_target <- crsp_monthly %>%
      dplyr::mutate(name = "spx_vw", .before = 1)

    panel_blocks <- dplyr::bind_rows(spx_blocks, factor_blocks)

    ### Sorted by month first: the walk-forward engine requires month_id to be
    ### non-decreasing so that every key of a given month is contiguous and
    ### therefore lands in the same train/validation/test block.
    panel_target <- dplyr::bind_rows(spx_target, factor_target) %>%
      dplyr::arrange(month_id, name)


  ## Minimum-history inclusion rule (unbalanced panel)

    ### Keys enter as soon as they can support the HAR block and then stay, so
    ### the pre-1951 history of the long-running factors is preserved.
    ### Two filters, for two different reasons:
    ###  1. the first two months of any key are structurally unusable, because
    ###     month t's terms are lags of month t-1 and the jump/continuous block
    ###     needs a 22-day rolling window plus a one-day lag before it exists;
    ###  2. the block must then be complete, so that the LOCF step inside
    ###     prep_covariates() never has to invent a value at the very start of a
    ###     key's history.
    har_block_cols <- setdiff(names(panel_blocks), c("name", "month_id"))

    panel_blocks_trimmed <- panel_blocks %>%
      dplyr::group_by(name) %>%
      dplyr::arrange(month_id, .by_group = TRUE) %>%
      dplyr::filter(dplyr::row_number() > 2) %>%
      dplyr::ungroup()

    panel_blocks <- panel_blocks_trimmed[
      stats::complete.cases(panel_blocks_trimmed[, har_block_cols]),
    ]


  ## Join the shared covariates onto every key

    ### The shared blocks (macro, implied vol, calendar, oil) are calendar-time
    ### series, so they are the same for every key in a given month. Only the
    ### S&P-specific HAR columns are dropped before the join, because each key
    ### carries its own.
    sp_block_cols <- unlist(
      lapply(sp500_blocks, function(x) setdiff(names(x), "month_id")),
      use.names = FALSE
    )

    ### covariates_*_df are already restricted to their tier window, so the
    ### inner join applies the sample window as well as the covariate set.
    panel_1986_df <- panel_blocks %>%
      dplyr::inner_join(
        covariates_1986_df[, setdiff(names(covariates_1986_df), sp_block_cols)],
        by = "month_id"
      ) %>%
      dplyr::arrange(month_id, name)

    panel_1927_df <- panel_blocks %>%
      dplyr::inner_join(
        covariates_1927_df[, setdiff(names(covariates_1927_df), sp_block_cols)],
        by = "month_id"
      ) %>%
      dplyr::arrange(month_id, name)

    cat("\nPanel 1986:", nrow(panel_1986_df), "rows,",
        dplyr::n_distinct(panel_1986_df$name), "keys,",
        format(min(panel_1986_df$month_id)), "to",
        format(max(panel_1986_df$month_id)), "\n")
    cat("Panel 1927:", nrow(panel_1927_df), "rows,",
        dplyr::n_distinct(panel_1927_df$name), "keys,",
        format(min(panel_1927_df$month_id)), "to",
        format(max(panel_1927_df$month_id)), "\n")


  ## Preprocessing

    ### key_col makes every expanding statistic run within key. Winsorization
    ### ranges match the established convention: 0.01/0.99 for the z-score
    ### specification, 0.025/0.975 for the range specification.
    panel_1986_zscore <- prep_covariates(panel_1986_df, scaling = "zscore",
                                         key_col = "name")
    panel_1986_range  <- prep_covariates(panel_1986_df, scaling = "range",
                                         winsorize_range = c(0.025, 0.975),
                                         key_col = "name")
    panel_1927_zscore <- prep_covariates(panel_1927_df, scaling = "zscore",
                                         key_col = "name")
    panel_1927_range  <- prep_covariates(panel_1927_df, scaling = "range",
                                         winsorize_range = c(0.025, 0.975),
                                         key_col = "name")

    ### S&P-only versions, emitted from this same build so that the optional
    ### S&P-exclusive runs share the feature ordering and hashes recorded below
    spx_1986_zscore <- prep_covariates(covariates_1986_df, scaling = "zscore")
    spx_1986_range  <- prep_covariates(covariates_1986_df, scaling = "range",
                                       winsorize_range = c(0.025, 0.975))
    spx_1927_zscore <- prep_covariates(covariates_1927_df, scaling = "zscore")
    spx_1927_range  <- prep_covariates(covariates_1927_df, scaling = "range",
                                       winsorize_range = c(0.025, 0.975))


  ## Leakage check: the market key must be untouched by the factors

    ### Because preprocessing runs within key, the spx_vw rows of the panel must
    ### reproduce the standalone S&P frame exactly. If any factor's history ever
    ### reaches the market series, this fails. Compared on shared columns only,
    ### since the panel additionally carries the `name` key.
    check_spx_isolated <- function(panel_prepped, spx_prepped) {
      shared <- intersect(names(panel_prepped), names(spx_prepped))
      lhs <- panel_prepped %>%
        dplyr::filter(name == "spx_vw") %>%
        dplyr::arrange(month_id) %>%
        dplyr::select(dplyr::all_of(shared))
      rhs <- spx_prepped %>%
        dplyr::arrange(month_id) %>%
        dplyr::select(dplyr::all_of(shared))
      isTRUE(all.equal(as.data.frame(lhs), as.data.frame(rhs)))
    }

    stopifnot(
      check_spx_isolated(panel_1986_zscore, spx_1986_zscore),
      check_spx_isolated(panel_1986_range,  spx_1986_range),
      check_spx_isolated(panel_1927_zscore, spx_1927_zscore),
      check_spx_isolated(panel_1927_range,  spx_1927_range)
    )

    cat("\nLeakage check passed: spx_vw rows match the standalone S&P frames.\n")


  ## Save the v3 artifacts

    ### Targets are stored RAW. The Box-Cox transform is a modelling choice, so
    ### it is owned by the run specification in fit_models.R and recorded in the
    ### provenance of each fitted object rather than baked into the dataset.
    v3_outputs <- list(
      "covariates_panel_1986_v3.rds"        = panel_1986_df,
      "covariates_panel_1986_zscore_v3.rds" = panel_1986_zscore,
      "covariates_panel_1986_range_v3.rds"  = panel_1986_range,
      "covariates_panel_1927_v3.rds"        = panel_1927_df,
      "covariates_panel_1927_zscore_v3.rds" = panel_1927_zscore,
      "covariates_panel_1927_range_v3.rds"  = panel_1927_range,
      "covariates_spx_1986_zscore_v3.rds"   = spx_1986_zscore,
      "covariates_spx_1986_range_v3.rds"    = spx_1986_range,
      "covariates_spx_1927_zscore_v3.rds"   = spx_1927_zscore,
      "covariates_spx_1927_range_v3.rds"    = spx_1927_range,
      "target_panel_v3.rds"                 = panel_target,
      "target_spx_v3.rds"                   = crsp_monthly
    )

    for (v3_name in names(v3_outputs)) {
      saveRDS(v3_outputs[[v3_name]],
              file = file.path(here::here(), "data", v3_name))
    }


  ## Manifest

    ### Hashes are taken from the files on disk after writing, so the manifest
    ### describes what was actually produced. No timestamp is recorded, which
    ### keeps the manifest content-deterministic and therefore diffable.
    manifest_outputs <- dplyr::bind_rows(
      purrr::imap(v3_outputs, function(obj, v3_name) {
      tibble::tibble(
        role     = "output",
        artifact = v3_name,
        sha256   = digest::digest(
          file = file.path(here::here(), "data", v3_name), algo = "sha256"
        ),
        n_rows   = nrow(obj),
        n_cols   = ncol(obj),
        n_keys   = if ("name" %in% names(obj)) dplyr::n_distinct(obj$name) else 1L,
        date_min = as.character(min(obj$month_id, na.rm = TRUE)),
        date_max = as.character(max(obj$month_id, na.rm = TRUE)),
        notes    = NA_character_
      )
      })
    )

    manifest_inputs <- tibble::tibble(
      role     = "input",
      artifact = c(basename(jkp_path), "crsp.csv"),
      sha256   = c(
        digest::digest(file = jkp_path, algo = "sha256"),
        digest::digest(file = file.path(here::here(), "data", "crsp.csv"),
                       algo = "sha256")
      ),
      n_rows   = NA_integer_,
      n_cols   = NA_integer_,
      n_keys   = NA_integer_,
      date_min = NA_character_,
      date_max = NA_character_,
      notes    = NA_character_
    )

    manifest_policy <- tibble::tibble(
      role     = "policy",
      artifact = c("universe", "direction", "min_history", "preprocessing",
                   "target_transform", "market_key", "feature_key"),
      sha256   = NA_character_,
      n_rows   = NA_integer_,
      n_cols   = NA_integer_,
      n_keys   = NA_integer_,
      date_min = NA_character_,
      date_max = NA_character_,
      notes    = c(
        "usa, all JKP vw_cap factors; industries excluded; market series pooled in",
        "JKP ret is already sign-adjusted upstream; direction recorded, never re-applied",
        "each key drops its first 2 months, then requires a complete HAR block",
        "expanding, within key; zscore winsorized 0.01/0.99, range 0.025/0.975, min_n=36",
        "none; the Box-Cox transform is applied by the fit_models.R run specification",
        "spx_vw = CRSP vwretd, the series this repository labels S&P 500",
        "`name` is a relational key only and must never enter the design matrix"
      )
    )

    manifest_v3 <- dplyr::bind_rows(manifest_inputs, manifest_outputs,
                                    manifest_policy)

    utils::write.csv(
      manifest_v3,
      file = file.path(here::here(), "data", "manifest_v3.csv"),
      row.names = FALSE
    )

  ## Save legacy artifacts only after the full panel build and its checks pass.
  saveRDS(covariates_1927_df,
          file = file.path(here::here(), "data", "covariates_1927v2.rds"))
  saveRDS(covariates_1962_df,
          file = file.path(here::here(), "data", "covariates_1962v2.rds"))
  saveRDS(covariates_1986_df,
          file = file.path(here::here(), "data", "covariates_1986v2.rds"))
  saveRDS(crsp_monthly,
          file = file.path(here::here(), "data", "crsp_monthly.rds"))

    cat("\nWrote", length(v3_outputs), "v3 artifacts and data/manifest_v3.csv\n")
    
    
