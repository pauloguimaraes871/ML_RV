######################
##  RV Predictions  ##
######################


# Initial Setup------------------------------------------------------------------
renv::install("dplyr")
renv::install("lubridate")
library("dplyr")

  ## Load
  crsp <- read.csv(file = file.path(here::here(), "data", "crsp.csv")) %>%
                     as_tibble()
  
  ## Paste caldt into date
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
  
    ### Lagged RV 
    rv <- generate_HAR_terms(
      data = crsp %>% dplyr::mutate(rv = vwretd ^ 2),
      date_col = caldt,
      value_col = "rv",
      prefix = NULL,
      week_len = 5,
      month_trading_days = 22,
      scale_to_month = TRUE,
      month_agg = "sum"
    )
  
    ### Realized Quarticity (Bollerslev et al. (2016))
    rq <- generate_HAR_terms(
      data = crsp %>% dplyr::mutate(rq = (vwretd ^ 4)/3),
      date_col = caldt,
      value_col = "rq",
      prefix = NULL,
      week_len = 5,
      month_trading_days = 22,
      scale_to_month = TRUE,
      month_agg = "mean"
    )
  
     
    ### Jump and Continuous Comp (Anderson et al. (2007); Bekaert-Horoeva (2014)
    jc <- crsp %>%
      dplyr::arrange(caldt) %>%
      dplyr::mutate(
        r         = vwretd,             # daily return in decimal
        rv        = r^2,                # daily realized variance proxy
        abs_ret   = abs(r)              # absolute daily return
      ) %>%
      ### Local variance estimate Vhat_t -> 22-day rolling MEAN of past daily RVs
      dplyr::mutate(
        vhat_raw = as.numeric(stats::filter(rv, rep(1/22, 22), sides = 1)),
        vhat = dplyr::lag(vhat_raw, 1), #Lag by 1 day
        theta_t = 3^2 * vhat #Threshold c^2 * Vhat_t
      ) %>%
      ### Bipower variation (BPV) and Jump variation (J) 
      ### TBPV analog: keep |r_t||r_{t-1}| only if both RVs <= theta_t
      ### TBPV_t := mu1^{-2} * |r_t||r_{t-1}| * I(RV_t<=theta_t) * 
      ### I(RV_{t-1}<=theta_{t-1})
      ### This is equation 2.14 of Corsi-Pirino-Renò (2010)
      dplyr::mutate(
        abs_ret_lag1 = dplyr::lag(abs_ret, 1),
        rv_lag1      = dplyr::lag(rv, 1),
        theta_lag1   = dplyr::lag(theta_t, 1),
        #### Indicator RV_t <= theta_t
        ind_t        = !is.na(theta_t)   & (rv <= theta_t),
        ind_lag1     = !is.na(theta_lag1) & (rv_lag1  <= theta_lag1),
        keep_pair    = ind_t & ind_lag1,
        mu1          = sqrt(2/pi),
        #### Pseudo TBPV
        pseudo_tbpv  = (abs_ret * abs_ret_lag1) / (mu1 ^ 2),
        pseudo_tbpv  = dplyr::if_else(keep_pair, pseudo_tbpv, NA_real_),
        #### Daily TBPV with fallback so TBPV does not exceed RV
        tbvp         = dplyr::coalesce(pseudo_tbpv, pmin(rv, theta_t)),
        #### Define J and C as Bekaert and Horoeva (2014)
        j            = pmax(rv - tbvp, 0),
        c            =  rv - j
      ) %>%
      dplyr::select(caldt, j, c)
    
      #### jump
      jump <- generate_HAR_terms(
        data = jc %>% dplyr::rename(jump = j),
        date_col = caldt,
        value_col = "jump",
        prefix = NULL,
        week_len = 5,
        month_trading_days = 22,
        scale_to_month = TRUE,
        month_agg = "mean"
      )
      
      #### continuos
      cont <- generate_HAR_terms(
        data = jc %>% dplyr::rename(cont = c),
        date_col = caldt,
        value_col = "cont",
        prefix = NULL,
        week_len = 5,
        month_trading_days = 22,
        scale_to_month = TRUE,
        month_agg = "mean"
      )
    
    ### Leverage (Campbell & Hentschel (1992); Bekaert & Hoerova (2014))
    negret <- generate_HAR_terms(
      data = crsp,
      date_col = caldt,
      value_col = "vwretd",
      prefix = "r",
      week_len = 5,
      month_trading_days = 22,
      scale_to_month = TRUE,
      month_agg = "sum"
    ) %>%
      dplyr::transmute(
        month_id,
        negret_month_lag1 = pmin(r_month_lag1, 0),
        negret_tw_lag1    = pmin(r_tw_lag1,    0),
        negret_td_lag1    = pmin(r_td_lag1,    0)
      )
    
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
    dplyr::arrange(
      month_id
    ) %>%
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
                               lubridate::month(dates), "01", sep = "-")),
      dfr_lag1 = dplyr::lag(dfr, 1)
    ) %>%
    dplyr::arrange(month_id) %>%
    dplyr::select(month_id, dfr_lag1)
  
  ## Default Spread (Paye 2012) - Not Significant
  ## https://fred.stlouisfed.org/series/BAA10YM
  dfy <- readxl::read_excel(file.path(here::here(), "data", "DFY.xlsx"),
                            sheet = "Monthly", range = "A1:B870", 
                            col_names = TRUE) %>%
    dplyr::rename(dfy = 2, dates = 1) %>%
    dplyr::mutate(
     month_id = as.Date(paste(lubridate::year(dates),
                              lubridate::month(dates), "01", sep = "-")),
     dfy_lag1 = dplyr::lag(dfy, 1)
    ) %>%
    dplyr::arrange(month_id) %>%
    dplyr::select(month_id, dfy_lag1)
    
  ## Term Spread (Paye 2012) - Not Significant
  ## https://fred.stlouisfed.org/series/T10YFFM
  tms <- readxl::read_excel(file.path(here::here(), "data", "TMS.xlsx"),
                            sheet = "Monthly", range = "A1:B855", 
                            col_names = TRUE) %>%
    dplyr::rename(tms = 2, dates = 1) %>%
    dplyr::mutate(
      month_id = as.Date(paste(lubridate::year(dates),
                               lubridate::month(dates), "01", sep = "-")),
      tms_lag1 = dplyr::lag(tms, 1)
    ) %>%
    dplyr::arrange(month_id) %>%
    dplyr::select(month_id, tms_lag1)
    
  ## PPI Vol (Schwert (1989), Paye (2012))
  ## https://fred.stlouisfed.org/series/PPIACO (Non-Seas Adj)
  ppi <- readxl::read_excel(file.path(here::here(), "data", "PPI.xlsx"),
                           sheet = "Monthly", range = "A1:B1353", 
                           col_names = TRUE) %>%
    dplyr::rename(ppi = 2, dates = 1) %>%
    dplyr::mutate(
      month_id   = as.Date(paste(lubridate::year(dates),
                                 lubridate::month(dates), "01", sep = "-")),
      ppi_growth = 100 * (log(ppi) - log(dplyr::lag(ppi)))
    ) %>%
    dplyr::arrange(month_id)
    
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
                                 lubridate::month(dates), "01", sep = "-")),
        ip_growth = 100 * (log(indpro) - log(dplyr::lag(indpro)))
      ) %>%
      dplyr::arrange(month_id)
    
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
                                   lubridate::month(dates), "01", sep = "-")),
        mbas_growth = 100 * (log(mbas) - log(dplyr::lag(mbas)))
      ) %>%
      dplyr::arrange(month_id)  
    
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
                               lubridate::month(dates), "01", sep = "-")),
      dy_lag1  = dplyr::lag(dy, 1)
    ) %>%
    dplyr::arrange(month_id) %>%
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
                               lubridate::month(dates), "01", sep = "-")),
      ey_lag1  = dplyr::lag(ey, 1)
    ) %>%
    dplyr::arrange(month_id) %>%
    dplyr::select(month_id, ey_lag1)
  
  ## T-bill (Audrino et al. (2020))
  ## https://fred.stlouisfed.org/series/TB3MS
  tbill <- readxl::read_excel(file.path(here::here(), "data", "TBILL.xlsx"),
                              sheet = "Monthly", range = "A1:B1101",
                              col_names = TRUE) %>%
    dplyr::rename(tbill = 2, dates = 1) %>%
    dplyr::mutate(
      month_id   = as.Date(paste(lubridate::year(dates),
                                 lubridate::month(dates), "01", sep = "-")),
      tbill_lag1 = dplyr::lag(tbill, 1)
    ) %>%
    dplyr::arrange(month_id) %>%
    dplyr::select(month_id, tbill_lag1)
  
  ## Consumer Sentiment (Audrino et al. (2020))
  ## https://data.sca.isr.umich.edu/data-archive/mine.php
  cons_sent <- utils::read.csv(file.path(here::here(), "data", "CSENT.csv"),
                               skip = 1 ) %>%
    tibble::as_tibble() %>%
    dplyr::rename(cons_sent = 3, month = 1, year = 2) %>%
    dplyr::mutate(
      month_id           = as.Date(paste(year, month, "01", sep = "-")),
      log_cons_sent_diff = dplyr::if_else(dplyr::row_number() == 1, NA_real_,
                                         log(cons_sent) - 
                                           log(dplyr::lag(cons_sent))),
      log_cons_sent_diff_lag1 = dplyr::lag(log_cons_sent_diff, 1)
    ) %>%
    dplyr::arrange(month_id) %>%
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
                                    gpr),
      log_gpr_diff = dplyr::if_else(dplyr::row_number() == 1, NA_real_,
                                         log(gpr) - log(dplyr::lag(gpr))),
      ### Lagged
      gpr_lag1 = dplyr::lag(log_gpr_diff, 1)
    ) %>%
    dplyr::arrange(month_id) %>%
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
      oil     = dplyr::if_else(oil <= 0, NA_real_, oil),
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
      vix      = ( (vix/100)^2 ) / 12, #Follows Bekaert & Hoerova (2014)
      vix_lag1 = dplyr::lag(vix, 1)
    ) %>%
    ### Agg to month
    dplyr::group_by(month_id) %>%
    dplyr::filter(dates == max(dates)) %>% 
    dplyr::arrange(month_id)
  
    #### Optional: Expand with VXO (for more 4 years of data)
    vxo <- readxl::read_excel(file.path(here::here(),"data", "VXO.xls"),
                              sheet = "Sheet1", range = "A3:E3535") %>%
      tibble::as_tibble() %>%
      dplyr::rename(vxo = 5, dates = 1) %>%
      dplyr::select(dates, vxo) %>% 
      dplyr::mutate(
        month_id = as.Date(paste(lubridate::year(dates),
                                 lubridate::month(dates), "01", sep = "-")),
        vxo      = ( (vxo/100)^2 ) / 12, #Follows Bekaert & Hoerova (2014)
        vxo_lag1 = dplyr::lag(vxo, 1)
      ) %>%
      ### Agg to month
      dplyr::group_by(month_id) %>%
      dplyr::filter(dates == max(dates)) %>% 
      dplyr::arrange(month_id)
    
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
    
    
  ## Save objects to data folder
  saveRDS(covariates_1927_df, file = file.path(here::here(), "data", "covariates_1927.rds"))
  saveRDS(covariates_1962_df, file = file.path(here::here(), "data", "covariates_1962.rds"))
  saveRDS(covariates_1986_df, file = file.path(here::here(), "data", "covariates_1986.rds"))
  saveRDS(crsp_monthly,       file = file.path(here::here(), "data", "crsp_monthly.rds"))
    
    
    