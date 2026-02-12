## Helper function to generate HAR-RV-like terms
## (Bekaert et al. (2025) HAR-style model from Corsi (2009))
generate_HAR_terms <- function(
    data,                        # input data frame (tibble)
    date_col,                    # Date column (Date class)
    value_col,                   # daily series to transform 
    prefix = NULL,               # prefix for output column names
    week_len = 5,                # number of trading days for the 'week' block
    month_trading_days = 22,     # scaling base (monthly units)
    scale_to_month = TRUE,       # if TRUE, apply 22/h scaling for week/day
    month_agg = c("sum", "mean") # how to aggregate within month 
){
  
  ### Get symbols/strings
  month_agg <- match.arg(month_agg)
  date_sym  <- rlang::ensym(date_col)
  val_sym   <- rlang::ensym(value_col)
  base_name <- if (is.null(prefix)) rlang::as_string(val_sym) else prefix
  
  ### Create time identifier
  data <- data %>%
    dplyr::mutate(
      month_id = lubridate::floor_date(!!date_sym, unit = "month")
    ) %>%
    dplyr::arrange(!!date_sym)
  
  ### Monthly aggregation
  monthly_agg <- data %>%
    dplyr::group_by(month_id) %>%
    { if (month_agg == "sum"){
      #### Summarize by sum
      dplyr::summarize(., agg_month = sum(!!val_sym, na.rm = TRUE), 
                       .groups = "drop")
    } else {
      #### Summarize by mean
      dplyr::summarize(., agg_month = mean(!!val_sym, na.rm = TRUE), 
                       .groups = "drop")
    }
    } %>%
    #### Arrange by month
    dplyr::arrange(month_id) %>%
    #### Get lagged value
    dplyr::mutate("{paste0(base_name, '_month_lag1')}" := 
                    dplyr::lag(agg_month, 1)) %>%
    dplyr::select(month_id, dplyr::ends_with("_month_lag1"))
  
  ### Last week aggregation
  last_week_in_month <- data %>%
    dplyr::group_by(month_id) %>%
    #### Arrange by date, considering months
    dplyr::arrange(!!date_sym, .by_group = TRUE) %>%
    #### Identify days inside month
    dplyr::mutate(
      day_idx = dplyr::row_number(),
      n_days  = dplyr::n()
    ) %>% 
    #### Filter last week_len trading days
    dplyr::filter(day_idx > (n_days - week_len)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(month_id) %>%
    { if (month_agg == "sum"){
      #### Summarize by sum
      dplyr::summarize(., agg_last_week = sum(!!val_sym, na.rm = TRUE), 
                       .groups = "drop")
    } else {
      #### Summarize by mean
      dplyr::summarize(., agg_last_week = mean(!!val_sym, na.rm = TRUE), 
                       .groups = "drop")
    }
    } %>%
    #### Scale if asked
    dplyr::mutate(
      agg_last_week = if (scale_to_month) 
        agg_last_week * (month_trading_days / week_len) else agg_last_week
    ) %>%
    #### Arrange by month
    dplyr::arrange(month_id) %>%
    #### Get lagged value
    dplyr::mutate("{paste0(base_name, '_tw_lag1')}" := 
                    dplyr::lag(agg_last_week, 1)) %>%
    dplyr::select(month_id, dplyr::ends_with("_tw_lag1"))
  
  ### Last trading day aggregation
  last_day_in_month <- data %>%
    dplyr::group_by(month_id) %>%
    #### Filter max date per month
    dplyr::filter(!!date_sym == max(!!date_sym)) %>%
    dplyr::ungroup() %>%
    #### Scale if asked
    dplyr::mutate(
      agg_last_day = if (scale_to_month) 
        !!val_sym * month_trading_days else !!val_sym
    ) %>%
    dplyr::arrange(month_id) %>%
    #### Get lagged value
    dplyr::mutate("{paste0(base_name, '_td_lag1')}" := 
                    dplyr::lag(agg_last_day, 1)) %>%
    dplyr::select(month_id, dplyr::ends_with("_td_lag1"))
  
  ### Merge all
  design <- monthly_agg %>%
    dplyr::left_join(last_week_in_month, by = "month_id") %>%
    dplyr::left_join(last_day_in_month, by = "month_id")
  
  ### Return
  return(design)
  
}


## Helper function to calculate vol as in Schwert (1989), without
## introducting forward-looking bias
schwert_vol_oos <- function(data,
                            date_col    = "dates", # Date class
                            value_col   = "value", # numeric
                            p           = 12,      # AR order
                            start_year,            # first year to consider
                            annualize   = TRUE     # if TRUE, mult by sqrt(12)
){
  
  ### Check
  stopifnot(all(c(date_col, value_col) %in% names(data)))
  
  ### Create month dummies
  d <- data %>% 
    dplyr::arrange(!!rlang::ensym(date_col)) %>%
    dplyr::mutate(
      year        = lubridate::year(!!rlang::ensym(date_col)),
      month       = lubridate::month(!!rlang::ensym(date_col)),
      month_dummy = factor(month, levels = 1:12)
    )
  
  ### Build lags
  lag_names <- paste0("L", 1:p)
  for (k in seq_len(p)) {
    d[[lag_names[k]]] <- dplyr::lag(d[[value_col]], k)
  }
  
  ### Set OOS years
  years_oos <- sort(unique(d$year[d$year > start_year]))
  out_list <- vector("list", length(years_oos))
  
  ### Helper to build absolute residual lag row from a vector of past abs res
  build_abs_e_lags <- function(abs_e_hist, p){
    
    #### Initialize
    v <- rep(NA_real_, p)
    n <- length(abs_e_hist)
    
    #### Iterate
    for (k in seq_len(p)){
      
      ##### For each lag, get the appropriate index
      idx <- n - (k - 1)
      v[k] <- if (idx > 0) abs_e_hist[idx] else NA_real_
      
    }
    
    #### Return
    out <- as.list(v)
    names(out) <- paste0("AE_L", seq_len(p))
    out
    
    
  }
  
  ### Loop over years
  for (i in seq_along(years_oos)){
    
    #### Get train and test sets 
    y <- years_oos[i]
    tr <- d %>% dplyr::filter(year < y)
    te <- d %>% dplyr::filter(year == y)
    
    #### Step (i) - Equation 3a
    ##### Keep rows with complete lags
    tr_i  <- tr %>% dplyr::filter(dplyr::if_all(dplyr::all_of(lag_names),
                                                ~ !is.na(.)))
    te_i  <- te %>% dplyr::filter(dplyr::if_all(dplyr::all_of(lag_names),
                                                ~ !is.na(.)))
    
    if (nrow(tr_i) == 0L || nrow(te_i) == 0L) next
    
    ##### Fit model from step (i)
    fml_i <-  stats::as.formula(
      paste(value_col, "~", paste(c(lag_names, "month_dummy"),
                                  collapse = " + "))
    )
    fit_i <- stats::lm(fml_i, data = tr_i)
    
    ##### Predict and get residuals 
    tr_i$pred_i <- stats::fitted(fit_i)
    tr_i$e      <- tr_i[[value_col]] - tr_i$pred_i
    tr_i$ae     <- abs(tr_i$e)
    ae_constant <- !base::any(tr_i$ae != 0, na.rm = TRUE)
    
    #### Step (ii) - Equation 3b  
    ##### Create lags of abs residuals
    tr_i <- tr_i %>% dplyr::arrange(!!rlang::ensym(date_col))
    ae_lag_names <- paste0("AE_L", seq_len(p))
    
    for (k in seq_len(p)) {
      tr_i[[ae_lag_names[k]]] <- dplyr::lag(tr_i$ae, k)
    }
    
    ##### Keep rows with complete lags
    tr_ii <- tr_i %>% dplyr::filter(
      dplyr::if_all(dplyr::all_of(ae_lag_names), ~ !is.na(.))
    )
    if (nrow(tr_ii) == 0L) next
    
    ##### Fit model from step (ii)
    fml_ii <-  stats::as.formula(
      paste("ae ~", paste(c(ae_lag_names, "month_dummy"),
                          collapse = " + "))
    )
    fit_ii <- stats::lm(fml_ii, data = tr_ii)
    
    #### Step (iii) - Predict for test set
    te_i <- te_i %>% dplyr::arrange(!!rlang::ensym(date_col))  
    
    #### Match test month factor levels to training
    md_levels <- base::levels(tr_i$month_dummy)
    te_i$month_dummy <- base::factor(te_i$month_dummy, levels = md_levels)
    
    ##### Create abs residual lags for test set
    abs_e_hist <- tr_i %>% 
      dplyr::arrange(!!rlang::ensym(date_col)) %>%
      dplyr::pull(ae)
    abs_e_hist <- abs_e_hist[!is.na(abs_e_hist)]
    
    ##### Predict
    te_i$res_i     <- NA_real_  # step (i) residuals (realized) 
    te_i$pred_i    <- NA_real_  # step (i) prediction
    te_i$sigma_hat <- NA_real_  # step (iii) conditional std dev (the target)
    
    for (row in seq_len(nrow(te_i))){
      
      #### Fallback: if month level wasn't seen in training, use the baseline level
      if (base::is.na(te_i$month_dummy[row])) {
        te_i$month_dummy[row] <- md_levels[1]
      }
      
      #### Build regressors for step (ii) using history
      ae_lags_row <- build_abs_e_lags(abs_e_hist, p)
      #### If any AE lag is NA (e.g., very early sample), skip this month
      if (any(is.na(unlist(ae_lags_row)))) next
      
      #### Map test month to levels actually seen by each model
      m_i  <- base::factor(te_i$month_dummy[row], levels = fit_i$xlevels$month_dummy)
      if (base::is.na(m_i))  m_i  <- base::factor(fit_i$xlevels$month_dummy[1],
                                                  levels = fit_i$xlevels$month_dummy)
      
      m_ii <- base::factor(te_i$month_dummy[row], levels = fit_ii$xlevels$month_dummy)
      if (base::is.na(m_ii)) m_ii <- base::factor(fit_ii$xlevels$month_dummy[1],
                                                  levels = fit_ii$xlevels$month_dummy)
      
      #### Add month dummy
      new_ii <- c(
        list(month_dummy = m_ii),
        ae_lags_row
      ) %>% tibble::as_tibble()
      
      
      #### Predict conditional std dev
      if (ae_constant) {
        te_i$sigma_hat[row] <- 0                           
      } else {
        te_i$sigma_hat[row] <- base::suppressWarnings(
          stats::predict(fit_ii, newdata = new_ii)
        )
      }
      
      #### Predict value using step (i) model to roll forward
      te_row <- te_i[row, ]
      te_row$month_dummy <- m_i
      te_i$pred_i[row]   <- base::suppressWarnings(
        stats::predict(fit_i,  newdata = te_row)
      )
      te_i$res_i[row]    <- te_i[[value_col]][row] - te_i$pred_i[row]
      
      #### Update abs_e_hist with realized abs residual
      if (!base::is.na(te_i$res_i[row])) {
        abs_e_hist <- c(abs_e_hist, base::abs(te_i$res_i[row]))
      }
      
    }
    
    ##### Store results
    out_list[[i]] <- te_i %>%
      dplyr::transmute(
        !!date_col := .data[[date_col]],
        sigma_month = pmax(sigma_hat, .Machine$double.eps), # guard
        sigma_ann   = if (annualize) sigma_month * sqrt(12) else NA_real_,
        res_i       = res_i                
      )
    
  }
  
  ### Bind
  oos <- dplyr::bind_rows(out_list)
  if (nrow(oos) == 0L) return(oos)
  
  ### Return 
  oos
  
} 


### Helper for summary tables
make_var_summary <- function(df, date_col = "month_id", digits = 4) {
  stopifnot(date_col %in% names(df))
  
  vars_num <- names(df)[vapply(df, is.numeric, logical(1L))]
  vars_num <- setdiff(vars_num, date_col)  # keep numeric covariates only
  
  purrr::map_dfr(vars_num, function(v) {
    x     <- df[[v]]
    dates <- df[[date_col]]
    ok    <- !is.na(x)
    
    n_obs   <- sum(ok)
    n_miss  <- sum(!ok)
    start_d <- if (n_obs > 0) min(dates[ok]) else as.Date(NA)
    end_d   <- if (n_obs > 0) max(dates[ok]) else as.Date(NA)
    span_yr <- if (n_obs > 0) as.numeric(difftime(end_d, start_d, units = "days"))/365.25 else NA_real_
    
    acf1 <- NA_real_
    if (sum(ok) >= 2) {
      acf1 <- stats::acf(x[ok], lag.max = 1, plot = FALSE, na.action = stats::na.omit)$acf[2]
    }
    
    tibble::tibble(
      variable = v,
      start    = start_d,
      end      = end_d,
      span_yrs = span_yr,
      n        = n_obs,
      n_na     = n_miss,
      mean     = mean(x, na.rm = TRUE),
      sd       = stats::sd(x, na.rm = TRUE),
      p10      = stats::quantile(x, 0.10, na.rm = TRUE, names = FALSE),
      p50      = stats::quantile(x, 0.50, na.rm = TRUE, names = FALSE),
      p90      = stats::quantile(x, 0.90, na.rm = TRUE, names = FALSE),
      min      = suppressWarnings(min(x, na.rm = TRUE)),
      max      = suppressWarnings(max(x, na.rm = TRUE)),
      acf1     = acf1
    )
  }) %>%
    dplyr::arrange(variable) %>%
    gt::gt() %>%
    gt::tab_header(
      title = "Summary Statistics of Covariates",
      subtitle = "Monthly sample; per-series effective sample periods"
    ) %>%
    gt::fmt_date(columns = c(start, end), date_style = 3) %>%
    gt::fmt_number(columns = c(span_yrs, mean, sd, p10, p50, p90, min, max, acf1),
                   decimals = digits) %>%
    gt::fmt_number(columns = c(n, n_na), decimals = 0, use_seps = TRUE) %>%
    gt::cols_label(
      variable = "Series",
      start = "Start",
      end = "End",
      span_yrs = "Span (yrs)",
      n = "N",
      n_na = "N miss",
      mean = "Mean",
      sd = "SD",
      p10 = "P10",
      p50 = "Median",
      p90 = "P90",
      min = "Min",
      max = "Max",
      acf1 = "ACF(1)"
    ) %>%
    gt::tab_options(
      table.font.names = "Helvetica",
      data_row.padding = gt::px(2),
      table.border.top.width = gt::px(0),
      table.border.bottom.width = gt::px(0)
    ) %>%
    gt::opt_table_outline()
}

### Helper for faceted time series plots
plot_ts_grid <- function(df, vars, date_col = "month_id",
                         ncol = 3, free_y = TRUE, smooth = TRUE, 
                         title = NULL,
                         smooth_span = 0.15) {
  stopifnot(date_col %in% names(df))
  if (length(vars) == 0L) stop("No variables provided to plot_ts_grid().")
  
  dd <- df %>%
    dplyr::select(dplyr::all_of(c(date_col, vars))) %>%
    tidyr::pivot_longer(cols = -dplyr::all_of(date_col),
                        names_to = "series", values_to = "value") %>%
    dplyr::mutate(series = factor(series, levels = sort(unique(series))))
  
  p <- ggplot2::ggplot(dd, ggplot2::aes(x = .data[[date_col]], y = value)) +
    ggplot2::geom_line(linewidth = 0.4, alpha = 0.9) +
    { if (smooth) ggplot2::geom_smooth(method = "loess", se = FALSE, span = smooth_span, linewidth = 0.5) } +
    ggplot2::facet_wrap(~ series, scales = if (free_y) "free_y" else "fixed", ncol = ncol) +
    ggplot2::labs(x = NULL, y = NULL, title = title) +
    ggplot2::theme_minimal(base_family = "Helvetica") +
    ggplot2::theme(
      strip.text = ggplot2::element_text(face = "bold"),
      plot.title = ggplot2::element_text(face = "bold"),
      panel.grid.minor = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(size = 8)
    )
  p
}

### Helper: pick variable families automatically by prefix (rv_, rq_, jump_, cont_, negret_, etc.)
vars_by_prefix <- function(df, prefix) {
  stringr::str_subset(names(df),
                      paste0("^", stringr::str_replace(
                        prefix, "\\$", "\\\\$")))
}

## Imputation
impute_locf <- function(data){
  
  ### Check if imputation is needed
  if (any(is.na(data))){
    
    #### Which colnames have NAs?
    cols_with_na <- colnames(data)[colSums(is.na(data)) > 0]
    
    #### Print NAs
    print(paste("Imputing NAs for columns:", paste(cols_with_na, collapse = ", ")))
    
    #### Impute NAs using LOCF
    data_imputed <- data %>% 
      dplyr::arrange(month_id) %>%
      tidyr::fill(dplyr::all_of(cols_with_na), .direction = "down")
    
    #### Return data imputed and print dates that were filled
    for (na_col in cols_with_na){
      na_filled_dates <- data$month_id[is.na(data[[na_col]]) & !is.na(data_imputed[[na_col]])]
      if (length(na_filled_dates) > 0){
        print(paste("Filled NAs in column", na_col, "for dates:", paste(na_filled_dates, collapse = ", ")))
      }
    }
    
    return(data_imputed)
    
  } else {
    
    ### If no NAs, return original data
    print("No NAs found. No imputation needed.")
    return(data)
  }
  
  
}

## Winsorization
winsorize_expanding <- function(x, p_lo = 0.01, p_hi = 0.99,
                                min_n = 36L) {
  ### Exclude current obs
  x_lag <- dplyr::lag(x)
  
  ### Compute expanding quantiles
  #### Low
  q_lo <- slider::slide_dbl(
    x_lag,
    ~ if (sum(!is.na(.x)) >= min_n){
      stats::quantile(.x, p_lo, na.rm = TRUE, type = 8) 
    } else NA_real_,
    .before = Inf
  )
  
  #### High
  q_hi <- slider::slide_dbl(
    x_lag,
    ~ if (sum(!is.na(.x)) >= min_n){
      stats::quantile(.x, p_hi, na.rm = TRUE, type = 8) 
    } else NA_real_ ,
    .before = Inf
  )
  
  ### Winsorize
  dplyr::case_when(
    is.na(x) ~ NA_real_,
    TRUE     ~ pmin(pmax(x, q_lo, na.rm = TRUE),
                    q_hi, na.rm = TRUE)
  )
}
## Scale
scale_expanding     <- function(x, min_n = 36L,  mad_floor = 1e-2) {
  ### Exclude current obs
  x_lag <- dplyr::lag(x)
  
  ### Compute expanding median
  med <- slider::slide_dbl(
    x_lag,
    ~ if (sum(!is.na(.x)) >= min_n){
      stats::median(.x, na.rm = TRUE)
    } else NA_real_,
    .before = Inf
  )
  
  ### Compute expanding mad (with consistency factor for normal distribution)
  mad <- slider::slide_dbl(
    x_lag,
    ~ if (sum(!is.na(.x)) >= min_n){
      stats::mad(.x, constant = 1.4826, na.rm = TRUE)
    } else NA_real_,
    .before = Inf
  )
  
  ### Use floor when MAD is too small
  denom  <- pmax(mad, mad_floor)  # mad_floor is a fixed hyperparameter
  
  ### Scale
  scaled <- (x - med) / denom
  
  ### Return
  dplyr::if_else(is.na(med) | is.na(mad), x, scaled)
}

## Expanding min-max normalization to [-1, 1]
rescale_expanding_to_unit <- function(x, min_n = 36L) {
  out <- rep(NA_real_, length(x))
  
  for (i in seq_along(x)) {
    
    # Not enough history → return raw value (like scale_expanding)
    if (i <= min_n || sum(!is.na(x[1:(i-1)])) < min_n) {
      out[i] <- x[i]
      next
    }
    
    window <- x[1:(i-1)]  # exclude current observation (consistent with z-score)
    
    mn <- suppressWarnings(min(window, na.rm = TRUE))
    mx <- suppressWarnings(max(window, na.rm = TRUE))
    
    # Constant window: neutral value
    if (!is.finite(mn) || !is.finite(mx) || mx == mn) {
      out[i] <- 0  # fallback for constant window
    } else {
      out[i] <- 2*((x[i] - mn)/(mx - mn)) - 1
    }
  }
  out
}

## Box-Cox
### Fit Box-Cox lambda and offset on an initial training window
boxcox_fit   <- function(x, train_n, lower = -2, upper = 2, eps = 1e-8) {
  
  ### Fit lambda on an initial training window and freeze it
  x_train <- x[seq_len(train_n)]
  
  ### Single offset used everywhere so x + off > 0 on the training slice
  off <- max(0, eps - min(x_train, na.rm = TRUE)) + eps
  z   <- x_train + off
  
  ### Gaussian log-likelihood up to constants
  ll_fun <- function(lambda) {
    yt <- if (abs(lambda) < 1e-8) log(z) else (z^lambda - 1) / lambda
    n  <- sum(!is.na(yt))
    mu <- mean(yt, na.rm = TRUE)
    s2 <- mean((yt - mu)^2, na.rm = TRUE)
    jac <- (lambda - 1) * sum(log(z), na.rm = TRUE)  # Jacobian term
    - (n / 2) * log(s2) + jac
  }
  
  opt <- stats::optimize(function(l) -ll_fun(l), interval = c(lower, upper))
  lambda <- max(opt$minimum, 0)      # <- force lambda >= 0 (log if <= 0)
  list(lambda = lambda, offset = off)
}

### Apply the (lambda, offset) everywhere (past-only parameters)
boxcox_apply <- function(x, lambda, offset) {
  z <- x + offset
  dplyr::if_else(
    is.na(z),
    NA_real_,
    if (abs(lambda) < 1e-8) log(z) else (z^lambda - 1) / lambda
  )
}

### Back-transform predictions
boxcox_inv <- function(y, lambda, offset) {
  if (abs(lambda) < 1e-8) exp(y) - offset else (lambda*y + 1)^(1/lambda) - offset
}

## Validator
validator <- function(target, covariates, model,
                      hyper_grid_domain_list,
                      obj_fun, eval_metric,
                      train_n, val_n, rebal_months,
                      early_stop, gsm_algo,
                      keras_architecture_pars,
                      n_ensembles,
                      parallel
){
  
  ## Initial checks
  if (!model %in% c("har", "glmnet", "rf", "xgb", "nn", "lstm")){
    stop("Model not recognized. Please choose one of the following models: 
         'har', 'glmnet', 'rf', 'xgb', 'nn' or 'lstm'.")
  }
  if (!is.numeric(train_n) || train_n <= 0 || train_n != round(train_n)){
    stop("train_n must be a positive integer.")
  }
  if (!is.numeric(val_n) || val_n < 0 || val_n != round(val_n)){
    stop("val_n must be a positive integer.")
  }
  ## For har, val_n must be 0
  if (model == "har" && val_n != 0){
    stop("val_n must be 0 when model is 'har'.")
  }
  ## For others, val_n > 0
  if (model != "har" && val_n == 0){
    stop("val_n must be positive when model is not 'har'.")
  }
  if (!is.numeric(rebal_months) || any(rebal_months < 1) || 
      any(rebal_months > 12) || any(rebal_months != round(rebal_months))){
    stop("rebal_months must be an integer between 1 and 12.")
  }
  if (!is.character(obj_fun) || length(obj_fun) != 1){
    stop("obj_fun must be a single string.")
  }
  if (!is.character(eval_metric) || length(eval_metric) != 1){
    stop("eval_metric must be a single string.")
  }
  if (!is.null(early_stop)){
    if (!is.numeric(early_stop) || early_stop <= 0 || 
        early_stop != round(early_stop)){
      stop("early_stop must be a positive integer.")
    }
  }
  if (!is.list(hyper_grid_domain_list)) {
    stop("hyper_grid_domain_list must be a list (can be empty for 'har').")
  }
  if (model != "har" && length(hyper_grid_domain_list) == 0) {
    stop("hyper_grid_domain_list cannot be empty when model is not 'har'.")
  }
  if (model == "har" && length(hyper_grid_domain_list) > 0) {
    warning("hyper_grid_domain_list is ignored when model is 'har'.")
  }
  #GLMNET
  if(model == "glmnet" &&
     !all(names(hyper_grid_domain_list) == c("alpha", "lambda.min.ratio"))){
    stop("hyperparameters do not match model choice")
  }
  if(model == "glmnet"){
    #alpha
    ##########
    hyper_domain <- hyper_grid_domain_list$alpha
    
    #Check domain
    if(!all(0 <= hyper_domain, hyper_domain <= 1)){
      stop("alpha should be set in interval [0,1]")
    }
    ##########
    
    #lambda.min.ratio
    ##########
    hyper_domain <- hyper_grid_domain_list$lambda.min.ratio
    
    #Check domain
    if(!all(0 <= hyper_domain, hyper_domain < 1)){
      stop("lambda.min.ratio should be set in interval [0,1)")
    }
  }
  
  #RF
  if(model == "rf" && 
     !all(names(hyper_grid_domain_list) == c("mtry", "num.trees", 
                                             "max.depth", "min.bucket"))){
    stop("hyperparameters do not match model choice")
  }
  if(model == "rf"){
    #num.trees
    ##########
    hyper_domain <- hyper_grid_domain_list$num.trees

    #Check domain
    if(!all(is.integer(hyper_domain))){
        stop("num.trees should be integer")
      }
    if(!all(hyper_domain > 0)){
      stop("num.trees should be positive")
    }
    ##########
    
    #mtry
    ##########
    hyper_domain <- hyper_grid_domain_list$mtry
    
    #Check domain
    if(!all(0 <= hyper_domain, hyper_domain <= 1)){
      stop("mtry should be set in interval [0,1]")
    }
    ##########
    
    #max.depth
    ##########
    hyper_domain <- hyper_grid_domain_list$max.depth
    
    #Check domain
    if(!all(is.integer(hyper_domain))){
      stop("max.depth should be integer")
    }
    if(!all(hyper_domain > 0)){
      stop("max.depth should be positive")
    }
    ##########
    
  }
  
  #XGB
  if(model == "xgb" && 
     !all(names(hyper_grid_domain_list) == c("min_child_weight", "max_depth", 
                                             "subsample", "colsample_bytree", 
                                             "eta", "alpha", "gamma", "nrounds"))){
    stop("hyperparameters do not match model choice")
  }
  if(model == "xgb"){
    #eta
    ##########
    #bayesian opt or grid search
    hyper_domain <- hyper_grid_domain_list$eta
    
    #Check domain
    if(!all(0 <= hyper_domain, hyper_domain <= 1)){
      stop("eta should be set in interval [0,1]")
    }
    ##########
    
    #max_depth
    ##########
    hyper_domain <- hyper_grid_domain_list$max_depth
    
    #Check domain
    if(!all(is.integer(hyper_domain))){
      stop("max_depth should be integer")
      }
    
    if(!all(hyper_domain > 0)){
      stop("max_depth should be positive")
    }
    ##########
    
    #colsample_bytree
    ##########
    #bayesian opt or grid search
    hyper_domain <- hyper_grid_domain_list$colsample_bytree
    
    #Check domain
    if(!all(0 <= hyper_domain, hyper_domain <= 1)){
      stop("colsample_bytree should be set in interval [0,1]")
    }
    ##########
    
    #subsample
    ##########
    #bayesian opt or grid search
    hyper_domain <- hyper_grid_domain_list$subsample
    
    #Check domain
    if(!all(0 <= hyper_domain, hyper_domain <= 1)){
      stop("subsample should be set in interval [0,1]")
    }
    ##########
    
  }
  
  #NN
  if(model == "nn" && 
     !all(names(hyper_grid_domain_list) == c("regularizer_l1", "regularizer_l2",
                                             "droprate", "lr", "size_of_batch",
                                             "number_of_epochs"))){
    stop("hyperparameters do not match model choice")
  }
  if(model == "nn"){
    
    if (!is.numeric(n_ensembles) || length(n_ensembles) != 1L || is.na(n_ensembles)) {
      stop("`n_ensembles` must be a single non-missing numeric/integer value.", call. = FALSE)
    }
    n_ensembles <- as.integer(n_ensembles)
    if (n_ensembles < 1L) {
      stop("`n_ensembles` must be >= 1.", call. = FALSE)
    }
    
    #droprate
    ##########
    #bayesian opt or grid search
    hyper_domain <- hyper_grid_domain_list$droprate
    
    #Check domain
    if(!all(0 <= hyper_domain, hyper_domain < 1)){
      stop("droprate should be set in interval [0,1)")
    }
    ##########
    
    #number_of_epochs
    ##########
    #bayesian opt or grid search
    hyper_domain <- hyper_grid_domain_list$number_of_epochs
    
    #Check domain
    if(!all(is.integer(hyper_domain))){
     stop("number_of_epochs should be integer")
    }
    
    if(!all(hyper_domain > 0)){
      stop("number_of_epochs should be positive")
    }
    ##########
    
    #size_of_batch
    ##########
    #bayesian opt or grid search
    hyper_domain <- hyper_grid_domain_list$size_of_batch
    
    #Check domain
    if(!all(is.integer(hyper_domain))){
      stop("size_of_batch should be integer")
    }
    
    if(!all(hyper_domain > 0)){
      stop("size_of_batch should be positive")
    }
    ##########
  }
  
  #LSTM
  if(model == "lstm" && 
     !all(names(hyper_grid_domain_list) == c("regularizer_l2",
                                             "droprate","rec_droprate",
                                              "lr", "size_of_batch",
                                             "number_of_epochs"))){
    stop("hyperparameters do not match model choice")
  }
  if (model == "lstm") {

    #regularizer_l2
    ##########
    #bayesian opt or grid search
    hyper_domain <- hyper_grid_domain_list$regularizer_l2
    
    #Check domain
    if(!is.numeric(hyper_domain)){
      stop("regularizer_l2 should be numeric")
    }
    if(!all(hyper_domain >= 0)){
      stop("regularizer_l2 should be non-negative")
    }
    ##########
    
    #droprate
    ##########
    #bayesian opt or grid search
    hyper_domain <- hyper_grid_domain_list$droprate
    
    #Check domain
    if(!is.numeric(hyper_domain)){
      stop("droprate should be numeric")
    }
    if(!all(0 <= hyper_domain, hyper_domain < 1)){
      stop("droprate should be set in interval [0,1)")
    }
    ##########
    
    #rec_droprate
    ##########
    #bayesian opt or grid search
    hyper_domain <- hyper_grid_domain_list$rec_droprate
    
    #Check domain
    if(!is.numeric(hyper_domain)){
      stop("rec_droprate should be numeric")
    }
    if(!all(0 <= hyper_domain, hyper_domain < 1)){
      stop("rec_droprate should be set in interval [0,1)")
    }
    ##########
    
    #lr
    ##########
    #bayesian opt or grid search
    hyper_domain <- hyper_grid_domain_list$lr
    
    #Check domain
    if(!is.numeric(hyper_domain)){
      stop("lr should be numeric")
    }
    if(!all(hyper_domain > 0)){
      stop("lr should be positive")
    }
    ##########
    
    #number_of_epochs
    ##########
    #bayesian opt or grid search
    hyper_domain <- hyper_grid_domain_list$number_of_epochs
    
    #Check domain
    if(!all(is.integer(hyper_domain))){
      stop("number_of_epochs should be integer")
    }
    if(!all(hyper_domain > 0)){
      stop("number_of_epochs should be positive")
    }
    ##########
    
    #size_of_batch
    ##########
    #bayesian opt or grid search
    hyper_domain <- hyper_grid_domain_list$size_of_batch
    
    #Check domain
    if(!all(is.integer(hyper_domain))){
      stop("size_of_batch should be integer")
    }
    if(!all(hyper_domain > 0)){
      stop("size_of_batch should be positive")
    }
    ##########
    
    if (!is.numeric(n_ensembles) || length(n_ensembles) != 1L || is.na(n_ensembles)) {
      stop("`n_ensembles` must be a single non-missing numeric/integer value.", call. = FALSE)
    }
    n_ensembles <- as.integer(n_ensembles)
    if (n_ensembles < 1L) {
      stop("`n_ensembles` must be >= 1.", call. = FALSE)
    }
  }
  
  
  
  ## Keras architecture parameters
  if (model == "nn"){
    
    if(is.data.frame(keras_architecture_pars) || 
       !is.list(keras_architecture_pars) ||
       !all(c("units", "n_layers", "activation", 
              "nn_optimizer", "batch_norm_option") %in%
            names(keras_architecture_pars))){
      stop("keras_architecture_pars should be a list with units, n_layers,",
           "activation, nn_optimizer and batch_norm_option elements")
    }
    
    if(!all(is.numeric(keras_architecture_pars$units))){
      stop("units should be numeric")
    }
    
    if(!keras_architecture_pars$n_layers %in% c(1,2,3,4,5) ||
       length(keras_architecture_pars$n_layers) > 1){
      stop("n_layers should be an integer between 1 and 5.")
    }
    
    if(!all(keras_architecture_pars$activation %in%
            c("relu", "sigmoid", "softmax", "softplus", "tanh", "leaky_relu"))){
      stop("activation should be one of relu, sigmoid, softmax, softplus,",
           "tanh or leaky_relu.")
    }
    
    if(length(keras_architecture_pars$units) != 
       keras_architecture_pars$n_layers ||
       length(keras_architecture_pars$activation) != 
       keras_architecture_pars$n_layers ||
       length(keras_architecture_pars$batch_norm_option) != 
       keras_architecture_pars$n_layers
    ){
      stop("length of units, activation and batch_norm_option should match n_layers")
    }
    
    if(!keras_architecture_pars$nn_optimizer %in% c("Adam", "RMSProp")){
      stop("nn_optimizer should be Adam or RMSProp.")
    }
    
    
    if(!all(is.logical(keras_architecture_pars$batch_norm_option))){
      stop("batch_norm_option should be logical")
    }
    
    if(parallel){
      warning("keras models have some limitations regarding parallel computations.")
    }
    
  }
  if (model == "lstm") {
    
    ##keras_architecture_pars (LSTM)
    if (is.data.frame(keras_architecture_pars) ||
        !is.list(keras_architecture_pars) ||
        !all(c("units", "sequence_length", "nn_optimizer", "padding") %in% names(keras_architecture_pars))) {
      stop("For model = 'lstm', keras_architecture_pars must be a list with elements: ",
           "units, sequence_length, nn_optimizer, padding.")
    }
    
    if (!is.numeric(keras_architecture_pars$units) || length(keras_architecture_pars$units) != 1L ||
        keras_architecture_pars$units <= 0) {
      stop("keras_architecture_pars$units must be a single positive number.")
    }
    
    if (!is.numeric(keras_architecture_pars$sequence_length) || length(keras_architecture_pars$sequence_length) != 1L ||
        keras_architecture_pars$sequence_length < 1) {
      stop("keras_architecture_pars$sequence_length must be a single integer >= 1.")
    }
    
    if (!keras_architecture_pars$nn_optimizer %in% c("Adam", "RMSProp")) {
      stop("keras_architecture_pars$nn_optimizer must be 'Adam' or 'RMSProp'.")
    }
    
    if (!is.logical(keras_architecture_pars$padding)){
      stop("keras_architecture_pars$padding must be either TRUE or FALSE.")
    }
    
    ## ensure sequence_length fits in train/val windows
    if (train_n <= as.integer(keras_architecture_pars$sequence_length)) {
      stop("train_n must be > sequence_length for LSTM.")
    }
    if (val_n <= as.integer(keras_architecture_pars$sequence_length)) {
      stop("val_n must be > sequence_length for LSTM.")
    }
    
    if (!is.null(keras_architecture_pars$timesteps)) {
      if (!is.numeric(keras_architecture_pars$timesteps) ||
          length(keras_architecture_pars$timesteps) != 1L ||
          keras_architecture_pars$timesteps < 2) {
        stop("keras_architecture_pars$timesteps must be a single integer >= 2")
      }
    }
    
    if (isTRUE(parallel)) {
      warning("Keras LSTM may have limitations with parallel computations.")
    }
  }
   
  
  #Bayesian Opt
  if(any(
    #Check if hyper_grid_domain_list is a list
    !is.list(hyper_grid_domain_list),
    #Check if hyper_grid_domain_list elements have length of 2 (boundaries)
    !all(sapply(hyper_grid_domain_list, function(x) length(x) == 2)),
    #Check if hyper_grid_domain_list elements are vectors
    !all(sapply(hyper_grid_domain_list, function(x) is.vector(x))),
    #Check if hyper_grid_domain_list contains numeric values
    !all(sapply(hyper_grid_domain_list, function(x) is.numeric(x)))
  )
  ){
    stop("hyper_grid_domain_list not in correct format for bayesian_opt tuning.")
  }
  
  ## Covariates first column should be month_id and others should be numeric
  if (!"month_id" %in% colnames(covariates)){
    stop("covariates must contain a 'month_id' column.")
  }
  if (!lubridate::is.Date(covariates$month_id) &&
      !lubridate::is.POSIXt(covariates$month_id)){
    stop("covariates$month_id must be of Date or POSIXt type.")
  }
  if (ncol(covariates) < 2){
    stop("covariates must contain at least one feature column in addition to 'month_id'.")
  }
  if (!all(sapply(covariates[,-1], is.numeric))){
    stop("All columns in covariates except 'month_id' must be numeric.")
  }
  ## Target should contain only 2 cols: month_id and rv_month
  if (ncol(target) != 2){
    stop("target must contain exactly two columns: 'month_id' and 'rv_month'.")
  }
  if (!all(c("month_id", "rv_month") %in% colnames(target))){
    stop("target must contain columns named 'month_id' and 'rv_month'.")
  }
  if (!lubridate::is.Date(target$month_id) &&
      !lubridate::is.POSIXt(target$month_id)){
    stop("target$month_id must be of Date or POSIXt type.")
  }
  if (!is.numeric(target$rv_month)){
    stop("target$rv_month must be numeric.")
  }
  ## Check month_id in covariates and target match and are sorted
  if (!all(sort(covariates$month_id) == sort(target$month_id))){
    stop("month_id values in covariates and target must match.")
  }
  if (!isTRUE(all.equal(covariates$month_id, sort(covariates$month_id)))){
    stop("covariates$month_id must be sorted in ascending order.")
  }
  if (!isTRUE(all.equal(target$month_id, sort(target$month_id)))){
    stop("target$month_id must be sorted in ascending order.")
  }
  ## Check there are no duplicated month_id
  if (any(duplicated(covariates$month_id))){
    stop("covariates$month_id contains duplicate values.")
  }
  if (any(duplicated(target$month_id))){
    stop("target$month_id contains duplicate values.")
  }
  ## Check there are no NAs in covariates and target
  if (any(is.na(covariates))){
    stop("covariates contains NA values.")
  }
  if (any(is.na(target))){
    stop("target contains NA values.")
  }
  ## Check there are enough data points
  total_n <- nrow(covariates)
  if (total_n < (train_n + val_n + 12)){
    stop("Not enough data points for the specified train_n and val_n.")
  }
  ## Check there are enough rebalancing months
  if (length(rebal_months) == 0){
    stop("rebal_months must contain at least one month.")
  }
  if (any(duplicated(rebal_months))){
    stop("rebal_months contains duplicate months. Please ensure all months are unique.")
  }
  ## Check obj_fun and eval_metric are recognized
  recognized_obj_funs <- c("squared_error", "pseudo_huber_error",
                           "absolute_error")
  recognized_eval_metrics <- c("rss", "cp", "rmse", "mae", 
                               "mphe", "mpe", "mape", "hr", "mb")
  if (!obj_fun %in% recognized_obj_funs){
    stop(paste("obj_fun not recognized. Please choose one of the following:",
               paste(recognized_obj_funs, collapse = ", "), "."))
  }
  if (!eval_metric %in% recognized_eval_metrics){
    stop(paste("eval_metric not recognized. Please choose one of the following:",
               paste(recognized_eval_metrics, collapse = ", "), "."))
  }
  ## gsm algo
  if (!gsm_algo %in% c("ols", "tree")){
    stop("gsm_algo must be either 'ols' or 'tree'.")
  }
  
}

## Translate metrics and obj fun to algorithm pattern
translate_metrics <- function(model, eval_metric, obj_fun, 
                              early_stop, huber_delta, verbose){
  
  #Translate obj_fun and eval_metric
  if (model %in% c("har","glmnet","rf")){
    obj_fun_trans <- NULL
    eval_metric_trans <- NULL
  }
  
  if (model == "xgb"){
    obj_fun_trans <- switch(obj_fun,
                            squared_error = "reg:squarederror",
                            absolute_error = "reg:absoluteerror",
                            pseudo_huber_error = "reg:pseudohubererror",
                            "reg:squarederror"
    )
    
    eval_metric_trans <- switch(eval_metric,
                                rmse = "rmse",
                                mae = "mae",
                                mphe = "mphe",
                                #mpe = mpe_xgb(quantile_tau = quantile_tau),
                                mape = "mape",
                                #rss = rss_xgb, MAX
                                #hr = hr_xgb, MAX
                                #mb = mb_xgb,
                                #cp = cp_xgb, MAX
                                "rmse"
    )
    
  }
  if (model %in% c("nn", "lstm")){
    obj_fun_trans <- switch(obj_fun,
                            squared_error = "mean_squared_error",
                            absolute_error = "mean_absolute_error",
                            pseudo_huber_error = 
                              keras::loss_huber(delta = huber_delta),
                            "mean_squared_error"
    )
    
    eval_metric_trans <- switch(
      eval_metric,
      rmse = list(metric = "mean_squared_error", 
                  name = "val_mean_squared_error",
                  mode = "min"),
      mae =  list(metric = "mean_absolute_error",
                  name = "val_mean_absolute_error",
                  mode = "min"),
      mphe = list(metric = keras::loss_huber(delta = huber_delta),
                  name = "val_huber_loss",
                  mode = "min"),#Pseudo huber with custom delta
      #mpe = mpe_keras(quantile_tau = quantile_tau),
      mape = list(metric = "mean_absolute_percentage_error",
                  name = "val_mean_absolute_percentage_error",
                  mode = "min"),
      #rss = rss_keras,
      #cp = cp_keras,
      list(metric = "mean_squared_error",
           name = "val_mean_squared_error", 
           mode = "min")
    )
    
  }
  
  
  #Commentary about early_stop and using a eval metric not supported
  if(verbose){
    if(all(!is.null(early_stop), 
           model %in% c("xgb", "nn", "lstm"), 
           !eval_metric %in% c("rmse", "mae", "mphe", "mape"))){
      cat(crayon::yellow(
        paste0("This eval_metric is not supported by early stop.",
               "Applying rmse as criteria for early_stop instead."))
      )
      cat("\n")
      cat(paste("However", eval_metric,
                "will still be applied in hyperparameter tuning."))
    } 
    
    #Commentary about pseudo_huber_error in nn
    if(all(model %in% c("nn", "lstm"), #If Neural Network AND
           obj_fun == "pseudo_huber_error")){#pseudo_huber_error
      cat(crayon::yellow(
        paste0("Internal keras operations do not handle pseudo huber metric, ",
               "applying huber metric instead."))
      )
      cat("\n")
      
      if(eval_metric == "mphe"){ #If obj_obj is pseudo_huber and eval is mphe
        cat(paste("However, mphe will still be applied in hyper tuning."))
        cat("\n")
      }
    }
  }
  
  
  return(list(eval_metric = eval_metric,
              obj_fun_trans = obj_fun_trans,
              eval_metric_trans = eval_metric_trans)
  )
  
}

## Split helper
split_ts <- function(covariates, target, dates, current_date,
                     train_n, val_n){
  
  ### Checks
  stopifnot(!base::is.unsorted(covariates$month_id))               
  stopifnot(!base::is.unsorted(target$month_id))                                               
  
  ### Get d
  d <- which(dates == current_date)
  
  
  ### Train
  covariates_train <- covariates %>%
    dplyr::filter(month_id >= dates[1] &
                    month_id <= 
                    #Is it rebal month or first training sample?
                    ifelse(
                      d == (train_n + val_n), 
                      # First training sample
                      dates[train_n - 1],
                      # Rebal month
                      dates[d - val_n - 1]
                    )
    )
  full_data_train_clean <- covariates_train %>% 
    dplyr::left_join(target, by = "month_id") %>%
    dplyr::select(-month_id)
  target_train          <- full_data_train_clean %>% 
    dplyr::pull(rv_month)
  
  ### Validation
  if (val_n > 0){
    covariates_val <- covariates %>%
      dplyr::filter(month_id >= 
                      #Is it rebal month or first training sample?
                      ifelse(
                        d == (train_n + val_n), 
                        # First training sample
                        dates[train_n],
                        # Rebal month
                        dates[d - val_n]
                      ) &
                      month_id <= 
                      #Is it rebal month or first training sample?
                      ifelse(
                        d == (train_n + val_n),
                        # First val sample
                        dates[train_n + val_n - 1],
                        # Rebal month
                        dates[d - 1]
                      )
      )
    full_data_val_clean <- covariates_val %>% 
      dplyr::left_join(target, by = "month_id") %>%
      dplyr::select(-month_id)
    target_val          <- full_data_val_clean %>%
      dplyr::pull(rv_month)
  }
  
  ### Refit
  covariates_refit <- covariates %>%
    dplyr::filter(month_id >= dates[1] &
                    month_id <= 
                    #Is it rebal month or first training sample?
                    ifelse(
                      d == (train_n + val_n), 
                      # First training sample
                      dates[train_n + val_n - 1],
                      # Rebal month
                      dates[d - 1]
                    )
    )
  full_data_refit_clean <- covariates_refit %>% 
    dplyr::left_join(target, by = "month_id") %>%
    dplyr::select(-month_id)
  target_refit          <- full_data_refit_clean %>%
    dplyr::pull(rv_month)
  
  ### Return
  if (val_n > 0){
    list(
      covariates_train       = covariates_train,
      full_data_train_clean  = full_data_train_clean,
      target_train           = target_train,
      covariates_val         = covariates_val,
      full_data_val_clean    = full_data_val_clean,
      target_val             = target_val,
      covariates_refit       = covariates_refit,
      full_data_refit_clean  = full_data_refit_clean,
      target_refit           = target_refit
    )
  } else {
    list(
      covariates_train       = covariates_train,
      full_data_train_clean  = full_data_train_clean,
      target_train           = target_train,
      covariates_refit       = covariates_refit,
      full_data_refit_clean  = full_data_refit_clean,
      target_refit           = target_refit
    )
  }
}

## Get best lambda helper for glmnet
get_best_lambda <- function(glmnet_fit, lambda_seq,
                            covariates_val_clean, target_val,
                            huber_delta, quantile_tau, eval_metric){
  lambda_seq[which.max( #Which max score?
    sapply(
      apply(stats::predict( #Predict to find best_lam
        glmnet_fit, newx = as.matrix(covariates_val_clean)
      ), 
      2,
      function(x){
        #Calculate eval metrics for all lambdas
        calc_eval_metrics(
          pred = x, target = target_val,
          huber_delta = huber_delta, quantile_tau = quantile_tau,
          eval_metric = eval_metric
        )
      }
      ), 
      function(x) x$Score #Takes only score value
    )
  )]
}

## Calculate eval metrics
calc_eval_metrics <- function(pred, target, 
                              huber_delta = 1, quantile_tau = 0.5, 
                              eval_metric = "rmse",
                              early_stop = NULL, best_iter = NULL,
                              return_error = FALSE
){
  
  ### Checks
  if (!is.numeric(best_iter) && !is.null(best_iter)){
    stop ("best_iter should either be NULL or numeric.")
  }
  
  if(!eval_metric %in% c("rmse", "rss", "cp", "mae",
                         "mphe", "mpe", "mape", "hr")){
    stop("eval_metric should be one of rmse, rss, cp, mae, mphe, mpe, mape, hr")
  }
  
  ### Error
  error <- target - pred
  
  
  ### Calculate eval metrics
  if(all(is.na(error), is.na(target)) || any(is.na(pred))){
    val_rss <- NA
    val_cp <- NA
    val_rmse <- NA
    val_mae <- NA
    val_mphe <- NA
    val_mpe <- NA
    val_mape <- NA
    val_hr <- NA
    val_mb <- NA
  } else {
    eps <- 1e-12
    sst <- sum((target - mean(target))^2)
    
    val_rss <- if (sst < eps) NA_real_ else 1 - sum(error^2)/sst #R2
    val_cp <- mean(pred*target) #Cross-Product
    val_rmse <- sqrt(mean(error^2)) #RMSE
    val_mae <- mean(abs(error)) #mae
    val_mphe <- mean(
      huber_delta^2 * (sqrt(1 + (error / huber_delta)^2) - 1)
    ) #Pseudo-Huber
    val_mpe <- mean(ifelse(error>=0,
                           quantile_tau * (error),
                           (1-quantile_tau)*(-error))) #Pinball
    val_mape <- mean(abs(error / pmax(abs(target), eps))) #MAPE
    val_hr <- mean(sign(pred) == sign(target)) #Hit Rate
    val_mb <- mean(error)
  }
  
  ### Return DF
  df_eval_metrics <- data.frame(
    Score = switch(eval_metric,
                   rss = val_rss, #RSS
                   cp = val_cp, #CP
                   rmse = -val_rmse, #RMSE
                   mae = -val_mae, #MAE
                   mphe = -val_mphe, #MPHE
                   mpe = -val_mpe, #Pinball
                   mape = -val_mape, #MAPE
                   hr = val_hr #Hit Rate
    ),
    rss = val_rss,
    cp = val_cp,
    rmse = val_rmse,
    mae = val_mae,
    mphe = val_mphe,
    mpe = val_mpe,
    mape = val_mape,
    hr = val_hr,
    mb = val_mb
  )
  
  ### Include best iteration from early_stop
  if (!is.null(early_stop)){
    df_eval_metrics$best_iter <- best_iter
  }
  
  if (return_error){
    return(list(df_eval_metrics = df_eval_metrics,
                error = error))
  }
  
  return (df_eval_metrics)
  
}

## Set eval function helper
set_eval_function <- function(model){ #General Parameters
  
  ### Return a function to be passed to ParBayesianOptimization package
  
  #If tuning method is bayesian_opt, calls to eval function should be made through a wrapper
  eval_function <-
    switch(model,
           # GLMNET
           glmnet = function(...){ #Wrapper function
             
             ## Get args
             #######################
             args <- list(...)
             
             ### Data arguments
             full_data_train_clean <- args$full_data_train_clean #full
             covariates_val        <- args$covariates_val #validation 
             target_val            <- args$target_val #validation target
             target_name           <- "rv_month" #target
             
             ### Eval Function Parameters
             eval_metric       <- args$eval_metric #Chosen Eval
             eval_metric_trans <- args$eval_metric_trans #in algo terms
             huber_delta       <- args$huber_delta #Huber delta
             quantile_tau      <- args$quantile_tau #Quantile tau
             
             ### Early Stop
             early_stop <- args$early_stop #Eartly Stop
             
             ### Custom Loss
             obj_fun_trans <- args$obj_fun_trans
             
             ### Keras Network Parameters
             keras_architecture_pars <- args$keras_architecture_pars 
             
             #######################
             
             ## Deliver GLMET function  
             fit <- function(alpha, lambda.min.ratio){ #Hyperparameters
               
               ### Set objects in GLM format
               covariates_matrix_train_clean <- full_data_train_clean %>% 
                 dplyr::select(-dplyr::all_of(target_name)) #Get training
               target_vector_train <- full_data_train_clean %>% 
                 dplyr::pull(target_name) #Get training target vector
               covariates_val_clean <- covariates_val %>% 
                 dplyr::select(-month_id)
               
               
               ### Fit GLM model
               glmnet_fit <- glmnet::glmnet(
                 as.matrix(covariates_matrix_train_clean), #train matrix
                 target_vector_train, #target vector
                 alpha = alpha, #alpha hyperparameter
                 lambda.min.ratio = lambda.min.ratio  #lambda min ratio
               )
               
               ### Get best lambda
               best_lam <- get_best_lambda(
                 glmnet_fit = glmnet_fit,
                 lambda_seq = glmnet_fit$lambda, #Glmnet Specific
                 covariates_val_clean = covariates_val_clean, #Val Data
                 target_val = target_val,  
                 huber_delta = huber_delta,  #Eval Metrics Parameters
                 quantile_tau = quantile_tau,
                 eval_metric = eval_metric
               ) 
               
               ### Predict with best lam
               pred <- stats::predict(
                 glmnet_fit,#GLM model
                 newx = as.matrix(covariates_val_clean),  #Features test
                 s = best_lam #Predict with best_lam
               ) 
               
               ### Calculate eval metrics
               df_eval_metrics <- calc_eval_metrics(
                 pred = pred, target = target_val,
                 huber_delta = huber_delta, quantile_tau = quantile_tau,
                 eval_metric = eval_metric
               )
               
               ### Return List
               return(list(Score = df_eval_metrics$Score,
                           rss = df_eval_metrics$rss,
                           cp = df_eval_metrics$cp,
                           rmse = df_eval_metrics$rmse,
                           mae = df_eval_metrics$mae,
                           mphe = df_eval_metrics$mphe,
                           mpe = df_eval_metrics$mpe,
                           mape = df_eval_metrics$mape,
                           hr = df_eval_metrics$hr,
                           mb = df_eval_metrics$mb,
                           best_lam = best_lam)
               )
               
             }
             
           },
           # Random Forest
           rf = function(...){ 
             
             ## Get args
             #######################
             args <- list(...)
             
             ### Data arguments
             full_data_train_clean <- args$full_data_train_clean #full 
             covariates_val        <- args$covariates_val #validation
             target_val            <- args$target_val #validation target
             target_name           <- "rv_month" #target
             
             ### Eval Function Parameters
             eval_metric       <- args$eval_metric #Chosen Eval
             eval_metric_trans <- args$eval_metric_trans #For early stop
             huber_delta       <- args$huber_delta #Huber delta
             quantile_tau      <- args$quantile_tau #Quantile tau
             
             ### Early Stop
             early_stop <- args$early_stop #Eartly Stop
             
             ### Custom Loss
             obj_fun_trans <- args$obj_fun_trans
             
             ### Keras Network Parameters
             keras_architecture_pars <- args$keras_architecture_pars 
             
             #######################
             
             ## Deliver RF function
             fit <- function(mtry, num.trees, max.depth, min.bucket){
               
               ### Fit RF model
               rf_fit <- ranger::ranger(
                 paste(target_name,'~.'),
                 #Names need to be clean
                 data = janitor::clean_names(full_data_train_clean), 
                 #Proportion of variables used to forecast
                 mtry = mtry * (ncol(full_data_train_clean) - 1), 
                 num.trees = num.trees,  #Number of trees
                 max.depth = max.depth,  #Max Depth of tree
                 min.bucket = min.bucket #Min Size of Terminal Node
               ) 
               
               ### Format
               covariates_val_clean <- covariates_val %>%
                 dplyr::select(-month_id)
               
               
               ### Predict
               pred <- stats::predict(
                 rf_fit, #RF model
                 data = janitor::clean_names(covariates_val_clean) # val
               )$predictions
               
               ### Calculate eval metrics
               df_eval_metrics <- calc_eval_metrics(
                 pred = pred, target = target_val,
                 huber_delta = huber_delta, 
                 quantile_tau = quantile_tau,
                 eval_metric = eval_metric
               )
               
               #Return List
               return(list(Score = df_eval_metrics$Score,
                           rss = df_eval_metrics$rss,
                           cp = df_eval_metrics$cp,
                           rmse = df_eval_metrics$rmse,
                           mae = df_eval_metrics$mae,
                           mphe = df_eval_metrics$mphe,
                           mpe = df_eval_metrics$mpe,
                           mape = df_eval_metrics$mape,
                           hr = df_eval_metrics$hr,
                           mb = df_eval_metrics$mb)
               )
               
             }
             
           },
           # XGB
           xgb = function(...){ #Wrapper function
             
             ## Get args
             ########################
             args <- list(...)
             
             ### Data arguments
             full_data_train_clean <- args$full_data_train_clean #full 
             covariates_val        <- args$covariates_val #validation 
             target_val            <- args$target_val #validation target
             target_name           <- "rv_month" #target
             
             ### Eval Function Parameters
             eval_metric       <- args$eval_metric #Chosen Eval
             eval_metric_trans <- args$eval_metric_trans #Early Stop
             huber_delta       <- args$huber_delta #Huber delta
             quantile_tau      <- args$quantile_tau #Quantile tau
             
             ### Early Stop
             early_stop <- args$early_stop #Early Stop
             
             ## #Custom Loss
             obj_fun_trans <- args$obj_fun_trans
             
             ### Keras Network Parameters
             keras_architecture_pars <- args$keras_architecture_pars 
             
             ########################
             
             ## Deliver XGB function
             fit <- function(min_child_weight, max_depth, subsample,
                             colsample_bytree, eta, alpha, gamma, nrounds){ 
               
               ### Set objects in XGB Format
               covariates_matrix_train_clean <- full_data_train_clean %>% 
                 dplyr::select(-dplyr::all_of(target_name)) #Get training 
               target_vector_train <- full_data_train_clean %>%
                 dplyr::pull(target_name) #Get training target vector
               covariates_val_clean <- covariates_val %>%
                 dplyr::select(-month_id)
               
               full_data_train_xgb     <- xgboost::xgb.DMatrix(
                 data = as.matrix(covariates_matrix_train_clean), 
                 label = target_vector_train
               )
               full_data_val_clean_xgb <- xgboost::xgb.DMatrix(
                 data = as.matrix(covariates_val_clean),
                 label = target_val
               )
               
               ### Fit XGB model
               xgb_fit <- xgboost::xgb.train(
                 data = full_data_train_xgb,
                 eta = eta, #Learning Rate
                 early_stopping_rounds = early_stop, #Number of rounds to early stop
                 min_child_weight = min_child_weight, #Minimum sum of instance weight (hessian) needed in a child
                 max_depth = round(max_depth, 0), #Max tree depth
                 nrounds = nrounds, #Number of trees (boosting interations)
                 subsample = subsample, #Subsample ratio of training instance
                 colsample_bytree = colsample_bytree, #Col subsample
                 alpha = alpha, #L1 regularization on weights
                 gamma = gamma, #Min loss reduction to make a further partition
                 print_every_n = 25,
                 verbose = FALSE,
                 eval_metric = eval_metric_trans, #Set eval metric for ealy stop
                 #Set custom objective
                 objective = obj_fun_trans,
                 #Watchlist,
                 watchlist = list(train = full_data_train_xgb, 
                                  validation = full_data_val_clean_xgb),
                 huber_slope = huber_delta #Huber delta
                 #quantile_alpha = quantile_tau #Tau for quantile regression
               )
               
               
               ### Predict
               pred <- stats::predict(
                 xgb_fit,#XGB model
                 newdata = as.matrix(covariates_val_clean) #Features val
               )
               
               #Calculate eval metrics
               df_eval_metrics <- calc_eval_metrics(
                 pred = pred, target = target_val,
                 huber_delta = huber_delta, quantile_tau = quantile_tau,
                 eval_metric = eval_metric,
                 early_stop = early_stop,
                 best_iter = xgb_fit$best_iteration
               )
               
               #Return List
               if(is.null(early_stop)){
                 return(list(Score = df_eval_metrics$Score,
                             rss = df_eval_metrics$rss,
                             cp = df_eval_metrics$cp,
                             rmse = df_eval_metrics$rmse,
                             mae = df_eval_metrics$mae,
                             mphe = df_eval_metrics$mphe,
                             mpe = df_eval_metrics$mpe,
                             mape = df_eval_metrics$mape,
                             hr = df_eval_metrics$hr,
                             mb = df_eval_metrics$mb)
                 )
                 
               } else {
                 return(list(Score = df_eval_metrics$Score,
                             rss = df_eval_metrics$rss,
                             cp = df_eval_metrics$cp,
                             rmse = df_eval_metrics$rmse,
                             mae = df_eval_metrics$mae,
                             mphe = df_eval_metrics$mphe,
                             mpe = df_eval_metrics$mpe,
                             mape = df_eval_metrics$mape,
                             hr = df_eval_metrics$hr,
                             mb = df_eval_metrics$mb,
                             best_iter = df_eval_metrics$best_iter)
                 )
                 
                 
               }
               
               
             }
             
           },
           #NN
           nn = function(...){ #Wrapper function
             
             ## Get args
             ########################
             args <- list(...)
             
             ### Data arguments
             full_data_train_clean <- args$full_data_train_clean #full data
             covariates_val        <- args$covariates_val #validation 
             target_val            <- args$target_val #validation target
             target_name           <- "rv_month" #target
             
             ### Eval Function Parameters
             eval_metric       <- args$eval_metric #Chosen Eval
             eval_metric_trans <- args$eval_metric_trans #For Early Stop
             huber_delta       <- args$huber_delta #Huber delta
             quantile_tau      <- args$quantile_tau #Quantile tau
             
             ### Early Stop
             early_stop  <- args$early_stop #Eartly Stop
             
             ### Custom Loss
             obj_fun_trans <- args$obj_fun_trans
             
             ### Keras Network Parameters
             keras_architecture_pars <- args$keras_architecture_pars 
             
             ########################
             
             ## Deliver NN function   
             fit <- function(regularizer_l1, regularizer_l2, droprate, lr,
                             number_of_epochs, size_of_batch){ 
               
               ### Format
               covariates_matrix_train_clean <- full_data_train_clean %>% 
                 dplyr::select(-dplyr::all_of(target_name)) #Get training features matrix
               target_vector_train <- full_data_train_clean %>%
                 dplyr::pull(target_name) #Get training target vector
               covariates_val_clean <- covariates_val %>% 
                 dplyr::select(-month_id)
               
               ### Fit keras model
               keras_results <- fit_keras_model(
                 #### Hyperparameters
                 regularizer_l1 = regularizer_l1, 
                 regularizer_l2 = regularizer_l2,
                 droprate = droprate, #Hyperparameters Part 1
                 lr = lr,
                 number_of_epochs = number_of_epochs,
                 size_of_batch = size_of_batch, #Hyperparameters Part 2
                 
                 #### Architecture choices
                 keras_architecture_pars = keras_architecture_pars,
                 n_ensembles = 1L,
                 
                 #### Early Stop
                 early_stop = early_stop,
                 eval_metric_trans = eval_metric_trans,
                 
                 #### Loss Function
                 obj_fun_trans = obj_fun_trans, 
                 huber_delta = huber_delta,
                 
                 #### Data
                 covariates_matrix_train_clean = covariates_matrix_train_clean,
                 target_vector_train = target_vector_train, #Data Part I
                 covariates_val_clean = covariates_val_clean,
                 target_val = target_val #Data Part II
               )
               
               model_obj  <- keras_results$model_nn #Neural network models
               hist_obj   <- keras_results$fit_nn #Training history
               Xv <- as.matrix(covariates_val_clean) #Features val
               
               # ---- Predict: handle single vs ensemble ----
               if (is.list(model_obj)) {
                 # matrix: [n_obs x n_ensembles]
                 pred_list <- purrr::map(
                   model_obj, #NN model
                   ~ base::as.numeric(stats::predict(.x, Xv))
                 )
                   ## Defensive check: all predictions must have same length
                   pred_len <- base::vapply(pred_list, length, integer(1L))
                   if (any(pred_len != pred_len[1L])) {
                     stop("Ensemble members returned predictions with different lengths.", call. = FALSE)
                   }
                   
                 pred_mat <- base::do.call(cbind, pred_list)   # n_obs x n_ensembles
                 pred     <- base::rowMeans(pred_mat)
                 
                 # best_iter: median of per-model best epochs (if histories exist)
                 best_iter_vec <- purrr::map_int(
                   hist_obj,
                   ~ base::which.min(.x$metrics[[eval_metric_trans$name]])
                 )
                 best_iter_agg <- base::as.integer(stats::median(best_iter_vec))
               } else {
                 pred <- base::as.numeric(stats::predict(model_obj, Xv))
                 best_iter_agg <- base::which.min(hist_obj$metrics[[eval_metric_trans$name]])
               }
            
               #Calculate eval metrics
               df_eval_metrics <- calc_eval_metrics(
                 pred = pred, target = target_val,
                 huber_delta = huber_delta, quantile_tau = quantile_tau,
                 eval_metric = eval_metric,
                 early_stop = early_stop,
                 best_iter = best_iter_agg
               )
               
               #Improve memory usage
               rm(covariates_matrix_train_clean, target_vector_train,
                  covariates_val_clean,
                  model_obj, hist_obj)
               gc()
               
               
               #Return List
               if(is.null(early_stop)){
                 return(list(Score = df_eval_metrics$Score,
                             rss = df_eval_metrics$rss,
                             cp = df_eval_metrics$cp,
                             rmse = df_eval_metrics$rmse,
                             mae = df_eval_metrics$mae,
                             mphe = df_eval_metrics$mphe,
                             mpe = df_eval_metrics$mpe,
                             mape = df_eval_metrics$mape,
                             hr = df_eval_metrics$hr,
                             mb = df_eval_metrics$mb)
                 )
                 
               } else {
                 return(list(Score = df_eval_metrics$Score,
                             rss = df_eval_metrics$rss,
                             cp = df_eval_metrics$cp,
                             rmse = df_eval_metrics$rmse,
                             mae = df_eval_metrics$mae,
                             mphe = df_eval_metrics$mphe,
                             mpe = df_eval_metrics$mpe,
                             mape = df_eval_metrics$mape,
                             hr = df_eval_metrics$hr,
                             mb = df_eval_metrics$mb,
                             best_iter = df_eval_metrics$best_iter)
                 )
               }
             }
           },
           #LSTM
           lstm = function(...){
             
             ## Get args
             ########################
             args <- list(...)
             
             ### Data arguments
             full_data_train_clean <- args$full_data_train_clean #full data
             covariates_val        <- args$covariates_val #validation 
             target_val            <- args$target_val #validation target
             target_name           <- "rv_month" #target
             
             ### Eval Function Parameters
             eval_metric       <- args$eval_metric #Chosen Eval
             eval_metric_trans <- args$eval_metric_trans #For Early Stop
             huber_delta       <- args$huber_delta #Huber delta
             quantile_tau      <- args$quantile_tau #Quantile tau
             
             ### Early Stop
             early_stop <- args$early_stop #Eartly Stop
             
             ### Custom Loss
             obj_fun_trans <- args$obj_fun_trans
             
             ### Keras Network Parameters
             keras_architecture_pars <- args$keras_architecture_pars
             
             ########################
            
             ##Deliver LSTM function
             fit <- function(droprate, rec_droprate, lr,
                             number_of_epochs, size_of_batch,
                             regularizer_l2){
               
               # Train split -> build sequences from full_data_train_clean
               padding_flag <- isTRUE(keras_architecture_pars$padding)
               Tseq         <- base::as.integer(keras_architecture_pars$sequence_length)
               
               cov_train <- full_data_train_clean %>%
                 dplyr::select(-dplyr::all_of(target_name))
               y_train <- full_data_train_clean %>%
                 dplyr::pull(target_name)
               covariates_val_clean <- covariates_val %>%
                 dplyr::select(-month_id)
               
               train_seq <- build_lstm_sequences(
                 X_df     = cov_train,
                 y_vec    = y_train,
                 T        = Tseq,
                 padding  = padding_flag,
                 pad_value = -999
               )
               
               # Validation split -> build sequences from covariates_val / target_val
               val_seq <- build_lstm_sequences(
                 X_df     = covariates_val_clean,
                 y_vec    = target_val,
                 T        = Tseq,
                 padding  = padding_flag,
                 pad_value = -999
               )
               
               
               # Fit LSTM using helper
               # Batch must not exceed number of sequences
               n_seq_train <- dim(train_seq$X)[1]
               bs <- as.integer(size_of_batch)
               if (bs > n_seq_train) bs <- n_seq_train
               
               lstm_fit <- fit_lstm_model(
                 units              = as.integer(keras_architecture_pars$units),
                 droprate           = droprate,
                 rec_droprate       = rec_droprate,
                 lr                 = lr,
                 number_of_epochs   = as.integer(number_of_epochs),
                 size_of_batch      = bs,
                 regularizer_l2     = regularizer_l2,
                 early_stop         = early_stop,
                 eval_metric_trans  = eval_metric_trans,
                 obj_fun_trans      = obj_fun_trans,
                 covariates_seq_train = train_seq$X,
                 target_seq_train     = train_seq$y,
                 covariates_seq_val   = val_seq$X,
                 target_seq_val       = val_seq$y,
                 padding              = padding_flag,     
                 pad_value            = -999      
                 )
               
           
               # Predict on validation sequences
               pred <- predict(lstm_fit$model, val_seq$X)
               pred <- base::as.numeric(pred)
               
               # Compute metrics against the aligned target (val_seq$y)
               df_eval_metrics <- calc_eval_metrics(
                 pred         = pred,
                 target       = val_seq$y,
                 huber_delta  = huber_delta,
                 quantile_tau = quantile_tau,
                 eval_metric  = eval_metric,
                 early_stop   = early_stop,
                 best_iter    = base::which.min(lstm_fit$history$metrics[[eval_metric_trans$name]])
               )
               
               # Return in the same shape as other models
               if (is.null(early_stop)){
                 return(list(
                   Score = df_eval_metrics$Score,
                   rss   = df_eval_metrics$rss,
                   cp    = df_eval_metrics$cp,
                   rmse  = df_eval_metrics$rmse,
                   mae   = df_eval_metrics$mae,
                   mphe  = df_eval_metrics$mphe,
                   mpe   = df_eval_metrics$mpe,
                   mape  = df_eval_metrics$mape,
                   hr    = df_eval_metrics$hr,
                   mb    = df_eval_metrics$mb
                 ))
               } else {
                 return(list(
                   Score    = df_eval_metrics$Score,
                   rss      = df_eval_metrics$rss,
                   cp       = df_eval_metrics$cp,
                   rmse     = df_eval_metrics$rmse,
                   mae      = df_eval_metrics$mae,
                   mphe     = df_eval_metrics$mphe,
                   mpe      = df_eval_metrics$mpe,
                   mape     = df_eval_metrics$mape,
                   hr       = df_eval_metrics$hr,
                   mb       = df_eval_metrics$mb,
                   best_iter = df_eval_metrics$best_iter
                 ))
               }
             }
           }
           
    ) #End switch
  
  return(eval_function)
  
}

## Keras helper
fit_keras_model <- function(
    regularizer_l1, regularizer_l2, droprate, lr,
    number_of_epochs, size_of_batch, #Hyperparameters
    keras_architecture_pars, #Network
    early_stop = NULL, #Training
    obj_fun_trans, huber_delta, #Loss Function Parameters
    covariates_matrix_train_clean, target_vector_train, #Data
    n_ensembles = 1L,
    ...
){
  
  ### Clear the session after each model training
  on.exit({
    keras::k_clear_session()
    gc()
  }, add = TRUE)
  . <- NULL
  
  ### Validation arguments necessary only for early stop on validation set
  args <- list(...)
  try({ #early_stop: Can either be NULL (not set, which is a refit),
    #            NULL (set, ie do not apply early stop)
    #            and NUMBER (set, for tuning only)
    covariates_val_clean <- args$covariates_val_clean
    target_val           <- args$target_val
  })
  eval_metric_trans <- args$eval_metric_trans
  
  ### Seed setter
  set_all_seeds <- function(seed) {
    base::set.seed(seed)
    if (base::requireNamespace("tensorflow", quietly = TRUE)) {
      tensorflow::tf$random$set_seed(as.integer(seed))
    }
    invisible(TRUE)
  }
  
  fit_once <- function(seed = NULL) {
    if (!is.null(seed)) {
      set_all_seeds(seed)
    }
  
      ### Define the structure of the network (how layers are organized)
      ### Typical NN1 Architecture
      if(keras_architecture_pars$n_layers == 1){
        model_nn <- keras::keras_model_sequential()
        tryCatch(
          {#Try to create keras network
            model_nn %>%
              keras::layer_dense(
                units       = keras_architecture_pars$units[1],
                activation  = keras_architecture_pars$activation[1],
                #Shape = # of features
                input_shape =  ncol(covariates_matrix_train_clean), 
                #L1 and L2 Regularization
                kernel_regularizer = keras::regularizer_l1_l2(
                  l1 = regularizer_l1, l2 = regularizer_l2)
              )
            
            #Batch normalization
            if (isTRUE(keras_architecture_pars$batch_norm_option[1])) {
              model_nn <- model_nn %>% keras::layer_batch_normalization()
            }
            
            model_nn <- model_nn %>%
              keras::layer_dropout(rate = droprate) %>% #Adds dropout
              #No activation means linear: f(x) = x
              keras::layer_dense(units = 1) 
          },
          error = function(e) {
            stop(
              paste(
                "Failure in creating keras network.",
                "Check units, activation, input_shape, regularizer, BN, droprate.\n",
                "Original error:", conditionMessage(e)
              ),
              call. = FALSE
            )
          }
        )
      }
      # Typical NN2 Architecture
      if(keras_architecture_pars$n_layers == 2){
        model_nn <- keras::keras_model_sequential()
        tryCatch(
          {#Try to create keras network
            model_nn %>%
              keras::layer_dense(
                units = keras_architecture_pars$units[1],
                #Units and activation may vary by layer
                activation  = keras_architecture_pars$activation[1], 
                #Shape = # of features
                input_shape =  ncol(covariates_matrix_train_clean), 
                #L1 and L2 Regularization
                kernel_regularizer = keras::regularizer_l1_l2(
                  l1 = regularizer_l1, l2 = regularizer_l2)
              ) %>% 
              #Batch normalization
              {if (keras_architecture_pars$batch_norm_option[1]) keras::layer_batch_normalization() else .}() %>% 
              #Adds dropout
              keras::layer_dropout(rate = droprate) %>% 
              keras::layer_dense(
                units = keras_architecture_pars$units[2],
                #Units and activation may vary by layer
                activation = keras_architecture_pars$activation[2], 
                kernel_regularizer = keras::regularizer_l1_l2(
                  l1 = regularizer_l1, l2 = regularizer_l2)
              ) %>%
              #Batch normalization
              {if (keras_architecture_pars$batch_norm_option[2]) keras::layer_batch_normalization() else .}() %>% 
              keras::layer_dropout(rate = droprate) %>% #Adds dropout
              #No activation means linear: f(x) = x
              keras::layer_dense(units = 1) 
          },
          error = function(e) {
            stop(
              paste(
                "Failure in creating keras network.",
                "Check units, activation, input_shape, regularizer, BN, droprate.\n",
                "Original error:", conditionMessage(e)
              ),
              call. = FALSE
            )
          }
        )
      }
      # Typical NN3 Architecture
      if(keras_architecture_pars$n_layers == 3){
        model_nn <- keras::keras_model_sequential()
        tryCatch(
          {#Try to create keras network
            model_nn %>%
              keras::layer_dense(
                units       = keras_architecture_pars$units[1],
                # Units and activation may vary by layer
                activation  = keras_architecture_pars$activation[1], 
                # Shape = # of features
                input_shape = ncol(covariates_matrix_train_clean), 
                # L1 and L2 Regularization
                kernel_regularizer = keras::regularizer_l1_l2(
                  l1 = regularizer_l1, l2 = regularizer_l2)
              ) %>% 
              # Batch normalization
              {if (keras_architecture_pars$batch_norm_option[1]) keras::layer_batch_normalization() else .}() %>% 
              # Adds dropout
              keras::layer_dropout(rate = droprate) %>% 
              keras::layer_dense(
                #Units and activation may vary by layer
                units              = keras_architecture_pars$units[2],
                activation         = keras_architecture_pars$activation[2], 
                kernel_regularizer = keras::regularizer_l1_l2(
                  l1 = regularizer_l1, l2 = regularizer_l2)
              ) %>%
              #Batch normalization
              {if (keras_architecture_pars$batch_norm_option[2]) keras::layer_batch_normalization() else .}() %>% 
              #Adds dropout
              keras::layer_dropout(rate = droprate) %>% 
              keras::layer_dense(
                #Units and activation may vary by layer
                units      = keras_architecture_pars$units[3],
                activation = keras_architecture_pars$activation[3],
                kernel_regularizer = keras::regularizer_l1_l2(
                  l1 = regularizer_l1, l2 = regularizer_l2)
              ) %>%
              #Batch normalization
              {if (keras_architecture_pars$batch_norm_option[3]) keras::layer_batch_normalization() else .}() %>% 
              #Adds dropout
              keras::layer_dropout(rate = droprate) %>%
              #No activation means linear: f(x) = x
              keras::layer_dense(units = 1) 
          },
          error = function(e) {
            stop(
              paste(
                "Failure in creating keras network.",
                "Check units, activation, input_shape, regularizer, BN, droprate.\n",
                "Original error:", conditionMessage(e)
              ),
              call. = FALSE
            )
          }
        )
      } 
      # Typical NN4 Architecture
      if(keras_architecture_pars$n_layers == 4){
        model_nn <- keras::keras_model_sequential()
        tryCatch(
          {# Try to create keras network
            model_nn %>%
              keras::layer_dense(
                units              = keras_architecture_pars$units[1],
                #Units and activation may vary by layer
                activation         = keras_architecture_pars$activation[1],
                #Shape = # of features
                input_shape        =  ncol(covariates_matrix_train_clean), 
                #L1 and L2 Regularization
                kernel_regularizer = keras::regularizer_l1_l2(
                  l1 = regularizer_l1, l2 = regularizer_l2)
              ) %>% 
              # Batch normalization
              {if (keras_architecture_pars$batch_norm_option[1]) keras::layer_batch_normalization() else .}() %>% 
              # Adds dropout
              keras::layer_dropout(rate = droprate) %>% 
              keras::layer_dense(
                units              = keras_architecture_pars$units[2],
                #Units and activation may vary by layer
                activation         = keras_architecture_pars$activation[2], 
                kernel_regularizer = keras::regularizer_l1_l2(
                  l1 = regularizer_l1, l2 = regularizer_l2)
              ) %>%
              #Batch normalization
              {if (keras_architecture_pars$batch_norm_option[2]) keras::layer_batch_normalization() else .}() %>%
              #Adds dropout
              keras::layer_dropout(rate = droprate) %>% 
              keras::layer_dense(
                #Units and activation may vary by layer
                units              = keras_architecture_pars$units[3],
                activation         = keras_architecture_pars$activation[3], 
                kernel_regularizer = keras::regularizer_l1_l2(
                  l1 = regularizer_l1, l2 = regularizer_l2)
              ) %>%
              #Batch normalization
              {if (keras_architecture_pars$batch_norm_option[3]) keras::layer_batch_normalization() else .}() %>%
              #Adds dropout
              keras::layer_dropout(rate = droprate) %>% 
              keras::layer_dense(
                #Units and activation may vary by layer
                units              = keras_architecture_pars$units[4],
                activation         = keras_architecture_pars$activation[4], 
                kernel_regularizer = keras::regularizer_l1_l2(
                  l1 = regularizer_l1, l2 = regularizer_l2)
              ) %>%
              #Batch normalization
              {if (keras_architecture_pars$batch_norm_option[4]) keras::layer_batch_normalization() else .}() %>% 
              #Adds dropout
              keras::layer_dropout(rate = droprate) %>% 
              #No activation means linear: f(x) = x
              keras::layer_dense(units = 1) 
          },
          error = function(e) {
            stop(
              paste(
                "Failure in creating keras network.",
                "Check units, activation, input_shape, regularizer, BN, droprate.\n",
                "Original error:", conditionMessage(e)
              ),
              call. = FALSE
            )
          }
        )
      }
      #Typical NN5 Architecture
      if(keras_architecture_pars$n_layers == 5){
        model_nn <- keras::keras_model_sequential()
        tryCatch(
          {#Try to create keras network
            model_nn %>%
              keras::layer_dense(
                #Units and activation may vary by layer
                units              = keras_architecture_pars$units[1],
                activation         = keras_architecture_pars$activation[1], 
                #Shape = # of features
                input_shape        =  ncol(covariates_matrix_train_clean), 
                kernel_regularizer = keras::regularizer_l1_l2(
                  l1 = regularizer_l1, l2 = regularizer_l2)
              ) %>% #L1 and L2 Regularization
              #Batch normalization
              {if (keras_architecture_pars$batch_norm_option[1]) keras::layer_batch_normalization() else .}() %>% 
              #Adds dropout
              keras::layer_dropout(rate = droprate) %>% 
              keras::layer_dense(
                #Units and activation may vary by layer
                units              = keras_architecture_pars$units[2],
                activation         = keras_architecture_pars$activation[2],
                kernel_regularizer = keras::regularizer_l1_l2(
                  l1 = regularizer_l1, l2 = regularizer_l2)
              ) %>%
              #Batch normalization
              {if (keras_architecture_pars$batch_norm_option[2]) keras::layer_batch_normalization() else .}() %>%
              #Adds dropout
              keras::layer_dropout(rate = droprate) %>% 
              keras::layer_dense(
                #Units and activation may vary by layer
                units              = keras_architecture_pars$units[3],
                activation         = keras_architecture_pars$activation[3],
                kernel_regularizer = keras::regularizer_l1_l2(
                  l1 = regularizer_l1, l2 = regularizer_l2)
              ) %>%
              #Batch normalization
              {if (keras_architecture_pars$batch_norm_option[3]) keras::layer_batch_normalization() else .}() %>% 
              keras::layer_dropout(rate = droprate) %>% #Adds dropout
              keras::layer_dense(
                #Units and activation may vary by layer
                units              = keras_architecture_pars$units[4],
                activation         = keras_architecture_pars$activation[4], 
                kernel_regularizer = keras::regularizer_l1_l2(
                  l1 = regularizer_l1, l2 = regularizer_l2)
              ) %>%
              #Batch normalization
              {if (keras_architecture_pars$batch_norm_option[4]) keras::layer_batch_normalization() else .}() %>%
              keras::layer_dropout(rate = droprate) %>% #Adds dropout
              keras::layer_dense(
                #Units and activation may vary by layer
                units              = keras_architecture_pars$units[5],
                activation         = keras_architecture_pars$activation[5], 
                kernel_regularizer = keras::regularizer_l1_l2(
                  l1 = regularizer_l1, l2 = regularizer_l2)
              ) %>%
              #Batch normalization
              {if (keras_architecture_pars$batch_norm_option[5]) keras::layer_batch_normalization() else .}() %>%
              keras::layer_dropout(rate = droprate) %>% #Adds dropout
              keras::layer_dense(units = 1) #No activation means linear: f(x) = x
          },
          error = function(e) {
            stop(
              paste(
                "Failure in creating keras network.",
                "Check units, activation, input_shape, regularizer, BN, droprate.\n",
                "Original error:", conditionMessage(e)
              ),
              call. = FALSE
            )
          }
        )
      } 
      
      #Backpropagation
      tryCatch(
        {#Try to compile keras model
          model_nn %>% keras::compile( #Model Specification
            #Loss function
            loss = obj_fun_trans,
            #Optimization method and learning rate
            optimizer = switch(
              keras_architecture_pars$nn_optimizer,
              "Adam"    = keras::optimizer_adam(learning_rate = lr, clipnorm = 1.0),
              "RMSProp" = keras::optimizer_rmsprop(learning_rate = lr),
              keras::optimizer_adam(learning_rate = lr)
            ),
            #Custom eval metric translated
            metrics = eval_metric_trans$metric
          )
        },
        error = function(e) {
          stop(
            paste(
              "Failure in creating keras network.",
              "Check units, activation, input_shape, regularizer, BN, droprate.\n",
              "Original error:", conditionMessage(e)
            ),
            call. = FALSE
          )
        }
      )
      
      #Fit
      tryCatch(
        {
          if(is.null(early_stop)){
            #In case no early_stop
            fit_nn <- model_nn %>% 
              keras::fit(
                x          = as.matrix(covariates_matrix_train_clean), #features
                y          = target_vector_train, #Training label
                epochs     = number_of_epochs, #Number of epochs
                batch_size = size_of_batch, #Batch size (should be a multiple of 2)
                verbose    = FALSE
              )
          } else {
            #In case of early_stop
            fit_nn <- model_nn %>% 
              keras::fit(
                x          = as.matrix(covariates_matrix_train_clean), #features
                y          = target_vector_train, #Training label
                epochs     = number_of_epochs, #Number of epochs
                batch_size = size_of_batch, #Batch size (should be a multiple of 2)
                verbose    = FALSE,
                callbacks = list(
                  keras::callback_early_stopping(
                    monitor              = eval_metric_trans$name,
                    #Early stop (nº epochs with no improvement)
                    patience             = early_stop, 
                    #Restore best weights after stopping
                    restore_best_weights = TRUE, 
                    #Min for RMSE, MAE and HUBER
                    mode                 = eval_metric_trans$mode 
                  ) 
                ), 
                validation_data = 
                  #Validation data
                  list(as.matrix(covariates_val_clean), target_val)
              )
          }
        },
        error = function(e) {
          stop(
            paste(
              "Failure in creating keras network.",
              "Check units, activation, input_shape, regularizer, BN, droprate.\n",
              "Original error:", conditionMessage(e)
            ),
            call. = FALSE
          )
        }
      )
      return(list(model_nn = model_nn,
                  fit_nn   = fit_nn))
    
  }
  
  if (n_ensembles == 1L){
    out <- fit_once(seed = NULL)
    return(out)
  }
  
  # Distinct seeds. Uses current RNG state; reproducible if caller set.seed() before.
  seeds <- base::sample.int(n = 1000000000L, size = n_ensembles, replace = FALSE)
  outs <- base::lapply(seeds, function(s) fit_once(seed = s))
  
  # Return list of models and fits
  models <- base::lapply(outs, `[[`, "model_nn")
  fits   <- base::lapply(outs, `[[`, "fit_nn")
  
  return(list(model_nn = models,
              fit_nn   = fits,
              seeds    = seeds))
  
}


build_lstm_sequences <- function(
    X_df,
    y_vec      = NULL,
    T,
    padding    = TRUE,
    pad_value  = -999
){
  
  # Sort and drop with month_id  
  if ("month_id" %in% colnames(X_df)) {
    X_df <- X_df[order(X_df$month_id), , drop = FALSE]
    X_df <- X_df[, setdiff(colnames(X_df), "month_id"), drop = FALSE]
  }
  # guard: all remaining columns must be numeric
  non_num <- colnames(X_df)[!vapply(X_df, is.numeric, logical(1L))]
  if (length(non_num) > 0L) {
    stop(
      "build_lstm_sequences: all features must be numeric. ",
      "Non-numeric columns: ", paste(non_num, collapse = ", ")
    )
  }
  
  #Convert as needed
  X <- base::as.matrix(X_df)
  n <- base::nrow(X); p <- base::ncol(X)
  
  if (n < 1L || p < 1L) {
    base::stop("build_lstm_sequences: empty X_df.")
  }
  if (!is.null(y_vec) && length(y_vec) != n) {
    base::stop("build_lstm_sequences: y_vec must have length nrow(X_df).")
  }
  
  
  if (isTRUE(padding)) {
    ## --- PADDING VERSION (build_sequences_pad) -------------------------
    X_seq <- base::array(pad_value, dim = c(n, T, p))
    
    for (t in base::seq_len(n)) {
      start <- base::max(1L, t - T + 1L)
      k     <- t - start + 1L
      X_seq[t, (T - k + 1L):T, ] <- X[start:t, , drop = FALSE]
    }
    
    y_out <- if (is.null(y_vec)) NULL else y_vec  # same length n
    
  } else {
    ## --- NO PADDING VERSION (.build_sequences_refit) -------------------
    if (n < T) {
      base::stop(
        "build_lstm_sequences (no padding): need at least sequence_length rows (",
        T, "), got ", n, "."
      )
    }
    n_seq <- n - T + 1L
    X_seq <- base::array(NA_real_, dim = c(n_seq, T, p))
    
    for (i in base::seq_len(n_seq)) {
      X_seq[i, , ] <- X[i:(i + T - 1L), , drop = FALSE]
    }
    
    y_out <- if (is.null(y_vec)) NULL else y_vec[T:n]
  }
  
  list(X = X_seq, y = y_out)
}


## LSTM helper
fit_lstm_model <- function(
  units, droprate, rec_droprate, lr, number_of_epochs, size_of_batch, 
  regularizer_l2, #Hyperparameters   
  early_stop = NULL, #Training
  eval_metric_trans,
  obj_fun_trans,
  covariates_seq_train, target_seq_train,  # X: (n, T, p); y: (n,)
  covariates_seq_val = NULL, target_seq_val = NULL,
  padding = TRUE, pad_value = -999
){
  
  ### Clear the session after each model training
  on.exit({
    keras::k_clear_session()
    gc()
  }, add = TRUE)
  
  ### Defensive checks
  stopifnot(is.array(covariates_seq_train), length(dim(covariates_seq_train)) == 3)
  stopifnot(length(target_seq_train) == dim(covariates_seq_train)[1])
  if (!is.null(covariates_seq_val)) {
    stopifnot(is.array(covariates_seq_val), length(dim(covariates_seq_val)) == 3)
    stopifnot(length(target_seq_val) == dim(covariates_seq_val)[1])
    stopifnot(dim(covariates_seq_val)[2] == dim(covariates_seq_train)[2])  # same T
    stopifnot(dim(covariates_seq_val)[3] == dim(covariates_seq_train)[3])  # same p
  }
  stopifnot(is.numeric(size_of_batch), size_of_batch >= 1)
  stopifnot(as.integer(size_of_batch) <= dim(covariates_seq_train)[1])
  size_of_batch <- as.integer(size_of_batch)
  
  ### Build
  model <- keras::keras_model_sequential()
  if (isTRUE(padding)) {
    # With padding: add masking so the LSTM ignores pad_value timesteps
    model <- model %>%
      keras::layer_masking(mask_value = pad_value) %>%
      keras::layer_lstm(
        units              = as.integer(units),
        input_shape        = c(
          base::dim(covariates_seq_train)[2],
          base::dim(covariates_seq_train)[3]
        ),
        dropout            = droprate,
        recurrent_dropout  = rec_droprate,
        kernel_regularizer = keras::regularizer_l2(regularizer_l2),
        return_sequences   = FALSE
      )
  } else {
    # No padding: no masking layer
    model <- model %>%
      keras::layer_lstm(
        units              = as.integer(units),
        input_shape        = c(
          base::dim(covariates_seq_train)[2],
          base::dim(covariates_seq_train)[3]
        ),
        dropout            = droprate,
        recurrent_dropout  = rec_droprate,
        kernel_regularizer = keras::regularizer_l2(regularizer_l2),
        return_sequences   = FALSE
      )
  }
  
  model <- model %>%
    keras::layer_dense(units = 1)
  
  ### Compile (loss can be huber or mse; metric must match monitor)
  model %>% keras::compile(
    loss      = obj_fun_trans,  # e.g., "mean_squared_error" or huber
    optimizer = keras::optimizer_adam(learning_rate = lr, clipnorm = 1.0),
    metrics   = eval_metric_trans$metric   # ensures val_mean_squared_error exists
  )
  
  ### Callback
  callbacks_list <- list()
  if (!is.null(early_stop)) {
    callbacks_list <- list(
      keras::callback_early_stopping(
        monitor              = eval_metric_trans$name,
        patience             = as.integer(early_stop),
        restore_best_weights = TRUE,
        mode                 = eval_metric_trans$mode
      ),
      keras::callback_reduce_lr_on_plateau(
        monitor  = eval_metric_trans$name,
        factor   = 0.5,
        patience = max(1L, floor(as.integer(early_stop)/2L)),
        min_lr   = 1e-6
      )
    )
  }
  
  ### Fit
  if (is.null(covariates_seq_val)) {
    history <- model %>% keras::fit(
      x = covariates_seq_train, y = target_seq_train,
      epochs = as.integer(number_of_epochs),
      batch_size = as.integer(size_of_batch),
      shuffle = FALSE,
      verbose = FALSE
    )
  } else {
    history <- model %>% keras::fit(
      x = covariates_seq_train, y = target_seq_train,
      epochs = as.integer(number_of_epochs),
      batch_size = as.integer(size_of_batch),
      shuffle = FALSE,
      verbose = FALSE,
      validation_data = list(covariates_seq_val, target_seq_val),
      callbacks = callbacks_list
    )
  }
  
  list(model = model, history = history)
}
  
  



## Hyper tuning
hyper_tune <- function(
    model, full_data_train_clean, covariates_val, target_val, #Data
    eval_fun, obj_fun_trans, #Eval Function and custom obj
    eval_metric_trans, early_stop, #Early Stop
    eval_metric, huber_delta, quantile_tau,  #Chosen eval metric
    hyper_grid_domain_list, n_iter, #Hyperparameter grid
    init_points, k_iter, acq, #Bayesian Optimization
    keras_architecture_pars, #Keras Parameters
    parallel, #Parallelization (default is true with future backend)
    verbose){ #Verbose
  
  ### Hyperparameter tuning following Bayesian Optimization!
  
  if(parallel){
    
    #### Check if doRNG is available (required by doFuture::withDoRNG)
    if (!requireNamespace("doRNG", quietly = TRUE)) {
      stop("The 'doRNG' package is required. Please install it.")
    }
    
    #### Bayesian Optimization
    bayes_opt <- doFuture::withDoRNG(
      ParBayesianOptimization::bayesOpt(
        #Passing variables to set_eval_fun
        FUN = eval_fun(
          
          #Data
          full_data_train_clean = full_data_train_clean, #full_data_train
          covariates_val = covariates_val, #Pass feat_val
          target_val = target_val, #Pass target_val
          
          #General Parameters
          model = model,
          
          #Eval Function Parameters
          eval_metric = eval_metric, #Chosen Eval
          eval_metric_trans = eval_metric_trans,
          huber_delta = huber_delta, #Huber delta for pseudo huber loss
          quantile_tau = quantile_tau, #Quantile tau for pinball loss
          
          #Early Stop
          early_stop = early_stop, #Halting criteria
          
          #Custom Loss
          obj_fun_trans = obj_fun_trans, #Custom objective
          
          #Keras Network Parameters
          keras_architecture_pars = keras_architecture_pars,
          
          verbose = FALSE
          
          #Future Implementation:
          #Functions for custom eval and loss - XGB
          #mpe_xgb <- mpe_xgb, #Custom mpe
          #rss_xgb = rss_xgb, #Custom rss
          #cp_xgb = cp_xgb, #custom cp
          #pinball_loss_xgb = pinball_loss_xgb #Custom pinball loss
          
        ),
        bounds = hyper_grid_domain_list, #Boundaries
        initPoints = init_points, #Number of randomly chosen points to 
        #sample the target function before B.O.
        acq = acq, #Acquisition function to be used
        iters.n = n_iter, #Number of times BO is to be repeated
        iters.k = k_iter, #Number of times to sample the scoring function
        #at each epoch. If running in parallel,
        #set iters.k to some multiple of the number of 
        #cores designated for the process
        verbose = verbose, #Display msgs?
        parallel = if (model %in% c("nn", "lstm")) FALSE else parallel #Parallel?
      )
    )
  } else { #In case of not PARALLEL
    
    bayes_opt <-
      ParBayesianOptimization::bayesOpt(
        #Passing variables to set_eval_fun
        FUN = eval_fun(
          
          #Data
          full_data_train_clean = full_data_train_clean, #full_data_train
          covariates_val = covariates_val, #Pass feat_val
          target_val = target_val, #Pass target_val
          
          #General Parameters
          model = model,
          
          #Eval Function Parameters
          eval_metric = eval_metric, #Chosen Eval
          eval_metric_trans = eval_metric_trans,
          huber_delta = huber_delta, #Huber delta for pseudo huber loss
          quantile_tau = quantile_tau, #Quantile tau for pinball loss
          
          #Early Stop
          early_stop = early_stop, #Halting criteria
          
          #Custom Loss
          obj_fun_trans = obj_fun_trans, #Custom objective
          
          #Keras Network Parameters
          keras_architecture_pars = keras_architecture_pars,
          
          verbose = TRUE
          
          #Future Implementation:
          #Functions for custom eval and loss - XGB
          #mpe_xgb <- mpe_xgb, #Custom mpe
          #rss_xgb = rss_xgb, #Custom rss
          #cp_xgb = cp_xgb, #custom cp
          #pinball_loss_xgb = pinball_loss_xgb #Custom pinball loss
          
        ),
        bounds = hyper_grid_domain_list, #Boundaries
        initPoints = init_points, #Number of randomly chosen points to 
        #sample the target function before B.O.
        acq = acq, #Acquisition function to be used
        iters.n = n_iter, #Number of times BO is to be repeated
        iters.k = k_iter, #Number of times to sample the scoring function
        #at each epoch. If running in parallel, 
        #set iters.k to some multiple of the number of 
        #cores designated for the process
        verbose = verbose, #Display msgs?
        parallel = parallel #Parallel?
      )
    
  }

  ### Store results
    #### Hyperparameters
    score_df <- as.data.frame(bayes_opt$scoreSummary)
    keep_cols <- intersect(
      colnames(score_df),
      c(names(hyper_grid_domain_list), "best_lam", "best_iter")
    )
    
    #### Create data frame to store combinations of hyperparameters tried
    eval_metric_val_current_date <- 
      score_df[, keep_cols, drop = FALSE] 
    
    #### Chosen Eval metric
    eval_metric_val_current_date$eval_metric <-
      as.numeric(score_df[[eval_metric]])
    
  #### Create expanded hyper grid list
  expanded_hyper_grid_list <- list() #Create expanded hyper_grid_list 
  for (j in seq_len(ncol(dplyr::select(eval_metric_val_current_date,
                                       -eval_metric)))){
    expanded_hyper_grid_list[[j]] <- #To each element, a column!
      dplyr::select(eval_metric_val_current_date, -eval_metric)[,j]
  }
  
  #### Get optimal values
  ##### Optimal Hyper Choice
  optimal_hyper <- unlist(ParBayesianOptimization::getBestPars(bayes_opt)) 
  
  ##### Add best lam
  try(optimal_hyper <- 
        c(optimal_hyper,
          best_lam = bayes_opt$scoreSummary$best_lam[
            which.max(bayes_opt$scoreSummary$Score)]
        ),
      silent = TRUE
  )
  
  ##### Add best_iteration
  try(optimal_hyper <- 
        c(optimal_hyper, 
          best_iter = bayes_opt$scoreSummary$best_iter[
            which.max(bayes_opt$scoreSummary$Score)
          ]),
      silent = TRUE
  )
  
  #### Assign val eval of optimal hyper choice
  val_eval_metrics_hyper_choice_current_date <-
    bayes_opt$scoreSummary[
      ##### Take the row that maximizes the score
      which.max(bayes_opt$scoreSummary$Score),
      c("Score", "rss", "cp", "rmse", "mae",
        "mphe", "mpe", "mape", "hr", "mb")
    ]
  
  ### Print Results
  if(verbose){
    cat(paste0("Chosen hyperparameters were: "))
    if(model != "glmnet"){
      cat(paste0(names(hyper_grid_domain_list),":",
                 round(optimal_hyper, 4), sep=" ")) 
    } else {
      cat(paste0(c(names(hyper_grid_domain_list), "best_lam"),":",
                 round(optimal_hyper, 4), sep=" "))
    } 
    cat("\n")
    cat(paste0("Validation eval_metrics for hyperparameters chosen were: "))
    cat(paste0(names(val_eval_metrics_hyper_choice_current_date),":",
               round(val_eval_metrics_hyper_choice_current_date,4), sep=" "))
    cat("\n")
  }
  
  return(list(
    eval_metric_val_current_date = eval_metric_val_current_date,
    optimal_hyper = optimal_hyper,
    val_eval_metrics_hyper_choice_current_date = val_eval_metrics_hyper_choice_current_date
  ))
  
}

## Fit RV model
fit_rv_model <- function(
    model, # Algorithm
    covariates_refit, target_refit, full_data_refit_clean = NULL, #Data
    obj_fun_trans, huber_delta, quantile_tau, 
    early_stop, keras_architecture_pars, #Model Parameters
    optimal_hyper = NULL, eval_metric_trans, #Validation Parameters
    n_ensembles = 1L,
    upper_quant_wins = 0.95, lower_quant_wins = 0.05, verbose){ #MISC
  
  ### Fit model based on 'model'
  fit <- switch(model,
                ## har
                har = stats::lm(
                  'rv_month ~.',
                  data = full_data_refit_clean
                ),
                
                ## GLMNET
                glmnet = glmnet::glmnet(
                  ### Features and target
                  covariates_refit[,-1, drop = FALSE], 
                  target_refit, 
                  ### Hyperparameters
                  alpha = optimal_hyper["alpha"],
                  lambda.min.ratio = optimal_hyper["lambda.min.ratio"],
                  verbose = verbose
                ),
                
                ## Ranger
                rf = ranger::ranger(
                  'rv_month ~.',
                  ### Features and target
                  data = janitor::clean_names(full_data_refit_clean), 
                  ### Hyperparameters
                  mtry = optimal_hyper["mtry"] * 
                    (ncol(full_data_refit_clean) - 1),
                  num.trees = optimal_hyper["num.trees"],
                  max.depth = optimal_hyper["max.depth"],
                  min.bucket = optimal_hyper["min.bucket"],
                  verbose = verbose
                ),
                
                ## XGB
                xgb = xgboost::xgb.train(
                  ### Features and target
                  data = xgboost::xgb.DMatrix(
                    data = as.matrix(covariates_refit[,-1,drop = FALSE]), 
                    label = target_refit),
                  objective = obj_fun_trans,
                  huber_slope = huber_delta,
                  #quantile_alpha = quantile_tau,
                  #Hyperparameters
                  min_child_weight = optimal_hyper["min_child_weight"],
                  max_depth = round(optimal_hyper["max_depth"],0),
                  subsample = optimal_hyper["subsample"],
                  colsample_bytree = optimal_hyper["colsample_bytree"],
                  eta = optimal_hyper["eta"],
                  alpha = optimal_hyper["alpha"],
                  gamma = optimal_hyper["gamma"],
                  nrounds = if (is.null(early_stop)){
                    c(optimal_hyper["nrounds"])
                  } else {
                    c(optimal_hyper["best_iter"])
                  },
                  verbose = verbose
                ),
                
                ## Keras
                nn = {
                  keras_res <- fit_keras_model(
                  ### Feature
                  covariates_matrix_train_clean = covariates_refit[,-1,drop = FALSE], 
                  ### Target
                  target_vector_train = target_refit, 
                  obj_fun_trans = obj_fun_trans, #No need for switch
                  huber_delta = huber_delta, 
                  eval_metric_trans = eval_metric_trans, 
                  ### Keras Parameters
                  #### Architecture
                  keras_architecture_pars = keras_architecture_pars,
                  n_ensembles             = n_ensembles,
                  
                  #### Hyperparameters
                  ##### Training
                  number_of_epochs = if(is.null(early_stop)){
                    c(optimal_hyper["number_of_epochs"])
                  } else {
                    c(optimal_hyper["best_iter"])
                  },
                  size_of_batch = optimal_hyper["size_of_batch"],
                  lr = optimal_hyper["lr"],
                  
                  ##### Regularization
                  regularizer_l1 = optimal_hyper["regularizer_l1"],
                  regularizer_l2 = optimal_hyper["regularizer_l2"],
                  droprate = optimal_hyper["droprate"],
                  
                  verbose = verbose
                )
                  # Store only the trained models, but keep metadata as attributes.
                  # (If n_ensembles==1L, model_nn is a single model; else it's a list of models.)
                  fit_obj <- keras_res$model_nn
                  if (!is.null(keras_res$seeds)) {
                    base::attr(fit_obj, "seeds") <- keras_res$seeds
                  }
                  if (n_ensembles > 1L) {
                    base::class(fit_obj) <- base::unique(c("keras_ensemble", base::class(fit_obj)))
                  }
                  if (n_ensembles > 1L && !base::is.list(keras_res$model_nn)) {
                    stop("n_ensembles > 1L but fit_keras_model() did not return a list of models.", call. = FALSE)
                  }
                  
                  fit_obj
                  
                },
                lstm = {
                  
                  ##### Guards
                  if (is.null(keras_architecture_pars) ||
                      is.null(keras_architecture_pars$sequence_length) ||
                      is.null(keras_architecture_pars$units)) {
                    stop("lstm: keras_architecture_pars must include sequence_length and units.")
                  }
                  padding_flag <- isTRUE(keras_architecture_pars$padding)
                  Tseq         <- base::as.integer(keras_architecture_pars$sequence_length)
                  units <- base::as.integer(keras_architecture_pars$units)
                  
                  ##### Build sequences from refit data
                  cov_refit_mat <- covariates_refit[, -1, drop = FALSE]  # remove id col
                  train_seq <- build_lstm_sequences(
                    X_df     = cov_refit_mat,   # covariates_refit[ , -1, drop = FALSE]
                    y_vec    = target_refit,
                    T        = Tseq,
                    padding  = padding_flag,
                    pad_value = -999
                  )
                  
                  ##### batch must not exceed number of sequences
                  n_seq_train <- base::dim(train_seq$X)[1]
                  bs <- base::as.integer(optimal_hyper["size_of_batch"])
                  if (bs > n_seq_train) bs <- n_seq_train
                  
                  ##### choose epochs (respect early_stop best_iter if present)
                  n_epochs <- if (is.null(early_stop)) {
                    base::as.integer(optimal_hyper["number_of_epochs"])
                  } else {
                    # during refit, run exactly best_iter (picked in tuning)
                    base::as.integer(optimal_hyper["best_iter"])
                  }
                  
                  ##### fit final LSTM
                  lstm_res <- fit_lstm_model(
                    units                = units,
                    droprate             = as.numeric(optimal_hyper["droprate"]),
                    rec_droprate         = as.numeric(optimal_hyper["rec_droprate"]),
                    lr                   = as.numeric(optimal_hyper["lr"]),
                    number_of_epochs     = n_epochs,
                    size_of_batch        = bs,
                    regularizer_l2       = as.numeric(optimal_hyper["regularizer_l2"]),
                    early_stop           = NULL,                        # no ES at refit
                    eval_metric_trans    = eval_metric_trans,
                    obj_fun_trans        = obj_fun_trans,
                    covariates_seq_train = train_seq$X,
                    target_seq_train     = train_seq$y
                    # no validation at refit
                  )
                  
                  lstm_res$model  # <- returned as `fit`
                }
  )
  
  ### Create S4 RV Model Object
  methods::new(
    "rv_model", 
    fit_obj                 = fit,
    covariates              = names(covariates_refit[,-1]),
    model_class             = if (model == "nn" && n_ensembles > 1L) "keras_ensemble" else base::class(fit),
    model                   = model,
    best_hyperparameters    = if (model %in% c("har")) NULL else optimal_hyper,
    obj_fun                 = obj_fun_trans,
    huber_delta             = huber_delta,
    keras_architecture_pars = keras_architecture_pars
  )

}


## Walk Forward Validation
run_walk_forward_validation <- function(
    target, covariates, model,
    hyper_grid_domain_list = list(),
    obj_fun = "mse", eval_metric = "rmse",
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
    parallel = TRUE,
    verbose = TRUE,
    .test_seed = 123
){
  
  elapsed_time <- system.time({

  
  ## Initial checks
  validator(
    target                  = target, 
    covariates              = covariates, 
    model                   = model,
    hyper_grid_domain_list  = hyper_grid_domain_list,
    obj_fun                 = obj_fun, 
    eval_metric             = eval_metric,
    train_n                 = train_n, 
    val_n                   = val_n,
    rebal_months            = rebal_months,
    early_stop              = early_stop,
    gsm_algo                = gsm_algo,
    keras_architecture_pars = keras_architecture_pars,
    parallel                = parallel
  )
  
  
  ## Print arguments
  has_tuning <- if (model != "har") TRUE else FALSE
  if (isTRUE(verbose)){
    cat("=============================\n")
    cat(crayon::cyan(paste("Model:", model)))
    cat("\n")
    cat(crayon::cyan(paste("Objective function:", obj_fun)))
    cat("\n")
    cat(crayon::cyan(paste("Evaluation metric:", eval_metric)))
    cat("\n")
    tictoc::tic("Total WF Validation Time")
    cat("=============================\n")
  }
  
  ## Translate metrics and obj fun
  adj_metrics <- translate_metrics(
    model = model, eval_metric = eval_metric, obj_fun = obj_fun,
    early_stop = early_stop, huber_delta = huber_delta,
    verbose = TRUE
  )
  obj_fun_trans     <- adj_metrics$obj_fun_trans
  eval_metric_trans <- adj_metrics$eval_metric_trans
  eval_metric       <- adj_metrics$eval_metric
  
  
  ## Get dates metrics
  dates            <- covariates %>% dplyr::pull(month_id)
  test_n           <- length(dates) - train_n - val_n + 1
  dates_test       <- dates[(train_n + val_n):
                              (train_n + val_n + test_n - 1)]
  first_rebal_date  <- min(dates_test)
  rebal_dates       <- unique(
    c(first_rebal_date,
      dates_test[which(lubridate::month(dates_test) %in% rebal_months)])
  ) %>% sort() 
  n_rebal_months    <- length(rebal_dates)  
  
  ## Create placeholders
    ### Hypers
    if (has_tuning && length(rebal_dates) > 0){
      
      ### Store hyperparameters for each rebalancing date
      eval_metric_val <- list()
      
      ### Store validation eval
      val_eval_metrics_hyper_choice <- xts::xts(data.frame(
        rss  = as.vector(rep(NA, n_rebal_months)), #R2
        cp   = as.vector(rep(NA, n_rebal_months)), #CP
        rmse = as.vector(rep(NA, n_rebal_months)), #Root Mean Squared Error
        mae  = as.vector(rep(NA, n_rebal_months)), #Mean Absolute Error
        mphe = as.vector(rep(NA, n_rebal_months)), #Mean Pseudo huber
        mpe  = as.vector(rep(NA, n_rebal_months)), #Mean Pinball Error
        mape = as.vector(rep(NA, n_rebal_months)), #Mean Absolute Percentage Error
        hr   = as.vector(rep(NA, n_rebal_months)), #Hit Rate
        mb   = as.vector(rep(NA, n_rebal_months)) #Mean Bias
      ), order.by = rebal_dates)
      
      ### Store hyper choice
      hyper_choice <- xts::xts(as.data.frame(
        matrix(NA, nrow = n_rebal_months, ncol = length(hyper_grid_domain_list))),
        order.by = rebal_dates)
      colnames(hyper_choice) <- 
        names(hyper_grid_domain_list) #Set colnames as hyperparameters
      
      ###Add best-lam and best-iteration
      hyper_choice$best_lam       <- if(model == "glmnet") NA
      hyper_choice$best_iter      <- if(!is.null(early_stop)) NA
      
    } else {
      eval_metric_val               <- NULL
      val_eval_metrics_hyper_choice <- NULL
      hyper_choice                  <- NULL
    }
  
    ###Prediction, error and Y objects
    oos_pred         <- vector(length = length(dates_test)) #initialize prediction 
    names(oos_pred)  <- as.character(dates_test)
    oos_error        <- vector(length = length(dates_test)) #Initialize error
    names(oos_error) <- as.character(dates_test)
    oos_y            <- vector(length = length(dates_test)) #Initialize y 
    names(oos_y)     <- as.character(dates_test)
    
    ###Feature importance
    feat_imp_list <- list()
  
  ## Start WF loop--------------------------------------------------------------
  for (d in (train_n + val_n):(train_n + val_n + test_n - 1)){
    
    ### Get current date
    current_date <- dates[d]
    if (isTRUE(verbose)){
      cat("\n")
      cat(crayon::blue(paste("Current date:", current_date)))
      cat("\n")
      if (d == train_n + val_n) tictoc::tic("WF Loop Time")
    }

    ### Rebal if it is rebal month
    is_rebal_month <- current_date %in% rebal_dates
    if (isTRUE(is_rebal_month)){
      if (isTRUE(verbose)){
        cat("\n")
        cat(crayon::yellow(paste("Starting model rebal at:", current_date)))
        cat("\n")
        tictoc::tic("Rebalancing Time")
      }
      
      #### Split data
      ts_splits <- split_ts(
        covariates   = covariates,
        target       = target,
        dates        = dates,
        current_date = current_date,
        train_n      = train_n,
        val_n        = val_n
      ) 
      
      #### Defensive checks
        ##### There is no date overlap between train and val
        if (isTRUE(has_tuning)){
          stopifnot(
            ###### Covariates train and covariates val
            dplyr::intersect(ts_splits$covariates_train$month_id,
                             ts_splits$covariates_val$month_id) %>% 
              length() == 0
          )
        }
        #### LSTM sequence_length feasibility on the split (cheap runtime guard)
        if (model == "lstm") {
          lb <- as.integer(keras_architecture_pars$sequence_length)
          # ensure each split has at least sequence_length+1 rows (sequence + target)
          stopifnot(
            base::nrow(ts_splits$covariates_train)  > lb,
            base::nrow(ts_splits$covariates_refit)  > lb
          )
          if (isTRUE(has_tuning)) {
            stopifnot(base::nrow(ts_splits$covariates_val) > lb)
          }
        }
      
        ##### current_date is not present in any obj
        stopifnot(
          !(current_date %in% ts_splits$covariates_train$month_id),
          !(current_date %in% ts_splits$covariates_refit$month_id)
        )
        
        if (isTRUE(has_tuning)){
          stopifnot(
            !(current_date %in% ts_splits$covariates_val$month_id)
          ) 
        }
        
        #### Print date intervals in each object
        if (isTRUE(verbose)){
          cat(crayon::blue("Date ranges in each object:"))
          cat("\n")
          cat(crayon::blue(paste0("Train: ",
                                  min(ts_splits$covariates_train$month_id),
                                  " to ",
                                  max(ts_splits$covariates_train$month_id))))
          cat("\n")
          if (isTRUE(has_tuning)){
            cat(crayon::blue(paste0("Validation: ",
                                    min(ts_splits$covariates_val$month_id),
                                    " to ",
                                    max(ts_splits$covariates_val$month_id))))
            cat("\n")
          }
          cat(crayon::blue(paste0("Refit: ",
                                  min(ts_splits$covariates_refit$month_id),
                                  " to ",
                                  max(ts_splits$covariates_refit$month_id))))
          cat("\n")
        }
        
      #### Hyper Tune-----------------------------------------------------------
      if (isTRUE(has_tuning)){
        
        ##### Set seed based on model
        if (model %in% c("nn", "lstm")){
          if (is.na(.test_seed)){
            seed_current <- as.integer(
              as.numeric(as.Date(current_date)) %% .Machine$integer.max
            )
            tensorflow::set_random_seed(seed_current)
            
          } else {
            tensorflow::set_random_seed(.test_seed)
          }
          
        }  else {
          set.seed(.test_seed)
        }
        
        ##### Get splits
        full_data_train_clean <- ts_splits$full_data_train_clean  
        covariates_val        <- ts_splits$covariates_val
        target_val            <- ts_splits$target_val
        
        ##### Set eval function
        eval_fun <- set_eval_function(model = model)
        
        ##### Run hyperparameter tuning
        if (isTRUE(verbose)){
          cat(crayon::blue(paste0("Hyper tuning at: ",
                                  current_date, "...")))
          cat("\n")
        }
        
        hyper_tune_res <- hyper_tune(
          model = model, # Model
          full_data_train_clean = full_data_train_clean, # Data
          covariates_val = covariates_val, # Data
          target_val = target_val, # Data
          eval_fun = eval_fun, obj_fun_trans = obj_fun_trans,
          eval_metric_trans = eval_metric_trans, # Eval and Obj
          early_stop = early_stop, # Early Stop
          eval_metric = eval_metric, huber_delta = huber_delta,
          quantile_tau = quantile_tau,  # Eval metric
          hyper_grid_domain_list = hyper_grid_domain_list,
          n_iter = n_iter, #Hyperparameter grid
          init_points = init_points, k_iter = k_iter,
          acq = acq, #Bayesian Optimization
          keras_architecture_pars = keras_architecture_pars, #Keras Pars
          parallel = parallel, #Parallelization (default is future backend)
          verbose = TRUE
        )
        
        ##### Store results
        
        ###### Fill chosen_eval_metric_validation
        eval_metric_val[[which(rebal_dates == current_date)]] <- 
          hyper_tune_res$eval_metric_val_current_date
        
        ###### Get Optimal Hypers and fill hyper_choice
        optimal_hyper <- hyper_tune_res$optimal_hyper
        hyper_choice[current_date, ] <- 
          optimal_hyper[colnames(hyper_choice)] #Get the row corresp to  
        #rebal date and replace 
        #with correct order
        
        ######  Fill validation_eval_metrics_hyper_choice
        val_eval_metrics_hyper_choice[current_date, ] <- 
          hyper_tune_res$val_eval_metrics_hyper_choice_current_date %>%
          dplyr::select(colnames(val_eval_metrics_hyper_choice)) %>% 
          as.numeric() #Turn into numeric
        
        cat(crayon::blue(paste0("Hyper tuning at: ",
                                current_date, " finished.")))
        
      } else {
        optimal_hyper <- NULL #Set optimal hyper as NULL for har
      }
      
   
      
      #### (Re)fitting----------------------------------------------------------
      
        #### Get splits  
        covariates_refit      <- ts_splits$covariates_refit
        target_refit          <- ts_splits$target_refit
        full_data_refit_clean <- ts_splits$full_data_refit_clean
        
        #### (Re)fit model
        if (isTRUE(verbose)){
          cat(crayon::blue(paste0("(Re)Fitting model at: ",
                                  current_date, "...")))
          cat("\n")
          tictoc::tic()
        }
        
        rv_model_fit <- fit_rv_model(
          model = model, # Algorithm
          covariates_refit        = covariates_refit,
          target_refit            = target_refit,
          full_data_refit_clean   = full_data_refit_clean, # Data
          obj_fun_trans           = obj_fun_trans,
          huber_delta             = huber_delta,
          quantile_tau            = quantile_tau,
          early_stop              = early_stop,
          keras_architecture_pars = keras_architecture_pars, # Model Pars
          optimal_hyper           = optimal_hyper,
          eval_metric_trans       = eval_metric_trans,
          upper_quant_wins        = upper_quant_wins, 
          lower_quant_wins        = lower_quant_wins,
          verbose                 = verbose
        )
        
        if (isTRUE(verbose)){
        cat(crayon::blue(paste0("(Re)Fitting model at: ",
                                current_date, " finished.")))
        cat("\n")
        tictoc::toc()
        }
        
      #### Feature importance---------------------------------------------------    
      
        ##### Join predictions to design matrix
        covariates_preds_refit <- covariates_refit %>%
          dplyr::mutate(
            preds = predict(rv_model_fit, covariates_refit)
          )
        
        ##### Fit a global surrogate model
        gsm <- switch(
          gsm_algo,
          #Fit OLS Global Surrogate Model
          ols  = stats::lm(preds ~ ., covariates_preds_refit[,-1]),
          #Fit Tree Global Surrogate Model
          tree = rpart::rpart(preds ~ ., data = covariates_preds_refit[,-1]) 
        )
        
        ##### Get feature importance
        feat_imp <- dplyr::full_join(
          # Full join to get both intercept and all feats
          switch(gsm_algo,
                 ols = withCallingHandlers({
                   summary(gsm)$coef %>%
                     as.data.frame() %>%
                     dplyr::mutate(feat = rownames(.), .before = Estimate) %>%
                     #Remove ` from feat to avoid double counting
                     dplyr::mutate(feat = gsub("`", "", feat)) %>% 
                     tibble::remove_rownames() %>% # Place rownames as columns
                     dplyr::select(feat, Estimate) %>% #Get only feat and coefs
                     dplyr::rename(imp = Estimate) # Rename columns
                 },
                 warning = function(w) {
                   #Supress this warning, 
                   #which is intrinsic to using ols to explain ols
                   if (grepl("essentially perfect fit: summary may be unreliable",
                             conditionMessage(w))) {
                     invokeRestart("muffleWarning")
                   }
                 }),
                 tree = data.frame(imp = gsm$variable.importance) %>%
                   dplyr::mutate(feat = rownames(.), .before = imp) %>%
                   tibble::remove_rownames()
          ),
          # Join to get all features even if 0 imp
          data.frame(feat = colnames(covariates_refit[,-1, drop = FALSE])),
          by = "feat"
        ) %>%
          dplyr::mutate(
            norm_imp = 
              if (is.na(sd(imp, na.rm = TRUE)) || sd(imp, na.rm = TRUE) == 0){0}
              else {(imp - mean(imp, na.rm = TRUE)) / sd(imp, na.rm = TRUE)},
            .after = imp
          ) %>% # Normalize importance
          dplyr::mutate(
            dplyr::across(dplyr::where(is.numeric), ~ tidyr::replace_na(., 0))
          ) %>% # Replace NAs with 0s
          dplyr::mutate(dates = current_date, .before = imp) %>% # Add  date
          dplyr::mutate(id = paste0(feat, "-", dates), .before = feat) %>% # Add id
          dplyr::arrange(id) # Arrange by id
        
        
        ##Save feature_importance_m_d_ref
        feat_imp_list[[which(rebal_dates == current_date)]] <- feat_imp
        
    } 
    
    ### Memory cleanup block and print rebal time
    suppressWarnings(
      rm(
        full_data_train_clean,
        covariates_val,
        target_val,
        ts_splits,
        covariates_refit,
        full_data_refit_clean,
        target_refit,
        covariates_preds_refit
      )
    )
    gc()
    if (isTRUE(is_rebal_month) && isTRUE(verbose)){
      tictoc::toc()
    }
    
    ### OOS Prediction---------------------------------------------------------
    if (isTRUE(verbose)){
      cat("\n")
      cat(crayon::green(paste("Starting OOS prediction for date:",
                              current_date)))
      cat("\n")
    }
    
     #### Test reference
     test_ref <- d - train_n - val_n + 1

     #### Get covariates and target for current date
     target_test     <- target %>% 
       dplyr::filter(month_id == current_date) %>%
       dplyr::pull(rv_month)
     
     if (model != "lstm"){
       covariates_test <- covariates %>% 
         dplyr::filter(month_id == current_date)
     } else {
       kap   <- rv_model_fit@keras_architecture_pars
       Tseq  <- kap$sequence_length
       
       #### Build the T-length test window ending at current_date
       covariates_test <- covariates %>%
         dplyr::filter(month_id <= current_date) %>%
         dplyr::arrange(month_id) %>%
         dplyr::slice_tail(n = Tseq)
       
       # Defensive check
       if (nrow(covariates_test) < Tseq) {
         stop(
           "Not enough observations to build an LSTM test window at date ",
           current_date, ". Needed ", Tseq, " rows, found ", nrow(covariates_test), "."
         )
       }
     }
     
     #### Make predictions
     pred_out <- predict(rv_model_fit, new_covariates = covariates_test)
     
       ##### For LSTM + no padding: use the last element (the one for current_date)
       if (model == "lstm" && !isTRUE(rv_model_fit@keras_architecture_pars$padding)) {
         pred <- tail(pred_out, 1L)
       } else {
         pred <- pred_out
       }

     oos_pred[test_ref] <- as.numeric(pred)
     oos_y[test_ref]    <- target_test 
       
  }
    
  ## End of WF loop-------------------------------------------------------------
    
    ### Print total WF time
    if (isTRUE(verbose)){
      cat(crayon::green("WF Validation finished."))
      cat("\n")
      tictoc::toc()
    }
  
    ### Calculate eval metrics and error
    testing_metrics <- calc_eval_metrics(
      pred   = oos_pred,
      target = oos_y,
      huber_delta = huber_delta,
      quantile_tau = quantile_tau,
      eval_metric = eval_metric,
      return_error = TRUE
      )
  
      #### Transform vector into data.frame in which names is a column
      eval_metrics <- testing_metrics$df_eval_metrics %>%  
        tibble::as_tibble() %>%
        tidyr::pivot_longer(dplyr::everything(), 
                            names_to = "metric",
                            values_to = "cons_oos") %>%
        dplyr::filter(metric != "Score")
    
    ### Consolidate oos y, pred and error into a single tibble
    oos_error <- as.numeric(testing_metrics$error)
    
    oos_outputs <- data.frame(
      month_id = as.Date(names(oos_y)),
      target   = as.numeric(oos_y),
      pred     = as.numeric(oos_pred),
      error    = as.numeric(oos_error)
    ) %>% tibble::as_tibble()
    
  
    ### Validation metrics
    if (has_tuning && length(rebal_dates) > 0){
      #### Assign names to eval_metric_val list
      names(eval_metric_val) <- rebal_dates
       
      #### Validation Summary
      avg_val_eval_metrics_hyper_choice <- data.frame(
        metric  = colnames(val_eval_metrics_hyper_choice),
        avg_val = colMeans(val_eval_metrics_hyper_choice)
      ) %>% tibble::as_tibble()
      
        ##### Join with oos eval
        eval_metrics <- eval_metrics %>%
          dplyr::left_join(avg_val_eval_metrics_hyper_choice, by = "metric")
    }
        
    ### Feature Importance
      #### Bind individual feat importance into data.frame
      feat_imp       <- do.call(rbind, feat_imp_list) %>%
        dplyr::arrange(id) %>%
        tibble::as_tibble()
      final_feat_imp <- feat_imp %>% dplyr::filter(dates == max(dates))
      
  }) # End of elapsed time    
     
    ### Workflow list
      #### Create a workflow obj that contains all the arguments used
      backtest_meta <- list(
        model                   = model,
        obj_fun                 = obj_fun,
        eval_metric             = eval_metric,
        train_n                 = train_n,
        val_n                   = val_n,
        rebal_months            = rebal_months,
        hyper_grid_domain_list  = hyper_grid_domain_list,
        early_stop              = early_stop,
        gsm_algo                = gsm_algo,
        dates                   = dates,
        dates_test              = dates_test,
        rebal_dates             = rebal_dates,
        last_rebal_date         = if (length(rebal_dates) > 0) max(rebal_dates) else NA,
        n_rebal_months          = n_rebal_months,
        huber_delta             = huber_delta,
        quantile_tau            = quantile_tau,
        keras_architecture_pars = keras_architecture_pars,
        n_obs                   = length(dates),
        n_test                  = length(dates_test),
        n_rebals                = length(rebal_dates),
        has_tuning              = has_tuning,
        n_iter                  = n_iter,
        init_points             = init_points,
        k_iter                  = k_iter,
        acq                     = acq,
        parallel                = parallel,
        timestamp               = Sys.time(),
        elapsed_time            = elapsed_time
      )
     
      
      ### Create a backtest results S4 object with main results
      backtest_id <- paste0(model, "_", 
                            gsub(":", "-", gsub(" ", "_", Sys.time())), "_",
                            eval_metric, "_",
                            obj_fun)
        
      methods::new(
        "wf_backtest_results",
        oos_outputs                   = oos_outputs,
        eval_metrics                  = eval_metrics,
        final_rv_model                = rv_model_fit,
        final_gsm                     = gsm,
        eval_metric_val               = eval_metric_val,
        hyper_choice                  = hyper_choice,
        val_eval_metrics_hyper_choice = val_eval_metrics_hyper_choice,
        feat_imp                      = feat_imp,
        final_feat_imp                = final_feat_imp,
        backtest_meta                 = backtest_meta,
        backtest_id                   = backtest_id
        )
     
  
}