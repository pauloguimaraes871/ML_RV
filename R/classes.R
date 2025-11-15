#rv_model-----------------------------------------------------------------------

#' @export
setClass(
  "rv_model",
  slots = list(
    fit_obj                 = "ANY",
    covariates              = "character",
    model_class             = "character",
    model                   = "character",
    best_hyperparameters    = "ANY",
    obj_fun                 = "ANY",
    huber_delta             = "numeric",
    keras_architecture_pars = "ANY"
  )
)

#' @export
setMethod("predict",
          signature = list(object = "rv_model"),
          function(object, new_covariates) {
            
            # Validate inputs -------------------------------------------------
            if (!is.data.frame(new_covariates)) {
              stop("new_covariates must be a data.frame or tibble.")
            }
            if (!"month_id" %in% colnames(new_covariates)) {
              stop("new_covariates must contain a 'month_id' column.")
            }
              ## keep a copy of original names
              original_cols <- colnames(new_covariates)
            
              ## Features expected by the fitted model
              feats <- object@covariates
              if (length(feats) == 0L) {
                stop("object@covariates is empty")
              }
              
              ## check presence 
              missing_feats <- setdiff(
                if (object@model=="rf"){
                  janitor::make_clean_names(object@covariates) 
                } else object@covariates,
                colnames(if (object@model=="rf"){
                  janitor::clean_names(new_covariates)
                }  else new_covariates)
              )
              if (length(missing_feats) > 0L) {
                stop(
                  sprintf("Missing required features in new_covariates: %s",
                          paste(missing_feats, collapse = ", "))
                )
              }
              
            # Prepare inputs----------------------------------------------------
              ## Remove ids and re-order
              new_data <- if (object@model == "rf") {
                janitor::clean_names(new_covariates)[
                  , janitor::make_clean_names(object@covariates),
                  drop = FALSE]
              } else {
                new_covariates %>% 
                  dplyr::select(dplyr::all_of(object@covariates))
              }
              
              ## Get parameters
              model         <- object@model
              optimal_hyper <- object@best_hyperparameters
              if (model == "glmnet") {
                if (is.null(optimal_hyper) || 
                    !"best_lam" %in% names(optimal_hyper)) {
                  stop("best_lam not found in object for glmnet model.")
                }
                best_lam <- as.numeric(optimal_hyper["best_lam"])
              }
              fit_obj <- object@fit_obj
    
            #Choose predict method----------------------------------------------
              
              ##Generate predictions
              predictions <- switch(
                model, #Depending on the algorithm
                har    = as.numeric(
                  stats::predict(fit_obj, newdata = as.data.frame(new_data))
                  ), 
                glmnet = as.numeric(
                  predict(fit_obj, newx = as.matrix(new_data), s = best_lam)
                  ), 
                rf     = as.numeric(
                  predict(fit_obj, data = new_data)$predictions
                  ), 
                xgb    = as.numeric(
                  predict(fit_obj, newdata = as.matrix(new_data))
                  ), 
                nn     = as.numeric(
                  predict(fit_obj, x = as.matrix(new_data))
                  )
              )
            ################
            
            return(predictions)
          })


#' Show Method for rv_model Class
#'
#' @param object An instance of the `rv_model` class.
#' @return The object invisibly.
#' @export
setMethod("show", "rv_model", function(object) {
  cat("RV Model Summary:\n")
  cat("=================================\n")
  
  # Algorithm / class
  cat("Model: ", object@model, "\n")
  cat("Model class: ", object@model_class, "\n")
  
  # Hyperparameters
  cat("Best Hyperparameters:\n")
  if (length(object@best_hyperparameters) > 0) {
    print(base::round(object@best_hyperparameters, 5))
  } else {
    cat("No hyperparameters available.\n")
  }
  
  # Objective / loss
  cat("Objective (training space): ")
  if (!is.null(object@obj_fun)) {
    print(object@obj_fun)
  } else {
    cat("None\n")
  }
  
  # Huber delta
  cat("Huber Delta: ", object@huber_delta, "\n")
  
  # Keras architecture (if any)
  if (!is.null(object@keras_architecture_pars)) {
    cat("Keras Architecture Parameters:\n")
    print(object@keras_architecture_pars)
  } else {
    cat("No Keras architecture parameters specified.\n")
  }
  
  # Covariates used
  cat("Covariates (features):\n")
  if (length(object@covariates) > 0) {
    print(object@covariates)
  } else {
    cat("None\n")
  }
  
  cat("=================================\n")
  cat("Model Object:\n\n")
  if (!is.null(object@fit_obj)) {
    print(object@fit_obj)
  } else {
    cat("No model object available.\n")
  }
  
  invisible(object)
})

#' @title Plot Method for 'rv_model' Objects
#' @description Dispatch plotting to the underlying fitted model stored
#'  in \code{fit_obj}.
#'
#' @param x An object of class \code{rv_model}.
#' @param type Currently unused.
#' @param ... Passed to the underlying model's plot method.
#'
#' @export
setMethod(
  "plot",
  signature(x = "rv_model", y = "missing"),
  function(x, type = NULL, ...) {
    if (is.null(x@fit_obj)) stop("No fitted model in 'fit_obj'.")
    plot(x@fit_obj, ...)
  }
)


#wf_backtest_results------------------------------------------------------------
#' Walk-Forward Backtest Results (RV)
#'
#' @export
setClass(
  "wf_backtest_results",
  slots = list(
    oos_outputs                   = "data.frame",  
    eval_metrics                  = "data.frame",
    final_rv_model                = "ANY",         # rv_model or NULL
    final_gsm                     = "ANY",         # lm/rpart or NULL
    eval_metric_val               = "ANY",         # list or NULL
    hyper_choice                  = "ANY",         # xts or NULL
    val_eval_metrics_hyper_choice = "ANY",         # xts or NULL
    feat_imp                      = "ANY",         # data.frame/tibble or NULL
    final_feat_imp                = "ANY",         # data.frame/tibble or NULL
    backtest_meta                 = "list",
    backtest_id                   = "character"
  ),
  validity = function(object){
    
    # oos & eval tables
    if (!is.data.frame(object@oos_outputs)) {
      return("oos_outputs must be a data.frame (tibble allowed).")
    }
    if (!is.data.frame(object@eval_metrics)) {
      return("eval_metrics must be a data.frame (tibble allowed).")
    }
    
    # final model
    if (!is.null(object@final_rv_model) &&
        !methods::is(object@final_rv_model, "rv_model")) {
      return("final_rv_model must be an 'rv_model' object.")
    }

    # GSM (lm or rpart)
    if (!is.null(object@final_gsm) &&
        !class(object@final_gsm) %in% c("lm", "rpart")) {
      return("final_gsm must be a 'lm' or 'rpart' object.")
    }
    
    # eval_metric_val list
    if (!is.null(object@eval_metric_val) &&
        !is.list(object@eval_metric_val)) {
      return("eval_metric_val must be a list.")
    }
    
    # xts holders
    if (!is.null(object@hyper_choice) &&
        !inherits(object@hyper_choice, "xts")) {
      return("hyper_choice must be an 'xts' object.")
    }
    if (!is.null(object@val_eval_metrics_hyper_choice) &&
        !inherits(object@val_eval_metrics_hyper_choice, "xts")) {
      return("val_eval_metrics_hyper_choice must be an 'xts' object.")
    }
    
    # feature importance frames
    if (!is.null(object@feat_imp) &&
        !is.data.frame(object@feat_imp)) {
      return("feat_imp must be a data.frame (tibble allowed).")
    }
    if (!is.null(object@final_feat_imp) &&
        !is.data.frame(object@final_feat_imp)) {
      return("final_feat_imp must be a data.frame (tibble allowed).")
    }
    
    # backtest_meta
    if (!is.list(object@backtest_meta)) {
      return("backtest_meta must be a list.")
    }
    
    # id
    if (length(object@backtest_id) != 1L) {
      return("backtest_id must be a length-1 character vector.")
    }
    
    TRUE
  }
)


#' @export
setMethod("show", "wf_backtest_results", function(object) {
  bm <- object@backtest_meta
  
  cat("WF Backtest Summary\n")
  cat("Backtest ID: ", object@backtest_id, "\n")
  cat("=================================\n")
  
  # Model / objective
  cat("Model: ", bm$model, "\n")
  cat("Objective: ", bm$obj_fun, "\n")
  cat("Eval metric: ", bm$eval_metric, "\n")
  cat("Final model class: ",
      if (!is.null(object@final_rv_model)){
        object@final_rv_model@model_class
      }  else "NULL", "\n")
  
  # Samples / dates
  cat("=================================\n")
  cat("Obs (total/test): ", bm$n_obs, " / ", bm$n_test, "\n", sep = "")
  cat("Train_n / Val_n: ", bm$train_n, " / ", bm$val_n, "\n", sep = "")
  cat("Date range: ", as.character(min(bm$dates)),
      " to ", as.character(max(bm$dates)), "\n", sep = "")
  cat("Test range: ",
      if (length(bm$dates_test)){
        paste0(as.character(min(bm$dates_test)), " to ", 
               as.character(max(bm$dates_test)))
      }  else "NA", "\n")
  
  # Rebal
  cat("Rebal months: {", paste(bm$rebal_months, collapse = ","), "}\n", sep = "")
  cat("Rebal count: ", bm$n_rebals, "\n", sep = "")
  cat("Last rebal: ", if (!is.null(bm$last_rebal_date)){
    as.character(bm$last_rebal_date) 
  } else "NA", "\n", sep = "")
  
  # Tuning
  cat("=================================\n")
  cat("Tuning: ", if (isTRUE(bm$has_tuning)) "ON" else "OFF", "\n", sep = "")
  if (isTRUE(bm$has_tuning)) {
    cat(" n_iter/init/k_iter: ", bm$n_iter, "/", bm$init_points, "/",
        bm$k_iter, "\n", sep = "")
    cat(" acq: ", bm$acq, " | early_stop: ",
        if (is.null(bm$early_stop)) "NULL" else bm$early_stop, "\n", sep = "")
  }
  
  # Performance snapshot
  cat("=================================\n")
  cat("Elapsed time: ", as.character(bm$elapsed_time["elapsed"]), "\n", sep = "")
  cat("Timestamp: ", as.character(bm$timestamp), "\n", sep = "")
  
  # Metrics (compact)
  if (is.data.frame(object@eval_metrics) && nrow(object@eval_metrics) > 0) {
    cat("=================================\n")
    cat("Eval metrics (OOS):\n")
    print(utils::head(object@eval_metrics, 10))
  }
  
  invisible(object)
})

#Theme for plot
theme_article <- function(base_size = 11) {
  ggplot2::theme_bw(base_size) %+replace%
    ggplot2::theme(
      plot.title       = ggplot2::element_text(face = "bold"),
      plot.subtitle    = ggplot2::element_text(color = "grey30"),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_line(color = "grey85", linewidth = 0.3),
      legend.position  = "bottom"
    )
}
pal_cbb <- c("#000000","#E69F00","#56B4E9","#009E73",
             "#F0E442","#0072B2","#D55E00","#CC79A7")


# small infix
`%||%` <- function(a,b) if (!is.null(a)) a else b

#' @export
setMethod("plot", signature(x="wf_backtest_results", y="missing"),
          function(x, plot_id=NULL, theme_map=NULL, metrics_to_plot = NULL){
            
            `%||%` <- function(a,b) if (!is.null(a)) a else b
            req <- c("ggplot2","dplyr","tidyr","scales","viridis","rlang","xts","zoo")
            mis <- req[!vapply(req, requireNamespace, logical(1), quietly=TRUE)]
            if (length(mis)) stop("Missing packages: ", paste(mis, collapse=", "), call.=FALSE)
            tm <- ggplot2::theme_minimal(base_size=12)
            
            add_theme <- function(fi, theme_map){
              if (is.null(theme_map)) stop("Provide theme_map with cols: feat, theme.")
              if (!all(c("feat","theme") %in% names(theme_map)))
                stop("theme_map must have cols: feat, theme.")
              if (!"feat" %in% names(fi)) stop("feat_imp must have 'feat'.")
              dplyr::left_join(fi, theme_map %>% dplyr::distinct(feat, theme), by="feat")
            }
            coerce_with_dates <- function(obj){
              if (xts::is.xts(obj)){
                df <- base::as.data.frame(obj); df$dates <- as.Date(zoo::index(obj))
                rownames(df) <- NULL; df
              } else if (is.data.frame(obj) && "dates" %in% names(obj)){
                obj$dates <- as.Date(obj$dates); obj
              } else if (is.data.frame(obj) && "month_id" %in% names(obj)){
                obj$dates <- as.Date(obj$month_id); obj
              } else NULL
            }
            # inline metrics (match your calc_eval_metrics names)
            metric_vec <- function(pred, target, huber_delta=1, tau=.5){
              eps <- 1e-12
              e   <- target - pred
              sst <- sum((target - mean(target))^2)
              c(
                rss  = if (sst < eps) NA_real_ else 1 - sum(e^2)/sst,
                cp   = mean(pred*target),
                rmse = sqrt(mean(e^2)),
                mae  = mean(abs(e)),
                mphe = mean(huber_delta^2*(sqrt(1 + (e/huber_delta)^2) - 1)),
                mpe  = mean(ifelse(e>=0, tau*e, (1-tau)*(-e))),
                mape = mean(abs(e)/pmax(abs(target), eps)),
                hr   = mean((pred>=0) & (target>=0)),
                mb   = mean(e)
              )
            }
            # cumulative metrics at cut dates
            cum_metrics <- function(oos_df, cuts, huber_delta=1, tau=.5){
              if (is.null(cuts) || !length(cuts)) stop("No rebal_dates.")
              cd <- as.Date(cuts)
              cd <- cd[cd >= min(oos_df$dates) & cd <= max(oos_df$dates)]
              if (!length(cd)) stop("rebal_dates outside OOS range.")
              do.call(rbind, lapply(cd, function(d){
                sub <- oos_df %>% dplyr::filter(dates <= d)
                as.data.frame(t(metric_vec(sub$pred, sub$target, huber_delta, tau)),
                              check.names=FALSE) %>% dplyr::mutate(dates=d, .before=1)
              }))
            }
            
            # slots/meta
            meta          <- x@backtest_meta %||% list()
            rebal_dates   <- tryCatch(as.Date(meta$rebal_dates), error=function(e) NULL)
            chosen_metric <- tryCatch(meta$eval_metric, error=function(e) "rmse") %||% "rmse"
            huber_delta   <- tryCatch(meta$huber_delta,  error=function(e) 1)
            quantile_tau  <- tryCatch(meta$quantile_tau, error=function(e) .5)
            
            if (is.null(x@oos_outputs) ||
                !all(c("month_id","pred","target") %in% names(x@oos_outputs)))
              stop("x@oos_outputs must have month_id, pred, target.")
            oos_df <- x@oos_outputs %>%
              dplyr::mutate(dates=as.Date(month_id)) %>% dplyr::arrange(dates)
            
            val_eval <- coerce_with_dates(tryCatch(x@val_eval_metrics_hyper_choice,
                                                   error=function(e) NULL))
            val_plain <- if (!is.null(val_eval) && nrow(val_eval)){
              val_eval %>% dplyr::rename_with(~sub("^validation_","",.x),
                                              dplyr::starts_with("validation_"))
            } else NULL
            
            cons_metrics <- tryCatch(x@eval_metrics, error=function(e) NULL)
            hyper_df     <- coerce_with_dates(tryCatch(x@hyper_choice, error=function(e) NULL))
            if (!is.null(hyper_df) && !nrow(hyper_df)) hyper_df <- NULL
            feat_imp     <- x@feat_imp %||% NULL
            feat_imp     <- feat_imp %>% dplyr::filter(feat != "(Intercept)")
            
            # precompute test cumulative
            test_ts <- tryCatch(
              cum_metrics(oos_df, rebal_dates %||% unique(oos_df$dates),
                          huber_delta=huber_delta, tau=quantile_tau),
              error=function(e) NULL
            )
            
            menu <- c(
              "Chosen Evaluation Metric Over Time",
              "Test vs Validation Chosen Evaluation Metric Over Time",
              "All Evaluation Metrics Over Time",
              "Consolidated OOS Testing Metrics",
              "OOS Predictions, Errors and Targets",
              "Time-Series Feature Importance by Covariate",
              "Feature Importance Heatmap by Theme"
            )
            if (is.null(plot_id)){
              cat("\nChoose a plot:\n"); for (i in seq_along(menu)) cat(i,": ",menu[i],"\n",sep="")
              sel <- as.numeric(readline("Enter number: "))
              if (is.na(sel) || sel<1 || sel>length(menu)) stop("Invalid selection.")
              plot_name <- menu[sel]
            } else if (is.numeric(plot_id)){
              if (plot_id<1 || plot_id>length(menu)) stop("Invalid plot_id.")
              plot_name <- menu[plot_id]
            } else if (is.character(plot_id)){
              if (!plot_id %in% menu) stop("Invalid plot_id.")
              plot_name <- plot_id
            } else stop("plot_id must be number or string.")
            
            # 1) chosen metric over time
            if (plot_name == menu[1]){
              if (is.null(test_ts)) stop("Could not compute test metrics over time.")
              if (!chosen_metric %in% names(test_ts)) stop("Chosen metric not found.")
              df <- test_ts %>% dplyr::select(dates, value = !!rlang::sym(chosen_metric)) %>%
                dplyr::arrange(dates)
              p <- ggplot2::ggplot(df, ggplot2::aes(dates, value, group = 1)) +
                ggplot2::geom_line(linewidth = .6) +
                ggplot2::geom_point(size = 1.6) +
                ggplot2::scale_x_date(labels = scales::label_date("%Y")) +
                ggplot2::labs(title = paste("Test", chosen_metric, "over time"),
                              x = "Date", y = chosen_metric) + tm
              print(p); 
            }
            
            
            # 2) test vs validation chosen metric
            if (plot_name == menu[2]){
              if (is.null(test_ts)) stop("Could not compute test metrics over time.")
              if (is.null(val_plain)) stop("No validation metrics to compare.")
              if (!chosen_metric %in% names(test_ts) || !chosen_metric %in% names(val_plain))
                stop("Chosen metric missing.")
              test <- test_ts %>% dplyr::select(dates, value = !!rlang::sym(chosen_metric)) %>%
                dplyr::arrange(dates) %>% dplyr::mutate(type = "Test")
              val  <- val_plain %>% dplyr::select(dates, value = !!rlang::sym(chosen_metric)) %>%
                dplyr::filter(!is.na(value)) %>% dplyr::arrange(dates) %>%
                dplyr::mutate(type = "Validation")
              df <- dplyr::bind_rows(test, val)
              p <- ggplot2::ggplot(df, ggplot2::aes(dates, value, color = type, group = type)) +
                ggplot2::geom_line(linewidth = .6) +
                ggplot2::geom_point(size = 1.6) +
                ggplot2::scale_color_manual(values = c("Test" = "#0072B2", "Validation" = "#D55E00")) +
                ggplot2::labs(title = paste("Test vs Validation", chosen_metric),
                              x = "Date", y = chosen_metric, color = NULL) + tm
              print(p); 
            }
            
            
            # 3) all metrics over time (prompt subset)
            if (plot_name == menu[3]){
              if (is.null(test_ts)) stop("Could not compute test metrics over time.")
              all_cols <- c("rss","cp","rmse","mae","mphe","mpe","mape","hr","mb")
              keep <- intersect(all_cols, names(test_ts))
              if (is.null(metrics_to_plot)){
                ask <- readline(sprintf("Metrics to plot (comma sep) [default: %s]: ",
                                        paste(keep, collapse=", ")))
                sel <- unique(strsplit(gsub("\\s+","", if (nzchar(ask)) ask else paste(keep,collapse=",")), ",")[[1]])
                metrics_to_plot <- intersect(keep, sel); if (!length(sel)) stop("No valid metrics chosen.")
              }
              
              df <- test_ts %>% dplyr::select(dates, dplyr::all_of(metrics_to_plot)) %>% dplyr::arrange(dates) %>%
                tidyr::pivot_longer(cols = -dates, names_to = "metric", values_to = "value") %>%
                dplyr::filter(!is.na(value))
              p <- ggplot2::ggplot(df, ggplot2::aes(dates, value, color = metric, group = metric)) +
                ggplot2::geom_line(linewidth = .6) +
                ggplot2::geom_point(size = 1.1) +
                ggplot2::facet_wrap(~metric, scales = "free_y") +
                ggplot2::labs(title = "OOS Evaluation Metrics Over Time",
                              x = "Date", y = "Value", color = NULL) + tm
              print(p); 
            }
            
            
            # 4) consolidated
            if (plot_name == menu[4]){
              df <- cons_metrics
              if (is.null(df) || !all(c("metric","cons_oos") %in% names(df)))
                stop("x@eval_metrics must have 'metric' and 'cons_oos'.")
              p <- ggplot2::ggplot(df, ggplot2::aes(metric, cons_oos, fill=metric)) +
                ggplot2::geom_col(alpha=.95) + ggplot2::coord_flip() +
                ggplot2::scale_fill_viridis_d(option="C", end=.9) +
                ggplot2::labs(title="Consolidated OOS Testing Metrics",
                              x=NULL, y="Value", fill="Metric") + tm
              print(p); 
            }
            
            # 5) oos series
            if (plot_name == menu[5]){
              df <- x@oos_outputs
              if (is.null(df) || !all(c("month_id","pred","target","error") %in% names(df)))
                stop("oos_outputs must have: month_id, pred, target, error.")
              df <- df %>% dplyr::mutate(dates=as.Date(month_id)) %>%
                tidyr::pivot_longer(cols=c(pred,target,error), names_to="series",
                                    values_to="value")
              p <- ggplot2::ggplot(df, ggplot2::aes(dates, value, color=series)) +
                ggplot2::geom_line(linewidth=.6) +
                ggplot2::facet_wrap(~series, scales="free_y", ncol=1) +
                ggplot2::scale_color_viridis_d(option="C", end=.9) +
                ggplot2::labs(title="OOS Predictions, Target and Error",
                              x="Date", y=NULL, color=NULL) + tm
              print(p); 
            }
            
            # 6) TS feature importance (Intercept vs others)
            if (plot_name == menu[6]){
              fi <- feat_imp
              if (is.null(fi)) stop("x@feat_imp is NULL.")
              if (!all(c("dates","feat","norm_imp") %in% names(fi)))
                stop("feat_imp must have cols: dates, feat, norm_imp.")
              df <- fi %>%
                dplyr::mutate(dates = as.Date(dates),
                              group = dplyr::if_else(feat == "(Intercept)",
                                                     "(Intercept)", "Covariates"))
              p <- ggplot2::ggplot(df, ggplot2::aes(dates, norm_imp, color = feat)) +
                ggplot2::geom_line(linewidth = .5, alpha = .9) +
                ggplot2::facet_wrap(~group, scales = "free_y", ncol = 1) +
                ggplot2::scale_color_viridis_d(option = "C", end = .9, name = "Feature") +
                ggplot2::guides(color = ggplot2::guide_legend(ncol = 3,
                                                              byrow = TRUE,
                                                              override.aes = list(size = 1))) +
                ggplot2::labs(title = "Time-Series Feature Importance",
                              x = "Date", y = "Normalized importance") +
                tm +
                ggplot2::theme(legend.position = "bottom")
              print(p); 
            }
            
            # 7) theme heatmap (expects theme_map with feat/theme)
            if (plot_name == menu[7]){
              fi <- feat_imp
              if (is.null(fi)) stop("x@feat_imp is NULL.")
              fi2 <- add_theme(fi, theme_map)
              if (!all(c("dates","theme","norm_imp") %in% names(fi2)))
                stop("After merge, need cols: dates, theme, norm_imp.")
              df <- fi2 %>% dplyr::mutate(dates=as.Date(dates)) %>%
                dplyr::group_by(theme, dates) %>%
                dplyr::summarise(Calc_Stat=mean(norm_imp, na.rm=TRUE), .groups="drop")
              p <- ggplot2::ggplot(df, ggplot2::aes(x=dates, y=theme, fill=Calc_Stat)) +
                ggplot2::geom_tile(color="white") +
                viridis::scale_fill_viridis(option="C") +
                ggplot2::labs(title="Feature Importance Heatmap by Theme",
                              x="Date", y="Theme", fill="Mean\nnorm. imp.") + tm
              print(p); 
            }
            
            invisible(x)
          })



#' @export
setMethod("summary", "wf_backtest_results", function(object, summary_id = NULL) {
  
  ## --------- deps (light, only for xts/zoo + optional dplyr/tidyr) ----------
  need <- c("zoo")  # used when slots are xts
  miss <- need[!vapply(need, requireNamespace, logical(1), quietly = TRUE)]
  if (length(miss)) stop("Missing packages: ", paste(miss, collapse = ", "))
  
  has_dplyr <- requireNamespace("dplyr", quietly = TRUE)
  has_tidyr <- requireNamespace("tidyr", quietly = TRUE)
  
  ## -------------------------- helpers ---------------------------------------
  to_df <- function(x) {
    if (is.null(x)) return(NULL)
    if (inherits(x, "xts")) {
      df <- as.data.frame(x)
      df$Date <- tryCatch(as.Date(zoo::index(x)), error = function(e) NA)
      rownames(df) <- NULL
      return(df)
    }
    if (is.data.frame(x)) return(x)
    stop("Unsupported object type for conversion to data.frame.")
  }
  
  round_numeric_columns <- function(df, digits = 4) {
    if (!is.data.frame(df)) return(df)
    num <- vapply(df, is.numeric, logical(1))
    df[num] <- lapply(df[num], function(v) round(v, digits))
    df
  }
  
  md_table_print <- function(df, title = NULL, digits = 4, max_rows = Inf) {
    if (is.null(df) || !nrow(df)) { cat("No data to display.\n"); return(invisible()) }
    df <- round_numeric_columns(df, digits)
    df[] <- lapply(df, function(col) if (is.factor(col)) as.character(col) else col)
    
    # Truncate rows if requested
    if (is.finite(max_rows) && nrow(df) > max_rows) {
      df <- df[seq_len(max_rows), , drop = FALSE]
    }
    
    header <- names(df)
    cells  <- lapply(df, function(col) format(col, trim = TRUE, justify = "right"))
    widths <- pmax(nchar(header), 
                   vapply(cells, function(c) max(nchar(c), na.rm = TRUE),
                          integer(1)))
    pad <- function(x, w, left = FALSE) {
      if (left){
        format(x, width = w, justify = "left")
      }  else format(x, width = w, justify = "right")
    }
    
    is_num <- vapply(df, is.numeric, logical(1))
    head_fmt <- mapply(function(h, w, num) pad(h, w, left = !num),
                       header, widths, is_num, USE.NAMES = FALSE)
    
    row_fmt <- function(i) {
      mapply(function(v, w, num) pad(v[i], w, left = !num),
             cells, widths, is_num, USE.NAMES = FALSE)
    }
    
    if (!is.null(title)) cat("\n", title, "\n", sep = "")
    
    cat("| ", paste(head_fmt, collapse = " | "), " |\n", sep = "")
    cat("| ", paste(vapply(widths, function(w) paste(rep("-", w), collapse=""),
                           character(1)),
                    collapse = " | "), " |\n", sep = "")
    for (i in seq_len(nrow(df))) {
      cat("| ", paste(row_fmt(i), collapse = " | "), " |\n", sep = "")
    }
    invisible()
  }
  
  summarize_numeric <- function(df) {
    if (is.null(df) || !nrow(df)) return(df)
    num <- vapply(df, is.numeric, logical(1))
    if (!any(num)) stop("No numeric columns to summarize.")
    nums <- df[ , num, drop = FALSE]
    data.frame(
      Variable = names(nums),
      N        = vapply(nums, function(x) sum(!is.na(x)), integer(1)),
      Mean     = vapply(nums, function(x) mean(x, na.rm = TRUE), numeric(1)),
      Median   = vapply(nums, function(x) stats::median(x, na.rm = TRUE), numeric(1)),
      SD       = vapply(nums, function(x) stats::sd(x, na.rm = TRUE), numeric(1)),
      Min      = vapply(nums, function(x) min(x, na.rm = TRUE), numeric(1)),
      Max      = vapply(nums, function(x) max(x, na.rm = TRUE), numeric(1)),
      check.names = FALSE
    )
  }
  
  long_by_metric <- function(df, date_cols = c("Date", "dates")) {
    if (is.null(df) || !nrow(df)) return(NULL)
    
    # Case 1: already long-shaped like eval_metrics (metric + value)
    if (all(c("metric", "cons_oos") %in% names(df))) {
      return(
        data.frame(
          Metric = df$metric,
          value  = df$cons_oos,
          stringsAsFactors = FALSE,
          check.names = FALSE
        )
      )
    }
    
    # Case 2: wide-shaped, pivot only the true metric columns
    id_cols <- intersect(names(df), c("metric", "Metric", date_cols))
    value_cols <- setdiff(names(df), id_cols)
    
    if (length(value_cols) == 0L) return(NULL)
    
    if (requireNamespace("tidyr", quietly = TRUE)) {
      tidyr::pivot_longer(
        df,
        cols = value_cols,
        names_to = "Metric",
        values_to = "value"
      )
    } else {
      # base fallback
      out <- do.call(rbind, lapply(value_cols, function(m) {
        data.frame(Metric = m, value = df[[m]], stringsAsFactors = FALSE)
      }))
      out
    }
  }
  
  
  # choose fi covariate column flexibly
  choose_fi_cov_col <- function(df) {
    cand <- c("feat","tickers","feature","covariate")
    cand[match(TRUE, cand %in% names(df))]
  }
  
  ## ---------------------- available tables -----------------------------------
  available_tables <- c(
    "OOS_Outputs_Summary",
    "OOS_Testing_Eval_Metrics",
    "Validation_Eval_Metrics_at_HyperChoice",
    "Hyperparameters_Chosen_Over_Time",
    "Feature_Importance",
    "Final_Feature_Importance"
  )
  
  ## ---------------------- always show main info ------------------------------
  cat("Backtest ID: ", object@backtest_id, "\n", sep = "")
  # best-effort pulls from backtest_meta
  bm <- object@backtest_meta
  safe <- function(x) tryCatch(x, error = function(e) NULL)
  dates_testing  <- safe(bm$dates_testing_sample)
  rebals         <- safe(bm$rebalance_dates)
  chosen_metric  <- safe(object@eval_metric_val)
  if (!is.null(chosen_metric)) {
    # can be list; try a friendly display
    if (is.list(chosen_metric) && length(chosen_metric) == 1L) {
      cat("Chosen Evaluation Metric: ", as.character(chosen_metric[[1]]), "\n", 
          sep = "")
    } else if (is.list(chosen_metric) && !is.null(chosen_metric$name)) {
      cat("Chosen Evaluation Metric: ", as.character(chosen_metric$name), "\n",
          sep = "")
    }
  }
  if (!is.null(dates_testing)) {
    cat("Testing Sample Dates: ",
        paste(format(as.Date(dates_testing), "%d-%m-%Y"), collapse = ", "), "\n",
        sep = "")
  }
  if (!is.null(rebals)) {
    cat("Rebalancing Dates: ",
        paste(format(as.Date(rebals), "%d-%m-%Y"), collapse = ", "), "\n",
        sep = "")
  }
  
  ## ---------------------- selection -----------------------------------------
  if (is.null(summary_id)) {
    cat("\nPlease choose a table to display:\n")
    for (i in seq_along(available_tables)) cat(i, ": ", available_tables[i], "\n",
                                               sep = "")
    selection <- readline(prompt = "Enter the number of your choice: ")
    summary_id <- as.numeric(selection)
    if (is.na(summary_id) || summary_id < 1 || summary_id > length(available_tables)) {
      stop("Invalid selection.")
    }
  }
  
  if (is.numeric(summary_id)) {
    if (summary_id >= 1 && summary_id <= length(available_tables)) {
      table_name <- available_tables[summary_id]
    } else stop("Invalid table number.")
  } else if (is.character(summary_id)) {
    if (summary_id %in% available_tables) {
      table_name <- summary_id
    } else stop("Invalid 'summary_id'. Options: ", paste(available_tables,
                                                         collapse = ", "))
  } else stop("'summary_id' must be string or number.")
  
  ## ---------------------- table builders + print -----------------------------
  
  if (table_name == "OOS_Outputs_Summary") {
    df <- to_df(object@oos_outputs)
    if (is.null(df) || !nrow(df)) {
      cat("oos_outputs is not specified or empty.\n")
    } else {
      md_table_print(summarize_numeric(df), title = "OOS Outputs - Summary")
      # If common columns exist, provide a tiny add-on correlation (optional)
      common <- intersect(names(df), c("pred","target","error"))
      if (all(c("pred","target") %in% common)) {
        cor_pt <- suppressWarnings(stats::cor(df$pred, df$target, 
                                              use = "complete.obs"))
        cat("\nPred–Target correlation: ", sprintf("%.4f", cor_pt), "\n",
            sep = "")
      }
    }
    
  } else if (table_name == "OOS_Testing_Eval_Metrics") {
    df <- to_df(object@eval_metrics)
    if (is.null(df) || !nrow(df)) {
      cat("eval_metrics not specified or empty.\n")
    } else {
      long <- long_by_metric(df, date_cols = c("Date","dates"))
      if (is.null(long) || !nrow(long)) {
        cat("No metrics found to summarize in eval_metrics.\n")
      } else {
        if (has_dplyr) {
          out <- dplyr::group_by(long, .data$Metric) %>%
            dplyr::summarise(
              Mean   = mean(.data$value, na.rm = TRUE),
              Median = stats::median(.data$value, na.rm = TRUE),
              SD     = stats::sd(.data$value, na.rm = TRUE),
              Min    = min(.data$value, na.rm = TRUE),
              Max    = max(.data$value, na.rm = TRUE),
              .groups = "drop"
            )
        } else {
          # base fallback
          f <- function(v) c(Mean=mean(v,na.rm=TRUE), 
                             Median=stats::median(v,na.rm=TRUE),
                             SD=stats::sd(v,na.rm=TRUE),
                             Min=min(v,na.rm=TRUE), 
                             Max=max(v,na.rm=TRUE))
          
          outm <- do.call(rbind, lapply(split(long$value, long$Metric), f))
          out <- data.frame(Metric = rownames(outm), outm, row.names = NULL, 
                            check.names = FALSE)
        }
        md_table_print(out, title = "OOS Testing Evaluation Metrics — Summary")
      }
    }
    
  } else if (table_name == "Validation_Eval_Metrics_at_HyperChoice") {
    df <- to_df(object@val_eval_metrics_hyper_choice)
    if (is.null(df) || !nrow(df)) {
      cat("val_eval_metrics_hyper_choice not specified or empty.\n")
    } else {
      long <- long_by_metric(df, date_cols = c("Date","dates"))
      if (is.null(long) || !nrow(long)) {
        cat("No metrics found to summarize in val_eval_metrics_hyper_choice.\n")
      } else {
        if (has_dplyr) {
          out <- dplyr::group_by(long, .data$Metric) %>%
            dplyr::summarise(
              Mean   = mean(.data$value, na.rm = TRUE),
              Median = stats::median(.data$value, na.rm = TRUE),
              SD     = stats::sd(.data$value, na.rm = TRUE),
              Min    = min(.data$value, na.rm = TRUE),
              Max    = max(.data$value, na.rm = TRUE),
              .groups = "drop"
            )
        } else {
          f <- function(v) c(Mean=mean(v,na.rm=TRUE), 
                             Median=stats::median(v,na.rm=TRUE),
                             SD=stats::sd(v,na.rm=TRUE),
                             Min=min(v,na.rm=TRUE), 
                             Max=max(v,na.rm=TRUE))
          outm <- do.call(rbind, lapply(split(long$value, long$Metric), f))
          out <- data.frame(Metric = rownames(outm), outm, row.names = NULL, 
                            check.names = FALSE)
        }
        md_table_print(out, title = "Validation Metrics at Hyper-Choice - Summary")
      }
    }
    
  } else if (table_name == "Hyperparameters_Chosen_Over_Time") {
    df <- to_df(object@hyper_choice)
    if (is.null(df) || !nrow(df)) {
      cat("hyper_choice not specified or empty.\n")
    } else {
      # summary per hyperparameter column (exclude Date)
      cols <- setdiff(names(df), c("Date","dates"))
      if (!length(cols)) { 
        cat("No hyperparameter columns in hyper_choice.\n") 
      } else {
        summarize_col <- function(v) {
          tab <- sort(base::table(v), decreasing = TRUE)
          most_common <- if (length(tab)) names(tab)[1] else NA_character_
          base::data.frame(
            N_Obs         = sum(!is.na(v)),
            Unique_Values = length(unique(v)),
            Most_Common   = most_common,
            Last_Value    = v[tail(which(!is.na(v)), 1)],
            stringsAsFactors = FALSE, check.names = FALSE
          )
        }
        
        out_list <- lapply(cols, function(cn) {
          res <- summarize_col(df[[cn]])
          if (!base::is.data.frame(res)) res <- base::as.data.frame(res, stringsAsFactors = FALSE)
          res$Hyperparameter <- cn
          res
        })
        
        out <- base::do.call(base::rbind, out_list)
        
        # Reorder only if the column exists
        if ("Hyperparameter" %in% names(out)) {
          out <- out[, c("Hyperparameter", setdiff(names(out), "Hyperparameter")), drop = FALSE]
        }
        
        md_table_print(out, title = "Hyperparameters Chosen Over Time - Summary")
      }
    }
    
  }  else if (table_name == "Feature_Importance") {
    fi <- to_df(object@feat_imp)
    if (is.null(fi) || !nrow(fi)) {
      cat("feat_imp not specified or empty.\n")
    } else {
      cov_col <- choose_fi_cov_col(fi)
      if (is.na(cov_col)){
        stop("Feature-importance table must contain one of: feat,
             feature, covariate.")
      } 
      if (!"norm_imp" %in% names(fi)){
        stop("'norm_imp' column not found in feat_imp.")
      } 
      if (has_dplyr) {
        out <- fi %>%
          dplyr::group_by(.data[[cov_col]]) %>%
          dplyr::summarise(
            Mean_Feature_Imp   = mean(.data$norm_imp, na.rm = TRUE),
            Median_Feature_Imp = stats::median(.data$norm_imp, na.rm = TRUE),
            Q25 = stats::quantile(.data$norm_imp, 0.25, na.rm = TRUE),
            Q75 = stats::quantile(.data$norm_imp, 0.75, na.rm = TRUE),
            Max = max(.data$norm_imp, na.rm = TRUE),
            Min = min(.data$norm_imp, na.rm = TRUE),
            .groups = "drop"
          )
        names(out)[1] <- "Covariate"
      } else {
        split_by <- split(fi$norm_imp, fi[[cov_col]])
        f <- function(v) c(Mean_Feature_Imp=mean(v,na.rm=TRUE),
                           Median_Feature_Imp=stats::median(v,na.rm=TRUE),
                           Q25=stats::quantile(v,0.25,na.rm=TRUE),
                           Q75=stats::quantile(v,0.75,na.rm=TRUE),
                           Max=max(v,na.rm=TRUE), Min=min(v,na.rm=TRUE))
        outm <- do.call(rbind, lapply(split_by, f))
        out <- data.frame(Covariate = rownames(outm), outm, row.names = NULL,
                          check.names = FALSE)
      }
      md_table_print(out, title = "Feature Importance — Summary")
    }
    
  } else if (table_name == "Final_Feature_Importance") {
    fi <- to_df(object@final_feat_imp)
    if (is.null(fi) || !nrow(fi)) {
      cat("final_feat_imp not specified or empty.\n")
    } else {
      cov_col <- choose_fi_cov_col(fi)
      if (is.na(cov_col)){
        stop("Final feature-importance must contain one of: feat, feature, covariate.")
      } 
      if (!"norm_imp" %in% names(fi)) stop("'norm_imp' column not found in final_feat_imp.")
      out <- fi[, c(cov_col, "norm_imp"), drop = FALSE]
      names(out) <- c("Covariate", "Final_Feature_Imp")
      md_table_print(out[order(-out$Final_Feature_Imp), ],
                     title = "Final Feature Importance — Last Model")
    }
  }
  
  invisible(object)
})
