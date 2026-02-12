fit_once <- function(seed = NULL) {
  if (!is.null(seed)) {
    set_all_seeds(seed)
  }
  browser()
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