###############################################################################
# Murilo Montanini Breve
# email: murilobreve@alunos.utfpr.edu.br
# Last modification: 17/11/2022
###############################################################################

#############################################################
## *What this main function does*
## Load the data contained inside /data/predictor or /data/predicted
## It trains a model for prediction (PLSR or PCR)
## Predict the data from selected predicted station (inside the folder /data/predicted)
##-----------------------------------------------------------
## *Details of the code*
## Functions = UpperCamelCase: e.g. SignatureMethod
## Parameters =  underscore_separated: e.g. numeric_version
## Inside variables = period.separated: e.g. plot.new
#############################################################

#%%Running%%
##-----------------------------------------------------------
# Install the libraries (inside /R/settings/)
# Verify the desired parameters (inside /R/settings/)
# Run the function Regressions.R
##-----------------------------------------------------------

Regressions <- function(params_list,
                        method = "PLSR",
                        variables,
                        variables_predict,
                        avail_threshold = 0.85) {
    message("[INFO] Running")
    message(variables,
            " Using to produce the model, and ",
            variables_predict,
            "to predict")
    start.time <- Sys.time()
    
    time_0 <- params_list$prediction_t0
    
    variables_all <- c("WSPD", "GST", "ATMP", "PRES")
    
    r_p_v_p <- list()
    for (f in seq_along(variables_all)) {
        variables <- variables_all[f]
        # Load data -----------------------------------
        loaded_stations <- LoadData(params_list, variables)
        
        # Assigning data ------------------------------
        pred <- loaded_stations$pred
        time_series <- pred[, 1]$stamp
        predictor_name <- loaded_stations$predictor_name
        target <- loaded_stations$target
        predicted_name <- loaded_stations$predicted_name
        
        # Organizing data ------------------------------
        data <-
            OrganizeIntoDataFrame(params_list, loaded_stations, avail_threshold)
        result_per_variables <- list()
        variables_station <- list()
        measures_v <- list()
        for (k in seq_along(data[1, ])) {
            variable_predicted <- colnames(data)[k]
            
            # Bind the predictors and the predicted to train the model
            data_pred_train <- cbind(data[, -k], data[, k])
            colnames(data_pred_train)[ncol(data_pred_train)] <- "target"
            
            # Setting the right period for prediction/training
            training_time <- time_series < time_0
            prediction_time <- !training_time
            data_period_training <- data_pred_train[training_time, ]
            
            data_period_prediction <-
                data_pred_train[prediction_time,-ncol(data_pred_train)]
            n_col <- ncol(data_period_prediction)
            
            observ_data_prediction <-
                data_pred_train[prediction_time, ncol(data_pred_train)]
            
            model.start.time <- Sys.time()
            
            # Apply the methods
            if (method == "PLSR") {
                model <-
                    pls::plsr(
                        target ~ .,
                        data = as.data.frame(data_period_training),
                        validation = "CV",
                        ncomp = n_col,
                        center = TRUE,
                        scale = TRUE
                    )
            }
            if (method == "PCR") {
                model <-
                    pls::pcr(
                        target ~ .,
                        data = as.data.frame(data_period_training),
                        validation = "CV",
                        ncomp = n_col,
                        center = TRUE,
                        scale = TRUE
                    )
            }
            
            # Predict the data ------------------------------
            prevAle <- predict(model, data_period_prediction, ncomp = n_col)
            
            pred.end.time <- Sys.time()
            
            # Measures of prediction ------------------------------
            
            
            E <- observ_data_prediction - prevAle
            BIAS <-
                ((1 / length(E)) * sum(E, na.rm = TRUE))
            RMSE <- sqrt(mean((E) ^ 2,  na.rm = TRUE))
            SDE <- sqrt((RMSE ** 2) - (BIAS ** 2))
            porcentage_na <-
                round((sum(
                    !complete.cases(as.vector(prevAle))
                ) / sum(complete.cases(
                    as.vector(prevAle)
                ))) * 100, 3)
            
            measures_v[[k]] <-
                c(BIAS, RMSE, SDE, porcentage_na, variable_predicted)
            names(measures_v[[k]]) <-
                c("BIAS", "RMSE", "SDE", "NA%", "Name")
            
            end.time <- Sys.time()
            
            # Output messages ------------------------------
            
            print(measures_v)
            
            
            result <- t(do.call(cbind, measures_v))
            r_p_v_p[[f]] <- result
            
        }
    }
    
}
