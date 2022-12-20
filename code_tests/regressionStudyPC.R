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
    
    variables_station <- list()
    measures <- list()
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
        
        for(k in seq_along(data[1, ])){
        
        # Apply the methods
        if (method == "PLSR") {
            model <-
                pls::plsr(
                    target ~ .,
                    data = as.data.frame(data_period_training),
                    validation = NULL,
                    ncomp = k,
                    center = TRUE,
                    scale = TRUE
                )
        }
        if (method == "PCR") {
            model <-
                pls::pcr(
                    target ~ .,
                    data = as.data.frame(data_period_training),
                    validation = NULL,
                    ncomp = k,
                    center = TRUE,
                    scale = TRUE
                )
        }
        
        # Predict the data ------------------------------
        prevAle <- predict(model, data_period_prediction, ncomp = k)
        
        pred.end.time <- Sys.time()
        
        # Measures of prediction ------------------------------
        E <- observ_data_prediction - prevAle
        BIAS <-
            ((1 / length(E)) * sum(E, na.rm = TRUE))
        RMSE <- sqrt(mean((E) ^ 2,  na.rm = TRUE))
        SDE <- sqrt((RMSE ** 2) - (BIAS ** 2))
        porcentage_na <-
            round((sum(!complete.cases(
                as.vector(prevAle)
            )) / sum(complete.cases(
                as.vector(prevAle)
            ))) * 100, 3)
        
        measures[[k]] <-
            c(BIAS, RMSE, SDE, porcentage_na, variable_predicted )
        names(measures[[k]]) <- c("BIAS", "RMSE", "SDE", "NA%", "Name")
        
        end.time <- Sys.time()
        
        # Output messages ----------------------------------------
        print(measures)
    }
    
    result <- t(do.call(cbind, measures))
    result_
    result[, 2]
    
    result_variable <- list()
    for (j in seq_along(variables)) {
        result_variable[[j]] <- result[grep(variables[j], result[, 5]), ]
        
    }
    
    names(result_variable) <- variables
    
    pcr_result <- result_variable
    plsr_result <- result_variable
    
    wspd <- as.data.frame(cbind(pcr_result$WSPD[,c("BIAS", "RMSE", "SDE")], plsr_result$WSPD[,c("BIAS", "RMSE", "SDE")], seq(length(plsr_result$WSPD[,1]))))
    colnames(wspd)[7] <- "Components"
    wspd <- apply(sapply(wspd, as.character),2,as.numeric)
    
    write.table( wspd, "wspd_regression.csv", sep = ",", row.names = FALSE )
    
    gst <- as.data.frame(cbind(pcr_result$GST[,c("BIAS", "RMSE", "SDE")], plsr_result$GST[,c("BIAS", "RMSE", "SDE")], seq(length(plsr_result$GST[,1]))))
    colnames(gst)[7] <- "Components"
    
    gst <- apply(sapply(gst, as.character),2,as.numeric)
    
    write.table( gst, "gst_regression.csv", sep = ",", row.names = FALSE )
    
    atmp <- as.data.frame(cbind(pcr_result$ATMP[,c("BIAS", "RMSE", "SDE")], plsr_result$ATMP[,c("BIAS", "RMSE", "SDE")], seq(length(plsr_result$ATMP[,1]))))
    colnames(atmp)[7] <- "Components"
    
    atmp <- apply(sapply(atmp, as.character),2,as.numeric)
    
    write.table( atmp, "atmp_regression.csv", sep = ",", row.names = FALSE )
    
    pres <- as.data.frame(cbind(pcr_result$PRES[,c("BIAS", "RMSE", "SDE")], plsr_result$PRES[,c("BIAS", "RMSE", "SDE")], seq(length(plsr_result$PRES[,1]))))
    colnames(pres)[7] <- "Components"
    
    pres <- apply(sapply(pres, as.character),2,as.numeric)
    
    write.table( pres, "pres_regression.csv", sep = ",", row.names = FALSE )
    
    
    
  
    target$stamp[prediction_time]
    
    if (plot) {
        
    }
    
}

norfolk_result_all <-
    cbind(do.call(rbind, result_variable), do.call(rbind, result_m))

norfolk_result_all[, c(1, 2, 3, 6, 7, 8)] <-
    round(as.numeric(norfolk_result_all[, c(1, 2, 3, 6, 7, 8)]), 3)
