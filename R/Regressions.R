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
                        variables_predict) {
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
    data <- OrganizeIntoDataFrame(params_list, loaded_stations)

    load.time <- Sys.time()
    message(
        "[INFO] Time consumed only by loading the stations: ",
        round(load.time - start.time, 3),
        " secs"
    )

    # Bind the predictors to the predicted to train the model
    data_pred_train <- cbind(data, target[[variables_predict]])
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

    model.end.time <- Sys.time()
    message(
        "[INFO] Time consumed only by the model: ",
        round(model.end.time - model.start.time, 3),
        " secs"
    )

    pred.start.time <- Sys.time()

    # Predict the data ------------------------------
    prevAle <- predict(model, data_period_prediction, ncomp = n_col)

    pred.end.time <- Sys.time()
    message(
        "[INFO] Time consumed by the prediction: ",
        round(pred.end.time - pred.start.time, 3),
        " secs"
    )

    # Measures of prediction ------------------------------
    RMSE <-
        sqrt(mean((observ_data_prediction - prevAle) ^ 2, na.rm = TRUE))

    porcentage_na <-
        round((sum(!complete.cases(
            as.vector(prevAle)
        )) / sum(complete.cases(
            as.vector(prevAle)
        ))) * 100, 3)

    end.time <- Sys.time()

    # Output messages ------------------------------
    print(summary(model))
    message("[INFO] All time consumed: ",
            round(end.time - start.time, 3),
            " secs")
    message("[RESULT] NAs quantity: ", sum(!complete.cases(as.vector(prevAle))))
    message("[RESULT] NAs porcentage: ", porcentage_na , "%")
    message("[RESULT] RMSE: ", round(RMSE, 3))
}
