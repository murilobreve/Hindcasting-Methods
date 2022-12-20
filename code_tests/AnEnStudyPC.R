###############################################################################
# Murilo Montanini Breve
# email: murilobreve@alunos.utfpr.edu.br
# Last modification: 11/11/2022
###############################################################################

#############################################################
## *What this main function does*
## Load the data contained inside /data/predictor or /data/predicted
## Organize these data and transform into a sequence of vectors (2k + 1) into a matrix
## Predict with different functions (AnEn or ClustAnEn)
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
# Run the function AnEn_methods.R
##-----------------------------------------------------------

AnEnMethods <-
    function(params_list,
             method = "monache",
             Pca = TRUE,
             predictors = 2,
             variables = c("WSPD", "GST"),
             avail_threshold = 0.85) {
        start_time <- Sys.time()
        
        # Load data -----------------------------------
        loaded_stations <- LoadData(params_list, variables)
        pred <- loaded_stations$pred
        target <- loaded_stations$target
        
        
        # Organize data -----------------------------------
        data_pca <-
            OrganizeIntoDataFrame(params_list, loaded_stations, avail_threshold)
        
        #loading_end_time <- Sys.time()
        #message("[INFO] Load time consumed: ",
        #        round(loading_end_time - start_time, 3),
        #        " secs")
        
        result_measures <- list()
        for (k in seq_along(data_pca[1,])) {
            variable_predicted <- colnames(data_pca)[k]
            
            # Setting the right period for prediction/training
            time_0 <- params_list$prediction_t0
            time_series <- pred[, 1]$stamp
            
            training_time <- time_series < time_0
            prediction_time <- !training_time
            
            # Bind the predictors and the predicted to train the model
            
            pred_all <- data_pca[,-k]
            target_all <- data_pca[, k]
            target_training <-  target_all[training_time]
            target_prediction <- target_all[prediction_time]
            
            
            # PCA analysis -----------------------------------
            if (dim_reduction == "PCA")
                PCs_and_pca <- DoPCA(pred_all)
            if (dim_reduction ==" PLS")
                PCs_and_pca <- DoPLS(pred_all, target_training)
            
            #pca_end_time <- Sys.time()
            #message("[INFO] PCA time consumed: ",
            #        round(pca_end_time - loading_end_time , 3),
            #        " secs")
            
            PCs_and_pca[[2]]
            
            # Organizing in vectors  -----------------------------------
            Y_result <-
                OrganizeSequenceVectors(PCs_and_pca, loaded_stations)
            
            Y <- Y_result$Y.analogues
            Y_pred <- Y_result$Y.pred
            Ynan <- Y_result$Ynan
            
            # Searching for analogues to every value in the prediction period  -----------------------------------
            monache_start_time <- Sys.time()
            
            if (method == "ClustAnEn") {
                nb.iter <- params_list$nb_iteracoes
                nb.centers <- params_list$nb_centros
                nb.series <- PCs_and_pca[[2]]
                
                Ynan = rowSums(is.na(Y)) == 0 # sum the row values and if the row have 0 Na values then set TRUE for the field if not set FALSE
                Y_purged <- Y[Ynan, ]# remove all the rows with FALSE
                analogs.final = target_training[c(Ynan, rep(FALSE, length(target_training) - length(Ynan)))]
                
                
                clusteredAn = kmeans(Y_purged, nb.centers, iter.max =
                                         nb.iter)
                cluster <-
                    makeCluster(ncores, type = "FORK")
                
                prevData <-
                    parSapply(cluster, 1:length(Y_pred[, 1]), ClustAnEn)
                
                stopCluster(cluster)
                
                E <- target_prediction - prevData[1, ]
                BIAS <-
                    ((1 / length(E)) * sum(E, na.rm = TRUE))
                RMSE <- sqrt(mean((E) ^ 2,  na.rm = TRUE))
                SDE <- sqrt((RMSE ** 2) - (BIAS ** 2))
                porcentage_na <-
                    round((sum(!complete.cases(
                        as.vector(prevData[1, ])
                    )) / sum(complete.cases(
                        as.vector(prevData[1, ])
                    ))) * 100, 3)
                
                result_measures[[k]] <-
                    c(BIAS, RMSE, SDE, porcentage_na, variable_predicted)
                names(result_measures[[k]]) <-
                    c("BIAS", "RMSE", "SDE", "NA%", "Name")
                print(result_measures)
            }
            
            if (method == "AnEn") {
                cluster <-
                    makeCluster(ncores, type = "FORK")
                
                prevData <-
                    parSapply(cluster, 1:length(Y_pred[, 1]), MonachePCA)
                
                stopCluster(cluster)
            }
            
            #monache_end_time <- Sys.time()
            
        }
        message(
            "[INFO] Prediction time consumed: ",
            round(monache_end_time - monache_start_time  , 3),
            " secs"
        )
        result1 <- t(do.call(cbind, result_measures))
        result1_
        result1[, 2]
        
        round_df <- function(x, digits) {
            # round all numeric variables
            # x: data frame
            # digits: number of digits to round
            numeric_columns <- sapply(result1, mode) == 'numeric'
            result1[, 1:3] <-  as.numeric(result1[, 1:3])
            result[, 1:3] <-  round(as.numeric(result[, 1:3]), 3)
            x
        }
        
        
        result_m <- list()
        for (j in seq_along(variables)) {
            result_m[[j]] <- result1[grep(variables[j], result1[, 5]), ]
        }
        
        names(result_m) <- variables
        
        
        target <- loaded_stations$target
        realData <- target[[variables]][!training_time]
        
        # Predicted values comparison to the real values -----------------------------------
        errors_result <-
            checkResults(prevData, target, forecasting_t0)
        
        message(errors_result)
    }
