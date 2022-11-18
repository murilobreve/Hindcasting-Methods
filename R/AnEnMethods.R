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

        # Organize data -----------------------------------
        data_pca <- OrganizeIntoDataFrame(params_list, loaded_stations, avail_threshold)

        loading_end_time <- Sys.time()
        message("[INFO] Load time consumed: ",
                round(loading_end_time - start_time, 3),
                " secs")

        # PCA analysis -----------------------------------
        PCs_and_pca <- DoPCA(data_pca)

        pca_end_time <- Sys.time()
        message("[INFO] PCA time consumed: ",
                round(pca_end_time - loading_end_time , 3),
                " secs")

        # Organizing in vectors  -----------------------------------
        Y_result <-
            OrganizeSequenceVectors(PCs_and_pca, target, loaded_stations)

        Y <- Y_result$Y.analogues
        Y_pred <- Y_result$Y.pred
        Ynan <- Y_result$Ynan

        # Searching for analogues to every value in the prediction period  -----------------------------------
        monache_start_time <- Sys.time()
        cluster <-
            makeCluster(ncores, type = "FORK")

        prevData <-
            parSapply(cluster, 1:length(Y_pred[, 1]), MonachePCA)

        stopCluster(cluster)

        monache_end_time <- Sys.time()

        message("[INFO] Prediction time consumed: ",
                round(monache_end_time - monache_start_time  , 3),
                " secs")

        target <- loaded_stations$target
        realData <- target[[variables]][!training_time]

        # Predicted values comparison to the real values -----------------------------------
        errors_result <- checkResults(prevData, target, forecasting_t0)

        message(errors_result)
    }
