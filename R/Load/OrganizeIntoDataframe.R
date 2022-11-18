##############################
# Organize the data into a dataframe
# The loading is in parallel
# The output is a list with the predictors and the predicted station
##############################

OrganizeIntoDataFrame <- function(params_list, loaded_stations, avail_threshold) {

    pred <- loaded_stations$pred
    predictor_name <- loaded_stations$predictor_name
    target <- loaded_stations$target
    predicted_name <- loaded_stations$predicted_name

    names.s <- NULL
    data <- list()
    for (f in seq_along(predictor_name)) {
        data[[f]] <- do.call(cbind, pred[, f][variables])
        names.s <- c(names.s, paste0(predictor_name[f], variables))
    }
    data.pca <-
        do.call(cbind, data)
    colnames(data.pca) <- names.s
    av <-
        colMeans(is.na(data.pca)) - 1
    data.pca <-
        data.pca[, av < -avail_threshold]

    return(data.pca)
}


