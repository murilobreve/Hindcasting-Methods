##############################
# Load the data of both predictors and predicted stations.
# The loading is in parallel
# The output is a list with the predictors and the predicted station
##############################

LoadData <-   function(params_list,
                       variables) {

    # Assigning parameters -----------------------------------
    ncores <- params_list$ncores
    path_predictor <- params_list$path_predictor
    path_predicted <- params_list$path_predicted

    # Read files names from the predictors -----------------------------------
    predictor.files.number <- list()
    predictor.files <- list.files(path = path_predictor)
    predictor.name <- unique(substr(predictor.files, 1 , 5))
    for (i in seq_along(predictor.name)) {
        predictor.files.number[[i]] <- grep(predictor.name[i], predictor.files)
    }

    # Loading of predictors files -----------------------------------
    setwd(path_predictor)
    cluster <-
        makeCluster(ncores, type = "FORK")
    if (length(grep(".txt", predictor_files)) > 0)  {
        pred. <-
            parSapply(cluster, seq_along(predictor.files.number), LoadStationCsv, station_files = predictor.files.number, files = predictor.files, params_list = params_list)
    } else{
        pred. <-
            parSapply(cluster, seq_along(predictor.files), LoadStation)
    }
    stopCluster(cluster)

    # Reading file names of the predicted station -----------------------------------
    target.files <- list.files(path = path_predicted)
    target.files.number <- list()
    target.name <- unique(substr(target.files, 1 , 5))
    for (i in seq_along(target.name)) {
        target.files.number[[i]] <- grep(target.name[i], target.files)
    }

    # Loading of the predicted file -----------------------------------
    setwd(path_predicted)
    cluster <-
        makeCluster(ncores, type = "FORK")
    if (length(grep(".txt", target_files)) > 0) {
        target. <-
            parSapply(cluster, seq_along(target.files.number), LoadStationCsv, station_files = target.files.number, files = target.files, params_list = params_list)
        target. <- target.[, 1]
    } else{
        target. <- LoadStation(1)
    }
    stopCluster(cluster)

    loaded.stations <- list(pred., predictor.name, target., target.name)
    names(loaded.stations) <- c("pred", "predictor_name", "target", "predicted_name")

    return(loaded.stations)
}


