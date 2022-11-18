##############################
# 0 - Change directories
##############################

message("[INFO] Loading inteporlate function")
setwd("~/")
setwd("AnEnMDataR/R/Load")
source("InterpolateData.R")
source("LoadStation.R", local = TRUE)
source("LoadStationCsv.R", local = TRUE)

##############################
# Load the data of both predictors and predicted stations.
# The loading is in parallel
# The output is a list with the predictors and the predicted station
##############################

LoadData <-   function(params_list,
                      variables) {

  # passing the parameters
  ncores <- params_list[[13]]
  path_predictor <- params_list[[14]]
  path_predicted <- params_list[[15]]

  # Predictors Loading
  predictor_files_number <- list()
  predictor_files <- list.files(path = path_predictor)
  predictor_name <- unique(substr(files, 1 , 5))
  for (i in seq_along(predictor_name)) {
    predictor_files_number[[i]] <- grep(predictor_name[i], predictor_files)
  }


  # Parallelization of predictors loading
  setwd(path_predictor)
  cluster <-
    makeCluster(ncores, type = "FORK")
  if (length(grep(".txt", files)) > 0)  {
    pred <-
      parSapply(cluster, seq_along(predictor_files_number), LoadStationCsv, station_files = predictor_files_number, files = predictor_files, params_list = params_list)
  } else{
    pred <-
      parSapply(cluster, seq_along(predictor_files), LoadStationCdf)
  }
  stopCluster(cluster)


  # Predicted Loading
  target_files <- list.files(path = path_predicted)
  target_files_number <- list()
  target_name <- unique(substr(target_files, 1 , 5))
  for (i in seq_along(predicted_name)) {
    target_files_number[[i]] <- grep(predicted_name[i], target_files)
  }

  # Parallelization of predicted loading
  setwd(path_predicted)
  cluster <-
    makeCluster(ncores, type = "FORK")
  if (length(grep(".txt", files)) > 0) {
    target <-
      parSapply(cluster, seq_along(target_files_number), LoadStationCsv, station_files = target_files_number, files = target_files, params_list = params_list)
    target <- target[, 1]
  } else{
    target <- LoadStation(1)
  }
  stopCluster(cluster)

  # Result: return the predicted and the predictors and their names
  loaded_stations <- list(pred, predictor_name, target, predicted_name)

  return(loaded_stations)
}


  names_s <- NULL
  data <- list()
  for (f in seq_along(station_name)) {
    data[[f]] <- do.call(cbind, pred[, f][variables])
    names_s <- c(names_s, paste0(station_name[f], variables))
  }

  data.pca <- do.call(cbind, data) ## bind all data from predictors
  colnames(data.pca) <- names_s

  av <-
    colMeans(is.na(data.pca)) - 1 ## count the amout of NAs of the predictors
  data.pca <-
    data.pca[, av < -0.85] ## select which stations with more than 85% of data



  #bind predictors data with the data of predicted station and also the time_series before omiting NAs
  data_pred_target <-
    cbind(data.pca, target[[variable.predict]], pred[, 1]$stamp)

  d <-
    as.data.frame(na.omit(data_pred_target))

  time_series <- d[, ncol(d)] #store the new time_series
  d <- d[,-ncol(d)] # take off the new time_series

  colnames(d) <-
    c(names_s[av < -0.85], "target") # name the predicter station with "target"

  cor_study <- cor(d)
  cor_study <-
    cor_study[1:(ncol(cor_study) - 1), ncol(cor_study)]



}
