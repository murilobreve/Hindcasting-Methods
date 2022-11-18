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
  # 1 - Load the predictors files
  ##############################

  setwd("~/")
  setwd("AnEnMDataR/load/predictor")

  path <- getwd()

  pred <- list()
  station_files_txt <- list()
  station_files_ncdf4 <- list()

  files <- list.files(path = path)
  txt_files <- grep(".txt", files)
  ncdf4_files <- grep(".nc", files)

  station_name_txt <- unique(substr(files[txt_files], 1 , 5))
  station_name_ncdf4 <- unique(substr(files[ncdf4_files], 1 , 5))

  for (i in seq_along(station_name_txt)) {
    station_files_txt[[i]] <- grep(station_name_txt[i], files)
  }

  for (i in seq_along(station_name_ncdf4)) {
    station_files_ncdf4[[i]] <- grep(station_name_ncdf4[i], files)
  }

  cluster <-
    makeCluster(ncores - 1, type = "FORK")

  if (length(station_files_txt) > 0)
    pred <-
    parSapply(cluster, seq_along(station_files_txt), LoadStationCsv)

  if (length(station_files_ncdf4) > 0)
    pred <-
    parSapply(cluster, seq_along(station_files_ncdf4), LoadStation)


  stopCluster(cluster)


  ##############################
  # 1 - Load the predicted file
  ##############################

  setwd("~/")
  setwd("AnEnMDataR/load/predicted")
  path <- getwd()

  files <- list.files(path = path)
  station_files <- list()


  predicted_name <- unique(substr(files, 1 , 5))

  for (i in seq_along(predicted_name)) {
    station_files[[i]] <- grep(predicted_name[i], files)
  }

  cluster <-
    makeCluster(ncores - 1, type = "FORK")

  if (length( grep(".txt", files) == 0)) {
    target <-
      parSapply(cluster, seq_along(station_files), LoadStationCsv)
    target <- target[,1]
  } else{
    target <- LoadStation(1)
  }

  data <- list()
  for (f in seq_along(station_name_ncdf4)) {
    data[[f]] <- do.call(cbind, pred[, f][variables])
  }

  data.pca <- do.call(cbind, data)
  av <- colMeans(is.na(data.pca)) - 1
  data.pca <- data.pca[, av < -0.85]
  cor_study <- cor(na.omit(cbind(data.pca, target[[variables]])))
  cor_study <- cor_study[1:(ncol(cor_study) - 1), ncol(cor_study)]

  ###############################
  # 2 - PCA analysis
  ################################

  setwd("~/")
  setwd("AnEnMDataR/R/Load/PCA")
  if (type.pca == "weights")
    source("pca_weighted.R", local = TRUE)
  if (type.pca == "classical")
    source("pca.R", local = TRUE)


  data.pca.1_3 <-
    matrix(data = NA,
           nrow = length(data.pca[, 1]),
           ncol = 3)

  v.1 <- "PC1"
  v.2 <- "PC2"
  v.3 <- "PC3"

  pred1 <- list()

  data.pca.1_3[complete.cases(data.pca), 1] <- PC1
  pred1[[v.1]] <- data.pca.1_3[, 1]
  pred1$n0 <- pred[,1]$n0
  pred1$N <- pred[,1]$N
  pred1$t0 <- pred[,1]$t0
  pred1$stamp <- pred[,1]$stamp

  if (nb.pca >= 2) {
    pc2 <- pca$x[, 2]
    data.pca.1_3[complete.cases(data.pca), 2] <- PC2
    pred1[[v.2]] <- data.pca.1_3[, 2]
  }

  if (nb.pca >= 3) {
    pc3 <- pca$x[, 3]
    data.pca.1_3[complete.cases(data.pca), 3] <- PC3
    pred1[[v.3]] <- data.pca.1_3[, 3]
  }

  message("[INFO] #PCA: ", nb.pca)
  nb.var <- nb.pca
  nb.series <- nb.pca
  nb.stations <- 1

  ################################
  # 6 - Organize data from inputs
  ################################

  Y.analogs.p1.v1 <- list()

  for (i in 1:(2 * M + 1)) {
    Y.analogs.p1.v1[[i]] <- pred1[[v.1]][i:(pred1$n0 + i - 2 * M - 2)]
  }
  Y.analogs.p1.v1 = do.call(cbind, Y.analogs.p1.v1)

  Y.predictions.p1.v1 <- list()
  for (i in 1:(2 * M + 1)) {
    Y.predictions.p1.v1[[i]] <-
      pred1[[v.1]][(i - M - 1 + pred1$n0):(pred1$N + i - 2 * M - 1)]
  }
  Y.predictions.p1.v1 = do.call(cbind, Y.predictions.p1.v1)

  if (nb.stations >= 2) {
    Y.analogs.p2.v1 <- list()
    for (i in 1:(2 * M + 1)) {
      Y.analogs.p2.v1[[i]] <- pred2[[v.1]][i:(pred2$n0 + i - 2 * M - 2)]
    }
    Y.analogs.p2.v1 = do.call(cbind, Y.analogs.p2.v1)

    Y.predictions.p2.v1 <- list()
    for (i in 1:(2 * M + 1)) {
      Y.predictions.p2.v1[[i]] <-
        pred2[[v.1]][(i - M - 1 + pred2$n0):(pred2$N + i - 2 * M - 1)]
    }
    Y.predictions.p2.v1 = do.call(cbind, Y.predictions.p2.v1)
  }

  if (nb.stations >= 3) {
    Y.analogs.p3.v1 <- list()
    for (i in 1:(2 * M + 1)) {
      Y.analogs.p3.v1[[i]] <- pred3[[v.1]][i:(pred3$n0 + i - 2 * M - 2)]
    }
    Y.analogs.p3.v1 = do.call(cbind, Y.analogs.p3.v1)

    Y.predictions.p3.v1 <- list()
    for (i in 1:(2 * M + 1)) {
      Y.predictions.p3.v1[[i]] <-
        pred3[[v.1]][(i - M - 1 + pred3$n0):(pred3$N + i - 2 * M - 1)]
    }
    Y.predictions.p3.v1 = do.call(cbind, Y.predictions.p3.v1)
  }

  if (nb.var >= 2) {
    Y.analogs.p1.v2 <- list()
    for (i in 1:(2 * M + 1)) {
      Y.analogs.p1.v2[[i]] <- (pred1[[v.2]][i:(pred1$n0 + i - 2 * M - 2)])
    }
    Y.analogs.p1.v2 = do.call(cbind, Y.analogs.p1.v2)

    Y.predictions.p1.v2 <- list()
    for (i in 1:(2 * M + 1)) {
      Y.predictions.p1.v2[[i]] <-
        pred1[[v.2]][(i - M - 1 + pred1$n0):(pred1$N + i - 2 * M - 1)]
    }
    Y.predictions.p1.v2 = do.call(cbind, Y.predictions.p1.v2)

    if (nb.stations >= 2) {
      Y.analogs.p2.v2 <- list()
      for (i in 1:(2 * M + 1)) {
        Y.analogs.p2.v2[[i]] <- (pred2[[v.2]][i:(pred2$n0 + i - 2 * M - 2)])
      }
      Y.analogs.p2.v2 = do.call(cbind, Y.analogs.p2.v2)

      Y.predictions.p2.v2 <- list()
      for (i in 1:(2 * M + 1)) {
        Y.predictions.p2.v2[[i]] <-
          pred2[[v.2]][(i - M - 1 + pred2$n0):(pred2$N + i - 2 * M - 1)]
      }
      Y.predictions.p2.v2 = do.call(cbind, Y.predictions.p2.v2)
    }

    if (nb.stations >= 3) {
      Y.analogs.p3.v2 <- list()
      for (i in 1:(2 * M + 1)) {
        Y.analogs.p3.v2[[i]] <- (pred3[[v.2]][i:(pred3$n0 + i - 2 * M - 2)])
      }
      Y.analogs.p3.v2 = do.call(cbind, Y.analogs.p3.v2)

      Y.predictions.p3.v2 <- list()
      for (i in 1:(2 * M + 1)) {
        Y.predictions.p3.v2[[i]] <-
          pred3[[v.2]][(i - M - 1 + pred3$n0):(pred3$N + i - 2 * M - 1)]
      }
      Y.predictions.p3.v2 = do.call(cbind, Y.predictions.p3.v2)
    }
  }

  if (nb.var >= 3) {
    Y.analogs.p1.v3 <- list()
    for (i in 1:(2 * M + 1)) {
      Y.analogs.p1.v3[[i]] <- (pred1[[v.3]][i:(pred1$n0 + i - 2 * M - 2)])
    }
    Y.analogs.p1.v3 = do.call(cbind, Y.analogs.p1.v3)

    Y.predictions.p1.v3 <- list()
    for (i in 1:(2 * M + 1)) {
      Y.predictions.p1.v3[[i]] <-
        pred1[[v.3]][(i - M - 1 + pred1$n0):(pred1$N + i - 2 * M - 1)]
    }
    Y.predictions.p1.v3 = do.call(cbind, Y.predictions.p1.v3)

    if (nb.stations >= 2) {
      Y.analogs.p2.v3 <- list()
      for (i in 1:(2 * M + 1)) {
        Y.analogs.p2.v3[[i]] <- (pred2[[v.3]][i:(pred2$n0 + i - 2 * M - 2)])
      }
      Y.analogs.p2.v3 = do.call(cbind, Y.analogs.p2.v3)

      Y.predictions.p2.v3 <- list()
      for (i in 1:(2 * M + 1)) {
        Y.predictions.p2.v3[[i]] <-
          pred2[[v.3]][(i - M - 1 + pred2$n0):(pred2$N + i - 2 * M - 1)]
      }
      Y.predictions.p2.v3 = do.call(cbind, Y.predictions.p2.v3)
    }

    if (nb.stations >= 3) {
      Y.analogs.p3.v3 <- list()
      for (i in 1:(2 * M + 1)) {
        Y.analogs.p3.v3[[i]] <- (pred3[[v.3]][i:(pred3$n0 + i - 2 * M - 2)])
      }
      Y.analogs.p3.v3 = do.call(cbind, Y.analogs.p3.v3)

      Y.predictions.p3.v3 <- list()
      for (i in 1:(2 * M + 1)) {
        Y.predictions.p3.v3[[i]] <-
          pred3[[v.3]][(i - M - 1 + pred3$n0):(pred3$N + i - 2 * M - 1)]
      }
      Y.predictions.p3.v3 = do.call(cbind, Y.predictions.p3.v3)
    }
  }

  ################################################################
  # 5.1 - REDUCTION OF TIME: FORECASTING BETWEEN 10AM AND 12AM
  ################################################################

  validHours <-
    (format(strptime(pred1$stamp, "%Y-%m-%d %H:%M:%S"), '%H:%M:%S') >=  format(
      paste(startH, ":00:00", sep = ""),
      format = "%H:%M:%S",
      tz = "UTC"
    )) &
    (format(strptime(pred1$stamp, "%Y-%m-%d %H:%M:%S"), '%H:%M:%S') <=  format(
      paste(endH, ":00:00", sep = ""),
      format = "%H:%M:%S",
      tz = "UTC"
    ))

  old.1n0 = pred1$n0
  old.1N = pred1$N

  pred1$t <- pred1$t[validHours]
  pred1$stamp <- pred1$stamp[validHours]
  pred1$N = length(pred1$stamp)
  pred1$WSPD <- pred1$WSPD[validHours]
  pred1$WDIR <- pred1$WDIR[validHours]
  pred1$ATMP <- pred1$ATMP[validHours]
  pred1$PRES <- pred1$PRES[validHours]
  pred1$GST <- pred1$GST[validHours]
  pred1$WTMP <- pred1$WTMP[validHours]
  pred1$n0 <- match(forecasting.t0, pred1$stamp)

  target$t <- target$t[validHours]
  target$stamp <- target$stamp[validHours]
  target$N = length(target$stamp)
  target$WSPD <- target$WSPD[validHours]
  target$WDIR <- target$WDIR[validHours]
  target$ATMP <- target$ATMP[validHours]
  target$PRES <- target$PRES[validHours]
  target$GST <- target$GST[validHours]
  target$WTMP <- target$WTMP[validHours]
  target$n0 <- match(forecasting.t0, target$stamp)

  n1 = which(pred1$t < pred1$t0)

  std.p1.v1 = sd(pred1[[v.1]][1:(pred1$n0 - 1)], na.rm = TRUE)
  std.p1.v2 = 1.0
  std.p1.v3 = 1.0
  std.p2.v1 = 1.0
  std.p2.v2 = 1.0
  std.p2.v3 = 1.0
  std.p3.v1 = 1.0
  std.p3.v2 = 1.0
  std.p3.v3 = 1.0

  mean.p1.v1 = mean(pred1[[v.1]][1:(pred1$n0 - 1)], na.rm = TRUE)
  mean.p1.v2 = 1.0
  mean.p1.v3 = 1.0
  mean.p2.v1 = 1.0
  mean.p2.v2 = 1.0
  mean.p2.v3 = 1.0
  mean.p3.v1 = 1.0
  mean.p3.v2 = 1.0
  mean.p3.v3 = 1.0

  if (v.1 == 'WDIR') {
    Y.analogs.n.p1.v1 <-
      (Y.analogs.p1.v1[validHours[(M + 1):(old.1n0 - 2 - M)], ])
    Y.predictions.n.p1.v1 <-
      (Y.predictions.p1.v1[validHours[old.1n0:(old.1N - M)], ])
  } else {
    Y.analogs.n.p1.v1 <-
      (Y.analogs.p1.v1[validHours[(M + 1):(old.1n0 - 2 - M)], ] - mean.p1.v1) /
      std.p1.v1
    Y.predictions.n.p1.v1 <-
      (Y.predictions.p1.v1[validHours[old.1n0:(old.1N - M)], ] - mean.p1.v1) /
      std.p1.v1
  }
  Y.analogs.p1.v1 <-
    (Y.analogs.p1.v1[validHours[(M + 1):(old.1n0 - 2 - M)], ])

  Y.predictions.p1.v1 <-
    (Y.predictions.p1.v1[validHours[old.1n0:(old.1N - M)], ])



  if (nb.var >= 2) {
    std.p1.v2 = sd(pred1[[v.2]][1:(pred1$n0 - 1)], na.rm = TRUE)
    mean.p1.v2 = mean(pred1[[v.2]][1:(pred1$n0 - 1)], na.rm = TRUE)

    if (v.2 == 'WDIR') {
      Y.analogs.n.p1.v2 <-
        (Y.analogs.p1.v2[validHours[(M + 1):(old.1n0 - 2 - M)], ])
      Y.predictions.n.p1.v2 <-
        (Y.predictions.p1.v2[validHours[old.1n0:(old.1N - M)], ])
    } else {
      Y.analogs.n.p1.v2 <-
        (Y.analogs.p1.v2[validHours[(M + 1):(old.1n0 - 2 - M)], ] - mean.p1.v2) /
        std.p1.v2
      Y.predictions.n.p1.v2 <-
        (Y.predictions.p1.v2[validHours[old.1n0:(old.1N - M)], ] - mean.p1.v2) /
        std.p1.v2
    }
    Y.analogs.p1.v2 <-
      (Y.analogs.p1.v2[validHours[(M + 1):(old.1n0 - 2 - M)], ])
    Y.predictions.p1.v2 <-
      (Y.predictions.p1.v2[validHours[old.1n0:(old.1N - M)], ])
  }

  if (nb.var >= 3) {
    std.p1.v3 = sd(pred1[[v.3]][1:(pred1$n0 - 1)], na.rm = TRUE)
    mean.p1.v3 = mean(pred1[[v.3]][1:(pred1$n0 - 1)], na.rm = TRUE)

    if (v.3 == 'WDIR') {
      Y.analogs.n.p1.v3 <-
        (Y.analogs.p1.v3[validHours[(M + 1):(old.1n0 - 2 - M)], ])
      Y.predictions.n.p1.v3 <-
        (Y.predictions.p1.v3[validHours[old.1n0:(old.1N - M)], ])
    } else {
      Y.analogs.n.p1.v3 <-
        (Y.analogs.p1.v3[validHours[(M + 1):(old.1n0 - 2 - M)], ] - mean.p1.v3) /
        std.p1.v3
      Y.predictions.n.p1.v3 <-
        (Y.predictions.p1.v3[validHours[old.1n0:(old.1N - M)], ] - mean.p1.v3) /
        std.p1.v3
    }
    Y.analogs.p1.v3 <-
      (Y.analogs.p1.v3[validHours[(M + 1):(old.1n0 - 2 - M)], ])
    Y.predictions.p1.v3 <-
      (Y.predictions.p1.v3[validHours[old.1n0:(old.1N - M)], ])
  }

  ################################################################
  # 6 - Organizing the Y data
  ################################################################

  Y <-
    matrix(
      data = NA,
      nrow = length(Y.analogs.p1.v1[, 1]),
      ncol = nb.series * (2 * M + 1)
    )
  Y[, 1:(2 * M + 1)] <- Y.analogs.p1.v1
  if (nb.var >= 2) {
    Y[, ((2 * M + 1) + 1):(2 * (2 * M + 1))] <- Y.analogs.p1.v2
  }
  if (nb.var >= 3) {
    Y[, (2 * (2 * M + 1) + 1):(3 * (2 * M + 1))] <- Y.analogs.p1.v3
  }

  ################################################################
  # 7 - Organizing the Y.n data
  ################################################################

  Y.n <-
    matrix(
      data = NA,
      nrow = length(Y.analogs.n.p1.v1[, 1]),
      ncol = nb.series * (2 * M + 1)
    )
  Y.n[, 1:(2 * M + 1)] <- Y.analogs.n.p1.v1
  if (nb.var >= 2) {
    Y.n[, ((2 * M + 1) + 1):(2 * (2 * M + 1))] <- Y.analogs.n.p1.v2
  }
  if (nb.var >= 3) {
    Y.n[, (2 * (2 * M + 1) + 1):(3 * (2 * M + 1))] <- Y.analogs.n.p1.v3
  }

  ################################################################
  # 8 - Organizing the Y.pred data
  ################################################################

  Y.pred <-
    matrix(
      data = NA,
      nrow = length(Y.predictions.p1.v1[, 1]),
      ncol = nb.series * (2 * M + 1)
    )
  Y.pred[, 1:(2 * M + 1)] <- Y.predictions.p1.v1
  if (nb.var >= 2) {
    Y.pred[, ((2 * M + 1) + 1):(2 * (2 * M + 1))] <- Y.predictions.p1.v2
  }
  if (nb.var >= 3) {
    Y.pred[, (2 * (2 * M + 1) + 1):(3 * (2 * M + 1))] <-
      Y.predictions.p1.v3
  }

  ################################################################
  # 9 - Organizing the Y.pred.n data
  ################################################################

  Y.pred.n <-
    matrix(
      data = NA,
      nrow = length(Y.predictions.n.p1.v1[, 1]),
      ncol = nb.series * (2 * M + 1)
    )

  Y.pred.n[, 1:(2 * M + 1)] <- Y.predictions.n.p1.v1
  if (nb.var >= 2) {
    Y.pred.n[, ((2 * M + 1) + 1):(2 * (2 * M + 1))] <-
      Y.predictions.n.p1.v2
  }

  if (nb.var >= 3) {
    Y.pred.n[, (2 * (2 * M + 1) + 1):(3 * (2 * M + 1))] <-
      Y.predictions.n.p1.v3
  }

  ################################################################
  # 10 - Organizing the same of previous for more than 2 station
  ################################################################

  if (nb.stations >= 2) {
    old.2n0 = pred2$n0
    old.2N = pred2$N

    pred2$t <- pred2$t[validHours]
    pred2$stamp <- pred2$stamp[validHours]
    pred2$N = length(pred2$stamp)
    pred2$WSPD <- pred2$WSPD[validHours]
    pred2$WDIR <- pred2$WDIR[validHours]
    pred2$ATMP <- pred2$ATMP[validHours]
    pred2$PRES <- pred2$PRES[validHours]
    pred2$GST <- pred2$GST[validHours]
    pred2$WTMP <- pred2$WTMP[validHours]
    pred2$n0 <- match(forecasting.t0, pred2$stamp)

    n2 = which(pred2$t < pred2$t0)

    std.p2.v1 = sd(pred2[[v.1]][1:(pred2$n0 - 1)], na.rm = TRUE)
    mean.p2.v1 = mean(pred2[[v.1]][1:(pred2$n0 - 1)], na.rm = TRUE)

    if (v.1 == 'WDIR') {
      Y.analogs.n.p2.v1 <-
        (Y.analogs.p2.v1[validHours[(M + 1):(old.2n0 - 2 - M)], ])
      Y.predictions.n.p2.v1 <-
        (Y.predictions.p2.v1[validHours[old.2n0:(old.2N - M)], ])
    } else {
      Y.analogs.n.p2.v1 <-
        (Y.analogs.p2.v1[validHours[(M + 1):(old.2n0 - 2 - M)], ] - mean.p2.v1) /
        std.p2.v1
      Y.predictions.n.p2.v1 <-
        (Y.predictions.p2.v1[validHours[old.2n0:(old.2N - M)], ] - mean.p2.v1) /
        std.p2.v1
    }
    Y.analogs.p2.v1 <-
      (Y.analogs.p2.v1[validHours[(M + 1):(old.2n0 - 2 - M)], ])
    Y.predictions.p2.v1 <-
      (Y.predictions.p2.v1[validHours[old.2n0:(old.2N - M)], ])


    if (nb.var >= 2) {
      std.p2.v2 = sd(pred2[[v.2]][1:(pred2$n0 - 1)], na.rm = TRUE)
      mean.p2.v2 = mean(pred2[[v.2]][1:(pred2$n0 - 1)], na.rm = TRUE)

      if (v.2 == 'WDIR') {
        Y.analogs.n.p2.v2 <-
          (Y.analogs.p2.v2[validHours[(M + 1):(old.2n0 - 2 - M)]])
        Y.predictions.n.p2.v2 <-
          (Y.predictions.p2.v2[validHours[old.2n0:(old.2N - M)], ])
      } else {
        Y.analogs.n.p2.v2 <-
          (Y.analogs.p2.v2[validHours[(M + 1):(old.2n0 - 2 - M)], ] - mean.p2.v2) /
          std.p2.v2
        Y.predictions.n.p2.v2 <-
          (Y.predictions.p2.v2[validHours[old.2n0:(old.2N - M)], ] - mean.p2.v2) /
          std.p2.v2

      }
      Y.analogs.p2.v2 <-
        (Y.analogs.p2.v2[validHours[(M + 1):(old.2n0 - 2 - M)], ])
      Y.predictions.p2.v2 <-
        (Y.predictions.p2.v2[validHours[old.2n0:(old.2N - M)], ])
    }

    if (nb.var >= 3) {
      std.p2.v3 = sd(pred2[[v.3]][1:(pred2$n0 - 1)], na.rm = TRUE)
      mean.p2.v3 = mean(pred2[[v.3]][1:(pred2$n0 - 1)], na.rm = TRUE)

      if (v.3 == 'WDIR') {
        Y.analogs.n.p2.v3 <-
          (Y.analogs.p2.v3[validHours[(M + 1):(old.2n0 - 2 - M)]])
        Y.predictions.n.p2.v3 <-
          (Y.predictions.p2.v3[validHours[old.2n0:(old.2N - M)], ])
      } else {
        Y.analogs.n.p2.v3 <-
          (Y.analogs.p2.v3[validHours[(M + 1):(old.2n0 - 2 - M)]] - mean.p2.v3) /
          std.p2.v3
        Y.predictions.n.p2.v3 <-
          (Y.predictions.p2.v3[validHours[old.2n0:(old.2N - M)], ] - mean.p2.v3) /
          std.p2.v3
      }
      Y.analogs.p2.v3 <-
        (Y.analogs.p2.v3[validHours[(M + 1):(old.2n0 - 2 - M)]])
      Y.predictions.p2.v3 <-
        (Y.predictions.p2.v3[validHours[old.2n0:(old.2N - M)], ])
    }

    index.debut = nb.var

    ################################################################
    # 11 - Completing Y with other stations
    ################################################################

    Y[, (index.debut * (2 * M + 1) + 1):((index.debut + 1) * (2 * M + 1))] <-
      Y.analogs.p2.v1
    if (nb.var >= 2) {
      Y[, ((index.debut + 1) * (2 * M + 1) + 1):((index.debut + 2) * (2 * M +
                                                                        1))] <-
        Y.analogs.p2.v2
    }
    if (nb.var >= 3) {
      Y[, (5 * (2 * M + 1) + 1):(6 * (2 * M + 1))] <- Y.analogs.p2.v3
    }

    ################################################################
    # 12 - Completing Y.n with other stations
    ################################################################

    Y.n[, (index.debut * (2 * M + 1) + 1):((index.debut + 1) * (2 * M + 1))] <-
      Y.analogs.n.p2.v1
    if (nb.var >= 2) {
      Y.n[, ((index.debut + 1) * (2 * M + 1) + 1):((index.debut + 2) * (2 * M +
                                                                          1))] <-
        Y.analogs.n.p2.v2
    }
    if (nb.var >= 3) {
      Y.n[, (5 * (2 * M + 1) + 1):(6 * (2 * M + 1))] <- Y.analogs.n.p2.v3
    }

    ################################################################
    # 13 - Completing Y.pred with other stations
    ################################################################

    Y.pred[, (index.debut * (2 * M + 1) + 1):((index.debut + 1) * (2 * M +
                                                                     1))] <-
      Y.predictions.p2.v1
    if (nb.var >= 2) {
      Y.pred[, ((index.debut + 1) * (2 * M + 1) + 1):((index.debut + 2) * (2 *
                                                                             M + 1))] <-
        Y.predictions.p2.v2
    }
    if (nb.var >= 3) {
      Y.pred[, (5 * (2 * M + 1) + 1):(6 * (2 * M + 1))] <-
        Y.predictions.p2.v3
    }

    ################################################################
    # 13 - Completing Y.pred.n with other stations
    ################################################################

    Y.pred.n[, (index.debut * (2 * M + 1) + 1):((index.debut + 1) * (2 * M +
                                                                       1))] <-
      Y.predictions.n.p2.v1
    if (nb.var >= 2) {
      Y.pred.n[, ((index.debut + 1) * (2 * M + 1) + 1):((index.debut + 2) * (2 *
                                                                               M + 1))] <-
        Y.predictions.n.p2.v2
    }
    if (nb.var >= 3) {
      Y.pred.n[, (5 * (2 * M + 1) + 1):(6 * (2 * M + 1))] <-
        Y.predictions.n.p2.v3
    }
  }

  ################################################################
  # 14 - The same thing for more than 3 stations
  ################################################################

  if (nb.stations >= 3) {
    old.3n0 = pred3$n0
    old.3N = pred3$N

    pred3$t <- pred3$t[validHours]
    pred3$stamp <- pred3$stamp[validHours]
    pred3$N = length(pred3$stamp)
    pred3$WSPD <- pred3$WSPD[validHours]
    pred3$WDIR <- pred3$WDIR[validHours]
    pred3$ATMP <- pred3$ATMP[validHours]
    pred3$PRES <- pred3$PRES[validHours]
    pred3$GST <- pred3$GST[validHours]
    pred3$WTMP <- pred3$WTMP[validHours]
    pred3$n0 <- match(forecasting.t0, pred3$stamp)

    n3 = which(pred3$t < pred3$t0)

    std.p3.v1 = sd(pred3[[v.1]][1:(pred3$n0 - 1)], na.rm = TRUE)
    mean.p3.v1 = mean(pred3[[v.1]][1:(pred3$n0 - 1)], na.rm = TRUE)

    if (v.1 == 'WDIR') {
      Y.analogs.n.p3.v1 <-
        (Y.analogs.p3.v1[validHours[(M + 1):(old.3n0 - 2 - M)], ])
    } else {
      Y.analogs.n.p3.v1 <-
        (Y.analogs.p3.v1[validHours[(M + 1):(old.3n0 - 2 - M)], ] - mean.p3.v1) /
        std.p3.v1
    }

    Y.analogs.p3.v1 <-
      (Y.analogs.p3.v1[validHours[(M + 1):(old.3n0 - 2 - M)], ])

    index.debut = 2 * nb.var

    Y[, (index.debut * (2 * M + 1) + 1):((index.debut + 1) * (2 * M + 1))] <-
      Y.analogs.p3.v1
    Y.n[, (index.debut * (2 * M + 1) + 1):((index.debut + 1) * (2 * M + 1))] <-
      Y.analogs.n.p3.v1

    if (v.1 == 'WDIR') {
      Y.predictions.n.p3.v1 <-
        (Y.predictions.p3.v1[validHours[old.2n0:(old.2N - M)], ])
    } else {
      Y.predictions.n.p3.v1 <-
        (Y.predictions.p3.v1[validHours[old.2n0:(old.2N - M)], ] - mean.p3.v1) /
        std.p3.v1
    }

    Y.predictions.p3.v1 <-
      (Y.predictions.p3.v1[validHours[old.2n0:(old.2N - M)], ])

    Y.pred[, (index.debut * (2 * M + 1) + 1):((index.debut + 1) * (2 * M +
                                                                     1))] <-
      Y.predictions.p3.v1
    Y.pred.n[, (index.debut * (2 * M + 1) + 1):((index.debut + 1) * (2 * M +
                                                                       1))] <-
      Y.predictions.n.p3.v1

    if (nb.var >= 2) {
      std.p3.v2 = sd(pred3[[v.2]][1:(pred3$n0 - 1)], na.rm = TRUE)
      mean.p3.v2 = mean(pred3[[v.2]][1:(pred3$n0 - 1)], na.rm = TRUE)

      if (v.2 == 'WDIR') {
        Y.analogs.p3.v2 <-
          (Y.analogs.p3.v2[validHours[(M + 1):(old.3n0 - 2 - M)], ])
        Y.predictions.p3.v2 <-
          (Y.predictions.p3.v2[validHours[old.2n0:(old.2N - M)], ])
      } else {
        Y.analogs.n.p3.v2 <-
          (Y.analogs.p3.v2[validHours[(M + 1):(old.3n0 - 2 - M)], ] - mean.p3.v2) /
          std.p3.v2
        Y.predictions.n.p3.v2 <-
          (Y.predictions.p3.v2[validHours[old.2n0:(old.2N - M)], ] - mean.p3.v2) /
          std.p3.v2
      }

      Y.analogs.p3.v2 <-
        (Y.analogs.p3.v2[validHours[(M + 1):(old.3n0 - 2 - M)], ])

      Y[, ((index.debut + 1) * (2 * M + 1) + 1):((index.debut + 2) * (2 *
                                                                        M + 1))] <-
        Y.analogs.p3.v2
      Y.n[, ((index.debut + 1) * (2 * M + 1) + 1):((index.debut + 2) * (2 *
                                                                          M + 1))] <-
        Y.analogs.n.p3.v2

      Y.predictions.p3.v2 <-
        (Y.predictions.p3.v2[validHours[old.2n0:(old.2N - M)], ])

      Y.pred[, ((index.debut + 1) * (2 * M + 1) + 1):((index.debut + 2) *
                                                        (2 * M + 1))] <-
        Y.predictions.p3.v2
      Y.pred.n[, ((index.debut + 1) * (2 * M + 1) + 1):((index.debut + 2) *
                                                          (2 * M + 1))] <-
        Y.predictions.n.p3.v2
    }
    if (nb.var >= 3) {
      std.p3.v3 = sd(pred3[[v.3]][1:(pred3$n0 - 1)], na.rm = TRUE)
      mean.p3.v3 = mean(pred3[[v.3]][1:(pred3$n0 - 1)], na.rm = TRUE)

      if (v.3 == 'WDIR') {
        Y.analogs.p3.v3 <-
          (Y.analogs.p3.v3[validHours[(M + 1):(old.3n0 - 2 - M)], ])
        Y.predictions.p3.v3 <-
          (Y.predictions.p3.v3[validHours[old.2n0:(old.2N - M)], ])
      } else {
        Y.analogs.n.p3.v3 <-
          (Y.analogs.p3.v3[validHours[(M + 1):(old.3n0 - 2 - M)], ] - mean.p3.v3) /
          std.p3.v3
        Y.predictions.n.p3.v3 <-
          (Y.predictions.p3.v3[validHours[old.2n0:(old.2N - M)], ] - mean.p3.v3) /
          std.p3.v3
      }

      Y.analogs.p3.v3 <-
        (Y.analogs.p3.v3[validHours[(M + 1):(old.3n0 - 2 - M)], ])

      Y[, (8 * (2 * M + 1) + 1):(9 * (2 * M + 1))] <-
        Y.analogs.p3.v3
      Y.n[, (8 * (2 * M + 1) + 1):(9 * (2 * M + 1))] <-
        Y.analogs.n.p3.v3


      Y.predictions.p3.v3 <-
        (Y.predictions.p3.v3[validHours[old.2n0:(old.2N - M)], ])

      Y.pred[, (8 * (2 * M + 1) + 1):(9 * (2 * M + 1))] <-
        Y.predictions.p3.v3
      Y.pred.n[, (8 * (2 * M + 1) + 1):(9 * (2 * M + 1))] <-
        Y.predictions.n.p3.v3
    }
  }

  Ynan <-
    rowSums(is.na(Y)) >= .5 * M * nb.series

  #message("[RESULT] Loading Running time: ", paste0(round(as.numeric(
  #  difftime(
  #    time1 = end_time,
  ##    time2 = start_time,
  #    units = "secs"
  #  )
  #), 3), " Seconds"))

  ################################################################
  # 15 - Changing to main directory
  ################################################################

  setwd("~/")
  setwd("AnEnMDataR")
  gc()
  ################################################################
  #  END OF FUNCTION
  ################################################################




