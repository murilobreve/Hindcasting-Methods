#####################################################################################
## This source loads and organizes inputs stations into usable data to the methods ##
## Adapted by Murilo Montanini, 1st is Leonardo from the old project               ##
#####################################################################################

##############################
# 0 - Change directories
##############################
nb.series <- nb.stations * nb.var

message("[INFO] Loading inteporlate function")
setwd("~/")
setwd("AnEnMDataR/R")
source("InterpolateData.R")

setwd("~/")
setwd("AnEnMDataR/weather-station-data")

##############################
# 1 - Load the first .nc file
##############################

ds <- nc_open(name.pred1)
pred1 <- list()
pred1$epoch <- as.POSIXct('1970-01-01',  tz = "UTC")
pred1$N <- length(ncvar_get(ds, "time"))
pred1$t <- ncvar_get(ds, "time")
pred1$stamp <-
  as.POSIXct(pred1$t * netCDF_resolution, origin = pred1$epoch, tz = "UTC") # To 30 min resolution

for (i in seq_len(length(variables))) {
  pred1[[variables[i]]] <- ncvar_get(ds, variables[i])
  if (ncatt_get(ds, variables[i], "_FillValue")$hasatt) {
    pred1[[variables[i]]][pred1[[variables[i]]] == ncatt_get(ds, variables[i], "_FillValue")$value] <-
      NA
  } else {
    pred1[[variables[i]]][pred1[[variables[i]]] > 9e+36] <-
      NA  # missing value
  }
}

nc_close(ds)

# 1.1 - Interpolation between start_of_date and end_of_date

if(interp){
  message("[INFO] Interpolating first station")
  pred1.interp <-
    InterpolateData(pred1$stamp, pred1[[v.1]], start_of_date, end_of_date, time_interval)
  pred1$stamp <- pred1.interp[, 1]
  pred1[[v.1]] <- pred1.interp[, 3]
}

# 1.12 - Transforming to 22.5 degree resolution, 16 different cardinales (Ex: NNE, NNW)
if(v.1 == "WDIR")
  pred1[[v.1]] <- 22.5*(round(pred1[[v.1]] / 22.5))

# 1.2 - Setting the intersection between forecasting.t0 and the pred1$stamp
pred1$t0 <-
  (as.numeric(as.POSIXct(forecasting.t0, tz = "UTC")) - as.numeric(pred1$epoch)) / netCDF_resolution
pred1$t0
pred1$n0 <-
  match(forecasting.t0, pred1$stamp)
pred1$n0
pred1$t <- as.numeric(as.POSIXct(pred1$stamp))
pred1$N <- length(pred1$stamp)

##############################
# 2 - Load the second .nc file
##############################

if (nb.stations >= 2) {
  ds <- nc_open(name.pred2)
  pred2 <- list()
  pred2$epoch <- as.POSIXct('1970-01-01',  tz = "UTC")
  pred2$N <- length(ncvar_get(ds, "time"))
  pred2$t <- ncvar_get(ds, "time")
  pred2$stamp <-
    as.POSIXct(pred2$t * netCDF_resolution, origin = pred2$epoch, tz = "UTC") # To 30 min resolution

  for (i in seq_len(length(variables))) {
    pred2[[variables[i]]] <- ncvar_get(ds, variables[i])
    if (ncatt_get(ds, variables[i], "_FillValue")$hasatt) {
      pred2[[variables[i]]][pred2[[variables[i]]] == ncatt_get(ds, variables[i], "_FillValue")$value] <-
        NA
    } else {
      pred2[[variables[i]]][pred2[[variables[i]]] > 9e+36] <-
        NA  # missing value
    }
  }

  nc_close(ds)

  # 2.1 - Interpolation between start_of_date and end_of_date
  if(interp){
    pred2.interp <-
      InterpolateData(pred2$stamp, pred2[[v.2]], start_of_date, end_of_date, time_interval)
    pred2$stamp <- pred2.interp[, 1]
    pred2[[v.2]] <- pred2.interp[, 3]

    if(v.2 == "WDIR")
      pred2[[v.2]] <- 22.5*(round(pred2[[v.2]] / 22.5))
  }


  # 2.2 - Setting the intersection between forecasting.t0 and the pred2$stamp
  pred2$t0 <-
    (as.numeric(as.POSIXct(forecasting.t0, tz = "UTC")) - as.numeric(pred2$epoch)) / netCDF_resolution
  pred2$t0
  pred2$n0 <-
    match(forecasting.t0, pred2$stamp)  # starting index for forecast period
  pred2$n0
  pred2$t <- as.numeric(as.POSIXct(pred2$stamp))
  pred2$N <- length(pred2$stamp)
}


##############################
# 3 - Load the third .nc file
##############################

if (nb.stations >= 3) {

  ds <- nc_open(name.pred3)

  pred3 <- list()
  pred3$epoch <- as.POSIXct('1970-01-01',  tz = "UTC")
  pred3$N <- length(ncvar_get(ds, "time"))
  pred3$t <- ncvar_get(ds, "time")
  pred3$stamp <-
    as.POSIXct(pred3$t * netCDF_resolution, origin = pred3$epoch, tz = "UTC") # To 30 min resolution

  pred3$WSPD <- ncvar_get(ds, "WSPD")
  if (ncatt_get(ds, "WSPD", "_FillValue")$hasatt) {
    pred3$WSPD[pred3$WSPD == ncatt_get(ds, "WSPD", "_FillValue")$value] <-
      NA
  } else {
    pred3$WSPD[pred3$WSPD > 9e+36] <- NA  # missing value
  }

  pred3$ATMP <- ncvar_get(ds, "ATMP")
  if (ncatt_get(ds, "ATMP", "_FillValue")$hasatt) {
    pred3$ATMP[pred3$ATMP == ncatt_get(ds, "ATMP", "_FillValue")$value] <-
      NA
  } else {
    pred3$ATMP[pred3$ATMP > 9e+36] <- NA  # missing value
  }

  pred3$PRES <- ncvar_get(ds, "PRES")
  if (ncatt_get(ds, "PRES", "_FillValue")$hasatt) {
    pred3$PRES[pred3$PRES == ncatt_get(ds, "PRES", "_FillValue")$value] <-
      NA
  } else {
    pred3$PRES[pred3$PRES > 9e+36] <- NA  # missing value
  }

  pred3$WDIR <- ncvar_get(ds, "WDIR")
  if (ncatt_get(ds, "WDIR", "_FillValue")$hasatt) {
    pred3$WDIR[pred3$WDIR == ncatt_get(ds, "WDIR", "_FillValue")$value] <- NA
  } else {
    pred3$WDIR[pred3$WDIR > 9e+36] <- NA  # missing value
  }

  pred3$GST <- ncvar_get(ds, "GST")
  if (ncatt_get(ds, "GST", "_FillValue")$hasatt) {
    pred3$GST[pred3$GST == ncatt_get(ds, "GST", "_FillValue")$value] <-
      NA
  } else {
    pred3$GST[pred3$GST > 9e+36] <- NA  # missing value
  }

  nc_close(ds)

  # 3.1 - Interpolation between start_of_date and end_of_date
  if(interp){
    pred3.interp <-
      InterpolateData(pred3$stamp, pred3[[v.aim]], start_of_date, end_of_date, time_interval)
    pred3$stamp <- pred3.interp[, 1]
    pred3[[v.3]] <- pred3.interp[, 3]

    if(v.3 == "WDIR")
      pred3[[v.3]] <- 22.5*(round(pred3[[v.3]] / 22.5))
  }

  # 3.2 Setting the intersection between forecasting.t0 and the pred3$stamp
  pred3$t0 <-
    (as.numeric(as.POSIXct(forecasting.t0, tz = "UTC")) - as.numeric(pred3$epoch)) / netCDF_resolution
  pred3$n0 <-
    match(forecasting.t0, pred2$stamp)
  pred3$t <- as.numeric(as.POSIXct(pred3$stamp))
  pred3$N <- length(pred3$stamp)
}

##############################
# 4 - Load the target station .nc file
##############################
ds <- nc_open(name.hist)

target <- list()
target$epoch <-
  as.POSIXct('1970-01-01',  tz = "UTC")
target$N <- length(ncvar_get(ds, "time"))
target$t <- ncvar_get(ds, "time")
target$stamp <-
  as.POSIXct(target$t * netCDF_resolution, origin = target$epoch, tz = "UTC") # To 30 min resolution

for (i in seq_len(length(variables))) {
  target[[variables[i]]] <- ncvar_get(ds, variables[i])
  if (ncatt_get(ds, variables[i], "_FillValue")$hasatt) {
    target[[variables[i]]][target[[variables[i]]] == ncatt_get(ds, variables[i], "_FillValue")$value] <-
      NA
  } else {
    target[[variables[i]]][target[[variables[i]]] > 9e+36] <-
      NA  # missing value
  }
}

nc_close(ds)

# 4.1 - Interpolation between start_of_date and end_of_date
if(interp){
  target.interp <-
    InterpolateData(target$stamp, target[[v.aim]], start_of_date, end_of_date, time_interval)
  target$stamp <- target.interp[, 1]
  target[[v.aim]] <- target.interp[, 3]

  if(v.aim == "WDIR")
    target[[v.aim]] <- 22.5 * (round(target[[v.aim]] / 22.5))
}

# 4.2 - Setting the intersection between forecasting.t0 and the target$stamp
target$t0 <-
  (as.numeric(as.POSIXct(forecasting.t0, tz = "UTC")) - as.numeric(target$epoch)) / netCDF_resolution
target$n0 <-
  match(forecasting.t0, target$stamp)  # starting index for forecast period
target$t <- as.numeric(as.POSIXct(target$stamp))
target$N <- length(target$stamp)

################################
# 5 - Organize data from inputs
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
    paste(startH, ":00:00",sep = ""),
    format = "%H:%M:%S",
    tz = "UTC"
  )) &
  (format(strptime(pred1$stamp, "%Y-%m-%d %H:%M:%S"), '%H:%M:%S') <=  format(
    paste(endH, ":00:00",sep = ""),
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
################################################################
# 15 - Changing to main directory
################################################################

setwd("~/")
setwd("AnEnMDataR")

################################################################
#  END OF FUNCTION
################################################################

