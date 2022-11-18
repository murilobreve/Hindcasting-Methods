library(ncdf4) # used to read .nc files (just used when load new combinations of stations)
library(future) # To predict witch variables need to pass to parsaply (maybe we don't need it)
library(parallel) # used for parallelization functions (always used)
library(forecast) # usando no tratamento das variáveis
library(zoo)
library(CircStats)
library(e1071) # used for C-means clusterization
library(cluster) #
library(dplyr)
library(factoextra)
library(circular)

name.hist = 'yktv2_2011_2019.nc'
name.pred1 = 'ykrv2_2011_2019.nc'
name.pred2 = 'domv2_2011_2019.nc'
name.pred3 = NULL
independent = FALSE
v.aim = 'WSPD'
v.1 = 'WSPD'
v.2 = 'WSPD'
v.3 = 'ATMP'
nb.stations = 2
nb.var = 1
cpus = 16
forecasting.t0 <- as.POSIXct("2018-01-01 10:00:00", tz="UTC")
M = 2
weight.type = "non"
method = "monache"
exitFile = "./Results_Rdata_Times/PRES_NON/"
Na = 150
nb.centros = 350
startH = 10
startM = 0
startS = 0
endH = 12
endM = 0
endS = 0
start_of_date <- "2011-01-01 10:00:00"
end_of_date <- "2019-01-01 10:00:00"
nb.pca <- 0
wdir <- FALSE
netCDF_resolution <- 60
interp <- FALSE
time_interval <- 6

name.hist <- "data_valongo.nc"
name.pred1 <- "data_edroso.nc"
name.pred1 <- "data_soutelo.nc"
name.pred3 = NULL
independent = FALSE
v.aim = 'GST'
v.1 = 'GST'
v.2 = 'GST'
v.3 = 'GST'
wdir <- TRUE
nb.pca <- 1
nb.stations <- 1
nb.var <- 1
cpus = 16
forecasting.t0 <- as.POSIXct("2006-01-01 10:00:00", tz="UTC")
start_of_date <- "2000-01-01 10:00:00"
end_of_date <- "2007-01-01 10:00:00"
M = 1
weight.type = "non"
method = "monache"
exitFile = "./Results_Rdata_Times/PRES_NON/"
Na = 50
nb.centros = 350
startH = 0
startM = 0
startS = 0
endH = 24
endM = 0
endS = 0
start_of_date <- "2000-01-01 10:00:00"
end_of_date <- "2007-01-01 10:00:00"
wdir <- FALSE
netCDF_resolution <- 1800
interp <- TRUE
time_interval <- 30
variables <- c("WSPD", "GST", "WDIR", "ATMP","PRES", "windchill", "HRH", "humidityin", "dew", "PRC", "tempin")
variables <- c("WSPD", "GST", "WDIR", "ATMP", "windchill", "humidity", "humidityin", "dew", "rain", "tempin")
  variables <- c("WSPD", "GST","ATMP", "HRH")
  variables <- c("WSPD", "GST","WDIR","ATMP", "HRH")

setwd("~/")
setwd("AnEnMDataR/R")
source("WhetherDataPrediction.R", local = TRUE)

variables = c("WSPD","GST","WDIR","ATMP","WINDCHILL", "HUMIDITY", "HUMIDITYin", "DEW", "PRC", "TEMPin")
variables = c("WSPD","GST","WDIR","ATMP","rain")

name.hist <- "data_valongo.nc"
name.pred1 <- "data_edroso.nc"
name.pred2 <- "data_soutelo.nc"

setwd("~/")
setwd("AnEnMDataR/R")
source("WhetherDataPrediction.R")

WhetherDataPrediction(name.hist,
                      name.pred1,
                      name.pred2,
                      name.pred3 <- NULL,
                      independent <- FALSE,
                      v.aim <- 'ATMP',
                      v.1 <- 'ATMP',
                      v.2 <- "ATMP",
                      v.3 <- NULL,
                      nb.stations <- 1,
                      nb.var <- 1,
                      start_of_date,
                      end_of_date,
                      forecasting.t0,
                      M <- 1,
                      Na <- 50,
                      startH <- 0,
                      startM <- 0,
                      startS <- 0,
                      endH <- 24,
                      endM <- 0,
                      endS <- 0,
                      nb.pca <- 0,
                      wdir <- FALSE,
                      time_interval <- 30,
                      netCDF_resolution <- 1800,
                      interp <- TRUE
)

