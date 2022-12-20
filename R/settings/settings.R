setwd("~/")
setwd("Hindcasting-Methods/data/predictor")
path_predictor <- getwd()
setwd("~/")
setwd("Hindcasting-Methods/data/predicted")
path_predicted <- getwd()

setwd("~/")
setwd("Hindcasting-Methods/data/NORFOLK_DATASET")
path_predictor <- getwd()
setwd("~/")
setwd("Hindcasting-Methods/data/predicted")
path_predicted <- getwd()

params_list <- list(
    # Period
    start_of_date <- "2016-01-01 00:00:00 UTC",
    #1
    end_of_date <- "2021-12-31 23:30:00 UTC",
    #2
    # Prediction Period
    prediction_t0 <-
        as.POSIXct("2021-01-01 10:00:00", tz = "UTC"),
    #3
    # Interval of time
    start_H <-  10,
    #4
    end_H <-  16,
    #5
    ## Data settings
    # If it's netCDF file
    netCDF_resolution <- 60,
    #6
    interp <- TRUE,
    #7
    # Resolution input data
    time_interval <- 6,
    #8
    ## Prediction Settings
    #Size of vector (2M + 1)
    M <-  5,
    #9
    #Number of Analogues
    Na <-  150,
    #10
    nb_centros <- 420,
    #11
    nb_iteracoes <- 400,
    #12
    # Parallelization
    ncores <- 16,
    #13
    # Paths
    path_predictor,
    #14
    path_predicted #15
)

names(params_list) <-
    c(
        "start_of_date",
        "end_of_date",
        "prediction_t0",
        "start_H",
        "end_H",
        "netCDF_resolution",
        "interp",
        "time_interval",
        "M",
        "Na",
        "nb_centros",
        "nb_iteracoes",
        "ncores",
        "path_predictor",
        "path_predicted"
    )
