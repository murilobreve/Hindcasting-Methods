#'@title Performs the hindcasting
#'@name WeatherDataPrediction
#'
#'@description The Analog Ensembles method (AnEn), introduced by Luca Delle Monache
#'is a post-processing tool that has shown good results to improve
#'whether predictions or perform hindcasting (reconstruction of missing meteorological data).
#'
#'@param name.hist Name of the historical station
#'@param name.pred1 Name of the predicted station
#'@param forecasting.t0 Prediction start time
#'@param start_of_date Data start time
#'@param end_of_date, Data end time
#'@param forecasting.t0 Prediction start time
#' (optional)
#'@param startH
#'@param startM
#'@param startS
#'@param endH
#'@param endM
#'@param endS
#'@param nc.pca PCA analysis quantity
#'@param wdir PCA analysis with wdir or not
#'@param time_interval Interpolation time interval
#'@param netCDF_resolution Data resolution
#'@param interp Data intepolation
#'
#'@return Results
#'
#'@author Murilo Montanini Breve
#'
#'@examples
#'library(AnEnDataR)
#'name.hist = 'data_valongo.nc'
#'name.pred1 = 'data_edroso.nc'
#'name.pred2 = 'data_soutelo.nc'
#'start_of_date <- "2000-01-01 10:00:00",
#'end_of_date <- "2007-01-01 10:00:00",
#'forecasting.t0 <- as.POSIXct("2006-01-01 10:00:00", tz="UTC"),
#'
#'WhetherDataPrediction <-
#'    function(name.hist <- 'data_valongo.nc',
#'    name.pred1 <- 'data_edroso.nc',
#'    name.pred2 <- 'data_soutelo.nc',
#'    name.pred3 <- NULL,
#'    independent <- FALSE,
#'    v.aim <- 'WSPD',
#'    v.1 <- 'WSPD',
#'    v.2 <- WSPD,
#'    v.3 <- NULL,
#'    nb.stations <- 2,
#'    nb.var <- 1,
#'    start_of_date <- "2000-01-01 10:00:00",
#'    end_of_date <- "2007-01-01 10:00:00",
#'    forecasting.t0 <- as.POSIXct("2006-01-01 10:00:00", tz="UTC"),
#'    M <- 1,
#'    Na <- 150,
#'    startH <- 0,
#'    startM <- 0,
#'    startS <- 0,
#'    endH <- 24,
#'    endM <- 0,
#'    endS <- 0,
#'    nb.pca <- 2,
#'    wdir <- FALSE,
#'    time_interval <- 30,
#'    netCDF_resolution <- 1800,
#'    interp <- TRUE)
#'
#'@import ncdf4
#'@import future
#'@import parallel
#'@import forecast
#'@import zoo
#'@import CircStats
#'@import dplyr
#'@import factoextra
#'@import circular
#'@export
WhetherDataPrediction <-
    function(name.hist,
             name.pred1,
             name.pred2 = NULL,
             name.pred3 = NULL,
             independent = FALSE,
             v.aim = 'WSPD',
             v.1 = 'WSPD',
             v.2 = NULL,
             v.3 = NULL,
             nb.stations = 2,
             nb.var = 1,
             start_of_date,
             end_of_date,
             forecasting.t0,
             M = 1,
             Na = 150,
             startH = 0,
             startM = 0,
             startS = 0,
             endH = 24,
             endM = 0,
             endS = 0,
             nb.pca = 0,
             wdir = FALSE,
             time_interval = 30,
             netCDF_resolution = 1800,
             interp = FALSE) {
        message("[INFO] RUNNING")
        start_time <- as.POSIXlt(Sys.time())
        message(paste("[CONFIG] Method =", method, sep = " "))

        ###############################################################################
        # Organize data
        ###############################################################################

        if (nb.pca >= 1) {
            message("[INFO] Loading and organizing the data (PCA)")
            setwd("~/")
            setwd("AnEnMDataR/R")
            source("loadDataPcaAnalysis.R", local = TRUE)
        } else {
            message("[INFO] Loading and organizing the data")
            setwd("~/")
            setwd("AnEnMDataR/R")
            source("LoadAndOrganizeData.R", local = TRUE)
        }

        weight.v.1 <- 1.0
        weight.v.2 <- 1.0
        weight.v.3 <- 1.0
        weight.st.1 <- 1.0
        weight.st.2 <- 1.0
        weight.st.3 <- 1.0

        if (nb.stations == 3) {
            weight.st.1 = 0.333
            weight.st.2 = 0.333
            weight.st.3 = 0.333
        }

        if (nb.var == 2) {
            weight.v.1 = 0.5
            weight.v.2 = 0.5
        }

        if (nb.var == 3) {
            weight.v.1 = 0.333
            weight.v.2 = 0.333
            weight.v.3 = 0.333
        }

        ###############################################################################
        #  MONACHE
        ###############################################################################

        if (method == "monache") {
            nb.centros <- FALSE
            nb.iteracoes <- FALSE
            if (nb.pca == 0) {
                if (independent) {
                    print("independente")
                    setwd("~/")
                    setwd("AnEnMDataR/R")
                    source("Monache_Current_Independent.R", local = TRUE)

                } else{
                    print("Dependente")
                    setwd("~/")
                    setwd("AnEnMDataR/R")
                    source("Monache.R", local = TRUE)
                }

            } else{

                if (independent) {
                    print("PCA Independent")
                    setwd("~/")
                    setwd("AnEnMDataR/R")
                    source("Monache_independent_PCA.R", local = TRUE)

                } else{
                    print("PCA Dependent")
                    setwd("~/")
                    setwd("AnEnMDataR/R")
                    source("Monache_PCA.R", local = TRUE)
                }
            }

            ncores <- detectCores()
            cluster <- makeCluster(ncores, type = "FORK")
            print(sprintf("Parallel parSapply loop using %d cores", ncores))

            prevAle <-
                array(data = NA,
                      dim = c(target$N - target$n0 + 1))

            prevAle <-
                parSapply(cluster, 1:length(Y.pred[, 1]), main)

            stopCluster(cluster)
        }

        ################################################################################################
        # Results
        ################################################################################################



        message("[INFO] Variable: ", v.aim)
        message("[RESULT] Data quantity predicted: ", length(prevTs))
        message("[RESULT] Data quantity to evaluate: ", length(target[[v.aim]][(target$stamp >= forecasting.t0)]))
        message("[RESULT] Start of Prediction: ", forecasting.t0)
        message("[RESULT] End of Prediction: ", tail(target$stamp, n = 1))
        message("[RESULT] Running time: ", end_time - start_time)


        if (nb.pca >= 1) {
            prevAle <- list.rbind(prevAle)
            f2 = (60 * 24 * 365.25) / 6

            prevTs = ts(data = prevAle, frequency = f2)
            end_time <- as.POSIXlt(Sys.time())

            measures <-
                matrix(
                    NA,
                    nrow = length(variables),
                    ncol = 2,
                    byrow = TRUE
                )
            for (i in seq_along(variables))            {

                if (variables[i] == "WDIR") {
                    prevWDIR <- abs((unlist(prevTs[, variables[i]]) * 180) / (pi))



                    prevWDIR <- 22.5 * (round(prevWDIR/ 22.5))


                    E <-
                        unlist(prevWDIR) - target[[variables[i]]][(target$stamp >= forecasting.t0)]
                    E <- abs(E - ceiling((E - 180) / 360) * 360)
                    E <- rad(E)
                    BIAS <-
                        ((1 / length(E)) * sum(E, na.rm = TRUE))
                    RMSE <- sqrt(mean((E) ^ 2,  na.rm = TRUE))
                    measures[i, ] <- c(BIAS, RMSE)
                } else{
                    f2 = (60 * 24 * 365.25) / 6
                    prevTs = ts(data = prevAle, frequency = f2)
                    end_time <- as.POSIXlt(Sys.time())
                    E <-
                        unlist(prevTs[, variables[i]]) - target[[variables[i]]][(target$stamp >= forecasting.t0)]

                    BIAS <- ((1 / length(E)) * sum(E, na.rm = TRUE))
                    RMSE <- sqrt(mean((E) ^ 2,  na.rm = TRUE))
                    measures[i, ] <- c(BIAS, RMSE)
                }
            }
            colnames(measures) <- c("BIAS", "RMSE")
            rownames(measures) <- variables
        } else{
            if (v.aim == 'WDIR') {
                prevTs <- abs((prevTs * 180) / (pi))
                E <-
                    prevTs - target[[v.aim]][(target$stamp >= forecasting.t0)]
                E <- abs(E - ceiling((E - 180) / 360) * 360)
                E <- rad(E)
                BIAS <- ((1 / length(E)) * sum(E, na.rm = TRUE))
                RMSE <- sqrt(mean((E) ^ 2,  na.rm = TRUE))

                message(paste(
                    "[RESULT]   Bias: ",
                    ((1 / length(E)) * sum(E, na.rm = TRUE)),
                    "   Sde: ",
                    sqrt((RMSE ** 2) - (BIAS ** 2)),
                    "   RMSE: ",
                    RMSE,
                    sep = ""
                ))
            } else {
                E <- prevAle - target[[v.aim]][(target$stamp >= forecasting.t0)]
                BIAS <- ((1 / length(E)) * sum(E, na.rm = TRUE))
                RMSE <- sqrt(mean((E) ^ 2,  na.rm = TRUE))

                message(paste(
                    "[RESULT]   Bias: ",
                    BIAS,
                    "   Sde: ",
                    sqrt((RMSE ** 2) - (BIAS ** 2)),
                    "   RMSE: ",
                    RMSE,
                    sep = ""
                ))
            }
        }
        print(measures)
        write.csv(measures, file = paste(independent,nb.pca, name.pred1, "nowdir", ".csv"))

        message("[INFO] Saving the results into Results folder")
        export_result <-
            c(
                BIAS,
                RMSE,
                paste(independent),
                v.aim,
                v.1,
                v.2,
                v.3,
                end_time - start_time,
                nb.pca,
                wdir,
                start_of_date,
                end_of_date,
                nb.stations,
                name.hist,
                name.pred1
            )
        df_export <- t(data.frame(export_result))
        colnames(df_export) <-
            c(
                "BIAS",
                "RMSE",
                "independent",
                "v.aim",
                "v.1",
                "v.2",
                "v.3",
                "running time (s)",
                "nb.pca",
                "wdir",
                "start_of_date",
                "end_of_date",
                "nb.stations",
                "name.hist",
                "name.pred1"
            )

        setwd("~/")
        setwd("AnEnMDataR/Results")
        write.csv(df_export,
                  file = paste(v.aim, name.hist, name.pred1, nb.pca, wdir, ".csv"))
        setwd("Data_predicted")
        write.table(prevTs, file = paste(v.aim, nb.pca, name.hist, name.pred1, ".txt"))

    }
