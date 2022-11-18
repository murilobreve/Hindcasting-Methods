#'@title Performs the hindcasting
#'@name WeatherDataPrediction
#'
#'@description The Analog Ensembles method (AnEn), introduced by Luca Delle Monache
#'is a post-processing tool that has shown good results to improve
#'weather predictions or perform hindcasting (reconstruction of missing meteorological data).
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
#'WeatherDataPrediction <-
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
WeatherDataPrediction <-
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
        ###############################################################################
        # 0- Starting
        ###############################################################################

        message("[INFO] RUNNING")
        start_time <- as.POSIXlt(Sys.time())
        message(paste("[CONFIG] Method =", method, sep = " "))

        ###############################################################################
        # 1- Organize data
        ###############################################################################

        if (nb.pca >= 1) {
            message("[INFO] Loading and organizing the data (PCA)")
            setwd("~/")
            setwd("AnEnMDataR/R")
            source("LoadDataPcaAnalysis.R", local = TRUE)
        } else {
            message("[INFO] Loading and organizing the data")
            setwd("~/")
            setwd("AnEnMDataR/R")
            source("LoadAndOrganizeData.R", local = TRUE)
        }

        ###############################################################################
        # 2- Weights
        ###############################################################################

        weight.v.1 <- 1.0
        weight.v.2 <- 1.0
        weight.v.3 <- 1.0
        weight.st.1 <- 1.0
        weight.st.2 <- 1.0
        weight.st.3 <- 1.0

        ###############################################################################
        # 3- Prediction
        ###############################################################################

        start_time <- as.POSIXlt(Sys.time())

        setwd("~/")
        setwd("AnEnMDataR/R")
        source("Load.R", local = TRUE)

        if (method == "monache") {
            nb.centros <- FALSE
            nb.iteracoes <- FALSE
            if (nb.pca == 0) {
                if (independent) {
                    print("independente")
                    setwd("~/")
                    setwd("AnEnMDataR/R")
                    source("Monache_Current_Independent2.R", local = TRUE)

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
            cluster <-
                makeCluster(ncores - 1, type = "FORK") # giving 1 core to the system
            print(sprintf("Parallel parSapply loop using %d cores", ncores))
            prevAle <-
                array(data = NA,
                      dim = c(target$N - target$n0))

            prevAle[1:(pred1$N - pred1$n0 + 1)] <-
                parSapply(cluster, 1:length(Y.pred[, 1]), main)

            stopCluster(cluster)
        }

        end_time <- as.POSIXlt(Sys.time())
        end_time - start_time

        E <- c(list.rbind(prevAle)) - target[[v.aim]][(target$stamp >= forecasting.t0)]

        BIAS <- ((1 / length(E)) * sum(E, na.rm = TRUE))

        RMSE <- sqrt(mean((E) ^ 2,  na.rm = TRUE))
        SDE <- sqrt((RMSE ** 2) - (BIAS ** 2))

        message(paste(
            "[RESULT] Bias: ",
            BIAS,
            "RMSE: ",
            RMSE,
            "SDE: ",
            SDE,
            sep = "   "
        ))





        ################################################################################################
        # 4- Results
        ################################################################################################

        # 4.1- PCA Results - the output is a table with all variables (input) errors measures

        if (nb.pca >= 1) {
            prevPCA <- list.rbind(prevAle)
            measures <-
                matrix(
                    NA,
                    nrow = length(variables),
                    ncol = 3,
                    byrow = TRUE
                )

            for (i in seq_along(variables)) {
                # Calculating the errors of every input variable

                if (variables[i] == "WDIR") {
                    prevPCA[, variables[i]] <-
                        abs((unlist(prevPCA[, variables[i]]) * 180) / (pi)) # Rad to angle transformation

                    E <-
                        unlist(prevPCA[, variables[i]]) - target[[variables[i]]][(target$stamp >= forecasting.t0)]
                    E <-
                        abs(E - ceiling((E - 180) / 360) * 360) # WDIR - output comparision with real values (angle subtraction formula)
                    E <- rad(E)

                    BIAS <- ((1 / length(E)) * sum(E, na.rm = TRUE))
                    RMSE <- sqrt(mean((E) ^ 2,  na.rm = TRUE))
                    SDE <- sqrt((RMSE ** 2) - (BIAS ** 2))

                    measures[i,] <- c(BIAS, RMSE, SDE)

                } else{
                    E <-
                        unlist(prevPCA[, variables[i]]) - target[[variables[i]]][(target$stamp >= forecasting.t0)]

                    BIAS <- ((1 / length(E)) * sum(E, na.rm = TRUE))
                    RMSE <- sqrt(mean((E) ^ 2,  na.rm = TRUE))
                    SDE <- sqrt((RMSE ** 2) - (BIAS ** 2))

                    measures[i, ] <- c(BIAS, RMSE, SDE)
                }
            }
            colnames(measures) <- c("BIAS", "RMSE", "SDE")
            rownames(measures) <- variables
            measures <- round(measures, 3)
            print(measures)
        }

        # 4.2- Standart AnEn Results (only 1 variable is predicted)

        if (nb.pca == 0) {
            if (v.aim == 'WDIR') {
                prevAle <- abs((prevAle * 180) / (pi)) # Rad to angle transformation

                E <-
                    prevAle - target[[v.aim]][(target$stamp >= forecasting.t0)]
                E <-
                    abs(E - ceiling((E - 180) / 360) * 360) # WDIR - output comparision with real values (angle subtraction formula)
                E <- rad(E)

                BIAS <- ((1 / length(E)) * sum(E, na.rm = TRUE))
                RMSE <- sqrt(mean((E) ^ 2,  na.rm = TRUE))
                SDE <- sqrt((RMSE ** 2) - (BIAS ** 2))

                message(paste(
                    "[RESULT] Bias: ",
                    BIAS,
                    "RMSE: ",
                    RMSE,
                    "SDE: ",
                    SDE,
                    sep = "   "
                ))

            } else {
                E <- prevAle - target[[v.aim]][(target$stamp >= forecasting.t0)]

                BIAS <- ((1 / length(E)) * sum(E, na.rm = TRUE))
                RMSE <- sqrt(mean((E) ^ 2,  na.rm = TRUE))
                SDE <- sqrt((RMSE ** 2) - (BIAS ** 2))

                message(paste(
                    "[RESULT] Bias: ",
                    BIAS,
                    "RMSE: ",
                    RMSE,
                    "SDE: ",
                    SDE,
                    sep = "   "
                ))
            }
        }

        end_time <- as.POSIXlt(Sys.time())
        message("[INFO] Variable: ", v.aim)
        message("[RESULT] Data quantity predicted: ", length(prevTs))
        message("[RESULT] Data quantity to evaluate: ", length(target[[v.aim]][(target$stamp >= forecasting.t0)]))
        message("[RESULT] Start of Prediction: ", forecasting.t0)
        message("[RESULT] End of Prediction: ", tail(target$stamp, n = 1))
        message("[RESULT] Running time: ", end_time - start_time)

        write.csv(measures,
                  file = paste(independent, nb.pca, name.pred1, "nowdir", ".csv"))

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
