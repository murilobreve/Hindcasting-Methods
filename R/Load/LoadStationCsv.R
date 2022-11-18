LoadStationCsv <-
    function(n, station_files, files, params_list) {

        # Assigning parameters -----------------------------------
        start_of_date <-  params_list$start_of_date
        end_of_date <- params_list$end_of_date
        forecasting_t0 <- params_list$prediction_t0
        start_H <-  params_list$start_H
        end_H <-  params_list$end_H
        netCDF_resolution <- params_list$netCDF_resolution
        interp <- params_list$interp
        time_interval <- params_list$time_interval

        # Load input station files -----------------------------------
        f <- files[unlist(station_files[n])]
        header <- scan(f[1], nlines = 1, what = character())
        DFs <-
            lapply(f,
                   fread,
                   sep = " ",
                   skip = 2,
                   header = FALSE)
        ds <- do.call(rbind, DFs)
        ds <- as.data.frame(ds)

        names(ds) <- header
        datast <- list()

        date <-
            paste(paste(ds[, 1], ds$MM, ds$DD, sep = "-"),
                  paste(ds$hh, ds$mm, "0", sep = ":"),
                  sep = " ")
        datast$epoch <- as.POSIXct('1970-01-01',  tz = "UTC")
        datast$stamp <-
            as.POSIXct(date,
                       origin = datast$epoch,
                       tz = "UTC")
        datast$N <- length(datast$stamp)
        datast$t <- as.numeric(datast$stamp)

        # Organizing and cleaning the inside variables -----------------------------------
        if (variables %in% header) {
            for (i in seq_len(length(variables))) {
                datast[[variables[i]]] <-
                    ds[, variables[i]]

                if (variables[i] %in% c("WSPD",
                                        "GST",
                                        "WVHT",
                                        "DPD",
                                        "APD",
                                        "WTMP",
                                        "VIS",
                                        "TIDE"))
                    datast[[variables[i]]][datast[[variables[i]]] == 99] <-
                        NA
                if (variables[i] %in% c("WDIR", "MWD", "ATMP", "DEWP"))
                    datast[[variables[i]]][datast[[variables[i]]] == 999] <-
                        NA
                if (variables[i] %in% c("PRES"))
                    datast[[variables[i]]][datast[[variables[i]]] == 9999] <-
                        NA
            }

            # Data interpolation and selection of the daily period (start_H and end_H) -----------------------------------
            if (interp) {
                for (i in seq_len(length(variables))) {
                    datast.interp <-
                        InterpolateData(params_list,
                                        datast$stamp,
                                        datast[[variables[i]]])
                    datast[[variables[i]]] <- datast.interp[, 3]
                }
                datast$stamp <- datast.interp[, 1]
            }

            # Defining the prediction and the training period-----------------------------------
            datast$t0 <-
                (as.numeric(as.POSIXct(prediction_t0, tz = "UTC")) - as.numeric(datast$epoch)) / netCDF_resolution
            datast$t0
            datast$N <- length(datast$stamp)
            datast$n0 <-
                match(forecasting.t0, datast$stamp)
            datast$n0
            datast$t <- as.numeric(as.POSIXct(datast$stamp))

            return(datast)
        }
        return("no_variables")
        message("[INFO] The chosen variables are not in the dataset")
    }
