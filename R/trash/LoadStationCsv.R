LoadStationCsv <-
    function(n) {
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

        datast$stamp <-
            as.POSIXct(date,
                       origin = datast$epoch,
                       tz = "UTC") # Changing resolution

        datast$N <- length(datast$stamp)
        datast$t <- as.numeric(datast$stamp)

        if (variables %in% header) {
            for (i in seq_len(length(variables))) {
                # Loading each input variable
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

            # 1.1 - Interpolation between start_of_date and end_of_date

            if (interp) {
                for (i in seq_len(length(variables))) {
                    datast.interp <-
                        InterpolateData(datast$stamp,
                                        datast[[variables[i]]],
                                        start_of_date,
                                        end_of_date,
                                        time_interval)
                    datast[[variables[i]]] <- datast.interp[, 3]

                }
                # 1.12 - Transforming to 22.5 degree resolution, 16 different cardinales (Ex: NNE, NNW)
                if ('WDIR' %in% variables) {
                    datast$WDIR <- 22.5 * (round(datast$WDIR / 22.5))
                }
                datast$stamp <- datast.interp[, 1]
            }

            # 1.2 - Setting the intersection between forecasting.t0 and the datast$stamp
            datast$t0 <-
                (as.numeric(as.POSIXct(forecasting.t0, tz = "UTC")) - as.numeric(datast$epoch)) / 60 / 30
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
