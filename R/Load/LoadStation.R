LoadStation <-
    function(n, station_files, files, params_list) {

        # Assigning parameters -----------------------------------
        start_of_date <-  params_list$start_of_date
        end_of_date <- params_list$end_of_date
        pred_t0 <- params_list$prediction_t0
        start_H <-  params_list$start_H
        end_H <-  params_list$end_H
        netCDF_resolution <- params_list$netCDF_resolution
        interp <- params_list$interp
        time_interval <- params_list$time_interval
        
        ds <- nc_open(files[n])
        datast <- list()
        datast$epoch <- as.POSIXct('1970-01-01',  tz = "UTC")
        datast$t <- ncvar_get(ds, "time")
        datast$N <- length(datast$t)
        datast$stamp <-
            as.POSIXct(datast$t * netCDF_resolution,
                       origin = datast$epoch,
                       tz = "UTC")

        for (i in seq_len(length(variables))) {
            # Loading each input variable
            datast[[variables[i]]] <- ncvar_get(ds, variables[i])
            if (ncatt_get(ds, variables[i], "_FillValue")$hasatt) {
                datast[[variables[i]]][datast[[variables[i]]] == ncatt_get(ds, variables[i], "_FillValue")$value] <-
                    NA
            } else {
                datast[[variables[i]]][datast[[variables[i]]] > 9e+36] <-
                    NA  # missing value
            }
        }

        nc_close(ds)
        # 1.1 - Interpolation between start_of_date and end_of_date

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
        

        # 1.2 - Setting the intersection between forecasting.t0 and the datast$stamp
        datast$t0 <-
            (as.numeric(as.POSIXct(pred_t0, tz = "UTC")) - as.numeric(datast$epoch)) / 60 / 30
        datast$t0
        datast$N <- length(datast$stamp)
        datast$n0 <-
            match(pred_t0, datast$stamp)
        datast$n0
        datast$t <- as.numeric(as.POSIXct(datast$stamp))

        return(datast)

    }
