InterpolateData <-
  function(params_list, dates, entry_data) {
    start_of_date <-  params_list$start_of_date
    end_of_date <- params_list$end_of_date
    prediction_t0 <- params_list$prediction_t0
    start_H <-  params_list$start_H
    end_H <-  params_list$end_H
    netCDF_resolution <- params_list$netCDF_resolution
    interp <- params_list$interp
    time_interval <- params_list$time_interval

    date.to.format <-
      as.POSIXct(dates, format = "%m/%d/%Y  %H:%M:%S", tz = 'UTC')

    entry_data <- as.numeric(entry_data)
    dim(date.to.format) <- NULL

    dataframe.interpolation <-
      data.frame(date.to.format, entry_data)
    dataframe.interpolation <- na.omit(dataframe.interpolation)

    interpolated.df <-
      data.frame(date.to.format = seq(
        as.POSIXct(start_of_date, tz = "UTC"),
        as.POSIXct(end_of_date, tz = "UTC") ,
        paste(time_interval, "min")
      )) %>%
      dplyr::left_join(dataframe.interpolation, by = "date.to.format")

    interpolated.df <-
      interpolated.df[!duplicated(interpolated.df$date.to.format), ]

    interpolated.df[, 3] <-
      na.approx(interpolated.df[, "entry_data"], maxgap = 4, rule = 2)

    valid.hours <-
      (format(strptime(interpolated.df[, 1], "%Y-%m-%d %H:%M:%S"), '%H:%M:%S') >=  format(
        paste(start_H, ":00:00", sep = ""),
        format = "%H:%M:%S",
        tz = "UTC"
      )) &
      (format(strptime(interpolated.df[, 1], "%Y-%m-%d %H:%M:%S"), '%H:%M:%S') <=  format(
        paste(end_H, ":00:00", sep = ""),
        format = "%H:%M:%S",
        tz = "UTC"
      ))

    interpolated.df <- interpolated.df[valid.hours,]

    return(interpolated.df)

  }
