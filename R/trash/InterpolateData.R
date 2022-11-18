InterpolateData <-
  function(dates, entry_data, start_of_date, end_of_date, time_interval) {

    date_to_format <-
      as.POSIXct(dates, format = "%m/%d/%Y  %H:%M:%S", tz = 'UTC')

    entry_data <- as.numeric(entry_data)
    dim(date_to_format) <- NULL

    dataframe_interpolation <-
      data.frame(date_to_format, entry_data)
    dataframe_interpolation <- na.omit(dataframe_interpolation)

    interpolated_df <-
      data.frame(date_to_format = seq(
        as.POSIXct(start_of_date, tz = "UTC"),
        as.POSIXct(end_of_date, tz = "UTC") ,
        paste(time_interval, "min")
      )) %>%
      left_join(dataframe_interpolation, by = "date_to_format")

    interpolated_df <-
      interpolated_df[!duplicated(interpolated_df$date_to_format), ]

    interpolated_df[,3] <- na.approx(interpolated_df[,"entry_data"], maxgap = 4, rule = 2)

    return(interpolated_df)

  }
