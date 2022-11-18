predicted <- prevAle
real <- observ_data_prediction

PlotResult <-
    function(predicted,
             real,
             variables_predict,
             stamp) {
        if (length(predicted) != length(real)) {
            message(
                "[ERROR-PlotResult] Could not plot the values because the vectors have different sizes"
            )
            break
        }
        
        data.month <- lubridate::month(stamp)
        
        data. <- cbind(predicted, real, data.month)
        nb.month <- unique(data.month)
        
        RMSE <- c()
        for (i in nb.month) {
            d.m <- data.[data.[, 3] == i, ]
            E <- d.m[, 2] - d.m[, 1]
            RMSE[i] <- sqrt(mean((E) ^ 2,  na.rm = TRUE))
            BIAS[i] <-
                ((1 / length(E)) * sum(E, na.rm = TRUE))
            SDE[i] <- sqrt((RMSE[i] ** 2) - (BIAS ** 2))
        }
        plot(RMSE,type = "l",
             col = "black",
             lwd = 2,
             xlab = "Month")
        lines(BIAS, col = "red", lwd = 2, lty = 3)
        lines(SDE, col = "blue", lwd = 2, lty = 4)
        
        plot(
            real,
            type = "l",
            col = "green",
            lwd = 5,
            xlab = "time"
        )
        lines(predicted, col = "red", lwd = 2)
        lines(RMSE, col = "red", lwd = 2)
        title(paste(variable.predict, "Prediction"))
        legend(
            x = "top",
            col = c("green", "red"),
            lty = 1,
            lwd = 5,
            legend = c('Real', 'Predicted')
        )
        predicted.values.pls <- predicted.values
        predicted.values.plsan <- predicted.values
    }