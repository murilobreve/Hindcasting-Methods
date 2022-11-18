checkResults <- function(prevData, target, forecasting.t0) {
    prevPCA <- list.rbind(prevData)
    measures <-
        matrix(NA,
               nrow = length(variables),
               ncol = 3,
               byrow = TRUE)
    #fit
    prediction <- (target$stamp >= forecasting_t0)
    training <- !prediction

    for (i in seq_along(variables)) {
        if (length(variables) > 1) {
            E <-
                unlist(prevPCA[, variables[i]]) - target[[variables[i]]][prediction]

            BIAS <-
                ((1 / length(E)) * sum(E, na.rm = TRUE))
            RMSE <- sqrt(mean((E) ^ 2,  na.rm = TRUE))
            SDE <- sqrt((RMSE ** 2) - (BIAS ** 2))

            measures[i,] <- c(BIAS, RMSE, SDE)
        }

        E <-
            c(list.rbind(prevPCA)) - target[[variables]][!training_time]

        if (length(variables) == 1) {
            E <-
                c(list.rbind(prevPCA)) - target[[variables]][prediction]

            BIAS <- ((1 / length(E)) * sum(E, na.rm = TRUE))
            RMSE <- sqrt(mean((E) ^ 2,  na.rm = TRUE))
            SDE <- sqrt((RMSE ** 2) - (BIAS ** 2))

            measures[1,] <- c(BIAS, RMSE, SDE)
        }
    }
    colnames(measures) <- c("BIAS", "RMSE", "SDE")
    rownames(measures) <- variables
    measures <- round(measures, 3)

    return(measures)

}
