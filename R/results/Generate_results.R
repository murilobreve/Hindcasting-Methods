if (method == "monache") {
    if (do.pca == TRUE) {
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
            if (length(variables) > 1) {
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

                    measures[i, ] <- c(BIAS, RMSE, SDE)

                } else{
                    E <-
                        unlist(prevPCA[, variables[i]]) - target[[variables[i]]][(target$stamp >= forecasting.t0)]

                    BIAS <- ((1 / length(E)) * sum(E, na.rm = TRUE))
                    RMSE <- sqrt(mean((E) ^ 2,  na.rm = TRUE))

SDE <- sqrt((RMSE ** 2) - (BIAS ** 2))
                    measures[i,] <- c(BIAS, RMSE, SDE)
                }
            }
        }

        if (length(variables) == 1) {
            E <- c(list.rbind(prevAle)) - target[[variables]][(target$stamp >= forecasting.t0)]

            BIAS <- ((1 / length(E)) * sum(E, na.rm = TRUE))

            RMSE <- sqrt(mean((E) ^ 2,  na.rm = TRUE))
            SDE <- sqrt((RMSE ** 2) - (BIAS ** 2))

            measures[1,] <- c(BIAS, RMSE, SDE)

        }

        colnames(measures) <- c("BIAS", "RMSE", "SDE")
        rownames(measures) <- variables
        measures <- round(measures, 3)
        print(measures)
    }
} else{
    E <- prevAle - target[[v.aim]][(target$stamp >= forecasting.t0)]

    BIAS <- ((1 / length(E)) * sum(E, na.rm = TRUE))
    RMSE <- sqrt(mean((E) ^ 2,  na.rm = TRUE))
    SDE <- sqrt((RMSE ** 2) - (BIAS ** 2))

    message(paste("[RESULT] Bias: ",
                  BIAS,
                  "RMSE: ",
                  RMSE,
                  "SDE: ",
                  SDE,
                  sep = "   "))
}


# 4.2- Standart AnEn Results (only 1 variable is predicted)

if (do.pca == FALSE) {
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

        message(paste("[RESULT] Bias: ",
                      BIAS,
                      "RMSE: ",
                      RMSE,
                      "SDE: ",
                      SDE,
                      sep = "   "))

    } else {
        E <- prevAle - target[[v.aim]][(target$stamp >= forecasting.t0)]

        BIAS <- ((1 / length(E)) * sum(E, na.rm = TRUE))
        RMSE <- sqrt(mean((E) ^ 2,  na.rm = TRUE))
        SDE <- sqrt((RMSE ** 2) - (BIAS ** 2))

        message(paste("[RESULT] Bias: ",
                      BIAS,
                      "RMSE: ",
                      RMSE,
                      "SDE: ",
                      SDE,
                      sep = "   "))
    }
}
