###############################################################################
# MONACHE PREDICTION FUNCTION
# STATIONS ARE CONSIDERED INDEPENDANT VARAIBLES
###############################################################################

rm(Y.n, Y.pred.n)
# n<- 1
main <- function(n) {
    y1 <- array(data = NA, dim = c((2 * M + 1)))
    y1 <- Y.pred[n, 1:(2 * M + 1)]

    Y1.local <- Y[, 1:(2 * M + 1)]

    if (sum(is.na(y1)) < .5 * (2 * M + 1)) {
        A11 <- (sweep(Y1.local, 2, y1, "-")) ** 2  # quad error
        if (v.1 == 'WDIR') {
            A11 <- abs(A11 - ceiling((A11 - 180) / 360) * 360)
        }

        Anan1 <- rowSums(is.na(A11)) > .5 * (2 * M + 1)
        A11 <-
            sqrt(rowSums(A11[, 1:(2 * M + 1)], na.rm = TRUE))

        Metric1 <- A11
        Metric1[Ynan] <- Inf
        Metric1[Anan1] <- Inf

        na1 <-
            order(Metric1, decreasing = FALSE)[1:Na]# + M cf correlation.R
        analogs.final1 <- list()
        for (j in seq_along(variables)) {
            analogs.final1[[variables[j]]] <- target[[variables[j]]][na1]
        }
        variables_noWDIR <- variables[variables != "WDIR"]
        result1 <-
            lapply(analogs.final1[variables_noWDIR], mean, na.rm = TRUE)

        if ('WDIR' %in% variables) {
            angles1 <-
                analogs.final1[["WDIR"]][!is.na(analogs.final1[["WDIR"]])]
            result1[["WDIR"]] <- circ.mean(angles1)
            if (result1[["WDIR"]] < 0)
                result1[["WDIR"]] <- 2 * pi + result1[["WDIR"]]
        }

    } else {
        result1 <- NA
    }
    ################################################################# STATION 2  #################################################################

    y2 <- array(data = NA, dim = c((2 * M + 1)))
    y2 <- Y.pred[n, ((2 * M + 1) + 1):((2 * M + 1) * 2)]

    Y2.local <- Y[, ((2 * M + 1) + 1):((2 * M + 1) * 2)]

    if (sum(is.na(y2)) < .5 * (2 * M + 1)) {
        # guarantees window has 50% of valid values

        A12 <- (sweep(Y2.local, 2, y2, "-")) ** 2  # quad error
        if (v.2 == 'WDIR') {
            A12 <- abs(A12 - ceiling((A12 - 180) / 360) * 360)
        }

        Anan2 <- rowSums(is.na(A12)) > .5 * (2 * M + 1)
        A12 <-
            sqrt(rowSums(A12[, 1:(2 * M + 1)], na.rm = TRUE))

        Metric2 <- A12
        Metric2[Ynan] <- Inf
        Metric2[Anan2] <- Inf

        na2 <-
            order(Metric2, decreasing = FALSE)[1:Na]# + M cf correlation.R
        analogs.final2 <- list()
        for (j in seq_along(variables)) {
            analogs.final2[[variables[j]]] <- target[[variables[j]]][na2]
        }
        result2 <-
            lapply(analogs.final2[variables_noWDIR], mean, na.rm = TRUE)

        if ('WDIR' %in% variables) {
            angles2 <-
                analogs.final2[["WDIR"]][!is.na(analogs.final2[["WDIR"]])]
            result2[["WDIR"]] <- circ.mean(angles2)
            if (result2[["WDIR"]] < 0)
                result2[["WDIR"]] <- 2 * pi + result2[["WDIR"]]
        }
    } else {
        result2 <- NA
    }
    ################################################################# STATION 3 #################################################################
    if (nb.pca > 2) {
        y3 <- array(data = NA, dim = c((2 * M + 1)))
        y3 <- Y.pred[n, (2 * (2 * M + 1) + 1):((2 * M + 1) * 3)]

        Y3.local <- Y[, (2 * (2 * M + 1) + 1):((2 * M + 1) * 3)]

        if (sum(is.na(y3)) < .5 * (2 * M + 1)) {
            # guarantees window has 50% of valid values

            A13 <- (sweep(Y3.local, 2, y3, "-")) ** 2  # quad error
            if (v.3 == 'WDIR') {
                A13 <- abs(A13 - ceiling((A13 - 180) / 360) * 360)
            }

            Anan3 <- rowSums(is.na(A13)) > .5 * (2 * M + 1)
            A13 <-
                sqrt(rowSums(A13[, 1:(2 * M + 1)], na.rm = TRUE))

            Metric3 <- A13
            Metric3[Ynan] <- Inf
            Metric3[Anan3] <- Inf

            na3 <-
                order(Metric3, decreasing = FALSE)[1:Na]# + M cf correlation.R

            analogs.final3 <- list()
            for (j in seq_along(variables)) {
                analogs.final3[[variables[j]]] <- target[[variables[j]]][na3]
            }
            result3 <-
                lapply(analogs.final3[variables_noWDIR], mean, na.rm = TRUE)

            if ('WDIR' %in% variables) {
                angles3 <-
                    analogs.final3[["WDIR"]][!is.na(analogs.final3[["WDIR"]])]
                result3[["WDIR"]] <- circ.mean(angles3)
                if (result3[["WDIR"]] < 0)
                    result3[["WDIR"]] <- 2 * pi + result3[["WDIR"]]
            }
        } else{
            result3 <- NA
        }
    }else{
        result3 <- NA
    }
    result.combined <-
        mapply(c, result1, result2, result3, SIMPLIFY = FALSE)
    if (!all(is.na(unlist(result.combined)))) {

                result.final <-
            mapply(mean,
                   result.combined[variables_noWDIR],
                   SIMPLIFY = FALSE,
                   na.rm = TRUE)

        if ('WDIR' %in% variables) {
            result.combined[["WDIR"]] <-
                result.combined[["WDIR"]][!is.na(result.combined[["WDIR"]])]
            result.final[["WDIR"]] <-
                circ.mean(result.combined[["WDIR"]])
            if (result.final[["WDIR"]] < 0)
                result.final[["WDIR"]] <-
                2 * pi + result.final[["WDIR"]]
        }
    } else{

            result.final <- NA

    }
    return(result.final)

}
