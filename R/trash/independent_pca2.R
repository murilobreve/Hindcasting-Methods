###############################################################################
# MONACHE PREDICTION FUNCTION
# STATIONS ARE CONSIDERED INDEPENDANT VARAIBLES
###############################################################################



main <- function(n) {
    y1 <- array(data = NA, dim = c((2 * M + 1)))
    y1 <- Y.pred[n, 1:(2 * M + 1)]

    Y1.local <- Y[, 1:(2 * M + 1)]

    if (sum(is.na(y1)) < .5 * (2 * M + 1)) {
        A11 <- (sweep(Y1.local, 2, y1, "-")) ** 2  # quad error

        Anan1 <- rowSums(is.na(A11)) > .5 * (2 * M + 1)
        A11 <-
            sqrt(rowSums(A11[, 1:(2 * M + 1)], na.rm = TRUE))

        Metric1 <- A11
        Metric1[Ynan] <- Inf
        Metric1[Anan1] <- Inf
        na1 <- 1

    }else{
        na1 <- NA

    }
    ################################################################# STATION 2  #################################################################
    if (nb.pca >= 2) {
        y2 <- array(data = NA, dim = c((2 * M + 1)))
        y2 <- Y.pred[n, ((2 * M + 1) + 1):((2 * M + 1) * 2)]

        Y2.local <- Y[, ((2 * M + 1) + 1):((2 * M + 1) * 2)]

        if (sum(is.na(y2)) < .5 * (2 * M + 1)) {
            # guarantees window has 50% of valid values

            A12 <- (sweep(Y2.local, 2, y2, "-")) ** 2  # quad error

            Anan2 <- rowSums(is.na(A12)) > .5 * (2 * M + 1)
            A12 <-
                sqrt(rowSums(A12[, 1:(2 * M + 1)], na.rm = TRUE))

            Metric2 <- A12
            Metric2[Ynan] <- Inf
            Metric2[Anan2] <- Inf
            na2 <- 1
        } else {
            na2 <- NA
        }
    }else{
        na2 <- NA
    }

    if (length(na.omit(na1, na2)) != 0) {
        MetricT <- c(Metric1,Metric2)

        naT <- order(MetricT, decreasing = FALSE)[1:Na]

        analogs.final <- list()


        for (j in seq_along(variables)) {

            duplicated_target <-
                c(target[[variables[j]]], target[[variables[j]]])
            analogs.final[[variables[j]]] <-
                duplicated_target[naT]
        }

        result.final <-
            lapply(analogs.final[variables], mean, na.rm = TRUE)

    }  else{
        result.final <- NA
    }
    return(result.final)

}

