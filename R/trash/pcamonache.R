###############################################################################
# MONACHE PREDICTION FUNCTION
# STATIONS ARE CONSIDERED INDEPENDANT VARAIBLES
###############################################################################


rm(Y.n, Y.pred.n)

main <- function(n) {
    y <- array(data = NA, dim = c(nb.series * (2 * M + 1)))
    y <- Y.pred[n, ]

    Y.local <- Y

    if (sum(is.na(y)) < .5 * (2 * M + 1) * nb.series) {
        # guarantees window has 50% of valid values

        A <- (sweep(Y.local, 2, y, "-")) ** 2  # quad error
        Anan <- rowSums(is.na(A)) > .5 * (2 * M + 1) * nb.series

        A1 <-
            sqrt(rowSums(A[, 1:(2 * M + 1)], na.rm = TRUE))   # Calcula a raiz quadrada do valor flutuante especificado.
        A2 <- 0.0
        A3 <- 0.0

        if (nb.stations >= 2) {
            index.debut = nb.var
            A2 <-
                sqrt(rowSums(A[, (index.debut * (2 * M + 1) + 1):((index.debut + 1) * (2 *
                                                                                           M + 1))], na.rm = TRUE))
        }

        if (nb.stations >= 3) {
            index.debut = 2 * nb.var
            A3 <-
                sqrt(rowSums(A[, (index.debut * (2 * M + 1) + 1):((index.debut + 1) * (2 *
                                                                                           M + 1))], na.rm = TRUE))
        }

        Metric <-
             A1 + A2 +  A3
        Metric[Ynan] <- Inf
        Metric[Anan] <- Inf

        na <- order(Metric, decreasing = FALSE)[1:Na]

        analogs.final <- list()
        for (j in seq_along(variables)) {
            analogs.final[[variables[j]]] <- target[[variables[j]]][na]
        }
        variables_noWDIR <- variables[variables != "WDIR"]
        result <- lapply(analogs.final[variables_noWDIR], mean, na.rm = TRUE)

        if ('WDIR' %in% variables) {
            angles <- analogs.final[["WDIR"]][!is.na(analogs.final[["WDIR"]])]
            result[["WDIR"]] <- circ.mean(angles)
            if (result[["WDIR"]] < 0)
                result[["WDIR"]] <- 2*pi + result[["WDIR"]]
        }

    } else   {
        #result <- list()
        #result[variables] <- NA
        result <- NA
    }

    return(result)

}
