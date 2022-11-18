###############################################################################
# MONACHE PREDICTION FUNCTION
# STATIONS ARE CONSIDERED INDEPENDANT VARAIBLES
###############################################################################

main <- function(n) {
    y <- array(data = NA, dim = c(nb.series * (2 * M + 1)))
    y <- Y.pred[n, ]

    Y.local <- Y

    if (sum(is.na(y)) < .5 * (2 * M + 1) * nb.series) {        # guarantees window has 50% of valid values

        A <- (sweep(Y.local, 2, y, "-")) ** 2  # quad error
        if (v.1 == 'WDIR') {
            A <- abs(A - ceiling((A - 180) / 360) * 360)
        }

        Anan <- rowSums(is.na(A)) > .5 * (2 * M + 1) * nb.series

        A1 <-
            sqrt(rowSums(A[, 1:(2 * M + 1)], na.rm = TRUE))
        A2 <- 0.0
        A3 <- 0.0

        if (nb.stations >= 2) {
            index.debut <- 1
            A2 <-
                sqrt(rowSums(A[, (index.debut * (2 * M + 1) + 1):((index.debut + 1) * (2 *
                                                                                           M + 1))], na.rm = TRUE))
        }

        if (nb.stations >= 3) {
            index.debut <- 2
            A3 <-
                sqrt(rowSums(A[, (index.debut * (2 * M + 1) + 1):((index.debut + 1) * (2 *
                                                                                           M + 1))], na.rm = TRUE))
        }

        Metric <-
            (weight.st.1 * weight.v.1 / std.p1.v1) * A1 + (weight.v.1 * weight.st.2 /  std.p2.v1) * A2 + (weight.v.1 * weight.st.3 / std.p3.v1) * A3
        Metric[Ynan] <- Inf
        Metric[Anan] <- Inf

        na <- order(Metric, decreasing = FALSE)[1:Na]
        analogs.final = target[[v.aim]][na]

        if (v.aim == 'WDIR') {
            angles <- rad(analogs.final)[!is.na(analogs.final)]
            result <- circ.mean(angles)
            if(result < 0)
                result <- result + 2*pi
        } else {
            result = mean(analogs.final, na.rm = TRUE)
        }

    } else   {
        result <- NA
    }

    return(result)

}
