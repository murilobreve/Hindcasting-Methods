###############################################################################
# MONACHE PREDICTION FUNCTION
###############################################################################

MonachePCA <- function(n) {
    y <- array(data = NA, dim = c(nb.series * (2 * M + 1)))
    y <- Y_pred[n,]
    Y.local <- Y

    if (sum(is.na(y)) < .5 * (2 * M + 1) * nb.series) {
        # guarantees window has 50% of valid values

        A <- Rfast::eachrow(Y.local, y, "-") ** 2
        # comparison with all analogues

        Anan <-
            Cpp_rowSums(is.na(A)) > .5 * (2 * M + 1) * nb.series
        #sum of the missing values per row ( < 0.5)

        A1 <- sqrt(Cpp_rowSums(A[, 1:(2 * M + 1)]))
        #square root of the sum of the analogues' differences
        A2 <- 0.0
        A3 <- 0.0

        if (nb.pca >= 2) {
            index.debut = 1
            A2 <-
                sqrt(Cpp_rowSums(A[, (index.debut * (2 * M + 1) + 1):((index.debut + 1) * (2 *
                                                                                                M + 1))]))
        }
        if (nb.pca >= 3) {
            index.debut = 2
            A3 <-
                sqrt(Cpp_rowSums(A[, (index.debut * (2 * M + 1) + 1):((index.debut + 1) * (2 *
                                                                                                M + 1))]))
        }
        Metric <-            A1 +  A2 + A3
        na <- order(Metric, decreasing = FALSE)[1:Na]
        analogs.final <- list()

        for (j in seq_along(variables)) {
            analogs.final[[variables[j]]] <- target[[variables[j]]][na]
        }
        result <- lapply(analogs.final, mean, na.rm = TRUE)
    }
    else   {
        result <- NA
    }
    return(result)
}
