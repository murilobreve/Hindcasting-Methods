###############################################################################
# MONACHE PREDICTION FUNCTION
###############################################################################


y <- array(data = NA, dim = c(nb.series * (2 * M + 1)))
y <- Y.pred[n, ]
Y.local <- Y

# guarantees window has 50% of valid values
if (sum(is.na(y)) < .5 * (2 * M + 1) * nb.series) {
    ##new function of comparison with all analogues

    #NEW
    A <- Rfast::eachrow(Y.local, y, "-") ** 2

    #OLD
    A.old <- (sweep(Y.local, 2, y, "-")) ** 2

    if (!all.equal(A, A.old))
        break

    ##sum of the missing values per row ( < 0.5)

    #NEW
    Anan <-
        Cpp_rowSums(is.na(A)) > .5 * (2 * M + 1) * nb.series

    #OLD
    Anan.old <-
        rowSums(is.na(A)) > .5 * (2 * M + 1) * nb.series

    if (!all.equal(Anan, Anan.old))
        break

    ##square root of the sum of the analogues' differences

    #NEW
    A1 <- sqrt(Cpp_rowSums(A[, 1:(2 * M + 1)]))

    #OLD
    A1.old <- sqrt(rowSums(A[, 1:(2 * M + 1)]))

    if (!all.equal(A1, A1.old))
        break

    A2 <- 0.0
    A3 <- 0.0

    if (nb.pca >= 2) {
        index.debut = 1

        #NEW
        A2 <-
            sqrt(Cpp_rowSums(A[, (index.debut * (2 * M + 1) + 1):((index.debut + 1) * (2 * M + 1))]))

        #OLD
        A2.old <-
            sqrt(rowSums(A[, (index.debut * (2 * M + 1) + 1):((index.debut + 1) * (2 * M + 1))]))

        if (!all.equal(A2, A2.old))
            break


    }
    if (nb.pca >= 3) {
        index.debut = 2

        #NEW
        A3 <-
            sqrt(Cpp_rowSums(A[, (index.debut * (2 * M + 1) + 1):((index.debut + 1) * (2 *
                                                                                           M + 1))]))
        #OLD
        A3.old <-
            sqrt(rowSums(A[, (index.debut * (2 * M + 1) + 1):((index.debut + 1) * (2 *
                                                                                       M + 1))]))

        if (!all.equal(A3, A3.old))
            break

    }

    Metric <-
        (weight.st.1 * weight.v.1 / std.p1.v1) * A1 + (weight.v.1 * weight.st.2 /  std.p2.v1) * A2 + (weight.v.1 * weight.st.3 / std.p3.v1) * A3
    Metric[Ynan] <- Inf
    Metric[Anan] <- Inf

    na <- order(Metric, decreasing = FALSE)[1:Na]
    analogs.final <- list()

    for (j in seq_along(variables)) {
        analogs.final[[variables[j]]] <- target[[variables[j]]][na]
    }
    result <- lapply(analogs.final, mean, na.rm = TRUE)
}
