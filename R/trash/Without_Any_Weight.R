main <- function(n) {
    #n=1
    y <- array(data = NA, dim = c(nb.series * (2 * M + 1)))
    y <- Y.pred[n, ]

    if (sum(is.na(y)) < .5 * (2 * M + 1) * nb.series) {
        A <- (sweep(clusteredAn$centers, 2, y, "-")) ** 2  # quad error
        Metric <- sqrt(rowSums(A, na.rm = TRUE))
        cluster <- which.min(Metric)
        ensCluster <- (clusteredAn$cluster == cluster)
        clusterElem <- Y.purged[ensCluster, ]

        if (length(clusterElem) != length(Y.purged[1, ])) {
            B <- (sweep(clusterElem, 2, y, "-")) ** 2  # quad error
            Metric2 <- sqrt(rowSums(B, na.rm = TRUE))
        } else{
            B <- (clusterElem - y) ** 2
            Metric2 <- sqrt(sum(B, na.rm = TRUE))
        }

        analogs.final.local = analogs.final[ensCluster]
        analogs.final.local <- analogs.final.local[order(Metric2)]

        if (length(Metric2) >= Na && Na > 0) {
            analogs.final.local <- analogs.final.local[1:Na]
        } else{
            #Na <- length(Metric2)
        }

        result <- matrix(data =  NA,
                         nrow = 2,
                         ncol = 1)
        result[1, 1] = mean(analogs.final.local, na.rm = TRUE) #FOI USADO EM TODOS OS TESTES ANTERIORES
        result[2, 1] <- length(analogs.final.local)
    } else {
        result <- matrix(data =  NA,
                         nrow = 2,
                         ncol = 1)
        result[1, 1] <- NA
        result[2, 1] <- -1
    }
    return(result)
}
