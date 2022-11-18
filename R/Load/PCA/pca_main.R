PcaMain <- function(data.pca) {
    pca <- prcomp(na.omit(data_pca), scale. = TRUE, center = TRUE)


    nb.pca <- length(which(pca$sdev >= 1, TRUE))
    PC1 <- pca$x[, 1]
    PC2 <- pca$x[, 2]
    PC3 <- pca$x[, 3]

    PCs <-
        matrix(data = NA,
               nrow = length(data_pca[, 1]),
               ncol = 3)

    v.1 <- "PC1"
    v.2 <- "PC2"
    v.3 <- "PC3"

    pred1 <- list()
    PCs[complete.cases(data_pca), 1] <- PC1
    pred1[[v.1]] <- PCs[, 1]
    pred1$n0 <- pred[, 1]$n0
    pred1$N <- pred[, 1]$N
    pred1$t0 <- pred[, 1]$t0
    pred1$stamp <- pred[, 1]$stamp

    if (nb.pca >= 2) {
        PCs[complete.cases(data_pca), 2] <- PC2
        pred1[[v.2]] <- PCs[, 2]
    }

    if (nb.pca >= 3) {
        PCs[complete.cases(data_pca), 3] <- PC3
        pred1[[v.3]] <- PCs[, 3]
    }

    message("[INFO] #PCA: ", nb.pca)


    return(pred1)

}
