DoPCA <- function(data.pca) {
    pca <- prcomp(na.omit(data_pca), scale. = TRUE, center = TRUE)

    nb.pca <- length(which(pca$sdev >= 1, TRUE))

    PCs <- list()
    array_pc <- array(NA, dim = length(data_pca[,1]))
    for(i in 1:length(data.pca[1,])){
        array_pc[complete.cases(data_pca)] <- pca$x[, i]
        PCs[[i]] <- array_pc
    }
    PCs <- do.call(cbind, PCs)

   result_pcaMain <- list(PCs, nb.pca)

    return(result_pcaMain)
}
