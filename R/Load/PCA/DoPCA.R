DoPCA <- function(pca.data) {
    pca <- prcomp(na.omit(pca.data), scale. = TRUE, center = TRUE)
    
    nb.pca <- length(which(pca$sdev >= 1, TRUE))
    
    PCs <- list()
    array_pc <- array(NA, dim = length(pca.data[,1]))
    for(i in 1:length(pca.data[1,])){
        array_pc[complete.cases(pca.data)] <- pca$x[, i]
        PCs[[i]] <- array_pc
    }
    
    PCs <- do.call(cbind, PCs)
    
    result_pcaMain <- list(PCs, nb.pca)
    
    return(result_pcaMain)
}
