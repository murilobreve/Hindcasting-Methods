DoPLS <- function(data_pca, target_training) {
    period.t.size <- length(target_training)
    
    period.all.size <- length(data_pca[, 1])
    
    target_training[(period.t.size + 1):period.all.size] <- NA
    
    all.training <- cbind(data_pca, target_training)
    all.training.scaled <- scale(all.training)
    
    model.plsr <-
        plsr(
            target_training ~ .,
            ncomp = (length(all.training.scaled[1,]) - 1),
            data = as.data.frame(all.training.scaled),
            validation = NULL,
            center = FALSE,
            scale = FALSE
        )
    pca <- prcomp(na.omit(data_pca))
    
    pls.PCs <-
        as.matrix(all.training.scaled[, -ncol(all.training.scaled)]) %*% model.plsr$loading.weights
    nb.pca <- length(which(pca$sdev >= 1, TRUE))
    
    result_pcaMain <- list(pls.PCs, nb.pca)
    
    return(result_pcaMain)
}
