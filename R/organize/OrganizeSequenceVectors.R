OrganizeSequenceVectors <-
    function(PCs_and_pca, target, loaded_stations) {
        time.start <- params_list$prediction_t0
        PCs <- PCs_and_pca[[1]]
        nb_pca <- PCs_and_pca[[2]]

        PCs <- PCs[, seq_len(nb_pca)]

        fit_data <- matrix(NA, ncol = nb_pca, nrow = M)

        PCs <- rbind(fit_data, PCs, fit_data)

        pred1 <- loaded_stations[[1]][, 1]

        Y <- list()
        for (j in seq_along(PCs[1,])) {
            data_into_vectors <- list()
            for (i in 1:(2 * M + 1)) {
                data_into_vectors[[i]] <- PCs[i:(length(PCs[, 1]) - (2 * M + 1) + i), j]
            }
            Y[[j]] = do.call(cbind, data_into_vectors)
        }
        Y.all = do.call(cbind, Y)

        training_time <- pred1$stamp < time.start

        Y.analogues <- Y.all[training_time, ]
    Y.pred <- Y.all[!training_time, ]

        Ynan <-
            rowSums(is.na(Y.analogues)) >= .5 * M * nb.series

        result.list <- list(Y.analogues, Y.pred, Ynan)
        names(result.list) <- c("Y.analogues", "Y.pred", "Ynan")

        return(result.list)
    }
