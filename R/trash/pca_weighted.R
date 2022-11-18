d.clean <- scale(na.omit(data.pca))

cov_matrix <- cov(d.clean)
e <- eigen(cov_matrix)

nb.pca <- length(e$values[e$values >= 1])

PC1 <- 0
for (i in 1:length(d.clean[1,])) {
    PC1 <- PC1 + d.clean[, i] * e$vectors[i, 1] * cor_study[i]
}

if (nb.pca > 1) {
    PC2 <- 0
    for (i in 1:length(d.clean[1,])) {
        PC2 <- PC2 + d.clean[, i] * e$vectors[i, 2] * cor_study[i]
    }
}

if (nb.pca > 2) {
    PC3 <- 0
    for (i in 1:length(d.clean[1,])) {
        PC3 <- PC3 + d.clean[, i] * e$vectors[i, 3] * cor_study[i]
    }
}
