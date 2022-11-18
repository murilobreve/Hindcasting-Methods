pca <- prcomp(na.omit(data.pca), scale. = TRUE, center = TRUE)

PC1 <- pca$x[,1]
PC2 <- pca$x[,1]
PC3 <- pca$x[,1]

data.pca.1_3 <-
    matrix(data = NA,
           nrow = length(data.pca[, 1]),
           ncol = 3)

v.1 <- "PC1"
v.2 <- "PC2"
v.3 <- "PC3"

pred1 <- list()
data.pca.1_3[complete.cases(data.pca), 1] <- PC1
pred1[[v.1]] <- data.pca.1_3[, 1]
pred1$n0 <- pred[,1]$n0
pred1$N <- pred[,1]$N
pred1$t0 <- pred[,1]$t0
pred1$stamp <- pred[,1]$stamp

if (nb.pca >= 2) {
    data.pca.1_3[complete.cases(data.pca), 2] <- PC2
    pred1[[v.2]] <- data.pca.1_3[, 2]
}

if (nb.pca >= 3) {
    data.pca.1_3[complete.cases(data.pca), 3] <- PC3
    pred1[[v.3]] <- data.pca.1_3[, 3]
}

message("[INFO] #PCA: ", nb.pca)
nb.var <- nb.pca
nb.series <- nb.pca
nb.stations <- 1

