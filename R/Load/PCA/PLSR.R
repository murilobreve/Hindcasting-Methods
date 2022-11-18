#modeling



prediction <- time_series < forecasting.t0

d.target.na <- d[!prediction,]
d.target.na[,ncol(d.target.na)] <- NA

d.scale <- rbind(d[prediction,],d.target.na)
d.scale <- scale(d.scale, center = TRUE)

model.plsr <-
    plsr(
        target ~ .,
        ncomp = (length(d.scale[1, ]) - 1),
        data = as.data.frame(d.scale),
        validation = "CV",
        center = FALSE,
        scale = FALSE
    )

model.show <-
    plsr(
        target ~ .,
        ncomp = (length(d.scale[1, ]) - 1),
        data = as.data.frame(d[prediction,]),
        validation = "CV",
        center = TRUE,
        scale = TRUE
    )

pcr_model <- pcr( target ~ .,
                  ncomp = (length(d.scale[1, ]) - 1),
                  data = as.data.frame(d[prediction,]),
                  validation = "CV",
                  center = TRUE,
                  scale = TRUE)

#predicting

pca <- as.matrix(d.scale[,-ncol(d.scale)]) %*% model$loading.weights


if(all.equal(pca[,1][prediction], model$scores[,1]) == TRUE){
    PC1 <- pca[,1]
    PC2 <- pca[,2]
    PC3 <- pca[,3]
    message("PCs were generated")
    }

validHours <-
    (format(strptime(pred[,1]$stamp, "%Y-%m-%d %H:%M:%S"), '%H:%M:%S') >=  format(
        paste(startH, ":00:00", sep = ""),
        format = "%H:%M:%S",
        tz = "UTC"
    )) &
    (format(strptime(pred[,1]$stamp, "%Y-%m-%d %H:%M:%S"), '%H:%M:%S') <=  format(
        paste(endH, ":00:00", sep = ""),
        format = "%H:%M:%S",
        tz = "UTC"
    ))

x <- data_pred_target
new.x <- x[validHours,]
x.pred <- new.x[new.x[,ncol(new.x)] > as.numeric(forecasting.t0),]
x.time <- x.pred[,ncol(x.pred)]
x.pred1 <- x.pred[,-ncol(x.pred)]
x.pred2 <- x.pred1[,-ncol(x.pred1)]

prevAle <- predict(model.show, x.pred2, ncomp = 8)

sqrt(mean((x.pred1[,ncol(x.pred1)] - prevAle)^2, na.rm = TRUE))


data.pca.1_3 <-
    matrix(data = NA,
           nrow = length(data.pca[, 1]),
           ncol = 3)

v.1 <- "PC1"
v.2 <- "PC2"
v.3 <- "PC3"

pred1 <- list()
data.pca.1_3[complete.cases(data_pred_target), 1] <- PC1
pred1[[v.1]] <- data.pca.1_3[, 1]
pred1$n0 <- pred[,1]$n0
pred1$N <- pred[,1]$N
pred1$t0 <- pred[,1]$t0
pred1$stamp <- pred[,1]$stamp

if (nb.pca >= 2) {
    data.pca.1_3[complete.cases(data_pred_target), 2] <- PC2
    pred1[[v.2]] <- data.pca.1_3[, 2]
}

if (nb.pca >= 3) {
    data.pca.1_3[complete.cases(data_pred_target), 3] <- PC3
    pred1[[v.3]] <- data.pca.1_3[, 3]
}

message("[INFO] #PCA: ", nb.pca)
nb.var <- nb.pca
nb.series <- nb.pca
nb.stations <- 1
