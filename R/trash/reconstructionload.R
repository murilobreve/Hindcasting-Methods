#####################################################################################
## This source loads and organizes inputs stations into usable data to the methods ##
## Adapted by Murilo Montanini, 1st is Leonardo from the old project               ##
#####################################################################################

##############################
# 0 - Change directories
##############################

message("[INFO] Loading inteporlate function")
setwd("~/")
setwd("AnEnMDataR/R")
source("InterpolateData.R")
source("LoadStation.R", local = TRUE)


##############################
# 1 - Load the .nc files
##############################

pred1 <- LoadStation(name.pred1)

if (pca.stations >= 2)
    pred2 <- LoadStation(name.pred2)

if (pca.stations >= 3)
    pred3 <- LoadStation(name.pred3)

if (pca.stations >= 4)
    pred4 <- LoadStation(name.pred4)

if (pca.stations >= 5)
    pred5 <- LoadStation(name.pred5)

if (pca.stations >= 6)
    pred6 <- LoadStation(name.pred6)

if (pca.stations >= 7)
    pred7 <- LoadStation(name.pred7)

if (pca.stations >= 8)
    pred8 <- LoadStation(name.pred8)

if (pca.stations >= 9)
    pred9 <- LoadStation(name.pred9)

target <- LoadStation(name.hist)




###############################
# 2 - PCA analysis
################################

data.pca1 <- do.call(cbind, pred1[variables])
data.pca <- data.pca1[, variables]

if (pca.stations >= 2) {
    data.pca2 <- do.call(cbind, pred2[variables])
    data.pca <- cbind(data.pca, data.pca2)
}

if (pca.stations >= 3) {
    data.pca3 <- do.call(cbind, pred3[variables])
    data.pca <- cbind(data.pca, data.pca3)
}

if (pca.stations >= 4) {
    data.pca4 <- do.call(cbind, pred4[variables])
    data.pca <- cbind(data.pca, data.pca4)
}

if (pca.stations >= 5) {
    data.pca5 <- do.call(cbind, pred5[variables])
    data.pca <- cbind(data.pca, data.pca5)
}

if (pca.stations >= 6) {
    data.pca6 <- do.call(cbind, pred6[variables])
    data.pca <- cbind(data.pca, data.pca6)
}

if (pca.stations >= 7) {
    data.pca7 <- do.call(cbind, pred7[variables])
    data.pca <- cbind(data.pca, data.pca7)
}

if (pca.stations >= 8) {
    data.pca8 <- do.call(cbind, pred8[variables])
    data.pca <- cbind(data.pca, data.pca8)
}

if (pca.stations >= 9) {
    data.pca9 <- do.call(cbind, pred9[variables])
    data.pca <- cbind(data.pca, data.pca9)
}
data.pca.scaled.na <- na.omit(data.pca)

pca <- prcomp(data.pca.scaled.na, center = TRUE, scale. = TRUE)
sd.pca <- (pca$sdev >= 1)
nb.pca <- length(sd.pca[sd.pca == TRUE])

data.pca.1_3 <-
    matrix(data = NA,
           nrow = length(data.pca[, 1]),
           ncol = 3)

v.1 <- "PC1"
v.2 <- "PC2"
v.3 <- "PC3"


pc1 <- pca$x[, 1]
data.pca.1_3[complete.cases(data.pca), 1] <- pc1
pred1[[v.1]] <- data.pca.1_3[, 1]

if (nb.pca >= 2) {
    pc2 <- pca$x[, 2]
    data.pca.1_3[complete.cases(data.pca), 2] <- pc2
    pred1[[v.2]] <- data.pca.1_3[, 2]
}

if (nb.pca >= 3) {
    pc3 <- pca$x[, 3]
    data.pca.1_3[complete.cases(data.pca), 3] <- pc3
    pred1[[v.3]] <- data.pca.1_3[, 3]
}

message("[INFO] #PCA: ", nb.pca)
message("[INFO] #Stations: ", pca.stations)
nb.var <- nb.pca
nb.series <- nb.pca
nb.stations <- 1

set.seed(123)
ind <-sort(sample(length(unlist(target[variables])), 1000))
target.random[ind] <- NA
exact.time <- rep(1:length(pred1[[v.1]]))[ind]



################################
# 6 - Organize data from inputs
################################

Y.analogs.p1.v1 <- list()

for (i in 1:(2 * M + 1)) {
    Y.analogs.p1.v1[[i]] <- pred1[[v.1]][i:(pred1$n0 + i - 2 * M - 2)]
}
Y.analogs.p1.v1 = do.call(cbind, Y.analogs.p1.v1)

Y.predictions.p1.v1 <-
    matrix(
        NA,
        nrow = length(exact.time),
        ncol = (2*M+1),
        byrow = TRUE
    )

for(i in 1:length(exact.time)){
    Y.predictions.p1.v1[i,] <- pred1[[v.1]][(exact.time[i]-M):(exact.time[i]+M)]
}

Y <-
    matrix(
        data = NA,
        nrow = length(Y.analogs.p1.v1[, 1]),
        ncol = (2 * M + 1)
    )
Y[, 1:(2 * M + 1)] <- Y.analogs.p1.v1

################################################################
# 7 - Organizing the Y.n data
################################################################

Y.n <-
    matrix(
        data = NA,
        nrow = length(Y.analogs.n.p1.v1[, 1]),
        ncol = (2 * M + 1)
    )
Y.n[, 1:(2 * M + 1)] <- Y.analogs.n.p1.v1

Y.pred <-
    matrix(
        data = NA,
        nrow = length(Y.predictions.p1.v1[, 1]),
        ncol = (2 * M + 1)
    )
Y.pred[, 1:(2 * M + 1)] <- Y.predictions.p1.v1





################################################################
# 15 - Changing to main directory
################################################################

setwd("~/")
setwd("AnEnMDataR")

################################################################
#  END OF FUNCTION
################################################################

