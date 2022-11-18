###############################################################################
# 0- Load Libraries
###############################################################################

message("[INFO] RUNNING")
start_time <- Sys.time()
message(paste("[CONFIG] Method =", method, sep = " "))

setwd("~/")
setwd("AnEnMDataR/R/settings")
source("libraries.R")
source("settings.R")

# 1- Start

ncores <- 16
method <-  "kmeans"

# 2- Load data

setwd("~/")
setwd("AnEnMDataR/R/Load")

if (do.pca == TRUE) {
    message("[INFO] Loading and organizing the data (PCA)")
    source("LoadPCA.R", local = TRUE)
} else {
    message("[INFO] Loading and organizing the data")
    source("LoadAndOrganizeData.R", local = TRUE)
}

# 3- Load methods

setwd("~/")
setwd("AnEnMDataR/R/methods")

if (method == "monache") {
    if (do.pca == FALSE) {
        if (independent) {
            print("Independent")
            source("Monache_independent.R", local = TRUE)

        } else{
            print("Dependent")
            source("Monache.R", local = TRUE)
        }
    } else{
        if (independent) {
            print("PCA Independent")
            source("Monache_independent_PCA.R", local = TRUE)

        } else{
            print("PCA Dependent")
            source("Monache_PCA.R", local = TRUE)
        }
    }
}

if (method == "kmeans") {
    if (independent) {
        clusteredAn1 = kmeans(Y[, 1:((2 * M) + 1)], nb.centros, iter.max = nb.iteracoes)
        clusteredAn2 = kmeans(Y[, (((2 *
                                         M) + 1) + 1):(((2 * M) + 1) * 2)], nb.centros, iter.max = nb.iteracoes)

        if (weight.type == "dtc") {
            source("Weight_Distance_To_Center_Independent.R")
        }

        else if (weight.type == "non") {
            source("Without_Any_Weight_Independent.R")
        }
    } else{
        gc()
        clusteredAn = kmeans(Y, nb.centros, iter.max =
                                 nb.iteracoes)

        if (weight.type == "dtc") {
            source("Weight_Distance_To_Center.R")
        }

        else if (weight.type == "non") {
            source("Without_Any_Weight.R")
        }
    }
}


# 4- Prediction
cluster <-
    makeCluster(ncores, type = "FORK") # giving 1 core to the system

prevAle <-
    matrix(data = NA,
           nrow = 2,
           ncol = (target$N - target$n0 + 1))
prevAle <-
    parSapply(cluster, 1:length(Y.pred[, 1]), main)
stopCluster(cluster)

# 6- Results
setwd("~/")
setwd("AnEnMDataR/R/results")
source("Generate_results.R", local = TRUE)

end_time <- Sys.time()

message("[RESULT] Running time: ", paste0(round(as.numeric(
    difftime(
        time1 = end_time,
        time2 = start_time,
        units = "secs"
    )
), 3), " Seconds"))
