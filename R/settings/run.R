# Libraries
setwd("~/")
setwd("AnEnMDataR/R/settings")
source("libraries.R")
source("settings.R")
source("LoadFunctions.R")

Regressions(params_list,
            method = "PCR",
            variables <- c("WSPD", "GST", "ATMP", "PRES"),
            variables_predict <- "WSPD")


