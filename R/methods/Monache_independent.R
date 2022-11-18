###############################################################################
# MONACHE PREDICTION FUNCTION
# STATIONS ARE CONSIDERED INDEPENDANT VARAIBLES
###############################################################################



main <- function(n) {
  y1 <- array(data = NA, dim = c((2 * M + 1)))
  y1 <- Y.pred[n, 1:(2 * M + 1)]

  Y1.local <- Y[, 1:(2 * M + 1)]

  if (sum(is.na(y1)) < .5 * (2 * M + 1)) {
    A11 <- (sweep(Y1.local, 2, y1, "-")) ** 2  # quad error
    if (v.1 == 'WDIR') {
      A11 <- abs(A11 - ceiling((A11 - 180) / 360) * 360)
    }

    Anan1 <- rowSums(is.na(A11)) > .5 * (2 * M + 1)
    A11 <-
      sqrt(rowSums(A11[, 1:(2 * M + 1)], na.rm = TRUE))

    Metric1 <- A11
    Metric1[Ynan] <- Inf
    Metric1[Anan1] <- Inf

    na1 <-
      order(Metric1, decreasing = FALSE)[1:Na]

  }else{
    na1 <- NA

  }
  ################################################################# STATION 2  #################################################################
  if (nb.pca >= 2) {
    y2 <- array(data = NA, dim = c((2 * M + 1)))
    y2 <- Y.pred[n, ((2 * M + 1) + 1):((2 * M + 1) * 2)]

    Y2.local <- Y[, ((2 * M + 1) + 1):((2 * M + 1) * 2)]

    if (sum(is.na(y2)) < .5 * (2 * M + 1)) {
      # guarantees window has 50% of valid values

      A12 <- (sweep(Y2.local, 2, y2, "-")) ** 2  # quad error
      if (v.2 == 'WDIR') {
        A12 <- abs(A12 - ceiling((A12 - 180) / 360) * 360)
      }

      Anan2 <- rowSums(is.na(A12)) > .5 * (2 * M + 1)
      A12 <-
        sqrt(rowSums(A12[, 1:(2 * M + 1)], na.rm = TRUE))

      Metric2 <- A12
      Metric2[Ynan] <- Inf
      Metric2[Anan2] <- Inf

      na2 <-
        order(Metric2, decreasing = FALSE)[1:Na]

    } else {
      na2 <- NA
    }
  }else{
    na2 <- NA
  }

  if (length(na.omit(na1, na2)) != 0) {
    MetricT <- na.omit(c(Metric1[na1], Metric2[na2]))

    naT <- order(MetricT, decreasing = FALSE)[1:Na]
    maxT <- max(naT)

    naResult <- c(na1[1:(2 * Na - maxT)], na2[1:(maxT - Na)])

    analogs.final <- list()
    for (j in seq_along(variables)) {
      analogs.final[[variables[j]]] <- target[[variables[j]]][naResult]
    }
    result.final <-
      lapply(analogs.final[variables], mean, na.rm = TRUE)

  }  else{
    result.final <- NA
  }
  return(result.final)

}

