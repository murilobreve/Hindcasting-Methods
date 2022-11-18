###############################################################################
# MONACHE PREDICTION FUNCTION
# STATIONS ARE CONSIDERED INDEPENDANT VARAIBLES
###############################################################################

rm(Y.n, Y.pred.n)
# n<- 1
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
      order(Metric1, decreasing = FALSE)[1:Na]# + M cf correlation.R
    analogs.final1 = target[[v.aim]][na1]

    result1 = mean(analogs.final1, na.rm = TRUE)
    if (v.aim == "WDIR")
      result1 <- mean.circular(analogs.final1, na.rm = TRUE)
  } else {
    result1 <- NA
  }
  ################################################################# STATION 2  #################################################################
  if (nb.stations >= 2) {
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
        order(Metric2, decreasing = FALSE)[1:Na]# + M cf correlation.R
      analogs.final2 = target[[v.aim]][na2]

      result2 = mean(analogs.final2, na.rm = TRUE)

      if (v.aim == "WDIR")
        result2 <- mean.circular(analogs.final2, na.rm = TRUE)

    } else {
      result2 <- NA
    }
  }else{
    result2 <- NA
  }
  ################################################################# STATION 3 #################################################################
  if (nb.stations >= 3) {
    y3 <- array(data = NA, dim = c((2 * M + 1)))
    y3 <- Y.pred[n, (2 * (2 * M + 1) + 1):((2 * M + 1) * 3)]

    Y3.local <- Y[, (2 * (2 * M + 1) + 1):((2 * M + 1) * 3)]

    if (sum(is.na(y3)) < .5 * (2 * M + 1)) {
      # guarantees window has 50% of valid values

      A13 <- (sweep(Y3.local, 2, y3, "-")) ** 2  # quad error
      if (v.3 == 'WDIR') {
        A13 <- abs(A13 - ceiling((A13 - 180) / 360) * 360)
      }

      Anan3 <- rowSums(is.na(A13)) > .5 * (2 * M + 1)
      A13 <-
        sqrt(rowSums(A13[, 1:(2 * M + 1)], na.rm = TRUE))

      Metric3 <- A13
      Metric3[Ynan] <- Inf
      Metric3[Anan3] <- Inf

      na3 <-
        order(Metric3, decreasing = FALSE)[1:Na]# + M cf correlation.R
      analogs.final3 = target[[v.aim]][na3]

      result3 = mean(analogs.final3, na.rm = TRUE)

      if (v.aim == "WDIR")
        result3 <-
        mean.circular(analogs.final3, na.rm = TRUE)

    } else {
      result3 <- NA
    }
  } else{
    result3 <- NA
  }

  if (length(na.omit(c(result1, result2, result3))) == 0) {
    result.final <- NA
  }  else {
    result.na <- na.omit(c(result1, result2, result3))
    result.final <- mean(result.na)
    if (v.aim == "WDIR")
      result.final <- mean(circular(result.na))
  }

  return(result.final)

}
