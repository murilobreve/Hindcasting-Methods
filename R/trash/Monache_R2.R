###############################################################################
# MONACHE PREDICTION FUNCTION
# STATIONS ARE CONSIDERED INDEPENDANT VARAIBLES
###############################################################################

rm(Y.n, Y.pred.n)

main <- function(n){

  y <- array(data=NA, dim=c(nb.series*(2*M+1)))

  y <- Y.pred[n,]

  Y.local <- Y

  if (sum(is.na(y)) < .5*(2*M+1)*nb.series) {  # guarantees window has 50% of valid values

    A <- (sweep(Y.local, 2, y, "-"))**2  # quad error
    Anan <- rowSums(is.na(A)) > .5*(2*M+1)*nb.series

    A1 <- sqrt(rowSums(A[,1:(2*M+1)],na.rm = TRUE))   # Calcula a raiz quadrada do valor flutuante especificado.
    A2 <- 0.0
    A3 <- 0.0
    A4 <- 0.0
    A5 <- 0.0
    A6 <- 0.0
    A7 <- 0.0
    A8 <- 0.0
    A9 <- 0.0

    if (nb.var >= 2) {
      A2 <- sqrt(rowSums(A[,((2*M+1)+1):(2*(2*M+1))], na.rm = TRUE))
    }
    if (nb.var >= 3) {
      A3 <- sqrt(rowSums(A[,(2*(2*M+1)+1):(3*(2*M+1))], na.rm = TRUE))
    }
    if (nb.stations >= 2) {
      index.debut = nb.var
      A4 <- sqrt(rowSums(A[,(index.debut*(2*M+1)+1):((index.debut+1)*(2*M+1))], na.rm = TRUE))
      if (nb.var >= 2) {
        A5 <- sqrt(rowSums(A[,((index.debut+1)*(2*M+1)+1):((index.debut+2)*(2*M+1))], na.rm = TRUE))
      }
      if (nb.var >= 3) {
        A6 <- sqrt(rowSums(A[,(5*(2*M+1)+1):(6*(2*M+1))], na.rm = TRUE))
      }
    }
    if (nb.stations >= 3) {
      index.debut = 2 * nb.var
      A7 <- sqrt(rowSums(A[,(index.debut*(2*M+1)+1):((index.debut+1)*(2*M+1))], na.rm = TRUE))
      if (nb.var >= 2) {
        A8 <- sqrt(rowSums(A[,((index.debut+1)*(2*M+1)+1):((index.debut+2)*(2*M+1))], na.rm = TRUE))
      }
      if (nb.var >= 3) {
        A9 <- sqrt(rowSums(A[,(8*(2*M+1)+1):(9*(2*M+1))], na.rm = TRUE))
      }
    }


    Metric <-
      (weight.st.1 * weight.v.1 / std.p1.v1) * A1 + (weight.v.2 * weight.st.1 /
                                                       std.p1.v2) * A2 + (weight.st.1 * weight.v.3 / std.p1.v3) * A3    + (weight.v.1 *
                                                                                                                             weight.st.2 / std.p2.v1) * A4 + (weight.st.2 * weight.v.2 / std.p2.v2) *
      A5 + (weight.v.3 * weight.st.2 / std.p2.v3) * A6    + (weight.v.1 * weight.st.3 /
                                                               std.p3.v1) * A7 + (weight.st.3 * weight.v.2 / std.p3.v2) * A8 + (weight.v.3 *
                                                                                                                                  weight.st.3 / std.p3.v3) * A9
    Metric[Ynan] <- Inf
    Metric[Anan] <- Inf

    na <- order(Metric, decreasing=FALSE)[1:Na]# + M cf correlation.R
    #wt <- c(1,0.9,0.8,0.7,0.6,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5)
    #wt <- c(25,24,23,22,21,20,19,18,17,16,15,14,13,12,11,10,9,8,7,6,5,4,3,2,1)
    analogs.final = target[[v.aim]][na]

    if (v.aim == 'WDIR') {
      angles <- rad(analogs.final)[!is.na(analogs.final)]
      result <- circ.mean(angles)
    } else {
      result = mean(analogs.final, na.rm = TRUE)
      #result = weighted.mean(analogs.final, wt, na.rm = TRUE)
    }
  } else {
    result <- NA
  }
  return(result)
}
