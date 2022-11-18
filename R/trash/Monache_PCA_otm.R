main <- function(n) {
  y <- array(data = NA, dim = c(nb.series * (2 * M + 1)))
  y <- Y.pred[n, ]

  Y.local <- Y

  if (sum(is.na(y)) < .5 * (2 * M + 1) * nb.series) {
    # guarantees window has 50% of valid values

    A <- (sweep(Y.local, 2, y, "-")) ** 2  # quad error
    Anan <- rowSums(is.na(A)) > .5 * (2 * M + 1) * nb.series

    A1 <-
      sqrt(rowSums(A[, 1:(2 * M + 1)], na.rm = TRUE))   # Calcula a raiz quadrada do valor flutuante especificado.
    A2 <- 0.0
    A3 <- 0.0
    A4 <- 0.0
    A5 <- 0.0
    A6 <- 0.0
    A7 <- 0.0
    A8 <- 0.0
    A9 <- 0.0

    if (nb.pca >= 2) {
      A2 <- sqrt(rowSums(A[,((2*M+1)+1):(2*(2*M+1))], na.rm = TRUE))
    }
    if (nb.pca >= 3) {
      A3 <- sqrt(rowSums(A[,(2*(2*M+1)+1):(3*(2*M+1))], na.rm = TRUE))
    }
    if (nb.stations >= 2) {
      index.debut = nb.pca
      A4 <- sqrt(rowSums(A[,(index.debut*(2*M+1)+1):((index.debut+1)*(2*M+1))], na.rm = TRUE))
      if (nb.pca >= 2) {
        A5 <- sqrt(rowSums(A[,((index.debut+1)*(2*M+1)+1):((index.debut+2)*(2*M+1))], na.rm = TRUE))
      }
      if (nb.pca >= 3) {
        A6 <- sqrt(rowSums(A[,(5*(2*M+1)+1):(6*(2*M+1))], na.rm = TRUE))
      }
    }
    if (nb.stations >= 3) {
      index.debut = 2 * nb.pca
      A7 <- sqrt(rowSums(A[,(index.debut*(2*M+1)+1):((index.debut+1)*(2*M+1))], na.rm = TRUE))
      if (nb.pca >= 2) {
        A8 <- sqrt(rowSums(A[,((index.debut+1)*(2*M+1)+1):((index.debut+2)*(2*M+1))], na.rm = TRUE))
      }
      if (nb.pca >= 3) {
        A9 <- sqrt(rowSums(A[,(8*(2*M+1)+1):(9*(2*M+1))], na.rm = TRUE))
      }
    }

    Metric <-
      (weight.st.1 * weight.v.1 / std.p1.v1) * A1 + (weight.v.2 * weight.st.1 /
                                                       std.p1.v2) * A2 + (weight.st.1 * weight.v.3 / std.p1.v3) * A3        +
      (weight.v.1 * weight.st.2 / std.p2.v1) * A4 + (weight.st.2 * weight.v.2 /
                                                       std.p2.v2) * A5 + (weight.v.3 * weight.st.2 / std.p2.v3) * A6        +
      (weight.v.1 * weight.st.3 / std.p3.v1) * A7 + (weight.st.3 * weight.v.2 /
                                                       std.p3.v2) * A8 + (weight.v.3 * weight.st.3 / std.p3.v3) * A9

    Metric[Ynan] <- Inf
    Metric[Anan] <- Inf

    na <- order(Metric, decreasing = FALSE)[1:Na]

    analogs.final <- list()
    for (j in seq_along(variables)) {
      analogs.final[[variables[j]]] <- target[[variables[j]]][na]
    }
    variables_noWDIR <- variables[variables != "WDIR"]
    result <- lapply(analogs.final[variables_noWDIR], mean, na.rm = TRUE)

    if ('WDIR' %in% variables) {
      angles <- analogs.final[["WDIR"]][!is.na(analogs.final[["WDIR"]])]
      result[["WDIR"]] <- circ.mean(angles)
      if (result[["WDIR"]] < 0)
        result[["WDIR"]] <- 2*pi + result[["WDIR"]]
    }

  } else   {
    #result <- list()
    #result[variables] <- NA
    result <- NA
  }

  return(result)

}
