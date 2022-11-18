main <- function(n){
  #n=1
  y1 <- array(data=NA, dim=c((2*M+1)))
  y2 <- array(data=NA, dim=c((2*M+1)))
  
  y1 <- Y.pred[n,1:(2*M+1)]
  y2 <- Y.pred[n,((2*M+1)+1):((2*M+1)*2)]
  
  ############################################ STATION 1
  if (sum(is.na(y1)) < .5*(2*M+1)) {
    A1 <- (sweep(clusteredAn1$centers, 2, y1, "-"))**2  # quad error#
    Metric1 <- sqrt(rowSums(A1, na.rm=TRUE))#
    cluster1 <- which.min(Metric1)#
    ensCluster1 <- (clusteredAn1$cluster == cluster1)#
    clusterElem1 <- Y.purged[ensCluster1,1:(2*M+1)]              ###
    
    if(length(clusterElem1) != (length(Y.purged[1,])/2)){###
      B1 <- (sweep(clusterElem1, 2, y1, "-"))**2  # quad error
      Metric21 <- sqrt(rowSums(B1, na.rm=TRUE))
    }else{
      B1 <- (clusterElem1 - y1)**2
      Metric21 <- sqrt(sum(B1, na.rm=TRUE))
    }
    
    analogs.final.local1 = analogs.final[ensCluster1]
    analogs.final.local1 <- analogs.final.local1[order(Metric21)]
    
    if(length(Metric21) >= Na && Na > 0){
      analogs.final.local1 <- analogs.final.local1[1:Na]
    }else{
      #Na <- length(Metric2)
    }
    
    result1 = mean(analogs.final.local1, na.rm = TRUE) #FOI USADO EM TODOS OS TESTES ANTERIORES
    
  } else {
    result1 <- NA
  }
  ############################################ STATION 2
  if (sum(is.na(y2)) < .5*(2*M+1)) {
    A2 <- (sweep(clusteredAn2$centers, 2, y2, "-"))**2  # quad error#
    Metric2 <- sqrt(rowSums(A2, na.rm=TRUE))#
    cluster2 <- which.min(Metric2)#
    ensCluster2 <- (clusteredAn2$cluster == cluster2)#
    clusterElem2 <- Y.purged[ensCluster2,((2*M+1)+1):((2*M+1)*2)]              ###
    
    if(length(clusterElem2) != (length(Y.purged[1,])/2)){###
      B2 <- (sweep(clusterElem2, 2, y2, "-"))**2  # quad error
      Metric22 <- sqrt(rowSums(B2, na.rm=TRUE))
    }else{
      B2 <- (clusterElem2 - y2)**2
      Metric22 <- sqrt(sum(B2, na.rm=TRUE))
    }
    
    analogs.final.local2 = analogs.final[ensCluster2]
    analogs.final.local2 <- analogs.final.local2[order(Metric22)]
    
    if(length(Metric22) >= Na && Na > 0){
      analogs.final.local2 <- analogs.final.local2[1:Na]
    }else{
      #Na <- length(Metric2)
    }
    
    result2 = mean(analogs.final.local2, na.rm = TRUE) #FOI USADO EM TODOS OS TESTES ANTERIORES
    
  } else {
    result2 <- NA
  }
  if(is.na(result1)){
    if(is.na(result2)){
      return(NA)
    }
    return(result2) 
  }else{
    if(is.na(result2)){
      return(result1) 
    }
    return((result1+result2)/2)
  }
}
