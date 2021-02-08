# Test de Moran

bimoran.test <- function(x,y=NULL,listw,CENT=mean,DESV=sd,nsim=999){
  if(is.null(y)){
    y <- as.numeric(x[,2])
    x <- as.numeric(x[,1])
  }
  index <- 1:length(x)
  
  res <- numeric(length = nsim + 1)
  
  for (i in 1:nsim){
    iter <- sample(index)
    res[i] <- bimoran(x[iter], y[iter], listw)
  } 
  res[nsim + 1] <- bimoran(x, y, listw)
  
  R <- abs(res) >= abs(res[nsim + 1])
  R <- sum(R)
  
  p.sim<-(R+1)/(nsim+1)
  
  moran <- list('sim_statistics'=res,'statistic'=res[nsim + 1],
                      'pvalue'=p.sim)
  class(moran) <- 'moranPerm'
  return(moran)
}


