## Función para calcular Moran Global Bivariado

bimoran <- function(x,y=NULL,listw,perm=999,CENT=mean,DESV=sd){
  if(is.null(y)){
    y <- as.numeric(x[,2])
    x <- as.numeric(x[,1])
  }
  
  zx <- (x-CENT(x))/DESV(x)
  zy <- (y-CENT(y))/DESV(y)
  
  n <- length(x)
  SW <- Szero(listw)
  #Sxy<-sqrt(sum(zx^2)/(n-1))*sqrt(sum(zy^2)/(n-1)) 
  
  lzy<-lag.listw(listw,zy)
  #I <- (n/SW)*(sum(zx%*%lzy)/Sxy) # Otra forma de calcular el Moran
  
  I <- (n/SW)*sum(zx*lzy)/(n-1)
  
  return(I)
}



