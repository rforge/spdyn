unimoran.test <- function(x,listw,CENT=mean,DESV=sd,nsim=999) {
  bimoran.test(x,x,listw,CENT=CENT,DESV=DESV,nsim=nsim)
}