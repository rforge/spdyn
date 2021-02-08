# Función para calcular moran univariado
unimoran <- function(x,listw,CENT=mean,DESV=sd) {
  bimoran(x,x,listw,CENT=CENT,DESV=DESV)
}
