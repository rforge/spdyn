###############################LISA Cluster Map################################

plot.lisaPerm <- function(x,y,signif=.05,legend.title='',lty=1,lwd=1,fillBorder='black',box.lwd=1,...){
  lmoran=x
  shape=y
  
  if(class(lmoran)!='lisaPerm')stop('object class is not lisa.perm')
  
  q <- clusterQuadrant(lmoran,signif=signif)
  brks <- c(0,1,2,3,4)
  colors <- c("white","red","blue",rgb(0,0,1,alpha=0.4),rgb(1,0,0,alpha=0.4))
  
  sp::plot(shape,border="lightgray",col=colors[findInterval(q,brks,all.inside=FALSE)],
       lty=lty,lwd=lwd)
  box(lwd=box.lwd)
  legend("bottomright",legend=c("insignificant","high-high","low-low","low-high","high-low"),
         fill=colors,bty="n",cex=0.7,y.intersp=1,x.intersp=1,title=legend.title,border=fillBorder)
  title("LISA Cluster Map")
  
}
