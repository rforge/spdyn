# Internal function required by roseDiagram()
.roseData <- function(t1, t2, listw){
  data.diff <- t2 - t1
  
  lag.diff <- lag.listw(listw,t2)-lag.listw(listw,t1)
  
  A <- sum(data.diff>0 & lag.diff>0) #++
  B <- sum(data.diff>0 & lag.diff<0) #+-
  C <- sum(data.diff<0 & lag.diff<0) #--
  D <- sum(data.diff<0 & lag.diff>0) #-+
  
  mov <- c(A,B,C,D); mov <- ifelse(is.na(mov),0,mov)
  names(mov) <- c('Up-Up','Up-Down','Down-Down','Down-Up')
  dir <- c(45,135,225,315)  #dir <- c(0,90,180,260)
  rose <- data.frame(group=names(mov),mov=mov,dir=dir)
  
  return(rose)
}

.roseDiagram <- function(roseData){
  dir <- mov <- group <- NULL # Define internal variables required by function locally
      # IN this way you avoid the note <no visible binding for global variable > by
      # package checks
  
  colors <- c("blue",rgb(0,0,1,alpha=0.4),rgb(1,0,0,alpha=0.4),"red")
  
  ggplot(data=roseData, aes(x=dir, y=mov, fill=group)) +
    geom_bar(stat = "identity") +
    coord_polar(start = 0) + 
    scale_fill_manual(values = colors) +     
    theme(
      axis.text.x = element_blank(),
      axis.ticks.y = element_blank()
    ) +
    scale_x_continuous(breaks=c(0,90,270,360)) +
    geom_text(aes(y =mov, 
                  label = mov), size=5) +
    labs(x='', y='', fill='Movimientos') 
}


roseDiagram <- function(t1,t2,listw){  
  
  roseData <- .roseData(t1=t1, t2=t2, listw=listw)

  .roseDiagram(roseData)

}

  
