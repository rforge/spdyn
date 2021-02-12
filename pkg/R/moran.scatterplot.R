
moran.scatterplot <- function(x,y=NULL,listw,CENT=mean,DESV=sd, lisa=NULL, signif=0.05, ...){
    
    ylab <- ifelse(is.null(y),'lag (x)','lag (y)')
    
    if(is.null(y) & is.null(ncol(x)) ){
        y <- x        
    }else{
        if(is.null(y) & !is.null(ncol(x)) ){
            y <- as.numeric(x[, 2])
            x <- as.numeric(x[, 1])
        }
        
    }
    
    
    #  browser()
    
    zx <- (x - CENT(x))/DESV(x)
    zy <- (y - CENT(y))/DESV(y)
    
    lag.zy <- lag.listw(listw,zy)
    
    eq <- formula(lag.zy ~ zx)
    reg <- lm( eq )
    
    col <- 'black'
    
    if(!is.null(lisa) & class(lisa)=='lisaPerm'){
        #browser()
        q <- clusterQuadrant(lisa,signif=signif)
        brks <- c(0,1,2,3,4)
        colors <- c("grey30","red","blue",rgb(0,0,1,alpha=0.4),rgb(1,0,0,alpha=0.4))
        
        col = colors[findInterval(q,brks,all.inside=FALSE)]        
    }
    
    #browser()
    plot(eq, pch=20, xlab='x', ylab=ylab, 
         main='Moran Scatter Plot',          
         col=col, 
         bg=col,        
         sub=paste('Moran statistic:',round(coef(reg)[2],3) ), ...)
    
    abline(v=CENT(zx),h=CENT(lag.zy), lty=2, lwd=0.5)
    abline( reg, lty=1, lwd=1, col='red')
    
    
}



