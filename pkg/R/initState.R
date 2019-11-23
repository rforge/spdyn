initState <- function(data,stateVars,n.states,stateNames=NULL,discretized=FALSE,
			pool=FALSE,std=FALSE,balanced=TRUE){{

       x<-data[,stateVars]
        n<-nrow(x)
        t<-ncol(x)
        
        if(is.null(stateNames)){stateNames=seq(from=1, to=n.states,length.out=n.states)}
        if(std==TRUE){for(i in 1:t)x[,i]<-x[,i]/mean(x[,i],na.rm=TRUE)}
        if(discretized==TRUE & pool==TRUE) stop('discretized==TRUE & pool==TRUE: "pool" is an option available only for non-discretized data ')
        if(balanced==TRUE){na.rm <- FALSE}else{na.rm <- TRUE}

            
        if(pool==FALSE){
           brks<-matrix(0,nrow=2,ncol=n.states+1)
           mt<-matrix(0,n.states,n.states)
           brks[1,]<-quantile(x[,1],seq(from=0,to=1,length.out=n.states+1),na.rm=na.rm)
           brks[1,1]<-brks[1,1]-1/1e+10;brks[1,n.states+1]<-brks[1,n.states+1]+1/1e+10
                    
           x$cut0<-cut(x[,1],breaks=brks[1,],labels=stateNames,ordered=TRUE,rigth=FALSE)
           mt<-table(x$cut0)
                
         }else{
           brks<-quantile(as.matrix(x),seq(from=0,to=1,length.out=n.states+1),na.rm=na.rm)
           brks[1]<-brks[1]-1/1e+10;brks[n.states+1]<-brks[n.states+1]+1/1e+10
           x$cut0<-cut(x[,1],breaks=brks,labels=stateNames,ordered=TRUE,rigth=FALSE)
           mt<-table(x$cut0)
                }
            }
 	   z <- list('initStates'=mt,'brks'=brks)
 	   return(z)		
        }



