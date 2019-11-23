
spInitState <-
    function(data,listw,stateVars,n.states,stateNames=NULL,style='quantile',
             breaks,breaks.lag,pool=TRUE,std=TRUE){
        x<-data[,stateVars]
        n<-nrow(x)
        t<-ncol(x)
        if(is.null(stateNames)){stateNames=seq(from=1, to=n.states,length.out=n.states)}
        if(std==TRUE){for(i in 1:t)x[,i]<-x[,i]/mean(x[,i],na.rm=TRUE)}
        lw<-listw
        w<-listw2mat(lw)
        ly<-w%*%as.matrix(x)
        if(pool==FALSE){
            if(style=='fixed'){
                brks<-breaks
                stopifnot(is.matrix(brks)==FALSE)
            }else{
                brks<-matrix(0,nrow=3,ncol=n.states+1)
            }
            
                brks[1,]<-quantile(ly[,1],seq(from=0,to=1,length.out=n.states+1))
                brks[1,1]<-brks[1,1]-brks[1,1]/1e+10;brks[1,n.states+1]<-brks[1,n.states+1]+brks[1,n.states+1]/1e+10
                brks[2,]<-quantile(x[,1],seq(from=0,to=1,length.out=n.states+1))
                brks[2,1]<-brks[2,1]-brks[2,1]/1e+10;brks[2,n.states+1]<-brks[2,n.states+1]+brks[2,n.states+1]/1e+10
                
                x$cutL<-cut(ly[,1],breaks=brks[1,],labels=stateNames,ordered=TRUE,rigth=FALSE)
                x$cut0<-cut(x[,1],breaks=brks[2,],labels=stateNames,ordered=TRUE,rigth=FALSE)
                mt<-table(x$cutL,x$cut0)
            
        }else{
            if(style=='fixed'){
                stopifnot(std==FALSE)
                brks<-breaks
                brks.ly<-breaks.lag
                stopifnot(is.numeric(brks)==TRUE)
            }else{
                brks.ly<-quantile(ly,seq(from=0,to=1,length.out=n.states+1))
                brks.ly[1]<-brks.ly[1]-brks.ly[1]/1e-10;brks.ly[n.states+1]<-brks.ly[n.states+1]+brks.ly[n.states+1]/1e-10
                
                brks<-quantile(as.matrix(x),seq(from=0,to=1,length.out=n.states+1))
                brks[1]<-brks[1]-brks[1]/1e+10;brks[n.states+1]<-brks[n.states+1]+brks[n.states+1]/1e+10
            }
            
                x$cutL<-cut(ly[,1],breaks=brks.ly,labels=stateNames,ordered=TRUE,rigth=FALSE)
                x$cut0<-cut(x[,1],breaks=brks,labels=stateNames,ordered=TRUE,rigth=FALSE)
                mt<-table(x$cutL,x$cut0)
            }

	mt <- matrix(mt, n.states, n.states)
	dimnames(mt) <- list( paste("nb.", stateNames, sep=''), stateNames)

	z <- list('initStates'=mt,'brks'=brks,'brks.lag'=brks.ly)
       
        return(z)
    }


