markov<-function(data,stateVars,n.states,stateNames=NULL,discretized=FALSE,pool=FALSE,std=FALSE){
 x<-data[,stateVars]
 n<-nrow(x)
 t<-ncol(x)

if(is.null(stateNames)){stateNames=seq(from=1, to=n.states,length.out=n.states)}
if(std==TRUE){for(i in 1:t)x[,i]<-x[,i]/mean(x[,i],na.rm=TRUE)}
if(discretized==TRUE & pool==TRUE) stop('discretized==TRUE & pool==TRUE: pool is an option available only for non-discretized data ')

if(discretized==TRUE){
  mt<-matrix(0,n.states,n.states)
   for(i in 1:(t-1)){ 
    mt<-table(x[,i],x[,i+1])+mt
	}
 }else{
   if(pool==FALSE){
    brks<-matrix(0,nrow=2,ncol=n.states+1)
    mt<-matrix(0,n.states,n.states)
	for(i in 1:(t-1)){
	  brks[1,]<-quantile(x[,i],seq(from=0,to=1,length.out=n.states+1))
	   brks[1,1]<-brks[1,1]-1/1e+10;brks[1,n.states+1]<-brks[1,n.states+1]+1/1e+10
	  brks[2,]<-quantile(x[,i+1],seq(from=0,to=1,length.out=n.states+1))
	   brks[2,1]<-brks[2,1]-1/1e+10;brks[2,n.states+1]<-brks[2,n.states+1]+1/1e+10
	 
	  x$cut0<-cut(x[,i],breaks=brks[1,],labels=stateNames,ordered=TRUE,rigth=FALSE)
	  x$cutT<-cut(x[,i+1],breaks=brks[2,],labels=stateNames,ordered=TRUE,rigth=FALSE) 
	  mt<-table(x$cut0,x$cutT)+mt
	   }
 	 }else{
   	  brks<-quantile(as.matrix(x),seq(from=0,to=1,length.out=n.states+1))
  	  brks[1]<-brks[1]-1/1e+10;brks[n.states+1]<-brks[n.states+1]+1/1e+10
  	  mt<-matrix(0,n.states,n.states)
 	   for(i in 1:(t-1)){
		x$cut0<-cut(x[,i],breaks=brks,labels=stateNames,ordered=TRUE,rigth=FALSE)
		x$cutT<-cut(x[,i+1],breaks=brks,labels=stateNames,ordered=TRUE,rigth=FALSE)
		mt<-table(x$cut0,x$cutT)+mt
		 }
        }
  }
	mp<-mt/rowSums(mt)
	class(mt)<-'matrix'
	class(mp)<-'matrix'
  z<-list('t'=mt,'p'=mp)
  z
 }
