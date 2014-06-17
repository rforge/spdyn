spMarkov <-
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
  	
	 mt<-array(0,dim=rep(n.states,3))
	for(i in 1:(t-1)){
	  brks[1,]<-quantile(ly[,i],seq(from=0,to=1,length.out=n.states+1))
	   brks[1,1]<-brks[1,1]-brks[1,1]/1e+10;brks[1,n.states+1]<-brks[1,n.states+1]+brks[1,n.states+1]/1e+10
	  brks[2,]<-quantile(x[,i],seq(from=0,to=1,length.out=n.states+1))
	   brks[2,1]<-brks[2,1]-brks[2,1]/1e+10;brks[2,n.states+1]<-brks[2,n.states+1]+brks[2,n.states+1]/1e+10
	  brks[3,]<-quantile(x[,i+1],seq(from=0,to=1,length.out=n.states+1))
	   brks[3,1]<-brks[3,1]-brks[3,1]/1e+10;brks[3,n.states+1]<-brks[3,n.states+1]+brks[3,n.states+1]/1e+10
	
	x$cutL<-cut(ly[,i],breaks=brks[1,],labels=stateNames,ordered=TRUE,rigth=FALSE)
	x$cut0<-cut(x[,i],breaks=brks[2,],labels=stateNames,ordered=TRUE,rigth=FALSE)
	x$cutT<-cut(x[,i+1],breaks=brks[3,],labels=stateNames,ordered=TRUE,rigth=FALSE) 
	mt<-table(x[,c('cut0','cutT','cutL')])+mt
	 }
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
  	 mt<-array(0,dim=rep(n.states,3))
for(i in 1:(t-1)){
x$cutL<-cut(ly[,i],breaks=brks.ly,labels=stateNames,ordered=TRUE,rigth=FALSE)
x$cut0<-cut(x[,i],breaks=brks,labels=stateNames,ordered=TRUE,rigth=FALSE)
x$cutT<-cut(x[,i+1],breaks=brks,labels=stateNames,ordered=TRUE,rigth=FALSE) 
mt<-table(x[,c('cut0','cutT','cutL')])+mt
 }
}
mp<-array(0,dim=rep(n.states,3))
for(i in 1:n.states)mp[,,i]<-mt[,,i]/rowSums(mt[,,i])
 mp[is.na(mp)]<-0
  class(mt)<-'array'
  class(mp)<-'array'

dimnames(mp)<-list(stateNames,stateNames,paste('neighbourhood:',stateNames))
  z<-list('t'=mt,'p'=mp)
  class(z)<-'spMarkov'
 return(z)
 }
