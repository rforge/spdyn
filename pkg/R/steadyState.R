steadyState<-function(P,tol=1e-10){
 stopifnot(rowSums(P)==1,nrow(P)==ncol(P))
	S<-eigen(t(P))$vectors
	D<-diag(eigen(t(P))$values)
	diag(D)=as.numeric(abs(diag(D)-1)<tol)
	ss<-S%*%D%*%solve(S)%*%rep(0.2,nrow(P))
   colnames(ss)<-'Steady State'
   rownames(ss)<-rownames(P)
   return(ss)
  }
