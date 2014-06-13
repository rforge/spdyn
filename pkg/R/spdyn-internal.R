.mfpt <-
function(P){
 stopifnot(rowSums(P)==1,nrow(P)==ncol(P))
  n<-nrow(P)
    ss = .steadyState(P)
    A = matrix(rep(ss,n),n,byrow=FALSE)
    A = t(A) 
    I = diag(1,nrow(P))
    Z = solve(I - P + A)
    E = matrix(1,nrow(P),ncol(P))
    D1 = diag(1/diag(A))
    Zdg = diag(diag(Z))
    M = (I - Z + E %*% Zdg) %*% D1
   return(M)
  }
.steadyState <-
function(P,tol=1e-10){
 stopifnot(rowSums(P)==1,nrow(P)==ncol(P))
	S<-eigen(t(P))$vectors
	D<-diag(eigen(t(P))$values)
	diag(D)=as.numeric(abs(diag(D)-1)<tol)
	ss<-S%*%D%*%solve(S)%*%rep(0.2,nrow(P))
   return(ss)
  }
