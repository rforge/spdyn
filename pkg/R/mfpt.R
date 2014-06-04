mfpt<-function(P){
 stopifnot(rowSums(P)==1,nrow(P)==ncol(P))
  n<-nrow(P)
    ss = steadyState(P)
    A = matrix(rep(ss,n),n,byrow=FALSE)
    A = t(A) 
    I = diag(1,nrow(P))
    Z = solve(I - P + A)
    E = matrix(1,nrow(P),ncol(P))
    D1 = diag(1/diag(A))
    Zdg = diag(diag(Z))
    M = (I - Z + E %*% Zdg) %*% D1
   colnames(M)<-colnames(P)
   rownames(M)<-rownames(P)
   return(M)
  }
