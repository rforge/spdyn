steadyState <-
function(M,tol=1e-10){
 if(class(M)=='Markov'){
  stopifnot(rowSums(M$p)==1,nrow(M$p)==ncol(M$p)) 
    E<-as.numeric(.steadyState(M$p,tol=tol))
	names(E)<-rownames(M$p)
	}else{
  if(class(M)=='spMarkov'){
  stopifnot(dim(M$p)[1]==dim(M$p)[2],dim(M$p)[2]==dim(M$p)[3])
   E<-matrix(0,dim(M$p)[1],dim(M$p)[1])
    for(i in 1:dim(M$p)[1]){
	stopifnot(rowSums(M$p[,,i])==1)
       E[i,]<-.steadyState(M$p[,,i],tol=tol)
	colnames(E)<-colnames(M$p)
	rownames(E)<-paste('nb',rownames(M$p),sep='.')
	}	
     }else{print('wrong class')}	
    }
return(E)
  }
