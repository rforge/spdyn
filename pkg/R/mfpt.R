mfpt <-
    function(M,tol=1e-10){
        if(class(M)=='Markov'){
            stopifnot(abs(rowSums(M$p)-1 )<tol, nrow(M$p)==ncol(M$p)) 
            T<-.mfpt(M$p, tol)
            colnames(T)<-colnames(M$p)
            rownames(T)<-rownames(M$p)
        }else{
            if(class(M)=='spMarkov'){
                stopifnot(dim(M$p)[1]==dim(M$p)[2],dim(M$p)[2]==dim(M$p)[3])
                T<-array(0,dim=rep(dim(M$p)[1],3))
                for(i in 1:dim(M$p)[1]){
                    stopifnot( abs(rowSums(M$p[,,i])-1 )<tol)
                    T[,,i]<-.mfpt(M$p[,,i], tol)
                    dimnames(T)<-list(dimnames(M$p)[[1]],dimnames(M$p)[[2]],dimnames(M$p)[[3]])  
                }
            }else{print('wrong class')}	
        }
        return(T)
    }
