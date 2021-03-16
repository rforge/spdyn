markov <-
    function(data,stateVars,n.states,stateNames=NULL,discretized=FALSE,pool=FALSE,std=FALSE,balanced=TRUE){
		
        x<-data[,stateVars]
        n<-nrow(x)
        t<-ncol(x)
        
        if(is.null(stateNames)){stateNames = seq(from=1, to=n.states,length.out=n.states)}
        if(std==TRUE){ for(i in 1:t)x[,i] <- x[,i]/mean(x[,i],na.rm=TRUE) }
        if(discretized==TRUE & pool==TRUE) stop('discretized==TRUE & pool==TRUE: pool is an option available only for non-discretized data ')
        if(balanced==TRUE){ na.rm <- FALSE }else{ na.rm <- TRUE }

        if(discretized==TRUE){
			
            mt <- matrix(0,n.states,n.states)
            
            for(i in 1:(t-1)){ 
				
                mt <- table(x[,i],x[,i+1])+ mt # This  line can be regarded as duplicated. 
                 # I could try to make an internal function in its place
            }
            
        }else{
			
            if(pool==FALSE){
				
				# As data is not pooled, it is required to compute period
				# specific breaks.
				# Hence, create a two row matrix to save breaks.
				# In row 1 store breaks for time t,
				# and in row 2 for time t+1
                brks<-matrix(0,nrow=2,ncol=n.states+1)
                mt<-matrix(0,n.states,n.states)
                
                for(i in 1:(t-1)){
					
					# In row 1 save breaks in time t
                    brks[1,] <- quantile(x[,i],seq(from=0,to=1,length.out=n.states+1),na.rm=na.rm)
                    
                    # These two lines are kind of a trick.
                    # When we pass the breaks to the cut command, we need 
                    # to be sure that the minimum and maximum values #
                    # are not excluded. So, for instance, we set the upper 
                    # value of the brks vectors a litte above the empirical maximum.                   
                    brks[1,1] <- brks[1,1]-1/1e+10
                    brks[1,n.states+1] <- brks[1,n.states+1]+1/1e+10  
                    
                    # In row 2 save breaks in time t+1 
                    brks[2,]<-quantile(x[,i+1],seq(from=0,to=1,length.out=n.states+1),na.rm=na.rm)
                    
                    brks[2,1]<-brks[2,1]-1/1e+10
                    brks[2,n.states+1]<-brks[2,n.states+1]+1/1e+10
                    
                    x$cut0<-cut(x[,i],breaks=brks[1,],labels=stateNames,ordered=TRUE,rigth=FALSE)
                    x$cutT<-cut(x[,i+1],breaks=brks[2,],labels=stateNames,ordered=TRUE,rigth=FALSE) 
                    mt<-table(x$cut0,x$cutT)+mt # This  line can be regarded as duplicated. 
                }
            }else{
				
				# As data is pooled, breaks are common to all time periods
				# So there is no need for time specific breaks
                brks <- quantile(as.matrix(x),seq(from=0,to=1,length.out=n.states+1),na.rm=na.rm)
                
                brks[1] <- brks[1]-1/1e+10
                brks[n.states+1] <- brks[n.states+1]+1/1e+10
                
                mt<-matrix(0,n.states,n.states)
                
                for(i in 1:(t-1)){
					
                    x$cut0<-cut(x[,i],breaks=brks,labels=stateNames,ordered=TRUE,rigth=FALSE)
                    x$cutT<-cut(x[,i+1],breaks=brks,labels=stateNames,ordered=TRUE,rigth=FALSE)
                    mt<-table(x$cut0,x$cutT)+mt # This  line can be regarded as duplicated. 
                }
            }
        }
        
        mp <- mt/rowSums(mt)
        class(mt) <- 'matrix'
        class(mp) <- 'matrix'
        z <- list('t'=mt,'p'=mp)
        class(z) <- 'Markov'
        return(z)
    }

# I've got to unify the argments to markov() and spMarkov() functions.
#  Arguments to unify:
#  - discretized / spMarkov() cannot be descretized, because the spatial lag requires a numeric variable.
#  - style 
#  - balanced 
