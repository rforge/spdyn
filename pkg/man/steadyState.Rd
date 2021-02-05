\name{steadyState}
\alias{steadyState}

\title{
Steady State
}
\description{
A function to estimate the ergodic distribution, or steady state distribution vector, of a regular first order 
 markov process.
}
\usage{
steadyState(M, tol = 1e-10)
}

\arguments{
  \item{M}{
An object of class \code{markov} or class \code{spMarkov}.
}
  \item{tol}{
A tolerance argument, used in determining the steady state distribution. By default set to 1e-100.
}
}
\details{
The long run behavior of a first order Markov process is governed by the unitary eigenvalues associated with the 
probability matrix. If an eigenvalue does not equal unity, but is very close to unity, the tolerance argument specifies
the maximum gap to be tolerated to regard an eigenvalue as equal to unity. Then this eigenvalue will govern the long run
behavior of the Markov process and, hence, will determine the steady state vector or ergodic distribution. The \code{steadyState}
function uses the \code{\link[base]{eigen}} function in the \pkg{base} package.
}
\value{
If an object of class \code{markov} is provided, it returns a vector. If an objecto of class \code{spMarkov} is provided,
 it returns a matrix.
}
\references{
Restrepo, Patricia; Franco, Rosa and Munoz, Luz (2010). \emph{Algebra Lineal con Aplicaciones}, Universidad Nacional de Colombia, Medellin.
}
\author{
Osmar Leandro Loaiza Quintero
}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
 \code{\link{spMarkov}, \link{markov}, \link{eigen}}
}
\examples{

data(usinc)
stateVars<-names(usinc@data[,7:87])
stateNames<-c('Poor','Lower','Middle','Upper','Rich')

##Classic Markov Matrix
Mc<-markov(usinc@data, stateVars=stateVars,n.states=5,stateNames=stateNames)
steadyState(Mc)

##Spatial Markov Matrix
#Create a list of spatial weights
require(spdep)
lw<-nb2listw(poly2nb(usinc,queen=TRUE),style='W')

Msp<-spMarkov(usinc@data, lw, stateVars=stateVars,
n.states=5,stateNames=stateNames,
pool=TRUE,std=TRUE)

steadyState(Msp)

}

\keyword{ Ergodico distribution }
\keyword{ Steady state distribution vector }
