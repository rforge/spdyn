\name{mfpt}
\alias{mfpt}

\title{Mean First Passage Times
}
\description{
Computes mean first passage times among states for a first order Markov process.
}
\usage{
mfpt(M, tol = 1e-10)
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
The mean first passage times matrix is estimated following the standard expression as found in Meyer (1978, p. 41).
The mean first passage time refers to the time-steps it takes to reach state \eqn{Si} for the first time, given the 
initial state \eqn{Sj}.
}
\value{
If an object of class \code{markov} is provided, it returns a vector. If an object of class \code{spMarkov} is provided, it returns a matrix.
}
\references{
Meyer, Carl (1978). 'An  Alternative  Expression  for  the  Mean  First  Passage  Matrix', \emph{Linear Algebra and its Applications}, Vol.22,pp.41-47.
}
\author{
Osmar Leandro Loaiza Quintero
}

\seealso{
 \code{\link{spMarkov},\link{markov}}
}
\examples{

data(usinc)
stateVars<-names(usinc@data[,7:87])
stateNames<-c('Poor','Lower','Middle','Upper','Rich')

##Classic Markov Matrix
Mc<-markov(usinc@data, stateVars=stateVars,n.states=5,stateNames=stateNames)
mfpt(Mc)

##Spatial Markov Matrix
#Create a list of spatial weights
require(spdep)
lw<-nb2listw(poly2nb(usinc,queen=TRUE),style='W')

Msp<-spMarkov(usinc@data, lw, stateVars=stateVars,
n.states=5,stateNames=stateNames,
pool=TRUE,std=TRUE)

mfpt(Msp)

}

\keyword{ Speed of convergence }
\keyword{ Transition times}
