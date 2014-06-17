\name{markov}
\alias{markov}

\title{
Classic Markov Probability Matrix
}
\description{
A function to estimate probability or transition matrices.
}
\usage{
markov(data, stateVars, n.states, stateNames, discretized = FALSE, pool = FALSE)
}

\arguments{
  \item{data}{
A data frame arranged in such a way that there is one row per observation, and one column
 recording the state of each observation, with as many columns as time periods.
}
  \item{stateVars}{
The columns in the data frame which describe the temporal evolution of the variable of interest (one column per year).
}
  \item{n.states}{
The number of states to be assumed for the Markov process (equals the number of rows and columns of the estimated
 transition matrix).
}
  \item{stateNames}{
The names to be given to the states of the Markov process.
}
  \item{discretized}{
Is the data already discretized? (Default: \code{discretized=FALSE}). If the user has a data set which registers a finite
 number of states through time, \code{discretized} should be set as \code{TRUE}. If the data set comprises a continous
 variable, data should be discretized, whereby \code{discretized} should be set as FALSE.
}
  \item{pool}{
If \code{pool=TRUE}, the data is pooled, so that a unique set of time-invariant breaks is determined. If \code{pool=FALSE}
 (the default), each variable is discretized using time-variant breaks (i.e. a different set of breaks for each time period).
 If \code{pool=TRUE}, it is recommended to set \code{std=TRUE}.
}
  \item{std}{
Should the data be standardized? If \code{std=TRUE}, the data is standardized by each period by the mean. 
}
}

\value{
 An object of class '\code{markov}' containing the following items:
  \item{t}{A matrix of transition counts.
}
  \item{p}{A probability matrix. The rows of this matrix sum to one.
}
}
\references{
Restrepo, Patricia; Franco, Rosa and Muñoz, Luz (2010). \emph{Álgebra Lineal con Aplicaciones}, Universidad Nacional de Colombia, Medellín.
}
\note{
If \code{pool=TRUE}, it is recommended to set \code{std=TRUE}.
}
\author{
Osmar Leandro Loaiza Quintero
}


\seealso{
\code{\link{steadyState}, \link{mfpt}, \link{spMarkov}}
}

\examples{

data(usinc)
stateVars<-names(usinc@data[,7:87])
stateNames<-c('Poor','Lower','Middle','Upper','Rich')

M<-markov(usinc@data, stateVars=stateVars,n.states=5,stateNames=stateNames)
M$t;M$p

}

\keyword{ markov chain }
\keyword{ probability matrix }
