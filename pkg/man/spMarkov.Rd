\name{spMarkov}
\alias{spMarkov}
\alias{spInitState}

\title{
Spatial Markov Matrices
}
\description{
A function to calculate Spatial Markov Matrices as proposed by Rey (2001).
}
\usage{
spMarkov(data, listw, stateVars, n.states, stateNames, style = "quantile", breaks, breaks.lag, pool = TRUE, std = TRUE)

spInitState(data,listw,stateVars,n.states,stateNames=NULL,style='quantile',
breaks,breaks.lag,pool=TRUE,std=TRUE)

}


\arguments{
  \item{data}{
A data frame arranged in such a way that there is one row per observation, and one column
 recording the state of each observation, with as many columns as time periods.
}
  \item{listw}{
A list of spatial weights, as given by the \pkg{spdep} package (see: \link[spdep]{mat2listw}, \link[spdep]{nb2listw}).
}
  \item{stateVars}{
The columns in the data frame which describe the temporal evolution of the variable of interest (one column per year).
}
  \item{n.states}{
The number of states to be assumed for the Markov process.
}
  \item{stateNames}{
The names to be given to the states of the Markov process.
}
  \item{style}{
The type of intervals or breaks to be computed. Currently, two options are allowed: 'quantile' breaks, or 'fixed'
 breaks. In the latter case, the breaks should be provided by the user.
}
  \item{breaks}{
If \code{style='fixed'}, the breaks to be provided by the user to discretize the data.
}
  \item{breaks.lag}{
If \code{style='fixed'}, user provided breaks for the spatial lag.
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
\details{
A spatial Markov matrix is a slight modification of the traditional Markov matrix, that conditions a region's transition
 probabilities on the initial state of its spatial lag, namely, on the initial state of its neighbours.

Note that unlike the \code{\link{markov}} function, currently \code{spMarkov} does not admit already discretized data.
 This is because to determine the spatial lag, a continous variable is required.
 
 The function '\code{spMarkov}' returns a spatial markov matrix. The function
'\code{spInitStates}' retrieves the initial states of the spatial markov process.
}


\value{
An object of class '\code{spMarkov}' containing two arrays of dimensions \code{k*k*k}, where \code{k=n.states}.
 \item{t}{An array of transition counts}
 \item{p}{An array of transition probabilities}

Note that each array can be interpreted as an object containing \code{k} matrices of dimensions \code{k*k}.
}
\references{
Rey, Sergio (2001). 'Spatial Empirics for Economic Growth and Convergence', \emph{Geographical Analysis}, Vol.33, No.3.
}
\author{
Osmar Leandro Loaiza Quintero
}

\seealso{
 \code{\link{steadyState}, \link{mfpt}, \link[spdep]{nb2listw}, \link[spdep]{poly2nb}}
}
\examples{

data(usinc)
stateVars<-names(usinc@data[,7:87])
stateNames<-c('Poor','Lower','Middle','Upper','Rich')

#Create a list of spatial weights
lw<-nb2listw(poly2nb(usinc,queen=TRUE),style='W')

M<-spMarkov(usinc@data, lw, stateVars=stateVars,n.states=5,stateNames=stateNames,pool=TRUE,std=TRUE)
M$t;M$p

}

\keyword{ Spatial Markov Matrices }
\keyword{ spatially conditioned transition matrices }
