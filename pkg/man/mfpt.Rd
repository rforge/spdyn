\name{mfpt}
\alias{mfpt}

\title{Mean First Passage Times
}
\description{
Computes mean first passage times among states for a first order Markov process.
}
\usage{
mfpt(P)
}

\arguments{
  \item{P}{A markov probability matrix, as given by the \code{\link{markov}} function.
}
}
\details{
The mean first passage times matrix is estimated following the standard expression as found in Meyer (1978, p. 41).
The mean first passage time refers to the time-steps it takes to reach state \eqn{Si} for the first time, given the 
initial state \eqn{Sj}.
}
\value{
A matrix
}
\references{
Meyer, Carl (1978) 'An  Alternative  Expression  for  the  Mean  First  Passage  Matrix', Linear Algebra and its Applications,Vol.22,pp.41-47.
}
\author{
Osmar Leandro Loaiza Quintero
}

\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

}

\keyword{ Speed of convergence }
\keyword{ Transition times}
