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
steadyState(P, tol = 1e-10)
}

\arguments{
  \item{P}{
A probability matrix as given by the \link{markov} function.
}
  \item{tol}{
A tolerance argument, used in determining the steady state distribution.
}
}
\details{
If necessary, more details than the description above
}
\value{
A vector
}
\references{
Restrepo, Franco and Muñoz (2010) Algebra Lineal con Aplicaciones. Universidad Nacional de Colombia.
}
\author{
Osmar Leandro Loaiza Quintero
}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{markov}}
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.


}

\keyword{ Ergodico distribution }
\keyword{ Steady state distribution vector }
