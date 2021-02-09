\name{print.moranPerm}
\alias{print.moranPerm}

\title{
 Print method for \code{moranPerm} object.
}

\description{
 Print method for \code{moranPerm} object.     
}

\usage{
\method{print}{moranPerm}(x, ...)
}

\arguments{
  \item{x}{An object of class \code{moranPerm}, as returned by \code{unimoran.test} or
  \code{bimoran.test}.
}
  \item{...}{Further arguments passed to \code{print}}
}



\references{
 Anselin, Luc (1995). Local Indicators of Spatial Association. Geographical Analysis
}
\author{
 Osmar Leandro Loaiza Quintero
}



\seealso{
  \code{\link{lisa.perm}, \link{bilisa.perm}} 
}
\examples{

data(usinc)

require(spdep)

lw<-nb2listw(
		poly2nb(usinc,queen=TRUE),style='W'
	)

mytest <- unimoran.test(usinc$X2009, listw=lw)

print(mytest)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ Local indicators of spatial association }% use one of  RShowDoc("KEYWORDS")
\keyword{ Hot spot analysis }% __ONLY ONE__ keyword per line
