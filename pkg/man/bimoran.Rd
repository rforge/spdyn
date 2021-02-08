\name{bimoran}
\alias{bimoran}

\title{
Bivariate Global Moran Statistic
}

\description{
This function computes the value of the Bivariate Global Moran Statistic
}

\usage{
bimoran(x,y=NULL,listw,perm=999,CENT=mean,DESV=sd)
}

\arguments{
  \item{x}{ Either a numeric vector or a numeric matrix with two columns. If \code{x} has
  more than two columns, only the first two columns will be used. 
}
  \item{y}{ A numeric vector. If \code{x} is a numeric matrix, there is no need
  to supply this argument.
}
  \item{listw}{ A list of spatial weights as provided by \code{spdep::nb2listw} }
  \item{perm}{ Number of permutations.}
  \item{CENT}{ Central tendency statistic used to compute the Moran Statistic.
   Defaults to \code{mean} }
  \item{DESV}{ Dispersion statistic. Defaults to \code{sd} }

}

\details{
 The \code{bimoran} function computes the value of the Bivariate Global Moran Statistic.
}

\references{
 Anselin, Luc (1995). Local Indicators of Spatial Association. Geographical Analysis
}
\author{
 Osmar Leandro Loaiza Quintero
}



\seealso{
  \code{\link{bilisa.perm}, \link{unimoran}, \link{bimoran.test}} 
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (x) 
{
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ Bivariate moran statistic }% use one of  RShowDoc("KEYWORDS")
\keyword{ Bivariate spatial autocorrelation }% __ONLY ONE__ keyword per line
