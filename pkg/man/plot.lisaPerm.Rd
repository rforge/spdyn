\name{plot.lisaPerm}
\alias{plot.lisaPerm}

\title{
Cluster Map
}

\description{
Based on the Local Moran Statistic, this function plots a clusters' map
}

\usage{
\method{plot}{lisaPerm}(lmoran,shape,signif=.05,legend.title='',lty=1,lwd=1,fillBorder='black',box.lwd=1)
}

\arguments{
  \item{lmoran}{ An object of class \code{lisaPerm} as returned by the functions \code{lisa.perm} or \code{bilisa.perm}
}
  \item{shape}{ A Spatial object supported by the \code{sp} package. Usually,
    an object of class \code{SpatialPolygons} or \code{SpatialPolygonsDataFrame}
}
  \item{signif}{ Significance cut off value}
  \item{legend.title}{ Title of the legend key}
  \item{lty}{ see \code{\link{par}}}
  \item{lwd}{ see \code{\link{par}}}
  \item{fillBorder}{ Color of the border line}
  \item{box.lwd}{ Line width of the outline box}
}

\details{
 This function plots a cluster map.
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
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
