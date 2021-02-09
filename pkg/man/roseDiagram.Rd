\name{roseDiagram}
\alias{roseDiagram}

\title{
Rose diagram
}

\description{
 Rose diagram to assess the spatial changes of a variable throught time.
}

\usage{
roseDiagram(t1,t2,listw)
}

\arguments{
  \item{t1}{ A numeric vector. The value of the variable of interest at the \emph{beginning} of the time period.
 }
  \item{t2}{ A numeric vector. The value of the variable of interest at the beginning of the \emph{end} time period.
 }
 \item{listw}{ List of spatial weights as returned by \code{\link{nb2listw}} 
 }

}

\details{
 This function plots a Rose Diagram as suggested by Ray, Murray and Anselin (2010), in order
to assess changes in the spatial distribution of a variable between two distinct time periods.
}

\references{
 Rey, Sergio; Murray, Alan and Anselin, Luc (2011). "Visualizing regional income distribution dynamics." \emph{Lett Spat Resour Sci} 4:81-90. DOI 10.1007/s12076-010-0048-2
}

\author{
 Osmar Leandro Loaiza Quintero
}



\seealso{
  \code{\link{lisa.perm}, \link{bilisa.perm}} 
}
\examples{

data(usinc, package="spdyn")

require(spdep)
nb <- poly2nb(usinc,queen=TRUE)
nbw <- nb2listw(nb)

roseDiagram(t1=usinc$X2007, t2=usinc$X2009, listw=nbw)
 

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ Local indicators of spatial association }% use one of  RShowDoc("KEYWORDS")
\keyword{ Rose diagram }% __ONLY ONE__ keyword per line
\keyword{ Directional statistics}
\keyword{ Spatial dynamics}
