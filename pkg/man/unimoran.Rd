\name{unimoran}
\alias{unimoran}

\title{
Univariate Global Moran Statistic
}

\description{
This function computes the value of the Univariate Global Moran Statistic.
}

\usage{
unimoran(x,listw,CENT=mean,DESV=sd)
}

\arguments{
  \item{x}{ A numeric vector.
}
  \item{listw}{ A list of spatial weights as provided by \code{spdep::nb2listw} }
  \item{CENT}{ Central tendency statistic used to compute the Moran Statistic.
   Defaults to \code{mean} }
  \item{DESV}{ Dispersion statistic. Defaults to \code{sd} }

}

\details{
 The \code{unimoran} function computes the value of the Univariate Global Moran Statistic.
 Morevoer, it generalizes the estimation of spatial autocorrelation, as this function
allows to use alternative measures of central tendency and of dispersion. However,
it should be noted that analytical results regarding the behavior of spatial autocorrelation
are only available in the literature when the central tendency and the dispersion are measured as 
the \emph{mean} and \emph{standard deviation}, respectively.
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

data(usinc)

require(spdep)

lw<-nb2listw(
		poly2nb(usinc,queen=TRUE),style='W'
	)
	
unimoran(usinc$X2009, listw=lw)
	
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ Univariate moran statistic }% use one of  RShowDoc("KEYWORDS")
\keyword{ Univariate spatial autocorrelation }% __ONLY ONE__ keyword per line
