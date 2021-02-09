\name{bilisa.perm}
\alias{bilisa.perm}

\title{
 Bivariate Local Moran Statistic
}

\description{
 This function computes a permutation test to assess the statistical significance of the Local Moran Statistics.
}

\usage{
bilisa.perm(x,y,listw,perm=999,FUN=mean,DESV=sd,...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ A numeric vector}
  \item{y}{ A numeric vector. This is the variable to be spatially lagged}
  \item{listw}{A \code{listw} object as produced by \code{\link{nb2listw}} or
  \code{\link{mat2listw}}. A list of spatial weights}
  \item{perm}{ Number of permutations}
  \item{FUN}{ By defautl \code{FUN=mean}. In general, a function to compute the central tendency of the data. It should return a scalar. See \code{notes}
  }
  \item{DESV}{ Dispersion statistic. Defaults to \code{sd} }
  \item{...}{ Further arguments passed to \code{FUN}}
}
\details{
 This function implements a conditional permutation test to assess the statistical significance of the bivariate LISA. This is a distribution free test that overcomes the limitations of the standard assumption of normality.
}
\value{
Returns a numeric matrix
  \item{Ii}{ Values of the Local Moran Statistics}
  \item{p-val}{ P-values}
  \item{Zi}{ Z scores or standardized scores}
}


\author{
 Osmar Leandro Loaiza Quintero
}

\note{
There is no theoretical nor simulation work assessing the behavior of the Local Moran Staistic when the measure of central tendency is different from the
  mean.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
  \code{\link{lisa.perm}, \link{plot.lisaPerm}}
}
\examples{

data(usinc)

require(spdep)

lw<-nb2listw(
		poly2nb(usinc,queen=TRUE),style='W'
	)

my_bilisa <- bilisa.perm(x=usinc$X1929, y=usinc$X2009, listw=lw)


plot(my_bilisa, usinc)
title(sub='State per capita income in 1929 vs. spatially lagged state per capita income in 2009')

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ Bivariate Local Moran Statistic }% use one of  RShowDoc("KEYWORDS")
\keyword{ Bivariate Local Indicators of Spatial Association }% __ONLY ONE__ keyword per line
