\name{bilisa.perm}
\alias{bilisa.perm}

\title{
 Bivariate Local Moran Statistic
}

\description{
 This function computes a permutation test to assess the statistical significance of the Local Moran Statistics.
}

\usage{
bilisa.perm(x,y,listw,perm=999,FUN=mean,...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ A numeric vector}
  \item{y}{ A numeric vector}
  \item{listw}{A \code{listw} object as produced by \code{\link{nb2listw}} or
  \code{\link{mat2listw}}. A list of spatial weights}
  \item{perm}{ Number of permutations}
  \item{FUN}{ By defautl \code{FUN=mean}. In general, a function to compute the central tendency of the data. It should return a scalar. See \code{notes}
  }
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
\keyword{ Bivariate Local Moran Statistic }% use one of  RShowDoc("KEYWORDS")
\keyword{ Bivariate Local Indicators of Spatial Association }% __ONLY ONE__ keyword per line
