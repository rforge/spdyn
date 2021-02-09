\name{bimoran.test}
\alias{bimoran.test}

\title{
Bivariate Global Moran Statistic permutation test
}

\description{
This function computes a conditional permutation test for the Bivariate 
Global Moran Statistic.
}

\usage{
bimoran.test(x,y=NULL,listw,CENT=mean,DESV=sd,nsim=999)
}

\arguments{
  \item{x}{ Either a numeric vector or a numeric matrix with two columns. If \code{x} has
  more than two columns, only the first two columns will be used, and the second column
  will be spatially lagged. 
}
  \item{y}{ A numeric vector. This is the variable to be spatially lagged. 
  If \code{x} is a numeric matrix, there is no need to supply this argument.
}
  \item{listw}{ A list of spatial weights as provided by \code{spdep::nb2listw} }
  \item{CENT}{ Central tendency statistic used to compute the Moran Statistic.
   Defaults to \code{mean} }
  \item{DESV}{ Dispersion statistic. Defaults to \code{sd} }
  \item{nsim}{ Number of permutations or simulations.}

}

\details{
 The \code{bimoran.test} implements a conditional permutation test to asses the 
 statistical significance of the  Bivariate Global Moran Statistic.
}

\value{
 An object of class \code{moranPerm}:
 \item{sim_statistics}{ A vector with the values of the simulated bivariate global moran statistic }
 \item{statistic}{ Value of the bivariate moran statistic as returned by the function \code{\link{bimoran}} }
 \item{pvalue}{ Pseudo p-value returned by the permutation test}
}

\references{
 Anselin, Luc (1995). Local Indicators of Spatial Association. Geographical Analysis
}
\author{
 Osmar Leandro Loaiza Quintero
}



\seealso{
  \code{\link{bilisa.perm}, \link{bimoran}, \link{unimoran.test}} 
}
\examples{

data(usinc)

require(spdep)

lw<-nb2listw(
		poly2nb(usinc,queen=TRUE),style='W'
	)

bimoran.test(x=usinc$X2007, y=usinc$X2009, listw=lw)

}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ Bivariate moran statistic }% use one of  RShowDoc("KEYWORDS")
\keyword{ Bivariate spatial autocorrelation }% __ONLY ONE__ keyword per line
\keyword{ Conditional permutation test }
