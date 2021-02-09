\name{unimoran.test}
\alias{unimoran.test}

\title{
Univariate Global Moran Statistic permutation test
}

\description{
This function computes a conditional permutation test for the Univariate 
Global Moran Statistic.
}

\usage{
unimoran.test(x,listw,CENT=mean,DESV=sd,nsim=999)
}

\arguments{
  \item{x}{ A numeric vector.
}
  \item{listw}{ A list of spatial weights as provided by \code{spdep::nb2listw} }
  \item{CENT}{ Central tendency statistic used to compute the Moran Statistic.
   Defaults to \code{mean} }
  \item{DESV}{ Dispersion statistic. Defaults to \code{sd} }
  \item{nsim}{ Number of permutations or simulations. }

}

\details{
 The \code{unimoran.test} function computes the value of the Univariate Global Moran Statistic.
 Morevoer, it generalizes the estimation of spatial autocorrelation, as this function
allows to use alternative measures of central tendency and of dispersion. However,
it should be noted that analytical results regarding the behavior of spatial autocorrelation
are only available in the literature when the central tendency and the dispersion are measured as 
the \emph{mean} and \emph{standard deviation}, respectively.
}

\value{
 An object of class \code{moranPerm}:
 \item{sim_statistics}{ A vector with the values of the simulated univariate global moran statistic }
 \item{statistic}{ Value of the univariate moran statistic as returned by the function \code{\link{unimoran}} }
 \item{pvalue}{ Pseudo p-value returned by the permutation test}
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

unimoran.test(usinc$X2009, listw=lw)


}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ Univariate moran statistic }% use one of  RShowDoc("KEYWORDS")
\keyword{ Univariate spatial autocorrelation }% __ONLY ONE__ keyword per line
