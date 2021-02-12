\name{moran.scatterplot}
\alias{moran.scatterplot}

\title{
 Moran Scatter Plot
}

\description{
This function produces the moran scatterplot.
}

\usage{
moran.scatterplot(x,y,listw,CENT=mean,DESV=sd, lisa, signif, ...)
}

\arguments{
  \item{x}{ A numeric vector. 
}
  \item{y}{ A numeric vector. This is the variable to be spatially lagged. 
  If \code{x} is a numeric matrix, there is no need to supply this argument.
  If \code{x} is a column vector and \code{y} is not provided, then
   the univariate moran scatter plot is produced.
}
  \item{listw}{ A list of spatial weights as provided by \code{spdep::nb2listw} }
  \item{CENT}{ Central tendency statistic used to compute the Moran Statistic.
   Defaults to \code{mean} }
  \item{DESV}{ Dispersion statistic. Defaults to \code{sd} }
  \item{lisa}{ If an object of class \code{lisaPerm} is provided as produced by 
    \code{lisa.perm} or by \code{bilisa.perm}, then the observations (points) 
    in the moran scatterplot which belong to a significatant cluster are coloured as in
    the LISA cluster map (ie., \emph{red} for observations belonging to the \emph{High-High} cluster, 
    \emph{blue} for observations in the \emph{Low-Low} cluster, and so on). However, observations with a non-significant value of
    the LISAs are coloured in \code{grey30}. See \code{signif} argument.     
    }
  \item{signif}{ If an object of class \code{lisaPerm} is provided to the \code{lisa} argument,
    then the \code{signif} argument determines the significance threshold to determina
    significance of LISA statistics.}. 
  \item{...}{Further arguments provided to \code{plot}}

}

\details{
 The moran scatterplot provides a way to visually inspect if there is 
 spatial autocorrelation.
}


\author{
 Osmar Leandro Loaiza Quintero
}



\seealso{
  \code{\link{unimoran}, \link{unimoran.test}, \link{bimoran}, \link{bimoran.test}} 
}
\examples{

data(usinc)

require(spdep)

lw<-nb2listw(
		poly2nb(usinc,queen=TRUE),style='W'
	)

moran.scatterplot(x=usinc$X2007, y=usinc$X2009, listw=lw)


}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ moran scatterplot }% use one of  RShowDoc("KEYWORDS")

