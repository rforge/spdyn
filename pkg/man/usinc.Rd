\name{usinc}
\alias{usinc}
\docType{data}
\title{
United States coterminous states per capita income, 1929:2009
}
\description{
A spatial longitudinal data set of United States fourty-eight conterminous states personal income, from 1929 to 2009.
}
\usage{data(usinc)}

\format{
A \code{SpatialPolygonsDataFrame}
}

\details{
  The slot \code{data} contains a longitudinal or
panel data frame in the wide format.
 
 \itemize{
\item{STATE_FIPS}{  : A numeric vector. US states numerical identifier. }
\item{AREA}{   A numeric vector. Area of the polygons. }
\item{PERIMETER}{  : A numeric vector. Perimeter of the polygons. }
\item{STATE_NAME}{  : A factor. state name. }
\item{SUB_REGION}{  : A factor. state region. }
\item{STATE_ABBR}{  : A factor. state name abbreviation. }
\item{X1929}{  : A numeric vector. states's per capita income in 1929}
\item{...}{}
\item{X2009}{  : A numeric vector. states's per capita income in 2009}
 }
}

\source{
Bureau of Economic Analysis, 2010. State Personal Income. http://www.bea.gov/newsreleases/regional/spi/sqpi_newsrelease.htm
}

\examples{
#data(usinc,package='spdyn')
#slotNames(usinc); names(usinc@data) ; plot(usinc)
}
\keyword{datasets}
