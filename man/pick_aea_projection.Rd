% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/misc.R
\name{pick_aea_projection}
\alias{pick_aea_projection}
\title{Pick a custom Albers Equal Area projection suitable for given dataset}
\usage{
pick_aea_projection(dat, latcol = NULL, longcol = NULL)
}
\arguments{
\item{dat}{object with latitudes in \code{latcol} and longitudes in \code{longcol}.}

\item{latcol}{name of \code{dat} column containing latitudes}

\item{longcol}{name of \code{dat} column containing longitudes}
}
\value{
The PROJ4 string.
}
\description{
Pick a custom Albers Equal Area projection suitable for given dataset
}
\details{
Figures out what standard parallels to use for Albers Equal Area projection.
As per: \href{https://pro.arcgis.com/en/pro-app/latest/help/mapping/properties/albers.htm}{https://pro.arcgis.com/en/pro-app/latest/help/mapping/properties/albers.htm}
See also: \href{https://proj.org/en/6.2/operations/projections/aea.html}{https://proj.org/en/6.2/operations/projections/aea.html}

Parameters for Albers Equal Area projection:\cr
lat_1: southern standard parallel - 1/6 of latitude range north of the southern limit\cr
lat_2: northern standard parallel - 1/6 of latitude range south of the northern limit\cr
lat_0: central parallel - midpoint of latitude range\cr
lon_0: central meridian - mean of longitude range (or maybe median?)\cr
}
\author{
Dave Fifield
}
