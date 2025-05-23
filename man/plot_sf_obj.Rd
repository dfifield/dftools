% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/misc.R
\name{plot_sf_obj}
\alias{plot_sf_obj}
\title{Plot an sf object on a world map}
\usage{
plot_sf_obj(dat, xcol = NULL, ycol = NULL, proj = 4326, title = NULL)
}
\arguments{
\item{dat}{object to plot. Must inherit from class \code{sf} or be something
that can be passed as the first argument to \link[sf]{st_as_sf} in which
case \code{xcol} and \code{ycol} must be the names of columns containing
the x- and y-coordinates, respectively.}

\item{xcol}{name of \code{dat} column containing x-coordinates if \code{dat}
does not inherit from class \code{sf}}

\item{ycol}{name of \code{dat} column containing y-coordinates if \code{dat}
does not inherit from class \code{sf}}

\item{proj}{projection in a format that can be passed to the \code{crs}
argument of \link[sf]{st_as_sf}. Only needed if \code{dat} does not
inherit from \code{sf}.}

\item{title}{title of the plot, suitable for passing to \link[ggplot2]{ggtitle}.}
}
\value{
The plot, invisibly.
}
\description{
Plot an sf object on a world map
}
\details{
A simple function to plot an \code{sf} object with \link[ggplot2]{ggplot}
using \link[ggplot2]{geom_sf} on a basemap from \link[rnaturalearth]{ne_countries}.
The \code{x-lim} and \code{y-lim} of the plot will be set to the bounding box
of \code{dat}.
}
\author{
Dave Fifield
}
