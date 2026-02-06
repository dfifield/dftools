#' Create common project folders
#'
#' @returns Nothing
#' @author Dave Fifield
create_project_folders <- function() {
  folders <- c(
    "R",
    "Results",
    "GIS/Shapefiles",
    "GIS/Maps",
    "GIS/Rasters",
    "Data/Raw",
    "Data/Generated",
    "Literature",
    "Tests"
  )
  message("\nCreating folders: ", paste(folders, collapse = ", "))
  sapply(folders, dir.create, recursive = TRUE)
}

#' Install standard set of packages
#'
#' @returns noting
#' @author Dave Fifield
install_standard_packages <- function() {
  packages <- c("here", "tidyverse", "usethis", "devtools")
  message("Installing packages: ", paste(packages, collapse = ", "))
  install.packages(setdiff(packages, rownames(installed.packages())))
}

#' Setup a new project
#'
#' @param create.folders \[Boolean, default TRUE\] Should folder hierarchy be
#'  created. The following folders will be created under the project root:
#'      "R", "Results", "GIS/Shapefiles", "GIS/Maps", "Data/Raw", "Data/Generated",
#'      "Literature", "Tests"
#'
#' @param install.pkgs \[Boolean, default TRUE\] Should standard set of packages
#'  be installed: The following packages will be installed: "here", "tidyverse",
#'  "usethis", "devtools".
#'
#' @param use.renv \[Boolean, default TRUE\] Indicates whether \code{renv} is in
#'  use with this project. If \code{TRUE} and \code{install.packages}
#'  is \code{TRUE}, then \link{renv::snapshot()} will be called after installing
#'  packages.
#'
#' @returns Nothing
#' @export
#' @author Dave Fifield
setup_project <- function(create.folders = TRUE,
                          install.pkgs = TRUE,
                          use.renv = TRUE) {

  old <- options("warn")$warn
  options(warn = 1)

  # Install basic packages
  if (install.pkgs)
    install_standard_packages()

  # Create project folder hierarchy
  if(create.folders)
      create_project_folders()

  if(use.renv && install.pkgs) {
    message("\nUpdating renv snapshot")
    renv::snapshot()
  }

  options(warn = old)
}



#' Plot an sf object on a world map
#'
#' @param dat object to plot. Must inherit from class \code{sf} or be something
#'     that can be passed as the first argument to \link[sf]{st_as_sf} in which
#'     case \code{xcol} and \code{ycol} must be the names of columns containing
#'     the x- and y-coordinates, respectively.
#' @param xcol  name of \code{dat} column containing x-coordinates if \code{dat}
#'     does not inherit from class \code{sf}
#' @param ycol  name of \code{dat} column containing y-coordinates if \code{dat}
#'     does not inherit from class \code{sf}
#' @param proj  projection in a format that can be passed to the \code{crs}
#'     argument of \link[sf]{st_as_sf}. Only needed if \code{dat} does not
#'     inherit from \code{sf}.
#' @param title title of the plot, suitable for passing to \link[ggplot2]{ggtitle}.
#'
#' @details A simple function to plot an \code{sf} object with \link[ggplot2]{ggplot}
#' using \link[ggplot2]{geom_sf} on a basemap from \link[rnaturalearth]{ne_countries}.
#' The \code{x-lim} and \code{y-lim} of the plot will be set to the bounding box
#' of \code{dat}.
#'
#' @returns The plot, invisibly.
#' @export
#'
#' @author Dave Fifield
plot_sf_obj <- function(dat, xcol = NULL, ycol = NULL, proj = 4326, title = NULL) {

  # Make sure dat is sf
  if(!inherits(dat, "sf")) {
    if (is.null(xcol) || is.null(ycol))
      stop("plot_sf: error - dat is not sf class and one of xcol or ycol is NULL")

    dat.sf <- sf::st_as_sf(dat, coords = c(xcol, ycol), crs = sf::st_crs(proj))
  } else
    dat.sf <- dat

  worldsf <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
  g <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = dat.sf,
                     size = 0.05,
                     color = "red") +
    ggplot2::geom_sf(data = worldsf,
                     size = 0.05,
                     fill = "grey20") +
    ggplot2::theme_bw() +
    # coord_sf()
    ggplot2::coord_sf(
      xlim = c(sf::st_bbox(dat.sf)$xmin, sf::st_bbox(dat.sf)$xmax),
      ylim = c(sf::st_bbox(dat.sf)$ymin, sf::st_bbox(dat.sf)$ymax),
      expand = TRUE
    )

  if (!is.null(title))
    g <- g + ggtitle(title)

  print(g)
  invisible(g)
}


#' Pick a custom Albers Equal Area projection suitable for given dataset
#'
#' @param dat object with latitudes in \code{latcol} and longitudes in \code{longcol}.
#' @param latcol  name of \code{dat} column containing latitudes
#' @param longcol  name of \code{dat} column containing longitudes
#' @param units units for the projection. Choices are "m", "km", or "kmi" for
#'  meters, kilometers,  and international nautical miles respectively
#' @param type type of formatting for the returned projection string
#'
#'@details Figures out what standard parallels to use for Albers Equal Area projection.
#' As per: \href{https://pro.arcgis.com/en/pro-app/latest/help/mapping/properties/albers.htm}{https://pro.arcgis.com/en/pro-app/latest/help/mapping/properties/albers.htm}
#' See also: \href{https://proj.org/en/6.2/operations/projections/aea.html}{https://proj.org/en/6.2/operations/projections/aea.html}
#'
#' Parameters for Albers Equal Area projection:\cr
#' lat_1: southern standard parallel - 1/6 of latitude range north of the southern limit\cr
#' lat_2: northern standard parallel - 1/6 of latitude range south of the northern limit\cr
#' lat_0: central parallel - midpoint of latitude range\cr
#' lon_0: central meridian - mean of longitude range (or maybe median?)\cr
#'
#' @returns The projection description string in the chosen format.
#' @export
#'
pick_aea_projection <- function(dat,
                                latcol = "lat",
                                longcol = "lon",
                                units = c("m", "km", "kmi"),
                                type = c("PROJ4", "WKT2")
                                ) {
  units <- match.arg(units)
  type <- match.arg(type)
  dat <- sf::st_drop_geometry(dat)
  y_range <- (abs(range(dat[, latcol])[1] - range(dat[, latcol])[2]))
  lat_1 <- min(dat[, latcol]) + y_range/6
  lat_2 <- max(dat[, latcol]) - y_range/6
  lat_0 <- mean(range(dat[, latcol]))
  lon_0 <- mean(range(dat[, longcol]))

  # Create projection object
  prjstring <-  sprintf(
    "+proj=aea +lat_1=%g +lat_2=%g +lat_0=%g +lon_0=%g +x_0=0 +y_0=0 +ellps=WGS84 +units=%s +no_defs",
    lat_1,
    lat_2,
    lat_0,
    lon_0,
    units
  )

  # Return projection in chosen format.
  if( type == "PROJ4") {
    prjstring
  } else {
    sf::st_crs(prjstring)$wkt
  }
}


#' Tell if a number is odd.
#'
#' @param num Number to check
#'
#' @returns \code{TRUE} if \code{num} is odd, \code{FALSE} otherwise.
#' @export
#'@author Dave Fifield
is_odd <- function(num) {
  !((num %% 2) == 0)
}

#' Tell if a number is even
#'
#' @param num Number to check
#'
#' @returns \code{TRUE} if \code{num} is even, \code{FALSE} otherwise.
#' @export
#'@author Dave Fifield
is_even <- function(num) {
  ((num %% 2) == 0)
}


#' Return sorted names of an object
#'
#' @param obj Object to get names of
#'
#' @returns Sorted vector of object names.
#' @export
#'
#'@author Dave Fifield
namesort <- function(obj) {
  names(obj) %>% sort
}
