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


#
#

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
plot_sf <- function(dat, xcol = NULL, ycol = NULL, proj = 4326, title = NULL) {

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

