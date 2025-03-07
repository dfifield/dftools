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
