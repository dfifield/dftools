#' Create common project folders
#'
#' @returns Nothing
#' @export
#'
create_project_folders <- function() {
  dir.create(here::here("R"))
  dir.create(here::here("Results"))
  dir.create(here::here("GIS/Shapefiles"), recursive = TRUE)
  dir.create(here::here("GIS/Maps"))
  dir.create(here::here("Data/Raw"), recursive = TRUE)
  dir.create(here::here("Data/Generated"))
  dir.create(here::here("Literature"))
  dir.create(here::here("Tests"))
}

#' Setup a new project
#'
#' @returns Nothing
#' @export
#'
setup_project <- function(use.renv = TRUE) {
  create_project_folders()
  if (use.renv == TRUE) {
    renv::activate()
    renv::install(c("dftools", "tidyverse", "usethis", "devtools"))
    renv::snapshot()
  }
}
