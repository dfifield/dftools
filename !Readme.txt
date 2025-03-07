In addition to installing the packages with CTRL-SHIFT-B, I should also
install it into the local package cellar for use by renv with:

devtools::build(path = "C:/Users/fifieldd/Documents/Offline/R/Local_package_cellar", binary = TRUE)

Alternatively, you can install from GitHub with:

devtools::install_github("dfifield/dftools")
