.onLoad <- function(libname, pkgname) {
  shiny::addResourcePath(
    prefix = "img",
    directoryPath = system.file(
      "app/www/img",
      package = "chagaspathway"
    )
  )
}

.onUnload <- function(libname, pkgname) {
  shiny::removeResourcePath("img")
}


