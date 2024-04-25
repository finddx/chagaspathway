.onLoad <- function(libname, pkgname) {
  shiny::addResourcePath(
    prefix = "www",
    directoryPath = system.file(
      "app/www",
      package = "chagaspathway"
    )
  )
}

.onUnload <- function(libname, pkgname) {
  shiny::removeResourcePath("www")
}


