#' List available icon files in gRappa
#'
#' This function lists all icon files stored in the package.
#'
#' @return A character vector with icon file names
#' @export

list_icons <- function() {

  path <- system.file("extdata", "icons", package = "gRappa")

  list.files(path)

}
