#' List available dataset files in gRappa
#'
#' This function lists all dataset files stored in the package.
#'
#' @return A character vector with dataset file names
#' @export

list_datasets <- function() {

  path <- system.file("extdata", "datasets", package = "gRappa")

  list.files(path)

}
