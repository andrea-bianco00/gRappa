#' List available audio files in gRappa
#'
#' This function lists all audio files stored in the package.
#'
#' @return A character vector with audio file names
#' @export

list_audio <- function() {

  path <- system.file("extdata", "audio", package = "gRappa")

  list.files(path)

}
