#' List available video files in gRappa
#'
#' This function lists all video files stored in the package.
#'
#' @return A character vector with video file names
#' @export

list_video <- function() {

  path <- system.file("extdata", "video", package = "gRappa")

  list.files(path)

}
