#' Get path to a video file in gRappa
#'
#' This function returns the full path to a video file stored in the package.
#'
#' @param file Name of the video file
#'
#' @return A character string containing the file path
#' @export

video_path <- function(file) {

  path <- system.file("extdata", "video", file, package = "gRappa")

  if (path == "") {
    stop("Video file not found in gRappa.", call. = FALSE)
  }

  path
}
