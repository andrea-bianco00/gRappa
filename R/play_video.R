#' Open a video file from gRappa
#'
#' This function opens a video file stored in the package
#' using the default application of the operating system.
#'
#' @param file Name of the video file
#'
#' @return Invisibly returns the file path
#' @export

play_video <- function(file) {

  path <- video_path(file)

  utils::browseURL(path)

  invisible(path)
}
