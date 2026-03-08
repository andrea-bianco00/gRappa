#' Open an audio file from gRappa
#'
#' This function opens an audio file stored in the package
#' using the default application of the operating system.
#'
#' @param file Name of the audio file
#'
#' @return Invisibly returns the file path
#' @export

play_audio <- function(file) {

  path <- audio_path(file)

  utils::browseURL(path)

  invisible(path)
}
