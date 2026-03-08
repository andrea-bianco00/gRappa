#' Get path to an audio file in gRappa
#'
#' This function returns the full path to an audio file stored in the package.
#'
#' @param file Name of the audio file
#'
#' @return A character string containing the file path
#' @export

audio_path <- function(file) {

  path <- system.file("extdata", "audio", file, package = "gRappa")

  if (path == "") {
    stop("Audio file not found in gRappa.", call. = FALSE)
  }

  path
}
