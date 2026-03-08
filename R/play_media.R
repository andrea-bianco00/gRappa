#' Play a media file from gRappa
#'
#' This function detects whether a file is an audio or a video
#' stored in the package and opens it with the appropriate function.
#'
#' @param file Name of the media file
#'
#' @return Invisibly returns the file name
#' @export

play_media <- function(file) {

  audio_files <- list_audio()
  video_files <- list_video()

  if (file %in% audio_files) {
    play_audio(file)
    return(invisible(file))
  }

  if (file %in% video_files) {
    play_video(file)
    return(invisible(file))
  }

  stop("Media file not found in gRappa.", call. = FALSE)
}
