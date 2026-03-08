#' List all media files in gRappa
#'
#' This function lists all audio and video files stored in the package.
#'
#' @return A named list containing audio and video files
#' @export

list_media <- function() {

  audio_files <- list_audio()
  video_files <- list_video()

  list(
    audio = audio_files,
    video = video_files
  )

}
