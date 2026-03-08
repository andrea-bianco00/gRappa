#' Play a random media file from gRappa
#'
#' This function randomly selects one available audio or video file
#' stored in the package and opens it.
#'
#' @return Invisibly returns the name of the selected file
#' @export

random_grappa <- function() {

  media_files <- c(list_audio(), list_video())

  if (length(media_files) == 0) {
    stop("No media files available in gRappa.", call. = FALSE)
  }

  chosen_file <- sample(media_files, size = 1)

  play_media(chosen_file)

  invisible(chosen_file)
}
