#' Get the next available grappa ID
#'
#' Reads the current gRappa dataset and returns the next available
#' identifier in the sequence (e.g. "grp_000007").
#'
#' @return Character string containing the next grappa id.
#' @export

next_grappa_id <- function() {

  df <- grappa_data()

  if (!"id" %in% names(df)) {
    stop("Dataset does not contain an 'id' column.")
  }

  ids <- df$id

  # remove NA
  ids <- ids[!is.na(ids)]

  # extract numeric part
  numbers <- as.integer(sub("grp_", "", ids))

  if (length(numbers) == 0) {
    next_id <- 1
  } else {
    next_id <- max(numbers) + 1
  }

  # format with leading zeros
  new_id <- sprintf("grp_%06d", next_id)

  return(new_id)
}
