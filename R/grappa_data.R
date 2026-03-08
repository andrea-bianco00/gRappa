#' Load grappa locations data
#'
#' Loads the dataset containing the locations where grappa has been
#' consumed by players of the gRappa project.
#'
#' The function first attempts to download the live dataset from the
#' gRappa GitHub repository. If the remote file cannot be accessed
#' (for example due to network issues), the function falls back to
#' the example dataset included inside the package.
#'
#' @return A data.frame containing grappa locations.
#' @export
grappa_data <- function() {

  # URL of the live dataset on GitHub
  url <- "https://raw.githubusercontent.com/andrea-bianco00/gRappa/main/data-live/grappa_locations.csv"

  # Try loading the live dataset
  df <- tryCatch(
    utils::read.csv(
      url,
      stringsAsFactors = FALSE
    ),
    error = function(e) NULL
  )

  # If successful, return the live data
  if (!is.null(df)) {
    message("Loading live grappa data from GitHub...")
    return(df)
  }

  # Otherwise fall back to the local dataset inside the package
  message("Live data unavailable, using local dataset.")

  path <- system.file(
    "extdata/datasets/grappa_locations.csv",
    package = "gRappa"
  )

  if (path == "") {
    stop("Cannot find grappa_locations.csv inside the package.")
  }

  df <- utils::read.csv(
    path,
    stringsAsFactors = FALSE
  )

  return(df)

}
