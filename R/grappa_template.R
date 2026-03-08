#' Create a CSV template row for a new grappa location
#'
#' Builds a single CSV-formatted row ready to be copied into the
#' live file \code{grappa_locations.csv} on GitHub.
#'
#' The output follows exactly the column order used by gRappa:
#' id, date, player, grappa_label, place_name, street, street_number,
#' city, postal_code, region, country, latitude, longitude, source_type, note
#'
#' @param id Character. Unique row identifier, e.g. "grp_000007".
#' @param date Character. Date in format "YYYY-MM-DD".
#' @param player Character. Name of the player.
#' @param grappa_label Character. Name of the grappa.
#' @param place_name Character. Name of the place.
#' @param street Character. Street name.
#' @param street_number Character. Street number.
#' @param city Character. City.
#' @param postal_code Character. Postal code.
#' @param region Character. Region/state.
#' @param country Character. Country.
#' @param latitude Numeric or character. Latitude.
#' @param longitude Numeric or character. Longitude.
#' @param source_type Character. "manual_coordinates", "geocoded_address", or "approximate".
#' @param note Character. Free note.
#'
#' @export
grappa_template <- function(
    id = "",
    date = "",
    player = "",
    grappa_label = "",
    place_name = "",
    street = "",
    street_number = "",
    city = "",
    postal_code = "",
    region = "",
    country = "",
    latitude = "",
    longitude = "",
    source_type = "",
    note = ""
) {

  escape_csv <- function(x) {
    if (is.null(x) || length(x) == 0 || is.na(x)) {
      x <- ""
    }
    x <- as.character(x)
    x <- gsub('"', '""', x, fixed = TRUE)
    paste0('"', x, '"')
  }

  fields <- c(
    id,
    date,
    player,
    grappa_label,
    place_name,
    street,
    street_number,
    city,
    postal_code,
    region,
    country,
    latitude,
    longitude,
    source_type,
    note
  )

  row_text <- paste(vapply(fields, escape_csv, character(1)), collapse = ",")

  cat(row_text, "\n")

  invisible(row_text)
}
