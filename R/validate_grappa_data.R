#' Validate grappa locations data
#'
#' Validates a grappa locations dataset by checking:
#' - required columns
#' - unique identifiers
#' - date format
#' - player presence
#' - coordinate validity
#' - source_type validity
#' - row-level logical consistency
#'
#' A row is considered archive-valid if it contains either:
#' - valid latitude and longitude
#' - or a minimum valid address: street, street_number, city, country
#'
#' A row is considered map-valid only if it contains valid latitude
#' and longitude.
#'
#' @param data A data.frame containing grappa locations data.
#'
#' @return A list with:
#' \describe{
#'   \item{ok}{Logical; TRUE if no validation errors are found.}
#'   \item{data}{Validated and cleaned data.frame with extra columns
#'   \code{archive_valid} and \code{map_valid}.}
#'   \item{issues}{A data.frame listing validation errors and warnings.}
#'   \item{summary}{A list with validation summary counts.}
#' }
#'
#' @export
validate_grappa_data <- function(data) {

  if (!is.data.frame(data)) {
    stop("'data' must be a data.frame.", call. = FALSE)
  }

  required_cols <- c(
    "id", "date", "player", "grappa_label", "place_name",
    "street", "street_number", "city", "postal_code", "region", "country",
    "latitude", "longitude", "source_type", "note"
  )

  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop(
      paste0(
        "Missing required columns: ",
        paste(missing_cols, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  df <- data

  # Convert empty strings in character columns to NA
  char_cols <- names(df)[vapply(df, is.character, logical(1))]
  for (col in char_cols) {
    df[[col]] <- trimws(df[[col]])
    df[[col]][df[[col]] == ""] <- NA_character_
  }

  issues <- data.frame(
    row = integer(),
    id = character(),
    issue_type = character(),
    message = character(),
    stringsAsFactors = FALSE
  )

  add_issue <- function(row, id, issue_type, message) {
    issues <<- rbind(
      issues,
      data.frame(
        row = row,
        id = if (length(id) == 0 || is.na(id)) NA_character_ else as.character(id),
        issue_type = issue_type,
        message = message,
        stringsAsFactors = FALSE
      )
    )
  }

  n <- nrow(df)

  # Parse date / coords
  parsed_dates <- suppressWarnings(as.Date(df$date))
  lat_num <- suppressWarnings(as.numeric(df$latitude))
  lon_num <- suppressWarnings(as.numeric(df$longitude))

  # id missing
  for (i in seq_len(n)) {
    if (is.na(df$id[i])) {
      add_issue(i, NA, "error", "Missing id")
    }
  }

  # duplicated id
  duplicated_ids <- unique(df$id[duplicated(df$id) & !is.na(df$id)])
  if (length(duplicated_ids) > 0) {
    for (dup_id in duplicated_ids) {
      dup_rows <- which(df$id == dup_id)
      for (r in dup_rows) {
        add_issue(r, dup_id, "error", "Duplicated id")
      }
    }
  }

  # date checks
  for (i in seq_len(n)) {
    if (is.na(df$date[i])) {
      add_issue(i, df$id[i], "error", "Missing date")
    } else if (is.na(parsed_dates[i])) {
      add_issue(i, df$id[i], "error", "Invalid date format; expected YYYY-MM-DD")
    }
  }

  # player checks
  for (i in seq_len(n)) {
    if (is.na(df$player[i])) {
      add_issue(i, df$id[i], "error", "Missing player")
    }
  }

  # source_type checks
  allowed_source_types <- c(
    "manual_coordinates",
    "geocoded_address",
    "approximate"
  )

  for (i in seq_len(n)) {
    if (is.na(df$source_type[i])) {
      add_issue(i, df$id[i], "error", "Missing source_type")
    } else if (!(df$source_type[i] %in% allowed_source_types)) {
      add_issue(
        i,
        df$id[i],
        "error",
        paste0(
          "Invalid source_type '", df$source_type[i],
          "'. Allowed values: ",
          paste(allowed_source_types, collapse = ", ")
        )
      )
    }
  }

  # coordinate checks
  for (i in seq_len(n)) {
    lat_raw <- df$latitude[i]
    lon_raw <- df$longitude[i]

    lat_present <- !is.na(lat_raw)
    lon_present <- !is.na(lon_raw)

    if (lat_present && is.na(lat_num[i])) {
      add_issue(i, df$id[i], "error", "Latitude is not numeric")
    }

    if (lon_present && is.na(lon_num[i])) {
      add_issue(i, df$id[i], "error", "Longitude is not numeric")
    }

    if (!is.na(lat_num[i]) && (lat_num[i] < -90 || lat_num[i] > 90)) {
      add_issue(i, df$id[i], "error", "Latitude out of valid range [-90, 90]")
    }

    if (!is.na(lon_num[i]) && (lon_num[i] < -180 || lon_num[i] > 180)) {
      add_issue(i, df$id[i], "error", "Longitude out of valid range [-180, 180]")
    }

    if (xor(lat_present, lon_present)) {
      add_issue(
        i,
        df$id[i],
        "error",
        "Latitude and longitude must either both be present or both be missing"
      )
    }
  }

  archive_valid <- logical(n)
  map_valid <- logical(n)

  for (i in seq_len(n)) {
    has_valid_coordinates <- !is.na(lat_num[i]) &&
      !is.na(lon_num[i]) &&
      lat_num[i] >= -90 && lat_num[i] <= 90 &&
      lon_num[i] >= -180 && lon_num[i] <= 180

    has_min_address <- all(!is.na(c(
      df$street[i],
      df$street_number[i],
      df$city[i],
      df$country[i]
    )))

    archive_valid[i] <- has_valid_coordinates || has_min_address
    map_valid[i] <- has_valid_coordinates

    if (!archive_valid[i]) {
      add_issue(
        i,
        df$id[i],
        "error",
        paste0(
          "Row must contain either valid latitude+longitude ",
          "or a complete minimum address (street, street_number, city, country)"
        )
      )
    }
  }

  # Consistency warnings between source_type and actual data
  for (i in seq_len(n)) {
    st <- df$source_type[i]
    has_valid_coordinates <- !is.na(lat_num[i]) &&
      !is.na(lon_num[i]) &&
      lat_num[i] >= -90 && lat_num[i] <= 90 &&
      lon_num[i] >= -180 && lon_num[i] <= 180

    has_min_address <- all(!is.na(c(
      df$street[i],
      df$street_number[i],
      df$city[i],
      df$country[i]
    )))

    if (!is.na(st) && st == "manual_coordinates" && !has_valid_coordinates) {
      add_issue(
        i,
        df$id[i],
        "warning",
        "source_type is 'manual_coordinates' but valid coordinates are missing"
      )
    }

    if (!is.na(st) && st == "geocoded_address" && !has_min_address) {
      add_issue(
        i,
        df$id[i],
        "warning",
        "source_type is 'geocoded_address' but minimum address fields are incomplete"
      )
    }

    if (!is.na(st) && st == "geocoded_address" && !has_valid_coordinates) {
      add_issue(
        i,
        df$id[i],
        "warning",
        "source_type is 'geocoded_address' but valid coordinates are missing"
      )
    }

    if (!is.na(st) && st == "approximate" && has_valid_coordinates) {
      add_issue(
        i,
        df$id[i],
        "warning",
        "source_type is 'approximate': coordinates may have low precision"
      )
    }
  }

  # Cleaned/enriched output
  df$date <- parsed_dates
  df$latitude <- lat_num
  df$longitude <- lon_num
  df$archive_valid <- archive_valid
  df$map_valid <- map_valid

  n_errors <- sum(issues$issue_type == "error")
  n_warnings <- sum(issues$issue_type == "warning")

  out <- list(
    ok = n_errors == 0,
    data = df,
    issues = issues,
    summary = list(
      n_rows = nrow(df),
      n_errors = n_errors,
      n_warnings = n_warnings,
      n_archive_valid = sum(df$archive_valid, na.rm = TRUE),
      n_map_valid = sum(df$map_valid, na.rm = TRUE)
    )
  )

  return(out)
}
