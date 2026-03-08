#' Show an icon from the gRappa package
#'
#' This function loads and displays a PNG icon stored in
#' the \code{inst/extdata/icons} directory of the package.
#'
#' @param name Character. Name of the icon file (e.g. "grappa_glass.png").
#'
#' @return The image object (invisibly).
#' @export

show_icon <- function(name) {

  path <- system.file(
    "extdata",
    "icons",
    name,
    package = "gRappa"
  )

  if (path == "") {
    stop(
      paste0(
        "Icon '", name, "' not found.\n",
        "Use list_icons() to see available icons."
      ),
      call. = FALSE
    )
  }

  if (!requireNamespace("png", quietly = TRUE)) {
    stop(
      "Package 'png' is required to display icons.",
      call. = FALSE
    )
  }

  img <- png::readPNG(path)

  grid::grid.newpage()
  grid::grid.raster(img)

  invisible(img)
}
