#' Show all icons in the gRappa package
#'
#' This function displays all PNG icons stored in the
#' \code{inst/extdata/icons} directory of the package.
#'
#' @param ncol Number of columns in the icon gallery.
#'
#' @return Invisibly returns the vector of icon file names.
#' @export
show_all_icons <- function(ncol = 3) {

  path <- system.file(
    "extdata",
    "icons",
    package = "gRappa"
  )

  if (path == "") {
    stop("Icons directory not found in the package.", call. = FALSE)
  }

  files <- list.files(
    path = path,
    pattern = "\\.png$",
    ignore.case = TRUE
  )

  if (length(files) == 0) {
    stop("No PNG icons found in the icons directory.", call. = FALSE)
  }

  if (!requireNamespace("png", quietly = TRUE)) {
    stop("Package 'png' is required to display icons.", call. = FALSE)
  }

  n <- length(files)
  nrow <- ceiling(n / ncol)

  old_par <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(old_par), add = TRUE)

  graphics::par(
    mfrow = c(nrow, ncol),
    mar = c(1, 1, 3, 1)
  )

  for (file in files) {
    img_path <- file.path(path, file)
    img <- png::readPNG(img_path)

    graphics::plot.new()
    graphics::rasterImage(
      img,
      xleft = 0.1, ybottom = 0.2,
      xright = 0.9, ytop = 0.9
    )
    graphics::title(main = file, line = 0.5, cex.main = 0.9)
  }

  invisible(files)
}
