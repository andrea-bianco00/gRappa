#' gRappa scrolling banner
#'
#' Opens a browser window with a continuously scrolling slogan.
#'
#' @param speed Duration of one full scrolling cycle in seconds.
#' Smaller values make the banner move faster.
#' @param size Font size in pixels.
#' @param text_color Color of the text.
#' @param bg_color Background color of the page.
#' @param position Vertical position of the banner: "top", "center", or "bottom".
#'
#' @export

grappa_banner <- function(speed = 8,
                          size = 70,
                          text_color = "orange",
                          bg_color = "black",
                          position = c("top", "center", "bottom")) {

  position <- match.arg(position)

  top_value <- switch(
    position,
    top = "0%",
    center = "50%",
    bottom = "100%"
  )

  translate_y <- switch(
    position,
    top = "0%",
    center = "-50%",
    bottom = "-100%"
  )

  html <- sprintf('
  <html>
  <head>
  <style>

  body {
    background-color: %s;
    margin: 0;
    overflow: hidden;
  }

  .ticker-wrapper {
    width: 100%%;
    overflow: hidden;
    white-space: nowrap;
    position: absolute;
    top: %s;
    transform: translateY(%s);
  }

  .ticker {
    display: inline-flex;
    white-space: nowrap;
    animation: scroll %.2fs linear infinite;
    font-family: Arial;
    font-size: %dpx;
    font-weight: bold;
    color: %s;
  }

  .ticker span {
    padding-right: 20px;
  }

  @keyframes scroll {
    from { transform: translateX(0); }
    to   { transform: translateX(-50%%); }
  }

  </style>
  </head>

  <body>

  <div class="ticker-wrapper">
    <div class="ticker">
      <span>LA GRAPPA TI ABBRACCIA - LA GRAPPA TI SCALDA -</span>
      <span>LA GRAPPA TI ABBRACCIA - LA GRAPPA TI SCALDA -</span>
      <span>LA GRAPPA TI ABBRACCIA - LA GRAPPA TI SCALDA -</span>
    </div>
  </div>

  </body>
  </html>
  ', bg_color, top_value, translate_y, speed, size, text_color)

  file <- tempfile(fileext = ".html")
  writeLines(html, file)

  utils::browseURL(file)
}
