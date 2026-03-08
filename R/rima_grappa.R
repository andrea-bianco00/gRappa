#' Words that rhyme with "grappa"
#'
#' Returns Italian words that rhyme with "grappa".
#'
#' @param n_letters Number of final letters used to define the rhyme.
#' @param exclude_self Logical. If TRUE, removes "grappa" from the output.
#'
#' @return A character vector of Italian words rhyming with "grappa".
#' @export

rima_grappa <- function(n_letters = 4, exclude_self = FALSE) {
  rime("grappa", n_letters = n_letters, exclude_self = exclude_self)
}
