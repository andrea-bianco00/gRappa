#' Find Italian rhymes
#'
#' Finds Italian words that rhyme with a given word,
#' using the last n letters of the word.
#'
#' @param word A word to rhyme.
#' @param n_letters Number of final letters used to define the rhyme.
#' @param exclude_self Logical. If TRUE, removes the input word from the output.
#'
#' @return A character vector of rhyming Italian words.
#' @export

rime <- function(word, n_letters = 4, exclude_self = FALSE) {

  if (!is.character(word) || length(word) != 1 || is.na(word)) {
    stop("'word' must be a single character string.", call. = FALSE)
  }

  if (!is.numeric(n_letters) || length(n_letters) != 1 || is.na(n_letters)) {
    stop("'n_letters' must be a single number.", call. = FALSE)
  }

  n_letters <- as.integer(n_letters)

  if (n_letters < 1) {
    stop("'n_letters' must be at least 1.", call. = FALSE)
  }

  word <- tolower(trimws(word))

  if (nchar(word) < n_letters) {
    stop("'word' must have at least 'n_letters' characters.", call. = FALSE)
  }

  url <- paste0(
    "https://raw.githubusercontent.com/",
    "napolux/paroleitaliane/main/paroleitaliane/660000_parole_italiane.txt"
  )

  words <- readLines(url, warn = FALSE, encoding = "UTF-8")
  words <- unique(tolower(trimws(words)))
  words <- words[nzchar(words)]

  ending <- substr(word, nchar(word) - n_letters + 1, nchar(word))

  rhymes <- words[grepl(paste0(ending, "$"), words)]

  if (exclude_self) {
    rhymes <- rhymes[rhymes != word]
  }

  sort(rhymes)
}
