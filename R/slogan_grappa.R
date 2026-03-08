#' Generate silly gRappa slogans
#'
#' Generates one or more random slogans using rhymes for "grappa".
#'
#' @param n_letters Number of final letters used to define the rhyme.
#' @param n_slogans Number of slogans to generate.
#'
#' @return A character vector of slogans. If `n_slogans = 1`, the slogan is also printed.
#' @export

slogan_grappa <- function(n_letters = 4, n_slogans = 1) {

  if (!is.numeric(n_slogans) || length(n_slogans) != 1 || is.na(n_slogans)) {
    stop("'n_slogans' must be a single number.", call. = FALSE)
  }

  n_slogans <- as.integer(n_slogans)

  if (n_slogans < 1) {
    stop("'n_slogans' must be at least 1.", call. = FALSE)
  }

  rhymes <- rime("grappa", n_letters = n_letters, exclude_self = TRUE)

  if (length(rhymes) == 0) {
    stop("No rhymes found for 'grappa'.", call. = FALSE)
  }

  templates <- c(
    "La grappa ti abbraccia, mica come %s.",
    "Se la vita ti acciappa, rispondi con la grappa e con %s.",
    "La grappa ti scalda meglio di %s.",
    "Quando arriva la grappa, tutto il resto \u00E8 %s.",
    "Prima la grappa, poi eventualmente %s.",
    "Chi beve grappa non teme %s.",
    "Meglio la grappa che %s.",
    "Con la grappa in mano, dimentichi pure %s."
  )

  slogans <- character(n_slogans)

  for (i in seq_len(n_slogans)) {
    rhyme_word <- sample(rhymes, 1)
    template <- sample(templates, 1)
    slogans[i] <- sprintf(template, rhyme_word)
  }

  if (n_slogans == 1) {
    cat(slogans, "\n")
  }

  slogans
}
