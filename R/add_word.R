#' Copies characters of the word into the puzzle_array
#' @param puzzle_array - character vector containing the puzzle
#' @param word 1 element character vector containing the word being placed
#' in the puzzle
#' @param location row and column location
#' @param offset number of rows and columns the location is to be offset.
#' @export
add_word <- function(puzzle_array, word, location, offset) {
  t_location <- location
  if (stringi::stri_length(word) <= 0)
    stop("word of no length found.")
  word_v <- expand_word(word)
  for (i in 1:length(word_v)) {
    puzzle_array[t_location[[1]], t_location[[2]]] <- word_v[[i]]
    t_location[1] <- t_location[[1]] + offset[[1]]
    t_location[2] <- t_location[[2]] + offset[[2]]
  }
  puzzle_array
}
