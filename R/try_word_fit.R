#' Returns a list consisting of a logical value indicating whether or not
#' the proposed word fits the puzzle array, a two integer vector of the row
#' and column location of the first character, and a two integer vector that
#' determines the direction of the word's orientation.
#' try_word_fit() - the characters found within
#' the array, an row and column location in a two integer vector
#' @param puzzle_array character vector containing the puzzle
#' @param word 1 element character vector containing the word being placed
#'          in the puzzle
#' @param direction  character vector of one element that specifies the 
#' proposed direction.
#' @param specifications  puzzle specifications
#' @import stringi
#' @export
try_word_fit <- function(puzzle_array, word, direction, specifications) {
  trial_word_l <- get_trial_word_list(puzzle_array, word, direction, 
                                      specifications)
  word_fits <- 
    stri_detect_fixed(word, collapse_word(trial_word_l[['trial_word_vector']], ''))
  list(word_fits = word_fits, 
       location = trial_word_l[['location']],
       offset = trial_word_l[['offset']])
}
