#' Creates puzzle text ready to print
#' 
#' @param puzzle_list list containing list of words to use and the puzzle 
#' specifications
#' @import stringi
#' @export
print_puzzle <- function(puzzle_list) {
  specifications <- puzzle_list$specifications
  puzzle_array <- puzzle_list$puzzle_array
  filler <- sample(LETTERS, length(puzzle_array[puzzle_array == '.']), 
                   replace = TRUE)
  j <- 1
  for (i in seq_along(puzzle_array)) {
    if (puzzle_array[[i]] == '.') {
      puzzle_array[i] <- filler[[j]]
      j <- j + 1
    }
  }
  puzzle_array <- 
    matrix(puzzle_array, nrow = specifications[['height']], byrow = FALSE)
  
  for (row in 1:dim(puzzle_array)[[1]]) {
    cat(stri_c(collapse_word(puzzle_array[row,], ' ')), '\n')
  }
}
