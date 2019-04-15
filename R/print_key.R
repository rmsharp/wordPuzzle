#' Creates puzzle text ready to print
#' 
#' @param puzzle_list list containing list of words to use and the puzzle 
#' specifications
#' @param key can have values of \emph{both} or \emph{row}. If \emph{both} 
#' then row and column location will be provided, else just the row is 
#' provided.
#' @import stringi
#' @import plyr
#' @export
print_key <- function(puzzle_list, key = 'both') {
  cat("\n\nWord key with Row and Column positions of the first letter:\n")
  word_key <- puzzle_list$word_key
  word_key <- word_key[order(word_key$word_order), ]
  n_words <- length(word_key$word)
  rows <- floor((n_words + 1) / 2)
  for (i in 1:rows) {
    n_col1_spaces <- 15 - stri_length(word_key$word[[i]]) 
    col1_spaces <- stri_c(rep(' ', n_col1_spaces), collapse = '')
    j <- i + rows # column 2 pointer
    if (j <= n_words) {
      if (key == 'row') {
        word_key$c[i] <- 0
        word_key$c[j] <- 0
      }
      
      n_col2_spaces <- 15 - stri_length(word_key$word[[j]]) 
      col2_spaces <- stri_c(rep(' ', n_col2_spaces), collapse = '')
      cat(stri_c(sprintf("%4d", i), ": ", word_key$word[[i]], 
                col1_spaces, sprintf("- [%3d,", word_key$r[[i]]),
                sprintf("%3d]    ", word_key$c[[i]]),
                sprintf("%4d", j), ": ", word_key$word[[j]], 
                col2_spaces, sprintf("- [%3d,", word_key$r[[j]]),
                sprintf("%3d]\n", word_key$c[[j]])))
      
    } else {
      if (key == 'row')
        word_key$c[i] <- 0
      
      cat(stri_c(sprintf("%4d", i), ": ", word_key$word[[i]], 
                col1_spaces, sprintf("- [%3d,", word_key$r[[i]]),
                sprintf("%3d]\n", word_key$c[[i]])))
    }
  }
}
