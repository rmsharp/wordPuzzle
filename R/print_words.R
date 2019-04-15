#' Prints list of words to find 
#'
#' @param puzzle_list list containing list of words to use and the puzzle 
#' @import stringi
#' @export
print_words <- function(puzzle_list) {
  cat("\n\nWord puzzle word list:\n")
  word_key <- puzzle_list$word_key
  word_key <- word_key[order(word_key$word_order), ]
  n_words <- length(word_key$word)
  rows <- floor((n_words + 1) / 3)
  for (i in 1:rows) {
    j <- i + rows # column 2 pointer
    k <- j + rows # column 3 pointer
    n_col1_spaces <- 20 - stri_length(word_key$word[[i]]) 
    col1_spaces <- stri_c(rep(' ', n_col1_spaces), collapse = '')
    if (k <= n_words) {
      n_col2_spaces <- 20 - stri_length(word_key$word[[j]]) 
      col2_spaces <- stri_c(rep(' ', n_col2_spaces), collapse = '')
      n_col3_spaces <- 20 - stri_length(word_key$word[[k]]) 
      col3_spaces <- stri_c(rep(' ', n_col2_spaces), collapse = '')
      cat(stri_c(sprintf("%4d", i), ": ", word_key$word[[i]], 
                 col1_spaces, 
                 sprintf("%4d", j), ": ", word_key$word[[j]], 
                 col2_spaces, 
                 sprintf("%4d", k), ": ", word_key$word[[k]], "\n"))
    } else if (j <= n_words) {
      n_col2_spaces <- 20 - stri_length(word_key$word[[j]]) 
      col2_spaces <- stri_c(rep(' ', n_col2_spaces), collapse = '')
      cat(stri_c(sprintf("%4d", i), ": ", word_key$word[[i]], 
                 col1_spaces, 
                 sprintf("%4d", j), ": ", word_key$word[[j]], "\n"))
    } else {
      cat(stri_c(sprintf("%4d", i), ": ", word_key$word[[i]], "\n"))
    }
  }
}
