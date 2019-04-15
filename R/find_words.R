#' Prints out location for each \emph{word} in a character vector
#' 
#' @param words charcter vector of words
#' @param text character vector of length 1 that is being searched
#' @param found_words dataframe of words found with three columns:
#' \emph{n_word} -- integer order of the word found, 
#' \emph{original_word} -- the word searched for and found with original case
#' retained.
#' \emph{starts_at} -- characer position of the first letter of the found
#' word within the \emph{text} provided.
#' @import stringi
#' @export
find_words <- function (words, text, found_words) {
  n_word <- nrow(found_words) + 1
  for (word in words) {
    original_word <- word
    word <- toupper(word)
    if (stri_detect_fixed(text, word)) {
      len <- length(stri_locate_all_fixed(text, word)[[1]][1])
      for (i in 1:len) {
        found_words <- 
          rbind(found_words, 
                data.frame(
                  n_word = n_word,
                  original_word = original_word,
                  starts_at = stri_locate_all_fixed(text, word)[[i]][1]))
        print(stri_c(n_word, ". ", original_word, ": starts at ", 
                    stri_locate_all_fixed(text, word)[[i]][1]))
        n_word <- n_word + 1
        
      }
    }
  }
  found_words
}
