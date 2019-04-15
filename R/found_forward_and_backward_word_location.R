#' Looks for words in text and returns found words listed forward and backward
#' 
#' Finds as many words as possible with in text and lists them in forward
#' and backword.
#' @return dataframe with words listed in orginal and reverse orders
#' 
#' @param words charcter vector of words
#' @param text character vector of length 1 that is being searched
#' @importFrom rmsutilityr str_reverse
#' @importFrom stringi stri_replace_all_fixed
#' @export
found_forward_and_backward_word_location <- function (words, text) {
  words <- unique(stri_replace_all_fixed(words, "[123]", ''))
  r_words <- rmsutilityr::str_reverse(words)
  found_words <- data.frame()
  found_words <- find_words(words, text, found_words) 
  found_words <- find_words(r_words, text, found_words) 
  found_words
}
