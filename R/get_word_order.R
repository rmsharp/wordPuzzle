#' Orders words in decreasing length. Ties are in ASCII order.
#' 
#' @param words Character vector with a word in each element.
#' @import stringi
#' @import plyr
#' @export
get_word_order <- function(words) {
  order(100 - stri_length(words), words, decreasing = FALSE)
}
