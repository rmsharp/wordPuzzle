#' Returns a list with each character from the word in seccessive list elements
#' 
#' @param word a 1 element character vector contain a word to be expanded
#' @export
expand_word <- function(word) {
  lapply(seq(1,nchar(word),1), function(i) substr(word, i, i))
}
