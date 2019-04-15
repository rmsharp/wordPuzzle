#' Returns the word either without change of folded to upper case depending on
#' the second argument of class logical.
#' 
#' @param word 1 element character vector with the word
#' @param upper_case a logical flag indicating whether or not the word is
#' folded to upper case or not.
#' @export
case <- function(word, upper_case) {
  if (upper_case)
    word <- toupper(word)
  
  word
}
