#' Returns the character vector collapsed into as a single character vector 
#' element.
#' 
#' @param x character vector to be collapsed
#' @param collapse charcter string with collapse argument.
#' @import stringi
#' @export
collapse_word <- function(x, collapse = '') {
  stri_c(x, collapse = collapse)
}
