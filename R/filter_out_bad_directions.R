#' Returns a vector of directions that can be used
#' given the word size, the demensions of the puzzle, and the proposed list of
#' directions.
#' @param word - character vector of one element with word to be put into 
#' puzzle array.
#' @param directions - character vector of the directions proposed for use.
#' @param specifications - puzzle specifications
#' @import stringi
#' @import plyr
#' @export
filter_out_bad_directions <- function(word, directions, specifications) {
  directions <- sapply(directions, function(direction) {
    if ((stri_detect_fixed(direction, 'up') | stri_detect_fixed(direction, 'down')) &
        stri_length(word) > specifications[['height']]) {
      direction <- NA
    } else if ((stri_detect_fixed(direction, 'left') | 
                stri_detect_fixed(direction, 'right')) &
               stri_length(word) > specifications[['width']]) {
      direction <- NA
    } else {
      direction
    }})
  directions <- directions[!is.na(directions)]
  if (length(directions) == 0)
    stop("Width or height dimension too small for word length and direction")
  names(directions) <- NULL
  directions
}
