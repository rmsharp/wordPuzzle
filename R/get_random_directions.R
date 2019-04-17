#' Returns a character vector of directs with \emph{n_directions} elements,
#' where \emph{n_directions} is from the \emph{specifications}. All directions
#' are allowed by the word size and the puzzle_array dimensions.
#' @param word - character vector of one element with word to be put into 
#' puzzle array.
#' @param specifications - puzzle specifications
#' @importFrom stringi stri_detect_fixed
#' @export
get_random_directions <- function(word, specifications) {
  directions <- attr(specifications, "names")[
    stri_detect_fixed(attr(specifications, "names"), "right") |
    stri_detect_fixed(attr(specifications, "names"), "left") |
    stri_detect_fixed(attr(specifications, "names"), "down")]
  directions <- 
    filter_out_bad_directions(word, directions, specifications)
  directions <- 
    sample(directions, specifications[['n_directions']], replace = TRUE, 
           prob = specifications[directions])
  directions
}
