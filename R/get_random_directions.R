#' Returns a character vector of directs with \emph{n_directions} elements,
#' where \emph{n_directions} is from the \emph{specifications}. All directions
#' are allowed by the word size and the puzzle_array dimensions.
#' @param word - character vector of one element with word to be put into 
#' puzzle array.
#' @param specifications - puzzle specifications
#' @export
get_random_directions <- function(word, specifications) {
  directions <- 
    filter_out_bad_directions(word, attr(specifications, 'names')[5:12], 
                              specifications)
  directions <- 
    sample(directions, specifications[['n_directions']], replace = TRUE, 
           prob = specifications[directions])
  directions
}
