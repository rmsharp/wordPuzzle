#' Forms the empty puzzle array with  `.' as the holder in each cell.
#' The dimensions are determined by the specifications.
#' @param specifications - puzzle specifications
#' @export
form_puzzle_array <- function(specifications) {
  place_holder <- '.'
  matrix(rep(place_holder, specifications[['width']] * 
               specifications[['height']]), 
         nrow = specifications[['height']], byrow = TRUE)
}
