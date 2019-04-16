#' Returns a two element numeric vector with the row and column specified
#' for a location to be tried.
#' @param word - character vector of one element with word to be put into 
#' puzzle array.
#' @param direction - character vector of one element that specifies the 
#' proposed direction.
#' @param specifications - puzzle specifications
#' @import stringi
#' @export
get_trial_location <- function(word, direction, specifications) {
  len <- stri_length(word)
  if (direction %in% c("down_left", "down_right", "up_down")) {
    r <- sample(1:(specifications[["height"]] - len + 1), 1)
  } else if (direction %in% c("up_right", "up_left", "down_up")) {
    r <- sample(len:specifications[["height"]], 1)
  } else {
    r <- sample(1:(specifications[["height"]]), 1)
  }
  if (direction %in% c("left_right", "down_right", "up_right")) {
    c <- sample(1:(specifications[["width"]] - len + 1), 1)
  } else if (direction %in% c("down_left", "up_left", "right_left")) {
    c <- sample(len:(specifications[["width"]]), 1)
  } else {
    c <- sample(1:(specifications[["width"]]), 1)
  }
  c(r, c)  
}
