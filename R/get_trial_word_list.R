#' Returns a list containing the following:
#'    trial_word_vector - character vector containing the contents of the 
#'        puzzle_array beginning at the location selected and continuing
#'        for the length of the word and in the direction indicated.
#'    location - 2 element integer vector containing a random location of the 
#'        first character of the word
#'    direction - 1 element character vector containing the direction
#'        descriptor
#' @param puzzle_array character vector containing the puzzle
#' @param word character vector of one element with word to be put into 
#' puzzle array.
#' @param direction character vector of one element that specifies the 
#' proposed direction.
#' @param specifications puzzle specifications
#' @import stringi
#' @export
get_trial_word_list <- function(puzzle_array, word, direction, 
                                specifications) {
  location <- get_trial_location(word, direction, specifications)
  r_offset <- 0
  if (direction %in% c("down_up", "up_left", "up_right")) {
    r_offset <- -1
  } else if (direction %in% c("up_down", "down_left", "down_right")) {
    r_offset <- 1
  }
  c_offset <- 0
  if (direction %in% c("down_left", "up_left", "right_left")) {
    c_offset <- -1
  } else if (direction %in% c("left_right", "up_right", "down_right"))
    c_offset <- 1
  len <- stri_length(word)
  trial_word_vector <- character(len)
  t_location <- location
  for (i in 1:len) {
    trial_word_vector[i] <- puzzle_array[t_location[[1]], t_location[[2]]]
    t_location[1] <- t_location[[1]] + r_offset
    t_location[2] <- t_location[[2]] + c_offset
  }
  list(trial_word_vector = trial_word_vector,
       location = location,
       offset = c(r_offset, c_offset))
}
