#' get_words reads in words to be inserted in the puzzle and the specifications of the puzzle.
#' 
#' Reads the puzzle description file and returns a character
#' vector with one word in each element. 
#' 
#' The letter case of words and order is not changed.
#' 
#' @param puzzle_description_file is a character varialble containing the 
#' name of the file containing the word list and specifications.
#' 
#' It has a list of words ended by a line beginning with a '#'. 
#' Words may be on the same or different lines.
#' The following elements make of the specifications, which follow the word
#' list:
#' \tabular{ll}{
#' puzzle_description_file \tab File name (with path if necessary) with the specifications and word list to use. \cr
#' title \tab Title for word puzzle first page.\cr
#' max_trials \tab Number of times the program is to try to fit each word into the puzzle \emph{n_direction} times.\cr
#' n_directions \tab Number of directions (not necessarily different) to be tried for each word if needed. \cr
#' width \tab Width of puzzle in characters. \cr
#' height \tab Height of puzzle in rows. \cr
#' left_right \tab Direction of word characters; normal. \cr
#' right_left \tab Direction of word characters; backward. \cr
#' up_down \tab Direction of word characters \cr
#' down_up \tab Direction of word characters \cr
#' up_right \tab Direction of word characters \cr
#' up_left \tab Direction of word characters \cr
#' down_right \tab Direction of word characters \cr
#' down_left \tab Direction of word characters \cr
#'}
#'
#' @import stringi
#' @export
get_words <- function(puzzle_description_file) {
  if (is.null(puzzle_description_file)) {
    conn <- file.choose()
  } else {
    conn <- file(puzzle_description_file)
  }
  open(conn)
  line_list <- list(1000) # much larger than needed
  i <- 1
  while (length(oneLine <- readLines(conn, n = 1, warn = FALSE)) > 0) {
    first_word <- stri_split_fixed(oneLine, " ")[[1]][1]
    if (first_word == '') { # empty line
      next
    } else if (first_word == "#") {
      if (i <= 1) {
        stop('empty puzzle description file')
      } else {
        line_list <- line_list[1:(i - 1)]        
      }
      break
    } else {
      line_list[i] <- (stri_split_fixed(oneLine, " "))
      i <- i + 1
    }
  }
  close(conn)
  words <- unlist(line_list)
  words <- words[stri_length(words) > 0]
}
