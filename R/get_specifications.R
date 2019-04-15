#' Reads the puzzle description file and returns a character
#' vector with one specification in each element. 
#'
#' @param puzzle_description_file a character varialble containing the 
#' name of the file containing the word list and the specifications, which
#' have the following elements:
#' @import stringi
#' @export
get_specifications <- function(puzzle_description_file) {
  if (is.null(puzzle_description_file))
    return(list(puzzle_description_file = file.choose(),
                title = "",
                max_trials = 1000,
                n_directions = 10,
                width = 50,
                height = 40,
                left_right = 0.3,
                right_left = 0.2,
                up_down = 0.2,
                down_up = 0.2,
                up_right = 0.1,
                up_left = 0.1,
                down_right = 0.1,
                down_left = 0.1))
  specifications <- 
    list(puzzle_description_file = puzzle_description_file,
         title = "",
         max_trials = 1000,
         n_directions = 10,
         width = 50,
         height = 40,
         left_right = 1,
         right_left = 1,
         up_down = 1,
         down_up = 1,
         up_right = 1,
         up_left = 1,
         down_right = 1,
         down_left = 1)
  conn <- file(puzzle_description_file)
  open(conn)
  while (length(oneLine <- readLines(conn, n = 1, warn = FALSE)) > 0) {
    first_word <- stri_split_fixed(oneLine, " ")[[1]][1]
    if (first_word != "#")
      next
    break
  }
  while (length(oneLine <- readLines(conn, n = 1, warn = FALSE)) > 0) {
    first_word <- stri_split_fixed(oneLine, "=")[[1]][1]
    if (first_word == "") { # empty line
      next
    } else {
      specs <- stri_trim_both(unlist(stri_split_fixed(oneLine, '=')))
      if (stri_sub(specs[[2]], 1, 1) %in% c(LETTERS, letters)) {
        specifications[specs[[1]]] <- specs[[2]]
      } else {
        specifications[specs[[1]]] <- as.numeric(specs[[2]])
      }
    }
  }
  close(conn)
  specifications
}
