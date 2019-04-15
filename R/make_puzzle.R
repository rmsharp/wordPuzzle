#' Returns a list with the filename, the specifications, a puzzle_array, and 
#' the word_key.
#' 
#' @param filename the name of the file with the puzzle words and specifications
#' @param upper_case a logical flag indicating whether or not the puzzle is
#' folded to upper case or not.
#' @export
make_puzzle <- function (filename, upper_case = TRUE) {
  words <- get_words(filename)
  word_order <- get_word_order(words)
  specifications <- get_specifications(filename)
  puzzle_array <- form_puzzle_array(specifications)
  word_key <- data.frame()
  word_num <- 1
  for (w_ptr in seq_along(words)) {
    word <- words[[word_order[[w_ptr]]]]
    a_word <- case(word, upper_case)
    force_break <- FALSE
    
    directions <- get_random_directions(word, specifications)
    for (direction in directions) {
      for (i in 1:specifications[['max_trials']]) {
        # word_fit_l will have a list containing the trial_word_vector, the location,
        #          and the offset
        
        word_fit_l <- try_word_fit(puzzle_array, a_word, direction, 
                                   specifications)
        if (word_fit_l[['word_fits']]) {
          location <- word_fit_l[['location']]
          puzzle_array <- add_word(puzzle_array, a_word, location,
                                   word_fit_l[['offset']])
          word_key <- rbind(word_key, 
                            data.frame(word_order = word_order[[w_ptr]],
                                       word = word, 
                                       r = location[1],
                                       c = location[2]))
          #         print(str_c("word is: ", word, "; location is: ", 
          #                     word_fit_l[['location']][[1]], ", ",
          #                     word_fit_l[['location']][[2]]))
          #         print(puzzle_array)
          #         print_puzzle(puzzle_array)
          force_break <- TRUE
          break
        }
      }
      if (force_break)
        break
    }
    #puzzle_array
  }
  list(filename = filename, specifications = specifications, 
       puzzle_array = puzzle_array, word_key = word_key)
}

