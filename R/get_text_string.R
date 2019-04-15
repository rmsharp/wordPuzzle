#' Concatenates and removes all blanks and punctuation all words in each line.
#' 
#' @param text_file file containing text to have spaces and punctuation 
#' removed and words concatenated removing any white space.
#' 
#' @import stringi
#' @export
get_text_string <- function(text_file) {
  if (is.null(text_file)) {
    conn <- file.choose()
  } else {
    conn <- file(text_file)
  }
  open(conn)
  text <- stri_c(readLines(conn), collapse = '')
  text <- stri_replace_all_fixed(text, "[[:punct:]]", "")
  text <- stri_replace_all_fixed(text, " ", "")
  close(conn)
  toupper(text)
}
