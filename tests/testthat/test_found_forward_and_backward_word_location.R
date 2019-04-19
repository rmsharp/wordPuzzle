context("found_forward_and_backward")
text_1 <- "abcdefgword1hijklmnop"
text_2 <- "ponmlkjih1drowgfedcba"
text_3 <- "WoRdabcdefg"
text_4 <- "gfedcbadRoW"
text_5 <- "WoRd"
text_6 <- "dRoW"
text <- toupper(stri_c(text_1, text_2, text_3, text_4, text_5, text_6, 
                       collapse = ""))
words <- c("word", "word1", "word2")
test_that("find_words identifies correct location", {
  df <- found_forward_and_backward_word_location(words, text)
  expect_equal(
    df$starts_at, c(8, 8, 32, 31))
  expect_equal(
    as.character(df$original_word),
    c("word", "word1", "drow", "1drow"))
})
