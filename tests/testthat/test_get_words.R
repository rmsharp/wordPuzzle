context("get_words")
words_1 <- get_words("test_file_1.txt")
words_2 <- get_words("test_file_2.txt")
test_that("get_words() tokenizes puzzle description file", {
  expect_equal(length(words_1), 25)
  expect_equal(length(words_2), 7)
})
test_that("get_words() returns the correct words", {
  expect_equal(words_1[[25]], "25")
  expect_equal(words_1[[22]], "twenty_two")
})
