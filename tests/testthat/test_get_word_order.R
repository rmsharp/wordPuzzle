context("get_word_order")
words_1 <- get_words("test_file_1.txt")
test_that("get_word_order() returns the correct word order", {
  expect_equal(get_word_order(c("word1", "word22", "word333")), c(3, 2, 1))
  expect_equal(get_word_order(words_1), 
               c(23L, 24L, 21L, 22L, 17L, 18L, 15L, 14L, 19L, 13L, 16L, 20L, 
                 11L, 12L, 8L, 7L, 3L, 5L, 4L, 9L, 1L, 6L, 10L, 2L, 25L))
})
