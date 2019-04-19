context("find_words")
text_1 <- "abcdefgword1hijklmnop"
text_2 <- "ponmlkjih1drowgfedcba"
text_3 <- "WoRdabcdefg"
text_4 <- "gfedcbadRoW"
text_5 <- "WoRd"
text_6 <- "dRoW"

found_words <- data.frame()
test_that("find_words identifies correct location", {
  expect_equal(
    find_words("word1", toupper(text_1), found_words)$starts_at,
    8)
  expect_equal(
    find_words("word1", stri_reverse(toupper(text_2)), found_words)$starts_at,
    8)
  expect_equal(
    find_words("word", toupper(text_3), found_words)$starts_at,
    1)
  expect_equal(
    find_words("word", stri_reverse(toupper(text_4)), found_words)$starts_at,
    1)
  expect_equal(
    find_words("word", toupper(text_5), found_words)$starts_at,
    1)
  expect_equal(
    find_words("word", stri_reverse(toupper(text_6)), found_words)$starts_at,
    1)
  })
