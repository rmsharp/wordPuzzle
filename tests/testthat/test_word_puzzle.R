library(testthat, quietly = TRUE)
library(wordPuzzle, quietly = TRUE)
context("word_puzzle")
test_that("get_words() tokenizes puzzle description file", {
  expect_equal(length(get_words("test_file_1.txt")), 25)
  expect_equal(length(get_words("test_file_2.txt")), 7)
})
test_that("get_words() returns the correct words", {
  expect_equal(get_words("test_file_1.txt")[[25]], '25')
  expect_equal(get_words("test_file_1.txt")[[22]], 'twenty_two')
})
test_that("get_specifications() returns the correct specifications", {
  expect_equal(get_specifications("test_file_1.txt")[['title']], "Test file 1")
  expect_equal(get_specifications("test_file_1.txt")[['n_directions']], 20)
  expect_equal(get_specifications("test_file_1.txt")[['width']], 25)
  expect_equal(get_specifications("test_file_1.txt")[['left_right']], 0.3)
  expect_equal(get_specifications("test_file_1.txt")[['right_left']], 0.2)
  expect_equal(get_specifications("test_file_1.txt")[['up_down']], 0.2)
  expect_equal(get_specifications("test_file_1.txt")[['down_up']], 0.2)
  expect_equal(get_specifications("test_file_1.txt")[['down_up']], 0.2)
  expect_equal(get_specifications("test_file_1.txt")[['up_right']], 0.1)
  expect_equal(get_specifications("test_file_1.txt")[['up_left']], 0.1)
  expect_equal(get_specifications("test_file_1.txt")[['down_right']], 0.1)
  expect_equal(get_specifications("test_file_1.txt")[['down_left']], 0.1)
})
test_that("get_word_order() returns words in order", {
  expect_equal(get_word_order(c("word1", "word22", "word333")), c(3, 2, 1))
})
test_that("filter_out_bad_directions() returns a vector of usable directions", {
  specifications <- 
    c(max_trials = 1000,
      n_directions = 10,
      width = 5,
      height = 10,
      left_right = 1,
      right_left = 1,
      up_down = 1,
      down_up = 1,
      up_right = 1,
      up_left = 1,
      down_right = 1,
      down_left = 1)
  
  directions <- c("left_right", "right_left", "up_down", "down_up", 
                  "up_right", "up_left", "down_right", "down_left")
  filtered_directions <- filter_out_bad_directions('ape', directions, 
                                                   specifications)
  expect_equal(filtered_directions, directions)
  filtered_directions <- filter_out_bad_directions('longWord', directions, 
                                                   specifications)
  expect_equal(filtered_directions, c("up_down", "down_up"))
  specifications['width'] <- 10
  specifications['height'] <- 5
  filtered_directions <- filter_out_bad_directions('longWord', directions, 
                                                   specifications)
  expect_equal(filtered_directions, c("left_right", "right_left"))
  })
test_that(
  "get_random_directions() returns correct number of random directions.", {
    specifications <- 
      c(max_trials = 1000,
        n_directions = 4,
        width = 5,
        height = 10,
        left_right = 1,
        right_left = 1,
        up_down = 1,
        down_up = 1,
        up_right = 1,
        up_left = 1,
        down_right = 1,
        down_left = 1)
    set.seed(4567)
    expect_equal(get_random_directions('ape', specifications), 
                 c("up_down", "down_right", "up_down", "up_right"))
    expect_error(get_random_directions('monodelphis', specifications), 
                 "Width or height dimension too small for word")
    specifications['height'] <- 12
    expect_equal(get_random_directions('monodelphis', specifications), 
                 c("up_down", "down_up", "up_down", "down_up"))
  })
test_that(
  "form_puzzle_array() form the right sized matrix filled with '.'", {
    specifications <- 
      c(max_trials = 1000,
        n_directions = 4,
        width = 5,
        height = 10,
        left_right = 1,
        right_left = 1,
        up_down = 1,
        down_up = 1,
        up_right = 1,
        up_left = 1,
        down_right = 1,
        down_left = 1)
    puzzle_array <- form_puzzle_array(specifications)
    expect_equal(length(puzzle_array), as.numeric(specifications['width'] *
                                                    specifications['height']))
    expect_equal(length(puzzle_array), length(puzzle_array[puzzle_array == '.']))
  })

