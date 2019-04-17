words_1 <- get_words("test_file_1.txt")
words_2 <- get_words("test_file_2.txt")
specs_1 <- get_specifications("test_file_1.txt")
test_that("case() performs as expected", {
  expect_true(case("AbcD", TRUE) == "ABCD")
  expect_false(case("AbcD") == "AbcD")
  expect_true(case("AbcD", FALSE) == "AbcD")
})
test_that(
  stri_c("get_random_directions() returns the correct psuedorandom ",
         "list of directions"), {
           set.seed(1)
           directions <- get_random_directions(words_1[1], specs_1)
           expect_equal(directions, 
                        c("up_down", "up_down", "down_up", "up_right", 
                          "left_right", "up_right", "down_left", "down_up",
                          "down_up", "left_right", "left_right", 
                          "left_right", "down_up", "up_down", "down_right", 
                          "right_left", "up_left", "down_left", "up_down",
                          "down_right"))
           set.seed(1)
           directions <- get_random_directions("ThisIsALongWord", specs_1)
           expect_equal(directions, 
                        c("up_down", "up_down", "down_up", "up_right", 
                          "left_right", "up_right", "down_left", "down_up", 
                          "down_up", "left_right", "left_right", "left_right",
                          "down_up", "up_down", "down_right", "right_left", 
                          "up_left", "down_left", "up_down", "down_right"))
         })
test_that
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
  filtered_directions <- filter_out_bad_directions("ape", directions, 
                                                   specifications)
  expect_equal(filtered_directions, directions)
  filtered_directions <- filter_out_bad_directions("longWord", directions, 
                                                   specifications)
  expect_equal(filtered_directions, c("up_down", "down_up"))
  specifications["width"] <- 10
  specifications["height"] <- 5
  filtered_directions <- filter_out_bad_directions("longWord", directions, 
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
    expect_equal(get_random_directions("ape", specifications), 
                 c("up_down", "down_right", "up_down", "up_right"))
    expect_error(get_random_directions("monodelphis", specifications), 
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

