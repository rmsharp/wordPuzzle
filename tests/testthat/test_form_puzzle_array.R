context("form_puzzle_array")
specs_1 <- get_specifications("test_file_1.txt")
test_that("form_puzzle_array creates the right object", {
  puzzle_array <- form_puzzle_array(specs_1)
  expect_equal(nrow(puzzle_array), 15)
  expect_equal(ncol(puzzle_array), 25)
  expect_true(all(puzzle_array == "."))
})
