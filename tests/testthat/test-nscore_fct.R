# Remark:
# most (eg. correct input) is handled by vario.mod


test_that("nscore calculates normal scores correctly for a numeric vector", {
  # Input vector
  input_vector <- dunif(1:10)
  result <- EgoCor:::nscore(input_vector)

  # Check that the result is a list
  expect_type(result, "list")

  # Check that the result has the correct components
  expect_named(result, c("nscore", "trn.table"))

  # Verify the length of the scores matches the input
  expect_equal(length(result$nscore), length(input_vector))

  # Verify the transformation table has correct dimensions
  expect_equal(dim(result$trn.table), c(length(input_vector), 2))
})

test_that("nscore works with negative values", {
  # Input vector
  input_vector <- c(-5, -2, -10, 0, 1)

  # Run nscore
  result <- EgoCor:::nscore(input_vector)

  # Verify the scores and transformation table
  expect_equal(length(result$nscore), length(input_vector))
  expect_equal(sort(result$trn.table$x), sort(input_vector))
})
