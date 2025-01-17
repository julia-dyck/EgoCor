test_that("distance.info throws an error if input is not a data frame or matrix", {
  data <- list(x = c(1, 3, 5), y = c(2, 4, 6))
  expect_error(distance.info(data), "Input data must be a data frame or matrix")
})

test_that("distance.info throws an error if input has fewer than 3 columns", {
  data <- cbind(x = c(1, 3, 5), y = c(2, 4, 6))
  expect_error(distance.info(data), "subscript out of bounds")
})

test_that("distance.info warns if input contains missing coordinate values or outcome variable", {
  data <- matrix(c(1, NA, 3, 4, 5, 6, NA, 8, 9, NA, 11, 12), ncol = 3)
  expect_warning(distance.info(data), "Data contain 2 rows with missing data")
})

test_that("distance.info throws an error if < 2 complete rows", {
  data <- matrix(c(1, NA, 3, 4, 5, NA, 7, 8, NA), ncol = 3)
  expect_error(distance.info(data), "Cannot calculate distances for < 2 complete rows.")
})

test_that("distance.info warns if input has a column with all missing values", {
  data <- cbind(
    x = c(1, 3, 5),
    y = c(2, 4, 6),
    z = c(NA, NA, NA)
  )
  expect_error(distance.info(data), "Cannot calculate distances for < 2 complete rows.")
})

test_that("distance.info warns if input has more than 3 columns", {
  data <- cbind(
    x = c(1, 3, 5),
    y = c(2, 4, 6),
    z = c(7, 8, 9),
    extra = c(10, 11, 12)
  )
  expect_warning(distance.info(data), "Data matrix contains more than 3 columns")
})


test_that("distance.info executes cleanly with valid input", {
  data <- cbind(
    x = c(1, 3, 5),
    y = c(2, 4, 6),
    z = c(7, 8, 9)
  )
  result <- suppressWarnings(distance.info(data))
  expect_type(result, "list")
})
