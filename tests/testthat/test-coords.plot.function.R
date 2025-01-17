test_that("coords.plot does not throw warnings for valid data input", {
  data <- cbind(
    x = rnorm(10, mean = 0, sd = 20),
    y = rnorm(10, mean = 0, sd = 20),
    value = rnorm(10)
  )
  expect_no_warning(coords.plot(data))
})

test_that("coords.plot does not throw errors for valid data input", {
  data <- cbind(
    x = rnorm(10, mean = 0, sd = 20),
    y = rnorm(10, mean = 0, sd = 20),
    value = rnorm(10)
  )
  expect_no_error(coords.plot(data))
})

test_that("coords.plot throws a warning for more than 3 columns", {
  data <- cbind(
    x = rnorm(10, mean = 0, sd = 20),
    y = rnorm(10, mean = 0, sd = 20),
    value = rnorm(10),
    extra = rnorm(10)
  )
  expect_warning(coords.plot(data), "Data matrix contains more than 3 columns")
})

test_that("coords.plot handles missing coordinate values with a warning", {
  data <- matrix(
    c(
      NA, 1, 2,
      3, 4, 5,
      6, NA, 8,
      9, 10, 11
    ),
    ncol = 3, byrow = TRUE
  )
  expect_warning(coords.plot(data), "Coordinates contain 2 rows with missing data")
})


test_that("coords.plot throws an error if data is not a matrix or data frame", {
  data <- list(
    x = rnorm(10, mean = 0, sd = 20),
    y = rnorm(10, mean = 0, sd = 20),
    value = rnorm(10)
  )
  expect_error(coords.plot(data), "Input data must be a data frame or matrix.")
})


test_that("coords.plot warns about invalid legend position", {
  data <- cbind(
    x = rnorm(10, mean = 0, sd = 20),
    y = rnorm(10, mean = 0, sd = 20),
    value = rnorm(10)
  )
  expect_error(coords.plot(data, legend.pos = "invalid_pos"), "Invalid legend position: should be one of 'none', 'bottomright', 'bottom', 'bottomleft', 'left', 'topleft', 'top', 'topright'")
})
