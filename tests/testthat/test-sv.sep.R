# warning results from gstat::fit.variogram and is valid


test_that("sv.sep works correctly with valid inputs", {
  # Create mock data
  set.seed(123)
  data <- rnorm(100)
  coords <- cbind(runif(100, 0, 100), runif(100, 0, 100))
  max.dist <- 2000
  nbins <- 13
  fit.method <- 7

  # Run the function
  result <- EgoCor:::sv.sep(data, coords, max.dist, nbins, fit.method)

  # Check if the result is a list containing model parameters and warning status
  expect_warning(EgoCor:::sv.sep(data, coords, max.dist, nbins, fit.method)) # cause of convergence warning in gstat
  expect_true(is.list(result))
  expect_true("mod.pars" %in% names(result))
  expect_true("warning" %in% names(result))
  expect_equal(length(result$mod.pars), 3)
})



