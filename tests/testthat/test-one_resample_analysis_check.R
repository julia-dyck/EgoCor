# warning results from gstat::fit.variogram and is valid

test_that("one_resample_analysis_check works correctly with valid inputs", {
  # Set a random seed for reproducibility
  set.seed(123)

  # Generate mock data
  y.iid <- rnorm(100)  # Example random data for y.iid
  coords <- cbind(runif(100, 0, 100), runif(100, 0, 100))  # Random coordinates
  max.dist <- 50  # Max distance for variogram calculation
  nbins <- 10  # Number of bins for variogram
  threshold.factor <- c(1.1, 1.2)  # Example threshold factors for model checks

  # Generate normal scores object using nscore function
  nscore.obj <- EgoCor:::nscore(y.iid)

  # Generate the L matrix for resampling (identity matrix in this example)
  L <- diag(length(y.iid))

  # Run the analysis function
  result <- EgoCor:::one_resample_analysis_check(platzhalter = NULL,  # placeholder is not used in the function
                                        y.iid = y.iid,
                                        L = L,
                                        nscore.obj = nscore.obj,
                                        coords = coords,
                                        max.dist = max.dist,
                                        nbins = nbins,
                                        threshold.factor = threshold.factor)

  # Check that the result is a vector of length 6 (3 estimates + 1 convergence + 2 threshold checks)
  expect_true(length(result) == 4 + length(threshold.factor))

  # Check that the first three elements are numeric values representing model estimates
  expect_true(is.numeric(result[1:3]))

  # Check that the threshold outcomes are properly calculated (0 or 1)
  expect_true(all(result[5:length(result)] %in% c(0, 1)))

  # Check that the model variance (mod.var) is properly calculated and not NaN
  mod.var <- result[1] + result[2]
  expect_true(!is.na(mod.var))
  expect_true(mod.var > 0)  # Variance should be positive

})
