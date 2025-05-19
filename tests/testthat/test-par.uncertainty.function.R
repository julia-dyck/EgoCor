# warnings result from gstat::fit.variogram and are valid


test_that("par.uncertainty throws an error if necessary input argument is missing", {

  # if variogram.model.output is chosen:
  expect_error(par.uncertainty())

  # if manual typing of variogram estimate is chosen:
  # Missing data input
  expect_error(par.uncertainty(par.est = c(1021.812, 225440.3, 0), max.dist = 1000, nbins = 13, B = 10),
               "One approach regarding the input arguments has to be chosen.
  and arguments have to be provided accordingly.")

  # Missing max.dist input
  expect_error(par.uncertainty(par.est = c(1021.812, 225440.3, 0), data = birth, nbins = 13, B = 10),
               "One approach regarding the input arguments has to be chosen.
  and arguments have to be provided accordingly.")

  # Missing nbins input
  expect_error(par.uncertainty(par.est = c(1021.812, 225440.3, 0), data = birth, max.dist = 1000, B = 10),
               "One approach regarding the input arguments has to be chosen.
  and arguments have to be provided accordingly.")
})


test_that("par.uncertainty throws a warning for more than 3 columns", {
  expect_warning(par.uncertainty(par.est = c(1021.812, 225440.3, 0),
                                 data = birth, max.dist = 1000, nbins = 13, B = 10),
  "Data matrix contains more than 3 columns")
})







