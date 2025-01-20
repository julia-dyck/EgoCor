# warnings result from gstat::fit.variogram and are valid

# shiny app test interrupts tests until shiny window closed and is therefore not included in this test script

# pdf generation test is not included in this script
# (instead, generate a pdf and check whether formatting fits the expectations manually)


# test input validation

test_that("vario.mod throws warning for more than 3 columns of input data", {
  expect_warning(vario.mod(data = birth, max.dist = 2000, shinyresults = F),
                   "Data matrix contains more than 3 columns.")
})

test_that("vario.mod throws error if max.dist and nbins input do not match", {
  expect_error(vario.mod(data = data.frame(x = 1:10, y = 1:10, z = 1:10), max.dist = c(100, 200), nbins = c(10,11,12)),
               "If vectors for both input parameters max.dist and nbins are specified, they must have the same length")
  expect_error(vario.mod(data = data.frame(x = 1:10, y = 1:10, z = 1:10), max.dist = c(100, 200, 300), nbins = c(10,11)),
               "If vectors for both input parameters max.dist and nbins are specified, they must have the same length")

})

test_that("vario.mod works with single max.dist and nbins", {
  result <- vario.mod(data = data.frame(x = 1:10, y = 1:10, z = 1:10), max.dist = 1000, nbins = 5, shinyresults = F)
  expect_equal(nrow(result$infotable), 1)
})

test_that("vario.mod works with multiple max.dist and nbins", {
  result <- vario.mod(data = data.frame(x = 1:10, y = 1:10, z = 1:10), max.dist = c(1000, 2000), nbins = c(5, 10), shinyresults = F)
  expect_equal(nrow(result$infotable), 2)
})


# output validation


test_that("vario.mod returns output with correct class and structure", {
  result <- vario.mod(data = birth, shinyresults = F)
  expect_s3_class(result, "vario.mod.output")
  expect_named(result, c("infotable", "variog.list", "vmod.list", "input.arguments", "call"))
})

