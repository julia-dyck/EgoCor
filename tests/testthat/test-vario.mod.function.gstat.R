# warnings result from gstat::fit.variogram and are valid

# shiny app test interrupts tests until shiny window closed and is therefore not included in this test script

# pdf generation test is not included in this script
# (instead, generate a pdf and check whether formatting fits the expectations manually)


# test input validation


# test_that("vario.mod throws error if max.dist and nbins input do not match", {
#   expect_error(vario.mod(data = data.frame(x = 1:10, y = 1:10, z = 1:10), max.dist = c(100, 200), nbins = c(10,11,12)),
#                "If vectors for both input parameters max.dist and nbins are specified, they must have the same length")
#   expect_error(vario.mod(data = data.frame(x = 1:10, y = 1:10, z = 1:10), max.dist = c(100, 200, 300), nbins = c(10,11)),
#                "If vectors for both input parameters max.dist and nbins are specified, they must have the same length")
#
# })
#
# test_that("vario.mod works with single max.dist and nbins", {
#   result <- suppressWarnings(vario.mod(data = data.frame(x = 1:10, y = 1:10, z = 1:10), max.dist = 1000, nbins = 5, shinyresults = F))
#   expect_equal(nrow(result$infotable), 1)
# })
#
# test_that("vario.mod works with multiple max.dist and nbins", {
#   result <- suppressWarnings(vario.mod(data = data.frame(x = 1:10, y = 1:10, z = 1:10), max.dist = c(1000, 2000), nbins = c(5, 10), shinyresults = F))
#   expect_equal(nrow(result$infotable), 2)
# })
#
#
# # output validation
#
#
# test_that("vario.mod returns output with correct class", {
#   expect_s3_class(suppressWarnings(vario.mod(data = birth, shinyresults = F)), "vario.mod.output")
# })
#
#
# test_that("vario.mod returns output with correct output elements", {
#   expect_named(suppressWarnings(vario.mod(data = birth, shinyresults = F)), c("infotable", "variog.list", "vmod.list", "input.arguments", "call"))
# })

