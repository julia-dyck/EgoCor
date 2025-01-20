test_that("vario.reg.prep handles non-lm or non-lmerMod objects", {
  expect_error(vario.reg.prep("not_a_model"),
               "Argument reg has to be an object of class lm or lmerMod.")
})

test_that("vario.reg.prep throws error when no data stored in lm/lmerMod object and no data arg", {
  mod = lm(birth$birthweight ~ birth$primiparous + birth$datediff + birth$bmi
           + birth$inc)
  expect_error(vario.reg.prep(mod),"The geo-coded dataset used for regression cannot be recalled from the current environment.")
})


test_that("vario.reg.prep returns correct structure for lm model", {
  x = runif(10, 0, 100)
  y = runif(10, 0, 100)
  covariate = rnorm(10)
  outcome = 2*covariate + rnorm(10)
  data = data.frame(x, y, covariate, outcome)

  mod <- lm(outcome ~ covariate)
  result <- vario.reg.prep(mod, data = data)

  expect_s3_class(result, "data.frame")
  expect_named(result, c("x", "y", "adj"))
  expect_equal(nrow(result), nrow(data))
})

test_that("vario.reg.prep calculates correct studentized residuals for lm model", {
  x = runif(10, 0, 100)
  y = runif(10, 0, 100)
  covariate = rnorm(10)
  outcome = 2*covariate + rnorm(10)
  data = data.frame(x, y, covariate, outcome)

  mod <- lm(outcome ~ covariate)
  result <- vario.reg.prep(mod, data = data)

  expect_true(all(result$adj == stats::rstudent(mod)))
})

test_that("vario.reg.prep handles missing data in lm model", {
  x = runif(10, 0, 100)
  y = runif(10, 0, 100)
  covariate = rnorm(10)
  outcome = 2*covariate + rnorm(10)
  data = data.frame(x, y, covariate, outcome)
  data[1, "covariate"] <- NA # Add missing data

  mod <- lm(outcome ~ covariate, data)
  result <- vario.reg.prep(mod, data = data)

  # Check that missing data row is excluded from the result
  expect_equal(nrow(result), sum(complete.cases(data)))
})

test_that("vario.reg.prep returns correct structure for lmerMod model", {
  if (requireNamespace("lme4", quietly = TRUE)) {
    library(lme4)
    data <- sleepstudy
    mod <- lmer(Reaction ~ Days + (1|Subject), data = data)
    result <- vario.reg.prep(mod, data)

    expect_s3_class(result, "data.frame")
    expect_named(result, c("x", "y", "adj"))
    expect_equal(nrow(result), nrow(data))
  }
})

test_that("vario.reg.prep calculates correct studentized residuals for lmerMod model", {
  if (requireNamespace("lme4", quietly = TRUE)) {
    library(lme4)
    data <- sleepstudy
    mod <- lmer(Reaction ~ Days + (1|Subject), data = data)
    result <- vario.reg.prep(mod, data = data)

    expect_true(all(result$adj == stats::rstudent(mod)))
  }
})


