test_that("vario.reg.prep handles non-lm or non-lmerMod objects", {
  expect_error(vario.reg.prep("not_a_model"),
               "Argument reg has to be an object of class lm or lmerMod.")
})

test_that("vario.reg.prep throws error when no data stored in lm/lmerMod object and no data arg", {
  mod = lm(birth$birthweight ~ birth$primiparous + birth$datediff + birth$bmi
            + birth$inc)
  expect_error(vario.reg.prep(mod),"The geo-coded dataset used for regression cannot be recalled from the current environment.")
})


# ------ hier weiter

test_that("vario.reg.prep handles lm model with insufficient columns", {
  mod <- lm(mpg ~ hp + wt, data = mtcars)
  expect_error(vario.reg.prep(mod, data = mtcars[, 1:2]),
               "subscript out of bounds")
})

test_that("vario.reg.prep returns correct structure for lm model", {
  data <- data.frame(
    x = runif(10, 0, 100),
    y = runif(10, 0, 100),
    outcome = rnorm(10),
    covariate = rnorm(10)
  )
  mod <- lm(outcome ~ covariate, data = data)
  result <- vario.reg.prep(mod)

  expect_s3_class(result, "data.frame")
  expect_named(result, c("x", "y", "adj"))
  expect_equal(nrow(result), nrow(data))
})

test_that("vario.reg.prep calculates correct studentized residuals for lm model", {
  data <- data.frame(
    x = runif(10, 0, 100),
    y = runif(10, 0, 100),
    outcome = rnorm(10),
    covariate = rnorm(10)
  )
  mod <- lm(outcome ~ covariate, data = data)
  result <- vario.reg.prep(mod)

  expect_true(all(result$adj == stats::rstudent(mod)))
})

test_that("vario.reg.prep handles missing data in lm model", {
  data <- data.frame(
    x = runif(10, 0, 100),
    y = runif(10, 0, 100),
    outcome = rnorm(10),
    covariate = rnorm(10)
  )
  data[1, "covariate"] <- NA # Add missing data

  mod <- lm(outcome ~ covariate, data = data)
  result <- vario.reg.prep(mod)

  # Check that missing data row is excluded from the result
  expect_equal(nrow(result), sum(complete.cases(data)))
})

test_that("vario.reg.prep returns correct structure for lmerMod model", {
  if (requireNamespace("lme4", quietly = TRUE)) {
    library(lme4)
    data <- sleepstudy
    mod <- lmer(Reaction ~ Days + (1|Subject), data = data)
    result <- vario.reg.prep(mod)

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
    result <- vario.reg.prep(mod)

    expect_true(all(result$adj == stats::rstudent(mod)))
  }
})

test_that("vario.reg.prep prints the correct message", {
  data <- data.frame(
    x = runif(10, 0, 100),
    y = runif(10, 0, 100),
    outcome = rnorm(10),
    covariate = rnorm(10)
  )
  mod <- lm(outcome ~ covariate, data = data)

  # Capture the output of the function
  expect_message(vario.reg.prep(mod),
                 "Message:\nAre the columns in the dataset used for regression in correct order?")
})

test_that("vario.reg.prep works with lm model and dataset provided in regression", {
  data <- data.frame(
    x = runif(10, 0, 100),
    y = runif(10, 0, 100),
    outcome = rnorm(10),
    covariate = rnorm(10)
  )
  mod <- lm(outcome ~ covariate, data = data)
  result <- vario.reg.prep(mod)

  expect_equal(result$x, data$x)
  expect_equal(result$y, data$y)
})

test_that("vario.reg.prep works with lmerMod model and dataset provided in regression", {
  if (requireNamespace("lme4", quietly = TRUE)) {
    library(lme4)
    data <- sleepstudy
    mod <- lmer(Reaction ~ Days + (1|Subject), data = data)
    result <- vario.reg.prep(mod)

    expect_equal(result$x, data$Days)
    expect_equal(result$y, data$Subject)
  }
})

test_that("vario.reg.prep works with lm model and dataset provided manually", {
  data <- data.frame(
    x = runif(10, 0, 100),
    y = runif(10, 0, 100),
    outcome = rnorm(10),
    covariate = rnorm(10)
  )
  mod <- lm(outcome ~ covariate)
  result <- vario.reg.prep(mod, data = data)

  expect_equal(result$x, data$x)
  expect_equal(result$y, data$y)
})

test_that("vario.reg.prep works with lmerMod model and dataset provided manually", {
  if (requireNamespace("lme4", quietly = TRUE)) {
    library(lme4)
    data <- sleepstudy
    mod <- lmer(Reaction ~ Days + (1|Subject))
    result <- vario.reg.prep(mod, data = data)

    expect_equal(result$x, data$Days)
    expect_equal(result$y, data$Subject)
  }
})
