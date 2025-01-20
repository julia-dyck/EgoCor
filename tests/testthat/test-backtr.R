test_that("backtr works with basic inputs", {
  # Example input
  scores <- c(-1, 0, 1)
  nscore_obj <- list(
    trn.table = data.frame(
      x = c(1, 2, 3, 4, 5),
      nscore = c(-1.5, -0.5, 0, 0.5, 1.5)
    )
  )
  result <- EgoCor:::backtr(scores, nscore_obj, tails = "none", draw = FALSE)

  expect_equal(length(result), length(scores))
  expect_true(all(result >= min(nscore_obj$trn.table$x) &
                    result <= max(nscore_obj$trn.table$x)))
})


