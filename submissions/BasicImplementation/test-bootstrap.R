library(testthat)

test_that("two_sample_bootstrap returns a numeric vector of the right length", {
  set.seed(1)
  g1 <- c(1, 2, 3, 4, 5)
  g2 <- c(10, 20, 30, 40, 50)

  out <- two_sample_bootstrap(g1, g2, iterations = 200, stat = "mean")

  expect_type(out, "double")
  expect_length(out, 200)
  expect_true(all(is.finite(out)))
})

test_that("two_sample_bootstrap is reproducible under set.seed()", {
  g1 <- rnorm(20)
  g2 <- rnorm(20)

  set.seed(123)
  out1 <- two_sample_bootstrap(g1, g2, iterations = 100, stat = "mean")

  set.seed(123)
  out2 <- two_sample_bootstrap(g1, g2, iterations = 100, stat = "mean")

  expect_equal(out1, out2)
})

test_that("constant groups give a constant bootstrap difference (mean)", {
  set.seed(99)
  g1 <- rep(0, 30)
  g2 <- rep(5, 30)

  out <- two_sample_bootstrap(g1, g2, iterations = 100, stat = "mean")

  # stat(group1) - stat(group2) = 0 - 5 = -5
  expect_true(all(out == -5))
})

test_that("constant groups give a constant bootstrap difference (median)", {
  set.seed(99)
  g1 <- rep(0, 30)
  g2 <- rep(5, 30)

  out <- two_sample_bootstrap(g1, g2, iterations = 100, stat = "median")

  # median(group1) - median(group2) = 0 - 5 = -5
  expect_true(all(out == -5))
})

test_that("mean vs median option returns valid outputs", {
  set.seed(42)
  g1 <- c(0, 0, 0, 0, 100)
  g2 <- c(0, 0, 0, 0, 0)

  out_mean <- two_sample_bootstrap(g1, g2, iterations = 200, stat = "mean")
  out_med  <- two_sample_bootstrap(g1, g2, iterations = 200, stat = "median")

  expect_type(out_mean, "double")
  expect_type(out_med, "double")
  expect_length(out_mean, 200)
  expect_length(out_med, 200)
  expect_true(all(is.finite(out_mean)))
  expect_true(all(is.finite(out_med)))
})

test_that("function runs on the project dataset (smoke test)", {
  # repo-root path
  data("lizard_data", package = "sta380", envir = environment())
  df <- lizard_data

  # Dynamically find the sex and snout length columns to avoid formatting errors
  sex_col <- grep("sex", colnames(df), ignore.case = TRUE, value = TRUE)[1]
  len_col <- grep("length", colnames(df), ignore.case = TRUE, value = TRUE)[1]

  male_svl <- df[[len_col]][df[[sex_col]] == "M"]
  fem_svl  <- df[[len_col]][df[[sex_col]] == "F"]

  male_svl <- male_svl[!is.na(male_svl)]
  fem_svl  <- fem_svl[!is.na(fem_svl)]

  set.seed(7)
  out <- two_sample_bootstrap(male_svl, fem_svl, iterations = 300, stat = "mean")

  expect_length(out, 300)
  expect_type(out, "double")
  expect_true(all(is.finite(out)))
})
