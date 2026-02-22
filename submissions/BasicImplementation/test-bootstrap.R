library(testthat)
source("bootstrap.R")

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
  # This is a valid input case with a fully predictable output.
  set.seed(99)
  g1 <- rep(0, 30)
  g2 <- rep(5, 30)
  
  out <- two_sample_bootstrap(g1, g2, iterations = 100, stat = "mean")
  
  expect_true(all(out == -5))
})

test_that("mean vs median option returns valid outputs", {
  # Valid input where mean and median are meaningfully different.
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

#ADDITIONAL TEST (not necessary)
test_that("function runs on the project dataset (smoke test)", {
  # Smoke test = “it runs and returns sensible numeric output” on real data.
  df <- read.csv("data.csv", check.names = FALSE)
  
  male_svl <- df$`Snout-vent length (mm)`[df$Sex == "M"]
  fem_svl  <- df$`Snout-vent length (mm)`[df$Sex == "F"]
  
  male_svl <- male_svl[!is.na(male_svl)]
  fem_svl  <- fem_svl[!is.na(fem_svl)]
  
  set.seed(7)
  out <- two_sample_bootstrap(male_svl, fem_svl, iterations = 300, stat = "mean")
  
  expect_length(out, 300)
  expect_type(out, "double")
  expect_true(all(is.finite(out)))
})
