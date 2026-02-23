library(testthat)
source("bootstrap.R")

# Test 1: Check basic output structure
# This makes sure the function returns:
# - numeric values
# - the correct number of bootstrap samples
# - no missing or infinite values
# This checks that the function runs properly for a normal input.
test_that("two_sample_bootstrap returns a numeric vector of the right length", {
  set.seed(1)
  g1 <- c(1, 2, 3, 4, 5)
  g2 <- c(10, 20, 30, 40, 50)
  
  out <- two_sample_bootstrap(g1, g2, iterations = 200, stat = "mean")
  
  expect_type(out, "double")
  expect_length(out, 200)
  expect_true(all(is.finite(out)))
})

# Test 2: Check reproducibility
# If we use the same random seed, we should get the same bootstrap results.
test_that("two_sample_bootstrap is reproducible under set.seed()", {
  g1 <- rnorm(20)
  g2 <- rnorm(20)
  
  set.seed(123)
  out1 <- two_sample_bootstrap(g1, g2, iterations = 100, stat = "mean")
  
  set.seed(123)
  out2 <- two_sample_bootstrap(g1, g2, iterations = 100, stat = "mean")
  
  expect_equal(out1, out2)
})

# Test 3: Constant groups
# If both groups contain only constant values,
# every bootstrap resample will also be constant.
# So the difference should always be the same value.
# This checks that the difference is computed correctly.
test_that("constant groups give a constant bootstrap difference (mean)", {
  set.seed(99)
  g1 <- rep(0, 30)
  g2 <- rep(5, 30)
  
  out <- two_sample_bootstrap(g1, g2, iterations = 100, stat = "mean")
  
  # Function returns stat(group1) - stat(group2) = 0 - 5 = -5
  expect_true(all(out == -5))
})

# Test 4: Constant groups (median case)
# Same idea as above, but for the median option.
# This confirms that the median branch of the function works correctly.
test_that("constant groups give a constant bootstrap difference (median)", {
  set.seed(99)
  g1 <- rep(0, 30)
  g2 <- rep(5, 30)
  
  out <- two_sample_bootstrap(g1, g2, iterations = 100, stat = "median")
  
  # Median(group1) - Median(group2) = 0 - 5 = -5
  expect_true(all(out == -5))
})

# Test 5: Mean vs median option works
# This checks that both "mean" and "median" options:
# - return numeric vectors
# - return the correct number of bootstrap samples
# - do not produce invalid values
# This ensures both statistic options run correctly.
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

# Test 6: 
# This checks that the function works on the actual project data.
# It does not check exact values — only that:
# - the function runs
# - the output has the correct length
# - the output is numeric and valid
test_that("function runs on the project dataset", {
  
  # Only run if the file exists (prevents failure if data.csv is missing)
  skip_if_not(file.exists("data.csv"))
  
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
