#' Two-Sample Bootstrap Method
#'
#' @description Calculates the bootstrap distribution of the difference between
#' two biological groups using either the difference in means or the difference in medians.
#'
#'@param resample_length A description of what this parameter does.
#' @param group1 A numeric vector representing the first biological sample (e.g., Male Snout Length).
#' @param group2 A numeric vector representing the second biological sample (e.g., Female Snout Length).
#' @param iterations A numeric value specifying the number of bootstrap samples to generate. Default is 1000.
#' @param stat A character string specifying the statistic of interest: either "mean" or "median". Default is "mean".
#'
#' @return A numeric vector containing the bootstrap distribution of the differences (`group1` - `group2`).
#'
#' @examples
#' set.seed(123)
#' # Simulate some fake lizard snout length data
#' male_lizards <- rnorm(30, mean = 45, sd = 5)
#' female_lizards <- rnorm(30, mean = 50, sd = 5)
#'
#' # Run the bootstrap function for difference in means
#' boot_results <- two_sample_bootstrap(male_lizards, female_lizards, iterations = 1000, stat = "mean")
#'
#' @importFrom stats median
#' @export
two_sample_bootstrap <- function(group1, group2,
                                 iterations = 1000, stat = "mean",
                                 resample_length = NULL) {


  boot_diffs <- numeric(iterations) # Pre-allocation

  # Determine which statistic function to use based on user input
  #if (stat == "median") {
  #  stat_func <- median
  #} else {
  #  stat_func <- mean
  #}

  ## Anna: this is shorter, more elegant
  stat_func <- if(stat == "median") median else mean

  ## Anna: I added this portion, because for the bootstrap
  ## you may want to add flexibility regarding the re-sample size.
  ## you should also document it because I won't for you lol
  if(is.null(resample_length)){
    resample_length = c(length(group1), length(group2))
  }

  for (i in 1:iterations) {
    # Note that set replace = TRUE
    resample1 <- sample(group1, size = resample_length[1], replace = TRUE)
    resample2 <- sample(group2, size = resample_length[2], replace = TRUE)

    # Calculate the difference in the chosen statistic and store it
    boot_diffs[i] <- stat_func(resample1) - stat_func(resample2)
  }

  return(boot_diffs)
}
