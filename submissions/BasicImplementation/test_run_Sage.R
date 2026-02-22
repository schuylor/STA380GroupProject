library(readr)
source("bootstrap.R")
# There is a Celcius symbol that crushes everything, use this line to load data
lizard_data <- read.csv("data.csv", fileEncoding = "latin1", check.names = FALSE)

male_svl <- na.omit(lizard_data$`Snout-vent length (mm)`[lizard_data$Sex == "M"])
female_svl <- na.omit(lizard_data$`Snout-vent length (mm)`[lizard_data$Sex == "F"])

boot_results <- two_sample_bootstrap(male_svl, female_svl, iterations = 10000, stat = "mean")

hist(boot_results,
     breaks = 50,
     col = "skyblue",
     border = "white",
     main = "Bootstrap Distribution of Difference in Means (Male vs Female)",
     xlab = "Difference in Snout Length (mm)",
     freq = FALSE)

