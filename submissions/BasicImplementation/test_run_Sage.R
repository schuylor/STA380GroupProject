source("bootstrap.R")

lizard_data <- read.csv("../../data/dataset.xlsx - Size_sex_reproductive_status_an.csv")

male_svl <- na.omit(lizard_data$Snout_vent_length[lizard_data$Sex == "Male"])
female_svl <- na.omit(lizard_data$Snout_vent_length[lizard_data$Sex == "Female"])

boot_results <- two_sample_bootstrap(male_svl, female_svl, iterations = 10000, stat = "mean")

hist(boot_results,
     breaks = 50,
     col = "skyblue",
     border = "white",
     main = "Bootstrap Distribution of Difference in Means (Male vs Female)",
     xlab = "Difference in Snout Length (mm)",
     freq = FALSE)