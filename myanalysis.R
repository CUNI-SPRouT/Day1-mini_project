# My Analysis Script
# Created: sometime in 2024?
# Author: me

# Load some packages
library(ggplot2)
require(dplyr)

# Set working directory - CHANGE THIS TO YOUR PATH!!!
setwd("C:/Users/John/Documents/My_Projects/analysis_project_final_FINAL")

# Read the data - make sure the file is there!
data1 <- read.csv("../Desktop/data_file1.csv")

# Load more packages
library(tidyr)

# Read another data file
data2 <- read.csv("C:/Users/John/Downloads/measurements.csv")

# Some data processing
data1$new_column <- data1$value * 2.5
data1$category <- ifelse(data1$score > 50, "high", "low")

# Calculate mean
mean_value <- mean(data1$value, na.rm = TRUE)
print(paste("Mean is:", mean_value))

# Load another package
library(readr)

# Merge datasets
merged <- merge(data1, data2, by = "id", all.x = TRUE)

# Some more processing
merged$ratio <- merged$value / merged$measurement
merged$log_value <- log(merged$value + 1)

# Create a plot
p <- ggplot(merged, aes(x = value, y = measurement, color = category)) +
  geom_point(size = 3) +
  theme_minimal() +
  labs(title = "My Plot")

# Save plot - REMEMBER TO CREATE THE FOLDER FIRST!
ggsave("C:/Users/John/Desktop/plot1_final_version2.png", p, width = 10, height = 6)

# Calculate summary statistics
summary_stats <- merged %>%
  group_by(category) %>%
  summarise(
    mean_val = mean(value, na.rm = TRUE),
    sd_val = sd(value, na.rm = TRUE),
    n = n()
  )

# Print results
print(summary_stats)

# Save results
write.csv(summary_stats, "C:/Users/John/Documents/results_new.csv", row.names = FALSE)

# More analysis - fit a linear model
model <- lm(measurement ~ value + category, data = merged)
summary(model)

# Save model results
sink("C:/Users/John/Desktop/model_output.txt")
print(summary(model))
sink()

# Create another plot
p2 <- ggplot(merged, aes(x = category, y = ratio)) +
  geom_boxplot(fill = "lightblue") +
  theme_classic()

# Save it somewhere
ggsave("boxplot_thing.png", p2)

# Load one more package because we need it now
library(scales)

# Final calculations
final_data <- merged %>%
  filter(!is.na(ratio)) %>%
  mutate(
    normalized_value = (value - min(value)) / (max(value) - min(value)),
    percentile = percent_rank(value)
  )

# Export everything
write.csv(final_data, "final_results_DO_NOT_DELETE.csv", row.names = FALSE)
write.csv(merged, "merged_data_backup_v3.csv", row.names = FALSE)

# Done! (I think...)
print("Analysis complete!")
