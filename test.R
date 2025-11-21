# Quick test script
# Just testing some things...

library(ggplot2)

# Read data
d <- read.csv("data_file1.csv")

# Quick plot
plot(d$value, d$score)

# Calculate something
mean(d$value)
sd(d$value)

# Test filter
library(dplyr)
d %>% filter(score > 50)
