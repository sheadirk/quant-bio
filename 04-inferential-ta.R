# Libraries ----
library(tidyverse)

# Data ----
grasscarp <- read.csv("data/grasscarp.csv")

glimpse(grasscarp)

# one sample t
mean_length <- mean(grasscarp$Length)

one_sample_ttest <- t.test(
  x = grasscarp$Length[grasscarp$Year == 2006],
  mu = mean_length
)

one_sample_ttest

## Uh oh we assumed normal distribution :/

hist(grasscarp$Length[grasscarp$Year == 2006])
