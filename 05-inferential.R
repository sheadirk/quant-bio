# Library ----
library(tidyverse)

# Data of week ----
otsego <- read.csv("data/physical.csv", 
                   stringsAsFactors = FALSE
                   )

# Data exploration ----
glimpse(otsego)

# Data manipulation ----
# We want bottom water in October
# We also need our indicator of when alewife were present.

#Alewife presence/absence
otsego$alewife <- "present"
otsego$alewife[otsego$year > 2010] <- "absent"

unique(otsego$alewife)
length(unique(otsego$alewife))

#Filter out just the data we want
hypo <- otsego %>% 
  filter(depth > 40, month == 10)

#Statistical summaries
means <- hypo %>% 
  group_by(alewife) %>% 
  summarize(avg = mean (do_mgl), 
            sds = sd (do_mgl)
            )

means

# Overall mean 
mean(hypo$do_mgl)

# Do some inferential stats ----
do_test <- t.test(
  do_mgl ~ alewife, data = hypo,
  conf.level = 0.95,
  var.equal = FALSE
  )

do_test

# Check yourself ----
# . Normality ----
# If I was doin g a one-sample test I would check the whole sampling dist
# for normality
shapiro.test(
  hypo$do_mgl
)
# For Shapiro-Wilk- H0: my sample dist is normal
shapiro.test(hypo$do_mgl[hypo$alewife == "present"])
shapiro.test(hypo$do_mgl[hypo$alewife == "absent"])

# .. Variances ----
var.test(do_mgl ~ alewife, data = hypo)

# Non-parametric alternatives ----
wilcox.test(do_mgl ~ alewife, data = hypo)

# Plotting the results ----
ggplot(hypo, aes(x = alewife, y = do_mgl)) +
  geom_boxplot()
# Boxplot lines in R are the 1.5x interquartile range, not 95th percentile :(

ggplot(hypo, aes(x = alewife, y = do_mgl)) +
  geom_violin()
