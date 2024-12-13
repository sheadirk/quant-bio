# Data read ----


# Data manipulation ----

# Data summaries ----

# Better data manipulation ----

# The Otsego data ----

# . Library ----
library (tidyverse)
# . Data read ----
otsego <- read.csv(file="data/physical.csv", header = TRUE)

glimpse(otsego)

# . Visual Data Checks ----
hist(otsego$temp)

ggplot(data = otsego, aes(x = temp)) +
  geom_histogram()

# Boxplot of temp by depth
ggplot(data = otsego, aes(x = depth, y = temp, group = depth)) +
  geom_boxplot() +
  coord_flip() +
  scale_x_reverse()

#Data summary plots
#Scatterplot

#Summarize our data
plotter <- otsego %>% 
  group_by(depth, month) %>% 
  summarize(avg = mean(temp),
            lwr = quantile(temp, 0.025),
            upr = quantile(temp, 0.975))

ggplot(data = plotter, aes(x = depth, y = avg,
                           color = factor(month)))+
  geom_line(linewidth = 1.5) +
  geom_point() +
  coord_flip() +
  scale_x_reverse() +
  facet_wrap(~month) +
  ylab("Temperature") +
  xlab("Depth (m)")
 
