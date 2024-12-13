# Library ----
library(tidyverse)
library(car)

# Load a dataset ----
data(iris)

# Look at the data ----
glimpse(iris)

ggplot(iris, aes(x = Sepal.Length)) +
  geom_histogram(color = "gray87") +
  facet_wrap(~Species)

# ANOVA ----
# Fit the model
iris_anova <- lm(Sepal.Length ~ Species,
                 data = iris)
# factor-level Significance
anova(iris_anova)

# y = mx + b and r-squared
summary(iris_anova)

# Make predictions
anova_preds <- predict(iris_anova,
                       interval = "confidence"
                       )

#Combine preds with raw data
iris_preds <- data.frame(iris, anova_preds)

#Plot predictions
ggplot(iris_preds, aes(x = Species, y = Sepal.Length)) +
  geom_boxplot(notch = TRUE, width = .3) +
  geom_point(aes(y = fit), size = 4, color = "gray40") +
  geom_errorbar(aes(xmax = Species, ymin = lwr, ymax = upr),
                width = 0, linewidth = 1, color = "turquoise2")

# Linear regression ----
iris_regression <- lm(Sepal.Length ~ Petal.Length,
                      data = iris)

# Statistical significance
anova(iris_regression)

# y = mx + b and r-squared
summary(iris_regression)

# Make predictions with regression
reg_preds <- predict(iris_regression, interval = "confidence")

iris_predsa <- data.frame(iris, reg_preds)

#Plot predictions
ggplot(iris_predsa, aes(x = Petal.Length, y = Sepal.Length)) +
  geom_point() +
  geom_line (aes(y = fit)) +
  geom_ribbon(aes(xmax = Petal.Length, ymin = lwr, ymax = upr, 
                  color = NULL),
              alpha = 0.10)
#This could be applied to Baseball Savant data!!!

# ANCOVA ----
iris_ancova <- lm(Sepal.Length ~ Species * Petal.Length, 
                  data = iris)

# Factor-level significance
Anova(iris_ancova, type = "III")

# Make predictions
ancova_preds <- predict(iris_ancova, interval = "confidence")
iris_predsb <- data.frame(iris, ancova_preds)

# Plot predictions against raw data
ggplot(iris_predsb, aes(x = Petal.Length, y = Sepal.Length)) +
  geom_point() +
  geom_line(aes(y = fit)) +
  geom_ribbon(aes(xmax = Petal.Length, ymin = lwr, ymax = upr, color = NULL), 
              alpha = 0.5) +
  facet_wrap(~Species, scales = "free_x")

