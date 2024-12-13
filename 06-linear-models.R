# Libraries ----
library(tidyverse)
library(car)

# ANOVA -----
# . Data ----
data(iris)
glimpse(iris)

# How many unique groups?
unique(iris$Species)

# Quick exploratory look at data
ggplot(iris, aes(x = Species, y = Petal.Width)) +
  geom_violin()

# . Model fitting ----
iris_mod <- lm(Petal.Width ~ Species, data = iris)

# Default print methods
print(iris_mod)

# How about this?
summary(iris_mod)

# Can we sanity check this? 
mean(iris$Petal.Width[iris$Species == "setosa"])

mean(iris$Petal.Width[iris$Species == "versicolor"])
.246 + 1.080

# . Assess statistical significance ----
anova(iris_mod)

#Group-specific differences
TukeyHSD(aov(iris_mod))

# . Make predictions and compare to raw data ----
preds <- data.frame(
  predict(iris_mod, interval = "confidence"))

iris_preds <- data.frame(iris, preds)

#..Plot those predictions ----
ggplot(iris_preds, aes(x = Species, y = Petal.Width)) +
  geom_violin() +
  geom_jitter(width = .1, color = "gray") +
  geom_point(aes(y = fit), size = 4) +
  geom_errorbar(aes(xmax = Species, ymin = lwr, ymax = upr),
                width = 0, linewidth = 1
                )

#.. Gets and predictions for each species ----

iris_preds %>% 
  group_by(Species) %>% 
  summarize(
    fit = mean(fit),
    lwr = mean(lwr),
    upr = mean(upr)
  )

# Here are my observed means and CIs

iris_preds %>% 
  group_by(Species) %>% 
  summarize(
    fit = mean(Petal.Width),
    lwr = mean(Petal.Width, 0.025),
    upr = mean(Petal.Width, 0.975)
  )

# Linear regression -----
# . Data ----
beans <- read.csv("data/beanvolution.csv")
glimpse(beans)

ggplot(beans, aes(x = Generation, y = N)) +
  geom_point() +
  facet_wrap(~Phenotype)

#Data Cleaning
beans$Phenotype <- tolower(beans$Phenotype)

kidneys <- beans %>% 
  filter(Phenotype == "kidney",
         !is.na (N)
         )

# . Model fitting ----
k_mod <- lm(N ~ Generation, data = kidneys)

# . Assess statistical significance ----
anova(k_mod)

summary(k_mod)

# . Make predictions and compare to raw data ----
preds <- data.frame(
  predict(k_mod, interval = "confidence"))

k_preds <- data.frame(kidneys, preds)

ggplot(k_preds, aes(x = Generation, y = N)) +
  geom_point() +
  geom_line(aes(y = fit)) +
  geom_ribbon(aes(xmax = Generation,
                  ymin = lwr, ymax = upr),
              alpha = 0.25
              )


# Analysis of covariance (ANCOVA) -----
# . Data ----
#Need to drop data points that are missing values for response
beans <- beans %>% 
  filter(!is.na(N))
# . Model fitting ----
#This model assumes all groups change the same way
bean_nova <- lm(N ~ Generation + Phenotype, data = beans)

#This model will assume that groups can change differently
bean_nova <- lm(N ~ Generation * Phenotype, data = beans)

# . Assess statistical significance ----
Anova(bean_nova, type = "III")
summary(bean_nova)

# . Make predictions and compare to raw data ----
preds <- data.frame(
  predict( bean_nova, interval = "confidence")
)

bean_preds <- data.frame(beans, preds)

ggplot(bean_preds, aes(x = Generation, y = N,
                       color = Phenotype, fill = Phenotype)) +
  geom_point()+
  geom_line(aes(y = fit))+
  geom_ribbon(aes(xmax = Generation, ymin = lwr, ymax = upr, 
                  color = NULL), 
              alpha = 0.10
              )

