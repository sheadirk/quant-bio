# Libraries ----
library(tidyverse)
library(car)
library(boot)
library(AICcmodavg)
library(rstanarm)

# Data ----
choice <- read.csv("data/StillwaterChoiceData.csv",
                    stringsAsFactors = FALSE
                   )

# Data exploration ----
head(choice)

# Are these normal? ----
ggplot(choice, aes(x = path)) +
  geom_histogram()

mean(choice$path)

sd(choice$path)

mean(choice$path) + 1.96 * sd(choice$path)
mean(choice$path) - 1.96 * sd(choice$path)

#Year should be a group cause I only got a few
choice$year <- as.factor(choice$year)

# Fit a model ----
# null_mod <- stan_glm(path ~ 1, data = choice, family = binomial)
# year_mod <- stan_glm(path ~ year, data = choice, family = binomial)
# flow_mod <- stan_glm(path ~ flow, data = choice, family = binomial)
year_flow_mod <- stan_glm(path ~ flow + year, data = choice, family = binomial)
# hatchery_mod <- stan_glm(path ~ hatchery, data = choice, family = binomial)

model_list <- list(null_mod, year_mod, flow_mod, year_flow_mod, hatchery_mod)
model_names <- c("null", "year", "flow", "year_flow", "hatchery")

# Have a look at null model (e.g) ----
summary(null_mod)

p = exp(-1.9206)/ (1 + exp (-1.9206))
p
inv.logit(-1.9206)

# Model selection ----
aictab(cand.set = model_list,
       modnames = model_names
       )
# Diagnostics ----
choice$resids <- year_flow_mod$residuals
choice

ggplot(choice, aes(resids)) +
  geom_histogram() +
  facet_wrap(~year)

# Statistical significance ----
Anova(year_flow_mod, type = "III")

# Predictions ----
lpreds <- data.frame(predict(year_flow_mod, se.fit = TRUE))
lpreds$lwr <- lpreds$fit + qnorm(0.025) * lpreds$se.fit
lpreds$upr <- lpreds$fit + qnorm(0.975) * lpreds$se.fit

rpreds <- apply(lpreds, 2, inv.logit)

# Smash it back together
choice_preds <- data.frame(choice, rpreds)

# Plot the results ----
ggplot(choice_preds, aes(x = flow, y = path)) +
  geom_point() +
  facet_wrap(~year) +
  geom_line(aes(y = fit)) +
  geom_ribbon(aes(xmax = flow, ymin = lwr, ymax = upr,
                  color = NULL), alpha = 0.5)

ggplot(choice_preds, aes(x = flow, y = path, color = year, fill = year)) +
  geom_point() +
  geom_line(aes(y = fit)) +
  geom_ribbon(aes(xmax = flow, ymin = lwr, ymax = upr,
                  color = NULL), alpha = 0.5)


