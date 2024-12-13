# Library ----
library(tidyverse)
library(lme4)
library(car)
library(lmerTest)
library(merTools)
library(AICcmodavg)
library(rstanarm)

# Data ----
choice <- read.csv("data/StillwaterChoiceData.csv")

# Change this year to a factor
choice$year <- as.factor(choice$year)

glimpse(choice)

# Standardization of flow variable
choice$flow_s <- as.vector(scale(choice$flow))

hist(choice$flow)
hist(choice$flow_s)

# Make a model ----
flow_mod <- stan_glmer(path ~ flow_s + (1|year),
                  family = binomial,
                  data = choice,
                  cores = 4,
                  iter = 1000, 
                  warmup = 200
                  )

# Statistical significance
Anova(flow_mod, type = "III")
summary(flow_mod)

# Predictions from our model ----
logit_preds <- predictInterval(
    merMod = flow_mod,
    level = 0.95, n.sims = 1000,
    stat = "median", type = "linear.prediction"
    )

#logit_preds <- data.frame(predictSE(flow_mod,newdata = choice, type = "link"))

#logit_preds$lwr <- logit_preds$fit + qnorm(0.025)*logit_preds$se.fit
#logit_preds$upr <- logit_preds$fit + qnorm(0.975)*logit_preds$se.fit

glimpse(logit_preds)

real_preds <- apply(logit_preds, 2, boot::inv.logit)

# Smoosh it
choice_preds <- data.frame(choice, real_preds)

#Now we can graph
ggplot(choice_preds, aes(x = flow, y = path, color = year, fill = year)) +
  geom_point() +
  geom_line(aes(y = fit)) +
  geom_ribbon(aes(xmax = flow, ymin = lwr, ymax = upr, 
                  color = NULL), alpha = 0.20)
