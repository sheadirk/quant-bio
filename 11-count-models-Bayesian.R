# Library ----
library(tidyverse)
library(car)
library(AICcmodavg)
library(MASS)
library(rstanarm)

# Data ----
crabs <- read.csv("data/crabs.csv")

# Have a look!
glimpse(crabs)

# Check out response
ggplot(crabs, aes(satellites)) +
  geom_histogram()

min(crabs$satellites)
max(crabs$satellites)
mean(crabs$satellites)

# Maybe by groups?
crabs$color <- as.factor(crabs$color)

ggplot(crabs, aes(x = color, y = satellites)) +
  geom_boxplot()

# Some candidate models ----
poiss_fit <- stan_glm(satellites ~ mass,
                 data = crabs,
                 family = poisson)

nb_fit <- stan_glm(satellites ~ mass,
              data = crabs, family = neg_binomial_2())


#AIC(poiss_fit, nb_fit)

# Some resids ----
crabs$resids <- nb_fit$residuals
ggplot(crabs, aes(x = mass, y = resids)) +
  geom_point()

# Some preds ----
# Make predictions on the link scale
lpreds <- data.frame(predict(nb_fit, se.fit = TRUE))
lpreds$lwr <- lpreds$fit + qnorm(0.025) * lpreds$se.fit
lpreds$upr <- lpreds$fit + qnorm(0.975) * lpreds$se.fit

# Convert fit, lwr, upr, to real scale of response var
rpreds <- apply(lpreds, 2, exp)

# Smash it together with the raw data 
crabs_preds <- data.frame(crabs, rpreds)

# Plot the predictions against the raw data
ggplot(crabs_preds, aes(x = mass, y = satellites)) +
  geom_point() +
  geom_ribbon(aes(xmax = mass, ymin = lwr, ymax = upr,
                  color = NULL), 
              alpha = 0.40) +
  geom_line(aes(y = fit)) +
  theme_linedraw() +
  theme(panel.grid = element_line (color = "gray87")) 

# HOLD UP ----
# . Break these 
# Step 1 (a logistic regression)
crabs$present <- 0
crabs$present[crabs$satellites > 0] <- 1

lmod <- stan_glm(present ~ mass, data = crabs, family = binomial)

# Step 2 (a count model)
crabs_present <- crabs[crabs$present == 1,]

cmod <- stan_glm(satellites ~ mass, data = crabs_present,
            family = neg_binomial_2(), iter = 1000, 
            warmup = 200)

summary(cmod, digits = 3)

# Predictions ----

logit_preds <- data.frame(predict(lmod, se.fit = TRUE))

logit_preds$lwr <- logit_preds$fit + logit_preds$se.fit * qnorm(0.025)
logit_preds$upr <- logit_preds$fit + logit_preds$se.fit * qnorm(0.975)

real_preds <- apply(logit_preds, 2, boot::inv.logit)
pres_preds <- data.frame(crabs, real_preds)

# Plot em
ggplot(pres_preds, aes(x = mass, y = fit)) +
  geom_line() +
  geom_ribbon(aes(xmax = mass, ymin = lwr, ymax = upr),
              alpha = 0.10) +
  geom_point(aes(y = present))

log_preds <- data.frame(predict(cmod, se.fit = TRUE))
log_preds$lwr <- log_preds$fit + log_preds$se.fit * qnorm(0.025)
log_preds$upr <- log_preds$fit + log_preds$se.fit * qnorm(0.975)

real_preds <- apply(log_preds, 2, exp)

count_preds <- data.frame(crabs_present, real_preds)

ggplot(count_preds, aes(x = mass, y = satellites)) +
  geom_point() +
  geom_line(aes(y = fit)) +
  geom_ribbon(aes(xmax = mass, ymin = lwr, ymax = upr),
              alpha = 0.20, fill = "magenta") 

