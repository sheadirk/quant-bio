# Library ----
library(tidyverse)
library(lme4)
library(lmerTest)
library(AICcmodavg)
library(merTools)

# Data ----
cray <- read.csv("data/cray.csv", stringsAsFactors = FALSE)

# Have a look
head(cray)

unique(cray$date)

# Sites within waterbodies

unique(cray$waterbody)

unique(cray$site)

with(cray, table(waterbody, site))

# Make a new site variable
cray$site2 <- as.factor(as.numeric(as.factor(paste(cray$site, cray$waterbody))))
  
# Let's look at length and mass values
any(is.na(cray$length))

any(is.na(cray$mass)) 

# Cool, no NAs, let's log transform both:
cray$loglength <- log(cray$length)

cray$logmass <- log(cray$mass)

# Fit a model ----
mod <- lmer(logmass ~ loglength + (1|site2), data = cray)

summary(mod)

# Diagnostics
cray$resids <- residuals(mod)

ggplot(cray, aes(x = loglength, y = resids)) +
  geom_point()

# here is the overall distribution
ggplot(cray, aes(x = resids)) +
  geom_histogram(bins = 100)

mean(cray$resids)

# we should look at all sites
ggplot(cray, aes(x = factor(site2), y = resids)) +
  geom_boxplot()

summary(mod)

#Predictions ----
lpreds <- data.frame(predictSE(mod, newdata = cray))

lpreds$lwr <- lpreds$fit + qnorm(0.025) * lpreds$se.fit
lpreds$upr <- lpreds$fit + qnorm(0.975) * lpreds$se.fit

rpreds <- apply(lpreds, 2, exp)

cray_preds <- data.frame(cray, rpreds)

glimpse(cray_preds)

ggplot(cray_preds, aes(x = length, y = mass,
                       color = site2, fill = site2)) +
  geom_point() +
  geom_line(aes(y = fit)) +
  geom_ribbon(aes(xmax = length, ymin = lwr, ymax = upr), alpha = 0.10) +
  theme_bw() +
  facet_wrap(~site2)

log_preds <- predictInterval(
  merMod = mod, 
  level = 0.95, n.sims = 1000,
  stat = "median",  type = "linear.prediction",
  include.resid.var = TRUE)


real_preds <- apply(log_preds, 2, exp)

mer_preds <- data.frame(cray, real_preds)


ggplot(mer_preds, aes(x = length, y = mass,
                       color = site2, fill = site2)) +
  geom_point() +
  geom_line(aes(y = fit)) +
  geom_ribbon(aes(xmax = length, ymin = lwr, ymax = upr), alpha = 0.10) +
  theme_bw() +
  facet_wrap(~site2)

