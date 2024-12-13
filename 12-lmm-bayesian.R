# Library ----
library(tidyverse)
library(rstanarm)

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
mod <- stan_glmer(logmass ~ loglength + (1|site2), data = cray)

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

summary(mod, digits = 3)

#Predictions ----
# Log-scale posterior predictions
lpred_mat <- data.frame(posterior_predict(mod))

# Descriptive statistics, with log-link inverted
fit <- exp( apply(lpred_mat, 2, mean) )
lwr <- exp( apply(lpred_mat, 2, quantile, probs = 0.025) )
upr <- exp( apply(lpred_mat, 2, quantile, probs = 0.975) )

# Smash it all together
cray_preds <- data.frame(cray, fit, lwr, upr)

ggplot(cray_preds, aes(x = length, y = mass, color = site2, fill = site2)) +
  geom_point(alpha = 0.10) +
  geom_line(aes(y = fit)) +
  geom_ribbon(aes(xmax = length, ymin = lwr, ymax = upr, 
                  color = NULL), 
              alpha = 0.20) +
  theme_bw() +
  xlab("Carapace length (mm)") +
  ylab("Mass (g)") +
  facet_wrap(~site2)
