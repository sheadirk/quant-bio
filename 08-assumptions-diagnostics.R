# Library ----
library(tidyverse)

# Data ----
turtles <- read.csv("data/turtles.txt",
                    stringsAsFactors = FALSE, 
                    strip.white = TRUE
                    )

head(turtles)

glimpse(turtles$Status)

unique(turtles$Status)
# Not normal

#Is stay normal?
ggplot(turtles, aes(Stay)) +
  geom_histogram()

# Can we make it normal?
ggplot(turtles, aes(log(Stay))) +
  geom_histogram()

# What does log do?
numbers <- seq(1, 1000, 1)

plot(log(numbers))

# Can I make some residuals?
# this would be equivalent to 
# lm(log(Stay)) ~ 1, data = turtles)
turtles$log_stay <- log(turtles$Stay)

turtles$log_resids <- turtles$log_stay - mean(turtles$log_stay)

#Plot the residuals
ggplot(turtles, aes(log_resids)) + geom_histogram()

ggplot(turtles, aes(y = log_resids, x = factor(Year))) +
  geom_boxplot()

# What about an actual model (like regression)
test_mod <- lm(log(Stay) ~ nHooks, data = turtles)

turtles$residuals <- test_mod$residuals

ggplot(turtles, aes(residuals)) +
  geom_histogram()

mean(turtles$residuals)
median(turtles$residuals)

ggplot(turtles, aes(x = nHooks, y = residuals)) +
  geom_point() +
  geom_hline(yintercept = 0)

plot(test_mod)

# One more worked example top to bottom ----
smolts <- read.csv("data/smolts.txt", 
                   stringsAsFactors = FALSE)

glimpse(smolts)

# Fit an ANCOVA ----
smolt_cova <- lm(osmolality ~ stage + nka, 
                 data = smolts
                 )
smolts$residuals <- smolt_cova$residuals

ggplot(smolts, aes(y = residuals, x = nka)) +
  geom_point() +
  facet_wrap(~stage)

ggplot(smolts, aes(x = residuals)) +
  geom_histogram() +
  facet_wrap(~stage)

ggplot(smolts, aes(y = residuals, x = stage)) +
  geom_boxplot()

shapiro.test(smolts$residuals[smolts$stage == "Postsmolt"])
shapiro.test(smolts$residuals[smolts$stage == "Smolt"])
shapiro.test(smolts$residuals[smolts$stage == "Presmolt"])

shapiro.test(smolts$residuals)

# . Statistical significance ----
car::Anova(smolt_cova, type = "III")

summary(smolt_cova)

# Prediction ----
preds <- predict(smolt_cova, interval = "confidence")
smolt_preds <- data.frame(smolts, preds)

smoltp <- ggplot(smolt_preds, aes(x = nka, y = osmolality)) +
  geom_point(color = "steelblue2") +
  geom_line(aes(y = fit)) +
  geom_ribbon(aes(xmax = nka, ymin = lwr, ymax = upr), 
              alpha = 0.30, fill = "magenta") +
  xlab("Gill NKA Activity") +
  ylab("Osmolality (moSm)") +
  facet_wrap(~stage) +
  theme_bw() +
  theme(strip.background = element_blank(),
        strip.text = element_text(size = 14))

smoltp

jpeg("smolt_plot.jpg",
     width = 2400, height = 1800, res = 300
     )
smoltp
dev.off()
  

