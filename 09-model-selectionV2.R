# Libraries ----
library(tidyverse)
library(AICcmodavg)
library(car)

# Data ----
smolts <- read.csv("data/smolts.txt",
                   stringsAsFactors = FALSE
                   )

# INSERT DATA EXPLORATION CODE HERE ----

# Build some models ----
nka_mod <- lm(osmolality ~ nka, data = smolts)
stage_mod <- lm(osmolality ~ stage, data = smolts)
stage_nka_mod <- lm(osmolality ~ stage + nka, data = smolts)
stage_int_nka_mod <- lm(osmolality ~ stage * nka, data = smolts)
null_mod <- lm(osmolality ~ 1, data = smolts)

# How to choose?
summary(stage_mod)
summary(nka_mod)
summary(stage_nka_mod)
summary(stage_int_nka_mod)

# Model selection ----
# Here is one way:
AIC(stage_mod)
AIC(nka_mod)
AIC(stage_nka_mod)
AIC(stage_int_nka_mod)

# AICcmodavg package way
model_list = list(nka_mod, stage_mod, 
                  stage_nka_mod,
                  stage_int_nka_mod,
                  null_mod
                  )

model_names = c("nka_mod", "stage_mod", 
                "stage_nka_mod",
                "stage_int_nka_mod",
                "null_mod"
                )

aictab(cand.set = model_list, 
       modnames = model_names
       )

# Residuals ----
smolts$resids <- stage_nka_mod$residuals

ggplot(smolts, aes(x = stage, y = resids)) +
  geom_boxplot()

ggplot(smolts, aes(x = nka, y = resids)) +
  geom_point()

mean(smolts$resids)

# Statistical significance ----
Anova(stage_nka_mod, type = "III")
summary(stage_nka_mod)

# Predictions ----
preds <- predict(stage_nka_mod, interval = "confidence")
smolt_preds <- data.frame(smolts, preds)

ggplot(smolt_preds, aes(x = nka, y = osmolality,
                        color = stage, fill = stage)) +
  geom_point() +
  geom_line(aes(y = fit)) +
  geom_ribbon(aes(xmax = nka, ymin = lwr, ymax = upr,
                  color = NULL), alpha = 0.30) +
  xlab(expression(paste(
                  "Gill Na"^"+","-","K"^"+"," - ATPase activity (",
                  mu, "mol ADP g Protein min"^"-1",")"
                  ))) +
  ylab("Osolality (mOsm)") +
  scale_color_manual(
    labels = c("Postsmolt", "Presmolt", "Smolt"),
    values = c("black", "gray40", "steelblue2")
  ) +
  scale_fill_manual(
    labels = c("Postsmolt", "Presmolt", "Smolt"),
    values = c("black", "gray40", "steelblue2")
  ) +
  theme(
    panel.grid = element_blank(),
    axis.title.x = element_text(vjust = -1),
    axis.title.y = element_text(vjust = 3)
  )

#One more example ----
# Data ----
data(ToothGrowth)

# Pretend we did data exploration ----
ToothGrowth$dose <- as.character(ToothGrowth$dose)

# Fit a model ----
tooth_mod <- lm(len ~ supp * dose, data = ToothGrowth)
null_mod <- lm(len ~ 1, data = ToothGrowth)

#Model selection ----
AIC(tooth_mod, null_mod)

# Residual diagnostics ----
ToothGrowth$resids <- tooth_mod$residuals
hist(ToothGrowth$resids)
ggplot(ToothGrowth, aes(x = supp, y = len)) +
  geom_boxplot() +
  facet_wrap(~dose)

# Statistical significance ----
anova(tooth_mod)
summary(tooth_mod)

# Make predictions and plot that shit ----
preds <- predict(tooth_mod, interval = "confidence")
tooth_preds <- data.frame(ToothGrowth, preds)

ggplot(data = tooth_preds, aes(x = supp, y = len)) +
  geom_violin() +
  geom_jitter(width = 0.1) +
  geom_point(aes(y = fit), size = 4) +
  geom_errorbar(aes(xmax = supp, ymin = lwr, ymax = upr),
                width = 0
                ) +
  facet_wrap(~dose)

tooth_preds %>% 
  group_by(supp, dose) %>% 
  summarize(fit = mean(fit),
            lwr = mean (lwr),
            upr = mean (upr))
