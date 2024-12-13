# Library ----
library(tidyverse)
library(AICcmodavg)

#Data ----
smolts <- read.csv("data/smolts.txt", 
                   stringsAsFactors = FALSE
                   )

# INSERT DATA CODE HERE ----

# Build some models ----
nka_mod <- lm(osmolality ~ nka, data = smolts)
stage_mod <- lm(osmolality ~ stage, data = smolts)
stage_nka_mod <- lm(osmolality ~ stage + nka, data = smolts)
stage_int_nka_mod <- lm(osmolality ~ stage * nka, data = smolts)

summary(stage_mod)

