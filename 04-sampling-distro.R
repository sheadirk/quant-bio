# Libraries ----
library(Rlab)
library(MASS)
library(tidyverse)

# Normal Distribution ----
#Batting averages for the real red dragons
avg = 0.270
minimum = 0.080
maximum = 0.414

#we want to simulate (or generate) new observations from this
rnorm(n = 1, mean = avg, sd = 0.05)

batting_average = rnorm(n = 300000, mean = avg, sd = 0.05)

print(batting_average)

hist(batting_average)

min(batting_average)
max(batting_average)
mean(batting_average)
sd(batting_average)
range(batting_average)
median(batting_average)
quantile(batting_average, probs = c(0.025, 0.975))

# Beta distribution (continuous) ----

# a and b or shape1 and shape2
#survival modeling, presence/absence, allows you to make predictions from 0 to 1 

hist(rbeta(1000, shape1 = 1, shape2 = 1 ))
hist(rbeta(1000, shape1 = 100, shape2 = 100))

# Bernoulli distribution (discrete) ----

#live/dead, marked/unmarked, etc.

feelthebern <- rbern(n = 10000, p = 0.64)

feelthebern

sum(feelthebern)/10000

sum(feelthebern)/length(feelthebern)

sd(feelthebern)
#standard deviation doesn't make sense when you only have two outcomes

# Binomial distribution (discrete) ----
binom_samp <- rbinom(n = 30, size = 3, p = 0.33)

mean(binom_samp)

sum(binom_samp)/(3*30)

# Poisson distribution (discrete) ----
parasites <- rpois(n = 30, lambda = 10)
hist(parasites)

# Negative binomial distribution (discrete) ----
negbin_samp <- rnegbin(n = 30, mu = 10, theta = 1)
hist(negbin_samp)

#Application ----
# symmetrical can be poisson or binomial
# non-symmetrical can be negative binomial or others

otsego <- read.csv("data/physical.csv", stringsAsFactors = FALSE)

head(otsego)

hist(otsego$temp)
