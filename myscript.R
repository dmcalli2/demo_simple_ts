library(tidyverse)
library(broom)
library(ggplot2)
library(MASS)
# 

## Simulate data
 set.seed(1233)
ap <- expand.grid(yr = 2005:2021) 
ap$events <- rpois(length(ap$yr), 10*exp(log(ap$yr-2005)))
ap <- ap %>% 
  mutate(future = if_else(yr >2020, "Future", "Current"))
ap_now <- ap  %>% 
  filter(future != "Future")

## Run model
mod1 <- glm(events ~ yr,  data = ap_now, family = "poisson")
summary(mod1)

## Get predictions for mean and 95%CI
res <- broom::augment(mod1, newdata = ap)
res <- res %>% 
  mutate(uci = .fitted + 1.96*.se.fit,
         lci = .fitted - 1.96*.se.fit,
         est = .fitted) %>% 
  mutate_at(vars(lci, est, uci), exp)

## Plot data

plot1 <- ggplot(ap, aes(x = yr, y = events, colour = future)) +
  geom_point() +
  geom_ribbon(data = res, mapping = aes(x = yr, ymin = lci, ymax = uci), alpha = 0.2)
plot1
