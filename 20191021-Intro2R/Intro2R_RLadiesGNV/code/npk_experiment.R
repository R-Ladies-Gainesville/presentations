#### info ####

# goal: analyze and visualize experimental data that includes categorical predictor variables and a random effect

# author: Amy Kendig

# notes: The npk dataset comes pre-loaded in R. Type ?npk to learn more about it.


#### set-up ####

# clear environment
rm(list = ls())

# load packages
library(nlme)
library(ggplot2)

# import data
npk <- read.csv("./data/NPK-peas-experiment.csv")

# examine data
head(npk)


#### edit data ####

# create a treatment variable
npk$treatment = as.factor(with(npk, paste(N, P, K, sep = "")))
npk$treatment

# translate to nutrient treatment
treatments = data.frame(treatment = levels(npk$treatment),
                        nutrients = factor(c("ctrl", "K", "P", "P+K", "N", "N+K", "N+P", "N+P+K"), levels = c("ctrl", "N", "P", "K", "N+P", "P+K", "N+K", "N+P+K")))

# merge with dataset
npk2 <- merge(npk, treatments, all.x = T)


#### initial visualization ####

# yield by nutrients
ggplot(npk2, aes(x = nutrients, y = yield)) +
  geom_point() +
  facet_wrap(~block)


#### statistics ####

# fit regression
mod <- lme(yield ~ N*P*K, random = ~1|block, data = npk2)

# look at output
summary(mod)

# look at output as an ANOVA table
anova(mod)
  

### create figure ####

# simple
ggplot(npk2, aes(x = nutrients, y = yield)) +
  stat_summary(geom = "point", fun.y = "mean") +
  stat_summary(geom = "errorbar", fun.data = "mean_se")

# remove background, edit text size, point size, error bar size, and axis labels
ggplot(npk2, aes(x = nutrients, y = yield)) +
  stat_summary(geom = "point", fun.y = "mean", size = 3) +
  stat_summary(geom = "errorbar", fun.data = "mean_se", width = 0.1) +
  xlab("Nutrient treatment") +
  ylab("Yield (lb/plot)") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12))

# save figure
pdf("./output/NPK-figure.pdf", width = 6, height = 3)

ggplot(npk2, aes(x = nutrients, y = yield)) +
  stat_summary(geom = "point", fun.y = "mean", size = 3) +
  stat_summary(geom = "errorbar", fun.data = "mean_se", width = 0.1) +
  xlab("Nutrient treatment") +
  ylab("Yield (lb/plot)") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12))

dev.off()