#### info ####

# goal: analyze and visualize observational data that includes continuous predictor variables

# author: Amy Kendig

# notes: The the waffle dataset and regression formulas come from Richard McElreath's package "rethinking" and textbook Statistical Rethinking, an excellent resource for introductory statistics and Bayesian analysis. The map code is from: https://bookdown.org/ajkurz/Statistical_Rethinking_recoded/ and https://www.datanovia.com/en/blog/how-to-create-a-map-using-ggplot2/.


#### set-up ####

# clear environment
rm(list = ls())

# load packages
library(ggplot2)
library(dplyr)
library(tidyr)

# import data
waffle <- read.csv("./data/Waffle-House-divorces.csv")

# examine data
head(waffle)


#### edit data ####

# make South a factor
waffle$South.f = as.factor(waffle$South)

# standardize some of the variables to make them on the same scale
waffle$Divorce.s <- (waffle$Divorce - mean(waffle$Divorce))/sd(waffle$Divorce)
waffle$MedianAgeMarriage.s <- (waffle$MedianAgeMarriage - mean(waffle$MedianAgeMarriage))/sd(waffle$MedianAgeMarriage)
waffle$Marriage.s <- (waffle$Marriage - mean(waffle$Marriage))/sd(waffle$Marriage)

# make location lowercase for maps
waffle$region = tolower(waffle$Location)

# format the data "long" for the maps
waffle.l <- waffle %>%
  select(region, Divorce.s, MedianAgeMarriage.s, Marriage.s) %>%
  gather(key, value, -region)

# check long data
head(waffle.l)

# merge the long data with map coordinates
waffle.m <- waffle.l %>%
  left_join(map_data("state"))

# check map data
head(waffle.m)
  

#### initial visualization ####

# Divorce rate per 1000 adults by number of Waffle Houses
ggplot(waffle, aes(x = WaffleHouses, y = Divorce, color = South.f)) +
  geom_point() +
  geom_hline(aes(yintercept = mean(Divorce)), linetype = "dashed")

# Divorce rate per 1000 adults by age at marriage
ggplot(waffle, aes(x = MedianAgeMarriage, y = Divorce)) +
  geom_point()

# Divorce rate per 1000 adults by marriage rate per 1000 adults
ggplot(waffle, aes(x = Marriage, y = Divorce)) +
  geom_point()

# Maps of three variables using long data
ggplot(waffle.m, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = value), color = "black") +
  facet_wrap(~key) +
  scale_fill_gradientn(colors = c("#e5f5f9", "#99d8c9", "#2ca25f"))


#### statistics ####

# fit regression
mod <- lm(Divorce ~ Marriage.s + MedianAgeMarriage.s, data = waffle)

# look at output
summary(mod)

# make a dataset for precition, set the scaled marriage rate to 0
waffle.p <- waffle %>%
  mutate(Marriage.s = 0)

# save predicted values
waffle$pred = predict(mod, newdata = waffle.p)
waffle$pred.se = predict(mod, newdata = waffle.p, se.fit = T)$se.fit


#### create figure ####

# remove background, edit text size, point size, error bar size, and axis labels
ggplot(waffle, aes(x = MedianAgeMarriage, y = Divorce)) +
  geom_point() +
  xlab("Median age at marriage") +
  ylab("Divorce rate per 1000 adults") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12))

# add the regression line
ggplot(waffle, aes(x = MedianAgeMarriage, y = Divorce)) +
  geom_point() +
  geom_line(aes(y = pred)) +
  geom_ribbon(aes(ymin = pred - pred.se, ymax = pred + pred.se), alpha = 0.5) +
  xlab("Median age at marriage") +
  ylab("Divorce rate per 1000 adults") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12))

# save figure
pdf("./output/divorce-figure.pdf", width = 3, height = 3)

ggplot(waffle, aes(x = MedianAgeMarriage, y = Divorce)) +
  geom_point() +
  geom_line(aes(y = pred)) +
  geom_ribbon(aes(ymin = pred - pred.se, ymax = pred + pred.se), alpha = 0.5) +
  xlab("Median age at marriage") +
  ylab("Divorce rate per 1000 adults") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12))

dev.off()