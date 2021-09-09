#######################################
#### R-Ladies GNV Kick-Off Meeting ####
#######################################


#### set-up ####

## if you don't have them already, install packages
install.packages("tidyverse")
install.packages("remotes")
install.packages("palmerpenguins")

## load packages
library(tidyverse) 
library(palmerpenguins)

## look at data
data(package = 'palmerpenguins')
head(penguins)

## remove rows with missing data
penguins2 <- penguins %>%
  filter(!is.na(bill_length_mm))


#### plots ####

## boxplot of bill_length by species
ggplot(penguins2, aes(x=species, y=bill_length_mm)) + 
  geom_boxplot()

## boxplot w/ points for bill_depth by species
ggplot(penguins2, aes(x=species, y=bill_depth_mm)) + 
  geom_boxplot() + 
  geom_point()

## boxplot w/ jittered points for flipper_length by species
ggplot(penguins2, aes(x=species, y=flipper_length_mm)) + 
  geom_boxplot() + 
  geom_jitter()

## boxplot w/ jittered points for flipper_length by species (fix jittering)
ggplot(penguins2, aes(x=species, y=flipper_length_mm)) + 
  geom_boxplot(outlier.shape=NA) + 
  geom_jitter(height=0, width=.15)

## dotplot for flipper_length by species (fix jittering)
ggplot(penguins2, aes(x=species, y=flipper_length_mm)) + 
  geom_dotplot(binaxis = "y", stackdir = "center") 

## violin plot for flipper_length by species
ggplot(penguins2, aes(x=species, y=flipper_length_mm)) + 
  geom_violin()  

## violin plot (w/ dotplot) for flipper_length by species
ggplot(penguins2, aes(x=species, y=flipper_length_mm)) + 
  geom_violin() + 
  geom_dotplot(binaxis = "y", stackdir = "center") 

## violin plot (w/ boxplot) for bill_length by species
ggplot(penguins2, aes(x=species, y=bill_length_mm)) + 
  geom_violin() + 
  geom_boxplot(width=.1, fill="grey")

## change color of boxes (color-blind friendly palette)
ggplot(penguins2, aes(x=species, y=bill_length_mm, fill=species)) + 
  geom_boxplot(outlier.shape=NA) + 
  geom_jitter(height=0, width=.15) + 
  scale_fill_manual(values=c("#E69F00", "#56B4E9", "#009E73")) +
  theme_light()

## change points to a color-blind friendly palette

#### enter your code here ####


## any plot you want! feel free to use other colors, aesthetics, other data info

#### enter your code here ####
