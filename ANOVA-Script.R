# Scott Schumacker
# ANOVA Example

# Loading libraries
library(tidyverse)
library(ggplot2)
library(hrbrthemes)
library(dplyr)
library(tidyr)
library(viridis)
library(car)
library(qqplotr)

# Exploring data sets
data()

# Loading the data
myData <- PlantGrowth

# Viewing the structure of the data
view(myData)
str(myData)

# Getting a random sample of our data set
set.seed(1234)
sample <- sample_n(myData, 10)

# Looking at the different means of the different groups in the data set
group_by(myData, group) %>%
  summarise(
    count = n(),
    mean = mean(weight, na.rm = TRUE),
    sd = sd(weight, na.rm = TRUE)
  )

# Assessing Normality
# Visualizing our data sets with multiple box plots and density curves
Density_Plots <- ggplot(data=sample, aes(x=weight, group=group, fill=group)) +
  geom_density(adjust=1.5, alpha=.4) +
  theme_ipsum()

Density_Plots

# Boxplots
Boxplot <- ggplot(data = sample, aes(x=group, y=weight, col = group)) + 
  geom_boxplot()

Boxplot

# qq plots
gg <- ggplot(data = myData, mapping = aes(sample = weight, color = group, fill = group)) +
  stat_qq_band(alpha = 0.2) +
  stat_qq_line() +
  stat_qq_point() +
  facet_wrap(~ group) +
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles")

gg

# Shapiro-Wilke test for normality
shapiro.test(myData$weight)
# With high confidence we can assume our data follows a normal distribution

# Assessing Homogeneity of Variance
# Ho: The groups have equal variance
# Ha: The groups do not have equal variance
leveneTest(weight ~ group, data = myData)
# Since the p-value is not significant, we canot reject the null hypothesis

# Compute the analysis of variance
res.aov <- aov(weight ~ group, data = myData)
# Summary of the analysis
summary(res.aov) 
# From this ANOVA, we can see that there is a significant difference between groups

# Tukey Honest Significant Difference test to look at what is significant
TukeyHSD(res.aov)
# We can see here that there is a sig difference between trt2-trt1
