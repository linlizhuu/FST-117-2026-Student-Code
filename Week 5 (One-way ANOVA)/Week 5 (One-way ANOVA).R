# load this up so we can pipe if need be
library(dplyr)

# Load Data
library(readr)
Jelly <- read_csv("Week 5 (One-way ANOVA)/Jelly Bean (Discussion).csv")

# Give data the correct structure
str(Jelly)
Jelly[,c(1:5)] <- lapply(X = Jelly[,c(1:5)], FUN = as.factor)
str(Jelly)

# 1-way ANOVA
library(agricolae)
Jelly.aov <- aov(Liking ~ Sample, data = Jelly)
summary(Jelly.aov)


# Visualizing Categorical Predictors
# Estimated means and pooled SE (sqrt(MSE / n)) from the ANOVA model
# These are the "least-squares means" or "estimated marginal means"
library(emmeans)
Jelly.anova.summary.df <- as.data.frame(emmeans(Jelly.aov, ~ Sample))
Jelly.anova.summary.df
 
# Plot sample means with standard error bars
library(ggplot2)
ggplot(Jelly.anova.summary.df, aes(x = Sample, y = emmean)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = emmean - SE, ymax = emmean + SE), width = 0.2) +
  labs(x = "Sample", y = "Mean Liking")

# Reorder by descreasing liking
ggplot(Jelly.anova.summary.df, aes(x = reorder(Sample, -emmean), y = emmean)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = emmean - SE, ymax = emmean + SE), width = 0.2)



# Multiple Mean Comparison Tests -----------------------------------------
Jelly.anova.emmeans <- emmeans(Jelly.aov, ~ Sample)

# Contrast maxtrices
pwpm(Jelly.anova.emmeans, adjust = "none")       # LSD
pwpm(Jelly.anova.emmeans, adjust = "tukey")      # HSD
pwpm(Jelly.anova.emmeans, adjust = "bonferroni") # bonferroni

# Get a better understanding of the differences between means using something called a compact letter display (CLD). 
library(multcomp)
cld(Jelly.anova.emmeans, adjust = "none", Letters = letters)
cld(Jelly.anova.emmeans, adjust = "tukey", Letters = letters)
cld(Jelly.anova.emmeans, adjust = "bonferroni", Letters = letters)



# Overlaying mean differences on a bar graph
# Save the test result, then pull out the letters
cld.df <- cld(Jelly.anova.emmeans, adjust = "bonferroni", Letters = letters)
cld.df$.group <- trimws(cld.df$.group)   # remove padding spaces

ggplot(cld.df, aes(x = reorder(Sample, -emmean), y = emmean)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = emmean - SE, ymax = emmean + SE), width = 0.2) +
  geom_text(aes(y = emmean + SE + 0.3, label = .group)) +
  labs(x = "Sample", y = "Mean Liking")



# The General Linear Model ()------------------------------------------------
# aov() is a wrapper for lm() that uses treatment coding by default.
# type in `aov` in the console to see the functions code. Look for lm().

# Treatment coding (default)
Jelly$Sample <- relevel(Jelly$Sample, ref = "juicy_pear") # choose your reference level
jelly.trt <- lm(Liking ~ Sample, data = Jelly)
summary(jelly.trt)

# Effects coding
contrasts(Jelly$Sample) <- contr.sum(levels(Jelly$Sample))
jelly.sum <- lm(Liking ~ Sample, data = Jelly)
summary(jelly.sum)


#If you want an ANOVA table use this.
anova(jelly.trt)
anova(jelly.sum)
