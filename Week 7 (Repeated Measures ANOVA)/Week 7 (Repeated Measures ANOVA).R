# load this up so we can pipe if need be
library(dplyr)

# Load Data
library(readr)
Jelly <- read_csv("Week 7 (Repeated Measures ANOVA)/Jelly Bean (for RM ANOVA).csv")

# Give data the correct structure
str(Jelly)
Jelly[,c(1:5)] <- lapply(X = Jelly[,c(1:5)], FUN = as.factor)
str(Jelly)

## Do sample differences explain the variation in sweetness ratings? ------

#is your data balanced?  Is your data crossed? Is your data nested(hierarchical)?
with(Jelly, table(Sample, Subject))

# 1-way ANOVA 
# (Ignore the fact that the samples are crossed with subjects)
m1 <- aov(Sweetness ~ Sample, data = Jelly)
summary(m1)

# 2-way ANOVA 
# (Accounting for the fact that the samples are crossed with subjects, but not using the correct error structure)
m2 <- aov(Sweetness ~ Sample + Subject, data = Jelly)
summary(m2)

# 1-way Repeated Measures ANOVA 
# (Accounting for the fact that the samples are crossed with subjects, and using the correct error structure)
# The Error(Subject/Sample) term tells R that the samples are nested within subjects.
m3 <- aov(Sweetness ~ Sample + Error(Subject/Sample), data = Jelly)
summary(m3)


## Do regional differences explain the variation in sweetness ratings? ------

#is your data balanced?  Is your data crossed? Is your data nested(hierarchical)?
with(Jelly, table(Sample, Subject, Region))

# Example of an incorrect model 
m3 <- aov(Sweetness ~ Sample + Region + Subject, data = Jelly)
summary(m3)

# Combining the two fixed effects (Sample and Region) and the random effect (Subject) in a single model, with the correct error structure.
# compare these two models to see if using the correct error structure changes your conclusions.
m4 <- aov(Sweetness ~ Sample + Region + Error(Subject/Sample), data = Jelly)
summary(m4)
