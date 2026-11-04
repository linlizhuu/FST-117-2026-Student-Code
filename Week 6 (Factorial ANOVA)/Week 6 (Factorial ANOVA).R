# load this up so we can pipe if need be
library(dplyr)

# Part 1.1 - Liquid DA Dataset ---------------------------------------------
library(readxl)
liquids <- read_excel("Week 6 (Factorial ANOVA)/liquids2.xlsx")

# Check the datastructure
str(liquids)
liquids[,c(1:4)] <- lapply(X = liquids[,c(1:4)], FUN = as.factor)
str(liquids)


#is your data balanced?  Is your data crossed? Is your data nested(hierarchical)?
with(liquids, table(Session, Panelist, Gum, Level))


# 2-way ANOVA
liquids.2way <- lm(Thickness ~ Panelist + Level, data = liquids)
anova(liquids.2way)


# 2-way ANOVA with interaction
liquids.2wayint <- lm(Thickness ~ Panelist * Level, data = liquids)
anova(liquids.2wayint)


# Visualizing Interactions
interaction.plot(x.factor = liquids$Gum,
                 trace.factor = liquids$Panelist,
                 response = liquids$Thickness)




# Part 1.2 - Jellybean Dataset ---------------------------------------------
# Load Data
library(readr)
Jelly <- read_csv("Week 6 (Factorial ANOVA)/Jelly Bean (Discussion).csv")


# Give data the correct structure
str(Jelly)
Jelly[,c(1:5)] <- lapply(X = Jelly[,c(1:5)], FUN = as.factor)
str(Jelly)


#is your data balanced?  Is your data crossed? Is your data nested(hierarchical)?
with(Jelly, table(Region, Sample))


# first, lets remove the person who skipped the question and collapse all regions of Asia into a single level ... and remove the two europeans
Jelly <- Jelly %>%
  filter(Region != "Prefer not to say",
         Region != "Europe")

Jelly <- Jelly %>%
  mutate(Region = case_when(
    grepl("Asia", Region) ~ "Asia",
    TRUE ~ Region
  ))

count(Jelly, Region)


# 2-way ANOVA
Jelly.m1 <- lm(Flavor_Novelty ~ Sample + Region, data = Jelly)
anova(Jelly.m1)


# 2-way ANOVA with interaction
Jelly.m2 <- lm(Flavor_Novelty ~ Sample * Region, data = Jelly)
anova(Jelly.m2)


# Visualizing Interactions
interaction.plot(x.factor = Jelly$Sample,
                 trace.factor = Jelly$Region,
                 response = Jelly$Flavor_Novelty)




# Part 2 - Factorial ANOVA  -------------------------------------------
library(readxl)
liquids <- read_excel("Week 6 (Factorial ANOVA)/liquids2.xlsx")

# Check the datastructure
str(liquids)
liquids[,c(1:4)] <- lapply(X = liquids[,c(1:4)], FUN = as.factor)
str(liquids)

with(liquids, table(Session, Panelist, Gum, Level))

 # Build a maximum model with all main effects and interactions
liquids.m.full <- lm(Thickness ~ (Session + Panelist + Gum + Level)^3, data = liquids)
anova(liquids.m.full)


# Different perspectives on finding parsimonious models

# 1. Build it up (start simple, add layers, justify complexity) 
# intercept only
liquids.m0 <- lm(Thickness ~ 1, data = liquids)  
# main effects
liquids.m1 <- lm(Thickness ~ Session + Panelist + Gum + Level, data = liquids)
# 2-way interactions
liquids.m2 <- lm(Thickness ~ (Session + Panelist + Gum + Level)^2, data = liquids)
# 3-way interactions
liquids.m3 <- lm(Thickness ~ (Session + Panelist + Gum + Level)^3, data = liquids)

# does each added layer improve the model?
anova(liquids.m0, liquids.m1, liquids.m2, liquids.m3)
AIC(liquids.m0, liquids.m1, liquids.m2, liquids.m3)

# 2. Strip it down (start with everything, remove what is not needed)
# let R do the stripping automatically (uses AIC, not p-values)
liquids.back <- step(liquids.m.full, direction = "backward")
anova(liquids.back)


# 3. Start from your hypothesis -------------------------------------------
# Write the question down BEFORE beginning your experiment
# Then build a model that tests that specific question.
# Example hypothesis: " 
liquids.hyp <- lm(Thickness ~ Session + Panelist*Gum + Panelist*Level + Gum:Level, data = liquids)
anova(liquids.hyp)


# Compare the three final models
anova(liquids.m.full, liquids.m#replace with parsimonious model, liquids.back, liquids.hyp)