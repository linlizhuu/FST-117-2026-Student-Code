# import data --------------
library(readxl)
Cereal_Lemonade <- read_excel("Week 2/Cereal_Lemonade_FST117_2026.xlsx", 
                              sheet = "Sheet1")



# slice out cereal and pivot into long -----------------
library(tidyverse)


## delete lemonade cols
cereal <- Cereal_Lemonade |> 
  select(-c(Lemonade_Raw, Lemonade_Accuracy))



## pivot into longer
## cols keep the same: Judge, Gender, Region, Frequencies
## cols into long: Sample_Position, Liking, Sweetness, Familiarity, Healthiness
## new col: Flavor

cereal_long <- cereal %>%
  pivot_longer(
    cols = matches(
      "^(Sample_Position|Liking|Sweetness|Familiarity|Healthiness)_"
      # tell R these are the cols i wanna pivot into long
      # ^ means col names that starts with (eg, Liking_Apple_Cinnamon, etc.)
      # | means or
      # translated: i want to pivot cols that names starts with Sample_Position,
      # or Liking, or Sweetness, or Familiarity, or Healthiness into long
      # assuming cols not mentioned here will be keep the same (aka. repeated by Judge)
    ),
    
    names_to = c(".value", "Flavor"), 
    # col names go to Flavor
    
    names_pattern = "^(Sample_Position|Liking|Sweetness|Familiarity|Healthiness)_(.*)$"
    # tell R how to split the original col names into 2 pieces as defined
    # ^ means start of the cols
    # _ means the underline separating the flavors as cols were named as XXX_Flavor
    # .* means whatever characters followed (aka. flavor in this case)
    # translated: col names break into XXX start with Sample_Position,
    # or Liking, or Sweetness, or Familiarity, or Healthiness; break with _;
    # then followed by Flavor
    # so latter half goes into new Flavor col for flavor name
  )

# Question: 52 consumers evaluating 5 products,
# each consumer*product combination gets 1 observation per Sample_Position,
# Liking, Sweetness, Familiarity, Healthiness
# How many rows you should get in this long format??





# check structure -----------
str(cereal_long)






# format and modify data type -------------

# think about data type per col

# factor cols: Judge, Gender, Region, Cereal_Frequency, Cheerio_Frequency,
# Multigrain_Frequency, Protein_Frequency, Flavor

# numeric cols: Liking, Sweetness, Familiarity, Healthiness

# What about Sample_Position? Ordinal data

cereal_long_formatted <- cereal_long |> 
  mutate(across(c(Judge, Gender, Region, Flavor, Sample_Position),
                as.factor),
         # set Judge, Gender, Region, Flavor, Sample_Position as factor
         
         across(ends_with("_Frequency"),
                as.factor),
         # set any cols ends with Frequency as factor
         
         across(c(Sample_Position, Liking, Sweetness, Familiarity, Healthiness),
                as.numeric))
         # set Sample_Position, Liking, Sweetness, Familiarity, Healthiness as numeric


# check structure again
str(cereal_long_formatted)





# measures of central tendency ---------

## mean ------------

# What is the average liking across all products?
avg_liking_all <- mean(cereal_long_formatted$Liking)

avg_liking_all



# What is the average liking for Plain?
avg_liking_by_flavor <- cereal_long_formatted |> 
  group_by(Flavor) |> 
  summarise(mean(Liking))

avg_liking_by_flavor







## median ------------

# What is median of liking across all flavors?
median(cereal_long_formatted$Liking)









## mode -------------

# R does not have a function for mode
# so we will create one manually

# run the following code to create function
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
# a `getmode` function should pop up in Functions in Environment


# What is mode for liking across all products?
getmode(cereal_long_formatted$Liking)






# measures of dispersion -----------

## range --------
range(cereal_long_formatted$Liking)



## IQR ----------
IQR(cereal_long_formatted$Liking)

ggplot(cereal_long_formatted) +
  geom_boxplot(aes(Liking))




## variance ----------
var(cereal_long_formatted$Liking)




## sd ----------
sd(cereal_long_formatted$Liking)

sqrt(var(cereal_long_formatted$Liking))




## summar of whole dataset ------
summary(cereal_long_formatted)








# scatter plot ------------------

# 2 continuous variables
# Liking and Familiarity


## basic scatter plot ----------
a1 <- cereal_long_formatted |> 
  ggplot(aes(x = Liking,
             y = Familiarity))

print(a1) # view plot

# only axes, no data input because we did not specify what plot


# add points for scatter plot
a2 <- a1 +
  geom_point() # adding a point layer

print(a2)



# jitter points
# shift points a little bit because we have repeated observation for
# liking scores and familiarity scores
a3 <- a1 +
  geom_jitter()

print(a3) # looks way better!


# add labels
a4 <- a3 +
  labs(x = "Liking Score",
       y =  "Flavor Familiarity",
       title = "Cereal: Liking vs. Familiarity")

print(a4)





## advanced scatter plot --------------

# scatter plot with colors and different theme
b1 <- cereal_long_formatted |>
  
  # specify axes
  ggplot(aes(x = Liking,
             y = Familiarity)) +
  
  # specify plot type + point color
  geom_jitter(color = "firebrick") +
  
  # specify labels
  labs(x = "Liking Score",
       y =  "Flavor Familiarity",
       title = "Cereal: Liking vs. Familiarity") +
  
  # change theme
  theme_minimal()

print(b1)





## add additional variables ----------
c1 <- cereal_long_formatted |>
  
  # specify axes
  ggplot(aes(x = Liking,
             y = Familiarity)) +
  
  # specify plot type + additional variable
  geom_jitter(mapping = aes(color = Cereal_Frequency)) +
  
  # specify labels
  labs(x = "Liking Score",
       y =  "Flavor Familiarity",
       title = "Cereal: Liking vs. Familiarity",
       color = "Cereal Consumption Frequency") +
  
  # change theme
  theme_minimal()

print(c1)


# split based on Cereal_Frequency
c2 <- c1 + 
  facet_wrap(~Cereal_Frequency,
                drop = FALSE)
# we see the Cereal_Frequency is not in order from lowest to highest consumption freq.


# put them into order
cereal_long_formatted |> count(Cereal_Frequency)

cereal_long_formatted$Cereal_Frequency <- factor(
  cereal_long_formatted$Cereal_Frequency,
  levels = c(
    "Never",
    "Less than once a month",
    "Once a month",
    "Several times a month",
    "Once a week",
    "Several times a week"
  )
)


c3 <- cereal_long_formatted |>
  
  # specify axes
  ggplot(aes(x = Liking,
             y = Familiarity)) +
  
  # specify plot type + additional variable
  geom_jitter(mapping = aes(color = Cereal_Frequency)) +
  
  # specify labels
  labs(x = "Liking Score",
       y =  "Flavor Familiarity",
       title = "Cereal: Liking vs. Familiarity",
       color = "Cereal Consumption Frequency") +
  
  # change theme
  theme_minimal() +
  
  # split based on Cereal_Frequency
  facet_wrap(~Cereal_Frequency,
             drop = FALSE)



print(c3)




# correlation -----------

# Pearson's correlation coefficient -> interval or ratio
# Spearman's correlation coefficient -> ordinal


## Is liking and familiarity correlated? --------------

# 2 intervals

cor.test(cereal_long_formatted$Liking,
         cereal_long_formatted$Familiarity,
         method = "pearson")





## Is consumption of cereal and consumption of Cheerio correlated? ---------

# 2 ordinals

cor.test(cereal$Cereal_Frequency,
         cereal$Cheerio_Frequency,
         method = "spearman")


# shows error 'x' must be a numeric vector

# needs to translate Frequencies into numbers
# YET use Spearman's for ordinal data Cereal_Frequency


cereal |> 
  count(Cereal_Frequency)

freq_levels_Cereal <- c(
  "Never",
  "Less than once a month",
  "Once a month",
  "Several times a month",
  "Once a week",
  "Several times a week"
)


cereal |> 
  count(Cheerio_Frequency)

freq_levels_Cheerio <- c(
  "I have never eaten Cheerios",
  "Less than once a month",
  "Once a month",
  "Several times a month",
  "Once a week",
  "Several times a week"
)

cereal_cheerio_freq <- cereal |> # we can directly use wide format!
  select(Cereal_Frequency,Cheerio_Frequency) |> 
  mutate(Cereal_Frequency_num = as.numeric(
    factor(Cereal_Frequency,
           levels = freq_levels_Cereal,
           ordered = TRUE)
  )) |> 
  mutate(Cheerio_Frequency_num = as.numeric(
    factor(Cheerio_Frequency,
           levels = freq_levels_Cheerio,
           ordered = TRUE)))

str(cereal_cheerio_freq)

view(cereal_cheerio_freq)
# now we have a col of Cereal_Frequency in numeric
# and a col of Cheerio_Frequency in numeric

cor.test(cereal_cheerio_freq$Cereal_Frequency_num,
         cereal_cheerio_freq$Cheerio_Frequency_num,
         method = "spearman")
