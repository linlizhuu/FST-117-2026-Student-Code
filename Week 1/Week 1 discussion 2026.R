# import yogurt -----------
library(readxl)

yogurt <- read_excel("Week 0/Yogurt_liking.xlsx", 
                     sheet = "Liking_8yogurts")

View(yogurt)




# rename col ------------
library(tidyverse)

yogurt1 <- rename_at(yogurt,
                     1,
                     ~"consumer")




# pivot into long format ---------------
yogurt2 <- pivot_longer(yogurt1,
                        !consumer,
                        names_to = "product",
                        values_to = "liking")



# slice out certain column --------
yogurt3 <- yogurt1 |> 
  select(consumer,P1)



# delete certain column --------
yogurt4 <- yogurt1 |> 
  select(-3, -4)

yogurt4_2 <- select(yogurt1,
                    -3, -4)

yogurt4_3 <- yogurt1 |> 
  select(-c(P2,P3))





# count -------
yogurt2 |> count(consumer, product) # how many obs per consumer*product combination

yogurt2 |> count(consumer) # how many obs per consumer

num_prod_per_consumer <- yogurt2 |> count(consumer)






# average ----------
yogurt2 |> 
  group_by(product) |> 
  summarise(Mean = mean(liking,
                        na.rm = TRUE)) # per product avergaed liking


prod_avg_liking <- yogurt2 |> 
  group_by(product) |> 
  summarise(Mean = mean(liking,
                        na.rm = TRUE))

yogurt2 |> 
  group_by(consumer) |> 
  summarise(Mean = mean(liking))










# import soda ----------
soda <- read_excel("Week 1/Soda.xlsx")





# 1-way chi-sq ---------

## option 1: create table from long data
soda_table1 <- table(soda$`Soda Preference`)

soda_table1

chisq.test(soda_table1)




## option 2: manual create table
soda |> count(`Soda Preference`)

soda_table2 <- c(34, 52, 76, 38)

chisq.test(soda_table2)




# 2-way chi-sq ------------
soda_table3 <- table(soda$Gender,
                     soda$`Soda Preference`)

chisq.test(soda_table3)







# histogram -----------
library(ggplot2)

ggplot(yogurt2) +
  geom_histogram(aes(x = liking),
                 binwidth = 1)

histo <- ggplot(yogurt2) +
  geom_histogram(aes(x = liking),
                 binwidth = 1)

histo



# boxplot -------------
ggplot(yogurt2) +
  geom_boxplot(aes(x = product,
                   y = liking))

box_colored <- ggplot(yogurt2) +
  geom_boxplot(aes(x = product,
                   y = liking,
                   color = product))

box_colored
