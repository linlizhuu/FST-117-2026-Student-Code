# import --------------
library(readxl)
Data <- read_excel("Week 4/Study Score and More.xlsx")
View(Data)















# format ---------------
str(Data)
# what are qualitative data? -> Student, Grade
# what are quantitative data?


library(tidyverse)

Data <- Data |> 
  
  rename(Attendance = `Attendence Rate during COVID (%)`,
         Hours = `Study Time per month (hr)`,
         `Mid-term` = `Midterm Exam Score`,
         Final = `Final Exam Score`) |> 
  
  mutate(Student = as.factor(Student),
         Grade = as.factor(Grade))

str(Data)
















# SLR -----------------

# How can I predict final exam score (Final) using study time per month (Hours)?









## modeling -----------------------

model1 <- lm(Final~Hours, Data)

summary(model1)
# intercept: significant, positive
# slope: significant, positive
# R^2 = 0.6545 (Multiple R-squared for SLR)
# model p-value = 6.275e-08 -> significant model
# model: Final = 38.8023 + 1.2997*Hour

# There is a significant positive association between study time per month and final exam score


?confint()
confint(model1)
# 95% confidence interval for intercept is [27.3317651, 50.272756]
# 95% confidence interval for study time per month is [0.9341243, 1.665233]

# We are 95% confident that each additional study hour per month is associated 
# with an increase of 0.93 to 1.67 points in the predicted final score



# remember that R-squared is the proportion of variance explained by the model
# and is equal to the square of the correlation between predictor and outcome
multiple_R_sqaured <- summary(model1)$r.squared # extract multiple R^2
multiple_R_sqaured

a <- cor(Data$Hours, Data$Final)
a^2

all.equal(a^2, multiple_R_sqaured)














## plotting --------------

# plot final exam score (Final) and study time per month (Hours)
Data %>% 
  ggplot(aes(x=Hours, y=Final)) + 
  geom_point()



# Adding a linear regression line
Data %>% 
  ggplot(aes(x=Hours, y=Final)) + 
  geom_point() + 
  stat_smooth(formula = y~x, method = lm, se = FALSE)



# Adding a linear regression line with confidence interval
Data %>% 
  ggplot(aes(x=Hours, y=Final)) + 
  geom_point() + 
  stat_smooth(formula = y~x, method = lm, se = TRUE) + #TRUE = confidence interval
  labs(title = "Predicting Final Exam Score Using Study Time per Month (hr)",
       subtitle = "Final = 38.30 + 1.30*Hours")
















## checking assumptions --------------
# checking if our model violates assumptions?

### visual checks -----------------
par(mfrow = c(1,1))
plot(model1)

plot(model1, which = 1) # Linearity + Homoscedasticity

plot(model1, which = 2) # Normality

plot(model1, which = 3) # Homoscedasticity


### (bonus, not needed for HW) statistical tests you can run -------------


# normality

# Shapiro–Wilk test
# H0: errors follow a normal distribution
# Ha: errors do not follow a normal distribution
shapiro.test(residuals(model1))
# accept H0





# homoscedasticity

# Breusch–Pagan test
# H0: error variance is constant
# Ha: error variance is not constant
library(lmtest)
bptest(model1)
# reject H0
















## predicting based on the model --------------
# (e.g., a student who studies 40 hours a week)

# model: Final = 38.8023 + 1.2997*Hour
y = 38.8023 + 1.2997*40
y
























# MLR ------------------

# How can I predict final exam score (Final) using study hour per month (Hours), 
# Attendence Rate during COVID (Attendance), and Midterm Exam Score (Mid-term)?












## initial modeling ----------------
model2a <- lm(Final~Hours+Attendance+`Mid-term`, Data)

summary(model2a)
# sig. intercept, Hours, Mid-term
# insig. Attendance
# Adjusted R-squared = 0.8299 -> good fitness of the model
# p-value = 9.33e-11 significant model

confint(model2a)











## refining model if you'd like to ------------
# by removing insignificant predictors 
# in this case, remove Attendance
model2b <- lm(Final~Hours+`Mid-term`, Data)

summary(model2b)
# p-value = 3.591e-11
# Adjusted R-squared = 0.8191
# Final = 17.50503 + 0.82324*Hours + 0.48221*Mid-term

# There is a significant positive association between final score and study hours, 
# as well as a sign significant positive association between final score and midterm scores


confint(model2b)












## predicting based on the model --------------
# e.g., A student who studied 40 hours a week and a 90 Mid Term Score?

### The "Manual" way ------------
y = 17.50503 + 0.82324*40 + 0.48221*90
y  

### The "Automatic" way --------------
new <- as.data.frame(cbind(40,90)) # the given numbers in order of model we just created
colnames(new) <- c("Hours","Mid-term")
predict.lm(model2b, new, level = 0.95)






## plotting ----------------

# 2 predictors, use 3D plot

### interactive 3D plot with plotly -------

# plotting the model using in 3 dimensions (interactive)

library(plotly)

p <- Data %>% 
  plot_ly(x = ~Hours, 
          y = ~`Mid-term`, 
          z = ~Final, 
          type = 'scatter3d', 
          mode = 'markers', 
          marker = list(size = 5, color = ~Final, colorscale = 'Viridis', opacity = 0.8)) %>%
  layout(scene = list( xaxis = list(title = 'Study Time per Month (Hours)'), 
                       yaxis = list(title = 'Midterm Exam Score'), 
                       zaxis = list(title = 'Final Exam Score')))

print(p)






### Non-interactive 3D plot with scatterplot3d ------------------

# plotting the model in 3 dimensions (non interactive)

s3d <- scatterplot3d::scatterplot3d(x=Data$Hours, 
                                    y=Data$`Mid-term`, 
                                    z=Data$Final, 
                                    pch = 16, 
                                    color="steelblue",
                                    xlab = "Study Time per month (hour)",
                                    ylab = "Midterm Exam Score",
                                    zlab = "Final Exam Score",
                                    angle=70)


# changing the angle for better visualization
s3d_2 <- scatterplot3d::scatterplot3d(x=Data$Hours, 
                                    y=Data$`Mid-term`, 
                                    z=Data$Final, 
                                    pch = 16, 
                                    color="steelblue",
                                    xlab = "Study Time per month (hour)",
                                    ylab = "Midterm Exam Score",
                                    zlab = "Final Exam Score",
                                    angle=50)

# Add regression plane
s3d_2$plane3d(model2b)


# Adding grade as a categorical variable
library(wesanderson)

str(Data)

colors <- wes_palette("GrandBudapest1",3) # select 3 colors from a color palette

colors <- colors[as.numeric(Data$Grade)]

s3d_3 <- scatterplot3d::scatterplot3d(x=Data$Hours, 
                                      y=Data$`Mid-term`, 
                                      z=Data$Final, 
                                      pch = 16, 
                                      color=colors,
                                    xlab = "Study Time per month (hour)",
                                    ylab = "Midterm Exam Score",
                                    zlab = "Final Exam Score",
                                    angle=50)
# Add regression plane
s3d_3$plane3d(model2b)

#add legend
legend("top", 
       legend = levels(Data$Grade), 
       col=wes_palette("GrandBudapest1",3), 
       pch = 16, 
       inset = -0.95, 
       xpd = TRUE, 
       horiz = TRUE)
