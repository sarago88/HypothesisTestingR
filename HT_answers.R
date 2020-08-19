################################################################################
###
### Hypothesis Testing in R
###
### Sara Gottlieb-Cohen, Manager of Statistical Support Services
### Center for Science and Social Science Information
### Yale University
###
################################################################################

## This session will focus on the following:
##  1. t-test
##  2. ANOVA
##  3. Correlation
##  4. Regression

################################################################################
### t-test ###

# There are three types of t-tests: one-sample, independent, and paired-sample
# (dependent) t-tests

# One-sample t-tests are used to test whether the mean of a sample is 
# significantly different from a single number (e.g., a known population average).

# Independent sample t-tests are used to test whether the mean of two independent 
# groups are significantly different.

# Paired-samples t-tests are used when the two samples contain matched observations
# (e.g., to compare values pre and post a drug intervention)

# We will work with the "iris" data set for this portion.

head(iris)

# Is the average sepal length of this sample significantly different from 5.0?
# We will conduct a one-sample t-test to find out:

t.test(iris$Sepal.Length, mu = 5.0)

# Do setosa and versicolor flowers differ significantly in their sepal length?

setosa_versicolor <- subset(iris, Species != "virginica")
t.test(setosa_versicolor$Sepal.Length ~ setosa_versicolor$Species)

# or: 

t.test(iris$Sepal.Length[iris$Species == "setosa"], 
       iris$Sepal.Length[iris$Species == "versicolor"])

# Are sepal length and petal length significantly different?
# When the values of our two samples are not from independent observations,
# we conduct a paired-samples t-test.

t.test(iris$Sepal.Length, iris$Petal.Length, paired = TRUE)

# or:

library(reshape2)

iris$ID <- 1:NROW(iris)

iris_long <- melt(iris, id.vars = c("ID", "Species"),
                  variable.name = "measurement",
                  value.name = "value")

iris_long <- separate(iris_long, measurement, into = c("part","measurement")) 

t.test(iris_long$value[iris_long$measurement == "Length"] ~ 
         iris_long$part[iris_long$measurement == "Length"], paired = TRUE)


################################################################################
### ANOVA ###

# An ANOVA is similar to a t-test in that it tests for whether means vary across
# groups. However, an ANOVA is used when there are more than two groups.

# A one-way ANOVA is used when we have one independent variable. We can use it to 
# test whether sepal length is different among the three species.

# We can first look at group means:  

tapply(iris$Sepal.Length, iris$Species, mean)

# Then perform our ANOVA:

sepal_anova <- aov(Sepal.Length ~ Species, data = iris)
summary(sepal_anova)

# To see which pairs of groups differ from each other, we perform post-hoc tests:

TukeyHSD(sepal_anova)

################################################################################
### Correlation and regression ###

# We use a correlation test to see if two continuous variables covary. 

# For example, the following will test covariance between sepal length and sepal
# width:

cor.test(iris$Sepal.Length, iris$Sepal.Width)

# A regression with a single predictor (sepal width) provides the same result:

sepal_model <- lm(Sepal.Length ~ Sepal.Width, data = iris)
summary(sepal_model)

# But we can add other predictors to our model, too:

sepal_model2 <- lm(Sepal.Length ~ Sepal.Width + Petal.Length, data = iris)
summary(sepal_model2)

# And we can even add a combination of factor and coninuous variables:

sepal_model3 <- lm(Sepal.Length ~ Sepal.Width + Petal.Length + Species, data = iris)
anova(sepal_model3)
summary(sepal_model3)

# A neat fact is that ANOVA is a special form of regression. We can use species to predict
# sepal length, and will see some familiar values. Examine the results of the 
# following model:

sepal_model4 <- lm(Sepal.Length ~ Species, data = iris)
anova(sepal_model4)
summary(sepal_model4)

tapply(iris$Sepal.Length, iris$Species, mean)

sepal_model4$coefficients[1] # setosa mean
sepal_model4$coefficients[1] + sepal_model4$coefficients[2] # versicolor mean
sepal_model4$coefficients[1] + sepal_model4$coefficients[3] # virginica mean

################################################################################
### Exercises ###

# You will use the mtcars data set for these exercises.

head(mtcars)
str(mtcars)

# For these exercises, conduct your tests here record your answers in the accompanying 
# worksheet. Some code is provided to get you started.

## Exercise 1: The average car gets 24.9 miles per gallon. Does our 
# sample of cars get significantly different mileage?

mean(mtcars$mpg)

t.test(mtcars$mpg, mu = 24.9)

## Exercise 2: Do automatic and manual cars get significantly different mileage?

# Important! Is "am" recognized as a factor variable? If not, make it one.

str(mtcars)
mtcars$am <- factor(mtcars$am)

# Calculate the means of each group and conduct the appropriate test:

tapply(mtcars$mpg, mtcars$am, mean)

t.test(mtcars$mpg ~ mtcars$am, var = TRUE)

# or:

mtcars_ttest <- lm(mpg ~ am, data = mtcars)
summary(mtcars_ttest)

## Exercise 3: Run the below code to add a variable to mtcars with each car's
## average mileage from 2019. Does the mileage today differ from the mileage in 1974?

mtcars$mpg_current <- c(35, 35, 41, 26, 24, 24, 17, 29, 27, 26, 
                        27, 27, 20, 12, 12, 16, 34, 33, 35, 21, 
                        10, 12, 12, 17, 26, 31, 43, 11, 19, 21, 
                        22, 35)

mean(mtcars$mpg_current)
mean(mtcars$mpg)

t.test(mtcars$mpg, mtcars$mpg_current, paired = TRUE)

## Exercise 4: Is there an interaction between transmission type (automatic or manual) and 
## engine type (V-shaped or straight) on mileage?

# This is called a factorial or two-way ANOVA since we have TWO independent variables,
# and are comparing four groups (2 x 2).

# Important! Always make sure both variables are recognized as factors.

mtcars$am <- factor(mtcars$am)
levels(mtcars$am) <- c("automatic", "manual")
mtcars$vs <- factor(mtcars$vs)
levels(mtcars$vs) <- c("v-shaped", "straight")

# Calculate the means of all four groups:

tapply(mtcars$mpg, list(mtcars$am, mtcars$vs), mean)

# A factorial ANOVA follows similar syntax to a one-way ANOVA, but we include an * between
# our two factor variables to test for an interaction.

car_anova <- aov(mpg ~ am * vs, data = mtcars)
summary(car_anova)
car_lm <- lm(mpg ~ am * vs, data = mtcars)
anova(car_lm)
summary(car_lm)

# Examine the coefficients in our model - do any of them look like familiar values? Can you
# add them up to calculate marginal (group) means across the four levels?

car_lm$coefficients[1] # mean for automatic, v-shaped 
car_lm$coefficients[1] + car_lm$coefficients[2] # mean for manual, v-shaped
car_lm$coefficients[1] + car_lm$coefficients[3] # mean for automatic, straight
car_lm$coefficients[1] + car_lm$coefficients[2] + 
  car_lm$coefficients[3] + car_lm$coefficients[4] # mean for manual, straight

# Run follow-up tests:

TukeyHSD(car_anova)

## Exericse 5: Do heavier cars get worse mileage?

cor.test(mtcars$mpg, mtcars$wt)

## Exercise 6: Which has a greater effect on mielage: weight or horsepower?

# Conduct a model with two predictors to answer this question.

car_model <- lm(mpg ~ wt + hp, data = mtcars)
summary(car_model)

car_model_scaled <- lm(scale(mpg) ~ scale(wt) + scale(hp), data = mtcars)
summary(car_model_scaled)
