#********************************************
# R Tutorial for CIST 2500
# Objectives:
# - implement numerical descriptive statistics
# 
# Author:  Dr. Christian Haas
# First Version: July 28th, 2017
# Current Version: January 15th, 2018
#********************************************

# set your working directory
setwd("select your working directory")


#############################################################
# Using numerical measures to summarize (quantitative) data
#############################################################

# read in the salary data
salaries = read.csv("Salaries.csv")

# calculate the mean 
n = nrow(salaries) # number of observations
(mean_salary_manual = sum(salaries$MonthlyStartingSalary) / n)

(mean_salary = mean(salaries$MonthlyStartingSalary)) # this is the built-in function to automatically calculate the mean

# calculate the median
# first: manual
# we have an even number of observations, so we need to use the average of the 6th and 7th sorted observation
salary_sorted = sort(salaries$MonthlyStartingSalary)
(median_salary_manual = (salary_sorted[6] + salary_sorted[7]) / 2)

# second: use the built-in function to automatically calculate the median
(median_salary = median(salaries$MonthlyStartingSalary))

# calculate the mode
mode(salaries$MonthlyStartingSalary) # not the mode that we want!!

# we have to create a new function to calculate the Mode!
Mode <- function(x) {
  ux <- unique(x)
  tab <- tabulate(match(x, ux)) 
  ux[tab == max(tab)]
}

# now we can use the new function like any regular function
(mode_salary = Mode(salaries$MonthlyStartingSalary))


# we can also calculate the weighted mean
# for this, we need to supply the weights as vector
(weights = c(0.25, 0.25, rep(0.05,10))) # note that we need to specify 12 weights
(weighted_mean_salary = weighted.mean(salaries$MonthlyStartingSalary, w = weights))


# calculate percentiles
percentiles = c(0, 0.2, 0.8, 1)
quantile(salaries$MonthlyStartingSalary, probs = percentiles)

percentiles = c(0, 0.25, 0.5, 0.75, 1)
(salary_percentiles = quantile(salaries$MonthlyStartingSalary, probs = percentiles))


# range of the salaries
(range_salary = max(salaries$MonthlyStartingSalary) - min(salaries$MonthlyStartingSalary))
(range_salary = range(salaries$MonthlyStartingSalary)) # note that this returns the actual range, i.e., 2 values


# interquartile range
(interquartile_range_salary = quantile(salaries$MonthlyStartingSalary, probs = c(0.75)) - quantile(salaries$MonthlyStartingSalary, probs = c(0.25)))


# variance of salaries
# first, let's do the manual calculation; note that we only have a sample, hence need to use the formula for sample standard deviation
n = nrow(salaries)
(salary_variance_manual = sum((salaries$MonthlyStartingSalary - mean(salaries$MonthlyStartingSalary))^2) / (n -1) )

# or, we use the built-in function
(salary_variance = var(salaries$MonthlyStartingSalary))

# standard deviation of sample salaries is similarly calculated
(salary_standard_deviation_manual = sqrt (sum((salaries$MonthlyStartingSalary - mean(salaries$MonthlyStartingSalary))^2) / (n - 1) ))
(salary_standard_deviation = sd(salaries$MonthlyStartingSalary))


# NOTE: If we want to get the population variance/sd, we either need to calculate it manually, or multiply the sample variance/sd with a correction factor
(salary_population_variance_manual = sum((salaries$MonthlyStartingSalary - mean(salaries$MonthlyStartingSalary))^2) / (n) )
(salary_population_variance = var(salaries$MonthlyStartingSalary)  * (n -1) / n )
# similar for standard deviation


#############################################################
# Hands-on Session 1: Numerical Mesaures 
#############################################################

# we use the Flights.csv dataset for this
flights = read.csv("Flights.csv")

# calculate the mean flight price from Atlanta to other cities and compare it to the mean flight price from Salt Lake City
(mean_price_Atl = mean(flights$Atlanta))

(mean_price_SLC = mean(flights$SaltLakeCity))

# then, calculate the range, variance, and standard deviation for flights from Atlanta and Salt Lake City
(range_ATL = range(flights$Atlanta))
(var_ATL = var(flights$Atlanta))
(sd_ATL = sd(flights$Atlanta))

(range_SLC = range(flights$SaltLakeCity))
(var_SLC = var(flights$SaltLakeCity))
(sd_SLC = sd(flights$SaltLakeCity))
# what could explain the observed difference in cost?


#############################################################
# Distribution Shape, Location, and Outliers
#############################################################

salaries_set2 = read.csv("MajorSalary.csv")

# let's calculate the skewness of the larger salary dataset. we need an extra package for that
library(e1071)
(skewness(salaries_set2$Monthly_Starting_Salary))
hist(salaries_set2$Monthly_Starting_Salary)

# excursus: using a different (nicer) graphics package
library(ggplot2)
ggplot(salaries_set2, aes(x = Monthly_Starting_Salary)) + geom_histogram()

# add the z scores to the data
(salaries$z_scores = (salaries$MonthlyStartingSalary - mean(salaries$MonthlyStartingSalary)) / sd(salaries$MonthlyStartingSalary))

# summary statistics 
summary(salaries$MonthlyStartingSalary)

# and also a boxplot
boxplot(salaries_set2$Monthly_Starting_Salary, xlab='Salaries')

# again, we can use the nicer ggplot package
ggplot(salaries_set2, aes(x = "Group 1" , y = Monthly_Starting_Salary)) + geom_boxplot()


#############################################################
# Using numerical measures to summarize two variables
#############################################################

# we'll use the Auto set for this again
Auto = read.csv("Auto.csv")

# let's start by calculating the covariance of horsepower and mpg
(cov(Auto$mpg, Auto$horsepower)) # this is a large negative number

# to get the standardized relationship between mpg and horsepower, we'll use the correlation coefficient
(cor(Auto$mpg, Auto$horsepower))

# we can also plot this relationship using a standard scatter plot
plot(Auto$mpg, Auto$horsepower, xlab = "Miles per Gallon", ylab = 'horsepower', main = 'Relationship between MPG and Horsepower')

ggplot(Auto, aes(x = mpg , y = horsepower)) + geom_point()


#############################################################
# Hands-on Session 2: Numerical Mesaures 
#############################################################

# we use the Travel.csv dataset for this
travel = read.csv("Travel.csv")

# first, calculate the mean and standard deviation of the number of rooms and the cost per night
mean_num_rooms = mean(travel$Rooms)
sd_rooms = sd(travel$Rooms)

mean_cost = mean(travel$Cost_per_Night)
sd_cost = mean(travel$Cost_per_Night)

# then, plot the two variables in a scatter diagram. do you see a relationship between them?
ggplot(travel, aes(x = Rooms, y = Cost_per_Night)) + geom_point()

# or
plot(travel$Rooms, travel$Cost_per_Night)

# finally, calculate the covariance and the correlation coefficient. What do they say about the linear association between the number of rooms and cost per night? Which measure would you prefer, and why?
(cov(travel$Rooms, travel$Cost_per_Night))
(cor(travel$Rooms, travel$Cost_per_Night)) # there seems to be a slightly negative relationship: hotels with more rooms tend to have slightly lower cost per night
