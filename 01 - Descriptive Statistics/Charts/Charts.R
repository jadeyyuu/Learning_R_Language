#********************************************
# R Tutorial for Pratical
# Objectives:
# - create basic charts
# 
# Author:  Jadeyyuu
# First Version: July 28th, 2020
# Current Version: January 15th, 2021
#********************************************

# set your working directory
setwd("select your working directory")


#############################################################
# Summarizing categorical variables
# note: in general, I recommend the ggplot2 package for visualization as it offers superior functionality
# for class purposes, feel free to use the built-in graph features, though
#############################################################

# read in the data set
soft_drink = read.csv("SoftDrink.csv")

# let's start with a frequency distribution table
(frequency_distribution = table(soft_drink$Brand.Purchased)) # note that R chooses the intervals in this case

# we can also get the relative and percent frequency
(relative_frequency = frequency_distribution / sum(frequency_distribution))
(percent_frequency = relative_frequency * 100)

# create a bar chart for the soft drink variable using the standard frequency distribution
barplot(frequency_distribution, main="Bar Chart of Soft Drinks", xlab = "Soft Drink Brand")

# we can also create pie charts
pie(frequency_distribution)


#############################################################
# Summarizing quantitative variables
#############################################################

# read in the data set
Auto = read.csv("Auto.csv")

# we can use the built-in table function to summarize quantiative variables using intervals/bins
(standard_frequency = table(Auto$mpg))

# if we want to use our own intervals, we need to specify them
intervals = seq(from = min(Auto$mpg), to = max(Auto$mpg), by = 5) # this creates intervals of width 5

# then, we can let R calculate which observation falls in which interval and then get the frequency distribution
categorized_mpg = cut(Auto$mpg, intervals)
(interval_frequency = table(categorized_mpg))

# we can also get the relative and percent frequency
(relative_frequency = interval_frequency / sum(interval_frequency))
(percent_frequency = relative_frequency * 100)


# we can build a histogram
hist(Auto$mpg)

# or a dot plot
dotchart(Auto$mpg) #using the standard interval of 1

# get the cumulative distribution function using the earlier specified intervals
cumulative_distribution = ecdf(Auto$mpg)
(cumulative_distribution_table = cumulative_distribution(intervals))

# finally, a stem and leaf diagram
stem(Auto$mpg, scale = 2) # note that we have to adjust the scale parameter depending on the data!


#############################################################
# Hands-on session 1: Tabular and Graphics for Categorical and Quantitative Data
#############################################################

# read in the dataset NewSAT.csv
sat = read.csv("NewSAT.csv")

# first, create intervals with a width of 200 points, starting at 800 and ending at 2400
intervals = seq(to = 2400, from = 800, by = 200)
intervals = seq(800, 2400, 200)

# second, categorize the scores using the intervals
intervals_cut = cut(sat$SAT_Scores, intervals)

# then, create the frequency distribution for the SAT scores
(sat_categorized = table(intervals_cut))


# also, create a histogram of the SAT scores

hist(sat$SAT_Scores)

#############################################################
# Summarizing two variables
#############################################################

# read in the data set
restaurant = read.csv("Restaurant.csv")

# let's start with cross-tabulation

# we'll use the xtabs function for this. it uses the so-called formula notation, which we will use a lot in the later chapters
(crosstab = xtabs( ~ QualityRating + MealPrice, data = restaurant))

# if we want to use different intervals, we need to specify them
intervals = seq(from = 10, to = 50, by = 10)
restaurant$MealPriceInterval = cut(restaurant$MealPrice, intervals)

(crosstab = xtabs( ~ QualityRating + MealPriceInterval, data = restaurant))


# finally, let's create a scatterplot where we plot values of mpg against horsepower
plot(Auto$mpg,Auto$horsepower)



#############################################################
# Hands-on session 2: Tabular and Graphics for Two Variables
#############################################################

# read in the dataset Colleges.csv
colleges = read.csv("Colleges.csv")

# Use classes starting with 1600 and ending with 2000 in increments of 50 for Year Founded. 
# For Tuition & Fees, use classes starting with 1 and ending 45,000 in increments of 5000.
intervals_year = seq(from = 1600, to = 2000, by = 50)
intervals_year_cut = cut(colleges$Year_Founded, intervals_year)

intervals_tuition = seq(from = 1, to = 45000, by = 5000)
intervals_tuition_cut = cut(colleges$Tuition_and_Fees, intervals_tuition)


# for that, create the intervals for both variables according to the information above 

# finally, use the xtabs function as above to create the crosstab information
(crosstab = xtabs( ~ intervals_tuition_cut + intervals_year_cut))


# plot the tuition vs the percent graduation
plot(colleges$Tuition_and_Fees, colleges$Percent_Graduate)
