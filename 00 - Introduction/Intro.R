#********************************************
# R Tutorial - 001 Basic Intro 
# Objectives:
# - Set working directory
# - import data
# - use basic indexing
# - use basic functions
# 
# Author: Jadeyyuu
# First Version: July 8th, 2020
# Current Version: June 28th, 2020
#********************************************

# set your working directory
setwd("select your working directory")

# let's import the dataset 

# load the sample data into the workspace. Note: Auto.csv must be saved in your working directory in case of manual import
Auto <- read.csv("Auto.csv")

# alternatively, use the Import Dataset button on the right

# show single variables:
Auto$mpg # this is a numerical variable
Auto$name # this is a categorical / quantitative variable

# now, let's look at how we can apply basic calculations in R

# operators (+, -, *, /) work both on single elements as well as vectors

4 * 5

Auto$mpg * 2


# if we want to only get parts of the dataset or variable, we can use the index functions

# get only the second element of the mpg variable
Auto$mpg[2]

# get only the first 5 elements of the mpg variable
Auto$mpg[1:5]

## for data sets, we have to use two indexes (row + column)
# get the first entry of the first row
Auto[1, 1]

# get the entire 3rd row
Auto[3, ]

# get the entire 2nd column
Auto[, 2]

# finally, let's look at some of the basic mathematical and statistical functions

# get the minimum mpg value
min(Auto$mpg)

# get the maximum horsepower
max(Auto$horsepower)

# bonus: get the name of the car with the maximum horsepower value
Auto[Auto$horsepower == max(Auto$horsepower), ]$name

