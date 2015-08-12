library(datasets)
data(iris)
?iris
head(iris)
        # Data frame:   150 rows, 
        #               5 variables     Sepal.Length, 
        #                               Sepal.Width, 
        #                               Petal.Length, 
        #                               Petal.Width, and 
        #                               Species
#Question 1
#There will be an object called 'iris' in your workspace. 
#In this dataset, what is the mean of 'Sepal.Length' for the species virginica? 
tapply(iris[["Sepal.Length"]],iris[["Species"]],mean,simplify = TRUE)
#setosa versicolor  virginica 
#5.006      5.936      6.588
#* 6.588

#Question 2
#Continuing with the 'iris' dataset from the previous Question, 
#what R code returns a vector of the means of the variables 
#'Sepal.Length', 'Sepal.Width', 'Petal.Length', and 'Petal.Width'?
apply(iris[, 1:4], 2, mean)

#Question 3
#Load the 'mtcars' dataset in R with the following code
library(datasets)
data(mtcars)
#There will be an object names 'mtcars' in your workspace. 
#You can find some information about the dataset by running
?mtcars
#A data frame with 32 observations on 11 variables.
# [, 1]	mpg	Miles/(US) gallon
# [, 2]	cyl	Number of cylinders
# [, 3]	disp	Displacement (cu.in.)
# [, 4]	hp	Gross horsepower
# [, 5]	drat	Rear axle ratio
# [, 6]	wt	Weight (lb/1000)
# [, 7]	qsec	1/4 mile time
# [, 8]	vs	V/S
# [, 9]	am	Transmission (0 = automatic, 1 = manual)
# [,10]	gear	Number of forward gears
# [,11]	carb	Number of carburetors
#How can one calculate the average miles per gallon (mpg) by number of cylinders 
#in the car (cyl)?
sapply(split(mtcars$mpg, mtcars$cyl), mean)
# 4        6        8 
# 26.66364 19.74286 15.10000 
tapply(mtcars$mpg, mtcars$cyl, mean)
# 4        6        8 
# 26.66364 19.74286 15.10000 

#Question 4
#Continuing with the 'mtcars' dataset from the previous Question, 
#what is the absolute difference between 
# - the average horsepower of 4-cylinder cars and 
# - the average horsepower of 8-cylinder cars?
sapply(split(mtcars$hp,mtcars$cyl),mean)
# 4         6         8 
# 82.63636 122.28571 209.21429 
hpMeans<-sapply(split(mtcars$hp,mtcars$cyl),mean)
hpMeans[["8"]]-hpMeans[["4"]]
# [1] 126.5779

#Question 5
#If you run
debug(ls)
#what happens when you next call the 'ls' function?
ls()
# - You will be prompted to specify at which line 
#   of the function you would like to suspend execution and enter the browser.
# * Execution of 'ls' will suspend at the beginning of the function 
#   and you will be in the browser.
# - The 'ls' function will return an error.
# - The 'ls' function will execute as usual.
