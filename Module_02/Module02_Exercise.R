# Exercise 1 - Create a vector with 30 integers
answer1 <- c(1:30)
answer1

# Exercise 2 - Create a matrix with 4 rows and 4 columns filled with integers
answer2 <- matrix(1:16, nrow = 4, ncol = 4, byrow = T)
answer2

# Exercise 3 - Create a list by joining the previously created vector and matrix
answer3 <- list(answer1, answer2)
answer3

# Exercise 4 - Using the read.table() function read the file from the link below to a dataframe
# http://data.princeton.edu/wws509/datasets/effort.dat - https://grodri.github.io/datasets/effort.dat
?read.table()
answer4 <- data.frame(read.table('https://grodri.github.io/datasets/effort.dat'))
View(answer4)

# Exercise 5 - Transform the previous dataframe into a named dataframe with the following labels:
# c("config", "esfc", "chang")
names(answer4) <- c("config", "esfc", "chang")
answer4

# Exercise 6 - Print the iris dataframe to the screen, check how many dimensions there are in the iris dataframe and print a dataset summary
answer6a <- View(iris)
answer6a

answer6b <- dim(iris)
answer6b

answer6c <- str(iris)
answer6c

answer6d <- summary(iris)
answer6d


# Exercise 7 - Create a simple plot using the first two columns of the iris dataframe
View(iris)
answer7 <- plot(iris$Sepal.Length, iris$Sepal.Width)
answer7

# Exercise 8 - Using the subset function, create a new dataframe with the iris dataframe dataset where Sepal.Length > 7
# Tip: see the help to learn how to use the subset() function
?subset
answer8 <- subset(iris, Sepal.Length > 7)
answer8

# Exercise 9 (Desafio) - Create a dataframe that is a copy of the iris dataframe and using the slice() function, split the dataframe into a subset of 15 lines
# Tip 1: You will have to install and load the dplyr package
# Tip 2: Consult the help to learn how to use the slice() function
install.packages('dplyr')
library(dplyr)

answer9a <- iris
answer9a

?slice

answer9b <- slice(resposta9a, 1:15)
answer9b

# Exercise 10 - Use the filter function on your new dataframe created in the previous item and only return values where Sepal.Length > 6
# Tip: Use RSiteSearch to learn how to use the filter function

RSiteSearch('filter')

answer10 <- filter(answer9a, Sepal.Length > 6)
answer10
