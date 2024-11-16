# Exercise 1 - Create a function that receives the two vectors below as a parameter,
# convert them to a dataframe and print to the console
vec1 <- (10:13)
vec2 <- c("a", "b", "c", "d")

answer1 <- function(x,y) {
  df <- data.frame(cbind(x,y))
  View(df)
}

answer1(vec1,vec2)

# Exercise 2 - Create a matrix with 4 rows and 4 columns filled with
# whole numbers and calculate the average of each row
answer2a <- matrix (c(1:16), nr = 4, nc = 4)
answer2a

answer2b <- apply(answer2a, 1, mean)
answer2b

# Exercise 3 - Consider the dataframe below.
# Calculate the average per course and then calculate the average for just one course
school <- data.frame(Student = c('Alan', 'Alice', 'Alana', 'Aline', 'Alex', 'Ajay'),
                     Math = c(90, 80, 85, 87, 56, 79),
                     Geography = c(100, 78, 86, 90, 98, 67),
                     Chemistry = c(76, 56, 89, 90, 100, 87))

answer3a <- apply(school[, c(2, 3, 4)], 2, mean)
answer3a

answer3b <- apply(school[, c(2), drop = F], 2, mean)
answer3b

answer3b <- apply(school[, c(3), drop = F], 2, mean)
answer3b

answer3b <- apply(school[, c(4), drop = F], 2, mean)
answer3b

# Exercise 4 - Create a list with 3 elements, all numeric
# and compute the sum of all elements in the list
answer4a <- list(x = 1:10, b = 20:30, c = 50:100 )
answer4a

answer4b <- sum(unlist(answer4a))
answer4b

answer4b <- do.call(sum, answer4a)
answer4b

# Exercise 5 - Transform the previous list into a vector
answer5 <- unlist(answer4a)
answer5

# Exercise 6 - Consider the string below. Replace the word "texts" with "phrases"
str <- c("Expressions", "regulars", "in R language",
         "allow the search for patterns", "and exploration of texts",
         "we can look for patterns in digits",
         "Like for example",
         "10992451280")

answer6 <- gsub("texts", "phrases", str)
answer6

# Exercise 7 - Using the mtcars dataset, create a graph with ggplot of type
# scatterplot. Use disp and mpg columns on x and y axes respectively
install.packages("ggplot2")
library(ggplot2)

View(mtcars)

ggplot(data = mtcars, aes(x = disp, y = mpg)) + geom_point()

# Exercise 8 - Consider the matrix below.
# Create a bar plot that represents the data in individual bars.
mat1 <- matrix(c(652,1537,598,242,36,46,38,21,218,327,106,67), nrow = 3, byrow = T)
mat1

?barplot

barplot(mat1, beside = T)

# Exercise 9 - What is the error in the code below?
date(diamonds)

View(diamonds)

ggplot(data = diamonds, aes(x = price, group = fill, fill = cut)) +
  geom_density(adjust = 1.5)

# The error is in the group parameter because the fill column does not exist.

# Exercise 10 - What is the error in the code below?
ggplot(mtcars, aes(x = as.factor(cyl), fill = as.factor(cyl))) +
  geom_barplot() +
  labs(fill = "cyl")

# The error is in the "geom_barplot()" function, it says that this function 
# could not be found. The correct would be "geom_bar()"