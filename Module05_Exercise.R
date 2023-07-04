setwd("C:/DSA/RDSA/Cap_05")
getwd()
# Exercise 1 - Find and correct the error in the instruction below:
write.table(mtcars, file = "mtcars2.txt", sep = "|", col.names = N, qmethod = "double")

?write.table

# The error is in "col.names = N", the correct thing would be "col.names= NA."

write.table(mtcars, file = "mtcars2.txt", sep = "|", col.names = NA, qmethod = "double")

# Exercise 2 - Find and correct the error in the instruction below:
install.packages("readr")
library(readr)
df_iris <- read_csv("iris.csv", col_types = matrix(
  Sepal.Length = col_double(1),
  Sepal.Width = col_double(1),
  Petal.Length = col_double(1),
  Petal.Width = col_double(1),
  Species = col_factor(c("setosa", "versicolor", "virginica"))
))

?read_csv

# Instead of "matrix" it would have to be "list" and the arguments only with "()"

df_iris <- read_csv("iris.csv", col_types = list(
  Sepal.Length = col_double(),
  Sepal.Width = col_double(),
  Petal.Length = col_double(),
  Petal.Width = col_double(),
  Species = col_factor(c("setosa", "versicolor", "virginica"))
))


# Exercise 3 - Below you will find two histograms created separately.
# But this makes it difficult to compare distributions. Create a new plot that makes the union
# of both histograms in only one plot area.

# random data
dataset1=rnorm(4000, 100, 30)
dataset2=rnorm(4000, 200, 30)

# Histograms
par(mfrow=c(1,2))
hist(dataset1, breaks=30 , xlim=c(0,300) , col=rgb(1,0,0,0.5) , xlab="Height" , ylab="Weight" , main="" )
hist(dataset2, breaks=30 , xlim=c(0,300) , col=rgb(0,0,1,0.5) , xlab="Height" , ylab="Weight" , main="")

# answer 3
par(mfrow=c(1,2))
hist(dataset1, breaks=30 , xlim=c(0,300) , col=rgb(1,0,0,0.5) , xlab="Altura" , ylab="Peso" , main="" )
hist(dataset2, breaks=30 , xlim=c(0,300) , col=rgb(0,0,1,0.5) , xlab="Altura" , ylab="Peso" , main="", add = TRUE)
legend("topright", c("Dataset 1", "Dataset 2"), fill = c(rgb(1, 0, 0, 0.5), rgb(0, 0, 1, 0.5)))

# Exercise 4 - Find and correct the error in the graph below
install.packages("plotly")
library(plotly)
head(iris)

plot_ly(iris,
        x = ~Petal.Length,
        y = ~Petal.Width,
        type="scatter",
        mode = "markers",
        color = Species , marker=list(size=20 , opacity=0.5))

?plot_ly

# Argument "color = Species", missing "~" before Species

plot_ly(iris,
        x = ~Petal.Length,
        y = ~Petal.Width,
        type="scatter",
        mode = "markers",
        color = ~Species , marker=list(size=20 , opacity=0.5))


# Exercise 5 - Attached you will find the file module5.png. Create the graph that results in this image.
library(plotly)

head(volcano)

?volcano

# 3D plot :
p = plot_ly(z = volcano, type = "surface")
p

# Exercise 6 - Load the module5.json file attached to this script and print the contents to the console
# Tip: Use the rjson package
install.packages("rjson")
library("rjson")

answer6 <- fromJSON(file = "/content/module5.json")
print(answer6)

class(answer6)

# Exercise 7 - Convert the object created by loading the json file from the previous exercise
# into a dataframe and print the contents to the console.

answer7 <- as.data.frame(answer6)
print(answer7)

class(answer7)

View(answer7)

# Exercise 8 - Load the module5.xml file attached to this script.
# Tip: Use the XML package
install.packages("XML")
library("XML")
library("methods")

answer8 <- xmlParse(file = "/content/module5.xml")
print(answer8)

class(answer8)

# Exercise 9 - Convert the object created in the previous exercise into a dataframe
answer9 <- xmlToDataFrame("module5.xml")
print(answer9)

class(answer9)

View(answer9)

# Exercise 10 - Upload the attached module5.csv file and then answer the following questions:
df_exercise10 <- read.csv("module5.csv")

View(df_exercise10)

# Question 1 - How many rows and how many columns does the resulting object have in R?
dim(df_exercise10)

print(ncol(df_exercise10))
print(nrow(df_exercise10))

# Question 2 - What is the highest salary?
max(df_exercise10$salary)

# Question 3 - Print the record of the person with the highest salary on the console.
name_highest_salary <- subset(df_exercise10, salary == max(salary))
print(name_highest_salary)

# Question 4 - Print to the console all the people who work in the IT department.
IT_department_names <- subset(df_exercise10, dept == "IT")
print(IT_department_names)

# Question 5 - Print in the console the people in the IT department with a salary greater than 600.
salaries_IT_department_greater_600 <- subset(df_exercise10, salary > 600 & dept == "IT")
print(salaries_IT_department_greater_600)
