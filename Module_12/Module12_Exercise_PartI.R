getwd()
# Part I
# Packages
install.packages('rpart')
install.packages('rpart.plot')
library(rpart)
library(rpart.plot)

# We will use a dataset that is provided along with the rpart package
# "Data kyphosis"
?kyphosis

# Review kyphosis
str(kyphosis)

round(prop.table(table(kyphosis$Kyphosis)) * 100, digits = 1)

# Exercise 1 - After exploring the dataset, create a decision tree model
?rpart

tree <- rpart(Kyphosis ~ ., method = "class", data = kyphosis)
tree

# To examine the result of a decision tree, there are several functions, 
# but you can use printcp()
printcp(tree)

# Viewing the tree (run one function for the plot and another for the text on the plot)
# Use the zoom to better visualize the graph
plot(tree, uniform = TRUE, main = "Decision Tree in R")
text(tree, use.n = TRUE, all = TRUE)

# This other package makes the view more readable
prp(tree)