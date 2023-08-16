getwd()

# Part II
# Packages
install.packages("cluster")
library(datasets)
library(ggplot2)
library(cluster)

# We will use the iris dataset in this example
# The iris dataset has observations of 3 species of flowers 
#(Iris setosa, Iris virginica and Iris versocolor)
# For each flower, 4 measurements are used:
# length and width of the stem (sepal) and length and width of the petal (petal)

View(iris)
str(iris)
round(prop.table(table(iris$Species)) * 100, digits = 1)

# Exploratory data analysis with ggplot2
# See that the data clearly has groups with similar characteristics
ggplot(iris, aes(Petal.Length, Petal.Width, color = Species)) + geom_point(size = 3)

# Now we will use K-Means to try to group the data into clusters
set.seed(101)
help(kmeans)

# Exercise 1 - Using the kmeans() function, create a clustering 
# (unsupervised learning) model. Use the documentation to do your research.
# In this case, we already know how many groups (clusters) exist in our data (3)
# Notice that the iris dataset has 5 columns, but we are using the first 4
irisCluster <- kmeans(iris[, 1:4], 3, nstart = 20)
irisCluster

# Getting information about the clusters
# 3 clusters were created: cluster 1, 2 and 3
# Note that even though the algorithm divided the data into clusters, 
# there was a problem dividing some of the data,
# that despite having different characteristics, were in the same cluster
table(irisCluster$cluster, iris$Species)
irisCluster

# Visualizando os clusters
help(clusplot)

# Plot
clusplot(iris, irisCluster$cluster, color = TRUE, shade = TRUE, labels = 0, lines = 0, )




