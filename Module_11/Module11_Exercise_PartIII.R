getwd()

# Defining the Problem: Analyzing data from houses in Boston, USA and making predictions.

# The Boston Housing Dataset
# http://www.cs.toronto.edu/~delve/data/boston/bostonDetail.html

# Your model must predict the MEDV (Median Occupancy Value of Houses). Use a neural network model!

# Loading the MASS package
library(MASS)

# Importing data from the Boston dataset
set.seed(101)

data <- Boston
View(data)

# Data summary
str(data)

summary(data)

any(is.na(data))

# Loading the package to Neural Networks
# install.packages("neuralnet")
library(neuralnet)

# Normalization
maxs <- apply(data, 2, max)
maxs

mins <- apply(data, 2, min)
mins

# Normalizing
normalized_data <- as.data.frame(scale(data, center = mins, scale = maxs - mins))
head(normalized_data)

# Creating the training and test data
# install.packages("caTools")
library(caTools)

split = sample.split(normalized_data$medv, SplitRatio = 0.70)

training = subset(normalized_data, split == TRUE)
test = subset(normalized_data, split == FALSE)

# Getting column names
column_names <- names(training)
column_names

# Adding
formula <- as.formula(paste("medv ~", paste(column_names[!column_names %in% "medv"], collapse = " + ")))
formula

# Training the Model
neural_net <- neuralnet(formula, data = training, hidden = c(5,3), linear.output = TRUE)

# Plot
plot(neural_net)

# Making predictions with test data
neural_net_prev <- compute(neural_net, test[1:13])
neural_net_prev

# The return of the Neural Network prediction is a list
str(neural_net_prev)

# Converting the test data
forecasts <- neural_net_prev$net.result * (max(data$medv) - min(data$medv)) + min(data$medv)
test_convert <- (test$medv) * (max(data$medv) - min(data$medv)) + min(data$medv)
test_convert

# Calculating the Mean Squared Error
MSE.nn <- sum((test_convert - forecasts)^2)/nrow(test)
MSE.nn

# Getting prediction errors
error.df <- data.frame(test_convert, forecasts)
head(error.df)

# Error plot
library(ggplot2)
ggplot(error.df, aes(x = test_convert, y = forecasts )) + 
  geom_point() + stat_smooth()






