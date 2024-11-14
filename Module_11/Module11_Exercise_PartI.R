getwd()


## Exercise 1 - Random mass of data

# Creating the data mass (although random, y has
# a relation with the data of x)
x <- seq(0, 100)
x

y <- 2 * x + 35
y

# Generating a normal distribution
y1 <- y + rnorm(101, 0, 50)
y1

hist(y1)

# I - Create a plot of the relationship of x and y1
plot(x, y1, pch = 19, col = "blue", xlab = "x", ylab = "y1", main = "Relationship of x and y1")

# II - Create a regression model for the two variables x and y1
model <- lm(y1 ~ x)
model

summary(modelo)

# III - Capture the coefficients
a <- model$coefficients[1]
a

b <- model$coefficients[2]
b

# Regression Formula
y2 <- a + b*x

# IV - View the regression line
lines(x, y2, col = "red", lwd = 2)

# Simulating other possible regression lines
y3 <- (y2[51]-50*(b-1))+(b-1)*x
y4 <- (y2[51]-50*(b+1))+(b+1)*x
y5 <- (y2[51]-50*(b+2))+(b+2)*x
lines(x,y3,lty=3)
lines(x,y4,lty=3)
lines(x,y5,lty=3)


## Exercise 2 - Research on age and reaction time

# Creating the data
Age <- c(9,13,14,21,15,18,20,8,14,23,16,21,10,12,20,
           9,13,5,15,21)

Time <- c(17.87,13.75,12.72,6.98,11.01,10.48,10.19,19.11,
           12.72,0.45,10.67,1.59,14.91,14.14,9.40,16.23,
           12.74,20.64,12.34,6.44)

# I - Create a ScatterPlot
plot(Age, Time, pch = 19, col = "blue", xlab = "Age", ylab = "Time", main = "Scatter Plot - Age vs. Time" )

# II - Create a regression model
model <- lm(Time ~ Age)
model

summary(model)

# III - Calculate the regression line
a <- model$coefficients[1]
a

b <- model$coefficients[2]
b

y2 <- 25.8134 - 0.9491 * Age
y2

# IV - Create the line graph
lines(Age, y2, col = "red", lwd = 2)

## Exercise 3 - Relationship between height and weight

# Creating the data
heights = c(176, 154, 138, 196, 132, 176, 181, 169, 150, 175)
weights = c(82, 49, 53, 112, 47, 69, 77, 71, 62, 78)

plot(heights, weights, pch = 16, cex = 1.3, col = "blue", 
     main = "Height x Weight", 
     ylab = "Body Weight (kg)", 
     xlab = "Height (cm)")

# I - Create a regression model
model <- lm(weights ~ heights)

# Viewing the model
model

summary(model)

# II - Generate the regression line
a <- model$coefficients[1]
a

b <- model$coefficients[2]
b

y2 <- a + b*heights
y2

# other way
# abline(a, b)

lines(heights, y2, col = "red", lwd = 2)

# Make weight predictions based on the new height list
heights2 = data.frame(c(179, 152, 134, 197, 131, 178, 185, 162, 155, 172))

weight_predictions <- predict(model, newdata = heights2)
weight_predictions

# Plot
plot(heights, weights, pch = 16, cex = 1.3, col = "blue", 
     main = "Height x Weight", 
     ylab = "Body Weight (kg)", 
     xlab = "Height (cm)")

# Building the regression line
abline(lm(weights ~ heights)) 

# Getting the size of one of the data samples
num <- length(heights)
num

# Generating a graph with residual values
for (k in 1: num)  
  lines(c(heights[k], heights[k]), 
        c(weights[k], weights[k]))

# Generating graphics with the distribution of residuals
par(mfrow = c(2,2))
plot(modelo)

