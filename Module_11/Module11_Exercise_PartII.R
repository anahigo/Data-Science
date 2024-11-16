getwd()

# Linear Regression
# Problem Statement: Predict student grades based on various metrics
# https://archive.ics.uci.edu/ml/datasets/Student+Performance

# Loading the dataset
df <- read.csv2('./data/module11_part2.csv')

# Exploring the data
View(df)

summary(df)

str(df)

any(is.na(df))

# Packages-Library
# install.packages("ggplot2")
# install.packages("ggthemes")
# install.packages("dplyr")
library(ggplot2)
library(ggthemes)
library(dplyr)

# Getting only the numeric columns
numerical_columns <- sapply(df, is.numeric)
numerical_columns

# Filtering numeric columns for correlation
data_cor <- cor(df[,numerical_columns])
data_cor
View(data_cor)

# Packages to visualize the correlation analysis
# install.packages('corrgram')
# install.packages('corrplot')
# install.packages('ggplot2')
library(ggplot2)
library(corrplot)
library(corrgram)

# Creating a corrplot
corrplot(data_cor, method = 'color')

# Creating a corrgram
corrgram(df)
corrgram(df, order = TRUE, lower.panel = panel.shade,
         upper.panel = panel.pie, text.panel = panel.txt)

# Creating a histogram
ggplot(df, aes(x = G3)) + 
  geom_histogram(bins = 20, 
                 alpha = 0.5, fill = 'blue') + 
  theme_minimal()

# Training and Interpreting the Model
# Import Library
#install.packages("caTools")
library(caTools)

# Creating samples randomly
set.seed(101)

sample <- sample.split(df$age, SplitRatio = 0.70)

# ***** We train our model on the training data *****
# ***** We make the predictions on the test data *****

# Creating training data - 70% of the data
training = subset(df, sample == TRUE)

# Creating test data - 30% of the data
test = subset(df, sample == FALSE)

# Generating the Model (Using all attributes)
model_v1 <- lm(G3 ~ ., training)
model_v2 <- lm(G3 ~ G2 + G1, training)
model_v3 <- lm(G3 ~ absences, training)
model_v4 <- lm(G3 ~ Medu, training)

# Interpretando o Modelo
summary(model_v1) # 0.86
summary(model_v2) # 0.82
summary(model_v3) # 0.0002675
summary(model_v4) # 0.06442


# Viewing the Model and Making Predictions

# Getting the waste
res <- residuals(model_v1)

# Converting the object to a dataframe
res <- as.data.frame(res)
head(res)

# Histogram of residuals
ggplot(res, aes(res)) +  
  geom_histogram(fill = 'blue', 
                 alpha = 0.5, 
                 binwidth = 1)

# Model Plot
plot(model_v1)

# Making predictions
model_v1 <- lm(G3 ~ ., training)
predicting_G3 <- predict(model_v1, test)
predicting_G3

# Viewing predicted and observed values
results <- cbind(predicting_G3, test$G3) 
colnames(results) <- c('Forecast','Real')
results <- as.data.frame(results)
results

min(results)

# Handling negative values
treat_zero <- function(x){
  if  (x < 0){
    return(0)
  }else{
    return(x)
  }
}

# Applying the function to treat negative values in our prediction
results$Forecast <- sapply(results$Forecast, treat_zero)
results$Forecast

# Calculating the average error
# How far your predicted values are from the observed values
# MSE
mse <- mean((results$Real - results$Forecast)^2)
print(mse)

# RMSE
rmse <- mse^0.5
rmse

# Calculating R Squared
SSE = sum((results$Forecast - results$Real)^2)
SST = sum((mean(df$G3) - results$Real)^2)

# R-Squared
# It helps to evaluate the accuracy level of our model. The higher the better, with 1 being the ideal value.
R2 = 1 - (SSE/SST)
R2




