# Lab 2 - Machine Learning in Digital Marketing - Predicting the Number of Converted Users

# You have been hired as a Data Scientist by a company that sells digital products. 
# The company employs various Digital Marketing strategies and aims to develop a 
# Machine Learning model capable of predicting how many users will convert (i.e., 
# how many people will purchase the company's products) after each campaign. 
# With this prediction, the company can better understand how much to invest in 
# each campaign and the expected return. This will also help in the company's 
# planning for product commercialization and delivery, as well as the use of tools 
# and social media.

# Historical data from past campaigns is available, and your job as a Data 
# Scientist is to build a model that can predict the number of converted users 
# for a Digital Marketing campaign based on new data. Additionally, the Marketing 
# Manager needs to understand the potential increase in converted users for every 
# additional unit spent on a campaign.

# However, the data contains issues (similar to what you would encounter in 
# real-world scenarios), and you must identify these issues, decide on the best 
# strategies to resolve them, and then create your model. It might be necessary 
# to create different versions of the model before arriving at the optimal one. 
# Once you have the ideal version, you must provide a complete interpretation of 
# how the model generates its results to give managers more confidence in using it.

# Finally, you must deliver a solution for deploying the model and using it 
# immediately with new data.

# We will use fictitious data that represent real-world scenarios.  

# Getting the current working directory
#setwd("")
getwd()

# Packages - Library
# install.packages("tidyverse")
# install.packages("corrplot")
# install.packages("ggplot2")
library(tidyverse)
library(corrplot)
library(ggplot2)

# Load the dataset
df_dsa <- read.csv("lab02.csv")

# View the data
View(df_dsa)

# Data types
print(str(df_dsa))

##### Exploratory Data Analysis (EDA) ##### 

# Statistical Summary
summary(df_dsa)

# Calculating the Correlation Matrix
# ?cor
cor_matrix <- cor(df_dsa)

# Corrplot
# For other colors, use: colors()
# ?corrplot
corrplot(cor_matrix, 
         method = 'color', 
         type = 'upper', 
         addCoef.col = 'springgreen2', 
         tl.col = "black", 
         tl.srt = 45)

# Scatter plot between Amount Spent on Campaign and Converted Users
ggplot(df_dsa, aes(x=valor_gasto_campanha, y=usuarios_convertidos)) +
  geom_point(aes(color=valor_gasto_campanha), alpha=0.6) +
  ggtitle("Scatter Plot between Amount Spent on Campaign and Converted Users") +
  xlab("Amount Spent on Campaign") +
  ylab("Converted Users")

# Scatter plot between Number of Views and Converted Users
ggplot(df_dsa, aes(x=numero_visualizacoes, y=usuarios_convertidos)) +
  geom_point(aes(color=numero_visualizacoes), alpha=0.6) +
  ggtitle("Scatter Plot between Number of Views and Converted Users") +
  xlab("Number of Views") +
  ylab("Converted Users")

# Scatter plot between Number of Clicks and Converted Users
ggplot(df_dsa, aes(x=numero_cliques, y=usuarios_convertidos)) +
  geom_point(aes(color=numero_cliques), alpha=0.6) +
  ggtitle("Scatter Plot between Number of Clicks and Converted Users") +
  xlab("Number of Clicks") +
  ylab("Converted Users")

##### Modeling #####

# Version 1 of the Model - Multiple Linear Regression
# ?lm
modelo_v1 <- lm(usuarios_convertidos ~ valor_gasto_campanha + numero_visualizacoes + numero_cliques, data = df_dsa)
summary(modelo_v1)

# Components of the Summary

# Regression Formula
# y = a + bx
# y = a + b1x1 + b2x2 + b3x3

# Residuals
# Residuals are the differences between the observed values and the values predicted by the model. 
# The quartiles of the residuals (Min, 1Q, Median, 3Q, Max) give an idea of the distribution of errors. 
# In general, you would like these values to be symmetrically distributed around zero, which indicates that 
# the model does a good job in prediction.

# Coefficients

# (Intercept): This is the value of the dependent variable (converted users) when all the independent variables 
# are zero. The value is -1.563, but the associated p-value is greater than 0.05, which indicates that the 
# intercept is not significantly different from zero in this model.

# valor_gasto_campanha: The coefficient is 0.0078, but the associated p-value is 0.621, which indicates that this 
# variable is not statistically significant in predicting converted users, at least in this model.

# numero_visualizacoes: The coefficient is -0.0035, with a p-value of 0.265. This also suggests that the variable 
# is not significant.

# numero_cliques: The coefficient is 0.944, with an extremely low p-value (< 2e-16). This indicates that this 
# variable is highly significant in predicting converted users.

# Other Metrics
# Residual standard error: This is a measure of the model's quality. The lower it is, the better the model. 
# In this case, it is 6.865.

# Multiple R-squared and Adjusted R-squared: These are indicators of the "quality" of the model in terms 
# of its ability to predict the dependent variable. A value closer to 1 is generally better. 
# In this case, they are relatively high (0.8617 and 0.8609, respectively), which is good.

# F-statistic and p-value: An F-test is performed to determine whether the model as a whole is significant. 
# The F-value is 1030, and the associated p-value is very low (< 2.2e-16), indicating that the model is significant.

# Final Interpretation
# The model seems to do a good job in predicting "converted users" (high R-squared), but only 
# the "number of clicks" variable is statistically significant in the prediction. This may imply that 
# "number of clicks" is the main variable you should focus on to understand user conversions.

# The other variables (campaign spend and number of views) are not significant in this model, 
# which suggests that they may not be useful in predicting the dependent variable, or that other factors may 
# be at play, such as multicollinearity.

# Version 2 of the Model - Simple Linear Regression
modelo_v2 <- lm(usuarios_convertidos ~ numero_cliques, data = df_dsa)
summary(modelo_v2)

# Version 3 of the Model - Feature Engineering Before Multiple Linear Regression

# Create the new variable taxa_de_clique
df_dsa$taxa_de_clique <- df_dsa$numero_cliques / df_dsa$numero_visualizacoes
View(df_dsa)

# Check if any value is zero (always check when dividing values)
any(df_dsa$taxa_de_clique == 0)

# Calculating the correlation matrix
cor_matrix <- cor(df_dsa)

# Corrplot
corrplot(cor_matrix, 
         method = 'color', 
         type = 'upper', 
         addCoef.col = 'springgreen2', 
         tl.col = "black", 
         tl.srt = 45)

# Version 3 of the model
modelo_v3 <- lm(usuarios_convertidos ~ valor_gasto_campanha + taxa_de_clique, data = df_dsa)
summary(modelo_v3)

# Summary Components

# Residuals:
# This section shows a statistical summary of the residuals (difference between
# observed values and values predicted by the model).

# Min, 1Q, Median, 3Q, Max describe the distribution of the residuals. 
# The goal is for these values to be symmetrically distributed around zero. In 
# this case, it seems that the median is close to zero, which is a good sign.

# Coefficients:
# This section describes the regression model coefficients.

# Estimate: The estimate of the coefficients. For example, for each unit increase 
# in valor_gasto_campanha, the variable usuarios_convertidos increases by an average 
# of 0.05105 units, keeping taxa_de_clique constant.

# Std. Error: The standard error of the coefficients, a measure of the variation 
# of the coefficients.

# t value: The t statistic, used to test the null hypothesis that the coefficient 
# is equal to zero (no effect). 
# A high t value may indicate that the variable is significant.

# Pr(>|t|): The p-value associated with the t statistic. A very low p-value 
# (< 0.05) indicates that you can reject the null hypothesis. This means the 
# coefficient is statistically significant in predicting the target variable.

# All coefficients are highly significant (p-value < 2e-16), indicating that 
# both are important predictors of the target variable.

# Other Statistics:
# Residual standard error: It is a measure of how well the model fits the data. 
# The lower it is, the better, although it should be interpreted in the context 
# of the problem.

# Multiple R-squared and Adjusted R-squared: These are measures that indicate 
# the proportion of variation in the dependent variable explained by the model. 
# The value is 0.8418, which is relatively high and indicates a good fit.

# F-statistic and p-value: These statistics test the null hypothesis that all 
# the regression coefficients are equal to zero. Given the extremely low p-value, 
# you can reject this hypothesis.

# Interpretation:

# The model explains approximately 84.18% of the variation in usuarios_convertidos, 
# which is good.

# The coefficient for valor_gasto_campanha is 0.05105, and for taxa_de_clique, 
# it is 3613. This means that, keeping all other variables constant, an increase
# of one unit in valor_gasto_campanha will result in an increase of 0.05105 units 
# in usuarios_convertidos, and an increase of one unit in taxa_de_clique will 
# increase usuarios_convertidos by 3613 units.

# All predictors are significant, with very low p-values.

# The model is statistically significant, as indicated by the p-value close to 
# zero for the F statistic.

# Remember that these are purely statistical interpretations. 
# The practical validity of these results should be evaluated in the context of 
# the business problem you are trying to solve.

# Let's check the assumptions of the regression model:

# Get the residuals from the model
residuals <- resid(modelo_v3)

# Residuals vs Fitted Values Plot
# This plot helps to check the assumption of homoscedasticity. 
# You expect to see a cloud of points with no clear pattern.
ggplot(df_dsa, aes(x = predict(modelo_v3), y = residuals)) +
  geom_point() +
  geom_smooth(se = FALSE, method = 'loess') +
  ggtitle("Residuals vs Fitted Values") +
  xlab("Fitted Values") +
  ylab("Residuals")

# Histogram of Residuals
# This plot helps to check the normality of the residuals. 
# A bell-shaped histogram indicates that the residuals are normally distributed, suggesting a good 
# regression model.
ggplot(df_dsa, aes(x = residuals)) +
  geom_histogram(binwidth = 1, fill = 'blue', alpha = 0.7) +
  ggtitle("Histogram of Residuals") +
  xlab("Residuals")

# QQ-plot
# This plot also helps to check the normality of the residuals. 
# Points aligned along the diagonal line suggest that the residuals are normally distributed, indicating 
# a good regression model.
ggplot(df_dsa, aes(sample = residuals)) +
  geom_qq() +
  geom_qq_line() +
  ggtitle("QQ-Plot of Residuals") +
  xlab("Theoretical Quantiles") +
  ylab("Sample Quantiles")

# Model Deployment

# Save the trained model to disk
save(modelo_v3, file = "modelo_v3.RData")

# Load the model from disk
load("modelo_v3.RData")

# New data:
valor_gasto_campanha <- 1350
numero_visualizacoes <- 7300
numero_cliques <- 100

# Create new data for prediction
novos_dados <- data.frame(valor_gasto_campanha = c(1350),
                          numero_visualizacoes = c(7300),
                          numero_cliques = c(100))

# Create the new variable as done for training the model
# Any transformation applied to the training data 
# should also be applied to the test data and new data
novos_dados$taxa_de_clique <- novos_dados$numero_cliques / novos_dados$numero_visualizacoes

# Remove variables that will not be used
novos_dados$numero_visualizacoes <- NULL
novos_dados$numero_cliques <- NULL

# View
View(novos_dados)

# Make predictions
previsoes <- predict(modelo_v3, newdata = novos_dados)

# Display predictions
cat("We expect this number of converted users:", as.integer(previsoes))
