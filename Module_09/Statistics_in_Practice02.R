# Statistics in Practice 02

# For EP2, we will use the "sleep" dataset available in the datasets package in 
# R as the data source.

# This dataset is the result of a research study involving patients with sleep 
# difficulties. The patients were divided into two groups, with each group 
# receiving a different medication to treat sleep disorders and help increase 
# their sleep duration.

# The dataset contains 3 variables:
# extra – A numerical variable indicating how many hours more or less the patient 
# slept after taking the medication. This will be our dependent variable.
# group – A factor (categorical) variable indicating the medication used by the 
# patient (1 or 2). This will be our independent variable.
# ID – Patient identification.

# Our goal in this study is to answer the following question:
  
# “Is there a significant difference in the average sleep duration between the
# two patient groups? In other words, is there a significant difference between 
# the two medications for treating sleep disorders?”

# Since we have two samples (two groups), we can apply the t-test to answer this 
# question. However, before applying the t-test, we first need to validate its 
# assumptions (explained in the previous section), and for that, we need the 
# Shapiro-Wilk Test and the F-Test.

# We define the following hypotheses for our test:
# H0 (Null Hypothesis) = There is no significant difference between the means 
# of the two groups.
# Ha (Alternative Hypothesis) = There is a significant difference between the 
# means of the two groups.

# The interpretation of the t-test result will help us determine whether we 
# should reject the null hypothesis and answer the business question of this 
# case study. Everything will be explained in detail in the upcoming lessons.

# Packages
if(!require(car)) install.packages("car")
library(car)
library(tidyverse)

View(sleep)

# Assumptions for Applying the t-Test

# To apply the t-test, we first need to validate the 5 assumptions of the test.
# 1 - The data is random and representative of the population.
# 2 - The dependent variable is continuous.
# 3 - Both groups are independent (i.e., the groups are exhaustive and exclusive).
# 4 - The model's residuals are normally distributed.
# 5 - The residual variance is homogeneous (homoscedasticity principle).

# For our example in this case study, we will assume that assumptions 1 to 3 
# are true and validate assumptions 4 and 5. For assumption 4, we will use the 
# Shapiro-Wilk Test, and for assumption 5, we will use the F-Test.

# Validating Assumptions with Shapiro-Wilk Test and Interpreting the Result

# Let's extract data from one of the groups
grupo_dois <- sleep$group == 2

# Validating Assumption 4 with qqPlot
# ?ggplot
qqPlot(sleep$extra[grupo_dois])

qqPlot(sleep$extra[! grupo_dois])

# Analysis: The data points for the variable "extra" are within the confidence 
# interval, indicating that the data follow a normal distribution.

# Validating Assumption 4 with the Shapiro-Wilk normality test (shapiro.test()):
# To claim that a distribution is normal, the p-value must be greater than 0.05.
# H0 = The data follow a normal distribution.
# ?shapiro.test

shapiro.test(sleep$extra[grupo_dois]) # p-value = 0.3511 > 0.05

shapiro.test(sleep$extra[! grupo_dois]) # p-value = 0.4079 > 0.05

# The p-value of the test for each group is greater than 0.05, and thus we fail 
# to reject the null hypothesis (H0).
# We can assume that the data follow a normal distribution.

# Validating Assumptions with F-Test and Interpreting the Result

# Validating Assumption 5 with F-Test
# First, we check if there are any missing values.
colSums(is.na(sleep))

# Let's look at a statistical summary of the dataset.
sleep %>% group_by(group) %>%
  summarise(
    count = n(),
    mean = mean(extra, na.rm = TRUE),
    sd = sd(extra, na.rm = TRUE))

# To reject the null hypothesis that the group means are equal, we need a high 
# F-value.
# H0 = The means of data extracted from a normally distributed population have 
# the same variance.
# ?var.test
resultado_teste_f <- var.test(extra ~ group, data = sleep)
resultado_teste_f

# The p-value is 0.7427, which is greater than 0.05. We fail to reject H0.
# There is no significant difference between the variances of the two groups.

# Validating Assumptions with t-Test and Interpreting the Result

# Assumptions validated. Now we can apply the t-Test.

# We apply the t-Test to answer the question:
# H0 (Null Hypothesis) – There is no significant difference between the means of 
# the two groups.
# ?t.test

resultado_teste_t <- t.test(extra ~ group, data = sleep, var.equal = TRUE)
resultado_teste_t

# Final Analysis:
  
# The p-value of the test is 0.07919, which is greater than 0.05. We fail to 
# reject H0.
# We can conclude that the two groups do not have a significant difference.
# There is no significant difference between the medications used to treat sleep 
# disorders.