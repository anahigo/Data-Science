# Module 18 - Mini-Project 2 - Financial Analysis with SQL Language and Linear 
# Regression in R Language

# The dataset was generated from the sources below:
# https://www.cms.gov/Research-Statistics-Data-and-Systems/Statistics-Trends-and-Reports/Medicare-Provider-Cost-Report/HospitalCostPUF
# https://healthdata.gov/

getwd()

# Load the dataset
dt <- read.csv("dataset.csv")

# View the dataset
View(dt)

# Dimensions
dim(dt)

# Variables and data types
str(dt)

# Column names
colnames(dt)

# Writes column names to a vector
myColumns <- colnames(dt)
myColumns

# Renaming column names
myColumns[1] <- "Age"
myColumns[2] <- "Gender"
myColumns[3] <- "HospitalizationTime"
myColumns[4] <- "Race"
myColumns[5] <- "Cost"
myColumns[6] <- "DiagnosticGroup"

# Check the result
myColumns

# Assigns new column names to the dataframe
colnames(dt) <- myColumns

# View the dataset
View(dt)

# Numeric variable summaries
summary(dt)

# Let's eliminate lines with NA values
dt <- na.omit(dt)

# Analyzing the dataset
# Age
age <- unique(dt$Age)
range(age)
length(unique(dt$Age))
table(dt$Age)

# Gender
gender <- unique(dt$Gender)
range(gender)
table(dt$Gender)

# HospitalizationTime
hospitalization_time <- unique(dt$HospitalizationTime)
range(hospitalization_time)
length(unique(dt$HospitalizationTime))
table(dt$HospitalizationTime)

# Race
race <- unique(dt$Race)
range(race)
length(unique(dt$Race))
table(dt$Race)

# Cost
cost <- unique(dt$Cost)
range(cost)
length(unique(dt$Cost))
table(dt$Cost)

# DiagnosticGroup
diagnostic_group <- unique(dt$DiagnosticGroup)
range(diagnostic_group)
length(unique(dt$DiagnosticGroup))
table(dt$DiagnosticGroup)

# Answering the questions - Part 1 (SQL)
install.packages("sqldf")
library(sqldf)

colnames(dt)
# Age Gender HospitalizationTime Race Cost DiagnosticGroup

# Question 1 - How many breeds are represented in the dataset?

answer_1a <- sqldf("SELECT COUNT(DISTINCT Race) AS TotalRaces FROM dt")
answer_1a

answer_1b <- sqldf("SELECT Race, 
                      COUNT(Race) AS TotalRaces 
                      FROM dt 
                      GROUP BY Race")
answer_1b



# Question 2 - What is the average age of patients?
answer_2 <- sqldf("SELECT AVG(Age) AS AverageAge FROM dt")
answer_2
round(answer_2)



# Question 3 - What is the age range of patients?

resposta_3a <- sqldf("SELECT Age 
                      FROM dt
                      GROUP BY Age
                      HAVING COUNT(Age) =
                      (SELECT MAX(count_age) 
                      FROM (SELECT Age,
                      COUNT(Age) as count_age 
                      FROM dt
                      GROUP BY Age))")

resposta_3a

answer_3b <- sqldf("SELECT Age
                    FROM (SELECT Age, 
                    COUNT(Age) AS count_age 
                    FROM dt
                    GROUP BY Age
                    ORDER BY count_age DESC)
                    LIMIT 1")
answer_3b



# Question 4 - What is the variance of the age column?

answer_4a <- sqldf("SELECT VARIANCE(Age) AS AgeVariance FROM dt")
answer_4a

answer_4b <- sqldf("SELECT SUM((Age - 
                                  (SELECT AVG(Age) FROM dt)) *
                                  (Age - (SELECT AVG(Age) FROM dt))) / 
                                  (COUNT(Age) - 1) AS AgeVariance 
                                  FROM dt")
answer_4b



# Question 5 - What is the total expenditure on hospital admissions by age?

answer_5 <- sqldf("SELECT Age, 
                     SUM(Cost) AS TotalCost 
                     FROM dt 
                     GROUP BY Age")
answer_5



# Question 6 - What age generates the highest total expenditure on hospital 
# admissions?

answer_6 <- sqldf("SELECT Age, 
                     SUM(Cost) AS TotalCost 
                     FROM dt 
                     GROUP BY Age 
                     ORDER BY TotalCost 
                     DESC LIMIT 1")
answer_6



# Question 7 - What is the total expenditure on hospital admissions by gender?

answer_7 <- sqldf("SELECT Gender, 
                     SUM(Cost) AS TotalCost
                     FROM dt 
                     GROUP BY Gender")
answer_7



# Question 8 - What is the average cost of hospital admissions by patient race?

answer_8 <- sqldf("SELECT Race, 
                     AVG(Cost) AS AverageCost
                     FROM dt 
                     GROUP BY Race")
answer_8



# Question 9 - What is the average cost of hospital admissions by patient race?

answer_9 <- sqldf("SELECT AVG(Cost) AS AverageCost
                     FROM dt 
                     WHERE Age > 10 
                     GROUP BY Age")
answer_9



# Question 10 - Considering the previous item, what age has an average expense 
# greater than 3000?

answer_10 <- sqldf("SELECT Age, 
                      AVG(Cost) AS AverageCost 
                      FROM dt
                      WHERE Age > 10
                      GROUP BY Age
                      HAVING AverageCost > 3000")
answer_10



# Answering the questions - Part 2 (Linear Regression in R Language)

install.packages("dplyr")
library(dplyr)

colnames(dt)
# Age Gender HospitalizationTime Race Cost DiagnosticGroup

# Question 1 - What is the age distribution of patients attending the hospital?

# We convert the variable to factor type and then we get the summary we need.
answer_1 <- summary(as.factor(dt$Age))
answer_1


hist(dt$Age, 
     main = "Histogram of the Age of Patients Attending the Hospital",
     xlab = "Age",
     xlim = c(0,20),
     ylab = "Frequency",
     ylim = c(0,350),
     col = "lightblue",
     border = "black")

# Answer: Between 0 and 1 year of age, they are the ones who attend the hospital 
# the most.



# Question 2 - Which age group has the highest total hospital spending?

answer_2a <- aggregate(Cost ~ Age, 
                        FUN = sum, 
                        data = dt)
answer_2a      

answer_2b <- answer_2a[which.max(answer_2a$Cost), ]
answer_2b

barplot(tapply(answer_2a$Cost, 
               answer_2a$Age, 
               FUN = sum),
               main = "Total Spend by Age Group",
               xlab = "Age Range", 
               ylab = "Total Spend")

# Answer: Between 0 and 1 year of age have the highest hospital costs



# Question 3 - Which diagnosis-based group (Aprdrg) has the highest total 
# hospital spending?

answer_3a <- aggregate(Cost ~ DiagnosticGroup, 
                         FUN = sum, 
                         data = dt)
answer_3a 

answer_3b <- answer_3a[which.max(answer_3a$Cost), ]
answer_3b 

barplot(tapply(answer_3a$Cost, 
               answer_3a$DiagnosticGroup, 
               FUN = sum),
               main = "Total Spending by Diagnostic Group", 
               xlab = "Diagnostic Group", 
               ylab = "Total Spend")

# Answer: The diagnosis-based group that has the highest total hospital spending 
# is 640



# Question 4 - Does the patient's race have a relationship with the total spent 
# on hospital stays?

# Linear Regression
# Dependent variable: Cost
# Independent variables: Race

# H0: There is no linear relationship between dependent and independent 
# variables.
# H1: There is a linear relationship between dependent and independent 
# variables.

answer_4a <- lm(Cost ~ Race, data = dt)
summary(answer_4a) 

# The coefficient for the Race variable is -137.3, but the p-value associated 
# with this coefficient is 0.686. This means that the coefficient for the Race 
# variable is not statistically significant as the p-value is greater than 0.05
# (common significance level).
# The value of R² (R-squared) is very close to zero (0.0003299), indicating that
# the Race variable does not explain a significant proportion of the variability 
# in spent on hospitalizations. The adjusted R² value is even lower (-0.001681).
# The F-statistic is 0.164 with a p-value of 0.6856. This also suggests that the
# regression model is not statistically significant.

# The p-value is greater than 0.05. We failed to reject H0.

# ANOVA Test.
# Dependent variable in the ANOVA Test: Cost
# Independent variable in the ANOVA Test: Race

# H0: There is no effect of RACE on Cost.
# H1: There is RACE effect in Cost.

answer_4b <- aov(Cost ~ Race, data = dt)
summary(answer_4b) 

# The p-value is greater than 0.05. We failed to reject H0.
# The patient's race does not influence the total expenditure on hospital 
# admission.

# Answer: The Race variable does not have a significant relationship with 
# spending on hospitalizations in your data set. The high p-value and very low 
# R² value indicate that the race variable is not a significant predictor of 
# spending on hospitalizations.



# Question 5 - Does the combination of age and gender of patients influence the 
# total expenditure on hospital admissions?

# Linear Regression
# Dependent variable: Cost
# Independent variables: Age and Gender

# H0: There is no linear relationship between dependent and independent 
# variables.
# H1: There is a linear relationship between dependent and independent 
# variables.

answer_5a <- lm(Cost ~ Age + Gender, data = dt)
summary(answer_5a) 

# The coefficients for the independent variables are as follows:
# - Intercept (Intercept): 2719.45
# - Patient Age (PatientAge): 86.04
# - Sex (Sex): -744.21
# The p-values associated with the coefficients indicate statistical 
# significance:
# - The coefficient for Patient Age is statistically significant
# (p-value < 0.001), which suggests that the patient's age has an impact
#significant in the cost of hospitalizations.
# - The coefficient for Sex is also statistically significant
# (p-value = 0.036), indicating that the patient's gender has an impact
# significant in the cost of hospitalizations, although less significant than 
# the age.
# - The value of R² (R-squared) is 0.02585, which indicates that the variables
# independents in the model explain approximately 2.6% of the variability in the
# cost of hospitalizations. The adjusted value of R² is 0.02192.
# The F-statistic is 6.581 with a p-value of 0.001511. This suggests that the 
# model as a whole is statistically significant.

# In both cases the p-value is less than 0.05. We reject the null hypothesis.

# We will use an ANOVA Test.
# Dependent variable in the ANOVA Test: Cost
# Independent variables in the ANOVA Test: AGE, FEMALE

# H0: There is no effect of AGE and FEMALE on Cost.
# H1: There is an effect of AGE and FEMALE on Cost.

answer_5b <- aov(Cost ~ Age + Gender, data = dt)
summary(answer_5b) 

# In both cases the p-value is less than 0.05. We reject the null hypothesis.
# There is a significant effect of age and gender on hospital costs.

# Answer: The model suggests that both patient age and patient gender have 
# significant impacts on hospitalization costs, although the impact of age is 
# stronger.



# Question 6 - Since length of stay is the crucial factor for hospitalized 
# patients, we wanted to find out whether length of stay can be predicted from 
# age, gender, and race.

# Linear Regression
# Dependent variable: HospitalizationTime
# Independent variables: Age, Gender and Race

# H0: There is no linear relationship between dependent and independent 
# variables.
# H1: There is a linear relationship between dependent and independent 
# variables.

answer_6 <- lm(HospitalizationTime ~ Age + Gender + Race , data = dt)
summary(answer_6)

# The coefficients for the independent variables are as follows:
# - Intercept (Intercept): 2.94377
# - Patient Age (PatientAge): -0.03960
# - Sex (Sex): 0.37011
# - Breed (Race): -0.09408
# The p-values associated with the coefficients indicate statistical 
# significance:
# - The coefficient for Patient Age has a p-value of 0.0766, indicating
# that the patient's age may not be statistically significant in
# explanation of length of stay (value close to the limit of 0.05).
# - The coefficient for Sex is also not statistically significant
# (p-value = 0.2334).
# - The coefficient for Race is also not statistically significant
# (p-value = 0.7484).
# - The value of R² (R-squared) is low, indicating that the independent
# variables in the model explain a very small proportion of the variability in 
# timing hospitalization.
# The F-statistic is 1.314 with a p-value of 0.2692. This suggests that the 
# model as a whole may not be statistically significant in explaining the time
# of hospitalization.

# The p-value is greater than 0.05. We failed to reject H0.

# Answer: Length of hospital stay cannot be predicted from the independent 
# variables used.



# Question 7 - Which variables have the greatest impact on hospital admission 
# costs?

# Linear Regression
# Dependent variable: Cost
# Independent variables: Age, Gender, HospitalizationTime, Race, DiagnosticGroup

# H0: There is no linear relationship between dependent and independent 
# variables.
# H1: There is a linear relationship between dependent and independent 
# variables.

answer_7a <- lm(Cost ~ ., data = dt)
summary(answer_7a) 

# As we can see from the values of the coefficients, the variables age,
# length of stay (HospitalizationTime) and patient refined diagnosis group 
# (DiagnosticGroup) have three asterisks (***) next to them. So they are the 
# only ones with statistical significance
# Furthermore, RACE is not significant.

answer_7b <- lm(Cost ~ Age + Gender + HospitalizationTime + DiagnosticGroup, 
                data = dt)
summary(answer_7b) 

# As we can see the variable that represents gender has the lowest significance 
# for the model

answer_7c <- lm(Cost ~ Age + HospitalizationTime + DiagnosticGroup, 
                data = dt)
summary(answer_7c)

# The 3 variables have high significance, but DiagnosticGroup has a negative 
# t-value.

answer_7d <- lm(Cost ~ Age + HospitalizationTime, data = dt)
summary(answer_7d) 

# Removing race and gender does not change the R2 value.
# Removing DiagnosticGroup from the model increases the standard error.
# Therefore, the model answer_7c seems to be the best and we will use it for our
# conclusion.

# Answer: As is evident from the various models above, healthcare costs depend 
# on age, length of stay and diagnostic group.
# These are the 3 most relevant variables to explain and predict spending on 
# hospital admissions.