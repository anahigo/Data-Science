# Module 17 - Mini-Project 1 - Exploratory Analysis of Socioeconomic Data

# The dataset was generated from the sources below:
# https://data.world/laurel/world-happiness-report-data

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
myColumns[1] <- "CountryName"
myColumns[2] <- "Year"
myColumns[3] <- "LifeLevel"
myColumns[4] <- "GDPPerCapita"
myColumns[5] <- "SocialSupport"
myColumns[6] <- "ExpectationLife"
myColumns[7] <- "Freedom"
myColumns[8] <- "Generosity"
myColumns[9] <- "Corruption"
myColumns[10] <- "PositiveEmotions"
myColumns[11] <- "NegativeEmotions"

# Check the result
myColumns

# Assigns new column names to the dataframe
colnames(dt) <- myColumns

# View the dataset
View(dt)

# Numeric variable summaries
summary(dt)

# --**--
# Checking the percentage of incomplete lines 
# How many lines are complete cases?
complete_cases <- sum(complete.cases(dt))
complete_cases

# How many lines have incomplete cases?
not_complete_cases <- sum(!complete.cases(dt))
not_complete_cases

# What is the percentage of incomplete data?
percentage <- (not_complete_cases / complete_cases) * 100
percentage

# Checking how many countries were included in the data collection
length(unique(dt$CountryName))

# List unique countries and save the result (before removing records with NA values)
list_countries_with_na <- unique(dt$CountryName)
list_countries_with_na

# --**--

# Eliminating NA values
dt <- na.omit(dt)

# Dimensions
dim(dt)

# --**--
# List of countries after removing NA values
list_of_countries_without_na <- unique(dt$CountryName)
list_of_countries_without_na

# Checking if we lost countries when removing NA values
length(list_countries_with_na)
length(list_of_countries_without_na)

# Checking the difference before and after removing NA values
setdiff(list_countries_with_na, list_of_countries_without_na)

# Checking which years are present in the data
years <- unique(dt$Year)
range(years)
length(unique(dt$Year))

# Number of records per year
table(dt$Year)

# Removing years with the lowest contribution (smallest data volume)
data_for_years <- dt[dt$Year!=2005 & dt$Year!=2006 & dt$Year!=2007 & dt$Year!=2020,]

# Extracting numeric variables
numeric_variable_list <- sapply(dt, is.numeric)
numerical_data <- dt[numeric_variable_list]

# Correlation Matrix
cor(numerical_data)

# Correlation Plot
pairs(numerical_data)
pairs(numerical_data[1:5],labels = colnames(numerical_data)[1:5])
pairs(numerical_data[6:10],labels = colnames(numerical_data)[6:10])


# Dataset organization
# We will carry out the analysis considering the average of indicators per 
# country.
# We calculate the averages by grouping by indicator and concatenate the 
# resulting dataframes.

install.packages("dplyr")
library(dplyr)

# Nomes das colunas
colnames(dt)

# Grouping the data and calculating average by country 
# Life Level
average_country_living_level <- dt %>%
  group_by(CountryName) %>%
  summarize(LifeLevel = mean(LifeLevel))

# GDP Per Capita
average_country_gpd_per_capita <- dt %>%
  group_by(CountryName) %>%
  summarize(GDPPerCapita = mean(GDPPerCapita))

# Merge dataframes
dt_average <- merge(average_country_living_level, average_country_gpd_per_capita)

# View the dataset
View(dt_average)

# Social Support
average_country_social_support <- dt %>%
  group_by(CountryName) %>%
  summarize(SocialSupport = mean(SocialSupport))

# Merge dos dataframes
dt_average <- merge(dt_average, average_country_social_support)

# View the dataset
View(dt_average)

# ExpectationLife
average_country_expectation_life <- dt %>%
  group_by(CountryName) %>%
  summarize(ExpectationLife = mean(ExpectationLife))

# Merge dataframes
dt_average <- merge(dt_average, average_country_expectation_life)

# View the dataset
View(dt_average)

# Freedom
average_country_freedom <- dt %>%
  group_by(CountryName) %>%
  summarize(Freedom = mean(Freedom))

# Merge dos dataframes
dt_average <- merge(dt_average, average_country_freedom)

# View the dataset
View(dt_average)

# Generosity
average_country_generosity <- dt %>%
  group_by(CountryName) %>%
  summarize(Generosity = mean(Generosity))

# Merge dos dataframes
dt_average <- merge(dt_average, average_country_generosity)

# View the dataset
View(dt_average)

# Corruption
average_country_corruption <- dt %>%
  group_by(CountryName) %>%
  summarize(Corruption = mean(Corruption))

# Merge dos dataframes
dt_average <- merge(dt_average, average_country_corruption)

# View the dataset
View(dt_average)

# Positive Emotions
average_country_positive_emotions <- dt %>%
  group_by(CountryName) %>%
  summarize(PositiveEmotions = mean(PositiveEmotions))

# Merge dos dataframes
dt_average <- merge(dt_average, average_country_positive_emotions)

# View the dataset
View(dt_average)

# NegativeEmotions
average_country_negative_emotions <- dt %>%
  group_by(CountryName) %>%
  summarize(NegativeEmotions = mean(NegativeEmotions))

# Merge dos dataframes
dt_average <- merge(dt_average, average_country_negative_emotions)

# View the dataset
View(dt_average)

# Answering the questions

install.packages("ggplot2")
library(ggplot2)

# Question 1 - Does an increase in a country's GDP per capita positively affect 
# its citizens' life expectancy at birth? What is the difference between these 
# two variables?

cor.test(dt_average$GDPPerCapita, dt_average$ExpectationLife, method = "pearson")

plot(dt_average$GDPPerCapita, dt_average$ExpectationLife)

# Answer: The results indicate that there is a strong positive correlation 
# between GDP per capita and life expectancy. As GDP per capita increases, life 
# expectancy tends to increase. This is consistent with the idea that a 
# country's income level is related to the quality of life and life expectancy 
# of its population.



# Question 2 - Is there a bright spot between the escalation of life and the 
# general public's awareness of corruption in business and government? What is 
# the difference between these two variables?

cor.test(dt_average$LifeLevel, dt_average$Corruption, method = "pearson")

plot(dt_average$LifeLevel, dt_average$Corruption)

# Answer: The results indicate that there is a moderately negative bias between 
# "LifeLevel" and "Corruption". As the level of corruption increases, the level 
# of quality of life tends to decrease. This suggests that corruption can have a 
# negative impact on quality of life in a given context or data set.



# Question 3 - Does the increase in the scale of living have any effect on the 
# average happiness among the general public? What is the difference 
# between these two variables?

cor.test(dt_average$LifeLevel, dt_average$PositiveEmotions, method = "pearson")
plot(dt_average$LifeLevel, dt_average$PositiveEmotions)

# Answer: The results indicate that there is a moderate positive correlation 
# between "Generosity" (generosity) and "PositiveEmotions" (positive emotions).
# This suggests that in contexts where generosity is greater, there also tend to 
# be more positive emotions. However, the strength of the correlation is 
# moderate, meaning that other factors can influence positive emotions.



# Question 4 - Does the country with the lowest social support index have a 
# greater perception of corruption in relation to companies and the government 
# in the country?

# --**--
# Indicators
dt_average[dt_average$SocialSupport == min(dt_average$SocialSupport),]

dt1 <- dt_average[dt_average$CountryName == "Central African Republic",]
View(dt1)

dt1$SocialSupport
dt1$Corruption
max(dt_average$SocialSupport)
max(dt_average$Corruption)

dt2 <- dt[dt$CountryName == "Central African Republic",]
View(dt2)

cor.test(dt2$SocialSupport, dt2$PositiveEmotions, method = "pearson")
plot(dt2$SocialSupport, dt2$PositiveEmotions)

# Answer: The results indicate a moderate negative correlation between 
# "SocialSupport" and "PositiveEmotions", but the correlation is not
# statistically significant based on the p-value. This means that, given the 
# data and sample size, there is no statistically significant evidence of a 
# correlation between these variables. The wide confidence interval suggests 
# uncertainty in estimating the correlation due to the small sample size.



# Question 5 - Are generous people happier?

cor.test(dt_average$Generosity, dt_average$PositiveEmotions, method = "pearson")
plot(dt_average$Generosity, dt_average$PositiveEmotions)

# Answer: These results indicate that there is a moderate positive correlation 
# between generosity (Generosity) and positive emotions (PositiveEmotions). In 
# other words, in situations where generosity is greater, there also tend to be 
# more positive emotions. The strength of the moderate correlation suggests that 
# a relationship exists, but other factors may also influence positive emotions.