# Statistics in Practice 01

# Case Study: A pharmaceutical company tested three formulations of a pain relief 
# medication for individuals suffering from headaches (specifically migraines). 
# For the experiment, 27 volunteers were selected, with 9 randomly assigned to 
# one of the three formulations.

# Participants were instructed to take the medication during their next migraine 
# episode and report their pain level on a scale from 1 to 10 (10 being the 
# highest pain). In the following practical sessions, we will create a simulated 
# dataset representing these results.

# The goal is to compare whether all three medications produce the same effect 
# or not, meaning:

# If the MEAN of each group is the same or very similar, it indicates that all 
# three medications are effective.
# If the MEAN is different, it indicates that only one or two of the three 
# medications are effective.
# Based on the response we want to obtain, we define the hypotheses for the 
# statistical test as follows:

# H₀ (Null Hypothesis): The MEAN of the groups is the same.

# Hₐ (Alternative Hypothesis): The MEAN of all groups is not the same.

# ANOVA will be used as the statistical test in this scenario. Our goal is to 
# determine whether to reject or fail to reject H₀ (we should never say we accept 
# or do not accept H₀; we should say we fail or do not fail to reject H₀).

# The ANOVA result will be interpreted based on the p-value. The p-value is the 
# probability that the test statistic would assume an extreme value relative to 
# the observed value when H₀ is true.

# General Interpretation Rule (considering a p-value of 0.05):

# Low p-value (less than 0.05): Strong empirical evidence against H₀ (we reject 
# H₀). This indicates a probability of less than 5% that H₀ is true.

# High p-value (greater than 0.05): Little or no empirical evidence against H₀ 
# (we fail to reject H₀).

# Exact p-value equal to 0.05: The data scientist decides whether to reject H₀ 
# or re-run the test.

# Data Provided:
# Medication A: 4, 5, 4, 3, 2, 4, 3, 4, 4
# Medication B: 6, 8, 4, 5, 4, 6, 5, 8, 6
# Medication C: 6, 7, 6, 6, 7, 5, 6, 5, 5

# List of volunteer responses
volunteer_score <- c(4, 5, 4, 3, 2, 4, 3, 4, 4, 6, 8, 4, 5, 4, 6, 5, 8, 6, 6, 7, 6, 6, 7, 5, 6, 5, 5)
volunteer_score

# List with the total tests for the 3 medications
medication <- c(rep("A", 9), rep("B", 9), rep("C", 9))
medication

# Create a dataframe
df <- data.frame(volunteer_score, medication)
View(df)

test_anova <- aov(volunteer_score ~ medication, data = df)
test_anova

# Test summary
summary(test_anova)

# Conclusion:
# p-value < 0.05, i.e., 0.000256 < 0.05. Therefore, we reject H₀.
# The means of the groups are not the same, and consequently, the medications do 
# not have the same effect.
