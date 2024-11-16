# Statistics in Practice 05

# In this case study, we will use the ToothGrowth dataset available in the 
# datasets package in R, which is loaded when you initialize RStudio.

# This dataset contains a set of records with information about the tooth length 
# of guinea pigs. Researchers administered different doses of two types of 
# vitamin supplements to 60 guinea pigs and evaluated the results on the animals' 
# tooth growth. Based on this data, we will address the following question:

# Is there a significant difference in tooth growth based on the type of 
# supplement used on the guinea pigs?

# A statistical test will help us find the answer!

# Exploratory Analysis and Understanding the Business Problem  
# Is there a significant difference in tooth growth based on the type of 
# supplement used on the guinea pigs?

# Packages - Library
# install.packages('GGally')
# install.packages('ggplot2')
library(ggplot2)
library(GGally)

# Understanding the Dataset
?ToothGrowth

# Loading the Dataset
data("ToothGrowth")

str(ToothGrowth)

View(ToothGrowth)

summary(ToothGrowth)

# Histogram of the len variable
hist(ToothGrowth$len)

# GGPairs
ggpairs(ToothGrowth)

# Boxplots por tipo de suplemento
qplot(supp,
      len,
      data = ToothGrowth,
      main = "Crescimento dos Dentes dos Porcos da Guiné Por Tipo de Suplemento",
      xlab = "Tipo de Suplemento",
      ylab = "Comprimento do Dente") +
  geom_boxplot(aes(fill = supp))

# Boxplots by Supplement Type
qplot(supp,
      len,
      data = ToothGrowth,
      main = "Tooth Growth in Guinea Pigs by Supplement Type",
      xlab = "Supplement Type",
      ylab = "Tooth Length") +
  geom_boxplot(aes(fill = supp))

# Analyzing the boxplots, it seems there is a difference in tooth growth 
# associated with the type of supplement. We need to validate this.

# Is there a significant difference in tooth growth based on the type of 
# supplement used on the guinea pigs?
  
# Solution 1: Apply an independent t-test to check if the types of supplements 
# have an impact on the animals' tooth growth. Hypotheses for the t-test:

# H0 (Null Hypothesis): There is no significant difference between the means of 
# the two groups (i.e., the type of supplement has no impact on tooth growth).
# H1 (Alternative Hypothesis): There is a significant difference between the 
# means of the two groups (i.e., the type of supplement has an impact on tooth growth).
# To apply the t-test, we first need to validate the five assumptions of the test:

# The data is random and representative of the population.
# The dependent variable is continuous.
# Both groups are independent (i.e., exhaustive and mutually exclusive groups).
# The residuals of the model are normally distributed.
# The residual variance is homogeneous (the principle of homoscedasticity).
# For our example in this case study, we will assume that assumptions 1 to 3 are 
# true and validate assumptions 4 and 5. To validate assumption 4, we will use 
# the Shapiro-Wilk test, and for assumption 5, we will use the F-test.

# Validating assumption 5 first using the F-test (reversed for didactic purposes):

# H0: The means of data drawn from a normally distributed population have the 
# same variance.
# H1: The means of data drawn from a normally distributed population do not have 
# the same variance.

var.test(len ~ supp, data = ToothGrowth)

# The p-value is greater than 0.05. We fail to reject the H0.

# There is no significant difference between the variances of the two groups.

# Now, let's validate assumption 4.

# Shapiro-Wilk Normality Test
# H0: The data is normally distributed.
# H1: The data is not normally distributed.

shapiro.test(ToothGrowth$len[ToothGrowth$supp == 'OJ'])

shapiro.test(ToothGrowth$len[ToothGrowth$supp == 'VC'])

t.test(df_OJ, df_VC, paired = FALSE, var.equal = FALSE, conf.level = 0.95)

# The p-value of the test is 0.06063, which is greater than 0.05. We failed to 
# reject H0.

# We can conclude that there is no significant difference between the two groups.

# In other words, there is no significant difference between the types of 
# supplements applied for the growth of guinea pigs. Compare this with the 
# boxplot result.

# Solution 1 is not valid for this type of problem because the data does not 
# satisfy one of the assumptions.

# Could it be that the supplement dosage actually makes the difference, and not 
# the type of supplement?
  
# Let's check!

# Solution 2: Apply an ANOVA test to verify if the dosages of the supplement 
# types are causing an impact on the growth of the animals' teeth.

# To use the ANOVA test, we have the following main assumptions:
# 1. Within each sample, the observations are randomly and independently sampled
# from each other.
# 2. Each sample group is drawn from a normally distributed population.

# We will consider assumption 1 as true and will test assumption 2.

# Now, we divide the data into 3 groups based on the supplement dosage.

unique(ToothGrowth$dose)

dose_0_5 = ToothGrowth$len[ToothGrowth$dose == 0.5]
dose_1_0 = ToothGrowth$len[ToothGrowth$dose == 1]
dose_2_0 = ToothGrowth$len[ToothGrowth$dose == 2]

# Shapiro-Wilk Normality Test for each group
# H0: The data is normally distributed.
# H1: The data is not normally distributed.

shapiro.test(dose_0_5)

shapiro.test(dose_1_0)

# The p-value for all groups is greater than 0.05. We failed to reject H0.
# The 3 samples are normally distributed.

# ANOVA
teste_anova = aov(len ~ dose, ToothGrowth)
summary(teste_anova)

# Based on this unidirectional analysis, the dosage has a very significant effect
# on the length of the tooth.

# Conclusion:
# The type of supplement does not seem to make a difference.
# What impacts the growth of the guinea pigs' teeth is the dosage of the supplement.
# Or, in other words: the type of supplement might make a difference depending 
# on the dosage!

# We can validate this with a linear regression model.

modelo_lr = lm(len ~ supp + dose, ToothGrowth)
summary(modelo_lr)