# Lab 3 - Machine Learning in Digital Marketing - Predicting Lead Conversion Probability

# As a Data Scientist, you successfully completed the previous project (Lab 2). 
# But another challenge is ahead.

# Your boss invites you to a meeting and informs you that the Digital Marketing 
# department was very pleased with your project on predicting the number of 
# converted users. However, now they want a model that provides a slightly 
# different prediction.

# Considering the details of a new marketing campaign, they want to know whether 
# a lead will or will not be converted (i.e., whether a potential customer will 
# or will not become a customer and purchase the product). Additionally, they 
# want to know the probability. They need to answer this business question: If 
# the model predicts that a lead will be converted, what is the probability of 
# that happening?

# Your boss explains that this is an important project with high visibility for 
# the Analytics department and that he is a bit concerned if you have the 
# necessary skills to carry out the project and deliver the results.

# You tell your boss that you completed a Data Science Academy (DSA) training 
# and feel capable and comfortable with the new challenge. Your boss knows the 
# quality and high level of DSA's courses (he himself has taken some courses) 
# and says that he feels more at ease and that you can start the project, which 
# should be delivered within 3 days in a presentation that will include the 
# entire Marketing department and some board members.

# You begin the project, gather the sample data, and apply your knowledge to: 
# interpret the data, perform exploratory analysis, preprocess the data (which 
# includes categorical variables), build several versions of the model, evaluate 
# the model using different metrics, and finally, create a deployment procedure 
# for the model. Having studied at DSA through various projects, labs, and 
# step-by-step case studies with a focus on solving business problems, you have 
# no trouble completing the Digital Marketing department's project.

# However, while analyzing the data, you come across an issue that makes you 
# uncomfortable. One of the variables in the dataset presents information that 
# would make the machine learning model biased toward a characteristic that could 
# cause discrimination. The dilemma is clear, and you need to make a decision 
# and justify it.

# As you learned at DSA, you don’t want to make a decision based on assumptions 
# but rather on data. You decide to consult the General Data Protection Law (LGPD) 
# and the LGPD Best Practices Guide to identify the legal implications of the 
# dilemma and then make your decision on what to do based on the data and the 
# law. You then make a decision and present your solution for the project during 
# the meeting. Everyone seems satisfied with the proposed solution.

# After the presentation, you return to your desk, already thinking about how to 
# improve the project and how the model will be retrained periodically. Then the 
# phone rings! It’s your boss calling you for another meeting, this time with the 
# company’s executive board.

# In the meeting, you are informed that your project had an impact, was technically 
# very good, but they were really impressed with your approach in identifying the 
# dilemma clearly present in the data, researching the legal implications, and 
# thus avoiding potential future problems for the company. Based on this, they 
# inform you that you will be promoted and receive a 42% salary increase starting 
# next month.

# You return to your desk, happy and satisfied, not only for having completed 
# the project but also for having made the right decision.

# Business Question for Lab 2: What is the likely number of converted users? 
# (Regression)
# Business Question for Lab 3: Will a Lead be converted? Yes or No? What is the 
# probability? (Classification)

# Getting the current working directory
# setwd("")
getwd()

# Packages - Library
# install.packages("caret")
# install.packages("dplyr")
# install.packages("ggplot2")
library(caret)
library(dplyr)
library(ggplot2)

# Loading the data
dados_dsa <- read.csv("lab03.csv")

# Data types
str(dados_dsa)

# Viewing the data
View(dados_dsa)

# What to do in this case?

# https://www.planalto.gov.br/ccivil_03/_ato2015-2018/2018/lei/l13709.htm

# https://www.gov.br/governodigital/pt-br/seguranca-e-protecao-de-dados/guias/guia_lgpd.pdf

# Remove the variable
dados_dsa$cor_da_pele <- NULL

# Viewing the data
View(dados_dsa)

names(dados_dsa)

##### Exploratory Data Analysis #####

# Bar plot
ggplot(dados_dsa, aes(x=converteu)) + 
  geom_bar(aes(fill=converteu), alpha=0.7) +
  ggtitle("Distribution of the 'Converteu' Variable") +
  xlab("Converteu") +
  ylab("Count")

# Boxplot
ggplot(dados_dsa, aes(x=converteu, y=numero_cliques, fill=converteu)) + 
  geom_boxplot() +
  ggtitle("Boxplot - Number of Clicks by Conversion") + 
  xlab("Converteu") + 
  ylab("Number of Clicks")

# Bar plot
ggplot(dados_dsa, aes(x=faixa_etaria)) + 
  geom_bar(fill="orangered3", alpha=0.7) +
  ggtitle("Age Group Distribution") + 
  xlab("Age Group") + 
  ylab("Count")

# Scatter plot
ggplot(dados_dsa, aes(x=numero_acessos, y=numero_cliques)) + 
  geom_point(aes(color=converteu), alpha=0.6) + 
  geom_smooth(method='lm') + 
  ggtitle("Relationship Between Number of Accesses and Number of Clicks") +
  xlab("Number of Accesses") + 
  ylab("Number of Clicks")

# Summarizing data to get the average number of accesses per city
dados_sumarizados <- aggregate(numero_acessos ~ cidade, data = dados_dsa, FUN = mean)

# Bar plot
ggplot(dados_sumarizados, aes(x=reorder(cidade, -numero_acessos), y=numero_acessos)) +
  geom_bar(stat="identity", aes(fill=cidade), alpha=0.7) +
  ggtitle("Bar Plot - Average Number of Accesses by City") +
  xlab("City") +
  ylab("Average Number of Accesses") +
  theme(axis.text.x = element_text(angle=45, hjust=1))

##### Preprocessing and Encoding Categorical Variables #####

# Viewing the data
View(dados_dsa)

# Splitting the data into training and testing sets
set.seed(42)  # for reproducibility
?createDataPartition
indices <- createDataPartition(dados_dsa$converteu, p = 0.75, list = FALSE)
treino <- dados_dsa[indices,]
teste <- dados_dsa[-indices,]

# Viewing the data and data types
View(treino)
str(treino)
str(teste)

# Applying label encoding to the target variable
?as.factor
treino$converteu <- as.factor(treino$converteu)
teste$converteu <- as.factor(teste$converteu)

# Viewing the data and data types
View(treino)
View(teste)
str(treino)
str(teste)

# Applying label encoding to categorical variables
treino$faixa_etaria <- as.factor(treino$faixa_etaria)
treino$cidade <- as.factor(treino$cidade)
treino$navegador_web <- as.factor(treino$navegador_web)

teste$faixa_etaria <- as.factor(teste$faixa_etaria)
teste$cidade <- as.factor(teste$cidade)
teste$navegador_web <- as.factor(teste$navegador_web)

# Viewing the data and data types
View(treino)
str(treino)
str(teste)

##### Predictive Modeling #####

# Training the logistic regression model
# Version 1 of the model
?glm
modelo_v1 <- glm(converteu ~ ., family = binomial(link = 'logit'), data = treino)

# Model summary
summary(modelo_v1)

# Making predictions on the test set
previsoes_prob <- predict(modelo_v1, newdata = teste, type = 'response')
print(previsoes_prob)
previsoes_classe <- ifelse(previsoes_prob > 0.5, 'sim', 'não') # cutoff
print(previsoes_classe)

# Confusion matrix
matriz_confusao <- confusionMatrix(as.factor(previsoes_classe), teste$converteu)
print(matriz_confusao)

# In the Generalized Linear Model (GLM) for logistic regression in R, the reference category (or "positive" class)
# is defined based on the factor levels of the response variable. The first category (or level) of the factor variable is used 
# as the reference by default. However, it is possible to reorder the factor levels to define which class should be treated 
# as the "positive" or reference class.
# treino$converteu <- relevel(treino$converteu, ref = "sim")
# teste$converteu <- relevel(teste$converteu, ref = "sim")
# This "positive" class is just a reference and does not mean good or bad.

# Evaluation metrics
acuracia <- sum(diag(matriz_confusao$table)) / sum(matriz_confusao$table)
cat("Accuracy:", acuracia, "\n")

# Do we keep the age group in the dataset?
summary(modelo_v1)

# Version 2 of the model
modelo_v2 <- glm(converteu ~ numero_acessos + numero_cliques, family = binomial(link='logit'), data = treino)

# Model summary
summary(modelo_v2)

# Making predictions on the test set
previsoes_prob <- predict(modelo_v2, newdata = teste, type = 'response')
previsoes_classe <- ifelse(previsoes_prob > 0.5, 'sim', 'não')

# Confusion matrix
matriz_confusao <- confusionMatrix(as.factor(previsoes_classe), teste$converteu)
print(matriz_confusao)

# Evaluation metrics
acuracia <- sum(diag(matriz_confusao$table)) / sum(matriz_confusao$table)
cat("Accuracy:", acuracia, "\n")

##### Deploy #####

# Save the trained model to disk
save(modelo_v2, file = "modelo_v2.RData")

# Load the model from disk
load("modelo_v2.RData")

# New data
novos_dados <- data.frame(numero_acessos = c(60), numero_cliques = c(20))
print(novos_dados)

# Predictions with new data
previsoes_novos_dados_prob <- predict(modelo_v2, newdata = novos_dados, type = 'response')
previsoes_novos_dados_classe <- ifelse(previsoes_novos_dados_prob > 0.5, 'sim', 'não')

# Showing the class and probability predictions
novos_dados$Lead_Convertido <- previsoes_novos_dados_classe
novos_dados$Probabilidade <- previsoes_novos_dados_prob * 100
print(novos_dados)
