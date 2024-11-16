# Statistics in Practice 04

# Consider a dataset with real estate records from various cities across Brazil. 

# Each record contains information such as the property price, size, type of 
# property, property status, whether it is rented or not, among other details.

# Our task is to check whether two of the variables (both categorical) are 
# related and impact the overall information available in the dataset. We will 
# apply a statistical test suitable for categorical variables, the Chi-Square Test.

# For this task, we will be using a fictional dataset.

# Analysis and Interpretation

# Assumption of the test:
# The variables must be independent!
# Loading the dataset
df = read.csv("dados.csv")

# Viewing the data
View(df)

# Dimensions
dim(df)

# Separating x and y
x = df$Tipo_Imovel
unique(x)

# Cross-table
table(x, y)

prop.table(table(x, y))

# Defining the hypotheses:
# H0 = There is no relationship between x and y
# H1 = x and y are related

# If the p-value is less than 0.05, we reject H0

# Chi-Square Test
# ?chisq.test
chisq.test(table(x, y))

# since the p-value is less than 0.05, we reject H0

# Exercise:
# If we do not consider properties of the type "Apartment," is there a difference 
# in the test result?
dados_sem_apartamento <- subset(df, !grepl("apartamento", Tipo_Imovel, ignore.case = TRUE))

View(dados_sem_apartamento)

x = dados_sem_apartamento$Tipo_Imovel
unique(x)

y = dados_sem_apartamento$Status_Imovel
unique(y)

chisq.test(table(x, y))

# since the p-value is greater than 0.05, we do not reject H0
# That is, there is no significant difference in the test result
# X-squared = 868.75, df = 4, p-value < 2.2e-16
