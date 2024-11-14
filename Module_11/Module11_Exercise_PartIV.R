getwd()

# Defining the Problem: OCR - Optical Character Recognition
# Your model must predict the character from the given dataset. Use an SVM template

## Exploring and preparing the data
letters <- read.csv("module11_part4.csv")

str(letters)

# Creating training data and test data
letters_training <- letters[1:16000, ]
letters_test  <- letters[16001:20000, ]

## Training the Model
# install.packages("kernlab")
library(kernlab)

# Creating the model with the vanilladot kernel
# Note: Convert the first variable of the dataset to the factor type:
letters_training$letter <- as.factor(letters_training$letter)
str(letters_training)

letter_classifier <- ksvm(letter ~ ., data = letters_training, kernel = "vanilladot")

# Visualizing the result of the model
letter_classifier

# Evaluating the performance of the model
letter_predictions <- predict(letter_classifier, letters_test)

head(letter_predictions)

table(letter_predictions, letters_test$letter)

# Creating a TRUE/FALSE vector indicating correct/incorrect predictions
agreement <- letter_predictions == letters_test$letter
table(agreement)
prop.table(table(agreement))

## Optimizing the Model
set.seed(12345)

# Recreating the model with another kernel type
letter_classifier_rbf <- ksvm(letter ~ ., data = letters_training, kernel = "rbfdot")

# New forecasts
letter_predictions_rbf <- predict(letter_classifier_rbf, letters_test)

# Compare the results with the first version of the model
agreement_rbf <- letter_predictions_rbf == letters_test$letter
table(agreement_rbf)
prop.table(table(agreement_rbf))
