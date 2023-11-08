# Module 19 - Mini-Project 3 - Explainability of AutoML Models with SHAP 
# (SHapley Additive exPlanations)

# This was an exercise and not a mini project

# Data Source and Data Dictionary
# For this mini-project we will create a mass of fictitious data, which 
# represents real data. Below is the data dictionary:
# - Predictor Variable 1 (productivity):
# Overall Equipment Effectiveness: This is a measure of productivity, which 
# describes the portion of the time that a machine works at peak performance. 
# The metric is a product of machine availability, performance, and quality.
# - Predictor Variable 2 (yield):
# First pass yield: This is the portion of products that leave the production 
# line and that do not present defects and meet specifications without the need 
# for any rectification work.
# - Predictor Variable 3 (cost):
# Energy cost per unit: This is the cost of electricity, steam, oil or gas 
# required to produce a given unit of product in the factory.
# - Predictor Variable 4 (priority):
# Equipment priority when entering the maintenance period (Low, Medium, High).
# - Predictor Variable 5 (efficiency):
# The amount of product a machine produces during a specific period.
# This metric can also be applied to the entire production line to check its efficiency.
# Target Variable (maintenance):
# - 0 means the equipment does not require maintenance (no)
# - 1 means the equipment requires maintenance (yes)

getwd()

# --*--
# Installing and Load packages
install.packages("h2o")
install.packages("tidyverse")
install.packages("ggbeeswarm")
library(h2o)
library(tidyverse)
library(ggbeeswarm)

# Data mass preparation
dt <- tibble(productivity = c(rnorm(1000), rnorm(1000, 0.25)),
             performance = runif(2000),
             cost = rf(2000, df1 = 5, df2 = 2),
             priority = c(sample(rep(c('Baixa', 'Media', 'Alta'), c(300, 300, 400))),
                          sample(c('Baixa', 'Media', 'Alta'), 1000, prob = c(0.25, 0.25, 0.5), replace = T)),
             efficiency = rnorm(2000),
             maintenance = rep(c(0,1), c(1050,950)))

# Dimensions
dim(dt)

# View the dataset
View(dt)

# Variables and data types
str(dt)

# Analyzing the dataset
# The target variable is "maintenance"
table(dt$maintenance)

# Variable "priority" is categorical
table(dt$priority)

# Converting the target variable to factor type
dt <- dt %>%
  mutate(maintenance = as.factor(maintenance)) %>%
  mutate_if(is.character, factor)

# Variables and data types
str(dt)

# View the dataset
View(dt)

# We initialize H2O (Machine Learning Framework)
# Pay attention to the Java JDK version. Install version 11 from the link below:
# https://www.oracle.com/java/technologies/downloads/
h2o.init()

# H2O requires data to be in H2O dataframe format
h2o_frame <- as.h2o(dt)
class(h2o_frame)
head(h2o_frame)

# Splitting data in training and testing
# ?h2o.splitFrame
h2o_frame_split <- h2o.splitFrame(h2o_frame, ratios = 0.77)
head(h2o_frame_split)

# AutoML Model
# ?h2o.automl
automl_model <- h2o.automl(y = 'maintenance',
                            balance_classes = TRUE,
                            training_frame = h2o_frame_split[[1]],
                            nfolds = 4,
                            leaderboard_frame = h2o_frame_split[[2]],
                            max_runtime_secs = 60 * 2,
                            include_algos = c('XGBoost', 'GBM', 'GLM'),
                            sort_metric = "AUC")

# Extract the leaderboard
leaderboard_automl <- as.data.frame(automl_model@leaderboard)
View(leaderboard_automl)

# Extracts the leader (model with best performance)
lider_automl <- automl_model@leader

# For the best model we extract the contribution of each variable to the predictions
# the extracted values are called SHAP values
# We use the test data
# ?predict_contributions.H2OModel
var_contrib <- predict_contributions.H2OModel(lider_automl, h2o_frame_split[[2]])

# Let's visualize the final result
# First we prepare a dataframe with the metrics we need
df_var_contrib <- var_contrib %>%
  as.data.frame() %>%
  select(-BiasTerm) %>%
  gather(feature, shap_value) %>%
  group_by(feature) %>%
  mutate(shap_importance = mean(abs(shap_value)), shap_force = mean(shap_value)) %>%
  ungroup()

# First we prepare a dataframe with the metrics we need
df_var_contrib <- var_contrib %>%
  as.data.frame() %>%
  select(-BiasTerm) %>%
  gather(feature, shap_value) %>%
  group_by(feature) %>%
  mutate(shap_importance = mean(abs(shap_value)), shap_force = mean(shap_value)) %>%
  ungroup()

View(df_var_contrib)

# Plot the importance of each variable to predict the target variable
df_var_contrib %>%
  select(feature, shap_importance) %>%
  distinct() %>%
  ggplot(aes(x = reorder(feature, shap_importance), y = shap_importance)) +
  geom_col(fill = 'blue') +
  coord_flip() +
  xlab(NULL) +
  ylab("Average Value of SHAP Metrics") +
  theme_minimal(base_size = 15)

# Plot of contribution of each variable to explain the target variable
ggplot(df_var_contrib, aes(x = shap_value, y = reorder(feature, shap_importance))) +
  ggbeeswarm::geom_quasirandom(groupOnX = FALSE, varwidth = TRUE, size = 0.9, alpha = 0.5, width = 0.15) +
  xlab("Variable Contribution") +
  ylab(NULL) +
  theme_minimal(base_size = 15)

# Turn off the H2O
h2o.shutdown()

#--*--