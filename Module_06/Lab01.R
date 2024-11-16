# Lab 1 - MapReduce for Data Analysis with R and MongoDB

# What if you had to perform an analysis on a large volume of semi-structured 
# data, as quickly and effortlessly as possible? That’s the goal of this Lab. 
# A large set of real, publicly available data will be loaded into MongoDB, and 
# the analysis process will be carried out using mapping and reduction techniques 
# in R language.
# Below you’ll find the data source link:
# https://www.mongodb.com/docs/atlas/sample-data/sample-airbnb/#std-label-sample-airbnb

# Getting the current working directory
getwd()

# Install Packages
# install.packages("ggplot2")
# install.packages("devtools")
# install.packages("mongolite")

# Packages
library(ggplot2)
library("devtools")
library(mongolite)

# Creates the connection to MongoDB
con <- mongo(collection = "airbnb",
             db = "dsadb",
             url = "mongodb://localhost",
             verbose = FALSE,
             options = ssl_options())

# View the connection
print(con)

# View the data
dados <- con$find()
View(dados)

# Check the number of records in the dataset
con$count('{}')

# Fetch a sample of data only with House properties and their cancellation policies
amostra1 <- con$find(
  query = '{"property_type" : "House" }', 
  fields = '{"property_type" : true, "cancellation_policy" : true}'
)
View(amostra1)

# We don't want the id field
amostra2 <- con$find(
  query = '{"property_type" : "House" }', 
  fields = '{"property_type" : true, "cancellation_policy" : true, "_id": false}'
)
View(amostra2)

# Let's sort the result
amostra3 <- con$find(
  query = '{"property_type" : "House" }', 
  fields = '{"property_type" : true, "cancellation_policy" : true, "_id": false}',
  sort = '{"cancellation_policy": -1}'
)
View(amostra3)

# Let's aggregate the data and return the average reviews per property type
amostra4 <- con$aggregate(
  '[{"$group":{"_id":"$property_type", "count": {"$sum":1}, "average":{"$avg":"$number_of_reviews"}}}]',
  options = '{"allowDiskUse":true}'
)
names(amostra4) <- c("tipo_propriedade", "contagem", "media_reviews")
View(amostra4)

# Let's plot the result in a graph
ggplot(aes(tipo_propriedade, contagem), data = amostra4) + geom_col()

# MapReduce - Mapping and Reducing

#  Count the number of reviews considering all properties
resultado <- con$mapreduce(
  map = "function(){emit(Math.floor(this.number_of_reviews), 1)}", 
  reduce = "function(id, counts){return Array.sum(counts)}"
)
names(resultado) <- c("numero_reviews", "contagem")
View(resultado)

# Plot
ggplot(aes(numero_reviews, contagem), data = resultado) + geom_col()

# Count the number of reviews by range considering all properties
resultado <- con$mapreduce(
  map = "function(){emit(Math.floor(this.number_of_reviews/100)*100, 1)}", 
  reduce = "function(id, counts){return Array.sum(counts)}"
)
names(resultado) <- c("numero_reviews", "contagem")
View(resultado)

# Plot
ggplot(aes(numero_reviews, contagem), data = resultado) + geom_col()

# Count the number of bedrooms considering all properties
resultado <- con$mapreduce(
  map = "function(){emit(Math.floor(this.bedrooms), 1)}", 
  reduce = "function(id, counts){return Array.sum(counts)}"
)
names(resultado) <- c("numero_quartos", "contagem")
View(resultado)

# Plot
ggplot(aes(numero_quartos, contagem), data = resultado) + geom_col()














