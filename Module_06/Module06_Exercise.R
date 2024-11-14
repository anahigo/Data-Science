# Exercise 1 - Install and load the necessary packages to work with SQLite and R
install.packages("dbplyr")
install.packages("RSQLite")

library(RSQLite)
library(dbplyr)
library(dplyr)

# Exercise 2 - Create the connection for the module6.sqlite file attached to this script
drv = dbDriver("SQLite")
con = dbConnect(drv, dbname = "module6.sqlite")

# Exercise 3 - What is the version of SQLite used in the database?
# Hint: Consult the help for the src_dbi() function
?src_dbi

src_dbi(con)

# Exercise 4 - Execute the query below in the database and record it in an object in R:
# SELECT year, species_id, plot_id FROM surveys
query = "SELECT year, species_id, plot_id FROM surveys"
answer4a = dbSendQuery(con, query)

# another suggestion
?tbl

answer4b <- tbl(con, sql("SELECT year, species_id, plot_id FROM surveys"))

# Exercise 5 - What is the type of object created in the previous item?
class(answer4a)
class(answer4b)

# Exercise 6 - We have already loaded the table below for you. Select the species_id, sex and weight columns with the following condition:
# Condition: weight < 5
pesquisas <- tbl(con, "surveys")

pesquisas %>%
  select(species_id, sex, weight) %>%
  filter(weight < 5)

# Exercise 7 - Record the result of the previous item in an R object. The final object must be a dataframe
answer7a <- pesquisas %>%
  select(species_id, sex, weight) %>%
  filter(weight < 5)

answer7b <- as.data.frame(answer7a)

class(answer7b)

View(answer7b)

# Exercise 8 - List the tables in the loaded database
dbListTables(con)

# Exercise 9 - From the dataframe created in item 7, create a table in the database
?dbWriteTable

dbWriteTable(con, "answer7b", answer7b)

dbListTables(con)

# Exercise 10 - Print the data from the table created in the previous item
dbReadTable(con, "answer7b")



