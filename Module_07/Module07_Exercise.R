getwd()

# Exercise 1 - Read the url below and record it in the page object
# http://forecast.weather.gov/MapClick.php?lat=42.31674913306716&lon=-71.42487878862437&site=all&smap=1#.VRsEpZPF84I

library(rvest)
library(stringr)
library(tidyr)

page <- read_html("http://forecast.weather.gov/MapClick.php?lat=42.31674913306716&lon=-71.42487878862437&site=all&smap=1#.VRsEpZPF84I")
page

# Exercise 2 - From the page collected in the previous item, extract the text that contains the tags:
# "#detailed-forecast-body and .forecast-text"

results_pageA <- page %>% 
                 html_nodes("#detailed-forecast-body") %>% 
                 html_nodes(".forecast-text")

results_pageA

# other way
results_pageB <- html_nodes(page, "#detailed-forecast-body b , .forecast-text")

# Exercise 3 - Turn the previous item into text

?html_text

text <- html_text(results_pageA)
text

final_text <- paste(text, collapse = " ")
final_text

# Exercise 4 - We extract the web page below for you. Now collect the "table" tag
url <- 'http://espn.go.com/nfl/superbowl/history/winners'
webpage <- read_html(url)

resultsC <- webpage %>% 
           html_nodes("table")

resultsC

class(resultsC)

# other way
resultsD <- html_nodes(webpage, 'table')

# Exercise 5 - Convert the previous item to a dataframe
df <- html_table(resultsC)[[1]]
class(df)
View(df)

# Exercise 6 - Remove the first two rows and add column names

# Remove the first two lines
df <- df[-c(1, 2), ]
View(df)

# other way
df2 <- df[-(1:2), ]
View(df)

# Add column names
colnames(df) <- c("NUMBER", "DATE", "SITE", "RESULT")
View(df)

# Exercise 7 - Convert from Roman numerals to integers

df$NUMBER <- 1:57
View(df)

# Exercise 8 - Split result column into 2 columns with winners and losers

df_split <- separate(df, col = RESULT, into = c('winner', 'loser'), sep = ",")
View(df_split)

# Exercise 9 - Add 2 more columns with the score of winners and losers
# Hint: You should do one more division on the columns
pattern <- " \\d+$"
df_split$winnerScore <- as.numeric(str_extract(df_split$winner, pattern))
df_split$loserScore <- as.numeric(str_extract(df_split$loser, pattern))

df_split$winner <- gsub(pattern, "", df_split$winner)
df_split$loser <- gsub(pattern, "", df_split$loser)

View(df_split)

# Exercise 10 - Write the result to a csv file
write.csv(df_split, file = "Module7_NFL_History_Super_Bowl_Winners.csv", row.names = FALSE)
dir()

