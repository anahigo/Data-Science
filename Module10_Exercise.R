getwd()

# Packages
install.packages("dplyr")
install.packages('nycflights13')

library('ggplot2')
library('dplyr')
library('nycflights13')

View(flights)
?flights

# Defining the Business Problem
# Create a hypothesis test to verify that Delta Airlines (DL) flights
# delay more than UA (United Airlines) flights


# Exercise 1 - Build the pop_data dataset with flight data from
# airlines UA (United Airlines) and DL (Delta Airlines).
# The dataset must contain only two columns, company name and arrival flight delay.
# Data must be extracted from the flights dataset to build the pop_data dataset
# Let's consider this dataset as our population of flights

pop_data <- na.omit(flights) %>%
  filter(carrier %in% c("UA", "DL")) %>%
  filter(arr_delay >= 0) %>%
  select(carrier, arr_delay)

# View(pop_data)

# Other way
pop_data = na.omit(flights) %>% 
  filter(carrier == 'UA' | carrier == 'DL', arr_delay >= 0) %>%
  select(carrier, arr_delay) %>%
  group_by(carrier) %>%
  sample_n(17000) %>%
  ungroup()

# View(pop_data)

# Exercise 2 - Create two samples of 1000 observations each from the
# dataset pop_data with only DL company data for sample 1 and data only
# of the UA company in sample 2

# Tip: include a column called sample_id filled with number 1 for the first
# sample and 2 for the second sample

# Sample DL (Delta Airlines)
sample_DL <- pop_data %>%
  filter(carrier == "DL") %>%
  sample_n(1000, replace = FALSE) %>%
  mutate(sample_id = 1)

# View(sample_DL)

# Sample UA (United Airlines)
sample_UA <- pop_data %>%
  filter(carrier == "UA") %>%
  sample_n(1000, replace = FALSE) %>%
  mutate(sample_id = 2)

# View(sample_UA)

# Other way
amostra1 = na.omit(pop_data) %>% 
  select(carrier, arr_delay) %>%
  filter(carrier == 'DL') %>%
  mutate(sample_id = '1') %>%
  sample_n(1000)

# View(amostra1)

amostra2 = na.omit(pop_data) %>% 
  select(carrier, arr_delay) %>%
  filter(carrier == 'UA') %>%
  mutate(sample_id = '2') %>%
  sample_n(1000)

# View(amostra2)


# Exercise 3 - Create a dataset containing the data of the 2 samples created in the previous item.

samples <- bind_rows(sample_DL, sample_UA)

#View(samples)

# Exercise 4 - Calculate the confidence interval (95%) of the sample_DL

# Standard_error
standard_error_sample_DL <- sd(sample_DL$arr_delay) / sqrt(nrow(sample_DL))

# Lower and upper limits
# 1.96 is the z score value for 95% confidence

lower_DL = mean(sample_DL$arr_delay) - 1.96 * standard_error_sample_DL  
upper_DL = mean(sample_DL$arr_delay) + 1.96 * standard_error_sample_DL


# Confidence interval
cat("95% Confidence Interval for sample_DL: [", lower_DL, ",", upper_DL, "]\n")
ic_DL = c(lower_DL,upper_DL)

# Exercise 5 - Calculate the confidence interval (95%) of the sample_UA

# Standard_error
standard_error_sample_UA <- sd(sample_UA$arr_delay) / sqrt(nrow(sample_UA))

# Lower and upper limits
# 1.96 is the z score value for 95% confidence

lower_UA = mean(sample_UA$arr_delay) - 1.96 * standard_error_sample_UA  
upper_UA = mean(sample_UA$arr_delay) + 1.96 * standard_error_sample_UA


# Confidence interval
cat("95% Confidence Interval for sample_UA: [", lower_UA, ",", upper_UA, "]\n")
ic_UA = c(lower_UA,upper_UA)

# Exercise 6 - Create a plot Visualizing the confidence intervals created in the previous items
# Tip: Use geom_point() and geom_errorbar() from ggplot2 package
toPlot = summarise(group_by(samples, sample_id), mean = mean(arr_delay))
toPlot = mutate(toPlot, lower = ifelse(toPlot$sample_id == 1,ic_DL[1],ic_UA[1]))
toPlot = mutate(toPlot, upper = ifelse(toPlot$sample_id == 1,ic_DL[2],ic_UA[2]))
ggplot(toPlot, aes(x = sample_id, y=mean, colour = sample_id )) + 
  geom_point() +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.1)

# Exercise 7 - Can we say that most likely, the samples came from the same population?
# Why?

# Yes. Most of the data resides in the same confidence interval in the two samples.

# Exercise 8 - Create a hypothesis test to verify that Delta Airlines (DL) flights
# delay more than UA (United Airlines) flights

# H0 and H1 must be mutually exclusive.

# H0 = There is no significant difference between DL and UA delays (average delay diff = 0).
# H1 = Delta delays more (average diff > 0).

# Create the samples
dl <- sample_n(filter(pop_data, carrier == "DL", arr_delay > 0), 1000)
ua <- sample_n(filter(pop_data, carrier == "UA", arr_delay > 0), 1000)

t.test(dl$arr_delay, ua$arr_delay, alternative="greater")

# We are working with alpha equal to 0.05 (95% confidence) p-value equal 0.08134

# We fail to reject the null hypothesis because p-value is greater than the significance level
# This means that there is a high probability that there is no significant difference between the delays.
# For our data, there is no statistical evidence that DL delays more than UA.





