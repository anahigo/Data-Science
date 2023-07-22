getwd()


# Exercise 1 - Generate 1000 numbers from a normal distribution with mean 3 and sd = .25 and write to the object called x.
# Normal Distribution
?rnorm

# rnorm(n, mean, sd)
m <- 3
s_d <- 0.25

x <- rnorm(1000, mean = m, sd = s_d )

# Exercise 2 - Create the histogram of the data generated in the previous item and add a layer with the normal curve.
?hist

hist(x, prob=TRUE, ylim=c(0,1.80),breaks=20, main = "Histogram of x")
curve(dnorm(x, m, s_d), add=TRUE, col="blue", lwd=2)

# Other way
install.packages("ggplot2")
library(ggplot2)

graphic <- ggplot(data.frame(x), aes(x)) +
  geom_histogram(aes(y = ..density..), bins = 30, color = "black", fill = "lightblue") +
  stat_function(fun = dnorm, args = list(mean = m, sd = s_d), color = "red", size = 1.5) +
  labs(title = "Histogram and Normal Distribution Curve",
       x = "Values",
       y = "Density") +
  theme_minimal()

graphic

# Exercise 3 - Assume that 80% of adults with allergies report symptomatic relief with a specific medication.
# If the drug is given to 10 new patients with allergies, what is the probability that it will be
# effective at exactly seven?

# Binomial Distribution
?dbinom

# dbinom(x, size, prob, log = FALSE)
s <- 10
p <- 0.8
x <- 7

probability_03 <- dbinom(x, size = s, prob = p)
percentage_03 <- probability_03 * 100

cat("The probability that it is effective at exactly seven is:", round(percentage_03, 2), "%\n")

# Graph to know any probability
graph <- function(n,p){
  x <- dbinom(0:n,size=n,prob=p)
  barplot(x,ylim=c(0,0.4),names.arg=0:n,
          main=sprintf(paste('Binomial Distribution(n,p) ',n,p,sep=',')))
}
graph(10,0.8)


# Exercise 4 - Suppose the test results of an entrance exam fit a normal distribution.
# Also, the mean test score is 72 and the standard deviation is 15.2.
# What percentage of students scored 84 or higher on the exam?

# Cumulative Distribution of the Normal Distribution.
?pnorm

# pnorm(q, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE)
m <- 72
s_d <- 15.2
limit_score <- 84

percentage_students <- pnorm(limit_score, mean = m, sd = s_d, lower.tail=FALSE)
percentage_04 <- percentage_students * 100

cat("The percentage of students who scored 84 or higher on the exam is:", round(percentage_04, 2), "%\n")

# We apply the pnorm function of the normal distribution with mean 72 and standard deviation 15.2.
# Since we are looking for the percentage of students with a score greater than 84,
# we are interested in the upper tail of the normal distribution.


# Exercise 5 - Suppose the average check-out time for a supermarket cashier is three minutes.
# Find the probability that a customer checkout is completed by the cashier in less than two minutes.

# Exponential Distribution
?pexp

# pexp(q, rate = 1, lower.tail = TRUE, log.p = FALSE)
mean_check_out <- 3

probability_05 <- pexp(2, rate = 1/mean_check_out, lower.tail = TRUE)
percentage_05 <- probability_05 * 100

cat("The probability that a customer checkout is completed by the cashier in less than two minutes is:", round(percentage_05, 2), "%\n")

# The checkout processing fee is equal to one divided by the average checkout completion time.
# Therefore, the processing rate is 1/3 of checkouts per minute.
# Next, we apply the pexp function of the exponential distribution with rate = 1/3.


# Exercise 6 - Select ten random numbers between one and three.
?runif

# runif(n, min, max)
n <- 10

random_numbers <- runif(n, min = 1, max = 3)
random_numbers 

# Exercise 7 - If there are 12 cars crossing a bridge per minute, on average
# find the probability of having 15 or more cars crossing the bridge in a given minute.

# Distribuição de Poisson
?ppois

# ppois(q, lambda, lower.tail = TRUE, log.p = FALSE)
average_rate <- 12
num_desired_cars <- 15

# upper tail
probability_upper_tail <- ppois(num_desired_cars, lambda = average_rate, lower= FALSE)
percentage_upper_tail <- probability_upper_tail * 100

cat("The probability of having 15 or more cars crossing the bridge in a given minute is:", round(percentage_upper_tail, 2), "%\n")

# So the probability of having 15 or more cars crossing the bridge in one minute is in the
# upper tail of the probability density function.

# Curiosity
# The probability of having 14 or fewer cars crossing the bridge in a given minute is given by the ppois function.
# lower tail
average_rate <- 12
num_desired_cars <- 14

probability_lower_tail <- ppois(num_desired_cars, lambda = average_rate)
percentage_lower_tail <- probability_lower_tail* 100

cat("The probability of having 14 or fewer cars crossing the bridge in a given minute is:", round(percentage_lower_tail, 2), "%\n")

# Exercise 8 - Suppose there are 12 multiple choice questions in an English quiz.
# Each question has five possible answers and only one of them is correct.
# Find the probability of getting four or fewer correct answers if a student tries
# Answer each question randomly.

# cumulative probability of the binomial distribution.
?pbinom

# pbinom(q, size, prob, lower.tail = TRUE, log.p = FALSE)
num_questions <- 12
probability_hit <- 1/5

probability_08 <- pbinom(4, size = num_questions, prob = probability_hit)
percentage_08 <- probability_08 * 100

cat("The probability of getting four or fewer correct answers if a student tries to answer each question at random is:", round(percentage_08, 2), "%\n")

# Other way
# To find the probability of having four or fewer correct answers by random trials,
# apply the dbinom function with x = 0,…, 4.
dbinom(0, size=12, prob=0.2) + 
  dbinom(1, size=12, prob=0.2) + 
  dbinom(2, size=12, prob=0.2) + 
  dbinom(3, size=12, prob=0.2) + 
  dbinom(4, size=12, prob=0.2) 






