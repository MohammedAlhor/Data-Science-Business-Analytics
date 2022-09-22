# Statistics for Data Science
# Lecture 2
# In-class assignment 2.1
# Calculating probabilities and quantiles

# Calculate the following probabilities
## Standard normally distributed variable is larger than 1.
1-pnorm(1)
dnorm(1)

## Normally distributed variable with mean 20 and variance 10 is smaller than 15.
pnorm(15, mean=20, sd=sqrt(10))

## Getting (exactly) 15 times head in 30 coin tosses.
dbinom(15, size=30, prob=0.5)
# check this using simulation
mean(  rbinom(10000000, size = 30, prob = .5) == 15  )

## Getting at least 15 times head in 30 coin tosses.
1-pbinom(14, size=30, prob=0.5)
## or
pbinom(14, size=30, prob=0.5, lower.tail = FALSE)

# Suppose that a soccer club has a 60% probability of winning each match they play.
# What is the probability that they do not win any of the first four matches of the year?
dbinom(0, size=4, prob=.6)

# Calculate some quantiles
## Calculate the number for which it holds that there is a 20\% probability that a standard normally distributed variable is larger, that is, calculate $ z $ such that $ \Pr[Z>z] = 0.2 $.
qnorm(.8)
### Check this
pnorm(qnorm(.8))

## Suppose that the waiting time for the bus has an exponential distribution with rate 1/10. How many minutes does one have to wait at most on the 20\% best days?
qexp(.2, rate = 1/10)


## How many minutes does one have to wait at least on the 5\% worst days?
qexp(1-0.05, rate = 1/10)

## Extra: plot the cdf of the exponential (also see Wikipedia for details on distributions)
plot(0:50, pexp(0:50,rate=1/10),'l')


