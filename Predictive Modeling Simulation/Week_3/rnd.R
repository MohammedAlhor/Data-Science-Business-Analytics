# Draw 10, 100, and 10.000 random numbers from:

# a uniform distribution on (0,1)
runif10 = runif(10, 0, 1)
hist(runif10, breaks = 100)
runif100 = runif(100, 0, 1)
hist(runif100, breaks = 100)
runif10K = runif(10000, 0, 1)
hist(runif10K, breaks = 100)

# a normal distribution with mean 10 and standard deviation 5
rnorm10 = rnorm(10, 10, 5)
hist(rnorm10, breaks = 100)
rnorm100 = rnorm(100, 10, 5)
hist(rnorm100, breaks = 100)
rnorm10K = rnorm(10000, 10, 5)
hist(rnorm10K, breaks = 100)

# a gamma distribution with shape 1 and rate 3
rgamma10 = rgamma(10, 1, 3)
hist(rgamma10, breaks = 100)
rgamma100 = rgamma(100, 1, 3)
hist(rgamma100, breaks = 100)
rgamma10K = rgamma(10000, 1, 3)
hist(rgamma10K, breaks = 100)
