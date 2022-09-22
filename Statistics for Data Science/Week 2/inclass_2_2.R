# Statistics for Data Science
# Lecture 2
# In-class assignment 2.2
# Exercises using Chi2 distribution

library(gridExtra)  # library to show multiple graphs side-by-side 
library(ggplot2)

# create variables to hold `constants'
k <- 10      
n <- 250

# Generate random variables
d <- rchisq(n, k)
simdata <- data.frame(draws=d)

# plot histogram, density and estimated cdf
p1 <- ggplot(data = simdata, aes(x = draws)) + geom_histogram()
p2 <- ggplot(data = simdata, aes(x = draws)) + geom_density()
p3 <- ggplot(data = simdata, aes(x = draws)) + geom_step(stat = "ecdf")
p4 <- ggplot(data = data.frame(x=seq(0,25,.1), y=pchisq(seq(0,25,.1), df=k)), aes(x = x, y=y)) + geom_line() + ylab("Theoretical cdf")
grid.arrange(p1, p2,  p3, p4, nrow = 2, ncol = 2)

# Calculate probability [X<k]
pchisq(10, df = k)

# Approximate this using simulated draws
mean(simdata$draws < k)
