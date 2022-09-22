# Statistics for Data Science
# Lecture 3
# In-class assignment 3.1
# Simulation exercise with t-test

library(ggplot2)

# define constants
truemean <- 0.05
var      <- 1
n        <- 100

# Generate the data
data <- rnorm(n, mean = truemean, sd=sqrt(var))
summary(data)

# perform t-test
t.test(data, mu = 0)
# or calculate p-value yourself
pval <- 2*pt(-abs(mean(data)/sqrt(var(data)/n)), n - 1)
pval

# Calculate power for this exact setting
power.t.test(n, delta = truemean - 0, sd = sqrt(var), sig.level = 0.05, type = "one.sample", strict = TRUE)

# Calculate n needed for power = 50%
power.t.test(power = .5, delta = truemean - 0, sd = sqrt(var), sig.level = 0.05, type = "one.sample", strict = TRUE)

# Create power plots
deltas <- c(0, 0.05, 0.1, 0.5, 1)
samplesizes <- 3:250
df <- NULL
for (d in deltas)
{
  res <- power.t.test(samplesizes, delta = d, sd = sqrt(var), sig.level = 0.05, type = "one.sample", strict = TRUE)
  df <- rbind(df, data.frame(Power=res$power, Delta=rep(d,length(samplesizes)), SampleSize=samplesizes))
}
ggplot(df, aes(x=SampleSize, y=Power, group=Delta, color=Delta)) + geom_line()
