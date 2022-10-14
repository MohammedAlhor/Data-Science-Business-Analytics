# Statistics for Data Science
# Lecture 5
# In-class assignment 5.1
# Basic diagnostics

library(ggplot2)
dataset <- as.data.frame(state.x77[, c("Murder", "Income", "Population","Illiteracy","Frost")])

qqnorm(dataset$Murder)
qqline(dataset$Murder)

ggplot(data = dataset, aes(sample = Murder)) + geom_qq() + geom_abline(intercept = mean(dataset$Murder), slope = sd(dataset$Murder), color = "red")
# Murder is not normally distributed, 
# but this does not matter for a model 
# -> The error terms should be approx normal!

# Linear model
m <- lm(Murder ~ Population+Income, data=dataset)
summary(m)

# Show the basic diagnostics
par(mfrow=c(2,2))
plot(m)

# Conclusions:
# Alaska seems to be a very strange/outlying observations
# Removing Alaska seems justified (as it is not a "regular" state)

# Without Alaska
states <- row.names(dataset)
m2 <- lm(Murder ~ Population+Income, data=dataset[states!="Alaska",])
summary(m2)

# Show the basic diagnostics again
par(mfrow=c(2,2))
plot(m2)
# Things look better

# Without Alaska & Nevada
m3 <- lm(Murder ~ Population+Income, data=dataset[states!="Alaska" & states!="Nevada",])
summary(m3)

# Show the basic diagnostics
par(mfrow=c(2,2))
plot(m3)
# Things look quite ok (but is the US still the US without Alaska and Nevada?)
# Do we really want to remove (many) states?

# What would the plots be with a perfect model?
# Generate `perfect` data!
x <- rnorm(100)
y <- 1 + 2*x + rnorm(100)
plot(lm(y~x))
