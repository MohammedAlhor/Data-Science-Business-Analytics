# Statistics for Data Science
# Lecture 5
# Take home assignment
# Outliers/strange observations

library(car)

#Load data and recreate models
dataset <- as.data.frame(state.x77[, c("Murder","Population", "Illiteracy", "Income", "Frost")])

fit <- lm(Murder~Population+Illiteracy+Income+Frost, data=dataset)

# Test for outliers
outlierTest(fit)
# 1 outlier: Nevada

# Calculate Cook's distance
cd <- cooks.distance(fit) 

# Select those that are large (6 is the number of parameters in the model)
sel <- cd > (4/(nrow(dataset)-6))
cd[sel]
# We find Alaska, Hawaii, Nevada

plot(fit, which=4)
influencePlot(fit)
# Worry most about Alaska (high hat-value (leverage) and high studentized residual)