# Statistics for Data Science
# Lecture 3
# Take home assignment 
# Correlations

#Load data
load("houseprice.RData")
load("~/Data-Science-Business-Analytics/Data/houseprice.RData")
# Visualize the correlation between some (continuous) variables 
plot(houseprice$lotsize,houseprice$price)

# Calculate the correlation
cor(houseprice$lotsize,houseprice$price)

# Perform a hypothesis test on this correlation 
# (clearly formulate the hypotheses and the conclusion)
cor.test(houseprice$lotsize,houseprice$price)
# H0: correlation equals 0, Ha: correlation ≠ 0
# Conclusion reject H0, there is a significant correlation
