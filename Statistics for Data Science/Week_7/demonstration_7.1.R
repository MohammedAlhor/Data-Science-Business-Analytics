# Statistics for Data Science
# Lecture 7
# In-class assignment 7.1
# Contingency table exercise

#Load data
load('houseprice.RData')

# Create factors
houseprice$catbed[as.numeric(houseprice$bedrooms)<=2] <- "small"
houseprice$catbed[as.numeric(houseprice$bedrooms)==3] <- "medium"
houseprice$catbed[as.numeric(houseprice$bedrooms)>=4] <- "large"
houseprice$catbed <- as.factor(houseprice$catbed)

houseprice$catbath[houseprice$bathrms<=1] <- "1"
houseprice$catbath[houseprice$bathrms >1] <- ">1"
houseprice$catbath <- as.factor(houseprice$catbath)

# Show some tables
table(houseprice$bathrms)
table(houseprice$bedrooms)
table(houseprice$catbath)
table(houseprice$catbed)
table(houseprice$catbath, houseprice$catbed)

# Save the cross table
m <-  table(houseprice$catbath, houseprice$catbed)

# Perform test(s) on contingency table
chisq.test(m)
#or 
chisq.test(houseprice$catbath, houseprice$catbed)

# Alternatively use the Fisher test
fisher.test(m)

# Conclusion
# for both tests we reject the null hypothesis of no relation
# conclusion: we have reason to believe that both variables are related.

## Some extra's
# Transform to propensities/show margins
pm <- prop.table(m)
pm
addmargins(pm)
# Note: do not apply the tests on these tables 
# (the numbers are treated as number of observations!)