# Statistics for Data Science
# Lecture 6
# In-class assignment 6.3
# ANCOVA
#
# requires package gplots and HH
# install.packages("gplots")	
# install.packages("HH")
#------------------------------------------------------------ 

library(HH)

load("houseprice.RData")

houseprice$catbed[as.numeric(houseprice$bedrooms)<=2]="small"
houseprice$catbed[as.numeric(houseprice$bedrooms)==3]="medium"
houseprice$catbed[as.numeric(houseprice$bedrooms)>=4]="large"
# Transform catbed to a "real factor"
houseprice$catbed <- as.factor(houseprice$catbed)

# In AN(C)OVA the order of the variables matters for the interpretation
fit1 <- aov(price~catbed+lotsize, data=houseprice)
summary(fit1)
coef(fit1)

fit2 <- aov(price~lotsize+catbed, data=houseprice)
summary(fit2)
coef(fit2)
# p values are about the "additional effect"

summary(lm(price~lotsize+catbed, data=houseprice))
summary(lm(price~catbed+lotsize, data=houseprice))
# in regression the p-values are not dependent on the order 
# (they always relate to the ceteris paribus effect)

# Considering interactions in ANCOVA
fit2 <- aov(price~catbed+lotsize, data=houseprice)
fit3 <- aov(price~catbed*lotsize, data=houseprice)
summary(fit2)
summary(fit3)
coef(fit3)

# The ancova function requires HH package
# Advantage is that it also gives graphical output
# It must run using a dataframe
ancova(price~catbed+lotsize, data=houseprice)
ancova(price~lotsize*catbed, data=houseprice)

