# import housing data
load("~/Documents/Data-Science-Business-Analytics/Data/houseprice.RData")
library(ggplot2)

lm <- lm(price ~ lotsize, data=houseprice)

summary(lm)

# The R squared is 0.2871 , R^2 = 1-(SSE/SSY), ssy is larger than sse
# verklaart 28 % van de variatie
attributes(lm)
attributes(summary(lm))

# H0: B = 0, Ha: B !=0
# H0 can be rejected, b is >0 and statistically significant
cor.test(houseprice$price, houseprice$lotsize)


lm_log <- lm(log(price) ~ log(lotsize), data =houseprice)
summary(lm_log)
# When x increases by 1 percent, y increases bij B*1%

plot(log(houseprice$price), log(houseprice$lotsize))
