# Use murder rate data
dataset <- as.data.frame(state.x77)
library(ggplot2)
##
ggplot(data = dataset, aes(sample = Murder)) + geom_qq() + geom_abline(intercept = mean(dataset$Murder), slope = sd(dataset$Murder), color = "red")
## Data zit redelijk goed op de lijn
shapiro.test(dataset$Murder)
qqnorm(dataset$Murder)
qqplot(dataset$Murder)

# h0: x is normaal verdeeld
# H0 kunnen we verwerpe met een significantieniveau van 5%. Murder is dus niet normaal verdeeld.
##
par(mfrow=c(2,2))
lm <- lm(Murder ~ Population + Income, data = dataset)
plot(lm)

# A3: linearity, zit geen patroon in de residuals vs. de fitted 
# A6: QQ plot for standardized residuals. Normality. 
# A4 homoskedasticity. Geen verband tussen grootte residu en gefitte waardes
# Outliers: leverage vs. residuals

# alaska eruit halen

states <- row.names(dataset)
m2 <- lm(Murder ~ Population + Income, data=dataset[states!="Alaska",])
plot(m2)

# A3: linearity, zit geen patroon in de residuals vs. de fitted 
# A6: QQ plot for standardized residuals. Normality. 
# A4 homoskedasticity. Geen verband tussen grootte residu en gefitte waardes
# Outliers: leverage vs. residuals

x<- rnorm(100)
y <- 1 + 2*x + rnorm(100)
plot(lm(x~y))
