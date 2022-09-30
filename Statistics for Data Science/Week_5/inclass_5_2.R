#libs
library(lmtest)
library(car)
lm <- lm(Murder~Population + Illiteracy + Income + Frost, data=dataset)
qqPlot(lm)
# studentized residuals vs. the student t distribution
# A3 test for linearity
crPlots(lm)

# antwoorden
fit1 <- lm(Murder~Population + Illiteracy + Income + Frost, data=dataset)
qqPlot(fit1)
# Nevada is een outlier, komt waarschijnlijk door frost

dataset["Nevada", ]
fitted(fit1)["Nevada"]
fit1$residuals["Nevada"]


# Autocorrelate A5
durbinWatsonTest(fit1)

fit2 <- lm(Murder~Population+Income, data=dataset)
crPlots(fit2)
#linearity Population is redelijk linear, income is parabalisch maar wordt verklaard door Alaska.
# Verband dat anders is dan lineair is waar we op zoek naar zijn.
# Inkomen lijkt dus linear A3 is aanname voor linear verband.
# blauwe lijn is beta coefficient * populatie

#consider alternative model with logs
fit3 <- lm(Murder~log(Population)+ Income + I(Income^2), data = dataset)
crPlots(fit3)
resettest(fit3, power =2)
summary(fit3)


# test for homoskedacticity 
fit4 <- lm(Murder~Income+Population, data=dataset)
library(sandwich)
library(car)

ncvTest(fit4)
# p waarde is klein, sig niv van 5% null hyptohese verwerpen. Model bevat heteroskedacticiteit

vcovHC(fit4)
coeftest(fit4, vcov = vcovHC)
?coeftest
# hoe interpreteren we deze.
vif(fit1)
# is niet groter dan 4 dus, nee. 


