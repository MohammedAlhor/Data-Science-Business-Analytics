### Eindopdracht deel 2 ###
# Libraries
install.packages("MASS")
install.packages("leaps")
library(MASS)
library(car)
library(HH)
library(ggplot2)


# laden data
# setwd("~/Documents/Data-Science-Business-Analytics/Data")
setwd("~/Data-Science-Business-Analytics/Data")
college_statistics <- read.csv("college_statistics.csv", header = TRUE )
# Rownames vullen met inhoud van de eerste kolom
rownames(college_statistics) <- college_statistics[,1]
# Verwijder eerste kolom
college_statistics <- college_statistics[,-1]


# 4 (a) Voer eerst een test uit voor de hypothese dat het aantal aanmeldingen een
# normale verdeling volgt. Wat is je conclusie? Is deze conclusie van belang voor
# het verder modelleren van deze variabele?
summary(college_statistics$Apps)
hist(college_statistics$Apps)
qqnorm(college_statistics$Apps)
# Both the histogram and qq plot indicate that this variable is not normally distributed (Positive skewness in histogram and )
# Lets put this to the test with a Shapiro-Wilk test for normality
# H0: Apps is normally distributed Ha: Apps is not normally distributed
shapiro.test(college_statistics$Apps)

# The p-value is less than 0.05, which means the null hypothesis can be rejected and the data is not normally distributed.

# 4 (b) Deel de data eerst op willekeurige manier op in een “estimation” en “test”
# sample. Neem 600 universiteiten in de estimation sample. Zorg ervoor dat deze
# opdeling reproduceerbaar is. Hint: de R-functies set.seed en sample kunnen
# hiervoor gebruikt worden.
set.seed(123)

train_ind <- sample(seq_len(nrow(college_statistics)), size=600)

college_statistics_est <- college_statistics[train_ind,]
college_statistics_test <- college_statistics[-train_ind,]

# 4 (c) Maak eerst een lineair model voor het aantal aanmeldingen. Gebruik hiervoor
# alleen de estimation sample.

fit1 <- lm(Apps ~ Private + Top10perc + Top25perc + F.Undergrad + P.Undergrad + Outstate + Room.Board + Books + Personal + PhD + Terminal +
           S.F.Ratio + perc.alumni + Expend + Grad.Rate , data = college_statistics_est)
summary(fit1)

# 4 (d) Pas backward elimination toe om het aantal variabelen terug te brengen.

# Backwards step regression
backresults <- stepAIC(fit1, direction = "backward")

# Record the best model selected by the backwards method
# This line takes the model specification as 'code'
backmodel <- backresults$call
backmodel
# This line evaluates the 'code' of the model
backmodel <- eval(backmodel)
summary(backmodel)

fit1 <- backmodel
summary(fit1)

# 4 (e) Voer diverse toetsen uit om de aannamen van het lineaire model te testen.
# Normality, Independence, Linearity, Homoskedasticity, Multicollinearity

# Test for normality
qqPlot(fit1)
shapiro.test(residuals(fit1))
# Some of the datapoints don't fall along the reference line, which means we can assume non normality. We alo observe outliers for Texas A&M Univ. at College Station
# and SUNY at Albany.


# Test for independence using the Durbin-Watson test. 
durbinWatsonTest(fit1)
# No autocorrelation detected (p-value is 0.382), which makes sense when we take into accoun the data we have

# Let's check for linearity(A3) using component + residual plot
crPlots(fit1)


# Check this with the RESET test
# H0: linear relation between x and y
# Ha: some nonlinearity
library(lmtest)
resettest(fit1, power=2)
# p-value < 0.05 -> we reject linearity


# A4 Homoskedasticity
ncvTest(fit1)
coeftest(fit1, vcov='vcovHC')
# We reject H0 of homoskedasticity -> variances are not constant


# A7 Multicolinearity (No perfect linear relationship in X)
vif(fit1)
# None are larger than 4, rule of thumb

# Simple outlier test
outlierTest(fit1)


# 4 (f) Maak vervolgens een model voor de logaritme van het aantal aanmeldingen (ook weer met backward elimination).

fit2 <- lm(log(Apps) ~ Private + Top10perc + Top25perc + F.Undergrad + P.Undergrad + Outstate + Room.Board + Books + Personal + PhD + Terminal +
             S.F.Ratio + perc.alumni + Expend + Grad.Rate , data = college_statistics_est)
summary(fit2)

# Backwards step regression
backresults <- stepAIC(fit2, direction = "backward")

# Record the best model selected by the backwards method
# This line takes the model specification as 'code'
backmodel <- backresults$call
backmodel
# This line evaluates the 'code' of the model
backmodel <- eval(backmodel)
summary(backmodel)

fit2 <- backmodel
summary(fit2)

# 4 (g) Voer opnieuw de diverse toetsen uit om de aannamen van het model te testen.

# Test for normality
qqPlot(fit2)
shapiro.test(residuals(fit2))

# Test for independence using the Durbin-Watson test. 
durbinWatsonTest(fit1)
# No autocorrelation detected (p-value is 0.382), which makes sense when we take into accoun the data we have

# Let's check for linearity(A3) using component + residual plot
crPlots(fit1)


# Check this with the RESET test
# H0: linear relation between x and y
# Ha: some nonlinearity
library(lmtest)
resettest(fit2, power=2)
# p-value < 0.05 -> we reject linearity


# A4 Homoskedasticity
ncvTest(fit2)
# We fail to reject the null-hypothesis (p-value > 0.05), and thus can assume homoskedacticity is present.


# A7 Multicolinearity (No perfect linear relationship in X)
vif(fit2)
# None are larger than 4 (this is the rule of thumb), we can assume no multicolinearity.

# Simple outlier test
outlierTest(fit2)

# 4 (h) Welk van de twee modellen heeft de voorkeur?
AIC(fit1,fit2)


# 4 (i) Probeer het gekozen model nog verder te verbeteren: denk aan het toevoegen van transformaties van verklarende variabelen.

# It is apparent, from the crPlot, that the relationship between the log(Apps) and the F.Undergrad variable is not linear. Let's try to do a 
# square root transformation on this variable and compare the models.

fit3 <- lm(formula = log(Apps) ~ Private + Top25perc + F.Undergrad + I(sqrt(F.Undergrad)) +
             Outstate + Room.Board + Books + PhD + S.F.Ratio + perc.alumni + 
             Expend + Grad.Rate, data = college_statistics_est)

summary(fit3)
crPlots(fit3)
# The crPlots presents us with much better results, we see a more linear relationship for F.Undergrad
# Let's check this using AIC
AIC(fit2,fit3)
# The AIC value goes from 1025 to 684, the transformation results in a much better model!

# Let's build on this model and perform a quadratic transformation on the S.F.Ratio (see crPlot for non linear relationship). 

fit4 <- lm(formula = log(Apps) ~ Private + Top25perc + F.Undergrad + I(sqrt(F.Undergrad)) +
             Outstate + Room.Board + Books + PhD + S.F.Ratio + I(S.F.Ratio^2) + perc.alumni + 
             Expend + Grad.Rate, data = college_statistics_est)

summary(fit4)
crPlots(fit4)
AIC(fit3,fit4)

           
fit5 <- lm(formula = log(Apps) ~ Private + Top25perc + F.Undergrad + I(sqrt(F.Undergrad)) +
             Outstate + Room.Board + Books + PhD + S.F.Ratio + I(S.F.Ratio^2) + perc.alumni + 
             Expend + Grad.Rate+ I(sqrt(Grad.Rate)), data = college_statistics_est)

summary(fit5)
crPlots(fit5)
AIC(fit4,fit5)


# 4 (j) Hoe interpreteer je de coefficienten in het model dat je uiteindelijk hebt gevonden? Wees hierbij heel precies. Welke factoren zijn uiteindelijk het meest van belang?

coef(fit5)


# (k) Gebruik het uiteindelijke model om voorspellingen te maken voor de waarnemingen in de estimation en de test sample.

# We use the predict function to make predictions on the test and estimation sets. Exp() is used on these predictions to get the real number of Apps, instead of the log values.
college_statistics_test$predict <- exp(predict(fit5, newdata = college_statistics_test))

college_statistics_est$predict <- exp(predict(fit5, newdata = college_statistics_est))

# 4 (l) Vergelijk de voorspelkracht (mbv. mean squared error) van het model op de
# estimation sample met die op de test sample. Wat concludeer je?

# First let's calculate the mean squared error of both predictions.

test_MSE <- mean((college_statistics_test$Apps - college_statistics_test$predict)^2)
est_MSE <- mean((college_statistics_est$Apps - college_statistics_est$predict)^2)

# The MSE for the test set is 8.509.323, for the estimation set it's 1.849.956. The model obviously has more explanatory power for the data it was built on.
# Outside of this sample this power decreases.

# 5. In dit onderdeel voer je een ANOVA analyse uit op de relatie tussen de student
# faculty ratio en het percentage studenten uit de top 25% van de high school. Volg
# de volgende stappen:


# 5 (a) Maak een factor met drie levels op basis van de variabele Top25perc. De levels zijn: laag (minder dan 20%)/midden/hoog (meer dan 40%)

college_statistics$catTop25perc[as.numeric(college_statistics$Top25perc)<20]="laag"
college_statistics$catTop25perc[as.numeric(college_statistics$Top25perc)>=20 & as.numeric(college_statistics$Top25perc)<=40]="midden"
college_statistics$catTop25perc[as.numeric(college_statistics$Top25perc)>40]="hoog"

college_statistics$catTop25perc <- as.factor(college_statistics$catTop25perc)


# (b) Voer de ANOVA analyse uit en geef je conclusie(s). Presenteer de ANOVA
# resultaten zowel numeriek als grafisch.

# One-way ANOVA
aov1 <- aov(S.F.Ratio~catTop25perc, data=college_statistics)
summary(aov1)
# H0 : The mean of the dependent variable is the same across all groups

# We can reject the null hypothesis (p-value < 0.05), thus there are significant differences between groups.

TukeyHSD(aov1)
plot(TukeyHSD(aov1))


# 5 (c) Onderzoek of er outliers zijn. Pas de analyse aan als dat nodig is.

outlierTest(aov1)

# We find an outlier for Indiana Wesleyan University
# Let's drop this observation and try again
college_statistics_outlier <- college_statistics[rownames(college_statistics) != 'Indiana Wesleyan University',]

aov2 <- aov(S.F.Ratio~catTop25perc, data=college_statistics_outlier)
summary(aov2)

TukeyHSD(aov2)
plot(TukeyHSD(aov2))

outlierTest(aov2)
