### Eindopdracht deel 2 ###
# Libraries
install.packages("MASS")
install.packages("leaps")
library(MASS)
library(car)




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

lm <- lm(Apps ~ Private + Accept + Enroll + Top10perc + Top25perc + F.Undergrad + P.Undergrad + Outstate + Room.Board + Books + Personal + PhD + Terminal +
           S.F.Ratio + perc.alumni + Expend + Grad.Rate , data = college_statistics_est)
summary(lm)

# 4 (d) Pas backward elimination toe om het aantal variabelen terug te brengen.

# Backwards step regression
backresults <- stepAIC(lm, direction = "backward")

# Record the best model selected by the backwards method
# This line takes the model specification as 'code'
backmodel <- backresults$call
backmodel
# This line evaluates the 'code' of the model
backmodel <- eval(backmodel)
summary(backmodel)

lm <- backmodel
summary(lm)

# 4 (e) Voer diverse toetsen uit om de aannamen van het lineaire model te testen.

# basic diagnostics:
qqPlot(lm)
par(mfrow=c(2,2))
plot(lm)

# Advanced diagnostics:


# 4 (f) Maak vervolgens een model voor de logaritme van het aantal aanmeldingen (ook weer met backward elimination).

lm2 <- lm(log(Apps) ~ Private + Top10perc + Top25perc + F.Undergrad + P.Undergrad + Outstate + Room.Board + Books + Personal + PhD + Terminal +
           S.F.Ratio + perc.alumni + Expend + Grad.Rate , data = college_statistics_est)
summary(lm)

# Backwards step regression
backresults <- stepAIC(lm2, direction = "backward")

crplot
