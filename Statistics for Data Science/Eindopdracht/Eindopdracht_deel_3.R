### Eindopdracht deel 2 ###
# Libraries
library(dplyr)
library(car)
library(effects)
library(tidyverse)
library(MASS)
library(leaps)
library(sandwich)
library(lmtest)
library(caret)
library(ggplot2)

# laden data
# setwd("~/Documents/Data-Science-Business-Analytics/Data")
setwd("~/Data-Science-Business-Analytics/Data")
college_statistics <- read.csv("college_statistics.csv", header = TRUE, strip.white = TRUE, stringsAsFactors = FALSE, na.strings = c("NA", "") )
# Rownames vullen met inhoud van de eerste kolom
rownames(college_statistics) <- college_statistics[,1]
# Verwijder eerste kolom
college_statistics <- college_statistics[,-1]

# 6. Maak een model om de factoren te vinden die bijdragen aan een hoog “slagingssucces”.


# 6 (a) Definieer een nieuwe variabele die 1 als het slagingspercentage groter is dan 60%
# en 0 als dat niet zo is.
summary(college_statistics$Grad.Rate)

df <- college_statistics %>%
        filter(!Grad.Rate>100) %>% # Graduation rate cannot be higher than 100, therefore we drop this observation
        mutate(gr_dummy = case_when(Grad.Rate > 60 ~ 1, Grad.Rate <=60 ~ 0)) # We use mutate to create a dummy variable and a case when to set the conditions for this variable.

df$gr_dummy <-  as.factor(df$gr_dummy) # Make this a factor variable

# 6 (b) Deel de data opnieuw op in een estimation en een test sample.

set.seed(123) # set the seed so results can be replicated.

train_ind <- sample(seq_len(nrow(df)), size=600) # take the sample for the training data set, we use the same sample size as in previous questions

college_statistics_est <- df[train_ind,] # estimation set
college_statistics_test <- df[-train_ind,] # test set


# 6 c) Maak mbv. de estimation data een logit model om de slagingssucces variabele
# te verklaren. Denk hierbij goed na over transformaties van je variabelen. Bij-
# voorbeeld heeft het zin om het aantal applicaties, aantal acceptaties, en het
# aantal enrollments in hetzelfde model op te nemen? Of kunnen sommige van
# deze variabelen beter als percentages opgenomen worden?

# First, we model without any transformations

fit1 <- glm(gr_dummy ~ Private + Apps + Accept + Enroll + Top10perc + Top25perc + F.Undergrad + P.Undergrad + Outstate + Room.Board + Books + Personal + PhD + Terminal +
           S.F.Ratio + perc.alumni + Expend ,family = binomial(link=logit), data = college_statistics_est)

summary(fit1)

vif(fit1)
# We see that Apps, Accept and enroll have vif values of 20+(this makes sense, there's obsiously some multicolineairity at play here). Let's do some transformations on Enroll and Accept.

college_statistics_est <- college_statistics_est %>% mutate(acc_rate = Accept/Apps, # percentage of students that applied that got accepted and the percentage of students that got
                                  enroll_rate = Enroll/Accept)                      #  accepted and actually enrolled
college_statistics_test <- college_statistics_test %>% mutate(acc_rate = Accept/Apps, 
                                   enroll_rate = Enroll/Accept)
  

fit2 <- glm(gr_dummy ~ Private + Apps + acc_rate + enroll_rate + Top10perc + Top25perc + F.Undergrad + P.Undergrad + Outstate + Room.Board + Books + Personal + PhD + Terminal +
           S.F.Ratio + perc.alumni + Expend ,family = binomial(link=logit), data = college_statistics_est)

summary(fit2)

vif(fit2) # We see a significant decrease of the VIF


# 6 (d) Gebruik wederom backward selection om het aantal verklarende variabelen te verkleinen.

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

vif(fit2) # all values below 4 (rule of thumb)

# The model selected by the backwards methods goes from 17 variables to 10 variables

# Lastly, to get a better feel for the model we can use the effects package to get ceteris paribus plots
plot(predictorEffects(fit2))


# 6 (e) Welke variabelen hebben uiteindelijk een significante invloed?

# Private, Apps, Top25perc, P.Undergrad, Outstate, Room.Board, Books, perc.alumni and Expend have a significant effect


# 6 (f) Bereken het percentage goed voorspelde scholen zowel voor de estimation sample
# als voor de test sample (maak eerst voorspellingen voor beide datasets en gebruik
# daarna bijvoorbeeld de functie confusionMatrix()).

college_statistics_test$predict <- predict(fit2, newdata = college_statistics_test)
college_statistics_est$predict <- predict(fit2, newdata = college_statistics_est)



college_statistics_test <- college_statistics_test %>% 
  mutate(predict2 = case_when(predict >= 0.5 ~ 1,predict < 0.5 ~ 0)) 

college_statistics_est <- college_statistics_est %>% 
  mutate(predict2 = case_when(predict >= 0.5 ~ 1,predict < 0.5 ~ 0)) 


confusionMatrix(college_statistics_test$gr_dummy, as.factor(college_statistics_test$predict2))

confusionMatrix(college_statistics_est$gr_dummy, as.factor(college_statistics_est$predict2))


