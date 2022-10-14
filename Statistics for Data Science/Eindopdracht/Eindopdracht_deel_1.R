### Eindopdracht deel 1 ###
# Libraries

# 1. Lees de data in als dataframe en zorg ervoor dat de eerste kolom als label voor de
#    rijen wordt gebruikt. Hint: zie ?row.names.
# setwd("~/Documents/Data-Science-Business-Analytics/Data")
setwd("~/Data-Science-Business-Analytics/Data")
college_statistics <- read.csv("college_statistics.csv", header = TRUE )
# Rownames vullen met inhoud van de eerste kolom
rownames(college_statistics) <- college_statistics[,1]
# Verwijder eerste kolom
college_statistics <- college_statistics[,-1]

# 2. Voer beschrijvende statistiek uit dmv. het maken van een aantal grafieken. Deze
#    grafieken mag je via de “Base R graphics” of mbv. ggplot maken.
library(ggplot2)
# (a) Zijn private universiteiten overwegend kleiner of groter dan publieke universi-
#     teiten? Je mag zelf een definitie voor groot/klein defini¨eren.
summary(college_statistics$Enroll)
sum(is.na(college_statistics$Enroll))
ggplot(college_statistics, aes(x=Enroll, color = Private)) + geom_boxplot()
ggplot(college_statistics, aes(x=Enroll, color = Private)) + geom_boxplot()


# (b) Hoe ziet de verdeling eruit van het acceptatiepercentage? Wat is het accepta-
#     tiepercentage voor de meest selectieve universiteit?
# Ik begin met het uitrekenen van het acceptatiepercentage (Accept/Apps*100)
sum(is.na(college_statistics$Apps))
sum(is.na(college_statistics$Accept))
college_statistics$Acc_perc <- (college_statistics$Accept/college_statistics$Apps)*100
hist(college_statistics$Acc_perc)
ggplot(college_statistics, aes(x=Acc_perc)) + geom_histogram(binwidth = 10)
# The distribution of acceptance percentage looks to be negatively skewed.
college_statistics[which.min(college_statistics$Acc_perc),]
# The school with the lowest acceptance rate seems to be Princeton.

# c) Zijn de meer selectieve universiteiten ook overwegend duurder dan minder selectieve universiteiten? 
#    (Je mag zelf bepalen welke kosten je wel/niet mee neemt.)
# To get this answer we must do some feature engineering. Which costs should we take into account?
# First, lets take a look at some histograms, summary stats and some quick plots of the different variables
hist(college_statistics$Outstate)
summary(college_statistics$Outstate)
plot(college_statistics$Acc_perc, college_statistics$Outstate)
# The scatterplot gives some evidence of a negative relationship between acceptance rate and out of state tuition. 
# We will be using this variable
hist(college_statistics$Room.Board)
summary(college_statistics$Room.Board)
plot(college_statistics$Acc_perc, college_statistics$Room.Board)
# The same is visible for room and board costs, to a lesser extent
hist(college_statistics$Personal)
summary(college_statistics$Personal)
plot(college_statistics$Acc_perc, college_statistics$Personal)
# No real relationship visible between acceptance rate and personal spending costs
summary(college_statistics$Books)
hist(college_statistics$Books)
plot(college_statistics$Acc_perc, college_statistics$Books)
# The same goes for the price of books. This is to be expected, the price of books shouldn't vary that much by school
college_statistics$total_cost <- college_statistics$Outstate + college_statistics$Room.Board

ggplot(college_statistics, aes(x=Acc_perc, y=total_cost)) + 
  geom_point() + 
  ylab("Total cost") + 
  xlab("Acceptance Rate") +
  labs(title = "Total costs vs. Accepance rate") + 
  geom_smooth(method='lm')

## (d) Bedenk zelf een extra vraag en cre¨eer een geschikte figuur om deze vraag mee
## te beantwoorden.
# Hebben duurdere universeiten een hoger percentage aan afgestudeerden die geld doneren? 
# Bekijk ook het verschil tussen private en publieke universiteiten.
summary(college_statistics$perc.alumni)
ggplot(college_statistics, aes(x=total_cost, y=perc.alumni)) + 
  geom_point() + 
  ylab("Percentage alumni donations") + 
  xlab("Total Cost") + 
  labs(title = "Alumni donations vs. Total costs")+ 
  geom_smooth(method='lm')

ggplot(college_statistics, aes(x=total_cost, y=perc.alumni, color =Private)) + 
  geom_point() + 
  ylab("Percentage alumni donations") + 
  xlab("Total Cost") + 
  labs(title = "Alumni donations vs. Total costs", subtitle = 'Private vs. Public schools')+ 
  geom_smooth(method = 'lm')

# 3 (a) Ontvangen elite scholen een ander aantal aanmeldingen in vergelijking met niet-
#     elite scholen? Definieer “elite-school” als scholen waarvoor geldt dat meer dan
#     50% van de studenten tot de top 10% van hun high school behoort.
elite <- subset(college_statistics, Top10perc > 50)
not_elite <- subset(college_statistics, Top10perc <= 50)
hist(elite$Apps)
hist(not_elite$Apps)
# not normally distributed
shapiro.test(elite$Apps)
shapiro.test(not_elite$Apps)
# not normally distributed for bot elite and non elite schools

var(elite$Apps)
var(not_elite$Apps)
sd(elite$Apps)
sd(not_elite$Apps)

# lets take a look at the variances with a Fischer F test for variance
# H0 : σ2elite = σ2not_elite Ha: σ2elite > σ2not_elite
var.test(elite$Apps,not_elite$Apps, alternative=c("two.sided","less","greater"))
# Ratio of variances is not equal to 1, and the null hypothesis can be rejected. This helps us in the answer whether the 
# amount of applications are statistically different for elite schools compared to non elite schools. We use a t.test with 
# unequal variance
mean(elite$Apps)
mean(not_elite$Apps)

wilcox.test(elite$Apps, not_elite$Apps, alternative=c("two.sided","less","greater"))

# (b) Is er een verband tussen acceptance rate en graduation rate?

college_statistics_b <- college_statistics[college_statistics$Grad.Rate <= 100,]

ggplot(college_statistics_b, aes(x=Acc_perc, y=Grad.Rate)) + 
  geom_point() + 
  ylab("Acceptance Rate") + 
  xlab("Graduation Rate") + 
  labs(title = "Acceptance Rate vs. Gradutation Rate")+ 
  geom_smooth(method = 'lm')


shapiro.test(college_statistics_b$Acc_perc)
shapiro.test(college_statistics_b$Grad.Rate)

cor.test(college_statistics_b$Acc_perc, college_statistics_b$Grad.Rate, method = 'kendall')


# (c) Bedenk zelf ook een extra hypothese om te toetsen en voer de hypothesetoets
# uit.
# Is er een verband tussen persoonlijke uitgaven van studenten en de graduation rate?
ggplot(college_statistics_b, aes(x=Personal, y=Grad.Rate)) + 
  geom_point()  + 
  xlab("Personal Spending") + 
  ylab("Graduation Rate") + 
  labs(title = "Personal spending vs. Gradutation Rate")+ 
  geom_smooth(method = 'lm')

shapiro.test(college_statistics_b$Personal)
shapiro.test(college_statistics_b$Grad.Rate)

cor.test(college_statistics_b$Personal, college_statistics_b$Grad.Rate, method = 'kendall')
