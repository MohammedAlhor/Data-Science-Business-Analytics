## install.packages("dplyr")
## install.packages('ggplot2')
library(dplyr)
library(ggplot2)


data(storms, package = 'dplyr')

plot(select(storms, status, category, wind, pressure))

## 2 elementen, 1ste is het ggplot element (Y,x variablen), het tweede is wat voor soort visualisatie wil je zien.
# dit is de grafiek zonder punten ingetekend
ggplot(storms, aes(x= wind, y=pressure))
# mbv +geom_point tekenen we de punten
ggplot(storms, aes(x= wind, y=pressure)) +geom_point()
# dit is een histogram van 1 variabele
ggplot(storms, aes(x= wind)) + geom_histogram()
# je kunt de bins ook aanpassen om de figuur wat duidelijker te maken (default is 30)
ggplot(storms, aes(x= wind)) + geom_histogram(bins = 15)
# aantal verschillende diagrammen
ggplot(storms, aes(x= wind)) + geom_density()
# visualisatie om te laten zien of de aanname van normaliteit aanhoudt, deze kan voor andere verdelingen gebruikt worden (distribution)
ggplot(storms, aes(sample= wind)) + geom_qq()
# boxplot, middelste is mediaan, onderste is 25 bovenste is 75. box bevat 50%. erboven zijn de uitschieters.
ggplot(storms, aes(x= "", y = wind)) + geom_boxplot()
# over factor variable, bv status. Kun je dit ook visualiseren.
ggplot(storms, aes(x= status, y = wind)) + geom_boxplot()       
# barplot
ggplot(storms, aes(x=status)) + geom_bar()

##
data(economics, package = 'ggplot2')

ggplot(economics, aes(x= date, y = unemploy)) + geom_line()


#setwd("~/Documents/Data-Science-Business-Analytics/Data")
#### Importeren dataset
patents <- readRDS("~/Documents/Data-Science-Business-Analytics/Data/patents.Rds")

#scatterplot
ggplot(patents, aes(x= total, y = density)) + geom_point()



