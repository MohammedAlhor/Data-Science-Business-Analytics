# filteren van data
install.packages('dplyr')
library(dplyr)
data(storms, package = 'dplyr')
#inladen data
eredivisie <- readRDS("~/Documents/Data-Science-Business-Analytics/Data/eredivisie.Rds")

#les opdrachten
ggplot(storms, aes(x= wind, y = pressure)) + geom_point(color='red',shape =10, size = 3)
#line types, fill, shape,
ggplot(eredivisie, aes(x= status, y = wind)) + geom_boxplot()
#dit kun je ook afhankelijk maken van een categorische variabele uit de dataset
ggplot(storms, aes(x= wind, y = pressure, color = status)) + geom_point(shape =10, size = 3)


#eredivisie
ggplot(eredivisie, aes(x=Year, y = Points, color = density) + geom_line()
 

# Exercises Patent Data
# 1
patents <- readRDS("~/Documents/Data-Science-Business-Analytics/Data/patents.Rds")
# a
ggplot(patents, aes(x=logtotal, color = densitycat)) +geom_density(fill='black', alpha=0.35)

ggplot(patents, aes(x=logtotal, y=logdensity, color = governor)) +geom_point(shape=21, size = 3)

ggplot(patents, aes(x=logtotal, y=logdensity, color = governor)) +geom_point(shape=16, size = 3)

ggplot(patents, aes(x=logtotal, color = densitycat)) +geom_boxplot()
# not really, it is unclear what logtotal means looking at the figure. Density might be more understandable.
ggplot(patents, aes(x=logtotal, color = densitycat)) +geom_boxplot()
?geom_boxplot

# 2
data(msleep, package = 'ggplot2')
colors <- c(A = "#333BFF", B = "#CC6600", C ="#9633FF", D = "#E2FF33", E = "#E3DB71")
View(msleep)
ggplot(msleep, aes(x=sleep_total,y=sleep_rem, shape = vore)) +geom_point(shape=15, size=2) + scale_shape_manual(1:3)
#manueel shape en kleur classificaties meegeven door +... daarnaast kun je missende negeren, dit doe je door na.translate = FALSE
# + scale_color_manual(values=colors)




