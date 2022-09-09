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
kleuren <- c()
View(msleep)
ggplot(msleep, aes(x=sleep_total,y=sleep_rem, color = vore)) +geom_point(shape=15, size=2)  + scale_color_manual(values=c('red','blue','green','yellow'))



#manueel shape en kleur classificaties meegeven door +... daarnaast kun je missende negeren, dit doe je door na.translate = FALSE
# + scale_color_manual(values=colors)
#ipv manuele kleurkeuze zou je een numerieke variabele willen classificeren op kleur, hier kun je scale_color_coninuous voor gebruiken
#scale_xxx_continuous(), rood naar wit naar groen is een logische schaling

# for some plots, legends are unnecessary. Inside the brackets you can do show.legend = FALSE

# Flipping coordinates + coord_flip()
# Printer-friendly coordinates with themes. + theme_minimal() do ? for extra information. ow_theme <- theme([args])
# Limieten y en x as door ylim() en xlim(), xlabel(), ylabel()

# Layers toevoegen met het plusteken. +geom_smooth(method="LM", show.legend = FALSE), +geom_hline( aes(yintercept = 0.04), size=2, linetype='dashed')


