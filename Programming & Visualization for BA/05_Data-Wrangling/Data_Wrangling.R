#Data wrangling
install.packages("nycflights13")
library(nycflights13)
library(tidyverse)
library(dplyr)

#data inladen en wat descriptives
data("flights", package="nycflights13")
View(flights)
head(flights)
dim(flights)
summary(flights)
?flights

#vluchten op 13 mei

flights_march13 <- filter(flights, flights$month == 3 & flights$day == 13)

flights_q4 <- filter(flights, flights$month %in% c(10:12))
View(flights_q4)
flights_no_dep <- filter(flights, is.na())


# Exercise 1
flights_delay <- filter(flights,  arr_delay >= 120 & dep_delay <= 0 )
flights_carr <- filter(flights, carrier %in% c("UA", "AA", "DL"))

# Exercise 2
flights_del <- arrange(flights,desc(arr_delay))
?arrange()
flights_tail <- arrange(flights,!is.na(tailnum))

# Exercise 3

flights_select <- select(flights, ends_with(c("_time","_delay")))

# Exercise 4

flights_mutate <- mutate(flights, hours= floor(dep_time/100),
                         minutes = dep_time-hours*100,
                         dep_time_mins = hours*60 +minutes)
flights_time <- select(flights_mutate, -hours, -minutes)

# Exercise 5

flights_group <- group_by(flights, month)
flights_summ <- summarise(flights_group, 
                          flights_total = sum(!is.na(flight)),
                          flights_cancel = sum(is.na(dep_time|arr_time)), 
                          perc_canc = flights_cancel/flights_total*100) %>% 
                          arrange(desc(perc_canc))
flights_group2 <- group_by(flights, dest)

flights_summ2 <- summarise(flights_group2, 
                           minimum = min(air_time, na.rm = TRUE),
                           maximum = max(air_time, na.rm = TRUE),
                           mediaan = median(air_time, na.rm=TRUE),
                           mean = mean(air_time, na.rm=TRUE),
                           sd = sd(air_time, na.rm=TRUE))
?summarise()


                          