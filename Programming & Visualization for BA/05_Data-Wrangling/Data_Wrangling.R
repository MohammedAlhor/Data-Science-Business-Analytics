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

# Exercise 6 

?lag
flights_group_orig <- group_by(flights, origin)

flights_lag <- group_by(flights, origin) %>%
  arrange(dep_time, .by_group = TRUE )
  (flights_group3, dep_delay_lag = lag(dep_delay,n=1))
?arrange()

# Exercise 7

forsale_10obs <- forsale %>%
  select(postcode, city, suburb, asking_price, living_area) %>%
  head(n=10)

forsale <- forsale %>%
  mutate(price_m2 = asking_price/living_area)

forsale_by_city <- forsale %>%
  group_by(city) %>%
  summarise(n=n(), min_price = min(asking_price, na.rm=TRUE), 
            average_price = mean(asking_price, na.rm=TRUE),
            max_price = max(asking_price, na.rm=TRUE)) %>%
  arrange(average_price)


forsale_filter <- forsale %>%
  filter(city == 'Rotterdam', bedrooms >- 3, bathrooms >= 2, asking_price <= 400000)


# Exercise 8
penguins <- readRDS("~/Documents/Data-Science-Business-Analytics/Data/penguins.Rds")

distinct_all <- penguins %>%
  summarise(across(.cols= everything(), fns = n_distinct()))

distinct_select <- penguins %>%
  summarise(across(c(species, island, sex), fns = n_distinct()))

distinct_fac <- penguins %>%
  summarise(across(where(is.factor), -sum(is.na(.))))


penguins %>%
  group_by(species) %>%
  select(col_names) %>%
  mutate(mean_bill = mean(bill_length_mm, na.rm=TRUE), relative_bill = bill_length_mm/mean_bill)


# 
  

#########Opdrachten les#
install.packages("nycflights13")
library(nycflights13)
library(tidyverse)
data('planes')
data('flights')
planes
flights
flights_select <- flights %>%
  select(year, day, tailnum, flight, carrier, origin, dest, dep_time, arr_time)

flights_select %>%
  inner_join(planes, by=c('tailnum' = 'tailnum'))
x <- tibble(key = c(1,2,3), x_val = c("x1", "x2", "x3"))
y <- tibble(key = c(1,2,4), x_val = c("y1", "y2", "y3"))
left_join(x,y, by=c('key'='key'))


flight_planes <- flights_select %>%
  left_join(planes, by= c('tailnum' = 'tailnum'), suffix = c('_flights', '_build'))
colnames(flight_planes)


print(table1, n=6)
print(table2, n=6)
print(table3, n=6)

print(table4a)

table_gathered <- table4a %>%
  pivot_longer(cols = c('1999','2000'), 
               names_to = 'Year',
               values_to = "Country")



############# Exercises
## 9.
data('airports', package = 'nycflights13')
airports
data(package = 'nycflights13')
flights_data <- flights %>%
  left_join(airports, by=c('origin' = 'faa')) %>%
  select(colnames(flights), 'lat', 'lon')
data('airlines', package = 'nycflights13')
data('planes', package = 'nycflights13')
plane_info <- planes %>% select(tailnum, year)

flights_ex9 <- flights %>%
  select(carrier, tailnum) %>%
  distinct() %>%
  left_join(plane_info) %>%
  left_join(airlines) %>%
  group_by(carrier) %>%
  summarise(nr_active_planes = n(),
            mean_build_year = mean(year, na.rm=TRUE),
            unknown_planes = sum(is.na(year))) %>% left_join(airlines) %>%
    select(name, everything(), carrier)




flights_group <- group_by(flights, month)
flights_summ <- summarise(flights_group, 
                          flights_total = sum(!is.na(flight)),
                          flights_cancel = sum(is.na(dep_time|arr_time)), 
                          perc_canc = flights_cancel/flights_total*100) %>% 
  arrange(desc(perc_canc))


## Exercise 10
pivot_example <-readRDS("~/Documents/Data-Science-Business-Analytics/Data/pivot_example.Rds")

pivot_long <- pivot_example %>%
  pivot_longer(cols = c('female_married', 'male_married'),
               names_to = 'Marrital',
               values_to = 'Amount')
?pivot_longer










table_gathered <- table4a %>%
  pivot_longer(cols = c('1999','2000'), 
               names_to = 'Year',
               values_to = "Country")



                          