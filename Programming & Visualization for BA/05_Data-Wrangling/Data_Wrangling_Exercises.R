# ---------------------------
# Erasmus Q-Intelligence B.V.
# ---------------------------

library("tidyverse")
library("nycflights13")

data("flights")

# --------------
# Data wrangling
# --------------

# ------
# filter
# ------

flights_arr_delay <- filter(flights, arr_delay >= 120 & dep_delay <= 0)

flights_UA_AA_DL <- filter(flights, carrier %in% c("UA","AA","DL"))
flights_UA_AA_DL <- filter(flights, carrier =="UA" | carrier == "AA" | carrier == "DL")

# -------
# arrange
# -------

flights_arranged <- arrange(flights, desc(arr_delay))

flights_arranged <- arrange(flights, !is.na(tailnum))

# ------
# select
# ------

flights_time_delay <- select(flights, ends_with("_time") | ends_with("_delay"))
flights_time_delay <- select(flights, ends_with(c("_time", "_delay")))

# ------
# mutate
# ------

flights_time <- mutate(flights, 
                       hours = floor(dep_time/100),
                       minutes = dep_time - hours*100,
                       dep_time_mins = hours*60 + minutes)
flights_time <- select(flights_time, -hours, -minutes)

# ----------------------
# summarise and group_by
# ----------------------

flights_grouped <- group_by(flights, month)
flights_summary <- summarise(flights_grouped, 
                             canceled_flights = sum(is.na(dep_time) | is.na(arr_time)),
                             count_flights = n(),
                             perc_canceled = canceled_flights / count_flights)
flights_summary <- arrange(flights_summary, desc(perc_canceled))

flights_grouped <- group_by(flights, dest)
flights_summary <- summarise(flights_grouped,
                             min = min(air_time, na.rm = TRUE),
                             max = max(air_time, na.rm = TRUE),
                             mean = mean(air_time, na.rm = TRUE),
                             median = median(air_time, na.rm = TRUE),
                             sd = sd(air_time, na.rm = TRUE))
flights_summary <- arrange(flights_summary, desc(mean))

# ---------------------------
# group_by, filter and mutate
# ---------------------------

lagged_delays <- flights %>%
  group_by(origin) %>%
  arrange(month, day, dep_time) %>%
  mutate(dep_delay_lag = lag(dep_delay)) 

lagged_delays <- lagged_delays %>%
  filter(!is.na(dep_delay), !is.na(dep_delay_lag))

lagged_delays <- lagged_delays %>%
  group_by(origin, dep_delay_lag) %>%
  summarise(dep_delay_avg = mean(dep_delay)) 

lagged_delays %>% 
  ggplot(aes(y = dep_delay_avg, x = dep_delay_lag, color = origin)) + 
  geom_point(alpha = 1/3) +
  scale_x_continuous(breaks = seq(0, 1500, by = 120)) + 
  scale_y_continuous(breaks = seq(0, 1500, by = 120)) + 
  labs(y = "Departure Delay", x = "Previous Departure Delay")

# --------
# combined
# --------

# load data
forsale <- readRDS('../../data/forsale.Rds')

# select
forsale_10obs <- forsale %>% 
  select(postcode, city, suburb, asking_price, living_area) %>%
  head(n = 10)
# top_n(10)
# slice(1:10)

# mutate
forsale <- forsale %>%
  mutate(price_m2 = asking_price/living_area)

# group_by and summarise
forsale_by_city <- forsale %>%
  group_by(city) %>%
  summarise(number_houses = n(),
            price_min = min(asking_price, na.rm = TRUE),
            price_avg = mean(asking_price, na.rm = TRUE),
            price_max = max(asking_price, na.rm = TRUE)) %>%
  arrange(price_avg)

# group_by and mutate
forsale <- forsale %>%
  group_by(pc_numeric) %>%
  mutate(price_premium = price_m2 / mean(price_m2, na.rm = TRUE)) %>%
  ungroup()

# filter
forsale_filter <- forsale %>%
  filter(city == 'Rotterdam', bedrooms >= 3, bathrooms >= 2, asking_price <= 400000)

forsale <- forsale %>%
  mutate(x = bathroom*2)

forsale %<>%
  mutate(x = bathroom*2)

# arrange
forsale_filter <- forsale_filter %>%
  arrange(desc(price_premium))

# ------
# across
# ------

# load data
penguins <- readRDS('../../data/penguins.Rds')

# n_distinct for all
distinct_all <- penguins %>%
  summarise(across(.fns = n_distinct))

# n_distinct for selection
distinct_select <- penguins %>%
  summarise(across(c(species, island, sex), n_distinct))

# missing values for factors
missing_factor <- penguins %>%
  summarise(across(where(is.factor), ~sum(is.na(.))))

# missing values for 'length'
missing_length <- penguins %>%
  summarise(across(contains('length'), ~sum(is.na(.))))

# bonus
relative_mm <- penguins %>%
  group_by(species) %>%
  mutate(across(contains('mm'), ~./mean(body_mass_g, na.rm = TRUE)))

# -----
# joins
# -----

airport_locations <- airports %>% select(faa, lat, lon)

flights %>%
  select(year:day, hour, origin, dest) %>% 
  left_join(airport_locations, by = c("origin" = "faa"))

plane_info <- planes %>% select(tailnum, year)

q9b <- flights %>%
  select(carrier, tailnum) %>% 
  distinct() %>% 
  left_join(plane_info) %>%
  group_by(carrier) %>% 
  summarize(nr_active_planes = n(),
            mean_build_year = mean(year, na.rm = TRUE),
            unknown_planes = sum(is.na(year))) %>% left_join(airlines) %>%
  select(name, everything(), -carrier)

# -----
# tidyr
# -----

## 1
# data
pivot_example <- readRDS('../../data/pivot_example.Rds')

pivot_example_longer <- pivot_example %>% 
  tidyr::pivot_longer(cols = -paygrade, names_to = 'gender_marital', values_to = 'amount')

## 2
nba <- readRDS('../../data/nba.Rds')

nba_longer <- nba %>%
  rename(`1` = day1points, `2` = day2points) %>%
  tidyr::pivot_longer(c(`1`, `2`), names_to = 'day', values_to = 'points')
