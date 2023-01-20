#install.packages("dbplyr")

library(dplyr)
# Open een verbinding met een lokale database
con <- DBI::dbConnect(RSQLite::SQLite(), dbname = ":memory:")

# Kopieer de nycflights13::flights tabel naar de database 
copy_to(con, nycflights13::flights, "flights",
        temporary = FALSE, 
        indexes = list(
          c("year", "month", "day"), 
          "carrier", 
          "tailnum",
          "dest"
        )
)

# Maak een referentie naar de flights tabel in de database
flights_db <- tbl(con, "flights")

# Een paar dbplyr queries: let op het onbekend aantal rijen en andere database info in print
flights_db %>% select(year:day, dep_delay, arr_delay)

flights_db %>% filter(dep_delay > 240)

flights_db %>% 
  group_by(dest) %>%
  summarise(delay = mean(dep_time))



# Deze query wordt pas op het eind naar de database gestuurd
tailnum_delay_db <- flights_db %>% 
  group_by(tailnum) %>%
  summarise(
    delay = mean(arr_delay),
    n = n()
  ) %>%
  filter(n > 100) %>%
  arrange(desc(delay))

# Dit is de SQL vertaling:
tailnum_delay_db %>% show_query()

# Haal alle data op met collect()
tailnum_delay <- tailnum_delay_db %>% collect()
tailnum_delay

#nrow en tail werken niet goed
nrow(tailnum_delay_db)
tail(tailnum_delay_db)


