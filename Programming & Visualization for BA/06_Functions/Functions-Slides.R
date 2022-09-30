# ---------------------------
# Erasmus Q-Intelligence B.V.
# ---------------------------

# -----------
# my function 
# -----------

my_function <- function(param_1, param_2 = 0) {
    som <- param_1 + param_2
    product <- param_1 * param_2
    list(output_1 = som, output_2 = product)
}
my_function(param_1 = 1, param_2 = 2)
my_function(1, 2)
my_function(param_1 = 1)
my_function(param_1 = 1, param_2 = 2)
my_function(param_2 = 2)

# -------------
# my function 2 
# -------------

# load packages
library(tidyverse)

# objects in parent environment
years <- 3
column_name <- "years"
mt_cars_subset <- mtcars %>%
    select(hp, cyl, mpg, wt) %>%
    slice(2, 3)

my_function2 <- function(row_name = "age") {
    years <- 9
    tibble(type = row_name,
           age = years)
}
my_function2()
my_function2("bla")

# -------------
# my function 3 
# -------------

# objects in parent environment
years <- 3
column_name <- "years"

my_function3 <- function(row_name = "age") {
    years <<- 9
    tibble(type = row_name,
           measure = column_name,
           age = years)
}
my_function3()

# -----------
# Reusability
# -----------
library(dplyr)
# repeating code
library(nycflights13)
data("flights")
flights %>%
    group_by(month) %>%
    summarize(mean_arr_delay = mean(arr_delay, na.rm = TRUE))
flights %>%
    group_by(carrier) %>%
    summarize(mean_arr_delay = mean(arr_delay, na.rm = TRUE))
flights %>%
    group_by(origin) %>%
    summarize(mean_arr_delay = mean(arr_delay, na.rm = TRUE))
flights %>%
    group_by(dest) %>%
    summarize(mean_arr_delay = mean(arr_delay, na.rm = TRUE))

# reusable function
get_delay_by <- function(group) {
    flights %>%
        group_by({{group}}) %>%
        summarise(mean_arr_delay = mean(arr_delay, na.rm = TRUE))
}

delay_by_month <- get_delay_by(month)
delay_by_carrier <- get_delay_by(carrier)
delay_by_origin <- get_delay_by(origin)
delay_by_dest <- get_delay_by(dest)
