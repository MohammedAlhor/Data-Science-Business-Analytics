## 1. House Prices
## Exercise 1.1
## a.
# Inladen data
forsale <- readRDS("~/Data-Science-Business-Analytics/Programming & Visualization for BA/Data/forsale.Rds")

## b.

View(forsale)
## This represents the data for a house including describing attributes
## 
colnames(forsale)
# [1] "postcode"      "pc_numeric"    "pc_character"  "city"          "suburb"        "latitude"      "longitude"     "asking_price"  "offered_since"
# [10] "living_area"   "available_now" "type_build"    "volume"        "nr_rooms"      "build_year"    "bedrooms"      "bathrooms"     "alarm"        
# [19] "jacuzzi"       "sauna"         "bath"
## I can only make assumptions on this.
## The data consists of numeric, characters, integers

## c.

nrow(forsale)
## Dataset consists of 3383 rows.
ncol(forsale)
## Dataset consists of 21 columns.

## d.

summary(forsale)

## This summary provides the following insights:
# First off the asking_price variable has a significant outlier considering the mean is roughly 280000 euros
# Secondly, living area has the same issue.

## Exercise 1.2
## a.

mean(forsale$asking_price)
## the mean asking price is 283791.5 euros

median(forsale$asking_price)
## b.
## the median asking price is 199500 which is lower than the mean, becasue of this we can draw the conlusion
## that there is probably outliers in the dataset. Asking price is probably skewed to the right.

## c.
min(forsale$asking_price)
## 59500
max(forsale$asking_price)
## 4500000
range(forsale$asking_price)
## the range lies beteween these two

## d.
## the range we calculated previously
sd(forsale$asking_price)
## the standard deviation is 277895

## e.
## these take on character values
table(forsale$city)
table(forsale$available_now)
table(forsale$type_build)
table(forsale$bedrooms)
## the amount of houses that have 5 rooms is 235, the amount that have 6 is 64
