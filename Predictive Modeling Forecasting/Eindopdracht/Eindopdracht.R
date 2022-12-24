library(dplyr)
# Inladen data
# russia_losses_equipment_correction.csv
# russia_losses_equipment.csv
# russia_losses_personnel.csv

russia_losses_equipment <- read_csv("russia_losses_equipment.csv")

plot(russia_losses_equipment$aircraft)

plot(russia_losses_personnel$personnel)
# first differences, moet eruit gehaald worden

russia_losses_equipment$aircraft_lag <- lag(russia_losses_equipment$aircraft)
russia_losses_equipment$aircraft_dif <- russia_losses_equipment$aircraft - russia_losses_equipment$aircraft_lag
plot(russia_losses_equipment$aircraft_dif)


plot(russia_losses_personnel$personnel)

