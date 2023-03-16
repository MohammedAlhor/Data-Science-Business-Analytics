# Opdracht in de klas
# Bereken checktijd per pakketjes, admin tijd per pakketje en de totale doorlooptijd van het pakketje.

parcels <- read_excel("Data/parcel processing data clean-crossed pakketjes.xlsx", sheet = "Data", skip=1) %>% drop_na()


check_time <- working_hours(parcels[1,]$`Aangepast Begin checken`, parcels[1,]$`Eind checken`)
admin_time  <- working_hours(parcels[1,]$`Begin admin`, parcels[1,]$`Eind admin`)
total_time <- check_time + admin_time

i <- 1
for (row in 1:nrow(parcels)) 
{

check_time[row] <- working_hours(parcels[row,]$`Aangepast Begin checken`, parcels[row,]$`Eind checken`)
admin_time[row] <- working_hours(parcels[row,]$`Begin admin`, parcels[row,]$`Eind admin`)
total_time[row] <- check_time[row] + admin_time[row]
}

total_time

# fitdistrplus maakt het mogelijk om te schatten wat de beste verdeling is en schat de parameters
# discrete data and continuous data


data("danishuni")
view(danishuni)
# Simple plots
plot(danishuni$Loss, pch=20)
hist(danishuni$Loss)
hist(danishuni$Loss, breaks = 30)
hist(danishuni$Loss, breaks = 100)
plotdist(log(danishuni$Loss), histo = TRUE, demp = TRUE)


fit_ln <- fitdist(danishuni$Loss, "lnorm")


#par(mfrow=c(2,2))
plot.legend <- c("Normal", "Weibull", "lognormal", "gamma")
denscomp(list(fit_ln), legendtext = plot.legend)
cdfcomp (list(fit_n, fit_w, fit_g, fit_ln), legendtext = plot.legend)
qqcomp  (list(fit_n, fit_w, fit_g, fit_ln), legendtext = plot.legend)
ppcomp  (list(fit_n, fit_w, fit_g, fit_ln), legendtext = plot.legend)