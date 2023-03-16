# install.packages("fitdistrplus")
library(fitdistrplus)

# Load data
data(groundbeef)
data <- groundbeef$serving

# Simple plots
plot(data, pch=20)
hist(data)
hist(data, breaks = 30)
hist(data, breaks = 100)
plotdist(data, histo = TRUE, demp = TRUE)

# Fit normal distribution
fit_n <- fitdist(data, "norm")
fit_n
summary(fit_n)
denscomp(fit_n, addlegend=FALSE)
qqcomp(fit_n, addlegend=FALSE)
plot(fit_n, demp = TRUE)

# Formal test
stats <- gofstat(fit_n, fitnames="normal")
stats
stats$kstest

# Fit some other distributions
fit_w  <- fitdist(data, "weibull")
fit_g  <- fitdist(data, "gamma")
fit_ln <- fitdist(data, "lnorm")

#par(mfrow=c(2,2))
plot.legend <- c("Normal", "Weibull", "lognormal", "gamma")
denscomp(list(fit_n, fit_w, fit_g, fit_ln), legendtext = plot.legend)
cdfcomp (list(fit_n, fit_w, fit_g, fit_ln), legendtext = plot.legend)
qqcomp  (list(fit_n, fit_w, fit_g, fit_ln), legendtext = plot.legend)
ppcomp  (list(fit_n, fit_w, fit_g, fit_ln), legendtext = plot.legend)

#
descdist(data)
descdist(data, boot = 1000)

print(c("AIC normal =",fit_n$aic))
print(c("AIC weibull =",fit_w$aic))
print(c("AIC gamma =",fit_g$aic))
print(c("AIC lnorm =",fit_ln$aic))

b_g <- bootdist(fit_g, bootmethod="nonparam", niter=500) 
summary(b_g)
#quantile(b1b)
plot(b_g)







