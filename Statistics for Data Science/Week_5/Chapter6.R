#install following packages only once, then comment out code
#install.packages(c("stargazer","ggplot2","gridExtra","broom","car","lmtest","sandwich","interplot","ape"),dependencies=TRUE)

# display regression results from Anscombe quartet
a1 <- lm(y1 ~ x1, data=anscombe)
a2 <- lm(y2 ~ x2, data=anscombe)
a3 <- lm(y3 ~ x3, data=anscombe)
a4 <- lm(y4 ~ x4, data=anscombe)

# report Anscombe quartet regression results
library(stargazer)
stargazer(a1, a2, a3, a4, type="text", no.space=TRUE,
          model.names=FALSE, notes="standard errors in parentheses")

# load two packages used to data visualization
library(ggplot2)
library(gridExtra)
# generate four scatterplots with regression lines
F1 <- ggplot(anscombe)+aes(x1,y1)+geom_point()
+geom_abline(intercept=3,slope=0.5)
F2 <- ggplot(anscombe)+aes(x2,y2)+geom_point()
+geom_abline(intercept=3,slope=0.5)
F3 <- ggplot(anscombe)+aes(x3,y3)+geom_point()
+geom_abline(intercept=3,slope=0.5)
F4 <- ggplot(anscombe)+aes(x4,y4)+geom_point()
+geom_abline(intercept=3,slope=0.5)
# display four scatter plots together
grid.arrange(F1,F2,F3,F4, ncol = 2)


# remove all objects from workspace
rm(list=ls(all=TRUE))
# change working directory to project folder
#setwd("C:/Project")

# load R data
load("final.85.RData")

# load packages
library(ggplot2)
library(gridExtra)
library(stargazer)

# produce formatted descriptive statistics in pwt7.final
stargazer(final.85, type="text", median = TRUE)
# estimate OLS model and create output object
model1 <- lm(logy ~ open + loglab + logland, data=final.85)
# show model output
summary(model1)

# load package
library(broom)
# tidy regression output
tidy(model1)
# add additional statistics to original data
final.85v2 <- augment_columns(model1, final.85)
# show variables in new data
names(final.85v2)
# produce descriptive statistics for new data
stargazer(final.85v2, type="text",
          median = TRUE)
# load packages
library(ggplot2)
library(gridExtra)
# residuals against fitted values: check linearity
ggplot(final.85v2, aes(x=.fitted, y=.resid)) +
  geom_hline(yintercept=0) +
  geom_point() +
  geom_smooth(method='loess', se=TRUE)

# residuals against trade
ropen <- ggplot(final.85v2, aes(x=open, y=.resid)) +
  geom_hline(yintercept=0) +
  geom_point() +
  geom_smooth(method='loess', se=TRUE)
# residuals against land
rland <- ggplot(final.85v2, aes(x=logland, y=.resid)) +
  geom_hline(yintercept=0) +
  geom_point() +
  geom_smooth(method='loess', se=TRUE)
# residuals against labor
rlab <- ggplot(final.85v2, aes(x=loglab, y=.resid)) +
  geom_hline(yintercept=0) +
  geom_point() +
  geom_smooth(method='loess', se=TRUE)
# display plots together
grid.arrange(ropen, rland, rlab, ncol=3)
# regression specification error test

# expanded model
model1.q1 <- lm(logy ~ open + loglab + logland + I(.fittedˆ2),
                data=final.85v2)
# F test of model difference
anova(model1, model1.q1)
# estimate a curvilinear model for trade
model1.q2 <- lm(logy ~ open + loglab + logland + I(openˆ2),
                data=final.85v2)

# F test of model difference
anova(model1, model1.q2)
summary(model1.q2)
# scatter plot of open and log of real income per capita

ggplot(final.85v2, aes(x=open, y=logy))+
  geom_point() +
  geom_vline(xintercept = 76.129) +
  geom_hline(yintercept = 8.256) +
  geom_text(aes(label=country),hjust=0,vjust=0)

# create a variable perfectly correlating with open
final.85v2$open4 <- 4*final.85v2$open
# check correlation between two variables
cor(final.85v2$open, final.85v2$open4, use="complete.obs")
# estimate model 1 adding open4 in two different orderings
lm(logy  ~ open + open4 + loglab + logland, data=final.85v2)
lm(logy  ~ open4 + open + loglab + logland, data=final.85v2)

# compute vif statistics
library(car)
vif(model1)
# test heteroskedasticity
# estimate OLS model and create output object
model1 <- lm(logy ~ open + loglab + logland, data=final.85v2)
# Cook/Weisberg score test of constant error variance
library(car)
ncvTest(model1)

# Breush/Pagan test of constant error variance
library(lmtest)
bptest(model1)

# weighted least squares
model1.wls <- lm(logy  ~ open+loglab+logland,
                 weights=1/open, data=final.85v2)
summary(model1.wls)

# load packages
library(lmtest)
library(sandwich)

# report default HC3 robust standard errors
model1.hc3 <- coeftest(model1, vcov=vcovHC)  
model1.hc3
# report HC1 robust standard errors as Stata
# variants:"HC3","HC","HC0","HC1","HC2","HC4","HC4m","HC5"
model1.hc1 <- coeftest(model1, vcov=vcovHC(model1, type="HC1"))
model1.hc1
# request the variance-covariance matrix HC3
vcovHC(model1, type="HC3")
# request the variance-covariance matrix HC1
vcovHC(model1, type="HC1")
# distribution of residuals in each region
ggplot(final.85v2, aes(.fitted, .resid)) +
  geom_hline(yintercept=0) +
  geom_point() +
  facet_wrap( ~ continent)
# scatter plot of trade and income by region
ggplot(final.85, aes(open, logy)) +
  geom_point() +
  facet_wrap(  ~ continent) +
  stat_smooth(method = "lm", se = FALSE)
# scatter plot open and logy by region
ggplot(final.85, aes(open, logy)) +
  geom_point() +
  facet_wrap(  ~ continent) +
  stat_smooth(method = "lm", se = FALSE)
# show attributes
attributes(final.85v2$continent)
# change value labels of factor variable continent
levels(final.85v2$continent) <- c(".Africa", ".C.N.America",
                                  ".S.America", ".Asia", ".Europe", ".Oceania")
# confirm the change
attributes(final.85v2$continent)

# model with all regional dummies and no intercept
lm(logy  ~ 0 + continent + open + loglab + logland,
   data=final.85v2)
# display model with all regional dummies but one
lm(logy  ~ continent + open + loglab + logland, data=final.85v2)
# display full model ouput
model1.reg <- lm(logy  ~ continent + open + loglab + logland,
                 data=final.85v2)
# display model output
summary(model1.reg)
# compare against original model
anova(model1, model1.reg)
# interaction model
model1.int <- lm(logy  ~ open*continent + loglab + logland,
                 data=final.85v2)
# compare models
anova(model1, model1.int)
anova(model1.reg, model1.int)
# display output
summary(model1.int)
# load package
library(interplot)
# plot effect of trade conditional on region
interplot(model1.int, var1="open", var2="continent") +
  geom_hline(yintercept = 0, linetype = "dashed")

# load packages
library(lmtest)
library(multiwayvcov)
# estimate OLS model and create output object
model1 <- lm(logy  ~ open + loglab + logland, data=final.85v2)
# show OLS results for comparison
summary(model1)
# request clustered variance and covariance matrix
vcov_region <- cluster.vcov(model1, final.85v2$continent)
# display clustered matrix
vcov_region
# request test statistics and p values
model1.cl <- coeftest(model1, vcov_region)
model1.cl

# load package
library(car)

# influence plot for influential observations
influencePlot(model1)
# create observation id
final.85v2$id <- as.numeric(row.names(final.85v2))
# load library
library(ggplot2)

# identify obs with Cook's D above cutoff
ggplot(final.85v2, aes(id, .cooksd)) +
  geom_bar(stat="identity", position="identity") +
  xlab("Obs. Number")+ylab("Cook's distance")+
  geom_hline(yintercept=0.03) +
  geom_text(aes(label=ifelse((.cooksd>0.03),id,"")),
            vjust=-0.2, hjust=0.5)
# list observations whose cook's D above threshold
final.85v2[final.85v2$.cooksd>0.03, c("id","country","logy",
                                      "open",".std.resid",".hat",".cooksd")]
# re-estimate model 1 without Singapore
model1.no1 <- lm(logy  ~ open + loglab + logland,
                 data=final.85v2[final.85v2$.cooksd<0.18,])
summary(model1.no1)

# re-estimate model 1 without Singapore and five others
model1.no2 <- lm(logy  ~ open + loglab + logland,
                 data=final.85v2[final.85v2$.cooksd<0.03,])
summary(model1.no2)
# normality diagnostic plot
library(car)
qqPlot(model1, distribution="t", simulate=TRUE)
# normality test
shapiro.test(final.85v2$.resid)
# test log-transformed open variable
model1.no3 <- lm(logy  ~ log(open) + loglab + logland,
                 data=final.85v2)
# normality test
shapiro.test(residuals(model1.no3))
# show model output
summary(model1.no3)
# robustness checks I
stargazer(model1,model1.q2,model1.wls,model1.hc1,model1.hc3,
          model1.no1, model1.no2, type="text", no.space=TRUE,
          covariate.labels = NULL, label="",
          omit.stat=c("f","ser"), model.names=FALSE,
          dep.var.labels.include=FALSE, dep.var.caption="",
          column.labels=c("OLS","Quadratic","WLS","Robust.hc1",
                          "Robust.hc3", "Singapore","outliers")
)
# robustness checks II
stargazer(model1.cl, model1.reg, model1.int, model1.no3,
          type="text", no.space=TRUE, covariate.labels = NULL,
          label="", omit.stat=c("f","ser"), model.names=FALSE,
          dep.var.labels.include=FALSE, dep.var.caption="",
          column.labels = c("cluster.se", "regions",
                            "interaction", "logopen")
)