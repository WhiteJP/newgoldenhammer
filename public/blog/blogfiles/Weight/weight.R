library(readr)
library(tidyverse)
library(quantreg)

weight <- read_csv("weight.csv", col_types = cols(`Date` = col_date(format = "%d/%m/%Y")))
weight$ndays <- as.integer(weight$Date - weight$Date[1] + 1)
weight

# models##
linear.model <- lm(Kg ~ ndays, data = weight)
log.model <- lm(log(Kg) ~ ndays, data = weight)
exp.model <- lm(Kg ~ exp(-ndays), data = weight)
power.model <-lm(log(Kg) ~ log(ndays), data = weight) # which is same as log-log model -- see http://bulldog2.redlands.edu/fac/eric_hill/Phys233/Lab/LabRefCh10%20Power%20Law%20and%20Log.pdf
#nls.model <- 

##non-linear least squares fit

#graph 
log.model.df <- data.frame(ndays = weight$ndays,
                           Kg.log = exp(fitted(log.model)))

pow.model.df <- data.frame(ndays = weight$ndays, 
                           Kg.pow = exp(fitted(power.model)))

ggplot(weight, aes(x = ndays, y = Kg)) + 
  geom_point() +
  geom_smooth(method = "lm", aes(color = "Linear Model"),
              se = FALSE) +
  geom_smooth(method ="lm", aes(color = "Exp Model"),
              formula = (y ~ exp(-x)), se= FALSE , linetype = 1) +
  geom_line(data = log.model.df, aes(ndays, Kg.log, color = "Log Model"),
            size = 1, linetype = 2) + 
  geom_line(data = pow.model.df, aes(ndays, Kg.pow, color = "Power Model"),
            size = 1, linetype = 2) +   
  geom_smooth(method = 'nls', formula = (y~a*x^b), 
              method.args = list(start = c(a = 80,b = -1)),se = FALSE, aes(color = 'nls')) +
  guides(color = guide_legend("Model Type"))

### different linear regression methods ### 
#gls 

library(nlme)
gls.regression <- gls(Kg ~ ndays, data = weight)
summary(gls.regression)

gls.regression.log <- gls(log(Kg) ~ ndays, data = weight)
summary(gls.regression.log)
str(gls.regression)

# https://www.r-bloggers.com/linear-regression-with-correlated-data/
#need to add auto-correlation structure. 

dayvalues <- seq(0, 90, by = 1)
log.model.values <- exp(predict(log.model, list(ndays=dayvalues)))
plot(log.model.values)

lin.modelvalues <- predict(linear.model, list(ndays=dayvalues))

lin.modelvalues
