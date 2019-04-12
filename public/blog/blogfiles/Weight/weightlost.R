library(readr)
library(tidyverse)
library(quantreg)

weight <- read_csv("weight.csv", col_types = cols(`Date` = col_date(format = "%d/%m/%Y")))
weight$ndays <- as.integer(weight$Date - weight$Date[1])
weight

weightlost <- weight$Kg[1] - weight$Kg[2:nrow(weight)]
wl_df <- data.frame(weightlost, days = weight$ndays[2:nrow(weight)])

ggplot(wl_df, aes(x = days, y = weightlost)) + geom_point() +
  scale_x_log10() + geom_smooth(method = "lm")

log.model.wl <- lm(weightlost ~ log(days), data = wl_df)
