library(quantreg)

qr <- rq(Kg ~ ndays, data = weight, tau = seq(0.1, 0.9, by = 0.1))
plot(qr)

ggplot(weight, aes(x = ndays, y = Kg)) +
  geom_point() +
  geom_quantile(quantiles = seq(0.1, 0.9, by = 0.1))