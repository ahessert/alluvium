ggplot(data = train, aes(x=ts_lag_diff)) + geom_density()

plot(train[, .(sin(pi*hour(ts)/12),watt_lead_avg)], pch = 20)
plot(train[, .(cos(pi*hour(ts)/12),watt_lead_avg)], pch = 20)

plot(train[355:, .(sin(pi*windDirection/180),watt_avg)], pch = 20)
plot(train[355:, .(cos(pi*windDirection/180),watt_avg)], pch = 20)

plot(train[rpm==0, .(minute=.I,watts)], pch = 20)
plot(train[, .(.I,watts)], pch = 20)
plot(train[,.(I=.I%/%10, watts)][, mean(watts), by=I], pch = 20)
plot(train[,.(hourlyPrecip,relativeHumidity,rpm,watts,wetBulbFahrenheit,windDirection,windSpeed)], pch=20)
