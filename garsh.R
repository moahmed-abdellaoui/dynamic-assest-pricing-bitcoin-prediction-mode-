library(rugarch)
library(xts)
library(PerformanceAnalytics)
library(quantmod)
library(readxl)
library(tseries)

BTC_data <- read_excel("C:/Users/okban/OneDrive/Documents/project/bitcoin_data.xlsx")
BTC_ts <-  subset(BTC_data, select= -c(date))
BTC_ts <- xts(BTC_ts, order.by = as.Date(BTC_data$date))

return <- CalculateReturns(BTC_ts$price)
return <- tail(return, -1)
chart_Series(return^2)

kpss.test(return)

chart.Histogram(return, method = c('add.density', 'add.normal'), colorset=c('blue', 'red', 'black'))
legend("topright", legend = c("return", "kernel", "normal dist"), fill = c('blue', 'red', 'black'))
sd(return)

mod_specify <- ugarchspec(variance.model = list(model = "eGARCH", garchOrder = c(4,4)),
                          mean.model = list(armaOrder = c(5,0)),
                          distribution.model = "sstd")
mod_fitting <- ugarchfit(mod_specify, data = return, solver.control = list(trace=0))
mod_fitting
n_periods <- 8

# Use ugarchforecast to generate forecast
forecast <- ugarchforecast(fitORspec = mod_fitting, n.ahead = n_periods)
vol_forecast <- as.numeric(sigma(forecast))

# View the forecast
forecast


chartSeries(BTC_ts, theme = "white")
plot(mod_fitting, which="all")
plot(vol_forecast, type = "l", xlab = "Weeks", ylab = "Volatility")