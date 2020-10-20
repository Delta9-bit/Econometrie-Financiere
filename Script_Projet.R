setwd("/Users/Lucas/Desktop/Cours/Econometrie-Financiere")

library(readxl)
library(zoo)
library(fBasics)
library(FinTS)
library(PerformanceAnalytics)
library(ggplot2)
library(DescTools)
library(robustbase)
library(rugarch)
library(GAS)

data <- zoo(diff(log(as.numeric(read.csv("Data/AF.PA.csv")[, c(6)]))))
data_df <- data.frame('Series' = diff(log(as.numeric(read.csv("Data/AF.PA.csv")[, c(6)])), 'index' = seq (1 : length(data))))

FinTS.stats(data)

basicStats(data)

data <- ts(data)

summary(data)

SemiDeviation(data)

JarqueBeraTest(data)

Box.test(data, lag = 10, type = c('Ljung-Box'))
Box.test(data^2, lag = 10, type = c('Ljung-Box'))

ArchTest(data^2, lag = 10)

ggplot(aes(x = index, y = Series), data = data_df)+
  geom_line(color = 'navyblue')+
  theme_minimal()

par(mfrow = c(2 ,2))
  acf(data)
  pacf(data)
  acf(data^2)
  pacf(data^2)
  
h <- hist(data, col = 'navyblue')
xfit <- seq(min(data), max(data), length = 40)
yfit <- dnorm(xfit, mean = mean(data), sd = sd(data))
yfit <- yfit * diff(h$mids[1 : 2]) * length(data)
lines(xfit, yfit, col = 'red', lwd = 2)

data <- Return.clean(data, method = 'boudt')

sGARCH_spec <- ugarchspec(variance.model = list(model = 'sGARCH'), 
                         mean.model = list(armaOrder = c(0, 0), include.mean = TRUE), distribution.model = 'norm')
sGARCH_fit <- ugarchfit(spec = sGARCH_spec, data = data)
sGARCH_fit

persistence(sGARCH_fit)

halflife(sGARCH_fit)

estim <- 1000 
h <- 260
statmat <- matrix(nrow = h, ncol = 1)
foremat <- matrix(nrow = h, ncol = 1)
VaRmat <- matrix(nrow = h, ncol = 1)
ESmat <- matrix(nrow = h, ncol = 1)

for(i in 1:h){
  data_pred = data[1 : (estim - 1 + i), 1]
  sGARCH_fit <- ugarchfit(data = data_pred, spec = sGARCH_spec)
  forecast <- ugarchforecast(sGARCH_fit, n.ahead = 1)
  statmat[i, 1] <- sigma(forecast)^2
  VaRmat[i, 1] <- qnorm(0.05) * sigma(forecast)
  ESmat[i, 1] <- dnorm(qnorm(0.05) / 0.05) * sigma(forecast)
}

statmat
VaRmat
ESmat

mean(VaRmat)
mean(ESmat)

write(t(statmat), file = "sGARCH_forecast.txt", ncolumn = 1, append = TRUE)
write(t(VaRmat), file = "VaR_forecast.txt", ncolumn = 1, append = TRUE)
write(t(ESmat), file = "ES_forecast.txt", ncolumn = 1, append = TRUE)

data_test <- ts(data[c(1001 : 1260), ])
VaR_forecast <- ts(VaRmat)
backtest <- BacktestVaR(data = data_test, VaR = VaR_forecast, alpha = 0.05)
show(backtest$LRcc)

data_test_df <- data.frame(data_test, seq (1 : length(data_test)))
VaR_forecast_df <- data.frame(VaR_forecast, seq (1 : length(data_test)))
data_test_df <- merge(data_test_df, VaR_forecast_df)
colnames(data_test_df) <- c('index', 'data_test', 'VaR')

ggplot(data = data_test_df, aes(x = index))+
  geom_line(aes(y = data_test), color = 'navyblue')+
  geom_line(aes(y = VaR), color = 'red')+
  theme_minimal()







