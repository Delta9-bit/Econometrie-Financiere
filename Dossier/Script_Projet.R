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

# importing data

data <- zoo(diff(log(as.numeric(read.csv("Data/WTI.csv")[, c(2)]))))
data_df <- data.frame('series' = as.numeric(read.csv("Data/WTI.csv")[, c(2)]))
data_df$index <- seq(1, nrow(data_df), 1)
returns <- diff(log(data_df$series))
data_df <- data_df[- c(1),]
data_df$returns <- returns
data_ts <- ts(data_df[3])

# Outliers removal

data_clean <- Return.clean(data_ts, method = 'boudt')
data_df$clean <- as.numeric(data_clean)

rm(returns)

# Series plots

ggplot(aes(x = index), data = data_df)+
  geom_line(aes(y = returns),color = 'red')+
  geom_line(aes(y = clean), color = 'steelblue')
  theme_minimal()
  
ggplot(mapping = aes(x = index, y = series), data_df)+
  geom_line(color = 'steelblue')+
  theme_minimal()

ggplot(mapping = aes(x = index), data_df)+
  geom_line(aes(y = returns^2),color = 'steelblue')+
  geom_line(aes(y = clean^2),color = 'red')+
  theme_minimal()

# ACF/PACF plots
par(mfrow = c(2 ,2))
acf(data_df$returns, main = 'acf returns')
pacf(data_df$returns, main = 'pacf returns')
acf(data_df$returns^2, main = 'acf returns^2')
pacf(data_df$returns^2, main = 'pacf returns^2')
dev.off()

# Summary statisitcs

FinTS.stats(data)

basicStats(data)

summary(data_ts)

SemiDeviation(data_ts)

JarqueBeraTest(data_ts)

Box.test(data_ts, lag = 10, type = c('Ljung-Box'))
Box.test(data_ts^2, lag = 10, type = c('Ljung-Box'))

ArchTest(data_ts^2, lag = 10)

h <- hist(data, col = 'navyblue')
xfit <- seq(min(data), max(data), length = 40)
yfit <- dnorm(xfit, mean = mean(data), sd = sd(data))
yfit <- yfit * diff(h$mids[1 : 2]) * length(data)
lines(xfit, yfit, col = 'red', lwd = 2)

# GARCH models with norm and st distributions

GARCHspec <- ugarchspec(variance.model = list(model = 'sGARCH', garchOrder = c(1, 1)), 
                      mean.model = list(armaOrder = c(0, 0), include.mean = TRUE), distribution.model = 'norm')
GARCHspec_st <- ugarchspec(variance.model = list(model = 'sGARCH', garchOrder = c(1, 1)), 
                        mean.model = list(armaOrder = c(0, 0), include.mean = TRUE), distribution.model = 'std')
GARCHfit <- ugarchfit(GARCHspec, data = data) # Standard GARCH(1, 1) spec and fit
GARCHfit <- ugarchfit(GARCHspec_st, data = data)

iGARCHspec <- ugarchspec(variance.model = list(model = 'iGARCH', garchOrder = c(1, 1)), 
                        mean.model = list(armaOrder = c(0, 0), include.mean = TRUE), distribution.model = 'norm')
iGARCHspec_st <- ugarchspec(variance.model = list(model = 'iGARCH', garchOrder = c(1, 1)), 
                         mean.model = list(armaOrder = c(0, 0), include.mean = TRUE), distribution.model = 'std')
iGARCHfit <- ugarchfit(iGARCHspec, data = data) # Integrated GARCH(1, 1) spec and fit 
iGARCHfit <- ugarchfit(iGARCHspec_st, data = data)

eGARCHspec <- ugarchspec(variance.model = list(model = 'eGARCH', garchOrder = c(1, 1)),
                        mean.model = list(armaOrder = c(0, 0), include.mean = TRUE), distribution.model = 'norm')
eGARCHspec_st <- ugarchspec(variance.model = list(model = 'eGARCH', garchOrder = c(1, 1)),
                         mean.model = list(armaOrder = c(0, 0), include.mean = TRUE), distribution.model = 'std')
eGARCHfit <- ugarchfit(eGARCHspec, data = data) # Exponential GARCH(1, 1) spec and fit 
eGARCHfit <- ugarchfit(eGARCHspec_st, data = data)

gjrGARCHspec <- ugarchspec(variance.model = list(model = 'gjrGARCH', garchOrder = c(1, 1)),
                           mean.model = list(armaOrder = c(0, 0), include.mean = TRUE), distribution.model = 'norm')
gjrGARCHspec_st <- ugarchspec(variance.model = list(model = 'gjrGARCH', garchOrder = c(1, 1)),
                           mean.model = list(armaOrder = c(0, 0), include.mean = TRUE), distribution.model = 'std')
gjrGARCHfit <- ugarchfit(gjrGARCHspec, data = data) # GJR GARCH(1, 1) spec and fit 
gjrGARCHfit <- ugarchfit(gjrGARCHspec_st, data = data)

tGARCHspec <- ugarchspec(variance.model = list(model = 'tGARCH', garchOrder = c(1, 1)),
                         mean.model = list(armaOrder = c(0, 0), include.mean = TRUE), distribution.model = 'norm')
tGARCHspec_st <- ugarchspec(variance.model = list(model = 'tGARCH', garchOrder = c(1, 1)),
                          mean.model = list(armaOrder = c(0, 0), include.mean = TRUE), distribution.model = 'std')
tGARCHfit <- ugarchfit(tGARCHspec, data = data) # Threshold GARCH(1, 1) spec and fit 
tGARCHfit <- ugarchfit(tGARCHspec_st, data = data)

riskMetspec <- ugarchspec(variance.model=list(model = "iGARCH"),
                        mean.model=list(armaOrder=c(0,0), include.mean=TRUE), fixed.pars=list(omega=0,alpha1=0.06,beta1=0.94))
riskMetfit <- ugarchfit(riskMetspec, data = data)

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






