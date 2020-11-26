setwd('/Users/lucasA/Desktop/Econométrie Financière/Econometrie-Financiere/TD')

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

data <- zoo(read_excel('brent1.xlsx'))

FinTS.stats(data)

basicStats(data)

data = ts(data)

summary(data)

SemiDeviation(data)

JarqueBeraTest(data)

Box.test(data, lag = 10, type = c('Ljung-Box'))
Box.test(data^2, lag = 10, type = c('Ljung-Box'))

ArchTest(data^2,lag = 10)

par(mfrow= c(2, 2))
  acf(data, main = 'acf data')
  pacf(data)
  acf(data^2)
  pacf(data^2)

h <- hist(data, col = 'navyblue')
xfit <- seq(min(data), max(data), length = 40) 
yfit <- dnorm(xfit, mean = mean(data), sd = sd(data)) 
yfit <- yfit * diff(h$mids[1:2]) * length(data) 
lines(xfit, yfit, col = "red", lwd = 2)

data <- Return.clean(data, method = 'boudt')

h <- hist(data, col = 'navyblue')
xfit <- seq(min(data), max(data), length = 40) 
yfit <- dnorm(xfit, mean = mean(data), sd = sd(data)) 
yfit <- yfit * diff(h$mids[1:2]) * length(data) 
lines(xfit, yfit, col = "red", lwd = 2)

GARCHspec <- ugarchspec(variance.model = list(model = 'sGARCH'), mean.model = list(armaOrder = c(0, 0), include.mean = TRUE), distribution.model = 'norm')
GARCHfit <- ugarchfit(GARCHspec, data = data) # Standard GARCH(1, 1)
GARCHfit

persistence(GARCHfit)

halflife(GARCHfit)

estim <- 1000 #nb jours 2015 - 2018
h <- 260 #nb jours 2019
statmat <- matrix(nrow=h, ncol=1)

for(i in 1:h){
  data2 <- data[1:(estim-1+i),1]
  GARCHfit = ugarchfit(data = data2, spec = GARCHspec)
  forc =  ugarchforecast(GARCHfit, n.ahead=1)
  statmat[i,1] <- sigma(forc)^2
  # statmat[i,1] <- rbind(sigma(forc))
}
statmat

# Writing the results to output file, change the output file name
write(t(statmat),file="forecast_sGARCH.txt",ncolumn=1,append=FALSE)

foremat <- matrix(nrow = h, ncol = 1)
varmat <- matrix(nrow = h, ncol = 1)
esmat <- matrix(nrow = h, ncol = 1)

for(i in 1:h){
  data2 <- data[1:(estim-1+i),1]
  GARCHfit = ugarchfit(data = data2, spec = GARCHspec)
  forc =  ugarchforecast(GARCHfit, n.ahead=1)
  statmat[i,1] <- sigma(forc)^2
  varmat[i, 1] <- qnorm(0.05) * sigma(forc)
  esmat[i, 1] <- - dnorm(qnorm(0.05)/0.05) * sigma(forc)
  # statmat[i,1] <- rbind(sigma(forc))
}

foremat
varmat
mean(varmat)
esmat
mean(esmat)

write(t(foremat),file="Risk_forecast.txt",ncolumn=1,append=FALSE)
write(t(foremat),file="VaR.txt",ncolumn=1,append=FALSE)
write(t(esmat),file="Exp_Short.txt",ncolumn=1,append=FALSE)

alpha = 0.05
testdata <- data[c(1001 : 1260), ]
test <- ts(testdata)
var <- ts(varmat)
backtest <- BacktestVaR(data = test, VaR = var, alpha = alpha)
show(backtest$LRcc)




