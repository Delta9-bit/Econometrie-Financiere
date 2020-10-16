setwd('/Users/lucasA/Desktop')

library(readxl)
library(zoo)
library(fBasics)
library(FinTS)
library(PerformanceAnalytics)
library(ggplot2)
library(DescTools)

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

