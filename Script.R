library(rugarch)
library(readxl)
library(ggplot2)
library(PerformanceAnalytics)
library(robustbase)

# Setting Work Directory

setwd ("/Users/lucasA/Desktop/Econométrie Financière/Econometrie-Financiere")

DataSP500 <- read.csv("Data/SP500.CSV")[, c(1, 5, 6)] # Importing Data

DataSP500$Date <- as.Date(DataSP500$Date) # Changing The Date Column to Date format

DataSP500[, c(2, 3)] <- log(DataSP500[, c(2, 3)]) # Switching to Log Prices

ggplot(data = DataSP500, aes(x = Date, y = Adj.Close))+ # Plotting the series
  geom_line(color = 'navyblue')+
  theme_minimal()

R <- diff(DataSP500[, 3]) # Computing Returns (Log(Pt) - Log(Pt-1))

DataSP500 <- DataSP500[-c(1), ] # Removing 1 row because of 1st difference

DataSP500$R <- R 
DataSP500$Vol <- R^2 # Computing Volatility

DataSP500 <- as.ts(DataSP500) # Converting to TS format for Outliers Detection

Clean_DataSP500 <- Return.clean(DataSP500, method = 'boudt') # Removing Outliers

Clean_DataSP500 <- as.data.frame(Clean_DataSP500)
DataSP500 <- as.data.frame(DataSP500) # Switching back to DFs (enabling ggplot2 to function)

ggplot(Clean_DataSP500, aes(x = Date, y = R))+
  geom_line(data = DataSP500, aes(x = Date, y = R), color = 'red')+
  geom_line(color = 'navyblue')

ggplot(data = DataSP500, aes(x = Date, y = R))+ # Plotting Returns
  geom_line(color = 'navyblue')+
  geom_line(aes(y = Vol), color = 'red')+
  theme_minimal()

ggplot(data = DataSP500, aes(x = Date, y = Vol))+ # Plotting Volatility
  geom_line(color = 'red')+
  theme_minimal()

GARCHspec <- ugarchspec(variance.model = list(model = 'sGARCH', garchOrder = c(1, 1)), distribution.model = 'norm')
GARCHfit <- ugarchfit(GARCHspec, data = DataSP500$Vol) # Standard GARCH(1, 1) spec and fit

iGARCHspec <- ugarchspec(variance.model = list(model = 'iGARCH', garchOrder = c(1, 1)), distribution.model = 'norm')
iGARCHfit <- ugarchfit(iGARCHspec, data = DataSP500$Vol) # Integrated GARCH(1, 1) spec and fit 

eGARCHspec <- ugarchspec(variance.model = list(model = 'eGARCH', garchOrder = c(1, 1)), distribution.model = 'norm')
eGARCHfit <- ugarchfit(eGARCHspec, data = DataSP500$Vol) # Exponential GARCH(1, 1) spec and fit 

Prediction <- data.frame()[1 : 3974, ] # Grouping results in a nex "prediction" data frame 

Prediction$sGARCH <- GARCHfit@fit$sigma
Prediction$iGARCH <- iGARCHfit@fit$sigma
Prediction$eGARCH <- eGARCHfit@fit$sigma
Prediction$Date <- DataSP500$Date
Prediction$Vol <- DataSP500$Vol

ggplot(data = Prediction, aes(x = Date, y = sGARCH))+ # PLotting Series and GARCH(1, 1) and iGARCH(1, 1) estimates
  geom_line(aes(y = Vol), color = 'navyblue')+ # and eGARCH(1, 1) estimates
  geom_line(color = 'red')+
  geom_line(aes(y = iGARCH), color = 'yellow')+
  geom_line(aes(y = eGARCH), color = 'green', lty = 'dotted')
  theme_minimal()
  
  # end


