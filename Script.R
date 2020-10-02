library(rugarch)
library(readxl)
library(ggplot2)

setwd ("/Users/lucasA/Desktop/Econométrie Financière/Econometrie-Financiere")

DataSP500 <- read.csv("Data/SP500.CSV")[, c(1, 5,6)]

DataSP500$Date <- as.Date(DataSP500$Date)

head(DataSP500)

DataSP500[, c(2, 3)] <- log(DataSP500[, c(2, 3)])

ggplot(data = DataSP500, aes(x = Date, y = Adj.Close))+
  geom_line(color = 'navyblue')+
  theme_minimal()

R <- diff(DataSP500[, 3])

DataSP500 <- DataSP500[-c(1), ]

DataSP500$R <- R 
DataSP500$Vol <- R^2

ggplot(data = DataSP500, aes(x = Date, y = R))+
  geom_line(color = 'navyblue')+
  geom_line(aes(y = Vol), color = 'red')+
  theme_minimal()
