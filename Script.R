library(rugarch)
library(readxl)

setwd ("/Users/lucasA/Desktop/Econométrie Financière")

DataSP500 <- read.csv("Data/SP500.CSV")[, c(1, 5,6)]

head(DataSP500)

