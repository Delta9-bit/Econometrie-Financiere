library(rugarch)
library(readxl)
library(ggplot2)
library(PerformanceAnalytics)
library(robustbase)
library(tidyverse)
library(GAS)
library(DescTools)
library(FinTS)
library(zoo)
library(fBasics)
library(readxl)
library(tsoutliers)
library(Hmisc)
library(rugarch)
library(quantmod)
library(DescTools)
library(multDM)
library(MCS)

#L'Ã©tude porte sur le cours du WTI du 1/01/2015 au 31/12/2019

baseWTI=read_csv(file.choose(),col_names=T)
View(baseWTI) 
str(baseWTI)
##on garde juste la colonne date close et adj close
baseWTI=baseWTI[,c(1,2,3)]

#on recode les variables
baseWTI$Date <- as.Date(baseWTI$Date, format = '%d/%m/%Y') # Changing The Date Column to Date format
baseWTI <- baseWTI[order(baseWTI$Date), ]

##Evolution des prix

ggplot(data = baseWTI, aes(x = Date, y = Dernier)) + 
  geom_line(color = "#00AFBB", size = 1)+
  theme_minimal()

##on va travailler sur les rentabilitÃ©s

rtWTI <- as.ts(diff(log(baseWTI$Dernier), lag = 1) * 100)

baseWTI <- baseWTI[- length(baseWTI$Dernier), ]

#On ajoute au DataFrame

baseWTI$rtWTI <- rtWTI

# On graphe les rentabilités

ggplot(data = baseWTI, aes(x = Date, y = rtWTI)) + 
  geom_line(color = "#00AFBB", size = 0.1)+
  theme_minimal()

# Stats descriptives des rentabilités

statWTI=data.frame(basicStats(rtWTI*100))
statWTI

#2) dÃ©tection des outliers

clean.rtWTI <- Return.clean(baseWTI$rtWTI,method = "boudt")
baseWTI$clean.rtWTI <- as.numeric(clean.rtWTI)

#3)Graphique sÃ©rie brute et corrigÃ©e pour rentabilitÃ©s
ggplot(data = baseWTI)+
  geom_line(aes(x = Date, y = rtWTI), size = 0.1, color = 'red') +
  geom_line(aes(x = Date, y = clean.rtWTI), size = 0.1, color = "#00AFBB")+
  ylab("Rentabilités")+
  theme_minimal()

#Pour rentabilitÃ©s au carrÃ© 
ggplot(data = baseWTI)+
  geom_line(aes(x = Date, y = rtWTI^2), size = 0.1, color = 'red') +
  geom_line(aes(x = Date, y = clean.rtWTI^2), size = 0.1, color = "#00AFBB") +
  ylab("Rentabilités²")+
  theme_minimal()

#4) Graphique des corrÃ©lations des rentabilitÃ©s et des rentabilitÃ©s au carrÃ© sur sÃ©rie corrigÃ©e
par(mfrow = c(2, 2))

acf(baseWTI$clean.rtWTI, main="Returns ACF")
pacf(baseWTI$clean.rtWTI, main="Returns PACF")
acf(baseWTI$clean.rtWTI^2, main="Squared returns ACF")
pacf(baseWTI$clean.rtWTI^2, main="Squared returns PACF")

rcorr(baseWTI$rtWTI,baseWTI$rtWTI^2)

#5) 6) Stats descriptives sur sÃ©rie corrigÃ©e et analyse de la distribution
statWTI=data.frame(basicStats(rtWTI*100))
statWTI

sdev = SemiDeviation(baseWTI$rtWTI)
show(sdev)
sdev = SemiDeviation(baseWTI$rtWTI*100)
show(sdev)

# Jarque-Bera normality test with DescTools package

JBtest = JarqueBeraTest(rtWTI.ts, robust = FALSE, method = "chisq")
show(JBtest)

# Autocorrelation Returns
autocorrTest = Box.test(baseWTI$rtWTI, lag = 10, type = "Ljung-Box", fitdf = 0)
show(autocorrTest)

#heteroscedasticitÃ© conditionnelle
condheteroTest = Box.test(baseWTI$rtWTI^2, lag = 10, type = "Ljung-Box", fitdf = 0)
show(condheteroTest)

# Histogramme des rentabilitÃ©s
sd = sd(baseWTI$clean.rtWTI)
mean = mean(baseWTI$clean.rtWTI)

ggplot(data = baseWTI)+
  geom_density(aes(x = clean.rtWTI), color = '#00AFBB')+
  stat_function(fun = dnorm, args = list(mean = mean, sd = sd), color = 'red')+
  xlab('Rendements')+
  theme_minimal()

########ESTIMATION VOLATILITE

#Duplication de la base
data <- baseWTI

# Année 2019
baseWTI_forecast <- baseWTI[1056:1318, ]

#???Années 2015-2018
baseWTI <- baseWTI[1:1055, ]

#7) Tableau des rentabilitÃ©s avec les deux distributions (Normale et Student)

#loi normale 

GARCHspec <- ugarchspec(variance.model = list(model = 'sGARCH', garchOrder = c(1, 1)), 
                        mean.model = list(armaOrder = c(0, 0), include.mean = TRUE), distribution.model = 'norm')
GARCHfit <- ugarchfit(GARCHspec, data = baseWTI$clean.rtWTI) # Standard GARCH(1, 1) spec and fit
GARCHfit
halflife(GARCHfit)

iGARCHspec <- ugarchspec(variance.model = list(model = 'iGARCH', garchOrder = c(1, 1)), 
                         mean.model = list(armaOrder = c(0, 0), include.mean = TRUE), distribution.model = 'norm')
iGARCHfit <- ugarchfit(iGARCHspec, data = baseWTI$clean.rtWTI) # Integrated GARCH(1, 1) spec and fit 
iGARCHfit
halflife(iGARCHfit)

eGARCHspec <- ugarchspec(variance.model = list(model = 'eGARCH', garchOrder = c(1, 1)),
                         mean.model = list(armaOrder = c(0, 0), include.mean = TRUE), distribution.model = 'norm')
eGARCHfit <- ugarchfit(eGARCHspec, data = baseWTI$clean.rtWTI) # Exponential GARCH(1, 1) spec and fit 
eGARCHfit
halflife(eGARCHfit)

gjrGARCHspec <- ugarchspec(variance.model = list(model = 'gjrGARCH', garchOrder = c(1, 1)),
                           mean.model = list(armaOrder = c(0, 0), include.mean = TRUE), distribution.model = 'norm')
gjrGARCHfit <- ugarchfit(gjrGARCHspec, data = baseWTI$clean.rtWTI) # GJR GARCH(1, 1) spec and fit 
gjrGARCHfit
halflife(gjrGARCHfit)

tGARCHspec <- ugarchspec(variance.model = list(model = 'tGARCH', garchOrder = c(1, 1)),
                         mean.model = list(armaOrder = c(0, 0), include.mean = TRUE), distribution.model = 'norm')
tGARCHfit <- ugarchfit(tGARCHspec, data = baseWTI$clean.rtWTI) # Threshold GARCH(1, 1) spec and fit 
tGARCHfit
halflife(tGARCHfit)

RISMspec = ugarchspec(variance.model=list(model = "iGARCH"),
                      mean.model=list(armaOrder=c(0,0), include.mean=TRUE), distribution.model = 'norm',
                      fixed.pars=list(omega=0,alpha1=0.06,beta1=0.94))
RISMfit = ugarchfit(data = baseWTI$clean.rtWTI, spec = RISMspec) # RiskMetrics
RISMfit
halflife(RISMfit)

#Loi de Student 

GARCHspec_st <- ugarchspec(variance.model = list(model = 'sGARCH', garchOrder = c(1, 1)), 
                           mean.model = list(armaOrder = c(0, 0), include.mean = TRUE), distribution.model = 'std')
GARCHfit_st <- ugarchfit(GARCHspec_st, data = baseWTI$clean.rtWTI) # Standard GARCH(1, 1) spec and fit
GARCHfit_st
halflife(GARCHfit_st)

iGARCHspec_st <- ugarchspec(variance.model = list(model = 'iGARCH', garchOrder = c(1, 1)), 
                            mean.model = list(armaOrder = c(0, 0), include.mean = TRUE), distribution.model = 'std')
iGARCHfit_st <- ugarchfit(iGARCHspec_st, data = baseWTI$clean.rtWTI) # Integrated GARCH(1, 1) spec and fit
iGARCHfit_st
halflife(iGARCHfit_st)

eGARCHspec_st <- ugarchspec(variance.model = list(model = 'eGARCH', garchOrder = c(1, 1)),
                            mean.model = list(armaOrder = c(0, 0), include.mean = TRUE), distribution.model = 'std')
eGARCHfit_st <- ugarchfit(eGARCHspec_st, data = baseWTI$clean.rtWTI) # Exponential GARCH(1, 1) spec and fit
eGARCHfit_st
halflife(eGARCHfit_st)

gjrGARCHspec_st <- ugarchspec(variance.model = list(model = 'gjrGARCH', garchOrder = c(1, 1)),
                              mean.model = list(armaOrder = c(0, 0), include.mean = TRUE), distribution.model = 'std')
gjrGARCHfit_st <- ugarchfit(gjrGARCHspec_st, data = baseWTI$clean.rtWTI) # GJR GARCH(1, 1) spec and fit
gjrGARCHfit_st
halflife(gjrGARCHfit_st)

tGARCHspec_st <- ugarchspec(variance.model = list(model = 'tGARCH', garchOrder = c(1, 1)),
                            mean.model = list(armaOrder = c(0, 0), include.mean = TRUE), distribution.model = 'std')
tGARCHfit_st <- ugarchfit(tGARCHspec_st, data = baseWTI$clean.rtWTI) # Threshold GARCH(1, 1) spec and fit
tGARCHfit_st
halflife(tGARCHfit_st)

RISMspec_st = ugarchspec(variance.model=list(model = "iGARCH"),
                      mean.model=list(armaOrder=c(0,0), include.mean=TRUE), distribution.model = 'std',
                      fixed.pars=list(omega=0,alpha1=0.06,beta1=0.94))
RISMfit_st = ugarchfit(data = baseWTI$clean.rtWTI, spec = RISMspec_st)
RISMfit_st
halflife(RISMfit_st)

#8) PrÃ©visions de la volatilitÃ© à partir des modÃ¨les validÃ©s pour 2019

estim <- 1055 
h <- 262

baseWTI_forecast$iGARCH <- baseWTI_forecast$clean.rtWTI
baseWTI_forecast$eGARCH <- baseWTI_forecast$clean.rtWTI
baseWTI_forecast$RISM <- baseWTI_forecast$clean.rtWTI

rm(iGARCHfit_st, eGARCHfit_st, RISMfit_st)

for(i in 1:h){
  data_pred <- data.frame('clean.rtWTI' = data[1 : (estim - 1 + i), 5])
 
  iGARCHfit_st <- ugarchfit(data = data_pred, spec = iGARCHspec_st)
  eGARCHfit_st <- ugarchfit(data = data_pred, spec = eGARCHspec_st)
  RISMfit_st <- ugarchfit(data = data_pred, spec = RISMspec_st)
  baseWTI_forecast$iGARCH[i] <- ugarchforecast(iGARCHfit_st, n.ahead = 1)@forecast$sigmaFor
  baseWTI_forecast$eGARCH[i] <- ugarchforecast(eGARCHfit_st, n.ahead = 1)@forecast$sigmaFor
  baseWTI_forecast$RISM[i] <- ugarchforecast(RISMfit_st, n.ahead = 1)@forecast$sigmaFor
  
  print(i)
}



# Plots
ggplot(data = baseWTI_forecast, aes(x = Date))+
  geom_line(aes(y = clean.rtWTI^2), color = '#00AFBB')+
  geom_line(aes(y = iGARCH^2), color = 'red')+
  geom_line(aes(y = eGARCH^2), color = 'green')+
  geom_line(aes(y = RISM^2), color = 'purple')+
  ylab("Volatilité")+
  theme_minimal()

# MSE
for(j in 6:8){
  error <- 0
  k <- 0
  for(i in 1:length(baseWTI_forecast$Date)){
    k <- (1 / 263) * (baseWTI_forecast[i, 5] - baseWTI_forecast[i, j])^2
    error <- error + k
  }
  print(error)
}

# DM test
DM.test(baseWTI_forecast$eGARCH, baseWTI_forecast$iGARCH, baseWTI_forecast$clean.rtWTI, H1="more")
DM.test(baseWTI_forecast$eGARCH, baseWTI_forecast$RISM, baseWTI_forecast$clean.rtWTI, H1="more")
DM.test(baseWTI_forecast$RISM, baseWTI_forecast$iGARCH, baseWTI_forecast$clean.rtWTI, H1="more")

# MDM test
reel <- baseWTI_forecast$clean.rtWTI
forecasts <- baseWTI_forecast[, c(6, 7, 8)]
forecasts <- t(forecasts)

MDM.test(reel, forecasts, q = 2)

# MCS
loss_eGARCH <- LossVol(realized = baseWTI_forecast$clean.rtWTI, baseWTI_forecast$eGARCH)
loss_iGARCH <- LossVol(realized = baseWTI_forecast$clean.rtWTI, baseWTI_forecast$iGARCH)
loss_RISM <- LossVol(realized = baseWTI_forecast$clean.rtWTI, baseWTI_forecast$RISM)

losses <- cbind(loss_eGARCH, loss_iGARCH, loss_RISM)

MCSprocedure(alpha = 0.05, losses)

# VaR
VaRmat <- matrix(nrow = h + 1, ncol = 3)
ESmat <- matrix(nrow = h + 1, ncol = 3)

for(i in 1:length(baseWTI_forecast$iGARCH)){
  VaRmat[i, 1] <- qnorm(0.05) * baseWTI_forecast$iGARCH[i]
  VaRmat[i, 2] <- qnorm(0.05) * baseWTI_forecast$eGARCH[i]
  VaRmat[i, 3] <- qnorm(0.05) * baseWTI_forecast$RISM[i]
  
  ESmat[i, 1] <- - dnorm(qnorm(0.05))/ 0.05 * baseWTI_forecast$iGARCH[i]
  ESmat[i, 2] <- - dnorm(qnorm(0.05)) / 0.05 * baseWTI_forecast$eGARCH[i]
  ESmat[i, 3] <- - dnorm(qnorm(0.05)) / 0.05 * baseWTI_forecast$RISM[i]
}

VaRmat <- data.frame(VaRmat)
VaR <- cbind(VaRmat, baseWTI_forecast$clean.rtWTI, baseWTI_forecast$Date)

colnames(VaR) <- c('IGARCH', 'EGARCH', 'RISM', 'REEL', 'DATE')

ggplot(data = VaR, aes(x = DATE))+
  geom_line(aes(y = REEL), color = '#00AFBB')+
  geom_line(aes(y = IGARCH), color = 'red')+
  ylab('VaR')+
  ggtitle('VaR avec IGARCH')+ # IGARCH
  theme_minimal()

ggplot(data = VaR, aes(x = DATE))+
  geom_line(aes(y = REEL), color = '#00AFBB')+
  geom_line(aes(y = EGARCH), color = 'red')+
  ylab('VaR')+
  ggtitle('VaR avec EGARCH')+ # EGARCH
  theme_minimal()

ggplot(data = VaR, aes(x = DATE))+
  geom_line(aes(y = REEL), color = '#00AFBB')+
  geom_line(aes(y = RISM), color = 'red')+
  ylab('VaR')+
  ggtitle('VaR avec RiskMetrics')+ # RiskMetrics
  theme_minimal()

# Exceptions

VaR$E_RISM <- c(0)
VaR$E_EGARCH <- c(0)
VaR$E_IGARCH <- c(0)

for(i in 1:length(VaR$REEL)){
  if(VaR$REEL[i] < VaR$IGARCH[i]){
    VaR$E_IGARCH[i] <- 1}

  if(VaR$REEL[i] < VaR$EGARCH[i]){
    VaR$E_EGARCH[i] <- 1}

  if(VaR$REEL[i] < VaR$RISM[i]){
    VaR$E_RISM[i] <- 1}
  
  print(i)
}

mean(VaR$IGARCH)
mean(VaR$EGARCH)
mean(VaR$RISM)

mean(ESmat[, 1]) # moyenne des alpha pires pertes prévues
mean(ESmat[, 2])
mean(ESmat[, 3])

hit_IGARCH <- sum(VaR$E_IGARCH == 1) # nombre de Hits
hit_EGARCH <- sum(VaR$E_EGARCH == 1)
hit_RISM <- sum(VaR$E_RISM == 1)

mean(VaR$E_IGARCH) # Doit être égal à 0.05 si H0 converture non condi valide
mean(VaR$E_EGARCH)
mean(VaR$E_RISM)

# fonction pour calculer Zuc

compute_Zuc <- function(t, N, alpha){
  Zuc = (N - alpha * t) / (sqrt(alpha * t * (1 - alpha)))
  print(Zuc)
  if(Zuc > 1.96){
    print(paste("Zuc =", Zuc,", E[It] > alpha -> VaR sous éstimée"))
  }else if(Zuc < -1.96){
    print(paste("Zuc =", Zuc,",E[It] < alpha -> VaR sur éstimée"))
  }else{
    print(paste("Zuc =", Zuc,", E[It] = alpha -> VaR correcte"))
    }
}

t = length(VaR$IGARCH)
alpha = 0.05

compute_Zuc(t, hit_IGARCH, alpha)
compute_Zuc(t, hit_EGARCH, alpha)
compute_Zuc(t, hit_RISM, alpha)

# Backtesting
BacktestVaR(VaR$REEL, VaR$IGARCH, alpha = 0.05)
BacktestVaR(VaR$REEL, VaR$EGARCH, alpha = 0.05)
BacktestVaR(VaR$REEL, VaR$RISM, alpha = 0.05)
