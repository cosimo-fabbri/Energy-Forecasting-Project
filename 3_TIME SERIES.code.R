rm(list=ls()) 
# 1 - LIBRARIES ----
# list of library needed
librerie_da_installare <- c("tidyverse", "readxl", "gridExtra", "tseries", "lmtest", "forecast", "urca", "rugarch")
#Install missing libraries
for (libreria in librerie_da_installare) {
  if (!requireNamespace(libreria, quietly = TRUE)) {
    install.packages(libreria, dependencies = TRUE)
    library(libreria, character.only = TRUE)
  }
}

library(tidyverse)
library(readxl)
library(gridExtra)
library(tseries)
library(lmtest)#test for residuals
library(forecast)
library(urca)   #for the p-values of the Dicket-Fuller test
library(rugarch)  #for Garch model

rm(list=ls()) 
# 2 - IMPORT DATASET ----
#set wd in the same folder of R-script
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
energy <- read_excel("energy.xlsx")
summary(energy)

#transform the date column in a date datatype
energy$Date <-as.Date(energy$Date)

#we have 4 years of data <<< we should have 365*4 = 1460 observations
#in this case we have 1461 because there is a 29th February on 2016,we remove it
energy<- energy[-425,]
dim(energy)
head(energy)

# 3 - EXPLORATORY DATA ANALYSIS ON FULL TIME SERIES ----


##summary of Price##
summary(energy)
#the days go from 2015 to 2018
#price goes from 14.48 to 99, the mean and the median are close

##TIme series plot of Price##
ggplot(energy, aes(x = Date, y = Price)) +
  geom_line() +
  xlab("") +
  ylab("Price") +
  ggtitle("Energy Price Time Series")+
  theme_light()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

#ACF AND PACF
par(mfrow=c(1,2))
acf(energy$Price, main="ACF Price")
pacf(energy$Price, main="PACF Price")
#the acf is persistent and there seems to be some seasonal pattern
#the fact that it is persistent maybe indicates that there 
#is an autoregressive component
#the pacf has the first lag significant, the 2nd not, and then there 
#are some significant ones 

#try with 100 lags
acf(energy$Price, main="ACF Price", lag=100)
pacf(energy$Price, main="PACF Price", lag=100)

#ACF and PACF of differences
acf(diff(energy$Price), main="ACF Price", lag=30)
pacf(diff(energy$Price), main="PACF Price", lag=30)

acf(diff(energy$Price), main="ACF Price", lag=100)
pacf(diff(energy$Price), main="PACF Price", lag=100)
#with the differences the acf loses its persistency, let's then 
#check if the series is stationary



## Time series decomposition##
price <- ts(data = energy$Price, frequency = 365)# Price as a time series
plot(stl(price, s.window = "periodic"))
#from here there seems only to be some seasonality (no trend)

##Let's check some seasonal statistics##
energy$month <- format(energy$Date, "%m")#add month columns
#plot to see if there are monthly differences
par(mfrow=c(1,1))
boxplot(energy$Price~energy$month)
#from here there seems to be seasonal variance, so variance may be not constant
#there seems to be a yearly pattern: the price goes down form january to april
#and then has an increasing pattern till december

#table to look at the monthly statistical differences
stat_price <- energy%>%
  group_by(month) %>%
  summarise(Mean = mean(Price),
            Median = median(Price),
            Max = max(Price),
            Min = min(Price),
            Var = var(Price))
stat_price

#Plots of monthly statistics
par(mfrow=c(2,3))
plot(x=stat_price$month, y = stat_price$Mean, type="l", main = "Monthly Mean", xlab="",ylab="")
plot(x=stat_price$month, y = stat_price$Median, type="l", main = "Monthly Median", xlab="",ylab="")
plot(x=stat_price$month, y = stat_price$Max, type="l", main = "Monthly Maximum", xlab="",ylab="")
plot(x=stat_price$month, y = stat_price$Min, type="l", main = "Monthly Minimum", xlab="",ylab="")
plot(x=stat_price$month, y = stat_price$Var, type="l", main = "Monthly Variance", xlab="",ylab="")

#the plots confirms a decreasing pattern for the mean/median/min/max between
#January and April and an increasing one from April to December for 
#mean/median/min
#fall of the max between January and February, and then it slightly increases 
#(this may be because of the change in price in 2017)
#last for the variance there is a decreasing pattern with some spikes


##Add seasonal dummy variables to the dataset to takes into account different 
#seasonal factors##

#let's create monthly dummies
mat1 <- matrix(rep(0,365*12*4), ncol=12)
i<-1
while (i<13){
  for (mese in unique(energy$month)){
    mat1[,i]<-ifelse(energy$month == mese, 1, 0)
    i = i +1
  }
}

mat1 <- as.data.frame(mat1)

colnames(mat1)[1] <- "January"
colnames(mat1)[2] <- "February"
colnames(mat1)[3] <- "March"
colnames(mat1)[4] <- "April"
colnames(mat1)[5] <- "May"
colnames(mat1)[6] <- "June"
colnames(mat1)[7] <- "July"
colnames(mat1)[8] <- "August"
colnames(mat1)[9] <- "September"
colnames(mat1)[10] <- "October"
colnames(mat1)[11] <- "November"
colnames(mat1)[12] <- "December"
energy.months <- cbind(energy, mat1)#add months dummies in original dataframe




#let's create trimestri dummies
#starting from January
#jan - Mar
#Apr - Jun
#Jul - Sep
#Oct - Dec

energy.trim <- energy.months
energy.trim$TRIM1 <- energy.trim$January + energy.trim$February + energy.trim$March
energy.trim$TRIM2 <- energy.trim$April + energy.trim$May + energy.trim$June
energy.trim$TRIM3 <- energy.trim$July + energy.trim$August + energy.trim$September
energy.trim$TRIM4 <- energy.trim$October + energy.trim$November + energy.trim$December

#remove months
energy.trim <- energy.trim[,-c(4:15)]



#let's create Quadrimestri dummy
#Starting from February
#Feb- May
#Jun - Sep
#Oct - Jan

energy.quad <- energy.months
energy.quad$QUAD1 <- energy.quad$February + energy.quad$March + energy.quad$April + energy.quad$May
energy.quad$QUAD2 <- energy.quad$June + energy.quad$July + energy.quad$August + energy.quad$September
energy.quad$QUAD3 <- energy.quad$October + energy.quad$November + energy.quad$December + energy.quad$January

#remove months
energy.quad <- energy.quad[,-c(4:15)]


#Let's create Semestre dummies
energy.sem <- energy.months
energy.sem$SEM1 <- energy.sem$February + energy.sem$March + energy.sem$April + 
  energy.sem$May+energy.sem$June + energy.sem$July
energy.sem$SEM2 <- energy.sem$August + energy.sem$September + energy.sem$October +
  energy.sem$November + energy.sem$December + energy.sem$January
#remove months
energy.sem <- energy.sem[,-c(4:15)]


#Let's create Weekly Dummies
#01/01/2015 was a tuesday
#29/02/2016 has been excluded, it was 425 obs
#week starts form monday with 1 (sunday with 7)

days <- rep(c(4, 5, 6, 7,  1, 2, 3), 60)#rep for 60*7 , 420
days <- c(days, c(4, 5, 6, 7, 2, 3))#we've create one week manually
days <- c(days, rep(c(4, 5, 6, 7,  1, 2, 3), 147))#147 weeks 
days <- c(days, c(4, 5, 6, 7,  1))#add last five days 

energy$Day <- days

energy.week <- energy
energy.week$Monday <- ifelse(energy.week$Day == 1, 1, 0)
energy.week$Tuesday <- ifelse(energy.week$Day == 2, 1, 0)
energy.week$Wednesday <- ifelse(energy.week$Day == 3, 1, 0)
energy.week$Thursday <- ifelse(energy.week$Day == 4, 1, 0)
energy.week$Friday <- ifelse(energy.week$Day == 5, 1, 0)
energy.week$Saturday <- ifelse(energy.week$Day == 6, 1, 0)
energy.week$Sunday <- ifelse(energy.week$Day == 7, 1, 0)

##Weekly boxplots##
par(mfrow=c(1,1))
boxplot(energy.week$Price ~ energy.week$Day)
#the variance seems the same between different days, but there seems to be 
#a slight decreasing pattern
stat_price.week <- energy.week%>%
  group_by(Day) %>%
  summarise(Mean = mean(Price),
            Median = median(Price),
            Max = max(Price),
            Min = min(Price),
            Var = var(Price))
stat_price.week
#variance is nearly the same (it increases a lot only on sunday)
#mean decreases significantly on the weekends



# 4 - TRAIN - TEST SPLIT ----
#train <<< 3 years
#test <<<< 1 year

x.is <- 1:(365*3)
x.oos <- (365*3+1):1460

energy.is <- energy[x.is,]
energy.oos <- energy[x.oos,]

price.is <- ts(energy.is$Price, start=2015, frequency = 365)
price.oos <- ts(energy.oos$Price, start=2018, frequency = 365)

# 5 - E.D.A. ON TRAIN DATA ----

##Let's check the plot for train set only##
#time series plot of Price
plot(energy.is$Date, energy.is$Price, type="l", xlab="Date", ylab="Energy Price (€)", 
     main="In-Sample Price 2015-2017")

#ACF and PACF
par(mfrow=c(1,2))
acf(energy.is$Price, main ="ACF In-Sample Price")
pacf(energy.is$Price, main ="PACF In-Sample Price")
#ACF still persistent
#PACF less significant than before (apart from the first lag) 
par(mfrow=c(1,1))


# 6 - STATIONARITY CHECK ----

##train time series decomposition##
decomposition <- stl(price.is, s.window = "periodic")
plot(decomposition)
#let's try to modeling seasonality

##To check for stationary we used Dicker fuller test, to do that  we used a 
#model like yt = costante + beta1 * yt-1 + epst
#e H0: beta1 = 1 vs H1:beta1 < 1
#test statistic = (beta1 - 1) /s.e.
#if REJECT then Stationary


fit.1 <- lm(price.is[2:1095] ~ price.is[1:1094] + as.matrix(energy.months[2:1095, c(5:15)]))
summary(fit.1)
#only a few months are significant
fit.2 <- lm(price.is[2:1095] ~ price.is[1:1094] + as.matrix(energy.trim[2:1095, c(5:7)]))
summary(fit.2)
#2 trimesters are not significant
fit.3 <- lm(price.is[2:1095] ~ price.is[1:1094] + as.matrix(energy.quad[2:1095, c(5:6)]))
summary(fit.3)
#model not bad
fit.4 <- lm(price.is[2:1095] ~ price.is[1:1094] + as.matrix(energy.sem[2:1095, 5]))
summary(fit.4)
#model not bad
fit.5 <- lm(price.is[2:1095] ~price.is[1:1094]+as.matrix(energy.week[2:1095, c(6:11)]))
summary(fit.5)
#all coefficients are significative and adj-R^2 greater then before 

plot(fit.5$residuals, type="l")
#Residual variance seems to vary over time
par(mfrow=c(1,2))
acf(fit.5$residuals,50)
pacf(fit.5$residuals,50)
#ACF loose his persistency
#PACF is better then before 

(fit.5$coefficients[2] - 1) / 0.01249 # 0.01249 is SE

#punitroot is used to compute pvalue of the test
punitroot(((fit.5$coefficients[2] - 1) / 0.01249),N = 1094, trend = "c")#only drift
#p-value is 0 so we reject H0 ==> STATIONARY

rm(list = c("fit.1", "fit.2", "fit.3", "fit.4", "fit.5"))

# 7 -  MODEL WITH DAILY SEASONALITY  ----
#fit a model of price on only the seasonal daily dummies
#and then look at acf and pacf to determine the best model

fit <- lm(price.is ~ as.matrix(energy.week[1:1095, c(6:11)]))
acf(fit$residuals)
#acf decreasing, it is persistent <<<< typical pattern of an autoregessive process
pacf(fit$residuals)
#maybe an AR1/AR5

#our process is stationary, we look at auto.arima to find the model with the best bic
auto.arima(price.is, xreg=as.matrix(energy.week[1:1095, c(6:11)]), 
           stationary = T,#we wante only stationary models
           ic = "bic")
#the function chooses ARIMA(5,0,0) (AR(5)), so we will look at it
#bic of 6500.35
rm(fit)


# 7.1 - ARIMA(5,0,0) WITH DAILY SEASONALITY ----
##estimating an ARIMA (5,0,0)
fit.ar5 <- Arima(price.is, order=c(5,0,0), 
                  xreg =  as.matrix(energy.week[1:1095, c(6:11)]))
summary(fit.ar5)


acf(fit.ar5 $residuals, lag=100)
pacf(fit.ar5 $residuals, lag=100)
par(mfrow=c(1,1))
plot(fit.ar5$residuals, type="l")
#there seems to be heteroskedasticity, maybe we can try ARCH/GARCH models

##FOorecasting##
#initialize the vector for the following for loop
train_price <- price.is
pred7.ahead.ar5 <- c()
lb7.ahead.ar5 <- c()
ub7.ahead.ar5 <- c()

#MY INTEREST IS ON A FORECAST 7-STEP AHEAD, AND OUR IDEA IS THAT EVERY 
#MONDAY WE UPDATE THE TRAIN WITH OBSERVED PRICE OF THE PREVIOUS WEEK 
#SO WE WANT TO FIND THE OPTIMAL MODEL CAPABLE OF DOING SO

# i goes form 1 to 52
for (i in 1:52) {
  
  model <- Arima(train_price, order=c(5,0,0), 
                 xreg = as.matrix(energy.week[1:(nrow(energy.is)+(i-1)*7), c(6:11)]))
  #train from 1 to 1095, then from 1 to 1095+7, then form 1 to 1095+14,...
  forecast <- forecast(model, h = 7, 
                       xreg=as.matrix(energy.week[(1096+7*(i-1)):(1102 + 7*(i-1)),c(6:11)]))
  #contrarly, test has to reduce at every iteration : 
  #1096+0 to 1102, 1096+7 to  1109,...
  
  #store the values of the current: forecast, upper and lower quantile
  values <- forecast$mean
  lb_values <- forecast$lower[,"95%"]
  ub_values <- forecast$upper[,"95%"]
  
  #update the initial vector with the forecast
  pred7.ahead.ar5 <- c(pred7.ahead.ar5, values)
  lb7.ahead.ar5 <- c(lb7.ahead.ar5, lb_values)
  ub7.ahead.ar5 <- c(ub7.ahead.ar5, ub_values)
  
  #update the train with the test
  train_price <- c(train_price, price.oos[(i*7-6):(i*7)])
  
}
#predicted values are 364, so we've to remove obs 365 from price.oos
mse.7ahead.ar5 <- mean((pred7.ahead.ar5-price.oos[-365])^2)
#mse7ahead with arima(5,0,0) is 39.39

##plotting the prediction of arima (5,0,0)
#point estimation
par(mfrow=c(1,1))
plot(energy$Date[-1460], energy$Price[-1460], type="l", 
     ylab="Price (€)", xlab="Date", main = "AR(5) model")
lines(energy.oos$Date[-365], pred7.ahead.ar5, col=2, lwd=1)
abline(v=energy.is$Date[1095],lty=2)
text(energy$Date[1300],95, "MSE: 39.39")
#prediction interval
par(mfrow=c(1,1))
plot(energy$Date[-1460], energy$Price[-1460], type="l", ylab="Price (€)", xlab="Date")
lines(energy.oos$Date[-365], lb7.ahead.ar5, col="blue", lwd=0.5)
lines(energy.oos$Date[-365], ub7.ahead.ar5, col="blue", lwd=0.5)
abline(v=energy.is$Date[1095],lty=2)

# 7.2 -  ARIMA(1,0,0) WITH DAILY DUMMIES ----
#fit an ARIMA (1,0,0)##
fit.ar1 <- Arima(price.is, order=c(1,0,0), 
                  xreg =  as.matrix(energy.week[1:1095, c(6:11)]))
summary(fit.ar1)
#greater bic then before 6582 vs 6500

acf(fit.ar1$residuals, lag=100)
pacf(fit.ar1$residuals, lag=100)
#first lag is still significant

##forecasting ARIMA (1,0,0)##

train_price <- price.is
pred7.ahead.ar1 <- c()
lb7.ahead.ar1 <- c()
ub7.ahead.ar1 <- c()

for (i in 1:52) {
  
  model <- Arima(train_price, order=c(1,0,0), 
                 xreg = as.matrix(energy.week[1:(nrow(energy.is)+(i-1)*7), c(6:11)]))

  forecast <- forecast(model, h = 7, 
                       xreg=as.matrix(energy.week[(1096+7*(i-1)):(1102 + 7*(i-1)),c(6:11)]))
  
  values <- forecast$mean
  lb_values <- forecast$lower[,"95%"]
  ub_values <- forecast$upper[,"95%"]
  
  pred7.ahead.ar1 <- c(pred7.ahead.ar1, values)
  lb7.ahead.ar1 <- c(lb7.ahead.ar1, lb_values)
  ub7.ahead.ar1 <- c(ub7.ahead.ar1, ub_values)
  
  train_price <- c(train_price, price.oos[(i*7-6):(i*7)])
}


mse.7ahead.ar1 <- mean((pred7.ahead.ar1-price.oos[-365])^2)
#mse7ahead con arima(1,0,0)is 49.19, greater thne before (39.39)


# 7.3 - ARIMA(3,0,0) WITH DAILY DUMMIES ----
fit.ar3 <- Arima(price.is, order=c(3,0,0), 
                      xreg =  as.matrix(energy.week[1:1095, c(6:11)]))
summary(fit.ar3)

#comparison of bic
BIC(fit.ar1)
BIC(fit.ar3)
BIC(fit.ar5)
#best one is ARIMA (5,0,0)

acf(fit.ar3$residuals, lag=100)
pacf(fit.ar3$residuals, lag=100)

##Forecasting##

train_price <- price.is
pred7.ahead.ar3 <- c()
lb7.ahead.ar3 <- c()
ub7.ahead.ar3 <- c()

for (i in 1:52) {
  
  model <- Arima(train_price, order=c(3,0,0), 
                 xreg = as.matrix(energy.week[1:(nrow(energy.is)+(i-1)*7), c(6:11)]))
  
  forecast <- forecast(model, h = 7, 
                       xreg=as.matrix(energy.week[(1096+7*(i-1)):(1102 + 7*(i-1)),c(6:11)]))
  
  values <- forecast$mean
  lb_values <- forecast$lower[,"95%"]
  ub_values <- forecast$upper[,"95%"]
  
  pred7.ahead.ar3 <- c(pred7.ahead.ar3, values)
  lb7.ahead.ar3 <- c(lb7.ahead.ar3, lb_values)
  ub7.ahead.ar3 <- c(ub7.ahead.ar3, ub_values)
  
  train_price <- c(train_price, price.oos[(i*7-6):(i*7)])
}


mse.7ahead.ar3 <- mean((pred7.ahead.ar3-price.oos[-365])^2)
# 41.56

#comparing MSE
ar_models <- c("ARIMA(1,0,0)", "ARIMA(3,0,0)", "ARIMA(5,0,0)")
mse_ar <- c(mse.7ahead.ar1, mse.7ahead.ar3, mse.7ahead.ar5)
cbind(ar_models, mse_ar)

# 8 - CHECK FOR HOMOSKEDASTICITY ----
##check for homoskedasticity in ARIMA(5,0,0) residuals, cause it was the best 
#model among the three
checkresiduals(fit.ar5)
#ljung box not rejceted (there is no autocorrelation between the residuals) 

##Breush-Pagan test for HOMOSKEDASTICITY (ARIMA (5,0,0))


res_squared <- residuals(fit.ar5)^2
#bgtest requires a lm object
#to overcome this problem, we will calculate a regression
#square of the residuals on xt

#regression of residuals squared on xreg
#we don't include the autoregressive component because it is just 
#included in the model's residuals
mod_bp <- lm(res_squared~as.matrix(energy.week[1:1095, c(6:11)]))
summary(mod_bp)

# test statistic = n*R^2
bp_statistic <- length(res_squared) * 0.01146
#n*R2 behaves like Chi-squared with m-1 degrees of freedom
#( 7 parameters  - intercept) 
#(we could also used F statistic instead of n*R^2)

#bp_statistic should be compared with a ChiSQ with 6 degrees of freedom
1 - pchisq(bp_statistic, length(coef(mod_bp))- 1)
#v p-value =0.05079, for alfa=5% donesn't reject H0 (homoskedasticity)
#it is a limit situation, we want to check for ARCH/GARCH models

rm(list=c("res_squared","mod_bp","bp_statistic"))
#9 - GARCH MODEL ----

#plot of residuals
plot(density(fit.ar5$residuals))
#we assume normality to simplify the analysis

## ARIMAX(5,0,0)-GARCH(1,1)##
#specification of the model
spec1 <- ugarchspec(variance.model = list(model="sGARCH", 
                                          garchOrder=c(1,1)),
                                          mean.model=list(armaOrder = c(5,0),
                                                          include.mean=T, 
                                                          external.regressors = as.matrix(energy.week[1:1095, c(6:11)])))

#fit GARCH  model
fit_garch <- ugarchfit(spec = spec1, data = price.is)
fit_garch

#mu --> conditional mean of the variance
#omega --> costant (intercept of variance dynamic)
#beta1 -->  auto regressive coefficient of the variance
#alpha1 -->  updating parameter (coef of epsilon t squared)

#ACF/PACF residuals
par(mfrow=c(1,2))
acf(fit_garch@fit$residuals, lag=200)
pacf(fit_garch@fit$residuals, lag=200)

##Forecast##
train_price <- price.is
pred7.ahead.garch11 <- c()
sigma7.ahead.garch11<- c()
#we take only the variance, cause there'arent any function to obtain quantiles

for (i in 1:52) {
  spec <- ugarchspec(variance.model = list(model="sGARCH", 
                                           garchOrder=c(1,1)),
                     mean.model=list(armaOrder = c(5,0),
                                     include.mean=T, 
                                     external.regressors = as.matrix(energy.week[1:(nrow(energy.is)+(i-1)*7), c(6:11)])))
  
  model.garch <- ugarchfit(spec = spec, data = train_price)
  
  forecast.garch <- ugarchforecast(model.garch, n.ahead=7, 
                                   external.forecasts = list(as.matrix(energy.week[(1096+7*(i-1)):(1102 + 7*(i-1)), c(6:11)])))
  
  mean_values <- forecast.garch@forecast$seriesFor
  sigma_values <- forecast.garch@forecast$sigmaFor
  
  pred7.ahead.garch11 <- c(pred7.ahead.garch11, mean_values)
  sigma7.ahead.garch11<- c(sigma7.ahead.garch11, sigma_values)
  
  train_price <- c(train_price,price.oos[(i*7-6):(i*7)])
}


mse.7ahead.garch11 <- mean((pred7.ahead.garch11-price.oos[-365])^2)
#mse7ahead con garch11 viene 45.83
#worse then ARIMA(5,0,0) which is 39.39


##Prediction intervals##

#lintervals at 95% = prediction_i + 1.96*sigma_i

lb7.ahead.garch11 <- pred7.ahead.garch11 - 1.96*sigma7.ahead.garch11
ub7.ahead.garch11 <- pred7.ahead.garch11 + 1.96*sigma7.ahead.garch11


# 9.2 - CHOICE OF THE BEST GARCH MODEL
#we are going to compare the models based on both BIC and MSE

##MSE##
#list of different specifications
order_specs <- list(c(0,1),c(1, 1), c(1, 2), c(2, 1), c(2, 2))
mse.garch <- c()

#!!WARNING!! it takes a lot of time to run
# for loop for best GARCH based on MSE 
#EXTERNAL FOR LOOP ---> changes the specifications listed above
#INTERNAL FOR LOOP --> calculate the predictions for the models
for (spec in order_specs) {
  #for every new iteration, initialize train and predictions values
  train_price <- price.is
  pred<- c()
  for (i in 1:52) {
    garch_spec <- ugarchspec(variance.model = list(model="sGARCH", 
                                             garchOrder=spec),
                       mean.model=list(armaOrder = c(5,0),
                                       include.mean=T, 
                                       external.regressors = as.matrix(energy.week[1:(nrow(energy.is)+(i-1)*7), c(6:11)])))
    
    model.garch <- ugarchfit(spec = garch_spec, data = train_price)
    
    forecast.garch <- ugarchforecast(model.garch, n.ahead=7, 
                                     external.forecasts = list(as.matrix(energy.week[(1096+7*(i-1)):(1102 + 7*(i-1)), c(6:11)])))
    
    mean_values <- forecast.garch@forecast$seriesFor
    
    pred <- c(pred, mean_values)
    
    train_price <- c(train_price,price.oos[(i*7-6):(i*7)])
  }
  #at the end the MSE is calculated using predictions of the model used for 
  #that stage of the iteration
  mse.spec <- mean((pred - price.oos[-365])^2)
  
  #store MSE 
  mse.garch <- c(mse.garch,mse.spec)
  
}
mse.garch
#Based on MSE best model is GARCH(1,1)


#FOR LOOP TO SEE THE BEST MODEL BASED ON BIC

# object for the results
best_model <- NULL
best_bic <- Inf

# for loop for best GARCH based on BIC
for (spec in order_specs) {
  garch_spec <-ugarchspec(variance.model = list(model="sGARCH", garchOrder=spec),
                          mean.model=list(armaOrder = c(5,0),
                                          include.mean=T, 
                                          external.regressors = as.matrix(energy.week[1:1095, c(6:11)])),
                          distribution.model = "sstd")
  
  garch_model <- ugarchfit(spec = garch_spec, data = price.is)
  
  # BIC
  bic <- infocriteria(garch_model)[2]
  
  
  # print the results on the console
  cat(paste("Order:", spec[1], spec[2], "\n"))
  cat(paste("BIC:", bic, "\n\n"))
  
  #update the best model based on BIC values
  if (bic < best_bic) {
    best_model <- garch_model
    best_bic <- bic
  }
}

# Print best model
cat("Best Model:\n")
print(best_model)
#based on BIC GARCH(1,1) is the best GARCH model 

# 10 - COMPARISON BETWEEN ARIMAX(5,0,0) and GARCH(1,1) ----

##BIC##
infocriteria(fit_garch)[2]#BIC of GARCH
#Inforcriteria divided BIC by 1000
BIC(fit.ar5)#BIC of ARIMAX(5,0,0)
#based on BIC GARCH is better then ARIMA

##MSE##
mse.7ahead.garch11
mse.7ahead.ar5
#based on MSE ARIMA is the better

#We're interested in forecasting, so we rely on MSE and choose
#ARIMA (5,0,0) as the best model

##Graphic comparison##
#Plotting only the point forecast
ar5_fore <- ggplot() +
  geom_line(data = energy[-1460, ], aes(x = Date, y = Price), color = "#444444") +
  geom_line(data = energy.oos[-365, ], aes(x = Date, y = pred7.ahead.ar5),
            color = "red", alpha = 0.5) +
  geom_vline(data = energy.is, aes(xintercept = Date[1095]), linetype = "dashed") +
  xlab("")+
  ylab("Price")+
  ggtitle("ARIMAX(5,0,0)")+
  theme_light()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size=8))


garch11_fore <- ggplot() +
  geom_line(data = energy[-1460, ], aes(x = Date, y = Price), color = "#444444") +
  geom_line(data = energy.oos[-365, ], aes(x = Date, y = pred7.ahead.garch11), color = "red", alpha=0.5) +
  geom_vline(data = energy.is, aes(xintercept = Date[1095]), linetype = "dashed") +
  xlab("")+
  ylab("")+
  ggtitle("GARCH(1,1) - ARIMAX(5,0,0)")+
  theme_light()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size=8))

grid.arrange(ar5_fore,garch11_fore, nrow=1, ncol=2)

#Plot with prediction interval
ar5_interval <- ggplot() +
  geom_ribbon(data = energy.oos[-365, ], aes(x = Date, ymin = lb7.ahead.ar5, ymax = ub7.ahead.ar5), fill = "blue", alpha = 0.4) +
  geom_line(data = energy[-1460, ], aes(x = Date, y = Price), color = "#444444") +
  geom_line(data = energy.oos[-365, ], aes(x = Date, y = pred7.ahead.ar5), color = "red", alpha=0.5) +
  geom_vline(data = energy.is, aes(xintercept = Date[1095]), linetype = "dashed") +
  labs(x = "Date", y = "Price (€)", title = "ARIMAX(5,0,0)") +
  theme_light() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size = 8))

gar11_interval<-ggplot() +
  geom_ribbon(data = energy.oos[-365, ], aes(x = Date, ymin = lb7.ahead.garch11, ymax = ub7.ahead.garch11), fill = "blue", alpha = 0.4) +
  geom_line(data = energy[-1460, ], aes(x = Date, y = Price), color = "#444444") +
  geom_line(data = energy.oos[-365, ], aes(x = Date, y = pred7.ahead.garch11), color = "red", alpha=0.5) +
  geom_vline(data = energy.is, aes(xintercept = Date[1095]), linetype = "dashed") +
  labs(x = "Date", y = "Price (€)", title = "GARCH(1,1) - ARIMAX(5,0,0)") +
  theme_light() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size = 8))


grid.arrange(ar5_interval,gar11_interval, nrow=1, ncol=2)



