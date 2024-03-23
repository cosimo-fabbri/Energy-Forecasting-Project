rm(list=ls())
# 1 - LIBRARIES ----
# list of library needed
librerie_da_installare <- c("readxl", "tidyverse", "expss", "car", "MASS", "gridExtra", "fGarch", "quantreg", "corrplot", "leaps", "statmod")
#Install missing libraries
for (libreria in librerie_da_installare) {
  if (!requireNamespace(libreria, quietly = TRUE)) {
    install.packages(libreria, dependencies = TRUE)
    library(libreria, character.only = TRUE)
  }
}

library(readxl)   #import dataset from excel
library(tidyverse) #ggplot e dplyr(manipulation of data)
library(expss)  #label variable
library(car)   #vif
library(MASS)  #for ML estimate
library(gridExtra)
library(fGarch)   #for t-student regression
library(quantreg)    #for quantile regression
library(corrplot)   #for correlation plot
library(leaps)     #for variable selection methods
library(statmod) #for tweedie distribution

rm(list=ls()) 
# 2 - EXTRA FUNCTIONS----

#for location-scale distributions
my.dt <- function(x, a, b, df){
  out <- (1/b)*dt((x-a)/b, df = df)
  return(out)
}

#for t-student regression
llk.t <- function(par, y, x){
  k <- ncol(x)
  beta  <- par[1:k]
  sigma <- par[k+1]
  nu    <- par[k+2]
  out <- -sum(log(dt(((y-x%*%beta)/sigma), df=nu)/sigma))
  return(out)
}

#for quantile of a location scale Student t
qt_ls <- function(prob, df, mu, sigma){
  out <- qt(prob, df)*sigma + mu
  return(out)
}

#extension of predict, it allows to apply the function to regsubset objects
predict.regsubsets <- function(object , newdata , id, ...) {
  form <- as.formula (object$call[[2]])
  mat <- model.matrix (form , newdata)
  coefi <- coef(object , id = id)
  xvars <- names(coefi)
  mat[, xvars] %*% coefi
} 


# 3 - IMPORT DATASET ----
#set wd in the same folder of R-script
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
Data2018 <- read_excel("Data2018.xlsx")

##Change name of the coloumns##
colnames(Data2018)
names(Data2018)[names(Data2018) == "Pitch_angle (deg)"] <- "pitch_angle"
names(Data2018)[names(Data2018) == "Hub_temperature (deg_C)"] <- "hub_temp"
names(Data2018)[names(Data2018)=="Active_power (kW)"] <- "power"
names(Data2018)[names(Data2018) == "Generator_speed (rpm)"] <- "gen_speed"
names(Data2018)[names(Data2018) == "Generator_bearing_temperature AVG (deg_C)"] <- "gen_bear_temp"
names(Data2018)[names(Data2018) == "Generator_stator_temperature (deg_C)"] <- "gen_stat_temp"
names(Data2018)[names(Data2018) == "Gearbox_bearing_temperature AVG (deg_C)"] <- "gear_bear_temp"
names(Data2018)[names(Data2018)=="Gearbox_oil_sump_temperature (deg_C)"] <- "gear_oilsump_temp"
names(Data2018)[names(Data2018) == "Nacelle_angle (deg)"] <- "nacelle_angle"
names(Data2018)[names(Data2018) == "Nacelle_temperature (deg_C)"] <- "nacelle_temp"
names(Data2018)[names(Data2018) == "Wind_speed (m/s)"] <- "wind_speed"
names(Data2018)[names(Data2018) == "Absolute_wind_direction (deg)"] <- "wind_direction"
names(Data2018)[names(Data2018)=="Outdoor_temperature (deg_C)"] <- "temperature"
names(Data2018)[names(Data2018) == "Rotor_speed (rpm)"] <- "rotor_speed"
names(Data2018)[names(Data2018)=="Rotor_bearing_temperature (deg_C)"] <- "rotor_bear_temp"

##Finding the NULL Values##
print(paste("There are ", nrow(Data2018) - sum(complete.cases(Data2018)), 
            " observations with at least 1 NAN value", sep = ""))
#"There are 202 observations with at least 1 NAN value"
(nrow(Data2018) - sum(complete.cases(Data2018)))/(nrow(Data2018))*100

print(paste("Percantage of non complete cases is", 
            round((nrow(Data2018) - sum(complete.cases(Data2018)))/(nrow(Data2018))*100, 2), "%"))
#"Percantage of non complete cases is 2.93 %"
nan_df <- Data2018[complete.cases((Data2018)) == 0, ]
#observing the NA matrix, we note that there are some observations that have 
#NA value for all variables and some observations that have NA for gen_speed and
#rot_speed only when power = 0

##Look at other units with power = 0 in the dataset##
print(Data2018[!is.na(Data2018$power) & Data2018$power==0,c(3,4,14)], n = 200)
#In the original dataset there are still  822 observations with power = 0
nrow(nan_df[!is.na(nan_df$power),])
#among those  822, only 17 have  NA values in  rotor/generator speed.


##Assign value 0 to GEN_SPEED and ROTOR_SPEED##
Data2018$gen_speed[Data2018$power==0 & is.na(Data2018$gen_speed)] <- 0
Data2018$rotor_speed[Data2018$power==0 & is.na(Data2018$rotor_speed)] <- 0

##Remove all the missing values##
df <- na.omit(Data2018)
#6719 observations with 15 variables









# 4 - EXPLORATORY DATA ANALYSIS----

##Distribution of Power##
ggplot(df, aes(power))+
  geom_density(fill = "grey", alpha = 0.5) +
  ggtitle("Power Density Plot")+
  xlab("Power Generated (kW)")+ylab("")+
  theme_light()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.y = element_blank())

##Histogram of Power##
ggplot(df, aes(power))+
  geom_histogram(fill = "darkgrey", color = "white", bins = 30) +
  ggtitle("(a) Histogram of Power")+
  xlab("Power Generated (kW)")+ylab("")+
  theme_light()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.y = element_blank(),
        plot.title = element_text(size=6.5),
        axis.title.x = element_text(size=7),
        axis.title.y = element_text(size=7))


##BoxPlot of Power##
ggplot(df, aes(x="", y=power)) +
  geom_boxplot(color="black", fill="lightgrey") +
  ggtitle("(b) Boxplot of Power") +
  stat_summary(fun=mean, geom="point", shape=23, size=2, fill="white", color="red", stroke=1) +
  stat_summary(fun=median, geom="point", shape=23, size=2, fill="white", color="blue", stroke=1) +
  ylab("Power Generated (kW)") +
  xlab("") +
  theme_light() + theme(panel.grid.major = element_blank(),
                        panel.grid.minor = element_blank(),
                        axis.text.x = element_blank(),
                        plot.title = element_text(size=6.5),
                        axis.title.x = element_text(size=7),
                        axis.title.y = element_text(size=7)) +
annotate("text",x = 1.3,y = 1900,label = "• Mean: 747.15 kW",color = "red",size = 2.5) +
annotate("text",x = 1.3,y = 1700,label = "• Median: 521.75 kW",color = "blue",size = 2.5)

##Histogram of Covariates##
#increase the size of the plot section to visualize it
par(mfrow=c(4,4))
for (col in colnames(df)) {
  hist(df[[col]], main = paste("Histogram of", col), xlab = col, freq=FALSE)
  lines(density(df[[col]])$x, density(df[[col]])$y, lwd=2, col="red")
}

##Boxplot of covariates##
par(mfrow=c(4,4))
for (col in colnames(df)) {
  boxplot(df[[col]], main = paste("Boxplot of", col), xlab = col)
}

##Scatterplot of covariates##
par(mfrow=c(4,4))
for (col in colnames(df)) {
  if (col != "power"){
    plot(df[[col]], y = df$power, xlab=col, ylab="Power")
  }
}
par(mfrow=c(1,1))
#considering the reletionship with gen_speed and rotor_speed it would be helpful
#to inlcude a cubic term in the regression

##Focus on scatterplot of PowerVSGen_speed and PowerVSWind_speed##
power_wind <- ggplot(df, aes(x = wind_speed, y = power)) +
  geom_point(size = 3, alpha = 0.8)+
  ggtitle("(c) Power vs Wind Speed")+
  xlab("Wind Speed (m/s)")+ylab("Power Generated (kW)")+
  theme_light()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size=6.5),
        axis.title.x = element_text(size=7),
        axis.title.y = element_text(size=7))

power_gen_speed <- ggplot(df, aes(x = gen_speed, y = power)) +
  geom_point(size = 3, alpha = 0.8)+
  ggtitle("(d) Power vs Gen_Speed")+
  xlab("Gen_Speed (rpm)")+ylab("Power Generated (kW)")+
  theme_light()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title=element_text(size=6.5),
        axis.title.x = element_text(size=7),
        axis.title.y = element_text(size=7))

grid.arrange(power_wind, power_gen_speed, nrow = 1, ncol = 2)
#it has sense to explore a poly of grade 3 in the regression

##Correlation between variables##

#corelation plot 
corrplot(cor(df), type = "upper")
mat_corr <- cor(df)
#High correlated variables: 
#Gen_speed , rotor_Speed and gear_bear_temp













# 5 - TRAIN/TEST SPLITTING----
set.seed(1)

indexes <- sample(nrow(df), size = nrow(df)*0.5, replace = F)
df.is <- df[indexes,]
df.oos <- df[-indexes,]


# 6.a - REGRESSION MODEL: Normal and Student's t regression (ALL VARIABLES)----

##First model with all the variables and Normality assumption on residual##
##as baseline##

fit0 <- lm(power ~., data=df.is)
summary(fit0)
plot(density(fit0$residuals))
#residuals don't look like they're normally distributed

#Simulation to check the normality
lines(seq(-2000,1000,by=1), dnorm(seq(-2000,1000,by=1), 
                                  mean = mean(fit0$residuals), 
                                  sd = sd(fit0$residuals)), col=2, lwd=2)
#from here we can see that our distribution of residuals has an higher peak, 
#so it's more similar to a t-Student

##Estimate the degrees of freedom of the t ditribution we want ot fit##

#use ML to estimate the degrees of freedom of the residual's distribution
est.t <- fitdistr(fit0$residuals, densfun = "t")
est.t
#degrees of freedom estimated = 4.7

#Simulation to check the student's t with 4.7 degrees of freedom
plot(density(fit0$residuals))
lines(seq(-2000,1000,by=1), my.dt(x = seq(-2000,1000,by=1), 
                                  a = est.t$estimate[1],
                                  b= est.t$estimate[2],
                                  df = est.t$estimate[3]), col=4, lwd=2)
#better then before

#Now we try with 2 degrees of freedom (!!!minumum allowed!!!) to check
#if it is betterx
plot(density(fit0$residuals), main="Degrees of Freedom = 2")
lines(seq(-2000,1000,by=1), my.dt(x = seq(-2000,1000,by=1), 
                                  a = est.t$estimate[1],
                                  b= est.t$estimate[2],
                                  df = 2), col=6, lwd=2)

par(mfrow=c(1,2))
plot(density(fit0$residuals))
lines(seq(-2000,1000,by=1), my.dt(x = seq(-2000,1000,by=1), 
                                  a = est.t$estimate[1],
                                  b= est.t$estimate[2],
                                  df = est.t$estimate[3]), col=4, lwd=2)
plot(density(fit0$residuals))
lines(seq(-2000,1000,by=1), my.dt(x = seq(-2000,1000,by=1), 
                                  a = est.t$estimate[1],
                                  b= est.t$estimate[2],
                                  df = 2), col=6, lwd=2)
#the distribution with 2 degrees of freedom seems to work better on the mean
#the distribution with 4.7 degrees of freedom seems to work better on the tails

##We try to fit the 2 model and compare the MSE to chose the one to keep##
##fit glm model with t-student##
set.seed(1)

##4.7 degrees of freedom##
beta <- fit0$coefficients#first value of beta is the intercept
sigma <- sd(fit0$residuals)
nu <- est.t$estimate[3]
t.sv <- c(beta, sigma, nu)
Y <- df.is$power

#matrix of covariates (in sample)
X.is<-cbind(1,df.is[,-3])#1 for the intercept, then remove power
X.is <- as.matrix(X.is)#necessary to transform the matrix so that 
#X%*%beta gives a result

#use the function llk.t to estimate parameters of the student's t regression
t.est<- optim(t.sv, llk.t,y = Y, x = X.is)

#get the parameter of the regression
t.est$par

#matrix of covariates (out of sample)
X.oos <- cbind(1,df.oos[-3])
X.oos <- as.matrix(X.oos)

#MSE Student's t regression with 4.7 degrees of freedom
y.pred_t <- X.oos%*%t.est$par[1:15]
MSE_tstud4_7 <- mean((df.oos$power - y.pred_t)^2)
#26704

##2 degrees of freedom##
beta <- fit0$coefficients#first value of beta is the intercept
sigma <- sd(fit0$residuals)
nu <- 2
t.sv <- c(beta, sigma, nu)
Y <- df.is$power

#matrix of covariates (in sample)
X.is<-cbind(1,df.is[,-3])#1 for the intercept, then remove power
X.is <- as.matrix(X.is)#necessary to transform the matrix so that 
#X%*%beta gives a result

t.est<- optim(t.sv, llk.t,y = Y, x = X.is)

#get the parameter of the regression
t.est$par

#matrix of covariates (out of sample)
X.oos <- cbind(1,df.oos[-3])
X.oos <- as.matrix(X.oos)

#MSE Student's t regression with 2 degrees of freedom
y.pred_t <- X.oos%*%t.est$par[1:15]
MSE_tstud_2 <- mean((df.oos$power - y.pred_t)^2)
#25731
#MSE is slightly better with 2 degrees of freedom
#maybe the one with 4.7 is worst in predict the central values of the distribution
#and better in predict the extreme ones.
#contrarily, the one with 2 degrees of freedom is better in predict the central values
#(where there are more obs) but struggle with extreme values





# 6.b - REGRESSION MODEL: Normal and Student's t regression (VARIABLE SELECTION)----

##Variable selection with best subset selection method##

#First of all fit a linear normal regression model to do the variable selection
reg.full <- regsubsets(power ~ ., data = df.is, method = "exhaustive",
                       nvmax = 14)
summary.full <- summary(reg.full)

#plot with decrease in BIC 
par(mfrow=c(1,1))
plot(summary.full$bic, type="l", xlab = "Number of variable", ylab="bic")
points(x = which.min(summary.full$bic), 
       y = summary.full$bic[which.min(summary.full$bic)], 
       cex=2, pch=20, col="red")
which.min(summary.full$bic)
#best model has 9 variables
#coefficients of the model
coef(reg.full, 9)
mse.full <-  mean((predict(reg.full, id = 9, newdata = df.oos) - df.oos$power)^2)
#25865
#similar to the student's t regression with nu=2

##Student's t regression with variable selected (9) and 2 degrees of freedom

#Let's call the following model as fit_tselection
fit_tselection <- lm(power~pitch_angle + hub_temp + gen_speed + gen_bear_temp + gen_stat_temp + gear_bear_temp+
             wind_speed + rotor_speed + rotor_bear_temp, data=df.is)
summary(fit_tselection)
par(mfrow=c(1,1))

est.tselection <- fitdistr(fit_tselection$residuals, densfun = "t")
est.tselection
par(mfrow=c(1,2))
plot(density(fit_tselection$residuals))
lines(seq(-2000,1000,by=1), my.dt(x = seq(-2000,1000,by=1), 
                                  a = est.tselection$estimate[1],
                                  b= est.tselection$estimate[2],
                                  df = est.tselection$estimate[3]), col=4, lwd=2)
plot(density(fit_tselection$residuals))
lines(seq(-2000,1000,by=1), my.dt(x = seq(-2000,1000,by=1), 
                                  a = est.tselection$estimate[1],
                                  b= est.tselection$estimate[2],
                                  df = 2), col=6, lwd=2)
#seems still better with nu=2, so use this model to make prediction

beta_tselection <- fit_tselection$coefficients
sigma_tselection <- sd(fit_tselection$residuals)
nu <- 2
t.sv_tselection <- c(beta_tselection, sigma_tselection, nu)

Y <- df.is$power
X.is.tselection<-cbind(1,df.is[,-c(3,8,9,10,12,13)])
X.is.tselection <- as.matrix(X.is.tselection)

t.est.tselection<- optim(t.sv_tselection, llk.t,y = Y, x = X.is.tselection)
t.est.tselection$par

X.oos.tselection <- cbind(1,df.oos[-c(3,8,9,10,12,13)])
X.oos.tselection <- as.matrix(X.oos.tselection)

y.pred_tselection <- X.oos.tselection%*%t.est.tselection$par[1:10]
MSE_tstud.tselection <- mean((df.oos$power - y.pred_tselection)^2)
#26405

##fit a model including to gen_speed and rotor_speed a quadratic term (to take 
#into account the effect of acceleration) and cubic term (to take the effect 
#detected in the exploratory analysis).
#let's call this model fit_poly##

fit_poly <- lm(power~pitch_angle + hub_temp + poly(gen_speed,3) + gen_bear_temp + gen_stat_temp +
             poly(wind_speed,3) + rotor_bear_temp, data=df.is)
summary(fit_poly)
#all terms are significant
y.pred.poly<-predict(fit_poly, df.oos)
mse.poly<- mean((y.pred.poly-df.oos$power)^2)
#mse  9173, way to small compared with the previous one


# 6.c - REGRESSION MODEL: Tweedie Distribution ----

##checking for distribution of Power in train set##
par(mfrow=c(1,1))
hist(df.is$power, freq=F)
#it looks like a tweedie distribution can be fitted.
#Tweedie distribution is a distributions in the exponential family that for a 
#given range of shape parameters p (1<𝑝<2) have a point mass in zero 
#and a skewed positive distribution for 𝑥>0

#Power: non negative zero inflated continuous variable, so 1<p<2

#Fit a glm with tweedie distribution (starting from the poly model fitted above)
?tweedie
#var.power = 0 --> normal family
#var.power = 1 (e link = 0) --> Poisson family 
#var.power = 2 --> Gamma family
#var.power = 3 --> Inverse Gaussian family 
#inverse Gaussian is not our case cause doesn't admit y=0

##initially, we want to fit a model with var.power=1,5 to make a try##
fit_tweedie.trial <- glm(power~pitch_angle + hub_temp + poly(gen_speed,3) + gen_bear_temp + gen_stat_temp +
                     poly(wind_speed,3) + rotor_bear_temp,
                   data=df.is, 
                   family=tweedie(var.power=1.5, link.power = 0))
summary(fit_tweedie.trial)

#prediction and MSE
pred_tweedie.trial <- predict(fit_tweedie.trial, newdata = df.oos, type = "response")
mse_tweedie.trial <- mean((pred_tweedie.trial-df.oos$power)^2)
#4433, way better then the other models but lets try to improve it

##checking for predicted value < 0##
sum(predict(fit_poly, df.oos)<0)#286, doesn't have sense that power is negative
sum(pred_tweedie.trial<0)#0, as we expected

##For loop for selecting the bast value of var.power#
# the starting value is 1.05 and the maximum is 2 (because we suspect 
#is in the middle between a Poisson and a Gamma)##
models_tweedie <- matrix(c(seq(1.05,2,by=0.05), rep(0, length(seq(1.05,2,by=0.05)))), ncol=2)
models_tweedie<- as.data.frame(models_tweedie)
colnames(models_tweedie)[1] <- "var.power"
colnames(models_tweedie)[2] <- "MSE"
for (i in 1:nrow(models_tweedie)){
  fit_tweedie <- glm(power~pitch_angle + hub_temp + poly(gen_speed,3) + gen_bear_temp + gen_stat_temp +
                       poly(wind_speed,3) + rotor_bear_temp,
                     data=df.is, 
                     family=tweedie(var.power=models_tweedie$var.power[i], link.power = 0))
  
  pred_tweedie <- predict(fit_tweedie, newdata = df.oos, type = "response")
  
  models_tweedie$MSE[i] <- mean((pred_tweedie-df.oos$power)^2)
}
models_tweedie
#plot the var.power values
plot(models_tweedie$var.power, models_tweedie$MSE, ylim=c(2000,10000), ylab="MSE", xlab="Var Power")
which.min(models_tweedie$MSE)
models_tweedie$var.power[1]
models_tweedie$MSE[1]
#the best model in term of MSE is the one with var.power=1,05

##Final model with glm (tweedie distribution, var.power=1.05) and poly##
fit_tweedie <- glm(power~pitch_angle + hub_temp + poly(gen_speed,3) + gen_bear_temp + gen_stat_temp +
                     poly(wind_speed,3) + rotor_bear_temp,
                   data=df.is, 
                   family=tweedie(var.power=1.05, link.power = 0))
summary(fit_tweedie)
pred_tweedie <- predict(fit_tweedie, newdata = df.oos, type = "response")

mse_tweedie <- mean((pred_tweedie-df.oos$power)^2)
#2926, way better then the other models

##Graphical comparison of FittedVSActual values for the best model of each family
par(mfrow=c(1,3))

plot(y.pred.poly, df.oos$power, xlab="Predicted Values", ylab="Observed Values", 
     main="Normal Regression")
lines(df.oos$power, df.oos$power, col="red")


plot(y.pred_tselection, df.oos$power, xlab="Predicted Values", ylab="Observed Values", 
     main="T-Student Regression", xlim=c(-800,2500))
lines(df.oos$power, df.oos$power, col="red")

plot(pred_tweedie, df.oos$power, xlab="Predicted Values", ylab="Observed Values", 
     main="Tweedie Regression")
lines(df.oos$power, df.oos$power, col="red")
#in tweedie regression no predictions below 0 and there are not strange pattern 
#at the boundary of the plot

# 7.a - PREPARATORY ANALYSIS FOR QUANTILE REGRESSION ----

##We started from fit_poly to modeling the quantile regression#

fit_poly<- lm(power~pitch_angle + hub_temp + poly(gen_speed,3) + gen_bear_temp + gen_stat_temp +
             poly(wind_speed,3) + rotor_bear_temp, data=df.is)

y.pred.poly<- predict(fit_poly, df.oos)
par(mfrow=c(1,1))
ggplot(data.frame(x = fit_poly$residuals), aes(fit_poly$residuals)) +
  geom_density() +
  theme_light()+xlab("")+ylab("Density")+
  theme(panel.grid = element_blank())
#the residual's distribution resemble a student's t/skewed student's t

#we've estimated the parameters of the residual distribution using ML
est.tpoly<- fitdistr(fit_poly$residuals, densfun = "t")
est.tpoly
#degrees of freedom are ~4

##We simulate some distributions##

#location-scale student t
plot(density(fit_poly$residuals),  main="Distribution of Residuals", sub="t-student", xlab="Residuals", col.sub="blue")
lines(seq(-2000,1000,by=1), my.dt(x = seq(-2000,1000,by=1), 
                                  a = est.tpoly$estimate[1],
                                  b= est.tpoly$estimate[2],
                                  df = est.tpoly$estimate[3]), col=4, lwd=2)
#it fits well,but we try even skewed t

#Skewed student's t
?sstd
#xi is the skewness parameter
#we need to estimate the degree of freedom of the skewed t
est.tpoly.skew <- sstdFit(fit_poly$residuals)
est.tpoly.skew$estimate
#nu=3.88

#let's see a comparison between 2 different skewed t (nu=3.88 ; nu=4.5)
par(mfrow=c(1,2))
plot(density(fit_poly$residuals), main="Distribution of Residuals", sub="Skewed t-student with 3.9 df", xlab="Residuals", col.sub=2)
lines(seq(-2000,1000,by=1), dsstd(x = seq(-2000,1000,by=1),  mean = est.tpoly.skew$estimate[1],
                                  sd= est.tpoly.skew$estimate[2],
                                  nu = est.tpoly.skew$estimate[3],
                                  xi= est.tpoly.skew$estimate[4]), col=2, lwd=2)
#the distribution fits better, but maybe the parameter could be change
plot(density(fit_poly$residuals), main="Distribution of Residuals", sub="Skewed t-student with 4.5 df", xlab="Residuals", col.sub=4)
lines(seq(-2000,1000,by=1), dsstd(x = seq(-2000,1000,by=1), 
                                  mean = est.tpoly.skew$estimate[1],
                                  sd= est.tpoly.skew$estimate[2],
                                  nu = 4.5,
                                  xi= est.tpoly.skew$estimate[4]), col=4, lwd=2)
#with a change in the degrees of freedom the distribution fits better

#let's compare the location scale t with the skwed-t (nu=4.5)
#location scale t
plot(density(fit_poly$residuals),  main="Distribution of Residuals", sub="t-student", xlab="Residuals", col.sub=6)
lines(seq(-2000,1000,by=1), my.dt(x = seq(-2000,1000,by=1), 
                                  a = est.tpoly$estimate[1],
                                  b= est.tpoly$estimate[2],
                                  df = est.tpoly$estimate[3]), col=6, lwd=2)
#skewd t
plot(density(fit_poly$residuals), main="Distribution of Residuals", sub="Skewed t-student with 4.5 df", xlab="Residuals", col.sub=4)
lines(seq(-2000,1000,by=1), dsstd(x = seq(-2000,1000,by=1), 
                                  mean = est.tpoly.skew$estimate[1],
                                  sd= est.tpoly.skew$estimate[2],
                                  nu = 4.5,
                                  xi= est.tpoly.skew$estimate[4]), col=4, lwd=2)
#skewd t seems better, but we want ot try both the distributions in quantiles
par(mfrow=c(1,1))

# 7.b - CALCULATION OF QUANTILES ----

#10% quantile with location scale student t
lb_t_stud <- y.pred.poly + qt_ls(0.1, df=est.tpoly$estimate[3],mu=est.tpoly$estimate[1],
                              sigma = est.tpoly$estimate[2])
#10% quantiles with skewed t
lb_t_skew <- y.pred.poly +qsstd(0.10,  mean = est.tpoly.skew$estimate[1],
                             sd = est.tpoly.skew$estimate[2], nu = 4.5, 
                             xi = est.tpoly.skew$estimate[3])


#10th quantile with QUANTILE REGRESSION (using the best model that we've obtained)
qrlb <- rq(power~pitch_angle + hub_temp + poly(gen_speed,3) + gen_bear_temp + gen_stat_temp +
             poly(wind_speed,3) + rotor_bear_temp, 
           tau = 0.1,
           data=df.is)
pre.qrlb <- predict(qrlb, df.oos)


##Comparison of quantiles predictions obtained for the 3 approaches##
#we put in the first column the values of the test-obs from which we want 
#to obtain the 10-th quantile
cbind(df.oos$power, pre.qrlb, lb_t_stud, lb_t_skew)
#in this case, the best would be the one more narrow (so the one that are
#greater value), because we are predicting lower quantiles
#from the table it isn't clear which are the best predictions, so we decided
#to perform a likelihood ratio test.

# 7.c - EVAULATION OF THE QUANTILES USING LIKELIHOOD TEST -----

##the "likelihood ratio test" is used to compare the likelihood of 
#two alternatives:
# H0: mean(I)=0.10 (quantiles predicted are effectively the right quantile)
# H1: mean(I)≠0.10

#test statistic is distributed as a Chi-Squared with 1 degree of freedom because there
#is only one parameter

##Quantile Regression##
i.qrlb <- 1*(df.oos$power < pre.qrlb)
mean(i.qrlb)# 0.09464
#it means that in test set Power is lower then 10th quantile estimated by 
#quantile regression in 9.46% of the cases

#pvalue
pv.qrlb <- 1-pchisq(-2*(sum((i.qrlb)*log(0.10)+(1-i.qrlb)*log(0.90))
                        -sum((i.qrlb)*log(mean(i.qrlb))+(1-i.qrlb)*log(1-mean(i.qrlb)))), 1)
pv.qrlb#0.30
#we DO NOT reject the H0:mean(I) = 0.10, so our estimation with the 
#quantile regression is fine

##Student's t##
i.tstud <- 1*(df.oos$power < lb_t_stud)
mean(i.tstud)#0.10417

pv.tstud <- 1-pchisq(-2*(sum((i.tstud)*log(0.10)+(1-i.tstud)*log(0.90))
                         -sum((i.tstud)*log(mean(i.tstud))+(1-i.tstud)*log(1-mean(i.tstud)))), 1)
pv.tstud#0.4236
#we DO NOT reject the H0:mean(I) = 0.10, so our estimation with the 
#student's t  is fine

##Skewed t##
i.tskew <- 1*(df.oos$power < lb_t_skew)
mean(i.tskew)#0.1300

pv.tskew <- 1-pchisq(-2*(sum((i.tskew)*log(0.10)+(1-i.tskew)*log(0.90))
                         -sum((i.tskew)*log(mean(i.tskew))+(1-i.tskew)*log(1-mean(i.tskew)))), 1)
pv.tskew #%0
##we  REJECT H0:p = 0.10, so we can discard this option


##Finally, we compare the quantiles obtained with QUANTILE REGRESSION and  
#STUDENT'S T ##

#as we said before, the narrower the better, and we could even look at the p-value 
# and at the mean of binary indicators to choose the best model

mean(pre.qrlb > lb_t_stud)
#52% of the times the quantiles predicted with the quantile regression
#are greater than the one predicted with student t
#so essentially the two methods are similar

cbind(mean(i.tstud),mean(i.qrlb))
#student's t model is more "conservative": in more cases test values are lower
#then quantiles predicted in respect to quantile regression


