rm(list=ls())
# 1 - LIBRARIES ----
# list of library needed
librerie_da_installare <- c("readxl", "expss", "tidyverse", "corrplot", "randomForest", "gridExtra", "scales", "glmnet")

#Install missing libraries
for (libreria in librerie_da_installare) {
  if (!requireNamespace(libreria, quietly = TRUE)) {
    install.packages(libreria, dependencies = TRUE)
    library(libreria, character.only = TRUE)
  }
}

library(readxl)
library(expss) 
library(tidyverse)
library(corrplot)
library(randomForest)
library(corrplot)
library(gridExtra)   #for grid ggplot
library(scales)   #for percentage plots
library(glmnet)  #variable selection with lasso 
rm(list=ls())
# 2 - IMPORT DATASET ----
#set wd in the same folder of R-script
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
#import dataset [it takes a few time](27 variables, 403042 obs, variables name in Portuguese)
MT_station_Brasil <- read_excel("MT_station_Brasil.xlsx")

##Formatting and dropping variables##

#checking for obs with value -9999 in total_precipitation 
sum(MT_station_Brasil$`PRECIPITA√á√ÉO TOTAL, HOR√ÅRIO (mm)` == -9999)
#114989 observations with -9999 (NAN)
#set them as NA values
MT_station_Brasil[MT_station_Brasil == -9999] <- NA
#checking for variables with sum = 0
colSums(is.na(MT_station_Brasil))
# no variable without any information

#We decided to remove:
#TEMPERATURA DO AR - BULBO SECO, HORARIA (¬∞C)
#TEMPERATURA ORVALHO MAX. NA HORA ANT. (AUT) (¬∞C)
#TEMPERATURA ORVALHO MIN. NA HORA ANT. (AUT) (¬∞C)
#UMIDADE REL. MAX. NA HORA ANT. (AUT) (%)
#UMIDADE REL. MIN. NA HORA ANT. (AUT) (%)
#VENTO, DIRE√á√ÉO HORARIA (gr) (¬∞ (gr))
#VENTO, RAJADA MAXIMA (m/s)
#region 
#state
#station
#station_code
#latitude
#longitude
#height
df <- MT_station_Brasil[,-c(21:27)]
df <- df[,-c(1,2,3,9,13,14,15,16,18,19)]

#And to remove all missing values
dati <- na.omit(df)
#161570 observations, with 10 variables

##rename variables##
names(dati)[names(dati) == "PRECIPITA√á√ÉO TOTAL, HOR√ÅRIO (mm)"] <- "precipitation"
names(dati)[names(dati) == "PRESSAO ATMOSFERICA AO NIVEL DA ESTACAO, HORARIA (mB)"] <- "sl_pressure"
names(dati)[names(dati) == "PRESS√ÉO ATMOSFERICA MAX.NA HORA ANT. (AUT) (mB)"] <- "max_pressure"
names(dati)[names(dati) == "PRESS√ÉO ATMOSFERICA MIN. NA HORA ANT. (AUT) (mB)"] <- "min_pressure"
names(dati)[names(dati) == "RADIACAO GLOBAL (Kj/m¬≤)"] <- "solar_radiation"
names(dati)[names(dati) == "TEMPERATURA DO PONTO DE ORVALHO (¬∞C)"] <- "dew_point_temp"
names(dati)[names(dati) == "TEMPERATURA M√ÅXIMA NA HORA ANT. (AUT) (¬∞C)"] <- "max_temp"
names(dati)[names(dati) == "TEMPERATURA M√çNIMA NA HORA ANT. (AUT) (¬∞C)"] <- "min_temp"
names(dati)[names(dati) == "UMIDADE RELATIVA DO AR, HORARIA (%)"] <- "relative_humid"
names(dati)[names(dati) == "VENTO, VELOCIDADE HORARIA (m/s)"]  <- "wind_speed"

#labels
dati <- apply_labels(dati,
                     precipitation = "Total Precipitation (mm)",
                     sl_pressure = "Air Pressure at the station level (mB)",
                     max_pressure = "Maximum Air Pressure (mB)",
                     min_pressure = "Minimum Air Pressure (mB)",
                     solar_radiation = "Solar Radiation (kj/m2)",
                     dew_point_temp = "Dew Point Temperature (°C)",
                     max_temp = "Maximum Temperature (°C)",
                     min_temp = "Minimum Temperature (°C)",
                     relative_humid = "Relative Humidity (%)",
                     wind_speed="Wind Speed (m/s)")

##transforming Wind_speed##
#wind_speed form m/s --> km/h
dati$wind_speed <- dati$wind_speed*3.6
summary(dati$wind_speed)
#now it's more interpretable for our purpose 

##We want to create a new binary variable Wind {1,0}, which is 1 when 
#wind_speed>5 and 0 when wind.speed<=5##
dati$energy <- 1*(dati$wind_speed>5)

##are classes balanced?##
sum(dati$energy)/nrow(dati)
#classes are balanced: 52% of the observation reported wind_speed > 5 km/h 

#finally, we remove wind_speed from the dataset
dati <- dati[,-10]




# 3 - EXPLORATORY DATA ANALYSIS ----

colnames(dati)

##WIND (our binary dependent variable)##

#barplot to see the percentage distribution of wind
barplot_energy<- dati%>%
  group_by(energy)%>%
  summarize(cnt=n())%>%
  mutate(freq=cnt/sum(cnt))%>%
  ggplot(aes(x=factor(energy), y=freq,  label=percent(freq)))+
  geom_bar(stat="identity", fill=c("lightblue", "#FFBEBE"), color="#444444")+
  geom_text(position = position_dodge(width = .9), vjust = 2, size = 3) + 
  scale_y_continuous(labels = percent)+
  xlab("")+ ylab("Frequency (%)")+
  ggtitle("Energy Classes")+
  theme_light()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), legend.position = "none",
        axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        plot.title = element_text(size = 10))
barplot_energy
# classes are balanced

##Histograms for covariates##
par(mfrow=c(3,3))
for (col in colnames(dati)[-10]) {
  hist(dati[[col]], main = paste("Histogram of", col), xlab = col, freq=FALSE)
  lines(density(dati[[col]])$x, density(dati[[col]])$y, lwd=2, col="red")
}

##Box Plot for covariates##
par(mfrow=c(3,3))
for (col in colnames(dati)[-10]) {
  boxplot(dati[[col]], main = paste("Boxplot of", col), xlab = col)
}

##Comparison between the two levels of wind and the other variables##

#Density plot of the covariates, conditioned to the values of energy#
# Subsets the data based on the binary variable
dati_0 <- dati[dati$energy == 0, ]
dati_1 <- dati[dati$energy == 1, ]
plots <- list()# Create an empty list to store the plots

for (col in colnames(dati)[-10]) {#Iterate through the columns
  # Plot the densities
  plot_title <- paste("Density Comparison:", col)
  
  plot <- ggplot() +
    geom_density(data = dati_0, aes(x = .data[[col]], fill = "0"), color = "blue", alpha = 0.5) +
    geom_density(data = dati_1, aes(x = .data[[col]], fill="1"), color = "red", alpha = 0.5) +
    xlab(col) +
    ylab("Density") +
    ggtitle(plot_title) +
    scale_fill_manual(values = c("0" = "blue", "1" = "red"), labels = c("0", "1"), name = "Wind") +
    theme_light()+
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), legend.position = "right")
  
  plots[[col]] <- plot
}
grid <- grid.arrange(grobs = plots, nrow = 3, ncol = 3)
grid

#temp variables, solar radiation and relative_humidity behave different
#conditioned to energy values

##Box plot of the covariates, conditioned to the values of energy##
par(mfrow=c(3,3))
for (i in colnames(dati)[-10]) {
  boxplot(dati[[i]] ~ dati$energy, main = paste("Boxplot of", i) ,
          ylab = i, xlab = "", col=c("blue", "red"))
}
#same consideration as for the density plots showed above

##CORRELATION##
par(mfrow=c(1,1))
corrplot(cor(dati), type = "upper")
cor(dati)
#min_temp and max_temp correlation=0.96 (very high as we expected)
#same for the pressures (min, max, sl)
#it would be helpful to remove one pressure and one temperature variable


# 4 - TRAIN/TEST SPLITTING ----
#train-test split
set.seed(1)
indexes <- sample(1:nrow(dati), nrow(dati)*0.7)
dati.is <- dati[indexes,]
dati.oos <- dati[-indexes,]
                 
# 5 - LOGIT MODEL ----

##first of all we want to try fitting a logistic regression 
#to estimate the probability of produce energy (energy=1)##

##Model with all the variables##
#We use glm() function, specifing the family option: binomial#
fit_logit0 <- glm(data = dati.is, energy~., family = "binomial")
summary(fit_logit0)
#dew_point and max_temp are not significant

#Get the predictions on the test set
pred.logit0 <- predict(fit_logit0, newdata=dati.oos, type="response")
#response set the scale of the prediction on the same scale of the y (0,1)
pred.logit0
#which predicted value is over 0.5?
d_logit0.oos <- 1*(pred.logit0>0.5)

#create the Confusion Matrix
cm.logit0<- table(dati.oos$energy, d_logit0.oos)
cm.logit0
#get the accuracy of the model. we compute that as the number of value
#correctly predicted over the total number of test set
acc.logit0 <- (cm.logit0[1,1] + cm.logit0[2,2])/nrow(dati.oos)
acc.logit0
#accuracy = 65%

##We now try to remove the highly correlated variables, fitting a model
# without min_pressure, max_pressure and max_temperature##
fit_logit1<- glm(energy~.-max_pressure-min_pressure-max_temp, data = dati.is, family = "binomial")
summary(fit_logit1)

pred.logit1 <- predict(fit_logit1, newdata = dati.oos, type="response")
d_logit1.oos <- 1*(pred.logit1>0.5)
cm.logit1<- table(dati.oos$energy, d_logit1.oos)
cm.logit1
acc.logit1 <- (cm.logit1[1,1] + cm.logit1[2,2])/nrow(dati.oos)
acc.logit1
#accuracy decreases to 63%

##For the scope of the analysis we were asked to penalize the FALSE POSITIVE
#cause for the company is worse to predict that energy will be produced when 
#actually it won't than the counterfactual case##

#try to fit the complete model with weights = 2
fit_logit_costi0 <- glm(energy ~ ., data = dati.is, 
             family = "binomial", 
             weights = ifelse(energy == 1, 1, 2))
summary(fit_logit_costi0)
pred.logit_costi0 <- predict(fit_logit_costi0, newdata = dati.oos, type="response")
d.logit_costi0.oos <- 1*(pred.logit_costi0>0.5)
cm.logit_costi0<- table(dati.oos$energy, d.logit_costi0.oos)
cm.logit_costi0
acc.logit_costi0<- (cm.logit_costi0[1,1] + cm.logit_costi0[2,2])/nrow(dati.oos)
acc.logit_costi0
#accuracy falls to 61% but FP decrease from  9266 to 2914

#we decided to use PRECISION metric in order to better judge the best model, 
#because in literature it is suggested in decision made upon False Positive

#logistic model with all the covariates
precision.logit0 <- (cm.logit0[2,2])/colSums(cm.logit0)[2]
precision.logit0
#0.66

#logistic model without variables highly correlated
precision.logit1 <- (cm.logit1[2,2])/colSums(cm.logit1)[2]
precision.logit1
#0.64

#logistic model with weights and all covariates
precision.logit_costi0 <- (cm.logit_costi0[2,2])/colSums(cm.logit_costi0)[2]
precision.logit_costi0
#0.76, better then the models without weights

##We now try to remove the highly correlated variables, fitting a model without
#min_pressure, max_pressure and max_temperature##

fit_logit_costi1<- glm(energy~.-max_pressure-min_pressure-max_temp, data = dati.is,
                       family = "binomial",
                       weights = ifelse(energy == 1, 1, 2))
summary(fit_logit_costi1)

pred.logit_costi1 <- predict(fit_logit_costi1, newdata = dati.oos, type="response")
d.logit_costi1.oos <- 1*(pred.logit_costi1>0.5)
cm.logit_costi1<- table(dati.oos$energy, d.logit_costi1.oos )
cm.logit_costi1
acc.logit_costi1 <- (cm.logit_costi1[1,1] + cm.logit_costi1[2,2])/nrow(dati.oos)
acc.logit_costi1 
#stay around 59%, FP 2809 so a bit less
precision.logit_costi1 <- (cm.logit_costi1[2,2])/colSums(cm.logit_costi1)[2]
precision.logit_costi1
#0.75
#worse than before

# 6 - PROBIT MODEL ----

##Model with all the variables##
fit_probit0 <- glm(data = dati.is, energy~., family = binomial(link="probit"))
summary(fit_probit0)

pred_probit0<- predict(fit_probit0, newdata=dati.oos, type="response")
pred_probit0
#pred are predicted probabilities 

#Put the treshold on 0.5 (if prediction is above --> 1; otherwise --> 0)
d_probit.0.oos <- 1*(pred_probit0>0.5)

#confusion matrix
cm_probit0<- table(dati.oos$energy, d_probit.0.oos)
cm_probit0
acc_probit0<- (cm_probit0[1,1] + cm_probit0[2,2])/nrow(dati.oos)
acc_probit0
#0.65, FP 8741

precision.probit0 <- (cm_probit0[2,2])/colSums(cm_probit0)[2]
precision.probit0
#0.66

##Probit model with weights##
fit_probit_costi0 <- glm(energy ~ ., data = dati.is, 
                    family = binomial(link="probit"), 
                    weights = ifelse(energy == 1, 1, 2))
summary(fit_probit_costi0)

pred_probit_costi0 <- predict(fit_probit_costi0, newdata = dati.oos, type="response")
d.costi.oos.probit.0 <- 1*(pred_probit_costi0>0.5)

cm_probit_costi0<- table(dati.oos$energy, d.costi.oos.probit.0)
cm_probit_costi0
acc.probit_costi0 <- (cm_probit_costi0[1,1] + cm_probit_costi0[2,2])/nrow(dati.oos)
acc.probit_costi0
#0.61, 2911 FP

precision.probit_costi0 <- (cm_probit_costi0[2,2])/colSums(cm_probit_costi0)[2]
precision.probit_costi0
#0.76

##Model without correlated varibales and weights##
fit_probit_costi1 <- glm(energy ~ .-max_pressure-min_pressure-max_temp, data = dati.is, 
                         family = binomial(link="probit"), 
                         weights = ifelse(energy == 1, 1, 2))
summary(fit_probit_costi1)

pred_probit_costi1<- predict(fit_probit_costi1, newdata = dati.oos, type="response")
d.costi.oos.probit.1 <- 1*(pred_probit_costi1>0.5)

cm_probit_costi1<- table(dati.oos$energy, d.costi.oos.probit.1)
cm_probit_costi1
acc.probit_costi1 <- (cm_probit_costi1[1,1] + cm_probit_costi1[2,2])/nrow(dati.oos)
acc.probit_costi1
#0.59, 2831 FP

precision.probit_costi1 <- (cm_probit_costi0[2,2])/colSums(cm_probit_costi0)[2]
precision.probit_costi1
#0.76

# 7 - VARIABLE SELECTION ----

##we decided to use Lasso method to do variable selection, performing the 
#selection on the models with weights (because we want to have weights in our
#final model)

##Logit
lasso_model.logit <- cv.glmnet(as.matrix(dati.is[, -10]), dati.is$energy, family="binomial", weights = ifelse(dati.is$energy == 0, 1, 2), alpha = 1)
summary(lasso_model.logit)
pred_lasso.logit <- predict(lasso_model.logit, newx = as.matrix(dati.oos[, -10]), type="response")

d.lasso.logit.oos <- 1*(pred_lasso.logit>0.5)

cm.lasso.logit <- table(d.lasso.logit.oos, dati.oos$energy)
cm.lasso.logit

acc.lasso.logit <- sum(diag(cm.lasso.logit))/sum(cm.lasso.logit)
acc.lasso.logit
#accuracy 0.61
precision.lasso.logit <- (cm.lasso.logit [2,2])/colSums(cm.lasso.logit)[2]
precision.lasso.logit
#accuracy 0.92
coef(lasso_model.logit)
#dew_point_temp and max_temp exit the model
plot(lasso_model.logit)

##Probit 

lasso_model.probit <- cv.glmnet(as.matrix(dati.is[, -10]), dati.is$energy, family=binomial(link="probit"), weights = ifelse(dati.is$energy == 0, 1, 2), alpha = 1)
summary(lasso_model.probit)
pred_lasso.probit <- predict(lasso_model.probit, newx = as.matrix(dati.oos[, -10]), type="response")

d.lasso.probit.oos <- 1*(pred_lasso.probit>0.5)

cm.lasso.probit <- table(d.lasso.probit.oos, dati.oos$energy)
cm.lasso.probit

acc.lasso.probit <- sum(diag(cm.lasso.probit))/sum(cm.lasso.probit)
acc.lasso.probit
#accuracy 0.61
precision.lasso.probit <- (cm.lasso.probit [2,2])/colSums(cm.lasso.probit)[2]
precision.lasso.probit
#accuracy 0.92
coef(lasso_model.probit)
#dew_point_temp and max_temp exit the model
plot(lasso_model.probit)




# 8 - RANDOM FOREST -----------------------------------------------------------
##Try to improve using a random forest##
?randomForest
set.seed(1)
#!!IT TAKES A LOT OF TIME TO RUN!!
fit.forest <- randomForest(energy ~ ., data = dati.is, importance = T, classwt=c(1,2))

# Predict on the test data
pred.forest <- predict(fit.forest, newdata = dati.oos)
d.forest.oos <- 1*(pred.forest>0.5)

# Evaluate the model performance
cm.forest <- table(d.forest.oos, dati.oos$energy)
cm.forest
acc.forest <- sum(diag(cm.forest)) / sum(cm.forest)
acc.forest#0.71
precision.forest0 <- (cm.forest[2,2])/colSums(cm.forest)[2]
precision.forest0 #0.74

#better accuracy but lower precision than before, so we decided to use Lasso logit 
# as the best model