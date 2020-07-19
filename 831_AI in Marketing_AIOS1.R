###################################
# Code for the Sales Prediction - Short Version
# Mahesh Kumar
# Based on code provided by @ Ceren Kolsarici
#################################################################

####### Load data from an external link
#adv_sales.df <- read.csv("http://t.ly/P2pa")
# or load data from your computer by browsing
adv_sales.df <- read.csv(file.choose())
####### Display the structure of the R object                   
str(adv_sales.df)

#removing Serial number column (1st column)
adv_sales_con.df <- subset(adv_sales.df, select = -c(1))

#Split test and train data
train.df<-adv_sales_con.df[1:750,]
test.df<-adv_sales_con.df[751:1000,]

###### Preliminary Data Inspection

# basic descriptive statistics 
summary(adv_sales_con.df)

# check basic data suitability
# generalized pair graphs to check for bivariate correlations
install.packages("gpairs") # only run once
library(gpairs)
#require(gpairs)

gpairs(adv_sales_con.df)

# NOTE: if using RStudio, it can be helpful to "Clear All" plots if a plot
# appears too small or too large; 
# this is a known issue in RStudio with various packages such as corrplot
#
install.packages("corrplot") # only run once
library(corrplot)
datamatrix <- cor(adv_sales_con.df)
corrplot(datamatrix, method='number')

#pairwise correlation
install.packages("ppcor")
library(ppcor)
pcor(adv_sales_con.df, method='pearson')

corrplot.mixed(cor(adv_sales_con.df[ , c(1:7)]), lower = "ellipse", upper="circle")

# The goal of a sales prediction analysis is to discover relationships between sales'
# numbers with features of factors.
# For example, to what extent is billboard  with the billboard related to
# total sales? Is the relationship strong or weak?

# Bi-variate plots
plot(sales~billboard, data=adv_sales_con.df, xlab="Sales with billboard", ylab="Overall Sales")

# Fitting a model with a single predictor
sales_model<-lm(sales~billboard, data=adv_sales_con.df)
str(sales_model)
sales_model$coefficients
# (Intercept)   billboard 
#6334.32620    11.21619 
summary(sales_model)

# R2 in the model fit equals the squared correlation for single predictor models
# In linear least squares regression with an estimated intercept term, R2 equals the square of the Pearson correlation coefficient between the observed y and modeled (predicted) f data values 
# Let's check that
cor(adv_sales_con.df$sales, adv_sales_con.df$billboard)^2
#[1] 0.5602392
# # F-statistic compares the model fit to the mean of the outcome variable
plot(sales~billboard, data=adv_sales_con.df,
     xlab="Sales with Billboard", ylab="Overall Sales")
abline(sales_model, col='blue')

# ####
# # confidence intervals-  finds the confidence intervals by interpolation in the profile traces
confint(sales_model)

##### Checking for Model Assumptions 
par(mfrow=c(2,2)) # partition the screen

plot(sales_model)
#=====================================================================================================
# Fitting a model with a single predictor- Store to sales
store_model<-lm(sales~store, data=adv_sales_con.df)
str(store_model)
store_model$coefficients
# (Intercept)   store 
#2643.8286    7.4977
summary(store_model)

# R2 in the model fit equals the squared correlation for single predictor models
# In linear least squares regression with an estimated intercept term, R2 equals the square of the Pearson correlation coefficient between the observed y and modeled (predicted) f data values 
# Let's check that
cor(adv_sales_con.df$sales, adv_sales_con.df$store)^2
#[1] 0.2586251
# # F-statistic compares the model fit to the mean of the outcome variable
plot(sales~store, data=adv_sales_con.df,
     xlab="Sales with Store", ylab="Overall Sales")
abline(store_model, col='blue')

# ####
# # confidence intervals-  finds the confidence intervals by interpolation in the profile traces
confint(store_model)

##### Checking for Model Assumptions 
par(mfrow=c(2,2)) # partition the screen

plot(store_model)

#=====================================================================================================
# Fitting a model with a single predictor- Printout to sales
sales_printout<-lm(sales~printout, data=adv_sales_con.df)
str(sales_printout)
sales_printout$coefficients
#  (Intercept)      printout 
#17806.2924586    -0.2735993 
summary(sales_printout)

# R2 in the model fit equals the squared correlation for single predictor models
# In linear least squares regression with an estimated intercept term, R2 equals the square of the Pearson correlation coefficient between the observed y and modeled (predicted) f data values 
# Let's check that
cor(adv_sales_con.df$sales, adv_sales_con.df$printout)^2
#[1] 0.0003548302
# # F-statistic compares the model fit to the mean of the outcome variable
plot(sales~printout, data=adv_sales_con.df,
     xlab="Sales with printout", ylab="Overall Sales")
abline(sales_printout, col='blue')

# ####
# # confidence intervals-  finds the confidence intervals by interpolation in the profile traces
confint(sales_printout)

##### Checking for Model Assumptions 
par(mfrow=c(2,2)) # partition the screen

plot(sales_printout)

#=========================================================================================================
# Fitting a model with a single predictor- Sales based on variable-price
m1<-lm(sales~price, data=adv_sales_con.df)
str(m1)
m1$coefficients
# (Intercept)       price 
#35862.645    -183.238 
summary(m1)
cor(adv_sales_con.df$sales, adv_sales_con.df$price)^2
#[1] 0.05867918
# # F-statistic compares the model fit to the mean of the outcome variable
plot(sales~price, data=adv_sales_con.df, xlab="Sales with price", ylab="Overall Sales")
abline(m1, col='blue')

# # confidence intervals-  finds the confidence intervals by interpolation in the profile traces
confint(m1)

##### Checking for Model Assumptions 
par(mfrow=c(2,2)) # partition the screen

plot(m1)
#=========================================================================================================
# Fitting a model with a 2 predictors- Sales based on variable-price,store
m2<-lm(sales~price+store, data =adv_sales_con.df)
str(m2)
m2$coefficients
# (Intercept)        price        store 
#21736.373702  -193.371523     7.595689 

summary(m2)

cor(adv_sales_con.df$sales, adv_sales_con.df$price+adv_sales_con.df$store)^2
#[1] 0.2534921
# F-statistic compares the model fit to the mean of the outcome variable
plot(sales~ price+store, data=adv_sales_con.df, xlab="Sales with price and store", ylab="Overall Sales")
abline(m2, col='blue')


# # confidence intervals-  finds the confidence intervals by interpolation in the profile traces
confint(m2)

##### Checking for Model Assumptions 
par(mfrow=c(2,2)) # partition the screen

plot(m2)

#=========================================================================================================
#*** price, store, billboard***

# Fitting a model with a 3 predictors- Sales based on variable-price,store,billboard
m3<-lm(sales~price+store+billboard, data =adv_sales_con.df)
str(m3)
m3$coefficients
# (Intercept)        price        store 
#21736.373702  -193.371523     7.595689 

summary(m3)

cor(adv_sales_con.df$sales, adv_sales_con.df$price + adv_sales_con.df$store + adv_sales_con.df$billboard)^2
#[1] 0.7391821
# F-statistic compares the model fit to the mean of the outcome variable
plot(sales~price+store+billboard, data=adv_sales_con.df, xlab="Sales with price, store and billboard", ylab="Overall Sales")
abline(m3, col='blue')

# ####
# # confidence intervals-  finds the confidence intervals by interpolation in the profile traces
confint(m3)

##### Checking for Model Assumptions 
par(mfrow=c(2,2)) # partition the screen

plot(m3)
#=========================================================================================================
# Fitting a model with a 4 predictors- Sales based on variable-price,store, billboard, printout
m4<-lm(sales~price+store+billboard+printout, data =adv_sales_con.df)
str(m4)
m4$coefficients
# (Intercept)        price        store 
#21736.373702  -193.371523     7.595689 
plot(sales~price+store+billboard+printout, data=adv_sales_con.df, xlab="Sales with price, store, billboard,printout", ylab="Overall Sales")
abline(m4, col='blue')

summary(m4)

cor(adv_sales_con.df$sales, adv_sales_con.df$price+adv_sales_con.df$store+adv_sales_con.df$billboard+adv_sales_con.df$printout)^2
#[1] 0.4851647
# F-statistic compares the model fit to the mean of the outcome variable

#0.4851647
# # confidence intervals-  finds the confidence intervals by interpolation in the profile traces
confint(m4)

##### Checking for Model Assumptions 
par(mfrow=c(2,2)) # partition the screen

plot(m4)
#=========================================================================================================
# Fitting a model with a 5 predictors- Sales based on variable-price,store, billboard, printout, Satisfaction
m5<-lm(sales~price+store+billboard+printout+sat, data =adv_sales_con.df)
str(m5)
m5$coefficients
# (Intercept)        price        store 
#21736.373702  -193.371523     7.595689 

summary(m5)

cor(adv_sales_con.df$sales, adv_sales_con.df$price+adv_sales_con.df$store+adv_sales_con.df$billboard+adv_sales_con.df$printout+adv_sales_con.df$sat)^2
#[1] 0.4892868
# F-statistic compares the model fit to the mean of the outcome variable
plot(sales~price+store+billboard+printout+sat, data=adv_sales_con.df, xlab="Sales with price and store", ylab="Overall Sales")
abline(m5, col='blue')

# # confidence intervals-  finds the confidence intervals by interpolation in the profile traces
confint(m5)

##### Checking for Model Assumptions 
par(mfrow=c(2,2)) # partition the screen

plot(m5)

#=========================================================================================================
# Fitting a model with a 6 predictors- Sales based on variable-price,store, billboard, printout, satisfaction,competition 
m6<-lm(sales~price+store+billboard+printout+sat+comp, data =adv_sales_con.df)
str(m6)
m6$coefficients
# (Intercept)        price        store 
#21736.373702  -193.371523     7.595689 

summary(m6)

cor(adv_sales_con.df$sales, adv_sales_con.df$price+adv_sales_con.df$store+adv_sales_con.df$billboard+adv_sales_con.df$printout+adv_sales_con.df$sat+adv_sales_con.df$comp)^2
#[1] 0.2534921
# F-statistic compares the model fit to the mean of the outcome variable
plot(sales~price+store+billboard+printout+sat+comp, data=adv_sales_con.df, xlab="Sales with price and store", ylab="Overall Sales")
abline(m6, col='blue')

# # confidence intervals-  finds the confidence intervals by interpolation in the profile traces
confint(m6)

##### Checking for Model Assumptions 
par(mfrow=c(2,2)) # partition the screen

plot(m6)

#=========================================================================================================
##### Interactions
# Fitting a model with a 7 predictors- Sales based on variable-price,store, billboard, printout, satisfaction,competition and Interactions of Ad spending channels
m7<-lm(sales~price + store + billboard + printout + sat + comp + store:billboard + store:printout + billboard:printout, data =adv_sales_con.df)
str(m7)
m7$coefficients
# (Intercept)        price        store 
#21736.373702  -193.371523     7.595689 

summary(m7)

cor(adv_sales_con.df$sales, adv_sales_con.df$price+adv_sales_con.df$store+adv_sales_con.df$billboard+adv_sales_con.df$printout+adv_sales_con.df$sat+adv_sales_con.df$comp+adv_sales_con.df$store:adv_sales_con.df$billboard + adv_sales_con.df$store:adv_sales_con.df$printout + adv_sales_con.df$billboard:adv_sales_con.df$printout)^2
#[1] 0.2534921
# F-statistic compares the model fit to the mean of the outcome variable
plot(sales~price+store+billboard+printout+sat+comp+store:billboard + store:printout + billboard:printout, data=adv_sales_con.df, xlab="Sales with price,store and otherr variables including interaction+", ylab="Overall Sales")
abline(m7, col='blue')

# # confidence intervals-  finds the confidence intervals by interpolation in the profile traces
confint(m7)

##### Checking for Model Assumptions 
par(mfrow=c(2,2)) # partition the screen

plot(m7)
#=========================================================================================================
# Comparing Models
summary(m1)$r.squared
summary(m2)$r.squared
summary(m3)$r.squared
summary(m4)$r.squared
summary(m5)$r.squared
summary(m6)$r.squared
summary(m7)$r.squared
summary(m1)$adj.r.squared
summary(m2)$adj.r.squared
summary(m3)$adj.r.squared
summary(m4)$adj.r.squared
summary(m5)$adj.r.squared
summary(m6)$adj.r.squared
summary(m7)$adj.r.squared
#==============================================================================
#Prediction
m1.train<-lm(sales~price, data=train.df) # "calibrate" model1 on the training sample
m2.train<-lm(sales~price+store, data =train.df) # "calibrate" model2 on the training sample
m3.train<-lm(sales~price+store+billboard, data =train.df) # "calibrate" model3 on the training sample
m4.train<-lm(sales~price+store+billboard+printout, data =train.df) # "calibrate" model4 on the training sample
m5.train<-lm(sales~price+store+billboard+printout+sat, data =train.df) # "calibrate" model5 on the training sample
m6.train<-lm(sales~price+store+billboard+printout+sat+comp, data =train.df) # "calibrate" model6 on the training sample
m7.train<-lm(sales~price+store, data =train.df) # "calibrate" model7 on the training sample

summary(m1.train)$r.squared
summary(m2.train)$r.squared
summary(m3.train)$r.squared
summary(m4.train)$r.squared
summary(m5.train)$r.squared
summary(m6.train)$r.squared
summary(m7.train)$r.squared

m1.test<-predict(m1.train,test.df)
m2.test<-predict(m2.train,test.df)
m3.test<-predict(m3.train,test.df)
m4.test<-predict(m4.train,test.df)
m5.test<-predict(m5.train,test.df)
m6.test<-predict(m6.train,test.df)
m7.test<-predict(m7.train,test.df)

# Compute R-squared in the test sample 
# R-squared = Explained variation / Total variation

SSE1 = sum((test.df$sales - m1.test)^2) # Explained variation
SST1 = sum((test.df$sales - mean(test.df$sales))^2) # Total Variation
Rsq1 =1 - SSE/SST

cor(test.df$sales, m1.test)^2

SSE2 = sum((test.df$sales - m2.test)^2) # Explained variation
SST2 = sum((test.df$sales - mean(test.df$sales))^2) # Total Variation
Rsq2 =1 - SSE2/SST2

cor(test.df$sales, m2.test)^2

SSE3 = sum((test.df$sales - m3.test)^2) # Explained variation
SST3 = sum((test.df$sales - mean(test.df$sales))^2) # Total Variation
Rsq3 =1 - SSE3/SST3

cor(test.df$sales, m3.test)^2

SSE4 = sum((test.df$sales - m4.test)^2) # Explained variation
SST4 = sum((test.df$sales - mean(test.df$sales))^2) # Total Variation
Rsq4 =1 - SSE4/SST4

cor(test.df$sales, m4.test)^2

SSE5 = sum((test.df$sales - m5.test)^2) # Explained variation
SST5 = sum((test.df$sales - mean(test.df$sales))^2) # Total Variation
Rsq5 =1 - SSE5/SST5

cor(test.df$sales, m5.test)^2

SSE6 = sum((test.df$sales - m6.test)^2) # Explained variation
SST6 = sum((test.df$sales - mean(test.df$sales))^2) # Total Variation
Rsq6 =1 - SSE6/SST6

cor(test.df$sales, m6.test)^2

SSE7 = sum((test.df$sales - m7.test)^2) # Explained variation
SST7 = sum((test.df$sales - mean(test.df$sales))^2) # Total Variation
Rsq7 =1 - SSE7/SST7

cor(test.df$sales, m7.test)^2