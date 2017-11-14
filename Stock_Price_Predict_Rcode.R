# Setting the working Directory 
#setwd("~/Insofe/Labs/PGP_Batch_31CUTE02__Instructions_and_Exam_details/data")
getwd()

# install packages 
install.packages("Amelia")
install.packages("mice")
install.packages("corrplot")
install.packages("DMwR")
install.packages("MASS")
install.packages("caret")
install.packages("glmnet")
install.packages("ggplot")
install.packages("vegan")
install.packages("Metrics")


#**********Understanding data**********

#Reading Train and Test Data sets

train_data<- read.csv("data14.csv")
test_data<- read.csv("test14.csv")

summary(train_data)
tail(train_data)
head(train_data)
str(train_data)

summary(test_data)
head(test_data)
tail(test_data)
str(test_data)



#*****************DATA PREPERATION **************** 


#removing time stamp, y1, y2 from train and test data sets
train1 <- subset(train_data,select = -c(y1,y2,timestamp))
Train_y1y2 <- subset(train_data,select = c(y1,y2))
test1<- subset(test_data,select=-c(timestamp))

#combining train and test to standardize data
train_test_combined <- rbind(train1,test1)

# standardizing data
library(vegan)
train_test_combined_standardized <- decostand(train_test_combined,"standardize")

#Visualizing data for NULL columns
library(Amelia)
missmap(train_test_combined_standardized)

#Removing NULL columns
train_test_combined_standardized_noNullcols <-train_test_combined_standardized[,!apply(train_test_combined_standardized, 2,function(x)all(is.na(x)))]
missmap(train_test_combined_standardized_noNullcols) # train_test_combined_standardized_noNullcols has no NULL columns

#Imuting NA values using central imputation method
library(DMwR)
train_test_combined_ImputeMissVals <- centralImputation(train_test_combined_standardized_noNullcols)
missmap(train_test_combined_ImputeMissVals) # train_test_combined_ImputeMissVals has no NA values

#Spliting back combined Data in to train(1769 rows) and test(30 rows)
train_data <- train_test_combined_ImputeMissVals[1:1769,]
test_data <- train_test_combined_ImputeMissVals[1770:1799,]

#combing target features y1 and y2 with train
train_data <-cbind(Train_y1y2,train_data)

#visually confirming completeness of data in train and test data sets
library(Amelia)
missmap(train_data)
missmap(test_data)

#Copying data frames to Train_Normal & Test_Normal which are cleaned up data frames and normalized -- for regression analysis. 
Train_Normal <- train_data
Test_Normal <- test_data


#Spliting Train data in to Train(80%) and Validation(20%) sets  
set.seed(786)
train_rows <- sample(x = 1:nrow(train_data), size = 0.80*nrow(train_data))
train_train <- train_data[train_rows, ]
train_validation <- train_data[-train_rows, ]


#****************MODEL BUILDING*********************

#Model 1 Linear Regression for Y1

train_notime <- subset(train_train, select = -c(y2))# removing the column y2
lm_train <- lm(formula = y1~., data = train_notime)
summary(lm_train) # model statistics
par(mfrow= c(2,2))
plot(lm_train) # plotting residuals
pred_model_lm <- predict(lm_train,train_validation)# prediction on validation data set
library(DMwR)
regr.eval(train_validation$y1, pred_model_lm) #ERROR METRICS for Linear Regression
Result_y1_lm <- predict(lm_train,Test_Normal)#predicting results in test data


#Model 2 StepAIC for Y1

library(MASS)
library(car)
lm_train_SAIC <- stepAIC(lm_train, direction = "both")

summary(lm_train_SAIC)#model statistics

pred_model_AIC <- predict(lm_train_SAIC,train_validation)#Predicted model
par(mfrow= c(2,2))
plot(lm_train_SAIC)
library(DMwR)
regr.eval(train_validation$y1, pred_model_AIC)#ERROR METRICS  for AIC Linear Regression
Result_y1_AIC <- predict(lm_train_SAIC,Test_Normal)# Predicting y1 values on test data




#Model 3 PCA with linear regression for Y1

traindata_pca <- subset(train_train, select = -c(y1))
traindata_y1 <- subset(train_train, select = c(y1))
library(caret)
pca_train <- princomp(traindata_pca)
names(pca_train)
summary(pca_train)
pca_train$loadings
train_score <- pca_train$scores
plot(pca_train,type = "lines")
screeplot(pca_train,type = "lines")
compressed_features = pca_train$scores[,1:5] # checking plot for elbow
trainFinal <- cbind(compressed_features,traindata_y1)
lm_pca <- lm(y1~.,data=trainFinal)# linear model on top of PCA  
summary(lm_pca)# model statistics


#Model 4 Time series for Y1

train_train_notime <- subset(train_train, select = -c(y2))# removing the column y2
train_validation_notime <- subset(train_train, select = -c(y2))# removing the column y2

# Visualizing times series
train_ts_model <- ts(train_train_notime$y1, frequency =365)
plot(train_ts_model)
summary(train_ts_model)

plot(train_ts_model,type="l",lwd=3,col="blue",xlab="week",ylab="Price", main="Time series plot for % change in asset price")
train_decomp <- decompose(train_ts_model)
plot(train_decomp)  # identify the trend, observed, seasonal and randomness.  

acf(train_ts_model,lag=30)
pacf(train_ts_model,lag=30)

#Building the Holt winter's model taking only Trend component. 
holt_stock_forecast <- HoltWinters(train_train_notime$y1, beta=TRUE, gamma=FALSE)
head(holt_stock_forecast$fitted)
plot(holt_stock_forecast)# Visualizing fitted or forecasted values


#Building the Holt winter's model taking only sesonality component. 

stock_holtpriceforecast <- HoltWinters(train_ts_model, beta=FALSE, gamma=TRUE,seasonal="additive")
head(stock_holtpriceforecast$fitted)
plot(stock_holtpriceforecast)# Visualizing fitted or forecasted values

summary(stock_holtpriceforecast$fitted)

par(mfrow=c(1,1))
plot(stock_holtpriceforecast$fitted, type="l", col="black")

holtforecastTrain <-data.frame(stock_holtpriceforecast$fitted)
holtforecastTrainpredictions <- holtforecastTrain$xhat
head(holtforecastTrainpredictions)

#Visualizing predictions on Training data
library("forecast")
library("stats")
stockforecast <- forecast(stock_holtpriceforecast, h=33)
plot(stockforecast,ylim=c(-.2,.2))# this model gives a better trend and seasonality



# Building ARIMA model 
library("forecast")
MODEL_ARIMA <- auto.arima(train_ts_model, ic='aic')
summary(MODEL_ARIMA)

# Performing ARIMA with different P,D,Q values
model1 <- arima(train_ts_model,c(0,0,0))
model1
model2 <- arima(train_ts_model,c(0,1,0))
model2
model3 <- arima(train_ts_model,c(0,2,0))
model3
model4 <- arima(train_ts_model,c(0,1,1))
model4

plot(model1$residuals,ylim=c(-50,50))
plot(model2$residuals,ylim=c(-50,50))
plot(model3$residuals,ylim=c(-50,50))
plot(model4$residuals,ylim=c(-50,50))

#Model diagnostics. checking the model residuals using BOX Jenkins test
Box.test(MODEL_ARIMA$residuals, lag = 2, type = "Ljung-Box") # Box test on our auto.arima model

plot(forecast(model4,h=360))




#Logistic Regression model for Y2

train_train_log <- subset(train_train,select = -c(y1))
train_validation_log <-subset(train_validation,select=-c(y1))

#Build logistic model excluding the response variable in the dataset
logmodel <- glm(y2~.,family = "gaussian",data = train_train_log)

#creating ROC Plot
library(ROCR)
prob_train_train_log <- predict(logmodel, type = "response")#predict function takes log odds values
pred_train_train <- prediction(prob_train_train_log, train_train$y2)#The prediction function takes the probability scores

#Extracting performance measures (True Positive Rate and False Positive Rate) using the "performance()" function from the ROCR package
perf <- performance(pred_train_train, measure="tpr", x.measure="fpr")

#Plot the ROC curve using the extracted performance measures (TPR and FPR) <> cutoff seen 0.5
plot(perf, col=rainbow(10), colorize=T, print.cutoffs.at=seq(0,1,0.05))

# Extract the AUC score from the ROC curve 
perf_auc <- performance(pred_train_train, measure="auc")

# Access the auc score from the performance object
auc <- perf_auc@y.values[[1]]
print(auc) 

#Choosing a Cutoff Value 0.5 as per ROC and Predicting on "validation" data set
prob_train_val <- predict(logmodel, train_validation, type = "response")
preds_train_val <- ifelse(prob_train_val > 0.5, "1", "0")

#Evaluation Metrics for logistic model classification >> Confusion Matrix
library(caret)
confusionMatrix(preds_train_val, train_validation$y2, positive = "1")

#Choose a Cutoff Value 0.5 as per ROC and Predicting on "test_data_final" data set
prob_test_val <- predict(logmodel, test_data, type = "response")
preds_test_val <- ifelse(prob_test_val > 0.5, "1", "0")
preds_test_val # outpout of predicted values on test data





#RESULTS & ANALYSIS

Results_y1_y2 <- data.frame(c(1783:1812),c(Result_y1_AIC),c(preds_test_val))
write.csv(file="Result_y1_y2.csv",Results_y1_y2)







