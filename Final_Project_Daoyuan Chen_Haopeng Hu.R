# Final Project
# Members: Daoyuan Chen & Haopeng Hu


rm(list=ls())

# Load the "attrition_data.csv"
csvfile<-file.choose()
data<-  read.csv(csvfile)

### Data preprocessing ###

# Summary data
summary(data)
sum(is.na(data))  #Total 5394 NAs. All in 'TERMINATION_YEAR'

summary(data$EMP_ID)
summary(data$ANNUAL_RATE)
summary(data$HRLY_RATE)
summary(data$JOBCODE)
summary(data$ETHNICITY)
summary(data$SEX)
summary(data$MARITAL_STATUS)
summary(data$JOB_SATISFACTION)
summary(data$AGE)
summary(data$NUMBER_OF_TEAM_CHANGED)
summary(data$REFERRAL_SOURCE)
summary(data$HIRE_MONTH)
summary(data$REHIRE)
summary(data$TERMINATION_YEAR)
summary(data$IS_FIRST_JOB)
summary(data$TRAVELLED_REQUIRED)
summary(data$PERFORMANCE_RATING)
summary(data$DISABLED_EMP)
summary(data$DISABLED_VET)
summary(data$EDUCATION_LEVEL)
summary(data$STATUS)
summary(data$JOB_GROUP)
summary(data$PREVYR_1)
summary(data$PREVYR_2)
summary(data$PREVYR_3)
summary(data$PREVYR_4)
summary(data$PREVYR_5)

# Histogram for "ANNUAL_RATE"
hist(data$ANNUAL_RATE, main = "histogram for ANNUAL_RATE")

# Histogram for "HRLY_RATE"
hist(data$HRLY_RATE, main = "histogram for HRLY_RATE")

# Convert the data type of "JOBCODE" from "numeric" to "factor"
data$JOBCODE <- as.factor(data$JOBCODE)
levels(data$JOBCODE)
summary(data$JOBCODE)

# # Replace all "" with a random ethnicity in "ETHNICITY"
ETHNICITY_random <- sample(c("AMIND", "ASIAN", "BLACK", "HISPA", "PACIF", "TWO", "WHITE"), size = 1)
ETHNICITY_random
levels(data$ETHNICITY)
data$ETHNICITY[data$ETHNICITY == " "] <- ETHNICITY_random
levels(data$ETHNICITY)
data$ETHNICITY <- factor(data$ETHNICITY)
levels(data$ETHNICITY)

# Replace all "" with "Unknown" in "REFERRAL_SOURCE"
data$REFERRAL_SOURCE[data$REFERRAL_SOURCE == ""] <- "Unknown"
summary(data)
summary(data$REFERRAL_SOURCE)

# Remove "" level from "REFERRAL_SOURCE"
levels(data$REFERRAL_SOURCE)
data$REFERRAL_SOURCE <- factor(data$REFERRAL_SOURCE)
levels(data$REFERRAL_SOURCE)

# Convert the data type of "REHIRE" from "logical" to "factor" 
data$REHIRE <- as.factor(data$REHIRE)
levels(data$REHIRE)
summary(data$REHIRE)

str(data)

# Use 30% test 70% training data
set.seed(123)
idx <- sort(sample(nrow(data),as.integer(.7*nrow(data))))
training <- data[idx,]
test <- data[-idx,]


### Predict STATUS using KNN ###
#install.packages("kknn")
library(kknn)

## k = 3
predict_k3 <- kknn(formula = STATUS ~ ANNUAL_RATE + HRLY_RATE + JOBCODE + ETHNICITY + SEX + MARITAL_STATUS + JOB_SATISFACTION + AGE + NUMBER_OF_TEAM_CHANGED + REFERRAL_SOURCE + HIRE_MONTH + REHIRE + IS_FIRST_JOB + TRAVELLED_REQUIRED + PERFORMANCE_RATING + DISABLED_EMP + DISABLED_VET + EDUCATION_LEVEL + JOB_GROUP + PREVYR_1 + PREVYR_2 + PREVYR_3 + PREVYR_4 + PREVYR_5, training, test[,-21], k = 3,kernel ="rectangular")
summary(predict_k3)
STATUS_knn_k3 <- fitted(predict_k3)

# Confusion table
table(STATUS = test$STATUS,STATUS_knn_k3 = STATUS_knn_k3)

# Compare the prediction to actual test data
test_knn_k3 <- cbind(test, STATUS_knn_k3 = STATUS_knn_k3)
View(test_knn_k3)

# Error rate
knn_k3_wrong <- sum(STATUS_knn_k3 != test$STATUS)
knn_k3_error_rate <- knn_k3_wrong/length(STATUS_knn_k3)
knn_k3_error_rate

## k = 5
predict_k5 <- kknn(formula = STATUS ~ ANNUAL_RATE + HRLY_RATE + JOBCODE + ETHNICITY + SEX + MARITAL_STATUS + JOB_SATISFACTION + AGE + NUMBER_OF_TEAM_CHANGED + REFERRAL_SOURCE + HIRE_MONTH + REHIRE + IS_FIRST_JOB + TRAVELLED_REQUIRED + PERFORMANCE_RATING + DISABLED_EMP + DISABLED_VET + EDUCATION_LEVEL + JOB_GROUP + PREVYR_1 + PREVYR_2 + PREVYR_3 + PREVYR_4 + PREVYR_5, training, test[,-21], k = 5,kernel ="rectangular")
summary(predict_k5)
STATUS_knn_k5 <- fitted(predict_k5)

# Confusion table
table(STATUS = test$STATUS,STATUS_knn_k5 = STATUS_knn_k5)

# Compare the prediction to actual test data
test_knn_k5 <- cbind(test, STATUS_knn_k5 = STATUS_knn_k5)
View(test_knn_k5)

# Error rate
knn_k5_wrong <- sum(STATUS_knn_k5 != test$STATUS)
knn_k5_error_rate <- knn_k5_wrong/length(STATUS_knn_k5)
knn_k5_error_rate

## k = 10
predict_k10 <- kknn(formula = STATUS ~ ANNUAL_RATE + HRLY_RATE + JOBCODE + ETHNICITY + SEX + MARITAL_STATUS + JOB_SATISFACTION + AGE + NUMBER_OF_TEAM_CHANGED + REFERRAL_SOURCE + HIRE_MONTH + REHIRE + IS_FIRST_JOB + TRAVELLED_REQUIRED + PERFORMANCE_RATING + DISABLED_EMP + DISABLED_VET + EDUCATION_LEVEL + JOB_GROUP + PREVYR_1 + PREVYR_2 + PREVYR_3 + PREVYR_4 + PREVYR_5, training, test[,-21], k = 10,kernel ="rectangular")
summary(predict_k10)
STATUS_knn_k10 <- fitted(predict_k10)

# Confusion table
table(STATUS = test$STATUS,STATUS_knn_k10 = STATUS_knn_k10)

# Compare the prediction to actual test data
test_knn_k10 <- cbind(test, STATUS_knn_k10 = STATUS_knn_k10)
View(test_knn_k10)

# Error rate
knn_k10_wrong <- sum(STATUS_knn_k10 != test$STATUS)
knn_k10_error_rate <- knn_k10_wrong/length(STATUS_knn_k10)
knn_k10_error_rate

### Predict STATUS using Naive Bayes ###
#install.packages('e1071', dependencies = TRUE)
library(e1071)

nBayes <- naiveBayes(STATUS ~ ANNUAL_RATE + HRLY_RATE + JOBCODE + ETHNICITY + SEX + MARITAL_STATUS + JOB_SATISFACTION + AGE + NUMBER_OF_TEAM_CHANGED + REFERRAL_SOURCE + HIRE_MONTH + REHIRE + IS_FIRST_JOB + TRAVELLED_REQUIRED + PERFORMANCE_RATING + DISABLED_EMP + DISABLED_VET + EDUCATION_LEVEL + JOB_GROUP + PREVYR_1 + PREVYR_2 + PREVYR_3 + PREVYR_4 + PREVYR_5, data = training)
STATUS_NB <- predict(nBayes, test)

# Confusion table
table(STATUS = test$STATUS,STATUS_NB = STATUS_NB)

# Compare the prediction to actual test data
test_NB <- cbind(test, STATUS_NB = STATUS_NB)
View(test_NB)

# Error rate
NB_wrong <- sum(STATUS_NB != test$STATUS)
NB_error_rate <- NB_wrong/length(STATUS_NB)
NB_error_rate

### Predict STATUS using CART ###
#install.packages("rpart")
#install.packages("rpart.plot")     # Enhanced tree plots

library(rpart)
library(rpart.plot)  			# Enhanced tree plots

CART <- rpart(STATUS ~ ANNUAL_RATE + HRLY_RATE + JOBCODE + ETHNICITY + SEX + MARITAL_STATUS + JOB_SATISFACTION + AGE + NUMBER_OF_TEAM_CHANGED + REFERRAL_SOURCE + HIRE_MONTH + REHIRE + IS_FIRST_JOB + TRAVELLED_REQUIRED + PERFORMANCE_RATING + DISABLED_EMP + DISABLED_VET + EDUCATION_LEVEL + JOB_GROUP + PREVYR_1 + PREVYR_2 + PREVYR_3 + PREVYR_4 + PREVYR_5, data = training)
rpart.plot(CART)
STATUS_CART <- predict(CART, test, type="class")

# Confusion table
table(STATUS = test$STATUS,STATUS_CART = STATUS_CART)

# Compare the prediction to actual test data
test_CART <- cbind(test, STATUS_CART = STATUS_CART)
View(test_CART)

# Error rate
CART_wrong <- sum(STATUS_CART != test$STATUS)
CART_error_rate <- CART_wrong/length(STATUS_CART)
CART_error_rate

### Predict STATUS using C50 ###
#install.packages("C50")
library(C50)

# drops <- c("TERMINATION_YEAR")
# training_C50 <- training[, !names(training) %in% drops]

C50 <- C5.0(STATUS ~ ANNUAL_RATE + HRLY_RATE + JOBCODE + ETHNICITY + SEX + MARITAL_STATUS + JOB_SATISFACTION + AGE + NUMBER_OF_TEAM_CHANGED + REFERRAL_SOURCE + HIRE_MONTH + REHIRE + IS_FIRST_JOB + TRAVELLED_REQUIRED + PERFORMANCE_RATING + DISABLED_EMP + DISABLED_VET + EDUCATION_LEVEL + JOB_GROUP + PREVYR_1 + PREVYR_2 + PREVYR_3 + PREVYR_4 + PREVYR_5, data=training)
summary(C50)
# plot(C50)
STATUS_C50 <- predict(C50, test , type="class" )

# Confusion table
table(STATUS = test$STATUS,STATUS_C50 = STATUS_C50)

# Compare the prediction to actual test data
test_C50 <- cbind(test, STATUS_C50 = STATUS_C50)
View(test_C50)

# Error rate
C50_wrong <- sum(STATUS_C50 != test$STATUS)
C50_error_rate <- C50_wrong/length(STATUS_C50)
C50_error_rate

### Predict STATUS using hclust ###
data_dist<-dist(data[,-c(1,14,21)])
hclust_results<-hclust(data_dist)
plot(hclust_results)
hclust_2<-cutree(hclust_results,2)
table(hclust_2,data[,21])

### Predict STATUS using k-means ### (error occurred)
# kmeans_2<- kmeans(data[,-c(1,14,21)],2,nstart = 10)
# kmeans_2$cluster
# table(kmeans_2$cluster,data[,21])

### Predict STATUS using k-modes ###
# install.packages("klaR")
library(klaR)
kmodes_2 <- kmodes(data[,-c(1,14,21)], 2, iter.max = 10, weighted = FALSE )
table(kmodes_2$cluster,data[,21])

### Predict STATUS using kohonen ### (need numerical data)
# install.packages("kohonen")
# library("kohonen")
# som_2<-som(as.matrix(data[,-c(1,14,21)]), grid = somgrid(2,1))
# table(cluster=som_2$unit.classif,data[,21])

### Predict STATUS using SVM ### (prediction has fewer rows than rows in test data)
# library(e1071)

# svm.model <- svm(STATUS ~ ANNUAL_RATE + HRLY_RATE + JOBCODE + ETHNICITY + SEX + MARITAL_STATUS + JOB_SATISFACTION + AGE + NUMBER_OF_TEAM_CHANGED + REFERRAL_SOURCE + HIRE_MONTH + REHIRE + IS_FIRST_JOB + TRAVELLED_REQUIRED + PERFORMANCE_RATING + DISABLED_EMP + DISABLED_VET + EDUCATION_LEVEL + JOB_GROUP + PREVYR_1 + PREVYR_2 + PREVYR_3 + PREVYR_4 + PREVYR_5, data = training)
# svm.pred <- predict(svm.model, test)
# View(svm.pred)
# 
# # Confusion table
# table(STATUS = test$STATUS,STATUS_svm = svm.pred)
# 
# # Compare the prediction to actual test data
# test_svm <- cbind(test, STATUS_svm = svm.pred)
# View(test_svm)
# 
# # Error rate
# svm_wrong <- sum(STATUS_svm != test$STATUS)
# svm_error_rate <- svm_wrong/length(STATUS_svm)
# svm_error_rate

### Predict STATUS using Random Forest ### (randomForest cannot handle categorical predictors with more than 53 categories.)
#install.packages("randomForest")
# library(randomForest)
# 
# RF_fit <- randomForest(STATUS ~ ANNUAL_RATE + HRLY_RATE + JOBCODE + ETHNICITY + SEX + MARITAL_STATUS + JOB_SATISFACTION + AGE + NUMBER_OF_TEAM_CHANGED + REFERRAL_SOURCE + HIRE_MONTH + REHIRE + IS_FIRST_JOB + TRAVELLED_REQUIRED + PERFORMANCE_RATING + DISABLED_EMP + DISABLED_VET + EDUCATION_LEVEL + JOB_GROUP + PREVYR_1 + PREVYR_2 + PREVYR_3 + PREVYR_4 + PREVYR_5, data=training, importance=TRUE, ntree =1000)
# importance(RF_fit)
# varImpPlot(RF_fit)
# STATUS_RF <- predict(RF_fit,test)
# 
# # Confusion table
# table(STATUS = test$STATUS,STATUS_RF = STATUS_RF)
# 
# # Compare the prediction to actual test data
# test_RF <- cbind(test, STATUS_RF = STATUS_RF)
# View(test_RF)
# 
# # Error rate
# RF_wrong <- sum(STATUS_RF != test$STATUS)
# RF_error_rate <- RF_wrong/length(STATUS_RF)
# RF_error_rate

### Predict STATUS using ANN ### (ANN needs all numerical data)
#install.packages("neuralnet")
# library(neuralnet)
# 
# m <- model.matrix(~ STATUS + ANNUAL_RATE + HRLY_RATE + JOBCODE + ETHNICITY + SEX + MARITAL_STATUS + JOB_SATISFACTION + AGE + NUMBER_OF_TEAM_CHANGED + REFERRAL_SOURCE + HIRE_MONTH + REHIRE + IS_FIRST_JOB + TRAVELLED_REQUIRED + PERFORMANCE_RATING + DISABLED_EMP + DISABLED_VET + EDUCATION_LEVEL + JOB_GROUP + PREVYR_1 + PREVYR_2 + PREVYR_3 + PREVYR_4 + PREVYR_5, data = training)
# head(m)
# m_ETHNICITY <- model.matrix(~ ETHNICITY, data = training)
# data_nn <- data.frame(data_nn, m_ETHNICITY)


# nn <- neuralnet(STATUST ~ ., data = m, hidden = 5, linear.output = FALSE)
# print(nn)
# plot(nn)
# nn_predict <- predict(nn, test)

# Replace all NA with "Unknown" in "TERMINATION_YEAR"(这部分麻烦你注释了，待会KNN那段按照你改后的新建列调整)
# data[is.na(data$TERMINATION_YEAR),"TERMINATION_YEAR"]<-"Unknown"
# data$TERMINATION_YEAR <- factor(data$TERMINATION_YEAR)
# summary(data)
# summary(data$TERMINATION_YEAR)

# #KNN For Data
# #A.create training and test data sets 
# index<-sort(sample(nrow( data),round(.30*nrow(data ))))
# training<- data[-index,]
# test<- data[index,]

# B.Use knn with k=1 and classify the test dataset
# library(kknn)
# predict_k1<-kknn(formula = TERMINATION_YEAR~.,training[,c(-1)] , test[,c(-1)], k=1,kernel = "rectangular")
# fit <- fitted(predict_k1)
# table(test$TERMINATION_YEAR,fit)
