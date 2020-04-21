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

# Replace all "" with "Unknown" in "REFERRAL_SOURCE"
data$REFERRAL_SOURCE[data$REFERRAL_SOURCE == ""] <- "Unknown"
summary(data)
summary(data$REFERRAL_SOURCE)

# Remove "" level from "REFERRAL_SOURCE"
levels(data$REFERRAL_SOURCE)
data$REFERRAL_SOURCE <- factor(data$REFERRAL_SOURCE)
levels(data$REFERRAL_SOURCE)

# Use 30% test 70% training data
idx <- sort(sample(nrow(data),as.integer(.7*nrow(data))))
training <- data[idx,]
test <- data[-idx,]


### Predict STATUS using knn ###

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
