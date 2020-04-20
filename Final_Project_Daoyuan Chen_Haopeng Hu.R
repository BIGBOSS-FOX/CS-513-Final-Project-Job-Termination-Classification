# Final Project
# Members: Daoyuan Chen & Haopeng Hu


rm(list=ls())

# Load the "attrition_data.csv"
csvfile<-file.choose()
data<-  read.csv(csvfile)

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

# Replace all "" with "Unknown" in "REFERRAL_SOURCE"
data$REFERRAL_SOURCE[data$REFERRAL_SOURCE == ""] <- "Unknown"
summary(data)
summary(data$REFERRAL_SOURCE)

# Remove "" level from "REFERRAL_SOURCE"
levels(data$REFERRAL_SOURCE)
data$REFERRAL_SOURCE <- factor(data$REFERRAL_SOURCE)
levels(data$REFERRAL_SOURCE)
