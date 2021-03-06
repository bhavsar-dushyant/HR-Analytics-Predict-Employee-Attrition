## Author: Dushyant Bhavsar
## Group Name: DM Assignment Grp7
## Random Forest

## Let us first set the working directory path

#setwd ("g:/xxxxx")
#getwd()

## Data Import
##RFDF <- read.table("HR_Employee_Attrition_Data.csv", sep = ",", header = T)
RFDF <- HR_Employee_Attrition_Data

View(RFDF)
str(RFDF)
summary(RFDF)

# converting factor into numeric
RFDF$Department <- as.numeric(as.factor(RFDF$Department))
RFDF$BusinessTravel <- as.numeric(as.factor(RFDF$BusinessTravel))
RFDF$EducationField <- as.numeric(as.factor(RFDF$EducationField))
RFDF$Gender <- as.numeric(as.factor(RFDF$Gender))
RFDF$JobRole <- as.numeric(as.factor(RFDF$JobRole))
RFDF$MaritalStatus <- as.numeric(as.factor(RFDF$MaritalStatus))
RFDF$OverTime <- as.numeric(as.factor(RFDF$OverTime))
#RFDF$Over18 <- as.numeric(as.factor(RFDF$Over18))

str(RFDF)

# Normality analysis via Histogram
par(mfrow= c(2,4))

hist(RFDF$DailyRate, main ="DailyRate",xlab="") 
hist((RFDF$Department), main = "Department",xlab="")
hist((RFDF$MonthlyIncome), main = "MonthlyIncome",xlab="")
hist(RFDF$MaritalStatus, main="MaritalStatus",xlab="")
hist(RFDF$OverTime, main ="OverTime",xlab="")
hist(RFDF$JobRole, main = "JobRole",xlab="")
hist(RFDF$TotalWorkingYears, main = "TotalWorkingYears",xlab="")
title("
      Histogram to check data distribution - Employee attrition", outer=TRUE)

# Outlier analysis via Boxplot
par(mfrow= c(2,4))

boxplot(RFDF$DailyRate, main ="DailyRate",xlab="") 
boxplot((RFDF$Department), main = "Department",xlab="")
boxplot((RFDF$MonthlyIncome), main = "MonthlyIncome",xlab="")
boxplot(RFDF$MaritalStatus, main="MaritalStatus",xlab="")
boxplot(RFDF$OverTime, main ="OverTime",xlab="")
boxplot(RFDF$JobRole, main = "JobRole",xlab="")
boxplot(RFDF$TotalWorkingYears, main = "TotalWorkingYears",xlab="")
title("
      Boxplot to check data outlier - Employee attrition", outer=TRUE)

##drop unwanted columns
rp <- c(9,10,20,22,27)
RFDF <- (RFDF[, -rp])
ncol(RFDF)

#check for missing values
table(is.na(RFDF))
colSums(is.na(RFDF))

##scaling the variables/transformation
library(data.table)
library(scales)
x <- RFDF
x<- x[,-2] #drop target varaible to avoid its scaling
x
RFDF.scaled <- scale(x)
RFDF.scaled <- cbind(RFDF[2], RFDF.scaled) #add target variable back
View(RFDF.scaled)


#Check skewness in data
library(moments)
skewness(RFDF.scaled$MonthlyIncome)
skewness(RFDF.scaled$DailyRate)
skewness(RFDF.scaled$DailyRate)
skewness(RFDF.scaled$Department)
skewness(RFDF.scaled$MaritalStatus)
skewness(RFDF.scaled$TotalWorkingYears)
skewness(RFDF.scaled$JobRole)


# Normality analysis via Histogram
par(mfrow= c(2,4))

hist(RFDF.scaled$DailyRate, main ="DailyRate",xlab="") 
hist((RFDF.scaled$Department), main = "Department",xlab="")
hist((RFDF.scaled$MonthlyIncome), main = "MonthlyIncome",xlab="")
hist(RFDF.scaled$MaritalStatus, main="MaritalStatus",xlab="")
hist(RFDF.scaled$OverTime, main ="OverTime",xlab="")
hist(RFDF.scaled$JobRole, main = "JobRole",xlab="")
hist(RFDF.scaled$TotalWorkingYears, main = "TotalWorkingYears",xlab="")
title("
      Histogram to check data distribution - Employee attrition", outer=TRUE)


View(RFDF.scaled)
summary(RFDF.scaled)


## Creating Development and Validation Sample
set.seed(1)
#set.seed(2940)
RFDF.scaled$random <- runif(nrow(RFDF.scaled), 0, 1);
#View(RFDF.scaled)
RFDF.scaled <- RFDF.scaled[order(RFDF.scaled$random),]
#View(RFDF.scaled)
RFDF.dev <- RFDF.scaled[which(RFDF.scaled$random <= 0.75),]
RFDF.holdout <- RFDF.scaled[which(RFDF.scaled$random > 0.75),]
c(nrow(RFDF.dev), nrow(RFDF.holdout))

# code for oversampleling 20:80
#RFDF.temp1 <- RFDF.dev[which(RFDF.dev$Target == 1),]
#RFDF.temp0 <- RFDF.dev[which(RFDF.dev$Target == 0),]
#RFDF.temp1 <- rbind(RFDF.temp1, RFDF.temp1)
#RFDF.sample1 <- RFDF.temp1[sample(nrow(RFDF.temp1),400),]
#RFDF.sample0 <- RFDF.temp0[sample(nrow(RFDF.temp0),1600),]
#RFDF.dev <- rbind(RFDF.sample0, RFDF.sample1)
#c(nrow(RFDF.dev), nrow(RFDF.holdout))


## Response Rate
sum(CTDF$Target) / nrow(CTDF)
sum(RFDF.dev$Target) / nrow(RFDF.dev)
sum(RFDF.holdout$Target) / nrow(RFDF.holdout)

#install.packages("randomForest")
library(randomForest)
#?randomForest
## Calling syntax to build the Random Forest

RF <- randomForest(as.factor(Target) ~ ., 
                   data = RFDF.dev, 
                   ntree=501, mtry = 3, nodesize = 10,
                   importance=TRUE)

#RF <- randomForest(as.factor(Target) ~. -BusinessTravel+Department+EducationField+Gender+JobRole+MaritalStatus+Over18+OverTime, 
#                   data = RFDF.dev[,-1], 
#                   ntree=501, mtry = 3, nodesize = 10,
#                   importance=TRUE)
print(RF)

plot(RF, main="")
legend("topright", c("OOB", "0", "1"), text.col=1:6, lty=1:3, col=1:3)
title(main="Error Rates Random Forest RFDF.dev")


RF$err.rate
##class(randomForest::importance(RF))
## List the importance of the variables.
##impVar <- round(randomForest::importance(RF), 2)
##impVar[order(impVar[,3], decreasing=TRUE),]

## Tuning Random Forest
tRF <- tuneRF(x = RFDF.dev[,-1], 
              y=as.factor(RFDF.dev$Target),
              mtryStart = 3, 
              ntreeTry=101, 
              stepFactor = 1.5, 
              improve = 0.001, 
              trace=TRUE, 
              plot = TRUE,
              doBest = TRUE,
              nodesize = 10, 
              importance=TRUE
              )


tRF$importance
## Scoring syntax
RFDF.dev$predict.class <- predict(tRF, RFDF.dev, type="class")
RFDF.dev$predict.score <- predict(tRF, RFDF.dev, type="prob")
head(RFDF.dev)



## deciling code
decile <- function(x){
  deciles <- vector(length=10)
  for (i in seq(0.1,1,.1)){
    deciles[i*10] <- quantile(x, i, na.rm=T)
  }
  return (
    ifelse(x<deciles[1], 1,
           ifelse(x<deciles[2], 2,
                  ifelse(x<deciles[3], 3,
                         ifelse(x<deciles[4], 4,
                                ifelse(x<deciles[5], 5,
                                       ifelse(x<deciles[6], 6,
                                              ifelse(x<deciles[7], 7,
                                                     ifelse(x<deciles[8], 8,
                                                            ifelse(x<deciles[9], 9, 10
                                                            ))))))))))
}

## deciling
RFDF.dev$deciles <- decile(RFDF.dev$predict.score[,2])


## Ranking code
library(data.table)
tmp_DT = data.table(RFDF.dev)
rank <- tmp_DT[, list(
  cnt = length(Target), 
  cnt_resp = sum(Target), 
  cnt_non_resp = sum(Target == 0)) , 
  by=deciles][order(-deciles)]
rank$rrate <- round(rank$cnt_resp * 100 / rank$cnt,2);
rank$cum_resp <- cumsum(rank$cnt_resp)
rank$cum_non_resp <- cumsum(rank$cnt_non_resp)
rank$cum_rel_resp <- round(rank$cum_resp / sum(rank$cnt_resp),2);
rank$cum_rel_non_resp <- round(rank$cum_non_resp / sum(rank$cnt_non_resp),2);
rank$ks <- abs(rank$cum_rel_resp - rank$cum_rel_non_resp);
View(rank)



library(ROCR)
pred <- prediction(RFDF.dev$predict.score[,2], RFDF.dev$Target)
perf <- performance(pred, "tpr", "fpr")
plot(perf)
KS <- max(attr(perf, 'y.values')[[1]]-attr(perf, 'x.values')[[1]])
auc <- performance(pred,"auc"); 
auc <- as.numeric(auc@y.values)

library(ineq)
gini = ineq(RFDF.dev$predict.score[,2], type="Gini")
auc
KS
gini

#confusion matrix for development
with(RFDF.dev, table(Target, predict.class))


## Scoring syntax
RFDF.holdout$predict.class <- predict(tRF, RFDF.holdout, type="class")
RFDF.holdout$predict.score <- predict(tRF, RFDF.holdout, type="prob")
with(RFDF.holdout, table(Target, predict.class))

RFDF.holdout$deciles <- decile(RFDF.holdout$predict.score[,2])
tmp_DT = data.table(RFDF.holdout)
h_rank <- tmp_DT[, list(
  cnt = length(Target), 
  cnt_resp = sum(Target), 
  cnt_non_resp = sum(Target == 0)) , 
  by=deciles][order(-deciles)]
h_rank$rrate <- round(h_rank$cnt_resp * 100 / h_rank$cnt,2);
h_rank$cum_resp <- cumsum(h_rank$cnt_resp)
h_rank$cum_non_resp <- cumsum(h_rank$cnt_non_resp)
h_rank$cum_rel_resp <- round(h_rank$cum_resp / sum(h_rank$cnt_resp),2);
h_rank$cum_rel_non_resp <- round(h_rank$cum_non_resp / sum(h_rank$cnt_non_resp),2);
h_rank$ks <- abs(h_rank$cum_rel_resp - h_rank$cum_rel_non_resp);

#confusion matrix for holdout
with(RFDF.holdout, table(Target, predict.class))

View(h_rank)

