## Author: Dushyant Bhavsar
## Group Name: DM Assignment Grp7
## CART 



## Let us first set the working directory path

#setwd ("G:/K2_Analytics/Datafile/")
#getwd()

## Data Import
##CTDF <- read.table("HR_Employee_Attrition_Data.csv", sep = ",", header = T)
CTDF <- HR_Employee_Attrition_Data

View(CTDF)
str(CTDF)
summary(CTDF)

# converting factor into numeric
CTDF$Department <- as.numeric(as.factor(CTDF$Department))
CTDF$BusinessTravel <- as.numeric(as.factor(CTDF$BusinessTravel))
CTDF$EducationField <- as.numeric(as.factor(CTDF$EducationField))
CTDF$Gender <- as.numeric(as.factor(CTDF$Gender))
CTDF$JobRole <- as.numeric(as.factor(CTDF$JobRole))
CTDF$MaritalStatus <- as.numeric(as.factor(CTDF$MaritalStatus))
CTDF$OverTime <- as.numeric(as.factor(CTDF$OverTime))
#CTDF$Over18 <- as.numeric(as.factor(CTDF$Over18))

str(CTDF)

# Normality analysis via Histogram before scaling
par(mfrow= c(2,4))

hist(CTDF$DailyRate, main ="DailyRate",xlab="") 
hist((CTDF$Department), main = "Department",xlab="")
hist((CTDF$MonthlyIncome), main = "MonthlyIncome",xlab="")
hist(CTDF$MaritalStatus, main="MaritalStatus",xlab="")
hist(CTDF$OverTime, main ="OverTime",xlab="")
hist(CTDF$JobRole, main = "JobRole",xlab="")
hist(CTDF$TotalWorkingYears, main = "TotalWorkingYears",xlab="")
title("
      Histogram to check data distribution - Employee attrition", outer=TRUE)

# Outlier analysis via Boxplot
par(mfrow= c(2,4))

boxplot(CTDF$DailyRate, main ="DailyRate",xlab="") 
boxplot((CTDF$Department), main = "Department",xlab="")
boxplot((CTDF$MonthlyIncome), main = "MonthlyIncome",xlab="")
boxplot(CTDF$MaritalStatus, main="MaritalStatus",xlab="")
boxplot(CTDF$OverTime, main ="OverTime",xlab="")
boxplot(CTDF$JobRole, main = "JobRole",xlab="")
boxplot(CTDF$TotalWorkingYears, main = "TotalWorkingYears",xlab="")
title("
      Boxplot to check data outlier - Employee attrition", outer=TRUE)


##drop unwanted columns
rp <- c(9,10,20,22,27)
CTDF <- (CTDF[, -rp])
ncol(CTDF)

#check for missing values
table(is.na(CTDF))
colSums(is.na(CTDF))

##scaling the variables/transformation
library(data.table)
library(scales)
x <- CTDF
x<- x[,-2] #drop target varaible to avoid its scaling
CTDF.scaled <- scale(x)
CTDF.scaled <- cbind(CTDF[2], CTDF.scaled) #add target variable back
View(CTDF.scaled)


#Check skewness in data 
library(moments)
skewness(CTDF.scaled$MonthlyIncome)
skewness(CTDF.scaled$DailyRate)
skewness(CTDF.scaled$DailyRate)
skewness(CTDF.scaled$Department)
skewness(CTDF.scaled$MaritalStatus)
skewness(CTDF.scaled$TotalWorkingYears)
skewness(CTDF.scaled$JobRole)

# Normality analysis via Histogram post scaling
par(mfrow= c(2,4))

hist(CTDF.scaled$DailyRate, main ="DailyRate",xlab="") 
hist((CTDF.scaled$Department), main = "Department",xlab="")
hist((CTDF.scaled$MonthlyIncome), main = "MonthlyIncome",xlab="")
hist(CTDF.scaled$MaritalStatus, main="MaritalStatus",xlab="")
hist(CTDF.scaled$OverTime, main ="OverTime",xlab="")
hist(CTDF.scaled$JobRole, main = "JobRole",xlab="")
hist(CTDF.scaled$TotalWorkingYears, main = "TotalWorkingYears",xlab="")
title("
      Histogram to check data distribution - Employee attrition", outer=TRUE)

View(CTDF.scaled)
summary(CTDF.scaled)


## Creating Development and Validation Sample
#?set.seed
set.seed(1)
#set.seed(2940)
CTDF.scaled$random <- runif(nrow(CTDF.scaled), 0, 1);
#View(CTDF.scaled)
CTDF.scaled <- CTDF.scaled[order(CTDF.scaled$random),]
#View(CTDF.scaled)
CTDF.dev <- CTDF.scaled[which(CTDF.scaled$random <= 0.75),]
CTDF.holdout <- CTDF.scaled[which(CTDF.scaled$random > 0.75),]
c(nrow(CTDF.dev), nrow(CTDF.holdout))

# code for oversampleling 20:80
#CTDF.temp1 <- CTDF.dev[which(CTDF.dev$Target == 1),]
#CTDF.temp0 <- CTDF.dev[which(CTDF.dev$Target == 0),]
#CTDF.temp1 <- rbind(CTDF.temp1, CTDF.temp1)
#CTDF.sample1 <- CTDF.temp1[sample(nrow(CTDF.temp1),400),]
#CTDF.sample0 <- CTDF.temp0[sample(nrow(CTDF.temp0),1600),]
#CTDF.dev <- rbind(CTDF.sample0, CTDF.sample1)
#c(nrow(CTDF.dev), nrow(CTDF.holdout))


## installing rpart package for CART
## install.packages("rpart")
## install.packages("rpart.plot")


## loading the library
library(rpart)
library(rpart.plot)



## Response Rate
sum(CTDF$Target) / nrow(CTDF)
sum(CTDF.dev$Target) / nrow(CTDF.dev)
sum(CTDF.holdout$Target) / nrow(CTDF.holdout)

## setting the control paramter inputs for rpart
r.ctrl = rpart.control(minsplit=100, minbucket = 10, cp = 0, xval = 5)

## calling the rpart function to build the tree
m1 <- rpart(formula = Target ~ ., data = CTDF.dev, method = "class", control = r.ctrl)
m1

#install.packages("rattle")
#install.packages("RColorBrewer")
library(rattle)
library(RColorBrewer)
fancyRpartPlot(m1)


## to find how the tree performs
printcp(m1)
plotcp(m1)

##rattle()
## Pruning Code
ptree<- prune(m1, cp= 0.0115942029 ,"CP")
printcp(ptree)
fancyRpartPlot(ptree, uniform=TRUE,  main="Pruned Classification Tree")

## Let's use rattle to see various model evaluation measures
##rattle()

#View(CTDF.dev)
## Scoring syntax
#CTDF.dev$predict.class <- predict(m1, CTDF.dev, type="class")
#CTDF.dev$predict.score <- predict(m1, CTDF.dev)
CTDF.dev$predict.class <- predict(ptree, CTDF.dev, type="class")
CTDF.dev$predict.score <- predict(ptree, CTDF.dev)

#View(CTDF.dev)
head(CTDF.dev)


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

class(CTDF.dev$predict.score)
## deciling
CTDF.dev$deciles <- decile(CTDF.dev$predict.score[,2])
#View(CTDF.dev)

## Ranking code
##install.packages("data.table")
library(data.table)
tmp_DT = data.table(CTDF.dev)
rank <- tmp_DT[, list(
  cnt = length(Target), 
  cnt_resp = sum(Target), 
  cnt_non_resp = sum(Target == 0)) , 
  by=deciles][order(-deciles)]
rank$rrate <- round(rank$cnt_resp * 100 / rank$cnt,2);
rank$cum_resp <- cumsum(rank$cnt_resp)
rank$cum_non_resp <- cumsum(rank$cnt_non_resp)
rank$cum_perct_resp <- round(rank$cum_resp * 100 / sum(rank$cnt_resp),2);
rank$cum_perct_non_resp <- round(rank$cum_non_resp * 100 / sum(rank$cnt_non_resp),2);
rank$ks <- abs(rank$cum_perct_resp - rank$cum_perct_non_resp);

View(rank)


##install.packages("ROCR")
library(ROCR)
pred <- prediction(CTDF.dev$predict.score[,2], CTDF.dev$Target)
perf <- performance(pred, "tpr", "fpr")
plot(perf)
KS <- max(attr(perf, 'y.values')[[1]]-attr(perf, 'x.values')[[1]])
auc <- performance(pred,"auc"); 
auc <- as.numeric(auc@y.values)

##install.packages("ineq")
library(ineq)
gini = ineq(CTDF.dev$predict.score[,2], type="Gini")
auc
KS
gini

#confusion matrix for development
with(CTDF.dev, table(Target, predict.class))

View(rank)
## Syntax to get the node path
tree.path <- path.rpart(ptree, node = c(26, 27))

sum(CTDF.holdout$Target)/nrow(CTDF.holdout)


## Scoring Holdout sample
#CTDF.holdout$predict.class <- predict(m1, CTDF.holdout, type="class")
#CTDF.holdout$predict.score <- predict(m1, CTDF.holdout)
CTDF.holdout$predict.class <- predict(ptree, CTDF.holdout, type="class")
CTDF.holdout$predict.score <- predict(ptree, CTDF.holdout)

CTDF.holdout$deciles <- decile(CTDF.holdout$predict.score[,2])
#View(CTDF.holdout)

## Ranking code
##install.packages("data.table")
library(data.table)
tmp_DT = data.table(CTDF.holdout)
h_rank <- tmp_DT[, list(
  cnt = length(Target), 
  cnt_resp = sum(Target), 
  cnt_non_resp = sum(Target == 0)) , 
  by=deciles][order(-deciles)]
h_rank$rrate <- round(h_rank$cnt_resp * 100 / h_rank$cnt,2);
h_rank$cum_resp <- cumsum(h_rank$cnt_resp)
h_rank$cum_non_resp <- cumsum(h_rank$cnt_non_resp)
h_rank$cum_perct_resp <- round(h_rank$cum_resp * 100 / sum(h_rank$cnt_resp),2);
h_rank$cum_perct_non_resp <- round(h_rank$cum_non_resp * 100 / sum(h_rank$cnt_non_resp),2);
h_rank$ks <- abs(h_rank$cum_perct_resp - h_rank$cum_perct_non_resp);

#confusion matrix for holdout
with(CTDF.holdout, table(Target, predict.class))

View(h_rank)





