## Author: Dushyant Bhavsar
## Group Name: DM Assignment Grp7
## ANN


## Let us first set the working directory path

#setwd ("g:/xxxxx")
#getwd()

## Data Import
##nn <- read.table("HR_Employee_Attrition_Data.csv", sep = ",", header = T)
nn <- HR_Employee_Attrition_Data

View(nn)
str(nn)
summary(nn)

##drop unwanted columns
rp <- c(9,10,20,22,27)
nn <- (nn[, -rp])
ncol(nn)

#check for missing values
table(is.na(nn))
colSums(is.na(nn))

# converting factor into numeric
nn$Department <- as.numeric(as.factor(nn$Department))
nn$BusinessTravel <- as.numeric(as.factor(nn$BusinessTravel))
nn$EducationField <- as.numeric(as.factor(nn$EducationField))
nn$Gender <- as.numeric(as.factor(nn$Gender))
nn$JobRole <- as.numeric(as.factor(nn$JobRole))
nn$MaritalStatus <- as.numeric(as.factor(nn$MaritalStatus))
nn$OverTime <- as.numeric(as.factor(nn$OverTime))
#nn$Over18 <- as.numeric(as.factor(nn$Over18))

str(nn)

# Normality analysis via Histogram
par(mfrow= c(2,4))

hist(nn$DailyRate, main ="DailyRate",xlab="") 
hist((nn$Department), main = "Department",xlab="")
hist((nn$MonthlyIncome), main = "MonthlyIncome",xlab="")
hist(nn$MaritalStatus, main="MaritalStatus",xlab="")
hist(nn$OverTime, main ="OverTime",xlab="")
hist(nn$JobRole, main = "JobRole",xlab="")
hist(nn$TotalWorkingYears, main = "TotalWorkingYears",xlab="")
title("
      Histogram to check data distribution - Employee attrition", outer=TRUE)


##scaling the variables/transformation
library(data.table)
library(scales)
x <- nn
x<- x[,-2] #drop target varaible to avoid its scaling
nn.scaled <- scale(x)
nn.scaled <- cbind(nn[2], nn.scaled) #add target variable back
View(nn.scaled)

#Check skewness in data
library(moments)
skewness(nn.scaled$MonthlyIncome)
skewness(nn.scaled$DailyRate)
skewness(nn.scaled$DailyRate)
skewness(nn.scaled$Department)
skewness(nn.scaled$MaritalStatus)
skewness(nn.scaled$TotalWorkingYears)
skewness(nn.scaled$JobRole)


# Normality analysis via Histogram
par(mfrow= c(2,4))

hist(nn.scaled$DailyRate, main ="DailyRate",xlab="") 
hist((nn.scaled$Department), main = "Department",xlab="")
hist((nn.scaled$MonthlyIncome), main = "MonthlyIncome",xlab="")
hist(nn.scaled$MaritalStatus, main="MaritalStatus",xlab="")
hist(nn.scaled$OverTime, main ="OverTime",xlab="")
hist(nn.scaled$JobRole, main = "JobRole",xlab="")
hist(nn.scaled$TotalWorkingYears, main = "TotalWorkingYears",xlab="")
title("
      Histogram to check data distribution - Employee attrition", outer=TRUE)


##Run Linear regression to find significant independent varaibles from HR Data set
library(lmtest)
library(car)
ft <- lm(Target~ ., data = nn.scaled)
ft<- lm(Target~ .-BusinessTravel -PerformanceRating -PercentSalaryHike -JobLevel -TotalWorkingYears -DailyRate -Department -Education -EducationField -HourlyRate -StockOptionLevel -TrainingTimesLastYear -YearsWithCurrManager -YearsAtCompany -MaritalStatus, data = nn.scaled)
summary(ft)
TukeyHSD(ft)
vif(ft)
ncol(nn.scaled)

## Creating Development and Validation Sample
set.seed(1)
#set.seed(2940)
nn.scaled$random <- runif(nrow(nn.scaled), 0, 1);
#View(nn.scaled)
nn.scaled <- nn.scaled[order(nn.scaled$random),]
#View(nn.scaled)
nn.dev <- nn.scaled[which(nn.scaled$random <= 0.75),]
nn.holdout <- nn.scaled[which(nn.scaled$random > 0.75),]
c(nrow(nn.dev), nrow(nn.holdout))

# code for oversampleling 20:80
#nn.temp1 <- nn.dev[which(nn.dev$Target == 1),]
#nn.temp0 <- nn.dev[which(nn.dev$Target == 0),]
#nn.temp1 <- rbind(nn.temp1, nn.temp1)
#nn.sample1 <- nn.temp1[sample(nrow(nn.temp1),400),]
#nn.sample0 <- nn.temp0[sample(nrow(nn.temp0),1600),]
#nn.dev <- rbind(nn.sample0, nn.sample1)
#c(nrow(nn.dev), nrow(nn.holdout))

## Response Rate
sum(CTDF$Target) / nrow(CTDF)
sum(nn.dev$Target) / nrow(nn.dev)
sum(nn.holdout$Target) / nrow(nn.holdout)


#View(nn.dev)
##occ.matrix <- model.matrix(~ Occupation - 1, data = nn.dev)
##nn.dev <- data.frame(nn.dev, occ.matrix)

##Gender.matrix <- model.matrix(~ Gender - 1, data = nn.dev)
##nn.dev <- data.frame(nn.dev, Gender.matrix)

##occ.matrix <- model.matrix(~ Occupation - 1, data = nn.holdout)
##nn.holdout <- data.frame(nn.holdout, occ.matrix)

##Gender.matrix <- model.matrix(~ Gender - 1, data = nn.holdout)
##nn.holdout <- data.frame(nn.holdout, Gender.matrix)

## Installing the Neural Net package; 

## If already installed do not run the below step
##install.packages("neuralnet")
library(neuralnet)
#?"neuralnet"

#lm significant independent variable based ANN model
nn1 <- neuralnet(formula = Target ~ Age+DistanceFromHome+EnvironmentSatisfaction+Gender+JobInvolvement+JobRole+JobSatisfaction+MonthlyIncome
                 +NumCompaniesWorked+OverTime+RelationshipSatisfaction+WorkLifeBalance+YearsInCurrentRole+YearsSinceLastPromotion, 
                 data = nn.dev, 
                 hidden = 3,
                 err.fct = "sse",
                 linear.output = FALSE,
                 lifesign = "full",
                 lifesign.step = 10,
                 threshold = 0.1,
                 stepmax = 2000
                 ##startweights = startweightsObj
)

plot (nn1)


## Assigning the Probabilities to Dev Sample
nn.dev$Prob = nn1$net.result[[1]] 

## The distribution of the estimated probabilities
quantile(nn.dev$Prob, c(0,1,5,10,25,50,75,90,95,99,100)/100)
hist(nn.dev$Prob)



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
nn.dev$deciles <- decile(nn.dev$Prob)


## Ranking code
##install.packages("data.table")
library(data.table)
tmp_DT = data.table(nn.dev)
rank <- tmp_DT[, list(
  cnt = length(Target), 
  cnt_resp = sum(Target), 
  cnt_non_resp = sum(Target == 0)) , 
  by=deciles][order(-deciles)]
rank$rrate <- round (rank$cnt_resp / rank$cnt,2);
rank$cum_resp <- cumsum(rank$cnt_resp)
rank$cum_non_resp <- cumsum(rank$cnt_non_resp)
rank$cum_rel_resp <- round(rank$cum_resp / sum(rank$cnt_resp),2);
rank$cum_rel_non_resp <- round(rank$cum_non_resp / sum(rank$cnt_non_resp),2);
rank$ks <- abs(rank$cum_rel_resp - rank$cum_rel_non_resp);


library(scales)
rank$rrate <- percent(rank$rrate)
rank$cum_rel_resp <- percent(rank$cum_rel_resp)
rank$cum_rel_non_resp <- percent(rank$cum_rel_non_resp)

## Assgining 0 / 1 class based on certain threshold and View Rank
nn.dev$Class = ifelse(nn.dev$Prob>0.5,1,0)
with( nn.dev, table(Target, as.factor(Class)  ))
View(rank)

#install.packages("caret")
library(caret)
## Error Computation
sum((nn.dev$Target - nn.dev$Prob)^2)/2

## Other Model Performance Measures
#install.packages("gplots")
library(gplots)
library(ROCR)
pred <- prediction(nn.dev$Prob, nn.dev$Target)
perf <- performance(pred, "tpr", "fpr")
plot(perf)
KS <- max(attr(perf, 'y.values')[[1]]-attr(perf, 'x.values')[[1]])
auc <- performance(pred,"auc"); 
auc <- as.numeric(auc@y.values)

library(ineq)
gini = ineq(nn.dev$Prob, type="Gini")

auc
KS
gini


########################    Holdout  ########################


## Scoring another dataset using the Neural Net Model Object
## To score we will use the compute function
?compute
x <- subset(nn.holdout, 
            select = c("Age","DistanceFromHome","EnvironmentSatisfaction","Gender","JobInvolvement","JobRole","JobSatisfaction","MonthlyIncome",
                       "NumCompaniesWorked","OverTime","RelationshipSatisfaction","WorkLifeBalance","YearsInCurrentRole","YearsSinceLastPromotion")
)
x.scaled <- scale(x)
compute.output = compute(nn1, x.scaled)
compute.output = compute(nn1, x)
nn.holdout$Predict.score = compute.output$net.result
#View(nn.holdout)

quantile(nn.holdout$Predict.score, c(0,1,5,10,25,50,75,90,95,99,100)/100)
nn.holdout$deciles <- decile(nn.holdout$Predict.score)

library(data.table)
tmp_DT = data.table(nn.holdout)
h_rank <- tmp_DT[, list(
  cnt = length(Target), 
  cnt_resp = sum(Target), 
  cnt_non_resp = sum(Target == 0)) , 
  by=deciles][order(-deciles)]
h_rank$rrate <- round (h_rank$cnt_resp / h_rank$cnt,2);
h_rank$cum_resp <- cumsum(h_rank$cnt_resp)
h_rank$cum_non_resp <- cumsum(h_rank$cnt_non_resp)
h_rank$cum_rel_resp <- round(h_rank$cum_resp / sum(h_rank$cnt_resp),2);
h_rank$cum_rel_non_resp <- round(h_rank$cum_non_resp / sum(h_rank$cnt_non_resp),2);
h_rank$ks <- abs(h_rank$cum_rel_resp - h_rank$cum_rel_non_resp);


library(scales)
h_rank$rrate <- percent(h_rank$rrate)
h_rank$cum_rel_resp <- percent(h_rank$cum_rel_resp)
h_rank$cum_rel_non_resp <- percent(h_rank$cum_rel_non_resp)

## Assgining 0 / 1 class based on certain threshold and View Rank
nn.holdout$Class = ifelse(nn.holdout$Predict.score>0.5,1,0)
with( nn.holdout, table(Target, as.factor(Class)  ))

View(h_rank)



