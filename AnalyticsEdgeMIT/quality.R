############################
#
#	Analytics Edge MIT Course
#	Unit3 Logistic Regression
#
# Medical service quality data
#	By : Calin Uioreanu
#
############################

# source('../TEST/AnalyticsEdgeMIT/flu.R', print.eval=T)

library(randomForest)
library(rpart)
library(rattle)
#install.packages("caTools")
#install.packages("ROCR")
library(caTools)
library(ROCR)


if (getwd()=='C:/Users/calin/Documents') {
  setwd('../TEST/AnalyticsEdgeMIT/')
}

# fetch source data
#url<-'http://courses.edx.org/asset-v1:MITx+15.071x_2a+2T2015+type@asset+block/quality.csv'
url<-'quality.csv'
dat<-read.csv(url)

# repetitive results
set.seed(88)

# we want to split the data into train/test, but the first question is
# how is the response variable distributed across the entire set?
prop.table(table(dat$PoorCare))

#       0         1 
#0.7480916 0.2519084 

# so a stratified sampling is required
split<-sample.split(dat$PoorCare, SplitRatio=0.75)

# now perform splitting:
train<-dat[split==TRUE,]
test<-dat[split==FALSE,]

dim(train)
#[1] 99 14
dim(test)
#[1] 32 14
dim(dat)
#[1] 131  14

# build the model using binomial logit logistic regression
dat.glm = glm(PoorCare ~ OfficeVisits + Narcotics, data=train, family=binomial)

# build a prediction, then compare it to the test set
train.predict<-predict(dat.glm, type="response")


# build the confusion matrix with a threshold of 0.3
table(train$PoorCare, train.predict>0.3)
#    FALSE TRUE
#  0    67    7
#  1    12   13
#Sensitivity = 13/25
#Specificity = 67/74


# build the confusion matrix with a threshold of 0.5
table(train$PoorCare, train.predict>0.5)
#    FALSE TRUE
#  0    70    4
#  1    15   10
#Sensitivity = 10/25
#Specificity = 70/74




# build the confusion matrix with a threshold of 0.8
table(train$PoorCare, train.predict>0.8)
#    FALSE TRUE
#  0    73    1
#  1    23    2
#Sensitivity = 2/25
#Specificity = 73/74

# build a prediction using the ROCR predictor to create the ROC plot
ROCRpred<-prediction(train.predict, train$PoorCare)
ROCRperf<-performance(ROCRpred, "tpr", "fpr")

# build the Receiver Operator Characteristic curve
plot(ROCRperf)
plot(ROCRperf, colorize=T)
plot(ROCRperf, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7))

predict.test<-predict(dat.glm, newdata=test, type="response")

# how about test data?

# compute the Area under the Curve (the ROC Curve, as a percent)
ROCRpredTest = prediction(predict.test, test$PoorCare)
auc = as.numeric(performance(ROCRpredTest, "auc")@y.values)


ROCRpred<-prediction(predict.test, test$PoorCare)
ROCRperf<-performance(ROCRpred, "tpr", "fpr")
# build the Receiver Operator Characteristic curve
plot(ROCRperf, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7))



#break

dat.rf<-randomForest(as.factor(PoorCare)~., data=dat, ntree=10000)

# dropping MemberID - unique for all records
# dropping ERVisits - corelated with DaysSinceLastERVisit and the least important according to randomForest
dat$MemberID<-NULL
dat$ERVisits<-NULL

# build decision tree
dat.tree <- rpart(as.factor(PoorCare) ~ ., data=dat, method="class")

# build overfitted decision tree 
dat.tree.of <- rpart(as.factor(PoorCare) ~ ., data=dat, method="class", control=rpart.control(minsplit=50, cp=0) )

# build random Forest over the Data
dat.rf<-randomForest(as.factor(PoorCare)~., data=dat, ntree=10000, importance=T)

# identify factors importance
importance(dat.rf)

#                     MeanDecreaseGini
#MemberID                     4.343658
#InpatientDays                1.843439
#ERVisits                     1.735386
#OfficeVisits                 4.710338
#Narcotics                    6.595360
#DaysSinceLastERVisit         3.214170
#Pain                         2.463805
#TotalVisits                  4.073536
#ProviderCount                4.497799
#MedicalClaims                3.304661
#ClaimLines                   3.906987
#StartedOnCombination         2.070972
#AcuteDrugGapSmall            6.194339

# sorting the output
dat.rf$importance[order(dat.rf$importance),]
#            ERVisits        InpatientDays StartedOnCombination                 Pain 
#            1.735386             1.843439             2.070972             2.463805 
#DaysSinceLastERVisit        MedicalClaims           ClaimLines          TotalVisits 
#            3.214170             3.304661             3.906987             4.073536 
#            MemberID        ProviderCount         OfficeVisits    AcuteDrugGapSmall 
#            4.343658             4.497799             4.710338             6.194339 
#           Narcotics 
#            6.595360 
#

fancyRpartPlot(dat.tree)
varImpPlot(dat.rf)
dat.rf$importance

#                                 0             1 MeanDecreaseAccuracy MeanDecreaseGini
#InpatientDays        -0.0006943443  0.0026957075         0.0001919211         2.223144
#OfficeVisits          0.0130171058  0.0008984626         0.0097418626         5.230845
#Narcotics             0.0192656235  0.0663816333         0.0308566620         7.453919
#DaysSinceLastERVisit  0.0012550298  0.0113710630         0.0036623188         3.973706
#Pain                 -0.0029532876 -0.0035082126        -0.0031010049         2.911217
#TotalVisits           0.0155174233 -0.0012821664         0.0111682321         4.686634
#ProviderCount         0.0033840485  0.0017025107         0.0028582637         5.136250
#MedicalClaims         0.0070730102 -0.0049601462         0.0039562980         3.807054
#ClaimLines            0.0063570225 -0.0114506871         0.0018343700         4.593661
#StartedOnCombination  0.0055358129  0.0185651133         0.0087370230         2.197343
#AcuteDrugGapSmall     0.0091833058  0.0646218519         0.0227453759         6.696708
