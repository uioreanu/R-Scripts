############################
#
#	Analytics Edge MIT Course
#	Unit4 Trees
#
#	PREDICTING EARNINGS FROM CENSUS DATA
#	By : Calin Uioreanu
#
############################

# source('census.R', print.eval=T)

# read data
url <- 'http://courses.edx.org/asset-v1:MITx+15.071x_2a+2T2015+type@asset+block/census.csv'
census <- read.csv(url)
#census <- read.csv('census.csv')

#describe data
str(census)
#'data.frame':	31978 obs. of  13 variables:
#$ age          : int  39 50 38 53 28 37 49 52 31 42 ...
#$ workclass    : Factor w/ 9 levels " ?"," Federal-gov",..: 8 7 5 5 5 5 5 7 5 5 ...
#$ education    : Factor w/ 16 levels " 10th"," 11th",..: 10 10 12 2 10 13 7 12 13 10 ...
#$ maritalstatus: Factor w/ 7 levels " Divorced"," Married-AF-spouse",..: 5 3 1 3 3 3 4 3 5 3 ...
#$ occupation   : Factor w/ 15 levels " ?"," Adm-clerical",..: 2 5 7 7 11 5 9 5 11 5 ...
#$ relationship : Factor w/ 6 levels " Husband"," Not-in-family",..: 2 1 2 1 6 6 2 1 2 1 ...
#$ race         : Factor w/ 5 levels " Amer-Indian-Eskimo",..: 5 5 5 3 3 5 3 5 5 5 ...
#$ sex          : Factor w/ 2 levels " Female"," Male": 2 2 2 2 1 1 1 2 1 2 ...
#$ capitalgain  : int  2174 0 0 0 0 0 0 0 14084 5178 ...
#$ capitalloss  : int  0 0 0 0 0 0 0 0 0 0 ...
#$ hoursperweek : int  40 13 40 40 40 40 16 45 50 40 ...
#$ nativecountry: Factor w/ 41 levels " Cambodia"," Canada",..: 39 39 39 39 5 39 23 39 39 39 ...
#$ over50k      : Factor w/ 2 levels " <=50K"," >50K": 1 1 1 1 1 1 1 2 2 2 ...

# Split the data
library(caTools)
set.seed(2000)
split = sample.split(census$over50k, SplitRatio = 0.6)
train = subset(census, split==TRUE)
test = subset(census, split==FALSE)
dim(census); dim(train); dim(test)
#[1] 31978    13
#[1] 19187    13
#[1] 12791    13

# build a logistic regression model to predict the dependent variable "over50k", using all of the other variables in the dataset as independent variables
MODglm <- glm(formula = over50k ~ ., data = train, family = "binomial")

# Which variables are significant, or have factors that are significant?
# Use 0.1 as your significance threshold, so variables with a period or dot in the stars column should be counted too. 
round(coef(summary(MODglm))[,4],4)

# confusion matrix
table(test$over50k, (predict(object = MODglm, newdata = test))>0.5)
#FALSE TRUE
#<=50K  9351  362
#>50K   1563 1515

# What is the accuracy of the model on the testing set?
(9351+1515)/nrow(test)
# 0.8495036

# What is the baseline accuracy for the testing set?
sum(test$over50k==' <=50K')/nrow(test)
# 0.7593621

# What is the area-under-the-curve (AUC) for this model on the test set?
library(ROCR)
ROCRpred = prediction(predict(MODglm, newdata=test, type="response"), test$over50k)
as.numeric(performance(ROCRpred, "auc")@y.values)
#[1] 0.9061598
ROCRperf = performance(ROCRpred, "tpr", "fpr")
plot(ROCRperf, main="Regression glm", colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7))

# A CART MODEL
library(rpart)
CARTm <- rpart(formula = over50k ~ ., data = train, method = "class")
plot(CARTm); text(CARTm)

# precision matrix
table(predict(CARTm, newdata = test)[,2]>0.5, test$over50k)
#<=50K  >50K
#FALSE   9243  1482
#TRUE     470  1596

# accuracy on the test data
(9243+1596)/nrow(test)
# 0.8473927

#This highlights a very regular phenomenon when comparing CART and logistic regression. CART often performs a little worse than logistic regression in out-of-sample accuracy. However, as is the case here, the CART model is often much simpler to describe and understand.

# build ROC curve
ROCRpred = prediction(predict(CARTm, newdata=test)[,2], test$over50k)
as.numeric(performance(ROCRpred, "auc")@y.values)
#[1] 0.8470256
ROCRperf = performance(ROCRpred, "tpr", "fpr")
plot(ROCRperf, main="CART model ROC Curve", colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7))

# Random Forest model
set.seed(1)
trainSmall = train[sample(nrow(train), 2000), ]

# build randomForest
set.seed(1)
MODrf <- randomForest(formula = over50k ~ ., data = trainSmall)

# confusion matrix
table(predict(MODrf, newdata = trainSmall), trainSmall$over50k)
#<=50K  >50K
#<=50K   1514   227
#>50K       0   259

# accuracy
(1514+259)/nrow(trainSmall)
# 0.8865

# the number of times, aggregated over all of the trees in the random forest model, 
# that a certain variable is selected for a split. 
vu = varUsed(MODrf, count=TRUE)
vusorted = sort(vu, decreasing = FALSE, index.return = TRUE)
dotchart(vusorted$x, names(MODrf$forest$xlevels[vusorted$ix]))

#mean reduction in impurity
varImpPlot(MODrf)

#how CART behaves with different choices of its parameters
library(caret)
set.seed(2)
cpGrid = expand.grid( .cp = seq(0.002,0.1,0.002))

# Define cross-validation experiment
numFolds = trainControl( method = "cv", number = 10 )

# Perform the cross validation
train(over50k ~ ., data = train, method = "rpart", trControl = numFolds, tuneGrid = cpGrid )

#Accuracy was used to select the optimal model using  the largest value.
#The final value used for the model was cp = 0.002. 

# Fit a CART model to the training data using this value of cp. 
CARTn <- rpart(formula = over50k ~ ., data = train, cp = 0.002)

# build confusion matrix
table(predict(CARTn, newdata = test)[,2]>0.5, test$over50k)
#<=50K  >50K
#FALSE   9178  1240
#TRUE     535  1838

# What is the prediction accuracy on the test set?
(9178+1838)/nrow(test)
# 0.8612306

library(rpart.plot)
prp(CARTn)
#This highlights one important tradeoff in building predictive models. By tuning cp, we improved our accuracy by over 1%, but our tree became significantly more complicated. In some applications, such an improvement in accuracy would be worth the loss in interpretability. In others, we may prefer a less accurate model that is simpler to understand and describe over a more accurate -- but more complicated -- model.





