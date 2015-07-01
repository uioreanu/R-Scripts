############################
#
#	Analytics Edge MIT Course
#	Unit4 Trees
#
#	multiclass classification problem
#	By : Calin Uioreanu
#
############################

# source('letters.R', print.eval=T)

# read data
url <- 'http://courses.edx.org/asset-v1:MITx+15.071x_2a+2T2015+type@asset+block/letters_ABPR.csv'
letters <- read.csv(url)
#letters <- read.csv('letters_ABPR.csv')

#describe data
str(letters)

# set factor
letters$isB = as.factor(letters$letter == "B")

# Split the data
library(caTools)
set.seed(1000)
split = sample.split(letters$isB, SplitRatio = 0.5)
train = subset(letters, split==TRUE)
test = subset(letters, split==FALSE)
dim(letters); dim(train); dim(test)
#[1] 3116   18
#[1] 1558   18
#[1] 1558   18

# build a baseline model
table(letters$isB)

# so "not B" is the baseline model
# What is the accuracy of this baseline method on the test set?
table(test$isB)
1175/nrow(test)
# 0.754172

# build a classification tree to predict isB
library(rpart)
CARTb <- rpart(isB ~ . - letter, data=train, method="class")

# build prediction set
CARTp <- predict(CARTb, newdata = test, type="class")

# calculate accuracy of the model
table(CARTp, test$isB)
#FALSE  1118   43
#TRUE     57  340
(1118+340)/nrow(test)
# 0.9358151

# build a randomForest
library(randomForest)
set.seed(1000)
MODrf <- randomForest(isB ~ . - letter, data=train) # default params

# build confusion matrix
table(predict(MODrf, newdata = test), test$isB)

# calculate accuracy
(1165+374)/nrow(test)
# 0.9878049

#  random forests tends to improve on CART in terms of predictive accuracy. Sometimes, this improvement can be quite significant, as it is here.

# PREDICTING THE LETTERS A, B, P, R
letters$letter = as.factor( letters$letter )

# Split the data again
library(caTools)
set.seed(2000)
split = sample.split(letters$letter, SplitRatio = 0.5)
train = subset(letters, split==TRUE)
test = subset(letters, split==FALSE)
dim(letters); dim(train); dim(test)
#[1] 3116   18
#[1] 1558   18
#[1] 1558   18

# build baseline model
table(letters$letter)
#A   B   P   R 
#789 766 803 758 

# a simple baseline model is to predict the most frequent class of all of the options.
# so all letters must be P

table(test$letter)
#A   B   P   R 
#395 383 401 379 

# accuracy of the baseline model
401/nrow(test)
# 0.2573813

# attempt a CART classification tree
CART <- rpart(letter~. - isB, data=train, method="class")

# build confusion matrix
table(predict(CART, newdata = test, type="class"), test$letter)

# calculate accuracy
(348+318+363+340)/nrow(test)
# 0.8786906

set.seed(1000)
MODrf <- randomForest(letter ~ . - isB, data=train) # default params

# build confusion matrix
table(predict(MODrf, newdata = test), test$letter)
#A   B   P   R
#A 390   0   0   3
#B   0 380   5  12
#P   3   1 393   0
#R   2   2   3 364

# calculate accuracy
(390+380+393+364)/nrow(test)
# 0.9801027
#You should find this value rather striking, for several reasons. The first is that it is significantly higher than the value for CART, highlighting the gain in accuracy that is possible from using random forest models. The second is that while the accuracy of CART decreased significantly as we transitioned from the problem of predicting B/not B (a relatively simple problem) to the problem of predicting the four letters (certainly a harder problem), the accuracy of the random forest model decreased by a tiny amount.


