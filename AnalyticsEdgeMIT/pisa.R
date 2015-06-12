#
# 2nd part of the file, PISA Study analysis
# lm analysis in detail
#

pisaTrain<-read.csv('pisa2009train.csv')
str(pisaTrain)

pisaTest <-read.csv('pisa2009test.csv')
dim(pisaTest)

pisaTrain = na.omit(pisaTrain)
pisaTest = na.omit(pisaTest)

pisaTrain$raceeth = relevel(pisaTrain$raceeth, "White")
pisaTest$raceeth = relevel(pisaTest$raceeth, "White")

# build a large model
pisaLM<-lm(readingScore~., data=pisaTrain);

# build the standard error
SSE<-sum((pisaLM$residuals)^2)

# the training-set root-mean squared error (RMSE) of lmScore?
RMSE<-sqrt(SSE/nrow(pisaTrain))

# run prediction on the test set
predTest<-predict(pisaLM, pisaTest);

# but how good is it? we need to compute the Root mean squared error for the test set. For that we need:

# the sum of squared errors (SSE) of lmScore on the testing set
SSEtest <- sum((predTest-pisaTest$readingScore)^2)
# 5762082

#root-mean squared error (RMSE) 
# or RMSEtest<-sqrt(SSEtest/nrow(pisaTest))
RMSEtest<-sqrt(mean((predTest-pisaTest$readingScore)^2))
#76.29079

# the predicted test score used in the baseline model
Predbaseline<-mean(pisaTrain$readingScore)
#517.9629

SSTbaseline<-sum((mean(pisaTrain$readingScore)-pisaTest$readingScore)^2)
#7802354

# the test-set R-squared value of lmScore?
(1-SSEtest/SSTbaseline)
#0.2614944, lower than the R2 of the train data: 0.3251

