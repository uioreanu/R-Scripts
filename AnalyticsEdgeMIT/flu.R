############################
#
#	Analytics Edge MIT Course
#	Unit2 Linear Regression
#
# DETECTING FLU EPIDEMICS VIA SEARCH ENGINE QUERY DATA 
#	By : Calin Uioreanu
#
############################

# source('../TEST/AnalyticsEdgeMIT/flu.R', print.eval=T)

if (getwd()=='C:/Users/calin/Documents') {
  setwd('../TEST/AnalyticsEdgeMIT/')
}

# read train data from HTTP
url<-'http://courses.edx.org/c4x/MITx/15.071x_2/asset/FluTrain.csv'
# Load "FluTrain.csv" into a data frame called FluTrain
FluTrain<-read.csv(url)
str(FluTrain)

url<-'http://courses.edx.org/c4x/MITx/15.071x_2/asset/FluTest.csv'
# Load "FluTrain.csv" into a data frame called FluTrain
FluTest<-read.csv(url)

cat ("Train/Test data read from url")
cat ("Press [enter] to continue")
line <- readline()


#"Week" - The range of dates represented by this observation, in year/month/day format.
#"ILI" - This column lists the percentage of ILI-related physician visits for the corresponding week.
#"Queries" - This column lists the fraction of queries that are ILI-related for the corresponding week, adjusted to be between 0 and 1 (higher values correspond to more ILI-related search queries).

# exploration graph
plot(FluTrain, main="explore the training set FluMain")

line <- readline()

# 2nd exploration, week by ILI
plot(FluTrain$Week, FluTrain$ILI, type="l", main="week by ILI")

# which week corresponds to the highest percentage of ILI-related physician visits?
FluTrain[which.max(ILI),]$Week
#  2009-10-18 - 2009-10-24

# Which week corresponds to the highest percentage of ILI-related query fraction?
plot(FluTrain$Week, FluTrain$Queries, type="l", main="Which week corresponds to the highest percentage of ILI-related query fraction?")
# [1] 2009-10-18 - 2009-10-24
FluTrain[which.max(Queries),]$Week
# so the queries matched the physician visits
line <- readline()

# What best describes the distribution of values of ILI?
hist(FluTrain$ILI, breaks=100)
# right skewed
cat ("What best describes the distribution of values of ILI?")
line <- readline()


# Plot the natural logarithm of ILI versus Queries. What does the plot suggest?.
plot(FluTrain$Queries, log(FluTrain$ILI), main="Plot the natural logarithm of ILI versus Queries. What does the plot suggest?")

cat ("Building linear regression model")
line <- readline()

# Based on our understanding of the data from the previous subproblem, which model best describes our estimation problem?
lm.Flu<-lm(log(ILI) ~ Queries, data=FluTrain)

# plot the linear model
abline(lm.Flu)

# fetch the R2 of the train data
summary(lm.Flu)
# Multiple R-squared:  0.709,     Adjusted R-squared:  0.7083 

cat ("For a single variable linear regression model, there is a direct relationship between the R-squared and the correlation between the independent and the dependent variables. ")

(cor(log(FluTrain$ILI), FluTrain$Queries)^2)

line <- readline()
# 0.7090201

cat ("applying the linear model on test data, result as inverse of log() <-> exp() ")
line <- readline()

# fetched the predicted data
PredictTest<-exp(predict(lm.Flu, newdata=FluTest))
Result <- data.frame("week"=FluTest$Week, "ILI"=PredictTest)

#What is our estimate for the percentage of ILI-related physician visits for the week of March 11, 2012? 
cat ("What is our estimate for the percentage of ILI-related physician visits for the week of March 11, 2012? ")
Result[which(Result$week=="2012-03-11 - 2012-03-17"),]

cat ("Estimated ILI: 2.187378 , real ILI: 2.293422")
line <- readline()

WeekId<-which(Result$week=="2012-03-11 - 2012-03-17")
cat ("What is the relative error betweeen the estimate (our prediction) and the observed value for the week of March 11, 2012? Note that the relative error is calculated as
(Observed ILI - Estimated ILI)/Observed ILI
")
(1-(PredictTest[WeekId]/FluTest[WeekId,]$ILI))
line <- readline()



