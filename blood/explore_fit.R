############################
#
#	Warm Up: Predict Blood Donations
#	DrivenData
#
# explore_fit.R
# By : Calin Uioreanu
#
# prepares data and fits the model for predicting blood donation
############################

if (getwd()=="C:/Dokumente und Einstellungen/cu/Eigene Dateien") {
	setwd("C:/Dokumente und Einstellungen/cu/Eigene Dateien/data_R/blood/");
}

train<-read.csv('train.csv', header=T);
test<-read.csv('test.csv', header=T);

train$Total.Volume.Donated..c.c..<-NULL
test$Total.Volume.Donated..c.c..<-NULL

train$Made.Donation.in.March.2007<-as.factor(train$Made.Donation.in.March.2007)
test$Made.Donation.in.March.2007<-NA
test$Made.Donation.in.March.2007 <- factor(test$Made.Donation.in.March.2007, levels=levels(train$Made.Donation.in.March.2007))

# filtering
plot(train$Months.since.Last.Donation, train$Months.since.First.Donation, pch=20)
# remove the 4 outliers from train data:
train<-subset(train, train$Months.since.Last.Donation<=30);

install.packages('randomForest')
library(randomForest)

############################
# inference
set.seed(2015)

fit10000 <- randomForest(Made.Donation.in.March.2007~., data=train, importance=TRUE, ntree=10000);
varImpPlot(fit10000)
predict.fit10000<-predict(fit10000, test);

out<-data.frame(X=test$X, "Made Donation in March 2007"=as.numeric(predict.fit10000)-1);

write.csv(out, file='OutRF10000.csv', row.names=F);


attach(train);
fit.lm<-lm(Made.Donation.in.March.2007~Months.since.Last.Donation+Number.of.Donations+Months.since.First.Donation, data=train);
pred.lm<-predict(fit.lm, test);
out<-data.frame(X=test$X, "Made Donation in March 2007"=pred.lm);
write.csv(out, file='LM.csv', row.names=F);
 


