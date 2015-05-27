
# path = ../TEST/DrivenData - Pump it Up Data Mining the Water Table/

train<-read.csv('TrainingSetValues.csv', header=T);
trainLabels<-read.csv('TrainingSetLabels.csv', header=T);

test<-read.csv('TestSetValues.csv', header=T);

quantile(train$amount_tsh,probs=seq(0,1,by=0.05));

# get an overview how many pumps are functional, need repair or broken
table(trainLabels$status_group)/length(trainLabels[,1]);
# or prop.table(table(train$status_group));

#             functional functional needs repair          non functional 
#             0.54308081              0.07267677              0.38424242 

# graphic
barplot(table(trainLabels$status_group)/length(trainLabels[,1]))

# merge the classification to the train data
train<-merge(train, trainLabels);

# pump is functional, check the value passed, OK
subset(train, train$id==69572);

# let's assume all pumps are functional:
test$status_group<-'functional';

write.csv(data.frame(id=test$id, status_group=test$status_group), file='Out.csv', row.names=F)

# investigating construction_year
attach(train);
plot(prop.table(table(construction_year, status_group)), col=1:3)

# ggplot2 for aggregations 
install.packages('ggplot2');library(ggplot2);

ggplot(train, aes(x=source_class, fill=factor(status_group))) + geom_histogram(width=0.5) + labs(fill=status_group)

test$status_group<-'unknown'
all<-rbind(train, test)

library(rpart);
attach(train); #fit<-rpart(status_group~payment+water_quality+construction_year, method="class");

fit<-rpart(status_group~basin+district_code+extraction_type_class, method="class");
# 	fit<-rpart(status_group~amount_tsh+ basin+ construction_year+ date_recorded+ district_code+ extraction_type+extraction_type_class+ extraction_type_group+ funder+ gps_height+ id+ installer+latitude+ lga+ longitude+ management+ management_group+ num_private+ payment+payment_type+ permit+ population+ public_meeting+ quality_group+ quantity+quantity_group+ recorded_by+ region+ region_code+ scheme_management+ scheme_name+source+ source_class+ source_type+ subvillage+ ward+ water_quality+ waterpoint_type+waterpoint_type_group+ wpt_name, method="class");


fit<-rpart(status_group~amount_tsh+gps_height+num_private+basin+region+population+extraction_type+payment+water_quality+quantity+source, method="class");

install.packages('rattle')
install.packages('rpart.plot')
install.packages('RColorBrewer')
library(rattle)
library(rpart.plot)
library(RColorBrewer)
fancyRpartPlot(fit)

Prediction<-predict(fit, test, method="class");


write.csv(data.frame(id=test$id, status_group=Prediction), file='Out_dec_tree.csv', row.names=F);

install.packages('randomForest')
library(randomForest)

fit <- randomForest(status_group~amount_tsh+gps_height+num_private+basin+region+population+extraction_type+payment+water_quality+quantity+source, data=train, importance=TRUE, ntree=200)
varImpPlot(fit)

# random forest found main atributes: quantity, then extraction_type
plot(table(quantity, status_group), col=c('green', 'yellow', 'red'));
plot(table(extraction_type, status_group), col=c('green', 'yellow', 'red'));

levels(test$date_recorded)<-levels(train$date_recorded);
levels(test$funder)<-levels(train$funder);
levels(test$installer)<-levels(train$installer);
levels(test$wpt_name)<-levels(train$wpt_name);
levels(test$subvillage)<-levels(train$subvillage);
levels(test$ward)<-levels(train$ward);
levels(test$scheme_management)<-levels(train$scheme_management);
levels(test$extraction_type)<-levels(train$extraction_type);
Predict<-predict(fit, test)

write.csv(data.frame(id=test$id, status_group=Predict), file='Out_randomf.csv', row.names=F);

attach(train);
trainGeo<-subset(train, longitude>=1 & latitude<=-1)
plot(trainGeo$longitude, trainGeo$latitude, pch=20, col=status_group);

#install.packages('randomForest')
#library(randomForest)


par(mfrow=c(2,2))
trainGeo2<-trainGeo[trainGeo$status_group=="functional",]; plot(trainGeo2$longitude, trainGeo2$latitude, col="green", pch=20);
trainGeo2<-trainGeo[trainGeo$status_group=="functional needs repair",]; plot(trainGeo2$longitude, trainGeo2$latitude, col="yellow", pch=20);
trainGeo2<-trainGeo[trainGeo$status_group=="non functional",]; plot(trainGeo2$longitude, trainGeo2$latitude, col="red", pch=20);
trainGeo2<-trainGeo[trainGeo$status_group=="unknown",]; plot(trainGeo2$longitude, trainGeo2$latitude, col="blue", pch=20);
