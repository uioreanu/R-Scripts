############################
#
#	"Pump it Up:Data Mining the Water Table" 
#	DrivenData
# 
# explore_fit.R
# By : Calin Uioreanu
#
# prepares data and fits the model for predicting functionality of water pumps in Tanzania
############################

if (getwd()=="C:/Dokumente und Einstellungen/cu/Eigene Dateien") {
	setwd("C:/Dokumente und Einstellungen/cu/Eigene Dateien/data_R/water/");
}

train<-read.csv('train.csv', header=T);
trainLabels<-read.csv('trainLabels.csv', header=T);
test<-read.csv('test.csv', header=T);

# merge the classification to the train data using primary index id
train<-merge(train, trainLabels);
trainLabels<-NULL;

############################
# date_recorded
# maximize the chart, shaped like a Kuznets curve
#plot(table(substr(train$date_recorded, 1, 7), train$status_group), col=train$status_group);

train$date_recorded<-substr(train$date_recorded, 1, 7);
test$date_recorded<-substr(test$date_recorded, 1, 7);

#table(substr(train$date_recorded, 1, 7), train$status_group);
# reducing future factor levels
train$date_recorded[train$date_recorded<="2004-12"]<-"2004-12";

train$date_recorded<-as.factor(train$date_recorded);
test$date_recorded<-as.factor(test$date_recorded);
#table(train$date_recorded, train$status_group)
############################


############################
# funder
NUM_LEVELS_FUNDER = 10
funderNames <- names(summary(train$funder)[1:NUM_LEVELS_FUNDER])
funder <- factor(train$funder, levels=c(funderNames, "Other"))
funder[is.na(funder)] <- "Other"
train$funder <- funder
train$funder[train$funder==""]<-"Other"
train$funder<-factor(as.character(train$funder)); # reset table data
funder <- factor(test$funder, levels=c(funderNames, "Other"))
funder[is.na(funder)] <- "Other"
test$funder <- funder
test$funder[test$funder==""]<-"Other"
test$funder<-factor(as.character(test$funder)); # reset table data
############################


############################
# installer
NUM_LEVELS_INSTALLER = 10
installerNames <- names(summary(train$installer)[1:NUM_LEVELS_INSTALLER])
installer <- factor(train$installer, levels=c(installerNames, "Other"))
installer[is.na(installer)] <- "Other"
train$installer <- installer
train$installer[train$installer==""]<-"Other"
train$installer[train$installer=="0"]<-"Other"
train$installer<-factor(as.character(train$installer)); # reset table data
installer <- factor(test$installer, levels=c(installerNames, "Other"))
installer[is.na(installer)] <- "Other"
test$installer <- installer
test$installer[test$installer==""]<-"Other"
test$installer[test$installer=="0"]<-"Other"
test$installer<-factor(as.character(test$installer)); # reset table data
############################

trainGeo<-subset(train, train$longitude>=1 & train$latitude<=-1);
plot(trainGeo$longitude, trainGeo$latitude, pch=20, col=trainGeo$status_group);

# too many levels
train$wpt_name<-NULL;		test$wpt_name<-NULL;
train$num_private<-NULL;	test$num_private<-NULL;
train$subvillage<-NULL;		test$subvillage<-NULL;
train$region_code<-NULL;	test$region_code<-NULL;
train$district_code<-NULL;	test$district_code<-NULL;
train$lga<-NULL;			test$lga<-NULL;
train$ward<-NULL;			test$ward<-NULL;
train$recorded_by<-NULL;	test$recorded_by<-NULL;
train$scheme_name<-NULL;	test$scheme_name<-NULL;
train$recorded_by<-NULL;	test$recorded_by<-NULL;

#scheme_management - Change level "None"(Not present in test) to "" 
train$scheme_management[train$scheme_management=="None"] <- ""
train$scheme_management <- factor(as.character(train$scheme_management))
test$scheme_management[test$scheme_management=="None"] <- ""
test$scheme_management <- factor(as.character(test$scheme_management))


############################
# construction_year
NUM_LEVELS_CONSTRUCTION_YEAR = 20 #construction_year will have this many + 1 levels
#############################################################################################
train$construction_year <- factor(paste0("",as.character(train$construction_year)))
cyears <- names(summary(train$construction_year)[order(-summary(train$construction_year))][1:NUM_LEVELS_CONSTRUCTION_YEAR])
cy <- factor(train$construction_year, levels=c(cyears, "Other"))
cy[is.na(cy)] <- "Other"
train$construction_year <- cy
train$construction_year[train$construction_year=="0"]<-"Other"
train$construction_year<-factor(as.character(train$construction_year));

test$construction_year <- factor(paste0("",as.character(test$construction_year)))
cy <- factor(test$construction_year, levels=c(cyears, "Other"))
cy[is.na(cy)] <- "Other"
test$construction_year <- cy
test$construction_year[test$construction_year=="0"]<-"Other"
test$construction_year<-factor(as.character(test$construction_year));
# table(train$construction_year, train$status_group);
############################

install.packages('randomForest')
library(randomForest)

############################
# inference
set.seed(2015)

# set the same factors over the test data as on the train data
test$date_recorded <- factor(test$date_recorded, levels=levels(train$date_recorded))
test$construction_year <- factor(test$construction_year, levels=levels(train$construction_year))
test$extraction_type <- factor(test$extraction_type, levels=levels(train$extraction_type))
test$extraction_type_group <- factor(test$extraction_type_group, levels=levels(train$extraction_type_group))
test$extraction_type_class <- factor(test$extraction_type_class, levels=levels(train$extraction_type_class))

# combine all remaining parameters into one randomForest with 20 trees, let's see if R can handle it
# fit <- randomForest(status_group ~ amount_tsh + date_recorded + funder + gps_height + installer + longitude + latitude + basin + region + population + public_meeting + scheme_management + permit + construction_year + extraction_type + extraction_type_group + extraction_type_class + management + management_group + payment + payment_type + water_quality + quality_group + quantity + quantity_group + source + source_type + source_class + waterpoint_type + waterpoint_type_group, data=train, importance=TRUE, ntree=10)
# OOB 22.5%
# varImpPlot(fit)
# date_recorded, latitude then longitude

fit10 <- randomForest(status_group ~ amount_tsh + date_recorded + funder + gps_height + installer + longitude + latitude + basin + region + population + public_meeting + scheme_management + permit + construction_year + extraction_type + extraction_type_group + extraction_type_class + management + management_group + payment + payment_type + water_quality + quality_group + quantity + quantity_group + source + source_type + source_class + waterpoint_type + waterpoint_type_group, data=train, importance=TRUE, ntree=10)

fit50 <- randomForest(status_group ~ amount_tsh + date_recorded + funder + gps_height + installer + longitude + latitude + basin + region + population + public_meeting + scheme_management + permit + construction_year + extraction_type + extraction_type_group + extraction_type_class + management + management_group + payment + payment_type + water_quality + quality_group + quantity + quantity_group + source + source_type + source_class + waterpoint_type + waterpoint_type_group, data=train, importance=TRUE, ntree=50)

fit100 <- randomForest(status_group ~ amount_tsh + date_recorded + funder + gps_height + installer + longitude + latitude + basin + region + population + public_meeting + scheme_management + permit + construction_year + extraction_type + extraction_type_group + extraction_type_class + management + management_group + payment + payment_type + water_quality + quality_group + quantity + quantity_group + source + source_type + source_class + waterpoint_type + waterpoint_type_group, data=train, importance=TRUE, ntree=100)

#if (FALSE) {
#
#}

# latitude, longitude, region, construction_year
# since latitude and longitude are so important, they can be filled with average based on region

fit10.pred<-predict(fit10, test)
out<-data.frame(id=test$id, status_group=fit10.pred);
out$status_group[is.na(out$status_group)]<-'functional'; # 10 elements NA
write.csv(out, file='OutRF10.csv', row.names=F);

fit50.pred<-predict(fit50, test)
out<-data.frame(id=test$id, status_group=fit50.pred);
out$status_group[is.na(out$status_group)]<-'functional'; # 10 elements NA
write.csv(out, file='OutRF50.csv', row.names=F);

fit100.pred<-predict(fit100, test)
out<-data.frame(id=test$id, status_group=fit100.pred);
out$status_group[is.na(out$status_group)]<-'functional'; # 10 elements NA
write.csv(out, file='OutRF100.csv', row.names=F);



fit200 <- randomForest(status_group ~., data=train, importance=TRUE, ntree=200)
fit200.pred<-predict(fit200, test)
out<-data.frame(id=test$id, status_group=fit200.pred);
out$status_group[is.na(out$status_group)]<-'functional'; # 10 elements NA
write.csv(out, file='OutRF200.csv', row.names=F);

# fit010 OOB estimate of  error rate: 25.42%
# fit010 OOB estimate of  error rate: 22.28%
# fit050 OOB estimate of  error rate: 19.16%
# fit100 OOB estimate of  error rate: 18.87%
# fit200 OOB estimate of  error rate: 18.63%

barplot(c(25.42,22.28,19.16,18.87,18.63)) # close to overfitting

