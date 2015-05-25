
train<-read.csv('../TEST/DrivenData - Pump it Up Data Mining the Water Table/TrainingSetValues.csv', header=T);
trainLabels<-read.csv('../TEST/DrivenData - Pump it Up Data Mining the Water Table/TrainingSetLabels.csv', header=T);

test<-read.csv('../TEST/DrivenData - Pump it Up Data Mining the Water Table/TestSetValues.csv', header=T);

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

write.csv(data.frame(id=test$id, status_group=test$status_group), file='../TEST/DrivenData - Pump it Up Data Mining the Water Table/Out.csv', row.names=F)
 

