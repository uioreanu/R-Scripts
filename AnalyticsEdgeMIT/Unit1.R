if (F) {

setwd('../AnalyticsEdgeMIT/');
WHO<-read.csv('WHO.csv');
attach(WHO);
mean(Over60);
hist(CellularSubscribers, breaks=100);
tapply(ChildMortality, Region, mean)

}

setwd('../AnalyticsEdgeMIT/');
WHO<-read.csv('WHO.csv');
summary(USDA);
USDA$Description[which.max(Sodium)]

USDA$HighSodium<-as.numeric(USDA$Sodium>mean(USDA$Sodium, na.rm=T))
attach(USDA); plot(Protein, TotalFat,cex=0.5, col=HighSodium)

