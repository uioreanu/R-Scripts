
life<-read.csv('../TEST/Statistics with R - Coursera/LifeExpGDPHIV.txt', header=F, sep=" ");
life$V6<-NULL; life$V4<-NULL; life$V2<-NULL; # and not the other way around
names(life)<-c('Country', 'LifeExpectancy', 'GDP', 'HIV');

attach(life);
plot(GDP, LifeExpectancy);

cor.test(HIV, LifeExpectancy);
cor.test(GDP, LifeExpectancy);

skell<-read.table('../TEST/Statistics with R - Coursera/SkeletonBMI.Quantitative.tst', header=T);
plot(BMIquant, DGDifference, pch=20, col=c('blue'));

cor(BMIquant, DGDifference);