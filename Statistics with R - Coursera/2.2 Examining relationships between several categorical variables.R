
skell<-read.table('../TEST/Statistics with R - Coursera/Skeleton.txt', header=T)

attach(skell);

BMI<-factor(BMI, levels=c('underweight', 'normal', 'overweight', 'obese'))

SEX<-factor(Sex, levels=c('1', '2'), labels=c('Male', 'Female'))

relfreqBMI<-table(BMI)/400; freqBMI<-table(BMI); cbind(freqBMI, relfreqBMI)

install.packages('gmodels'); library(gmodels);

joint <- gmodels::CrossTable(BMI, Sex); joint

barplot(joint$t, beside=T, col=rainbow(4), ylab='Frequency', xlab='Sex');

legend('topright', c('underweight', 'normal', 'overweight', 'obese'), pch=15, col=rainbow(4));

#Patricia