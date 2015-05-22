
life<-read.csv('../TEST/Statistics with R - Coursera/LifeExp.txt', header=F, sep=" ");

life$V2<-NULL; life$V4<-NULL; names(life)<-c('Country', 'LifeExp', 'Region');

attach(life); lifesplit<-split(life, Region);

lifeEAP<-life[Region=='EAP',]; lifeSSA<-life[Region=='SSA',]

boxplot(lifeEAP$LifeExp, lifeSSA$LifeExp, range=0, border=rainbow(2), names=c('EAP', 'SSA'), main="lifeexpectancy Boxplots for EastAsiaPacific vs SubsaharianAfrica");

#hist(lifeSSA$LifeExp, breaks=20)
#hist(lifeEAP$LifeExp, breaks=20)
