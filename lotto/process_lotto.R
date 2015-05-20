# load lotto digits between 2000 - 2014

lotto<-read.csv('../TEST/lotto/lotto.txt', header=T, sep="\t");
#lotto<-read.csv('lotto.txt', header=T);
attach(lotto);
plot(table(Jahr, Tag));

histogram(~Super|Jahr, data=lotto, breaks=20);


