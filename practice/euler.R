############################
#
#	Project Euler Problem #1 (R)
#	http://www.theresearchkitchen.com/archives/110
#
#	If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9. The sum of these multiples is 23.
#	Find the sum of all the multiples of 3 or 5 below 1000.
# 
#	By : Calin Uioreanu
#
############################

euler<- function(n=10) {
  sumEuler<-0
  for (i in seq(3, n-1)) {
    if (i%%3==0 || i%%5==0) {
      sumEuler<-sumEuler+i;
    }
  }
  sumEuler
}

euler2<- function(n=10) {
  sum(unique(c(seq(5, n-1, by=5), seq(3, n-1, by=3))))
}

euler3<- function(n=10) {
  i < - seq(1,n-1)
  sumEuler<-sum(i[i%%3 ==0 | i%%5 == 0])
  sumEuler
}

#euler(10);
#euler(20);
#euler(1000);
#euler(10000);
#euler(100000);
n<-1000^2;
system.time(print(paste("Euler dummy", sprintf("%010d", n), "=", euler(n))))
system.time(print(paste("Euler seq  ", sprintf("%010d", n), "=", euler2(n))))
system.time(print(paste("Euler seq3 ", sprintf("%010d", n), "=", euler3(n))))
