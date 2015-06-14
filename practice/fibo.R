############################
#
#	Project Euler Problem #2 (R)
#	https://projecteuler.net/problem=2
#
#	By considering the terms in the Fibonacci sequence whose values do not exceed four million, find the sum of the even-valued terms.
# 
#	By : Calin Uioreanu
#
############################
fibMax      <- 4*1000*1000;
fibsum      <- 0
fibvals     <- numeric(fibMax)
fibvals[1]  <- 1
fibvals[2]  <- 1
i           <- 3;

repeat {
   fibvals[i] <- fibvals[i-1]+fibvals[i-2]
   if (fibvals[i]%%2 ==0) {
#    print(fibvals[i]);
    fibsum<-fibsum+fibvals[i]
   }
  if (fibvals[i]>=fibMax) break
  i<-i+1
} 

fibsum