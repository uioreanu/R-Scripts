#
# http://www.rfortraders.com/simulation-of-a-coin-toss-in-r/
#

 seq <- sample(c(-1,1), 1000, replace = TRUE);
 plot(cumsum(seq), type = 'l');



# Create an empty list to store the results
 results <- list()
 for(i in 1:50000) {
     coinTosses   <- cumsum(sample(c(-1,1), 1000, replace = TRUE)) 
     results[[i]] <- coinTosses[length(coinTosses)]
 }
 
 # Unlist the list and create a histogram. Set a title and set the color and breaks
   hist(unlist(results), main = "Histogram of all the final p&l's",col = "lightblue", breaks = 100)
 
 # Place a vertical line at 0 with a width of 2 in order to show the average of the distribution
 abline(v = 0, col = "red", lwd = 2)