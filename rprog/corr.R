############################
#
#	"R Programming" Coursera
#	Programming Assignment 1: Air Pollution: Instructions
#
#	Part 3
#	Write a function that takes a directory of data files and a threshold for complete cases and calculates the correlation between sulfate and nitrate for monitor locations where the number of completely observed cases (on all variables) is greater than the threshold.
# 
#	By : Calin Uioreanu
#
############################
corr <- function(directory, threshold = 0) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files

        ## 'threshold' is a numeric vector of length 1 indicating the
        ## number of completely observed observations (on all
        ## variables) required to compute the correlation between
        ## nitrate and sulfate; the default is 0
	# data frame containing merged data-set
	dfMerged <- data.frame(correlation = numeric(0))
	id       <- 1:332

	# read sequentially each file 
	for (i in id) {
		fileName <- file.path(directory, paste0(sprintf("%03d", i), ".csv"));
		csvData  <- read.delim(fileName, header=T, sep=",");
		notNA    <- csvData[!is.na(csvData$sulfate) & !is.na(csvData$nitrate),];
		if (nrow(notNA)>threshold) {
			dataFrame<- data.frame(cor(notNA$sulfate, notNA$nitrate));
			names(dataFrame)<-c('correlation');
			dfMerged<-rbind(dfMerged,dataFrame);
		}
	}

        ## Return a numeric vector of correlations
        ## NOTE: Do not round the result!
	dfMerged[,1];
}
    
# test call

#cr <- corr("specdata", 150)
#head(cr)
#summary(cr)
#
#cr <- corr("specdata", 400)
#head(cr)
#summary(cr)
#
#cr <- corr("specdata", 5000)
#summary(cr)
#length(cr)
#
#
#cr <- corr("specdata")
#summary(cr)
#length(cr)
