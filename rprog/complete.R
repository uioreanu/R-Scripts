############################
#
#	"R Programming" Coursera
#	Programming Assignment 1: Air Pollution: Instructions
#
#	Part 2
#	Write a function that reads a directory full of files and reports the number of completely observed cases in each data file.
# 
#	By : Calin Uioreanu
#
############################
complete <- function(directory, id) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files

        ## 'id' is an integer vector indicating the monitor ID numbers
        ## to be used
        
	# data frame containing merged data-set
	dfMerged <- data.frame(id = numeric(0), nobs = numeric(0))

	# read sequentially each file 
	for (i in id) {
		fileName <- file.path(directory, paste0(sprintf("%03d", i), ".csv"));
		csvData  <- read.delim(fileName, header=T, sep=",");
		notNA    <- csvData[!is.na(csvData$sulfate) & !is.na(csvData$nitrate),];
		dataFrame<- data.frame(i, nrow(notNA));
		names(dataFrame)<-c('id', 'nobs');
		dfMerged<-rbind(dfMerged,dataFrame);
	}

	## Return a data frame of the form:
        ## id nobs
        ## 1  117
        ## 2  1041
        ## ...
        ## where 'id' is the monitor ID number and 'nobs' is the
        ## number of complete cases
	dfMerged;
}

# test call
#complete("specdata", 1);
#complete("specdata", c(2, 4, 8, 10, 12))
#complete("specdata", 30:25)
#complete("specdata", 3)
