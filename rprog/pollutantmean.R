############################
#
#	"R Programming" Coursera
#	Programming Assignment 1: Air Pollution: Instructions
#
#	Part 1
#	Write a function named 'pollutantmean' that calculates the mean of a pollutant (sulfate or nitrate) across a specified list of monitors
# 
#	By : Calin Uioreanu
#
############################
pollutantmean <- function(directory, pollutant, id = 1:332) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files

        ## 'pollutant' is a character vector of length 1 indicating
        ## the name of the pollutant for which we will calculate the
        ## mean; either "sulfate" or "nitrate".

        ## 'id' is an integer vector indicating the monitor ID numbers
        ## to be used

	# data frame containing merged data-set
	csvMerged<-NA;

	# read sequentially each file 
	for (i in id) {
		fileName <- file.path(directory, paste0(sprintf("%03d", i), ".csv"));
		csvData  <- read.delim(fileName, header=T, sep=",");
		csvMerged<- rbind(csvMerged, csvData);
	}

        ## Return the mean of the pollutant across all monitors list
        ## in the 'id' vector (ignoring NA values)
        ## NOTE: Do not round the result!
	mean(csvMerged[[pollutant]], na.rm=T);
}

# test call

#pollutantmean('specdata', 'sulfate', 1:10);
#pollutantmean('specdata', 'nitrate', 70:72);
#pollutantmean('specdata', 'nitrate', 23);
