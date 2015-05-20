#
# R Self-Quiz
# http://www.biostat.jhsph.edu/~rpeng/coursera/selfquiz/quiz.html
#

# RUN with source('quiz.R', print.eval  = TRUE)



url <- 'http://www.biostat.jhsph.edu/~rpeng/coursera/selfquiz/selfquiz-data.csv';
# url <- '../TEST/data analysis with R - Jeff Leek - Hopkins/selfquiz-data.csv';
air<-read.csv(url, header=T, sep=",");

readkey <- function()
{
    cat ("Press [enter] to continue")
    line <- readline()
}

print("What are the column names of the data frame?");
names(air);
readkey();

print("Extract the first 6 rows of the data frame and print them to the console");
head(air, 6);
readkey();

print("How many observations (i.e. rows) are in this data frame?");
length(air[,1])
readkey();

print("Extract the last 6 rows of the data frame and print them to the console");
tail(air, 6);
readkey();

print('How many missing values are in the "Ozone" column of this data frame?');
tmp<-subset(air, is.na(Ozone)); length(tmp[,1])
readkey();

print('What is the mean of the "Ozone" column in this dataset? Exclude missing values (coded as NA) from this calculation.');
tmp<-subset(air, !is.na(Ozone)); mean(tmp$Ozone)
readkey();

print('Extract the subset of rows of the data frame where Ozone values are above 31 and Temp values are above 90.');
tmp<-subset(air, Ozone>31 & Temp>90); tmp;
readkey();

print('Use a for loop to create a vector of length 6 containing the mean of each column in the data frame (excluding all missing values).');
for(i in names(air)){
	print (mean(air[[i]], na.rm=TRUE));
}

print('Use the apply function to calculate the standard deviation of each column in the data frame (excluding all missing values).');
apply(air, 2, sd);
readkey();

print('Calculate the mean of "Ozone" for each Month in the data frame and create a vector containing the monthly means (exclude all missing values).');
attach(air); aggregate(Ozone~Month, FUN=mean)
readkey();


print('Draw a random sample of 5 rows from the data frame');
air[sample(1:153,5),]
readkey();



