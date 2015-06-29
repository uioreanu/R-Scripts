##############
# Revolution R Enterprise ScaleR Course RevoScaleR
# Big Data Analysis with Revolution R Enterprise
# Revolution R Enterprise allows R users to process, visualize, and model 
# terabyte-class data sets at a fraction of the time of legacy products 
# without requiring expensive or specialized hardware. Introductory course 
# for accomplished R users to experience the functionality of Revolution R Enterprise.
##############

##############
# CHAPTER 1
# Introduction
# Big Data Analytics Overview
##############

# Question: do delayed flights fly faster?
# data-source: 2007 airflight data.
# data exploration, histogram
> rxGetInfo(data = myAirlineXdf, getVarInfo = TRUE, numRows = 10)
File name: /tmp/Rserv/conn18419/2007_subset.xdf 
Number of observations: 149139 
Number of variables: 5 
Number of blocks: 1 
Compression type: zlib 
Variable information: 
Var 1: ActualElapsedTime, Type: integer, Low/High: (17, 730)
Var 2: AirTime, Type: integer, Low/High: (0, 704)
Var 3: Distance, Type: integer, Low/High: (31, 4962)
Var 4: DepDelay, Type: integer, Low/High: (-38, 1243)
Var 5: ArrDelay, Type: integer, Low/High: (-63, 1262)
Data (10 rows starting with row 1):
   ActualElapsedTime AirTime Distance DepDelay ArrDelay
1                 62      44      237       24       26
2                191     179     1262       -3      -27
3                138     127     1111       24        2
4                131     110      842       18       19
5                138     123      889       92       75
6                 82      58      410       -3       -1
7                131     118      822       -1      -20
8                 61      48      335        0       -4
9                 66      55      325       -5       -9
10               295     279     2106       12      -23
> ## Summarize the variables corresponding to actual elapsed time, time in the air, departure delay, flight Distance.
> rxSummary(formula = ~ActualElapsedTime + AirTime + DepDelay + Distance, data = myAirlineXdf)
Call:
rxSummary(formula = ~ActualElapsedTime + AirTime + DepDelay + 
    Distance, data = myAirlineXdf)

Summary Statistics Results for: ~ActualElapsedTime + AirTime + DepDelay
    + Distance
Data: myAirlineXdf (RxXdfData Data Source)
File name: 2007_subset.xdf
Number of valid observations: 149139 
 
 Name              Mean      StdDev    Min Max  ValidObs MissingObs
 ActualElapsedTime 126.07290  71.07911  17  730 145568   3571      
 AirTime           102.54263  68.07955   0  704 145568   3571      
 DepDelay           11.36742  36.05140 -38 1243 145892   3247      
 Distance          717.78808 560.99114  31 4962 149139      0      
> # Histogram of departure delays
> rxHistogram(formula = ~DepDelay, data = myAirlineXdf)
> # Use parameters similar to a regular histogram to zero in on the interesting area
> rxHistogram(formula = ~DepDelay, data = myAirlineXdf, xAxisMinMax = c(-100, 400), numBreaks = 500, xNumTicks = 10)
> 

# add another variable

> ## Calculate an additional variable: airspeed (distance traveled / time in the air). 
> rxDataStep(inData = myAirlineXdf, outFile = myAirlineXdf, varsToKeep = c("Distance", "AirTime"), transforms = list(airSpeed = Distance/AirTime), append = "rows", overwrite = T)
> # Get Variable Information for airspeed
> rxGetInfo(data = myAirlineXdf, getVarInfo = TRUE, varsToKeep = "airSpeed")
File name: /tmp/Rserv/conn18419/2007_subset.xdf 
Number of observations: 149139 
Number of variables: 6 
Number of blocks: 1 
Compression type: zlib 
Variable information: 
Var 1: airSpeed, Type: numeric, Low/High: (0.7436, 833.0000)
> # Summary for the airspeed variable
> rxSummary(formula = ~airSpeed, data = myAirlineXdf)
Call:
rxSummary(formula = ~airSpeed, data = myAirlineXdf)

Summary Statistics Results for: ~airSpeed
Data: myAirlineXdf (RxXdfData Data Source)
File name: 2007_subset.xdf
Number of valid observations: 149139 
 
 Name     Mean     StdDev   Min       Max ValidObs MissingObs
 airSpeed 6.564265 3.008519 0.7435897 833 145565   3574      
> # Construct a histogtam for airspeed
> # We can use the xAxisMinMax argument to limit the X-axis.
> rxHistogram(formula = ~airSpeed, data = myAirlineXdf)
> rxHistogram(formula = ~airSpeed, data = myAirlineXdf, xNumTicks = 10, numBreaks = 1500, xAxisMinMax = c(0, 12))
> 

# tweaking airSpeed

> # Conversion to miles per hour (from miles per minute)
> rxDataStep(inData = myAirlineXdf, outFile = myAirlineXdf, varsToKeep = c("airSpeed"), transforms = list(airSpeed = airSpeed * 60), overwrite = TRUE)
> # Histogram for airspeed after conversion
> rxHistogram(formula = ~airSpeed, data = myAirlineXdf)
> 

# is there a strong correlation between airSpeed and DepDelay/ArrDelay?

> # Correlation for departure delay, arrival delay, and air speed
> rxCor(formula = ~DepDelay + ArrDelay + airSpeed, data = myAirlineXdf, rowSelection = (airSpeed > 50) & (airSpeed < 800))
           DepDelay    ArrDelay    airSpeed
DepDelay 1.00000000  0.93157838  0.02196339
ArrDelay 0.93157838  1.00000000 -0.06668119
airSpeed 0.02196339 -0.06668119  1.00000000
> 

# is there a linear relationship between airSpeed and DepDelay?

> # Regression for airSpeed based on departure delay
> myLMobj <- rxLinMod(formula = airSpeed ~ DepDelay, data = myAirlineXdf, rowSelection = (airSpeed > 50) & (airSpeed < 800))
> summary(myLMobj)
Call:
rxLinMod(formula = airSpeed ~ DepDelay, data = myAirlineXdf, 
    rowSelection = (airSpeed > 50) & (airSpeed < 800))

Linear Regression Results for: airSpeed ~ DepDelay
Data: myAirlineXdf (RxXdfData Data Source)
File name: 2007_subset.xdf
Dependent variable(s): airSpeed
Total independent variables: 2 
Number of valid observations: 145054
Number of missing observations: 0 
 
Coefficients:
             Estimate Std. Error  t value Pr(>|t|)    
(Intercept) 3.901e+02  2.091e-01 1866.095 2.22e-16 ***
DepDelay    4.638e-02  5.543e-03    8.367 2.22e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 75.95 on 145052 degrees of freedom
Multiple R-squared: 0.0004824 
Adjusted R-squared: 0.0004755 
F-statistic: 70.01 on 1 and 145052 DF,  p-value: < 2.2e-16 
Condition number: 1 

# low p factor, factor appears significant but the DepDelay is affected by a very low coefficient
# the next question is if the relationship is non-linear, for example if flights with longer delays
# do not "rush" anymore, and flights with shorter delays do "rush" to catch up.
# this requires splitting the data on 10-fold intervals 

##############
# CHAPTER 2
# Data Exploration 
# exploring the sample Dow Jones data

> ## extract the names of the possible options:
> names(rxOptions())
 [1] "cintSysDir"            "includeDir"            "libDir"               
 [4] "linkDllName"           "unitTestDir"           "unitTestDataDir"      
 [7] "sampleDataDir"         "demoScriptsDir"        "fileSystem"           
[10] "hdfsPort"              "hdfsHost"              "xdfCompressionLevel"  
[13] "computeContext"        "blocksPerRead"         "reportProgress"       
[16] "rowDisplayMax"         "memStatsReset"         "memStatsDiff"         
[19] "numCoresToUse"         "numDigits"             "showTransformFn"      
[22] "defaultDecimalColType" "defaultMissingColType" "dataPath"             
[25] "outDataPath"           "transformPackages"     "useSparseCube"        
[28] "dropMain"              "coefLabelStyle"       
> ## extract the sample data directory:
> rxGetOption("sampleDataDir")
[1] "/usr/lib64/Revo-7.3/R-3.1.1/lib64/R/library/RevoScaleR/SampleData"
> ## view the current value of the reportProgress option
> rxGetOption("reportProgress")
[1] 2
> ## set the value of the reportProgress option to 0
> rxOptions(reportProgress = 0)
> 

> ## set up the variable that has the address of the relevant data file:
> djiXdf <- file.path(rxGetOption("sampleDataDir"), "DJIAdaily.xdf")
> ## get information about that dataset:
> rxGetInfo(djiXdf, getVarInfo = TRUE)
File name: /usr/lib64/Revo-7.3/R-3.1.1/lib64/R/library/RevoScaleR/SampleData/DJIAdaily.xdf 
Number of observations: 20636 
Number of variables: 13 
Number of blocks: 4 
Compression type: zlib 
Variable information: 
Var 1: Date, Type: character
Var 2: Open, Type: numeric, Storage: float32, Low/High: (41.6300, 14165.0195)
Var 3: High, Type: numeric, Storage: float32, Low/High: (42.6100, 14279.9600)
Var 4: Low, Type: numeric, Storage: float32, Low/High: (40.5600, 13980.9004)
Var 5: Close, Type: numeric, Storage: float32, Low/High: (41.2200, 14164.5303)
Var 6: Volume, Type: numeric, Storage: float32, Low/High: (130000.0000, 11456230400.0000)
Var 7: Adj.Close, Type: numeric, Storage: float32, Low/High: (41.2200, 14164.5303)
Var 8: Year, Type: integer, Low/High: (1928, 2010)
Var 9: Month, Type: integer, Low/High: (1, 12)
Var 10: DayOfMonth, Type: integer, Low/High: (1, 31)
Var 11: DayOfWeek
       5 factor levels: Monday Tuesday Wednesday Thursday Friday
Var 12: DaysSince1928, Type: integer, Low/High: (274, 30286)
Var 13: YearFrac, Type: numeric, Storage: float32, Low/High: (1928.7501, 2010.9186)
> 

> rxGetVarInfo(djiXdf)
Var 1: Date, Type: character
Var 2: Open, Type: numeric, Storage: float32, Low/High: (41.6300, 14165.0195)
Var 3: High, Type: numeric, Storage: float32, Low/High: (42.6100, 14279.9600)
Var 4: Low, Type: numeric, Storage: float32, Low/High: (40.5600, 13980.9004)
Var 5: Close, Type: numeric, Storage: float32, Low/High: (41.2200, 14164.5303)
Var 6: Volume, Type: numeric, Storage: float32, Low/High: (130000.0000, 11456230400.0000)
Var 7: Adj.Close, Type: numeric, Storage: float32, Low/High: (41.2200, 14164.5303)
Var 8: Year, Type: integer, Low/High: (1928, 2010)
Var 9: Month, Type: integer, Low/High: (1, 12)
Var 10: DayOfMonth, Type: integer, Low/High: (1, 31)
Var 11: DayOfWeek
       5 factor levels: Monday Tuesday Wednesday Thursday Friday
Var 12: DaysSince1928, Type: integer, Low/High: (274, 30286)
Var 13: YearFrac, Type: numeric, Storage: float32, Low/High: (1928.7501, 2010.9186)


> ## get variable information for the dataset
> djiVarInfo <- rxGetVarInfo(djiXdf)
> names(djiVarInfo)
 [1] "Date"          "Open"          "High"          "Low"          
 [5] "Close"         "Volume"        "Adj.Close"     "Year"         
 [9] "Month"         "DayOfMonth"    "DayOfWeek"     "DaysSince1928"
[13] "YearFrac"     
> ## extract information about the closing cost variable
> (closeVarInfo <- djiVarInfo$Close)
Type: numeric, Storage: float32, Low/High: (41.2200, 14164.5303)
> ## get the class of the closeVarInfo object:
> class(closeVarInfo)
[1] "rxVarInfo"
> ## examine the structure of the closeVarInfo object:
> str(closeVarInfo)
List of 4
 $ varType: chr "numeric"
 $ storage: chr "float32"
 $ low    : num 41.2
 $ high   : num 14165
 - attr(*, "class")= chr "rxVarInfo"
> ## extract the global maximum of the closing cost variable:
> closeMax <- closeVarInfo[["high"]]





> ## Basic summary statistics:
> rxSummary(~DayOfWeek + Close + Volume, data = djiXdf)
Rows Read: 6000, Total Rows Processed: 6000, Total Chunk Time: 0.001 seconds
Rows Read: 6000, Total Rows Processed: 12000, Total Chunk Time: 0.001 seconds
Rows Read: 6000, Total Rows Processed: 18000, Total Chunk Time: 0.001 seconds
Rows Read: 2636, Total Rows Processed: 20636, Total Chunk Time: Less than .001 seconds 
Computation time: 0.007 seconds.
Call:
rxSummary(formula = ~DayOfWeek + Close + Volume, data = djiXdf)

Summary Statistics Results for: ~DayOfWeek + Close + Volume
Data: djiXdf (RxXdfData Data Source)
File name:
    /usr/lib64/Revo-7.3/R-3.1.1/lib64/R/library/RevoScaleR/SampleData/DJIAdaily.xdf
Number of valid observations: 20636 
 
 Name   Mean         StdDev      Min       Max          ValidObs MissingObs
 Close  2.516781e+03 3.65031e+03     41.22 1.416453e+04 20636    0         
 Volume 4.173323e+08 1.09826e+09 130000.00 1.145623e+10 20636    0         

Category Counts for DayOfWeek
Number of categories: 5
Number of valid observations: 20636
Number of missing observations: 0

 DayOfWeek Counts
 Monday    3989  
 Tuesday   4182  
 Wednesday 4205  
 Thursday  4139  
 Friday    4121  
> ## Frequency weighted:
> rxSummary(~DayOfWeek + Close + Volume, data = djiXdf, fweights = "Volume")
Rows Read: 6000, Total Rows Processed: 6000, Total Chunk Time: 0.001 seconds
Rows Read: 6000, Total Rows Processed: 12000, Total Chunk Time: 0.001 seconds
Rows Read: 6000, Total Rows Processed: 18000, Total Chunk Time: 0.002 seconds
Rows Read: 2636, Total Rows Processed: 20636, Total Chunk Time: 0.001 seconds 
Computation time: 0.008 seconds.
Call:
rxSummary(formula = ~DayOfWeek + Close + Volume, data = djiXdf, 
    fweights = "Volume")

Summary Statistics Results for: ~DayOfWeek + Close + Volume
Data: djiXdf (RxXdfData Data Source)
File name:
    /usr/lib64/Revo-7.3/R-3.1.1/lib64/R/library/RevoScaleR/SampleData/DJIAdaily.xdf
Frequency weights: Volume
Sum of weights of valid observations: 20636 
 
 Name      Mean         StdDev       Min       Max          SumOfWeights
 DayOfWeek 2.046997e+00 1.380471e+00      0.00 4.000000e+00 8.61207e+12 
 Close     9.681542e+03 2.712059e+03     41.22 1.416453e+04 8.61207e+12 
 Volume    3.307396e+09 2.289756e+09 130000.00 1.145623e+10 8.61207e+12 
 MissingWeights
 0             
 0             
 0             

Category Counts for DayOfWeek
Number of categories: 5

 DayOfWeek Counts      
 Monday    1.512617e+12
 Tuesday   1.774277e+12
 Wednesday 1.821944e+12
 Thursday  1.802208e+12
 Friday    1.701024e+12
> ## Basic frequency count:
> rxCrossTabs(~DayOfWeek, data = djiXdf)
Rows Read: 6000, Total Rows Processed: 6000, Total Chunk Time: Less than .001 seconds
Rows Read: 6000, Total Rows Processed: 12000, Total Chunk Time: Less than .001 seconds
Rows Read: 6000, Total Rows Processed: 18000, Total Chunk Time: Less than .001 seconds
Rows Read: 2636, Total Rows Processed: 20636, Total Chunk Time: Less than .001 seconds 
Computation time: 0.005 seconds.
Call:
rxCrossTabs(formula = ~DayOfWeek, data = djiXdf)

Cross Tabulation Results for: ~DayOfWeek
Data: djiXdf (RxXdfData Data Source)
File name:
    /usr/lib64/Revo-7.3/R-3.1.1/lib64/R/library/RevoScaleR/SampleData/DJIAdaily.xdf
Number of valid observations: 20636
Number of missing observations: 0 
Statistic: counts 
 
DayOfWeek (counts):
              
Monday    3989
Tuesday   4182
Wednesday 4205
Thursday  4139
Friday    4121
> 


> ## Numeric Variables
> rxHistogram(~Close, data = djiXdf)
Rows Read: 6000, Total Rows Processed: 6000, Total Chunk Time: 0.004 seconds
Rows Read: 6000, Total Rows Processed: 12000, Total Chunk Time: 0.004 seconds
Rows Read: 6000, Total Rows Processed: 18000, Total Chunk Time: 0.004 seconds
Rows Read: 2636, Total Rows Processed: 20636, Total Chunk Time: 0.003 seconds 
Computation time: 0.018 seconds.
> ## Categorical Variable:
> rxHistogram(~DayOfWeek, data = djiXdf)
Rows Read: 6000, Total Rows Processed: 6000, Total Chunk Time: Less than .001 seconds
Rows Read: 6000, Total Rows Processed: 12000, Total Chunk Time: Less than .001 seconds
Rows Read: 6000, Total Rows Processed: 18000, Total Chunk Time: Less than .001 seconds
Rows Read: 2636, Total Rows Processed: 20636, Total Chunk Time: Less than .001 seconds 
Computation time: 0.004 seconds.
> ## Different panels for different days of the week
> rxHistogram(~Close | DayOfWeek, data = djiXdf)
Rows Read: 6000, Total Rows Processed: 6000, Total Chunk Time: 0.005 seconds
Rows Read: 6000, Total Rows Processed: 12000, Total Chunk Time: 0.005 seconds
Rows Read: 6000, Total Rows Processed: 18000, Total Chunk Time: 0.004 seconds
Rows Read: 2636, Total Rows Processed: 20636, Total Chunk Time: 0.003 seconds 
Computation time: 0.021 seconds.
> ## Numeric Variables with a frequency weighting:
> rxHistogram(~Close, data = djiXdf, fweights = "Volume")
Rows Read: 6000, Total Rows Processed: 6000, Total Chunk Time: 0.004 seconds
Rows Read: 6000, Total Rows Processed: 12000, Total Chunk Time: 0.005 seconds
Rows Read: 6000, Total Rows Processed: 18000, Total Chunk Time: 0.004 seconds
Rows Read: 2636, Total Rows Processed: 20636, Total Chunk Time: 0.003 seconds 
Computation time: 0.021 seconds.
> 

> ## Simple bivariate line plot:
> rxLinePlot(Close ~ DaysSince1928, data = djiXdf)
Rows Read: 6000, Total Rows Processed: 6000, Total Chunk Time: 0.003 seconds
Rows Read: 6000, Total Rows Processed: 12000, Total Chunk Time: 0.002 seconds
Rows Read: 6000, Total Rows Processed: 18000, Total Chunk Time: 0.002 seconds
Rows Read: 2636, Total Rows Processed: 20636, Total Chunk Time: 0.001 seconds 
> ## Using different panels for different days of the week:
> rxLinePlot(Close ~ DaysSince1928 | DayOfWeek, data = djiXdf)
Rows Read: 6000, Total Rows Processed: 6000, Total Chunk Time: 0.003 seconds
Rows Read: 6000, Total Rows Processed: 12000, Total Chunk Time: 0.002 seconds
Rows Read: 6000, Total Rows Processed: 18000, Total Chunk Time: 0.002 seconds
Rows Read: 2636, Total Rows Processed: 20636, Total Chunk Time: 0.002 seconds 
> ## Using different groups.
> rxLinePlot(Close ~ DaysSince1928, groups = DayOfWeek, data = djiXdf)
Rows Read: 6000, Total Rows Processed: 6000, Total Chunk Time: 0.003 seconds
Rows Read: 6000, Total Rows Processed: 12000, Total Chunk Time: 0.003 seconds
Rows Read: 6000, Total Rows Processed: 18000, Total Chunk Time: 0.003 seconds
Rows Read: 2636, Total Rows Processed: 20636, Total Chunk Time: 0.002 seconds 
> ## Simple bivariate line plot, after taking the log() of the ordinate (y) variable.
> rxLinePlot(log(Close) ~ DaysSince1928, data = djiXdf)
Rows Read: 6000, Total Rows Processed: 6000, Total Chunk Time: 0.003 seconds
Rows Read: 6000, Total Rows Processed: 12000, Total Chunk Time: 0.002 seconds
Rows Read: 6000, Total Rows Processed: 18000, Total Chunk Time: 0.002 seconds
Rows Read: 2636, Total Rows Processed: 20636, Total Chunk Time: 0.001 seconds 
> 


> ## Compute the the summed volume for each day of the week for each month:
> rxCrossTabs(formula = Volume ~ F(Month):DayOfWeek, data = djiXdf)
Rows Read: 6000, Total Rows Processed: 6000, Total Chunk Time: 0.001 seconds
Rows Read: 6000, Total Rows Processed: 12000, Total Chunk Time: 0.001 seconds
Rows Read: 6000, Total Rows Processed: 18000, Total Chunk Time: 0.001 seconds
Rows Read: 2636, Total Rows Processed: 20636, Total Chunk Time: Less than .001 seconds 
Computation time: 0.007 seconds.
Call:
rxCrossTabs(formula = Volume ~ F(Month):DayOfWeek, data = djiXdf)

Cross Tabulation Results for: Volume ~ F(Month):DayOfWeek
Data: djiXdf (RxXdfData Data Source)
File name:
    /usr/lib64/Revo-7.3/R-3.1.1/lib64/R/library/RevoScaleR/SampleData/DJIAdaily.xdf
Dependent variable(s): Volume
Number of valid observations: 20636
Number of missing observations: 0 
Statistic: sums 
 
Volume (sums):
       DayOfWeek
F_Month       Monday      Tuesday    Wednesday     Thursday       Friday
     1  100453160640 140286240336 149087369328 152184470496 147892141520
     2   91926419616 134865669392 133602339728 135367399680 139232921600
     3  155211019552 158054359936 160485980304 156809870400 143296529568
     4  135360420160 149625269760 158095860384 160039070288 126733750400
     5  114411520096 152175269984 158420289616 160328410080 149583789488
     6  135188510400 147547849776 145226389792 143743650048 149822300080
     7  125614759936 152701159648 157332601008 163020899488 134820430224
     8  132849590176 138982279296 148299859456 147770430432 138013450560
     9  112979300912 157709201312 157600099424 151305940528 144968749200
     10 145755510672 162156030160 173506599040 176610670944 174333560160
     11 147301549648 155582169504 150121160240 130254850080 132521269088
     12 115565549936 124591890400 130164970256 124772270800 119805110368
> ## Compute the the average volume for each day of the week for each month:
> rxCrossTabs(formula = Volume ~ F(Month):DayOfWeek, data = djiXdf, mean = TRUE)
Rows Read: 6000, Total Rows Processed: 6000, Total Chunk Time: 0.001 seconds
Rows Read: 6000, Total Rows Processed: 12000, Total Chunk Time: 0.001 seconds
Rows Read: 6000, Total Rows Processed: 18000, Total Chunk Time: 0.001 seconds
Rows Read: 2636, Total Rows Processed: 20636, Total Chunk Time: Less than .001 seconds 
Computation time: 0.007 seconds.
Call:
rxCrossTabs(formula = Volume ~ F(Month):DayOfWeek, data = djiXdf, 
    means = TRUE)

Cross Tabulation Results for: Volume ~ F(Month):DayOfWeek
Data: djiXdf (RxXdfData Data Source)
File name:
    /usr/lib64/Revo-7.3/R-3.1.1/lib64/R/library/RevoScaleR/SampleData/DJIAdaily.xdf
Dependent variable(s): Volume
Number of valid observations: 20636
Number of missing observations: 0 
Statistic: means 
 
Volume (means):
       DayOfWeek
F_Month    Monday   Tuesday Wednesday  Thursday    Friday
     1  308138530 400817830 423543663 433573990 420148129
     2  340468221 418837483 417507312 421705295 435102880
     3  429947423 437823712 443331437 434376372 423954229
     4  385642223 426282820 450415557 454656450 432538397
     5  367882701 427458624 442514775 450360702 419002211
     6  384058268 419170028 417317212 409526069 428063715
     7  370545015 435046039 450809745 464447007 394211784
     8  365976832 382871293 415405769 408205609 380202343
     9  418441855 449313964 452873849 432302687 415383236
     10 406004208 444263096 481962775 485194151 478938352
     11 423280315 505136914 427695613 482425371 376480878
     12 340901327 353954234 372965531 356492202 351334635
> ## Compute the the average closing price for each day of the week for each month, using volume as frequency weights
> rxCrossTabs(formula = Close ~ F(Month):DayOfWeek, data = djiXdf, means = TRUE, fweights = "Volume")
Rows Read: 6000, Total Rows Processed: 6000, Total Chunk Time: 0.002 seconds
Rows Read: 6000, Total Rows Processed: 12000, Total Chunk Time: 0.002 seconds
Rows Read: 6000, Total Rows Processed: 18000, Total Chunk Time: 0.002 seconds
Rows Read: 2636, Total Rows Processed: 20636, Total Chunk Time: 0.001 seconds 
Computation time: 0.009 seconds.
Call:
rxCrossTabs(formula = Close ~ F(Month):DayOfWeek, data = djiXdf, 
    fweights = "Volume", means = TRUE)

Cross Tabulation Results for: Close ~ F(Month):DayOfWeek
Data: djiXdf (RxXdfData Data Source)
File name:
    /usr/lib64/Revo-7.3/R-3.1.1/lib64/R/library/RevoScaleR/SampleData/DJIAdaily.xdf
Frequency weights: Volume
Dependent variable(s): Close
Sum of weights of valid observations: 8612070155344
Number of missing observations: 0 
Statistic: means 
 
Close (means):
       DayOfWeek
F_Month   Monday  Tuesday Wednesday Thursday   Friday
     1  9447.143 9609.819  9713.556 9666.235 9520.421
     2  9318.145 9385.130  9328.339 9265.331 9356.839
     3  9290.767 9281.642  9352.612 9407.788 9292.174
     4  9722.881 9678.512  9668.298 9625.000 9820.420
     5  9906.000 9887.023  9847.209 9908.007 9721.381
     6  9752.588 9721.678  9770.470 9815.480 9878.071
     7  9798.827 9854.674  9645.457 9798.228 9759.297
     8  9843.677 9790.362  9987.167 9968.487 9942.661
     9  9889.898 9893.286  9826.807 9836.212 9875.476
     10 9729.465 9653.250  9666.057 9518.674 9523.039
     11 9819.758 9832.750  9757.541 9858.042 9770.669
     12 9473.085 9486.524  9671.284 9701.425 9642.693
> 


> ## Compute the the summed volume for each day of the week:
> rxCrossTabs(Volume ~ DayOfWeek, data = djiXdf)
Rows Read: 6000, Total Rows Processed: 6000, Total Chunk Time: 0.001 seconds
Rows Read: 6000, Total Rows Processed: 12000, Total Chunk Time: Less than .001 seconds
Rows Read: 6000, Total Rows Processed: 18000, Total Chunk Time: Less than .001 seconds
Rows Read: 2636, Total Rows Processed: 20636, Total Chunk Time: Less than .001 seconds 
Computation time: 0.006 seconds.
Call:
rxCrossTabs(formula = Volume ~ DayOfWeek, data = djiXdf)

Cross Tabulation Results for: Volume ~ DayOfWeek
Data: djiXdf (RxXdfData Data Source)
File name:
    /usr/lib64/Revo-7.3/R-3.1.1/lib64/R/library/RevoScaleR/SampleData/DJIAdaily.xdf
Dependent variable(s): Volume
Number of valid observations: 20636
Number of missing observations: 0 
Statistic: sums 
 
Volume (sums):
                      
Monday    1.512617e+12
Tuesday   1.774277e+12
Wednesday 1.821944e+12
Thursday  1.802208e+12
Friday    1.701024e+12
> rxCube(Volume ~ DayOfWeek, data = djiXdf, means = FALSE)
Rows Read: 6000, Total Rows Processed: 6000, Total Chunk Time: 0.001 seconds
Rows Read: 6000, Total Rows Processed: 12000, Total Chunk Time: Less than .001 seconds
Rows Read: 6000, Total Rows Processed: 18000, Total Chunk Time: Less than .001 seconds
Rows Read: 2636, Total Rows Processed: 20636, Total Chunk Time: Less than .001 seconds 
Computation time: 0.006 seconds.
Call:
rxCube(formula = Volume ~ DayOfWeek, data = djiXdf, means = FALSE)

Cube Results for: Volume ~ DayOfWeek
File name:
    /usr/lib64/Revo-7.3/R-3.1.1/lib64/R/library/RevoScaleR/SampleData/DJIAdaily.xdf
Dependent variable(s): Volume
Number of valid observations: 20636
Number of missing observations: 0 
Statistic: Volume sums 
 
  DayOfWeek Volume       Counts
1 Monday    1.512617e+12 3989  
2 Tuesday   1.774277e+12 4182  
3 Wednesday 1.821944e+12 4205  
4 Thursday  1.802208e+12 4139  
5 Friday    1.701024e+12 4121  
> ## Compute the the summed volume for each day of the week for each month:
> rxCrossTabs(Volume ~ F(Month):DayOfWeek, data = djiXdf)
Rows Read: 6000, Total Rows Processed: 6000, Total Chunk Time: 0.001 seconds
Rows Read: 6000, Total Rows Processed: 12000, Total Chunk Time: 0.001 seconds
Rows Read: 6000, Total Rows Processed: 18000, Total Chunk Time: 0.001 seconds
Rows Read: 2636, Total Rows Processed: 20636, Total Chunk Time: 0.001 seconds 
Computation time: 0.007 seconds.
Call:
rxCrossTabs(formula = Volume ~ F(Month):DayOfWeek, data = djiXdf)

Cross Tabulation Results for: Volume ~ F(Month):DayOfWeek
Data: djiXdf (RxXdfData Data Source)
File name:
    /usr/lib64/Revo-7.3/R-3.1.1/lib64/R/library/RevoScaleR/SampleData/DJIAdaily.xdf
Dependent variable(s): Volume
Number of valid observations: 20636
Number of missing observations: 0 
Statistic: sums 
 
Volume (sums):
       DayOfWeek
F_Month       Monday      Tuesday    Wednesday     Thursday       Friday
     1  100453160640 140286240336 149087369328 152184470496 147892141520
     2   91926419616 134865669392 133602339728 135367399680 139232921600
     3  155211019552 158054359936 160485980304 156809870400 143296529568
     4  135360420160 149625269760 158095860384 160039070288 126733750400
     5  114411520096 152175269984 158420289616 160328410080 149583789488
     6  135188510400 147547849776 145226389792 143743650048 149822300080
     7  125614759936 152701159648 157332601008 163020899488 134820430224
     8  132849590176 138982279296 148299859456 147770430432 138013450560
     9  112979300912 157709201312 157600099424 151305940528 144968749200
     10 145755510672 162156030160 173506599040 176610670944 174333560160
     11 147301549648 155582169504 150121160240 130254850080 132521269088
     12 115565549936 124591890400 130164970256 124772270800 119805110368
> rxCube(Volume ~ F(Month):DayOfWeek, data = djiXdf, means = FALSE)
Rows Read: 6000, Total Rows Processed: 6000, Total Chunk Time: 0.001 seconds
Rows Read: 6000, Total Rows Processed: 12000, Total Chunk Time: 0.001 seconds
Rows Read: 6000, Total Rows Processed: 18000, Total Chunk Time: 0.001 seconds
Rows Read: 2636, Total Rows Processed: 20636, Total Chunk Time: 0.001 seconds 
Computation time: 0.007 seconds.
Call:
rxCube(formula = Volume ~ F(Month):DayOfWeek, data = djiXdf, 
    means = FALSE)

Cube Results for: Volume ~ F(Month):DayOfWeek
File name:
    /usr/lib64/Revo-7.3/R-3.1.1/lib64/R/library/RevoScaleR/SampleData/DJIAdaily.xdf
Dependent variable(s): Volume
Number of valid observations: 20636
Number of missing observations: 0 
Statistic: Volume sums 
 
   F_Month DayOfWeek Volume       Counts
1  1       Monday    100453160640 326   
2  2       Monday     91926419616 270   
3  3       Monday    155211019552 361   
4  4       Monday    135360420160 351   
5  5       Monday    114411520096 311   
6  6       Monday    135188510400 352   
7  7       Monday    125614759936 339   
8  8       Monday    132849590176 363   
9  9       Monday    112979300912 270   
10 10      Monday    145755510672 359   
11 11      Monday    147301549648 348   
12 12      Monday    115565549936 339   
13 1       Tuesday   140286240336 350   
14 2       Tuesday   134865669392 322   
15 3       Tuesday   158054359936 361   
16 4       Tuesday   149625269760 351   
17 5       Tuesday   152175269984 356   
18 6       Tuesday   147547849776 352   
19 7       Tuesday   152701159648 351   
20 8       Tuesday   138982279296 363   
21 9       Tuesday   157709201312 351   
22 10      Tuesday   162156030160 365   
23 11      Tuesday   155582169504 308   
24 12      Tuesday   124591890400 352   
25 1       Wednesday 149087369328 352   
26 2       Wednesday 133602339728 320   
27 3       Wednesday 160485980304 362   
28 4       Wednesday 158095860384 351   
29 5       Wednesday 158420289616 358   
30 6       Wednesday 145226389792 348   
31 7       Wednesday 157332601008 349   
32 8       Wednesday 148299859456 357   
33 9       Wednesday 157600099424 348   
34 10      Wednesday 173506599040 360   
35 11      Wednesday 150121160240 351   
36 12      Wednesday 130164970256 349   
37 1       Thursday  152184470496 351   
38 2       Thursday  135367399680 321   
39 3       Thursday  156809870400 361   
40 4       Thursday  160039070288 352   
41 5       Thursday  160328410080 356   
42 6       Thursday  143743650048 351   
43 7       Thursday  163020899488 351   
44 8       Thursday  147770430432 362   
45 9       Thursday  151305940528 350   
46 10      Thursday  176610670944 364   
47 11      Thursday  130254850080 270   
48 12      Thursday  124772270800 350   
49 1       Friday    147892141520 352   
50 2       Friday    139232921600 320   
51 3       Friday    143296529568 338   
52 4       Friday    126733750400 293   
53 5       Friday    149583789488 357   
54 6       Friday    149822300080 350   
55 7       Friday    134820430224 342   
56 8       Friday    138013450560 363   
57 9       Friday    144968749200 349   
58 10      Friday    174333560160 364   
59 11      Friday    132521269088 352   
60 12      Friday    119805110368 341   
> ## Compute the the average volume for each day of the week for each month:
> rxCube(Volume ~ F(Month):DayOfWeek, data = djiXdf)
Rows Read: 6000, Total Rows Processed: 6000, Total Chunk Time: 0.001 seconds
Rows Read: 6000, Total Rows Processed: 12000, Total Chunk Time: 0.001 seconds
Rows Read: 6000, Total Rows Processed: 18000, Total Chunk Time: 0.001 seconds
Rows Read: 2636, Total Rows Processed: 20636, Total Chunk Time: Less than .001 seconds 
Computation time: 0.007 seconds.
Call:
rxCube(formula = Volume ~ F(Month):DayOfWeek, data = djiXdf)

Cube Results for: Volume ~ F(Month):DayOfWeek
File name:
    /usr/lib64/Revo-7.3/R-3.1.1/lib64/R/library/RevoScaleR/SampleData/DJIAdaily.xdf
Dependent variable(s): Volume
Number of valid observations: 20636
Number of missing observations: 0 
Statistic: Volume means 
 
   F_Month DayOfWeek Volume    Counts
1  1       Monday    308138530 326   
2  2       Monday    340468221 270   
3  3       Monday    429947423 361   
4  4       Monday    385642223 351   
5  5       Monday    367882701 311   
6  6       Monday    384058268 352   
7  7       Monday    370545015 339   
8  8       Monday    365976832 363   
9  9       Monday    418441855 270   
10 10      Monday    406004208 359   
11 11      Monday    423280315 348   
12 12      Monday    340901327 339   
13 1       Tuesday   400817830 350   
14 2       Tuesday   418837483 322   
15 3       Tuesday   437823712 361   
16 4       Tuesday   426282820 351   
17 5       Tuesday   427458624 356   
18 6       Tuesday   419170028 352   
19 7       Tuesday   435046039 351   
20 8       Tuesday   382871293 363   
21 9       Tuesday   449313964 351   
22 10      Tuesday   444263096 365   
23 11      Tuesday   505136914 308   
24 12      Tuesday   353954234 352   
25 1       Wednesday 423543663 352   
26 2       Wednesday 417507312 320   
27 3       Wednesday 443331437 362   
28 4       Wednesday 450415557 351   
29 5       Wednesday 442514775 358   
30 6       Wednesday 417317212 348   
31 7       Wednesday 450809745 349   
32 8       Wednesday 415405769 357   
33 9       Wednesday 452873849 348   
34 10      Wednesday 481962775 360   
35 11      Wednesday 427695613 351   
36 12      Wednesday 372965531 349   
37 1       Thursday  433573990 351   
38 2       Thursday  421705295 321   
39 3       Thursday  434376372 361   
40 4       Thursday  454656450 352   
41 5       Thursday  450360702 356   
42 6       Thursday  409526069 351   
43 7       Thursday  464447007 351   
44 8       Thursday  408205609 362   
45 9       Thursday  432302687 350   
46 10      Thursday  485194151 364   
47 11      Thursday  482425371 270   
48 12      Thursday  356492202 350   
49 1       Friday    420148129 352   
50 2       Friday    435102880 320   
51 3       Friday    423954229 338   
52 4       Friday    432538397 293   
53 5       Friday    419002211 357   
54 6       Friday    428063715 350   
55 7       Friday    394211784 342   
56 8       Friday    380202343 363   
57 9       Friday    415383236 349   
58 10      Friday    478938352 364   
59 11      Friday    376480878 352   
60 12      Friday    351334635 341   
> ## Compute the the average closing price for each day of the week for each month, using volume as frequency weights
> rxCube(Close ~ F(Month):DayOfWeek, data = djiXdf, fweights = "Volume")
Rows Read: 6000, Total Rows Processed: 6000, Total Chunk Time: 0.002 seconds
Rows Read: 6000, Total Rows Processed: 12000, Total Chunk Time: 0.002 seconds
Rows Read: 6000, Total Rows Processed: 18000, Total Chunk Time: 0.002 seconds
Rows Read: 2636, Total Rows Processed: 20636, Total Chunk Time: 0.001 seconds 
Computation time: 0.009 seconds.
Call:
rxCube(formula = Close ~ F(Month):DayOfWeek, data = djiXdf, fweights = "Volume")

Cube Results for: Close ~ F(Month):DayOfWeek
File name:
    /usr/lib64/Revo-7.3/R-3.1.1/lib64/R/library/RevoScaleR/SampleData/DJIAdaily.xdf
Frequency weights: Volume
Dependent variable(s): Close
Sum of weights of valid observations: 8612070155344
Number of missing observations: 0 
Statistic: Close means 
 
   F_Month DayOfWeek Close    Counts      
1  1       Monday    9447.143 100453160640
2  2       Monday    9318.145  91926419616
3  3       Monday    9290.767 155211019552
4  4       Monday    9722.881 135360420160
5  5       Monday    9906.000 114411520096
6  6       Monday    9752.588 135188510400
7  7       Monday    9798.827 125614759936
8  8       Monday    9843.677 132849590176
9  9       Monday    9889.898 112979300912
10 10      Monday    9729.465 145755510672
11 11      Monday    9819.758 147301549648
12 12      Monday    9473.085 115565549936
13 1       Tuesday   9609.819 140286240336
14 2       Tuesday   9385.130 134865669392
15 3       Tuesday   9281.642 158054359936
16 4       Tuesday   9678.512 149625269760
17 5       Tuesday   9887.023 152175269984
18 6       Tuesday   9721.678 147547849776
19 7       Tuesday   9854.674 152701159648
20 8       Tuesday   9790.362 138982279296
21 9       Tuesday   9893.286 157709201312
22 10      Tuesday   9653.250 162156030160
23 11      Tuesday   9832.750 155582169504
24 12      Tuesday   9486.524 124591890400
25 1       Wednesday 9713.556 149087369328
26 2       Wednesday 9328.339 133602339728
27 3       Wednesday 9352.612 160485980304
28 4       Wednesday 9668.298 158095860384
29 5       Wednesday 9847.209 158420289616
30 6       Wednesday 9770.470 145226389792
31 7       Wednesday 9645.457 157332601008
32 8       Wednesday 9987.167 148299859456
33 9       Wednesday 9826.807 157600099424
34 10      Wednesday 9666.057 173506599040
35 11      Wednesday 9757.541 150121160240
36 12      Wednesday 9671.284 130164970256
37 1       Thursday  9666.235 152184470496
38 2       Thursday  9265.331 135367399680
39 3       Thursday  9407.788 156809870400
40 4       Thursday  9625.000 160039070288
41 5       Thursday  9908.007 160328410080
42 6       Thursday  9815.480 143743650048
43 7       Thursday  9798.228 163020899488
44 8       Thursday  9968.487 147770430432
45 9       Thursday  9836.212 151305940528
46 10      Thursday  9518.674 176610670944
47 11      Thursday  9858.042 130254850080
48 12      Thursday  9701.425 124772270800
49 1       Friday    9520.421 147892141520
50 2       Friday    9356.839 139232921600
51 3       Friday    9292.174 143296529568
52 4       Friday    9820.420 126733750400
53 5       Friday    9721.381 149583789488
54 6       Friday    9878.071 149822300080
55 7       Friday    9759.297 134820430224
56 8       Friday    9942.661 138013450560
57 9       Friday    9875.476 144968749200
58 10      Friday    9523.039 174333560160
59 11      Friday    9770.669 132521269088
60 12      Friday    9642.693 119805110368
> 
