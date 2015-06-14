#
# Telecom Churn problem
# modeling attempt
# extension of http://stackoverflow.com/questions/27080207/survival-analysis-for-telecom-churn-using-r
#

# source('churn.R',print.eval=T)

library(rattle) # The weather dataset and normVarNames().
library(randomForest) # Impute missing values using na.roughfix().
library(rpart) # decision tree
library(tidyr) # Tidy the dataset.
library(ggplot2) # Visualise data.
library(dplyr) # Data preparation and pipes %>%.
library(lubridate) # Handle dates.
#library(FSelector) # Feature selection.

nm  <- read.csv("http://www.sgi.com/tech/mlc/db/churn.names", 
               skip=4, colClasses=c("character", "NULL"), header=FALSE, sep=":")[[1]]
dat <- read.csv("http://www.sgi.com/tech/mlc/db/churn.data", header=FALSE, col.names=c(nm, "Churn"))
nobs<- nrow(ds);

dsname <- "dat"
ds <- get(dsname)
dim(ds)
(vars  <- names(ds))
target<- 'Churn';

ds$phone.number<-NULL; 
ds$churn<-(as.numeric(ds$Churn) - 1)
ds$Churn<-NULL
ds$state<-NULL


## ----split ds into train and test------------------------------------------------------------
## 75% of the sample size
smp_size <- floor(0.75 * nrow(ds))
## set the seed to make your partition reproductible
set.seed(123)
train_ind <- sample(seq_len(nrow(ds)), size = smp_size)
train <- ds[train_ind, ]
test <- ds[-train_ind, ]
dim(train)
dim(test)

corrgram(train, lower.panel=panel.ellipse, upper.panel=panel.pie);

lm.fit<-lm(churn~., data=train);
# Multiple R-squared:  0.1784,    Adjusted R-squared:  0.1724 

# how does the liniar model perform
pred.lm.fit<-predict(lm.fit, test);
RMSE.lm.fit<-sqrt(mean((pred.lm.fit-test$churn)^2))
RMSE.lm.fit; #0.3232695

# build a simpler model, similar R2
lm.fit.step<-lm(churn ~ international.plan + voice.mail.plan + total.day.charge + 
    total.eve.minutes + total.night.charge + total.intl.calls + 
    total.intl.charge + number.customer.service.calls, data=train);
# Multiple R-squared:  0.1767,    Adjusted R-squared:  0.174

pred.lm.fit.step<-predict(lm.fit.step, test);
RMSE.lm.fit.step<-sqrt(mean((pred.lm.fit.step-test$churn)^2))
RMSE.lm.fit.step; #0.3227848 <- simpler, and better RMSE

# logistic regression using a generalized linear model 
glm.step <- glm(churn ~ international.plan + voice.mail.plan + total.day.charge + 
    total.eve.minutes + total.night.charge + total.intl.calls + 
    total.intl.charge + number.customer.service.calls, family = binomial, data = train)
pred.glm.step <- predict.glm(glm.step, newdata = test, type = "response")
RMSE.glm.step<-sqrt(mean((pred.glm.step-test$churn)^2))
RMSE.glm.step; #0.3179586 <- better than the liniar model

# build a decision tree based on the selected variables
rpart.fit.step<-rpart(churn ~ international.plan + voice.mail.plan + total.day.charge + 
    total.eve.minutes + total.night.charge + total.intl.calls + 
    total.intl.charge + number.customer.service.calls, data=train, method="class");

pred.rpart.step<-predict(rpart.fit.step, test); # See correction below
RMSE.rpart.step<-sqrt(mean((pred.rpart.step-test$churn)^2))
RMSE.rpart.step; #0.6742183 <- much worse than the liniar model
# fancyRpartPlot(rpart.fit.step) < -overfitting

# forgot type="class"
pred.rpart.step<-as.numeric(predict(rpart.fit.step, test, type="class")) - 1;
RMSE.rpart.step<-sqrt(mean((pred.rpart.step-test$churn)^2))
RMSE.rpart.step; #0.2423902 <- better than the liniar model

sum(pred.rpart.step==test$churn)/nrow(test)
# 0.941247 94% tests are corectly matched

# Build Random Forest Ensemble
set.seed(415)
rf.fit.step <- randomForest(as.factor(churn) ~ international.plan + voice.mail.plan + total.day.charge + 
    total.eve.minutes + total.night.charge + total.intl.calls + 
    total.intl.charge + number.customer.service.calls,
                    data=train, importance=TRUE, ntree=2000)
varImpPlot(rf.fit.step);

pred.rf.fit.step<-as.numeric(predict(rf.fit.step, test))-1;
RMSE.rf.fit.step<-sqrt(mean((pred.rf.fit.step-test$churn)^2))
RMSE.rf.fit.step; #0.2217221 <-  improvement from the liniar model, so a non-liniar, decision tree approach is better
sum(pred.rf.fit.step==test$churn)/nrow(test)
# 0.9508393 95% tests are corectly matched


if (FALSE) {

# ignore data repeating for every row
(ids <- which(sapply(ds, function(x) length(unique(x))) == nrow(ds)))
ignore <- names(ids)

mvc <- sapply(ds[vars], function(x) sum(is.na(x)))
mvn <- names(which(mvc == nrow(ds)))
ignore <- union(ignore, mvn)



## ----ignore_missing_variables--------------------------------------------
mvc <- sapply(ds[vars], function(x) sum(is.na(x)))
mvn <- names(which(mvc == nrow(ds)))
ignore <- union(ignore, mvn)


## ----ignore_mostly_missing_variables-------------------------------------
mvn <- names(which(mvc >= 0.7*nrow(ds)))
ignore <- union(ignore, mvn)


## ----ignore_factors_with_many_levels-------------------------------------
factors <- which(sapply(ds[vars], is.factor))
lvls    <- sapply(factors, function(x) length(levels(ds[[x]])))
(many   <- names(which(lvls > 20)))
ignore  <- union(ignore, many)


## ----ignore_variables_constant_values------------------------------------
(constants <- names(which(sapply(ds[vars], function(x) all(x == x[1L])))))
ignore     <- union(ignore, constants)

## ------------------------------------------------------------------------
length(vars)
vars <- setdiff(vars, ignore)
length(vars)

}
