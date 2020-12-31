## Defining train and test set variables
trainFileName = "adult.data";
testFileName = "adult.test"

#Downloading the files

getwd()
setwd("F:/")

if (!file.exists (trainFileName))
  download.file (url = "http://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data", 
                 destfile = trainFileName)

if (!file.exists (testFileName))
  download.file (url = "http://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.test", 
                 destfile = testFileName)




## Creating a dataframe
colNames = c ("age", "workclass", "fnlwgt", "education", 
              "educationnum", "maritalstatus", "occupation",
              "relationship", "race", "sex", "capitalgain",
              "capitalloss", "hoursperweek", "nativecountry",
              "incomelevel")
## Train Dataset
train = read.table (trainFileName, header = FALSE, sep = ",",
                    strip.white = TRUE, col.names = colNames,
                    na.strings = "?", stringsAsFactors = TRUE)
write.csv(train,'adultTrain.csv',row.names=FALSE)
str(train)
head(train)

# Cleaning the data
table(complete.cases(train))

summary(train[!complete.cases(train),])
### native country, occupation and workclass have NA's


## Check income distribution
table(train$incomelevel)

## replace the incomplete records in workclass

workclass <- train$workclass
workclass <-as.character(workclass)
workclass[is.na(workclass)] = 'other'
workclass <- as.factor(workclass)
train$workclass <- workclass
train$workclass

## replace the incomplete records in occupation

occupation <- train$occupation
occupation
occupation <-as.character(occupation)
occupation[is.na(occupation)] = 'other'
occupation
occupation <- as.factor(occupation)
occupation
train$occupation <- occupation
train$occupation

## replace the incomplete records in nativecountry

nativecountry <- train$nativecountry
nativecountry <-as.character(nativecountry)
nativecountry[is.na(nativecountry)] = 'other'
nativecountry <- as.factor(nativecountry)
nativecountry
train$nativecountry <- nativecountry
train$nativecountry

mycleanTrain = train
head(mycleanTrain)
summary(mycleanTrain)
summary(mycleanTrain[!complete.cases(mycleanTrain),])

## removing the fnlwgt which is the estimate of population totals derived from CPS based on socio-economic characteristics of the population.
mycleanTrain$fnlwgt = NULL

# Prediction model

## Boosting Algorithm() with cross validation(to avoid over fitting)

install.packages("caret")
library(caret)
trCtrl = trainControl (method = "cv", number = 10)
boostFit = train (incomelevel ~ age + workclass + education + educationnum +maritalstatus + occupation + relationship +race + capitalgain + capitalloss + hoursperweek +nativecountry, trControl = trCtrl, method = "gbm", data = mycleanTrain, verbose = FALSE)

boostFit
confusionMatrix (mycleanTrain$incomelevel, predict (boostFit, mycleanTrain))

##Testing the model

test=read.csv('adultTest.csv',stringsAsFactors = TRUE)
head(test)
summary(test[!complete.cases(test),])
table(complete.cases(test))
table(test$incomelevel)
table(train$incomelevel)

#cleaning test data
## replace the incomplete records in workclass

workclass1 <- test$workclass
workclass1 <-as.character(workclass1)
workclass1[is.na(workclass1)] = 'other'
workclass1 <- as.factor(workclass1)
test$workclass <- workclass1
test$workclass
train$workclass
## replace the incomplete records in occupation

occupation1 <- test$occupation
occupation1 <-as.character(occupation1)
occupation1[is.na(occupation1)] = 'other'
occupation1 <- as.factor(occupation1)
occupation1
test$occupation <- occupation1
test$occupation

## replace the incomplete records in nativecountry

nativecountry1 <- test$nativecountry
nativecountry1 <-as.character(nativecountry1)
nativecountry1[is.na(nativecountry1)] = 'other'
nativecountry1 <- as.factor(nativecountry1)
nativecountry1
test$nativecountry <- nativecountry1
test$nativecountry

mycleanTest = test


mycleanTest$fnlwgt=NULL
head(mycleanTest)
summary(mycleanTest)
summary(test[!complete.cases(test),])

predicted = predict (boostFit, mycleanTest)
table(predicted)
table(mycleanTest$incomelevel)


levels(mycleanTest$incomelevel)[1]='<=50K'
levels(mycleanTest$incomelevel)[2]='>50K'
levels(mycleanTest$incomelevel)

confusionMatrix (mycleanTest$incomelevel,predict (boostFit, mycleanTest))

## ROC curve
library(ROCR) 
predict_LR<-predict(boostFit, newdata=mycleanTest,type='prob')
ROCRpred<-prediction(predict_LR[,2],mycleanTest$incomelevel)
perf5= performance(ROCRpred,"tpr","fpr")
plot(perf5,main="Boosted Tree ROC")
