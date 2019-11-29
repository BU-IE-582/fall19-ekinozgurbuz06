install.packages("glmnet", dependencies=TRUE)
install.packages("data.table")
install.packages("dplyr")
install.packages("tidyr")
library(dplyr)
library(data.table)
library(glmnet)
library(tidyr)


getwd()

alldata = read.csv("GercekZamanliTuketim-01012016-25112019.csv")

head(alldata)
str(alldata)

#Checking missing values
sum(is.na(alldata$Tarih))
sum(is.na(alldata$Saat))
sum(is.na(alldata$ActualConsumption))

alldata$Saat=as.integer(alldata$Saat)

#Assigning an integer valued day index to both training and test data
counter = 1
for (i in 1:34176){
  if(i==1){
    alldata$DayIndex[i]=1
  }
  else if(alldata$Tarih[i]!=alldata$Tarih[i-1]){
    counter=counter+1
    alldata$DayIndex[i]=counter
  }
  else{
    alldata$DayIndex[i]=counter 
  }
}
counter = 1

#Converting the consumption to double variables.
alldata$ActualConsumption <- gsub("\\.", "", alldata$ActualConsumption)
alldata$ActualConsumption <- gsub("\\,", ".", alldata$ActualConsumption)

alldata$ActualConsumption = as.numeric(alldata$ActualConsumption)


###########Part A###########
for (i in 1:34176){
  if(i<=48){
    alldata$lag_48[i]=0
    alldata$lag_168[i]=0
  }
  else if(i>=48 && i<=168){
    alldata$lag_168[i]=0 
    alldata$lag_48[i]=alldata$ActualConsumption[i-48]
  }
  else{
    alldata$lag_168[i]=alldata$ActualConsumption[i-168]
    alldata$lag_48[i]=alldata$ActualConsumption[i-48]
  }
}

alldata$lag_48 = as.numeric(alldata$lag_48)
alldata$lag_168 = as.numeric(alldata$lag_168)

sum(alldata$DayIndex >= 1400)
#600 hours to forecast
sum(alldata$DayIndex >= 1400) / 24
#25 days to forecast. 

ErrorNaive2Days = 0
ErrorNaive7Days = 0 
for(i in 1:600){
  ErrorNaive2Days[i] = 0
  ErrorNaive7Days[i] = 0
}

1399*24
#Scope of training data

for(i in 1:600){
  ErrorNaive2Days[i] = ErrorNaive2Days[i] + abs(((alldata$ActualConsumption[33576+i] - alldata$lag_48[33576+i])/alldata$ActualConsumption[33576+i])*100) 
  ErrorNaive7Days[i] = ErrorNaive7Days[i] + abs(((alldata$ActualConsumption[33576+i] - alldata$lag_168[33576+i])/alldata$ActualConsumption[33576+i])*100)
}

summary(ErrorNaive2Days)
summary(ErrorNaive7Days)

#########Part b

regressiondata= alldata[169:33576,]
##Assumption: First 168 row is excluded due to not having lag_168 column.

regressionbasic = lm(ActualConsumption~lag_48+lag_168,data=regressiondata)
for(i in 1:3){
  print(regressionbasic$coefficients[i])
}

summary(regressionbasic)
##All p-values are significantly low, therefore the linear model is reliable.

testdata = alldata[33577:34176,]
PredictedValuesBasic = predict(regressionbasic, newdata = testdata)

ErrorRegression = 0
for(i in 1:600){
  ErrorRegression[i] =  0
}
for(i in 1:600){
  ErrorRegression[i] = ErrorRegression[i] + abs(((testdata$ActualConsumption[i] - PredictedValuesBasic)/testdata$ActualConsumption[i])*100) 
}

summary(ErrorRegression)
mean(ErrorRegression)

###Mean of regression error is significantly higher than naive approaches. Part c actually will handle the issue of "hourly seperated predictions."

###########Part c###############

regressionHourdata1 = regressiondata[regressiondata$Saat==1,]
regressionHourdata2 = regressiondata[regressiondata$Saat==2,]
regressionHourdata3 = regressiondata[regressiondata$Saat==3,]
regressionHourdata4 = regressiondata[regressiondata$Saat==4,]
regressionHourdata5 = regressiondata[regressiondata$Saat==5,]
regressionHourdata6 = regressiondata[regressiondata$Saat==6,]
regressionHourdata7 = regressiondata[regressiondata$Saat==7,]
regressionHourdata8 = regressiondata[regressiondata$Saat==8,]
regressionHourdata9 = regressiondata[regressiondata$Saat==9,]
regressionHourdata10 = regressiondata[regressiondata$Saat==10,]
regressionHourdata11 = regressiondata[regressiondata$Saat==11,]
regressionHourdata12 = regressiondata[regressiondata$Saat==12,]
regressionHourdata13 = regressiondata[regressiondata$Saat==13,]
regressionHourdata14 = regressiondata[regressiondata$Saat==14,]
regressionHourdata15 = regressiondata[regressiondata$Saat==15,]
regressionHourdata16 = regressiondata[regressiondata$Saat==16,]
regressionHourdata17 = regressiondata[regressiondata$Saat==17,]
regressionHourdata18 = regressiondata[regressiondata$Saat==18,]
regressionHourdata19 = regressiondata[regressiondata$Saat==19,]
regressionHourdata20 = regressiondata[regressiondata$Saat==20,]
regressionHourdata21 = regressiondata[regressiondata$Saat==21,]
regressionHourdata22 = regressiondata[regressiondata$Saat==22,]
regressionHourdata23 = regressiondata[regressiondata$Saat==23,]
regressionHourdata24 = regressiondata[regressiondata$Saat==24,]

regressionbasichour1 = lm(ActualConsumption~lag_48+lag_168,data=regressionHourdata1)
regressionbasichour2 = lm(ActualConsumption~lag_48+lag_168,data=regressionHourdata2)
regressionbasichour3 = lm(ActualConsumption~lag_48+lag_168,data=regressionHourdata3)
regressionbasichour4 = lm(ActualConsumption~lag_48+lag_168,data=regressionHourdata4)
regressionbasichour5 = lm(ActualConsumption~lag_48+lag_168,data=regressionHourdata5)
regressionbasichour6 = lm(ActualConsumption~lag_48+lag_168,data=regressionHourdata6)
regressionbasichour7 = lm(ActualConsumption~lag_48+lag_168,data=regressionHourdata7)
regressionbasichour8 = lm(ActualConsumption~lag_48+lag_168,data=regressionHourdata8)
regressionbasichour9 = lm(ActualConsumption~lag_48+lag_168,data=regressionHourdata9)
regressionbasichour10 = lm(ActualConsumption~lag_48+lag_168,data=regressionHourdata10)
regressionbasichour11 = lm(ActualConsumption~lag_48+lag_168,data=regressionHourdata11)
regressionbasichour12 = lm(ActualConsumption~lag_48+lag_168,data=regressionHourdata12)
regressionbasichour13 = lm(ActualConsumption~lag_48+lag_168,data=regressionHourdata13)
regressionbasichour14 = lm(ActualConsumption~lag_48+lag_168,data=regressionHourdata14)
regressionbasichour15 = lm(ActualConsumption~lag_48+lag_168,data=regressionHourdata15)
regressionbasichour16 = lm(ActualConsumption~lag_48+lag_168,data=regressionHourdata16)
regressionbasichour17 = lm(ActualConsumption~lag_48+lag_168,data=regressionHourdata17)
regressionbasichour18 = lm(ActualConsumption~lag_48+lag_168,data=regressionHourdata18)
regressionbasichour19 = lm(ActualConsumption~lag_48+lag_168,data=regressionHourdata19)
regressionbasichour20 = lm(ActualConsumption~lag_48+lag_168,data=regressionHourdata20)
regressionbasichour21 = lm(ActualConsumption~lag_48+lag_168,data=regressionHourdata21)
regressionbasichour22 = lm(ActualConsumption~lag_48+lag_168,data=regressionHourdata22)
regressionbasichour23 = lm(ActualConsumption~lag_48+lag_168,data=regressionHourdata23)
regressionbasichour24 = lm(ActualConsumption~lag_48+lag_168,data=regressionHourdata24)

testdata1 = testdata[testdata$Saat==1,]
testdata2 = testdata[testdata$Saat==2,]
testdata3 = testdata[testdata$Saat==3,]
testdata4 = testdata[testdata$Saat==4,]
testdata5 = testdata[testdata$Saat==5,]
testdata6 = testdata[testdata$Saat==6,]
testdata7 = testdata[testdata$Saat==7,]
testdata8 = testdata[testdata$Saat==8,]
testdata9 = testdata[testdata$Saat==9,]
testdata10 = testdata[testdata$Saat==10,]
testdata11 = testdata[testdata$Saat==11,]
testdata12 = testdata[testdata$Saat==12,]
testdata13 = testdata[testdata$Saat==13,]
testdata14 = testdata[testdata$Saat==14,]
testdata15 = testdata[testdata$Saat==15,]
testdata16 = testdata[testdata$Saat==16,]
testdata17 = testdata[testdata$Saat==17,]
testdata18 = testdata[testdata$Saat==18,]
testdata19 = testdata[testdata$Saat==19,]
testdata20 = testdata[testdata$Saat==20,]
testdata21 = testdata[testdata$Saat==21,]
testdata22 = testdata[testdata$Saat==22,]
testdata23 = testdata[testdata$Saat==23,]
testdata24 = testdata[testdata$Saat==24,]

ErrorRegressionHourly = 0
for(i in 1:25){
  ErrorRegressionHourly[i] =  0
}

#Hour 1
PredictedValuesHour = predict(regressionbasichour1, newdata = testdata1)
for(i in 1:25){
  ErrorRegressionHourly[i] = ErrorRegressionHourly[i] + abs(((testdata1$ActualConsumption[i] - PredictedValuesHour)/testdata1$ActualConsumption[i])*100) 
}
ErrorRegressionHour1 = ErrorRegressionHourly
for(i in 1:25){
  ErrorRegressionHourly[i] =  0
}
#Hour 2
PredictedValuesHour = predict(regressionbasichour2, newdata = testdata2)
for(i in 1:25){
  ErrorRegressionHourly[i] = ErrorRegressionHourly[i] + abs(((testdata2$ActualConsumption[i] - PredictedValuesHour)/testdata2$ActualConsumption[i])*100) 
}
ErrorRegressionHour2 = ErrorRegressionHourly
for(i in 1:25){
  ErrorRegressionHourly[i] =  0
}
#Hour 3
PredictedValuesHour = predict(regressionbasichour3, newdata = testdata3)
for(i in 1:25){
  ErrorRegressionHourly[i] = ErrorRegressionHourly[i] + abs(((testdata3$ActualConsumption[i] - PredictedValuesHour)/testdata3$ActualConsumption[i])*100) 
}
ErrorRegressionHour3 = ErrorRegressionHourly
for(i in 1:25){
  ErrorRegressionHourly[i] =  0
}
#Hour 4
PredictedValuesHour = predict(regressionbasichour4, newdata = testdata4)
for(i in 1:25){
  ErrorRegressionHourly[i] = ErrorRegressionHourly[i] + abs(((testdata4$ActualConsumption[i] - PredictedValuesHour)/testdata4$ActualConsumption[i])*100) 
}
ErrorRegressionHour4 = ErrorRegressionHourly
for(i in 1:25){
  ErrorRegressionHourly[i] =  0
}
#Hour 5
PredictedValuesHour = predict(regressionbasichour5, newdata = testdata5)
for(i in 1:25){
  ErrorRegressionHourly[i] = ErrorRegressionHourly[i] + abs(((testdata5$ActualConsumption[i] - PredictedValuesHour)/testdata5$ActualConsumption[i])*100) 
}
ErrorRegressionHour5 = ErrorRegressionHourly
for(i in 1:25){
  ErrorRegressionHourly[i] =  0
}
#Hour 6
PredictedValuesHour = predict(regressionbasichour6, newdata = testdata6)
for(i in 1:25){
  ErrorRegressionHourly[i] = ErrorRegressionHourly[i] + abs(((testdata6$ActualConsumption[i] - PredictedValuesHour)/testdata6$ActualConsumption[i])*100) 
}
ErrorRegressionHour6 = ErrorRegressionHourly
for(i in 1:25){
  ErrorRegressionHourly[i] =  0
}
#Hour 7
PredictedValuesHour = predict(regressionbasichour7, newdata = testdata7)
for(i in 1:25){
  ErrorRegressionHourly[i] = ErrorRegressionHourly[i] + abs(((testdata7$ActualConsumption[i] - PredictedValuesHour)/testdata7$ActualConsumption[i])*100) 
}
ErrorRegressionHour7 = ErrorRegressionHourly
for(i in 1:25){
  ErrorRegressionHourly[i] =  0
}
#Hour 8
PredictedValuesHour = predict(regressionbasichour8, newdata = testdata8)
for(i in 1:25){
  ErrorRegressionHourly[i] = ErrorRegressionHourly[i] + abs(((testdata8$ActualConsumption[i] - PredictedValuesHour)/testdata8$ActualConsumption[i])*100) 
}
ErrorRegressionHour8 = ErrorRegressionHourly
for(i in 1:25){
  ErrorRegressionHourly[i] =  0
}
#Hour 9
PredictedValuesHour = predict(regressionbasichour9, newdata = testdata9)
for(i in 1:25){
  ErrorRegressionHourly[i] = ErrorRegressionHourly[i] + abs(((testdata9$ActualConsumption[i] - PredictedValuesHour)/testdata9$ActualConsumption[i])*100) 
}
ErrorRegressionHour9 = ErrorRegressionHourly
for(i in 1:25){
  ErrorRegressionHourly[i] =  0
}
#Hour 10
PredictedValuesHour = predict(regressionbasichour10, newdata = testdata10)
for(i in 1:25){
  ErrorRegressionHourly[i] = ErrorRegressionHourly[i] + abs(((testdata10$ActualConsumption[i] - PredictedValuesHour)/testdata10$ActualConsumption[i])*100) 
}
ErrorRegressionHour10 = ErrorRegressionHourly
for(i in 1:25){
  ErrorRegressionHourly[i] =  0
}
#Hour 11
PredictedValuesHour = predict(regressionbasichour11, newdata = testdata11)
for(i in 1:25){
  ErrorRegressionHourly[i] = ErrorRegressionHourly[i] + abs(((testdata11$ActualConsumption[i] - PredictedValuesHour)/testdata11$ActualConsumption[i])*100) 
}
ErrorRegressionHour11 = ErrorRegressionHourly
for(i in 1:25){
  ErrorRegressionHourly[i] =  0
}
#Hour 12
PredictedValuesHour = predict(regressionbasichour12, newdata = testdata12)
for(i in 1:25){
  ErrorRegressionHourly[i] = ErrorRegressionHourly[i] + abs(((testdata1$ActualConsumption[i] - PredictedValuesHour)/testdata12$ActualConsumption[i])*100) 
}
ErrorRegressionHour12 = ErrorRegressionHourly
for(i in 1:25){
  ErrorRegressionHourly[i] =  0
}
#Hour 13
PredictedValuesHour = predict(regressionbasichour13, newdata = testdata13)
for(i in 1:25){
  ErrorRegressionHourly[i] = ErrorRegressionHourly[i] + abs(((testdata13$ActualConsumption[i] - PredictedValuesHour)/testdata13$ActualConsumption[i])*100) 
}
ErrorRegressionHour13 = ErrorRegressionHourly
for(i in 1:25){
  ErrorRegressionHourly[i] =  0
}
#Hour 14
PredictedValuesHour = predict(regressionbasichour14, newdata = testdata14)
for(i in 1:25){
  ErrorRegressionHourly[i] = ErrorRegressionHourly[i] + abs(((testdata14$ActualConsumption[i] - PredictedValuesHour)/testdata14$ActualConsumption[i])*100) 
}
ErrorRegressionHour14 = ErrorRegressionHourly
for(i in 1:25){
  ErrorRegressionHourly[i] =  0
}
#Hour 15
PredictedValuesHour = predict(regressionbasichour15, newdata = testdata15)
for(i in 1:25){
  ErrorRegressionHourly[i] = ErrorRegressionHourly[i] + abs(((testdata15$ActualConsumption[i] - PredictedValuesHour)/testdata15$ActualConsumption[i])*100) 
}
ErrorRegressionHour15 = ErrorRegressionHourly
for(i in 1:25){
  ErrorRegressionHourly[i] =  0
}
#Hour 16
PredictedValuesHour = predict(regressionbasichour16, newdata = testdata16)
for(i in 1:25){
  ErrorRegressionHourly[i] = ErrorRegressionHourly[i] + abs(((testdata16$ActualConsumption[i] - PredictedValuesHour)/testdata16$ActualConsumption[i])*100) 
}
ErrorRegressionHour16 = ErrorRegressionHourly
for(i in 1:25){
  ErrorRegressionHourly[i] =  0
}
#Hour 17
PredictedValuesHour = predict(regressionbasichour17, newdata = testdata17)
for(i in 1:25){
  ErrorRegressionHourly[i] = ErrorRegressionHourly[i] + abs(((testdata17$ActualConsumption[i] - PredictedValuesHour)/testdata17$ActualConsumption[i])*100) 
}
ErrorRegressionHour17 = ErrorRegressionHourly
for(i in 1:25){
  ErrorRegressionHourly[i] =  0
}
#Hour 18
PredictedValuesHour = predict(regressionbasichour18, newdata = testdata18)
for(i in 1:25){
  ErrorRegressionHourly[i] = ErrorRegressionHourly[i] + abs(((testdata18$ActualConsumption[i] - PredictedValuesHour)/testdata18$ActualConsumption[i])*100) 
}
ErrorRegressionHour18 = ErrorRegressionHourly
for(i in 1:25){
  ErrorRegressionHourly[i] =  0
}
#Hour 19
PredictedValuesHour = predict(regressionbasichour19, newdata = testdata19)
for(i in 1:25){
  ErrorRegressionHourly[i] = ErrorRegressionHourly[i] + abs(((testdata19$ActualConsumption[i] - PredictedValuesHour)/testdata19$ActualConsumption[i])*100) 
}
ErrorRegressionHour19 = ErrorRegressionHourly
for(i in 1:25){
  ErrorRegressionHourly[i] =  0
}
#Hour 20
PredictedValuesHour = predict(regressionbasichour20, newdata = testdata20)
for(i in 1:25){
  ErrorRegressionHourly[i] = ErrorRegressionHourly[i] + abs(((testdata20$ActualConsumption[i] - PredictedValuesHour)/testdata20$ActualConsumption[i])*100) 
}
ErrorRegressionHour20 = ErrorRegressionHourly
for(i in 1:25){
  ErrorRegressionHourly[i] =  0
}
#Hour 21
PredictedValuesHour = predict(regressionbasichour21, newdata = testdata21)
for(i in 1:25){
  ErrorRegressionHourly[i] = ErrorRegressionHourly[i] + abs(((testdata21$ActualConsumption[i] - PredictedValuesHour)/testdata21$ActualConsumption[i])*100) 
}
ErrorRegressionHour21 = ErrorRegressionHourly
for(i in 1:25){
  ErrorRegressionHourly[i] =  0
}
#Hour 22
PredictedValuesHour = predict(regressionbasichour22, newdata = testdata22)
for(i in 1:25){
  ErrorRegressionHourly[i] = ErrorRegressionHourly[i] + abs(((testdata22$ActualConsumption[i] - PredictedValuesHour)/testdata22$ActualConsumption[i])*100) 
}
ErrorRegressionHour22 = ErrorRegressionHourly
for(i in 1:25){
  ErrorRegressionHourly[i] =  0
}
#Hour 23
PredictedValuesHour = predict(regressionbasichour23, newdata = testdata23)
for(i in 1:25){
  ErrorRegressionHourly[i] = ErrorRegressionHourly[i] + abs(((testdata23$ActualConsumption[i] - PredictedValuesHour)/testdata23$ActualConsumption[i])*100) 
}
ErrorRegressionHour23 = ErrorRegressionHourly
for(i in 1:25){
  ErrorRegressionHourly[i] =  0
}
#Hour 24
PredictedValuesHour = predict(regressionbasichour24, newdata = testdata24)
for(i in 1:25){
  ErrorRegressionHourly[i] = ErrorRegressionHourly[i] + abs(((testdata24$ActualConsumption[i] - PredictedValuesHour)/testdata24$ActualConsumption[i])*100) 
}
ErrorRegressionHour24 = ErrorRegressionHourly
for(i in 1:25){
  ErrorRegressionHourly[i] =  0
}

GraphHourlyRegression = 0
for(i in 1:24){
  GraphHourlyRegression[i] =  0
}

Graph = c(mean(ErrorRegressionHour1), mean(ErrorRegressionHour2), mean(ErrorRegressionHour3), mean(ErrorRegressionHour4), mean(ErrorRegressionHour5), mean(ErrorRegressionHour6), mean(ErrorRegressionHour7), mean(ErrorRegressionHour8), mean(ErrorRegressionHour9), mean(ErrorRegressionHour10), mean(ErrorRegressionHour11), mean(ErrorRegressionHour12), mean(ErrorRegressionHour13), mean(ErrorRegressionHour14), mean(ErrorRegressionHour15), mean(ErrorRegressionHour16), mean(ErrorRegressionHour17), mean(ErrorRegressionHour18), mean(ErrorRegressionHour19), mean(ErrorRegressionHour20), mean(ErrorRegressionHour21), mean(ErrorRegressionHour22), mean(ErrorRegressionHour23), mean(ErrorRegressionHour24))
plot(Graph, type="h", xaxt = "n",xlab="Hours", ylab="MAPE Values", main = "Regression for Hours")
axis(side=1,at=seq(1:24))

##As can be seen from the graph, error during afternoon hours is relatively higher. Predictions to night hours are seem to be quite well.

#############Part d################

wideMatrix = matrix(nrow = 34176, ncol=48)
dummyIndex = 1
rowOperator = 169
for(i in 1:34176){
  for(j in 1:48){
    if(i<=168){
      wideMatrix[i,j] = 0
    }
    else{
      if(j<=24)
      {
        wideMatrix[i,j] = alldata$ActualConsumption[rowOperator-48+j-1]
      }
      else
      {
        wideMatrix[i,j] = alldata$ActualConsumption[rowOperator-168+j-1-24]
      }
    }
  }
  if(i>168)
  {
    dummyIndex = dummyIndex + 1
    if(dummyIndex >24)
    {
      dummyIndex = 1
      rowOperator = rowOperator + 24
    }
  }
}

wideMatrix = as.data.table(wideMatrix)
wideTrainingMatrix = rbind(wideMatrix[169:33576])
wideTestMatrix = rbind(wideMatrix[33577:34176])
#First 168 entries and test interval is excluded.
head(wideTrainingMatrix)
#Every consecutive 24 row has same values, since they all share same date.
#Row operator increases by 24 after a day passed. DummyIndex controls this mod process.

lassoMatrix = cbind(regressiondata,wideTrainingMatrix)
lassoTestMatrix = cbind(testdata,wideTestMatrix)
#Verification of wideMatrix can be done by comparing lag_48 & lag_168 values with newly added for loop values.
#Double check is done. So proud

lassoTestMatrix$Prediction = 0


lambdalist = data.table(model = paste0("LassoModel-",c(0:23)),lambdavalues = numeric(), lambdamin = numeric())

lassoTestMatrix$Saat = as.numeric(lassoTestMatrix$Saat)
lassoMatrix$Saat = as.numeric(lassoMatrix$Saat)

LassoTestHourly = split(lassoTestMatrix,lassoTestMatrix$Saat)
LassoTrainHourly = split(lassoMatrix,lassoMatrix$Saat)

set.seed(523)

resultLasso = subset(lassoTestMatrix[c(2,3)])
resultLasso$ActualConsumption = 0
resultLassoHourly = split(resultLasso, resultLasso$Saat)

xtest=lassoTestMatrix[c(7:54)]

errorLassoHourly = vector(mode="list", length = 24)
MAPEList = vector(mode = "list", length = 24)
Coefficients = vector(mode = "list", length = 25)

for(i in 1:24){
  x=as.matrix(LassoTrainHourly[[i]][c(7:54)])
  y=LassoTrainHourly[[i]]$ActualConsumption
  LassoModel = cv.glmnet(x,y,type.measure = "mse", alpha=1,family="gaussian", nfolds=10)
  assign(paste0("LassoModel",i),LassoModel)
  resultLassoHourly[[i]]$ActualConsumption = predict(LassoModel, newx = as.matrix(LassoTestHourly[[i]][c(7:54)]), s="lambda.1se")
  lambdalist[i]$lambdamin = LassoModel$lambda.min
  lambdalist[i]$lambdavalues = LassoModel$lambda.1se
  
  Coefficients[[i]] = coef(LassoModel)
  errorLassoHourly[[i]] = resultLassoHourly[[i]]$ActualConsumption - LassoTestHourly[[i]]$ActualConsumption
  MAPEList[[i]] = sum(abs(errorLassoHourly[[i]]/LassoTestHourly[[i]]$ActualConsumption))*100/25
}

plot(x=seq(1,24,1),MAPEList, type="h", xlab="Hours", ylab="MAPE Values")
axis(side=1,at=seq(1:24))

plot(lambdalist$lambdavalues, xlab="Hours", ylab="lambda Values")
axis(side=1,at=seq(1:24))
plot(lambdalist$lambdamin, xlab="Hours", ylab="lambda Values")
axis(side=1,at=seq(1:24))



for(i in 1:24){
  print(summary(Coefficients[[i]]))
}

par(mfrow = c(2,2))
plot(LassoModel1)
plot(LassoModel2)
plot(LassoModel3)
plot(LassoModel4)
plot(LassoModel5)
plot(LassoModel6)
plot(LassoModel7)
plot(LassoModel8)
plot(LassoModel9)
plot(LassoModel10)
plot(LassoModel11)
plot(LassoModel12)
plot(LassoModel13)
plot(LassoModel14)
plot(LassoModel15)
plot(LassoModel16)
plot(LassoModel17)
plot(LassoModel18)
plot(LassoModel19)
plot(LassoModel20)
plot(LassoModel21)
plot(LassoModel22)
plot(LassoModel23)
plot(LassoModel24)

dev.off()

#As you can see the graph below which is constructed by gathering all types of errors in same plot, Lasso regression seem to have best result. On the other hand, lag_168 naive approach prediction is quite well. 
#Scaling Lasso regressions may change the result.
#However, for the sake of simplicity, I would prefer lag_168 naive approach.

MAPElassoError = 0
for(i in 1:24){
  MAPElassoError[i] = 0
}
for(i in 1:24){
  MAPElassoError[i] = mean(MAPEList[[i]])
}

errors = data.frame(MAPElag48 = ErrorNaive2Days, MAPElag168 = ErrorNaive7Days, MAPElinearreg = ErrorRegression, MAPEhourlylinearreg = Graph, MAPElasso = MAPElassoError)
summary(errors)
boxplot(errors)
