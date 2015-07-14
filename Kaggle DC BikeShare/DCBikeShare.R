train = read.csv("train.csv")
test = read.csv("test.csv")
sampleSubmission = read.csv("sampleSubmission.csv")

#PreProcessing
train$workingday = as.factor(train$workingday)
test$workingday = as.factor(test$workingday)
train$season = as.factor(train$season)
test$season = as.factor(test$season)
train$holiday = as.factor(train$holiday)
test$holiday = as.factor(test$holiday)
train$weather = as.factor(train$weather)
test$weather = as.factor(test$weather)

train$workday <- train$workingday == 1 & train$holiday == 0
train$workday <- as.factor(train$workday)
train$nonworkday <- train$workingday == 0 | train$holiday == 1
train$nonworkday <- as.factor(train$nonworkday)

test$workday <- test$workingday == 1 & test$holiday == 0
test$workday <- as.factor(test$workday)
test$nonworkday <- test$workingday == 0 | test$holiday == 1
test$nonworkday <- as.factor(test$nonworkday)

#transform registered, count and casual columns into log to fit scoring metrics
library(math)

for col in ['casual', 'registered', 'count']:
  train['log-' + col] = train[col].apply(lambda x: log(1 + x))

train$casual <- train$casual.sapply(log(1 + x))

train$count <- log((train$count)+1)
train$registered <- log((train$registered)+1)
train$casual <- log((train$casual)+1)


#Date work

train$datetime <- strptime(train$datetime, format="%Y-%m-%d %H:%M:%S")
train$day <- as.factor(train$datetime$wday)
train$month <- as.factor(train$datetime$mon)
train$hour <- as.factor(train$datetime$hour)
train$year <- as.factor(train$datetime$year + 1900)
train$sunday <- as.factor(train$day == 0)

test$datetime <- strptime(test$datetime, format="%Y-%m-%d %H:%M:%S")
test$day <- as.factor(test$datetime$wday)
test$month <- as.factor(test$datetime$mon)
test$hour <- as.factor(test$datetime$hour)
test$year <- as.factor(test$datetime$year + 1900)
test$sunday <- as.factor(test$day == 0)

#features
features = ['datetime', 'season', 'holiday', 'workingday', 'weather',
            'temp', 'atemp', 'humidity', 'windspeed']

#Decision Trees
library(rpart)
DC.Tree = rpart(count~season+weather+atemp+humidity+day+hour+year+sunday, data=train)
plot(DC.Tree)
text(DC.Tree)
Tree.Pred = predict(DC.Tree,newdata=test)

#filter train data
library(dplyr)
castrain <- train %>% select(season, weather, atemp, humidity, day, hour, year, 
                             sunday, workday, nonworkday, casual)
regtrain <- train %>% select(season, weather, atemp, humidity, day, hour, year, 
                             sunday, workday, nonworkday, registered)
counttrain <- train %>% select(season, weather, atemp, humidity, day, hour, year, 
                               sunday, workday, nonworkday, count)

counttest <- test %>% select(season, weather, atemp, humidity, day, hour, 
                             year, sunday, workday, nonworkday, count)

#Random Forest
library(randomForest)
library(party)
library(miscTools)
Casual.RForest = randomForest(casual~weather+season+atemp+holiday+humidity+hour+year+workday+
                                sunday+nonworkday+day, data=train, ntrees=1000, mtry=5, importance=TRUE, 
                              cvfolds=5, verbose=TRUE)
Registered.RForest = randomForest(registered~season+weather+atemp+holiday+humidity+day+hour+
                                    year+workday+sunday+nonworkday,data=train,
                                  ntrees=1000, mtry=5, importance=TRUE, cvfolds=5)

test$casual <- predict(Casual.RForest, test)
test$casual <- exp((test$casual)-1)
test$registered <- predict(Registered.RForest, test)
test$registered <- exp((test$registered)-1)
test$count <- abs(test$casual + test$registered)
test$count <- round(test$count, 0)
summary(test$count)

DC.RForest = randomForest(count~season+weather+atemp+humidity+day+hour+year+workday+sunday+
                            nonworkday,data=train,ntrees=5000, cvfolds=10)
print(DC.RForest)
importance(DC.RForest)
RForest.Pred = predict(DC.RForest,newdata=test,ntrees=5000)

RForest.Submit = data.frame(datetime = sampleSubmission$datetime, count = test$count)

r2 <- rSquared(test$count, test$count - predict(DC.Rforest, test))
mse <- mean((test$count - RForest.Pred)^2)


#Boosting
library(gbm)
DC.Boost = gbm(count~season+weather+atemp+humidity+day+hour+year+sunday+
                 workday+nonworkday,data=train,n.trees=5000,interaction.depth=15, shrinkage=0.1)
Boost.Pred = predict(DC.Boost, newdata=test, n.trees=5000)

Casual.Boost = gbm(casual~.,data=castrain,n.trees=5000,interaction.depth=15, 
                   shrinkage=0.1, cv.fold=5, bag.fraction=.5, train.fraction=.5, verbose=TRUE)
Registered.Boost = gbm(registered~season+.,data=regtrain,n.trees=5000,interaction.depth=15, 
                       shrinkage=0.1, cv.fold=5, bag.fraction=.5, train.fraction=.5, verbose=TRUE)

test$casual = predict(Casual.Boost, test, n.trees=5000)
test$registered = predict(Registered.Boost, test, n.trees=5000)
test$count = abs(test$casual + test$registered)
test$count = round(test$count, 0)

Boost.Submit = data.frame(datetime = sampleSubmission$datetime, count = test$count)

pcount <- predict(gbmfit1, counttest)

#Neural Networks
library(lubridate)
library(neuralnets)

#Validation
library(caret)
set.seed(123)
inTrain <- createDataPartition(train$count, p=0.7, list=F, times=1)
modeltrain <- train[inTrain,]
cv <- train[-inTrain,]

fitControl <- trainControl(method="repeatedcv", number=5, repeats=5)

gbmGrid <- expand.grid(interaction.depth=3,n.trees=100,shrinkage=0.1)

gbmfit1 <- train(count ~ ., data=counttrain, method="gbm", trControl=fitControl, 
                 verbose=FALSE, tuneGrid=gbmGrid, metric="RMSLE")
tcount <- predict(gbmfit1, counttrain)

#Root mean sqaured log error
library(Metrics)
rmsle(counttrain, tcount)

#Write Files
write.csv(Boost.Submit, "Boost.Pred.csv", row.names=FALSE)
write.csv(RForest.Submit, "RForest.Pred.csv", row.names=FALSE)
