library(rpart)
library(cart)
library(Hmisc)
library(plyr)
library(randomForest)
library(tree)
library(gbm)
library(caret)

train = read.csv("train.csv")
test = read.csv("test.csv")
weather = read.csv("weather.csv")
key = read.csv("key.csv")
samplesub = read.csv("sampleSubmission.csv")

#impute values in weather
weather$tmax[weather$tmax=="M"] <- NA
weather$tmax <- impute(weather$tmax , mean)
weather$tmax <- as.integer(weather$tmax)

weather$tmin[weather$tmin=="M"] <- NA
weather$tmin <- impute(weather$tmin, mean)
weather$tmin <- as.integer(weather$tmin)

weather$tavg[weather$tavg=="M"] <- NA
weather$tavg <- impute(weather$tavg, mean)
weather$tavg <- as.integer(weather$tavg)

weather$depart[weather$depart=="M"] <- NA
weather$depart <- impute(weather$depart, mean)

weather$dewpoint[weather$dewpoint=="M"] <- NA
weather$dewpoint <- impute(weather$dewpoint, mean)
weather$dewpoint <- as.integer(weather$dewpoint)

weather$wetbulb[weather$wetbulb=="M"] <- NA
weather$wetbulb <- impute(weather$wetbulb, mean)

weather$heat[weather$heat=="M"] <- NA
weather$heat <- impute(weather$heat, mean)
weather$heat <- as.integer(weather$heat)

weather$cool[weather$cool=="M"] <- NA
weather$cool <- impute(weather$cool, mean)
weather$cool <- as.integer(weather$cool)

weather$snowfall[weather$snowfall=="M"] <- NA
weather$snowfall[weather$snowfall=="  T"] <- NA
weather$snowfall <- impute(weather$snowfall, mean)
weather$snowfall < as.integer(weather$snowfall)

weather$preciptotal[weather$preciptotal=="M"] <- NA
weather$preciptotal[weather$preciptotal=="  T"] <- NA
weather$preciptotal <- impute(weather$preciptotal, mean)

weather$stnpressure[weather$stnpressure=="M"] <- NA
weather$stnpressure <- impute(weather$stnpressure, mean)

weather$sealevel[weather$sealevel=="M"] <- NA
weather$sealevel <- impute(weather$sealevel, mean)

weather$resultspeed[weather$resultspeed=="M"] <- NA
weather$resultspeed <- impute(weather$resultspeed, mean)

weather$avgspeed[weather$avgspeed=="M"] <- NA
weather$avgspeed <- impute(weather$avgspeed, mean)

weather$codesum <- as.factor(weather$codesum)

#Do an outer join of the store keys, in the kev.csv with the store keys in train/test, match with weather
#as well

keytrain <- merge(train, key, by="store_nbr")
keytest <- merge(test, key, by="store_nbr")

trainweather <- merge(keytrain, weather, by=c("station_nbr", "date"))
testweather <- merge(keytest, weather, by=c("station_nbr", "date"))

#random forest
rmodel <- randomForest(units ~ tmax+tmin+tavg, data=trainweather,ntree=20)

#boost
boostmodel <- gbm(units ~ ., data=trainweather, n.trees=100, interaction.depth=20, shrinkage=.1, verbose=TRUE)

#predict
rpredict <- predict(rmodel, testweather, ntree=20)

rpredict <- predict(boostmodel, testweather, n.trees=100)

rsubmit <- data.frame(id = samplesub$id, units = as.integer(rpredict))

#write csv

write.csv(rsubmit, file="Rforest.csv", row.names=FALSE)
write.csv(rsubmit, file="BoostPred.csv", row.names=FALSE)
