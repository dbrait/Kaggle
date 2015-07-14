library(randomForest)
library(plyr)
library(gbm)
library(caret)
library(rpart)
library(stats)
library(dplyr)
library(FactoMineR)
library(ggplot2)
library(cluster)
library(Hmisc)
library(caret)


train = read.csv("train.csv")
test = read.csv("test.csv")
bids = read.csv("bids.csv")
samplesub = read.csv("sampleSubmission.csv")

#merge bids and train/test
trainbids <- merge(train, bids, by="bidder_id")
testbids <- merge(test, bids, by="bidder_id")

#count url
head(subset(trainbids, select="url"))
factor(trainbids$url)
ez = table(trainbids$url)
tz = as.data.frame(ez)
head(tz)
names(tz)[1] = "url"
urltrain <- merge(trainbids, tz, by="url")
names(urltrain)[13] = "urlFreq"

#best count method for making it a variable
head(subset(urltrain, select="ip"))
factor(urltrain$ip)
er = table(urltrain$ip)
class(er)
tb = as.data.frame(er)
head(tb)
names(tb)[1] = "ip"
urltrain <- merge(urltrain, tb, by="ip")
names(urltrain)[14] = "ipFreq"

#count auction
head(subset(urltrain, select="auction"))
factor(urltrain$auction)
ac = table(urltrain$auction)
class(ac)
az = as.data.frame(ac)
names(az)[1] = "auction"
urltrain <- merge(urltrain, az, by="auction")
names(urltrain)[15] = "aucFreq"

#test count url and ip
head(subset(testbids, select="url"))
factor(testbids$url)
tesz = table(testbids$url)
class(tesz)
tzest = as.data.frame(tesz)
head(tzest)
names(tzest)[1] = "url"
urltest <- merge(testbids, tzest, by="url")
names(urltest)[12] = "urlFreq"

head(subset(testbids, select="ip"))
factor(testbids$ip)
tesi = table(testbids$ip)
class(tesi)
tip = as.data.frame(tesi)
names(tip)[1] = "ip"
urltest <- merge(urltest, tip, by="ip")
names(urltest)[13] = "ipFreq"

#time feature eng
timsor <- urltrain %>% filter(outcome == 1) %>% group_by(time) %>% count(time)
timnil <- urltrain %>% filter(outcome == 0) %>% group_by(time) %>% count(time)
timtot <- join(timsor, timnil, type="inner", by="time")
names(timtot)[2] = "posfreq"
names(timtot)[3] = "negfreq"
timtot$timdiff = timtot$posfreq/timtot$negfreq

urltrainnew <- join(urltrainnew, timtot, type="left", by="time")
urltrainnew$timdif[is.na(urltrainnew$timdif)] <- 0

#country feature engineering
countsor <- urltrain %>% filter(outcome == 1) %>% group_by(country) %>% count(country)

countnil <- urltrain %>% filter(outcome == 0) %>% group_by(country) %>% count(country)

countot <- join(countsor, countnil, type="inner", by="country")
names(countot)[2] = "posfreq"
names(countot)[3] = "negfreq"
countot$diff = countot$posfreq/countot$negfreq

urltrainnew <- join(urltrain, countot, type="left", by="country")

names(urltrainnew)[18] <- "countrydiff"
urltrainnew$countrydiff[is.na(urltrainnew$countrydiff)] <- 0

counttestot <- join(countsortest, countniltest, type="inner", by="country")
names(counttestot)[2] = "posfreq"
names(counttestot)[3] = "negfreq"
counttestot = counttestot$posfreq/counttestot$negfreq

urltestnew <- join(urltest, countot, type="left", by="country")
names(urltestnew)[16] <- "countrydiff"
urltestnew$countrydiff[is.na(urltestnew$countrydiff)] <- 0

#device feature eng
countdev <- urltrain %>% filter(outcome == 1) %>% group_by(device) %>% count(device)

countdevnil <- urltrain %>% filter(outcome == 0) %>% group_by(device) %>% count(device)

countdevtot <- join(countdev, countdevnil, type="inner", by="device")
names(countdevtot)[2] = "pdevfreq"
names(countdevtot)[3] = "ndevfreq"
countdevtot$devdiff = countdevtot$pdevfreq/countdevtot$ndevfreq

urltrainnew <- join(urltrainnew, countdevtot, type="left", by="device")

urltestnew <- join(urltestnew, countdevtot, type="left", by="device")
urltestnew$devdiff[is.na(urltestnew$devdiff)] <- 0

#spliting dataset
set.seed(123)
inTrain <- createDataPartition(urltrainnew$outcome, p=0.7, list=F, times=1)
modeltrain <- urltrainnew[inTrain,]
cv <- urltrainnew[-inTrain,]

#First ROC look
fit <- glm(outcome ~ merchandise+ipFreq+urlFreq+countrydiff+devdiff+aucFreq, data=modeltrain, family=binomial())
prob=predict(fit, type=c("response"), cv)
cv$prob = prob
library(pROC)
ROC <- roc(outcome==1 ~ prob, data= cv)
plot(ROC)
confusion <- table(prob>.5, cv$outcome)
errorrate <- sum(diag(confusion))/sum(confusion)
errorrate

#logreg
fblogit <- glm(outcome ~ merchandise+ipFreq+urlFreq+countrydiff+devdiff, data=urltrainnew, family=binomial())

#predict logit

prob <- predict(fblogit, type=c("response"), urltestnew)
urltestnew$prob <- prob

#create bid prediction data frame
logbidpred <- data.frame(bidder_id = urltest$bidder_id, prediction=urltestnew$prob)

#want a column that adds total prediction value for each bidder_id and divdes that by number of occurenes of bibder_id
by_bid <- logbidpred %>% group_by(bidder_id)

endnum <- summarise(by_bid, mean(prediction))
names(endnum)[2] <- "newpred"

subread <- join(samplesub, endnum, type="left")
subread$prediction <- NULL
subread$newpred <- abs(subread$newpred)
subread$prediction <- subread$newpred
subread$newpred <- NULL

subread$prediction[is.na(subread$prediction)] <- 0

#Random Forest
Rforest <- randomForest(outcome ~ merchandise+ipFreq+urlFreq+countrydiff+devdiff, data=urltrainnew, ntrees=5)

#Predict Rforest
predRforest <- predict(Rforest, urltest, ntrees=500)

#Boost
Boost <- gbm(outcome ~ merchandise+ipFreq+urlFreq+countrydiff+devdiff, data=urltrainnew, interaction.depth=5, shrinkage=.1, n.trees=500, verbose=TRUE)

#predict Boost
predBoost <- predict(Boost, urltestnew, type=c("response"), n.trees=500)
pboost <- data.frame(bidder_id = urltest$bidder_id, prediction=predBoost)
p_bid <- pboost %>% group_by(bidder_id)
lasnum <- summarise(p_bid, mean(prediction))
names(lasnum)[2] <- "pred"
submissionre <- join(samplesub, lasnum, by="bidder_id", type="full")
submissionre$prediction <- NULL
submissionre$pred[is.na(submissionre$pred)] <- 0
submissionre$prediction <- submissionre$pred
submissionre$pred <- NULL

#ROC
ROC <- roc(outcome==1 ~ prob, data= urltest)
plot(ROC)
confusion <- table(prob>.5, cv$outcome)
errorrate <- sum(diag(confusion))/sum(confusion)
errorrate

#submit function
boostsubmit <- data.frame(bidder_id = samplesub$bidder_id, prediction=predBoost)

#write csv
write.csv(submissionre, file="PredBoost.csv", row.names=FALSE)
write.csv(subread, file="PredLog.csv", row.names=FALSE)
