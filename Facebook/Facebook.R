
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

summary(test)
summary(train)
summary(bids)

str(bids)
str(train)

#merge bids and train/test
trainbids <- merge(train, bids, by="bidder_id")
testbids <- merge(test, bids, by="bidder_id")

str(trainbids)

summary(trainbids)
summary(testbids)

#spliting dataset
set.seed(123)
inTrain <- createDataPartition(urltrainnew$outcome, p=0.7, list=F, times=1)
summary(inTrain)
modeltrain <- urltrainnew[inTrain,]
cv <- urltrainnew[-inTrain,]
summary(modeltrain)
str(modeltrain)

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

#auction duraction
trainbids %>% group_by(auction) %>% summ

#creating dummy variables for variables with too many levels
#for country
for(level in unique(trainbids$country)){
  trainbids[paste("dummy", level, sep="_")] <- ifelse(trainbids$country == level, 1, 0)
}
#for device
for(level in unique(trainbids$device)){
  trainbids[paste("dummy", level, sep="_")] <- ifelse(trainbids$device == level, 1, 0)
}

for(level in unique(trainbids$payment_account)){
  trainbids[paste("dummy", level, sep="_")] <- ifelse(trainbids$payment_account == level, 1, 0)
}

for(level in unique(trainbids$merchandise)){
  trainbids[paste("dummy", level, sep="_")] <- ifelse(trainbids$merchandise == level, 1, 0)
}

#feature based on bidder_ids that use multiple devices, bidder_ids that access from multiple countries, etc
countus <- trainbids %>% group_by(bidder_id) %>% filter(country == "us")

summary(countus)
countus.f <- factor(countus)

trainbids$factor_country_1 <- factor(with(trainbids, ifelse((country == "us"),1 ,0)))
trainbids$factor_county_1

#time feature eng
timsor <- urltrain %>% filter(outcome == 1) %>% group_by(time) %>% count(time)
timnil <- urltrain %>% filter(outcome == 0) %>% group_by(time) %>% count(time)
timtot <- join(timsor, timnil, type="inner", by="time")
names(timtot)[2] = "posfreq"
names(timtot)[3] = "negfreq"
timtot$timdiff = timtot$posfreq/timtot$negfreq

urltrainnew <- join(urltrainnew, timtot, type="left", by="time")
urltrainnew$timdif[is.na(urltrainnew$timdif)] <- 0

#data exploration

#country feature engineering
countsor <- urltrain %>% filter(outcome == 1) %>% group_by(country) %>% count(country)
class(countsor)
summary(countsor)
countsor

countnil <- urltrain %>% filter(outcome == 0) %>% group_by(country) %>% count(country)
summary(countnil)
countnil



countot <- join(countsor, countnil, type="inner", by="country")
summary(countot)
class(countot)
names(countot)[2] = "posfreq"
names(countot)[3] = "negfreq"
countot$diff = countot$posfreq/countot$negfreq
countot

urltrainnew <- join(urltrain, countot, type="left", by="country")


names(urltrainnew)[18] <- "countrydiff"
urltrainnew$countrydiff[is.na(urltrainnew$countrydiff)] <- 0


counttestot <- join(countsortest, countniltest, type="inner", by="country")
names(counttestot)[2] = "posfreq"
names(counttestot)[3] = "negfreq"
counttestot = counttestot$posfreq/counttestot$negfreq
counttestot

urltestnew <- join(urltest, countot, type="left", by="country")
names(urltestnew)[16] <- "countrydiff"
urltestnew$countrydiff[is.na(urltestnew$countrydiff)] <- 0

#device feature eng
countdev <- urltrain %>% filter(outcome == 1) %>% group_by(device) %>% count(device)
class(countdev)
summary(countdev)

countdevnil <- urltrain %>% filter(outcome == 0) %>% group_by(device) %>% count(device)
summary(countdevnil)

countdevtot <- join(countdev, countdevnil, type="inner", by="device")
summary(countdevtot)
names(countdevtot)[2] = "pdevfreq"
names(countdevtot)[3] = "ndevfreq"
countdevtot$devdiff = countdevtot$pdevfreq/countdevtot$ndevfreq
countdevtot

urltrainnew <- join(urltrainnew, countdevtot, type="left", by="device")

urltestnew <- join(urltestnew, countdevtot, type="left", by="device")
urltestnew$devdiff[is.na(urltestnew$devdiff)] <- 0

#preprocess
#url

#count url
head(subset(trainbids, select="url"))
factor(trainbids$url)
ez = table(trainbids$url)
class(ez)
tz = as.data.frame(ez)
head(tz)
names(tz)[1] = "url"
tz
urltrain <- merge(trainbids, tz, by="url")
summary(urltrain)
head(urltrain)
names(urltrain)[13] = "urlFreq"

#best count method for making it a variable
head(subset(urltrain, select="ip"))
factor(urltrain$ip)
er = table(urltrain$ip)
class(er)
tb = as.data.frame(er)
head(tb)
names(tb)[1] = "ip"
tb
urltrain <- merge(urltrain, tb, by="ip")
summary(urltrain)
head(urltrain)
names(urltrain)[14] = "ipFreq"

#count auction
head(subset(urltrain, select="auction"))
factor(urltrain$auction)
ac = table(urltrain$auction)
class(ac)
az = as.data.frame(ac)
head(az)
names(az)[1] = "auction"
az
urltrain <- merge(urltrain, az, by="auction")
summary(urltrain)
head(urltrain)
names(urltrain)[15] = "aucFreq"

#test count url and ip
head(subset(testbids, select="url"))
factor(testbids$url)
tesz = table(testbids$url)
class(tesz)
tzest = as.data.frame(tesz)
head(tzest)
names(tzest)[1] = "url"
tzest
urltest <- merge(testbids, tzest, by="url")
summary(urltest)
head(urltest)
names(urltest)[12] = "urlFreq"

head(subset(testbids, select="ip"))
factor(testbids$ip)
tesi = table(testbids$ip)
class(tesi)
tip = as.data.frame(tesi)
head(tip)
names(tip)[1] = "ip"
tip
urltest <- merge(urltest, tip, by="ip")
summary(urltest)
head(urltest)
names(urltest)[13] = "ipFreq"


#ip
#ip is multiple ip

library(plyr)
yr = count(trainbids, "ip")
class(yr)
yr

trainbids %>% group_by(ip) %>% mutate(count.2= ifelse(VAR1==2, cumsum(VAR1==2), NA))

#sorted data by frequeuncy of ip address

trainbids %>% group_by(ip)

cid <- len(ip)

table(trainbids$ip) %>%
ovfif <- sort(table(trainbids$ip)[table(trainbids$ip) > 5000])
ovfif

which(table(trainbids$ip >= 50))

freq <- ave(rep(1, times=nrow(trainbids)), trainbids$ip, FUN=sum)
ipord <- trainbids[order(freq, trainbids$ip), ] 
ipord1 <- trainbids[sort.list(freq), ] 
ipord1[1:5,]
summary(ipord)
ipord[1:5,]

ipdf <- data.frame(trainbids$ip, trainbids$outcome)
summary(ipdf)
rforestzip <- randomForest(trainbids$outcome ~ trainbids$ip, data=ipdf, ntrees=1000, mtry=5, importance=TRUE, cvfolds=5)

for(ip in trainbids$ip){
  if (count(ip) >= 2){
    trainbids[paste("dummy", level, sep="_")] <- ifelse(trainbids$ip == level, 1, 0)
}
}


str(trainbids$ip)
summary(trainbids$ip)

for(ip in unique(trainbids$ip)){
  if count(ip) >=2 & <50:
}
  

  
trainbids$highIP <- factor(with(trainbids, ifelse(())))
highIP <- trainbids$ip
hiIp <- (count(highIP > 50))
summary(hiIp)
multIP <- trainbids %>% count(ip) %>% filter(n >=2)
multbi <- data.frame(trainbids$ip, bin=cut(trainbids$ip, multIP), include.lowest=TRUE)
summary(multIP)
str(multIP)
highIP <- trainbids %>% count(ip) %>% filter(n >= 50)
summary(highIP)
str(highIP)
suphighIP <- trainbids %>% count(ip) %>% filter(n >= 1000)
suphighIP <- suphighIP$ip
suphighIP <- as.character(suphighIP)
summary(suphighIP)
str(suphighIP)
SingIP <- trainbids %>% count(ip) %>% filter(n < 2)
Sing <- SingIP$ip
summary(Sing)
str(Sing)

#auction
str(trainbids$auction)
summary(trainbids$auction)
trainbids$auction <- as.numeric(trainbids$auction)

combine.levels(trainbids$auction)

aucdf <- data.frame(trainbids$auction, trainbids$outcome)
summary(aucdf)
kauction <- kmeans(aucdf, centers=10)
summary(kauction)
kauction

#merchandise
str(trainbids$merchandise)

#feature engineering

#1  Filter bidder id's that have multiple countries
trainbids$extracountry <- 
  
#2 bidder id's in certain buckets of bidding frequency

#Implement PCA
pc <- prcomp(trainbids, scale.=TRUE)

pca = PCA(trainbids, graph=FALSE)
pca$eig
pca$var$coor


#Looking at trends by bidder id

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
summary(Boost)

#predict Boost

predBoost <- predict(Boost, urltestnew, type=c("response"), n.trees=500)
summary(predBoost)
predBoost

pboost <- data.frame(bidder_id = urltest$bidder_id, prediction=predBoost)
p_bid <- pboost %>% group_by(bidder_id)
lasnum <- summarise(p_bid, mean(prediction))
names(lasnum)[2] <- "pred"
submissionre <- join(samplesub, lasnum, by="bidder_id", type="full")
submissionre$prediction <- NULL
submissionre$pred[is.na(submissionre$pred)] <- 0
submissionre$prediction <- submissionre$pred
submissionre$pred <- NULL

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
