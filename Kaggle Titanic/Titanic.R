train = read.csv("train.csv")
test = read.csv("test.csv")

#Exploratory
summary(train)
str(train)
edit(train)

  #Preprocess
train$Sex = as.factor(train$Sex)
test$Sex = as.factor(test$Sex)

train$Pclass = as.factor(train$Pclass)
test$Pclass = as.factor(test$Pclass)

#Create Family parameter
train$family = as.factor(train$SibSp + train$Parch >= 1)
test$family = as.factor(test$SibSp + test$Parch >= 1)

which(train$Embarked == "")
train$Embarked[c(62, 830)] = "S"
train$Embarked = factor(train$Embarked)
test$Embarked = factor(test$Embarked)

train$child = as.factor(train$Age <= 18)
test$child = as.factor(test$Age <= 18)


train$Name <- as.character(train$Name)
train$Title <- sapply(train$Name, FUN=function(x) {strsplit(x, split="[,.]")[[1]][2]})
train$Title <- sub(" ", "", train$Title)
train$Title[train$Title %in% c('Mme', 'Mlle')] <- 'Mlle'
train$Title[train$Title %in% c('Capt', 'Don', 'Major', 'Sir')] <- 'Sir'
train$Title[train$Title %in% c('Dona', 'Lady', 'the Countess', 'Jonkheer')] <- 'Lady'
train$Title <- as.factor(train$Title)

test$Name <- as.character(test$Name)
test$Title <- sapply(test$Name, FUN=function(x) {strsplit(x, split="[,.]")[[1]][2]})
test$Title <- sub(" ", "", test$Title)
test$Title[test$Title %in% c("Mme", "Mlle")] <- "Mlle"
test$Title[test$Title %in% c("Capt", "Don", "Major", "Sir")] <- "Sir"
test$Title[test$Title %in% c("Dona", "Lady", "the Countess", "Jonkheer")] <- "Lady"
test$Title <- as.factor(test$Title)

test$Title <- factor(test$Title, levels=levels(train$Title))

train$Familysize = train$SibSp + train$Parch + 1
test$Familysize = test$SibSp + test$Parch + 1

train$Surname <- sapply(train$Name, FUN=function(x) {strsplit(x, split="[,.]")[[1]][1]})
train$FamilyID <- paste(as.character(train$Familysize), train$Surname, sep="")
train$FamilyID[train$Familysize <= 2] <- "Small"
famIDs <- data.frame(table(train$FamilyID))
famIDs <- famIDs[famIDs$Freq <= 2,]
train$FamilyID[train$FamilyID %in% famIDs$Var1] <- "Small"
train$FamilyID <- factor(train$FamilyID)

test$Surname <- sapply(test$Name, FUN=function(x) {strsplit(x, split="[,.]")[[1]][1]})
test$FamilyID <- paste(as.character(test$Familysize), test$Surname, sep="")
test$FamilyID[test$Familysize <= 2] <- "Small"
famIDs.test <- data.frame(table(test$FamilyID))
famIDs.test <- famIDs.test[famIDs.test$Freq <= 2,]
test$FamilyID[test$FamilyID %in% famIDs.test$Var1] <- "Small"
test$FamilyID <- factor(test$FamilyID)

test$FamilyID <- factor(test$FamilyID, levels=levels(train$FamilyID))

#Fare Parameter
summary(train$Fare)
train$rich <- as.factor(train$Fare > 70)
str(train$rich)
test$rich <- as.factor(test$Fare > 70)

#Missing Data
library(plyr)

Agefit <- randomForest(Age ~ Pclass+Sex+SibSp+Parch+Fare+Embarked, data=train[!is.na(train$Age),], ntree=5000)
train$Age[is.na(train$Age)] <- predict(Agefit, train[is.na(train$Age),], ntree=5000)
str(train$Age)

test$Fare[is.na(test$Fare)] <- 0
Agefit.test <- randomForest(Age ~ Pclass+Sex+SibSp+Parch+Fare+Embarked, data=test[!is.na(test$Age),], ntree=5000)
test$Age[is.na(test$Age)] <- predict(Agefit.test, test[is.na(test$Age),], ntree=5000)

#Log Reg
library(MASS)
LogReg = glm(Survived~Sex+family+Embarked+Pclass+Age+rich+child, data=train)
LogReg
anova(LogReg)
summary(LogReg)
Log.pred = predict(LogReg, newdata=test)
Log.pred <- round(Log.pred)

Log.submit = data.frame(PassengerID = test$PassengerId, Survived=Log.pred)

#Random Forest
library(randomForest)
set.seed(111)
RForest = randomForest(as.factor(Survived)~Sex+SibSp+Parch+family+Pclass+Age+child+Familysize+Title,data=train,ntree=5000,mtry=3,importance=TRUE)
RForest
print(RForest)
importance(RForest)
pred.rforest = predict(RForest, newdata=test, ntree=5000)

rforest.submit = data.frame(PassengerID = test$PassengerId, Survived=pred.rforest)

library(party)
set.seed(123)
tit.cforest <- cforest(as.factor(Survived)~Sex+SibSp+Parch+Fare+Embarked+Pclass+Age+child+Title+Familysize+FamilyID,data=train,controls=cforest_unbiased(ntree=5000, mtry=3))
tit.cforest
cforest.pred <- predict(tit.cforest, test, OOB=TRUE, type="response")


cforest.submit= data.frame(PassengerID = test$PassengerId, Survived=cforest.pred)

#Boosting
library(gbm)
Boost = gbm(as.factor(Survived) ~ Sex+family+SibSp+Parch+Embarked+Pclass+Age+rich+child+Familysize+Title,data=train,n.trees=5000,shrinkage=0.01,interaction.depth=5,distribution="gaussian")
Boost
summary(Boost)
pred.boost = predict(Boost, newdata=test, n.trees=5000)

boost.submit = data.frame(PassengerID = test$PassengerId, Survived=pred.boost)

#Decision Tree
library(tree)
library(rpart)
Dectree = rpart(Survived ~ Sex+family+Embarked+Pclass+Age+rich+child,data=train, method="class")
plot(Dectree)
text(Dectree)
pred.tree = predict(Dectree, test, type="class")

tree.submit <- data.frame(PassengerID = test$PassengerId, Survived=pred.tree)


#NEED CROSSVALIDATION AND MODEL EVALUATION
trainsub <- combi[1:891,]


#Ensemble Learning

#Write files
write.csv(boost.submit, "Pred.Boost.csv", row.names=FALSE)
write.csv(Log.submit, "Log.pred.csv", row.names=FALSE)
write.csv(rforest.submit, "Rforest.csv", row.names=FALSE)
write.csv(tree.submit, "Tree.Pred.csv", row.names=FALSE)
write.csv(cforest.submit, "CForest Pred.csv", row.names=FALSE)
