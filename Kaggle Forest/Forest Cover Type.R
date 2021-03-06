train = read.csv("train.csv")
test = read.csv("test.csv")
sample.sub = read.csv("sampleSubmission.csv")

#Remove null variables
train$Soil_Type7 <- NULL
train$Soil_Type15 <- NULL
train$ID <- NULL

test$Soil_Type7 <- NULL
test$Soil_Type15 <- NULL

#Feature Engineering

def r(x):
  if x + 180>360:
    return x-180
  else:
    return x+180
train$Aspect2 = train$Aspect$Map(r)
test$Aspect2 = test$Aspect$Map(r)

train$EVDtH <- train$Elevation - train$Vertical_Distance_To_Hydrology
test$EVDtH <- test$Elevation - test$Vertical_Distance_To_Hydrology

train$EHDtH <- train$Elevation - train$Horizontal_Distance_To_Hydrology *.2
test$EHDtH <- test$Elevation - test$Horizontal_Distance_To_Hydrology *.2


train$Highwater <- train$Vertical_Distance_To_Hydrology < 0
test$Highwater <- test$Vertical_Distance_To_Hydrology < 0

train$Distance_to_Hyrdology <- (train$Horizontal_Distance_To_Hydrology**2+
                                  train$Vertical_Distance_To_Hydrology**2)**0.5
test$Distance_to_Hydrology <- (test$Horizontal_Distance_To_Hydrology**2+
                                 test$Vertical_Distance_To_Hydrology**2)**0.5

train$Hydro_Fire_1 <- train$Horizontal_Distance_To_Hydrology+train$Horizontal_Distance_To_Fire_Points
test$Hydro_Fire_1 <- test$Horizontal_Distance_To_Hydrology+test$Horizontal_Distance_To_Fire_Points

train$Hydro_Fire_2 <- train$Horizontal_Distance_To_Hydrology - train$Horizontal_Distance_To_Fire_Points
test$Hydro_Fire_2 <- test$Horizontal_Distance_To_Hydrology - test$Horizontal_Distance_To_Fire_Points

train$Hydro_Road_1 <- abs(train$Horizontal_Distance_To_Hydrology + train$Horizontal_Distance_To_Roadways)
test$Hydro_Road_1 <- abs(test$Horizontal_Distance_To_Hydrology + test$Horizontal_Distance_To_Roadways)

train$Hydro_Road_2 <- abs(train$Horizontal_Distance_To_Hydrology - train$Horizontal_Distance_To_Roadways)
test$Hydro_Road_2 <- abs(test$Horizontal_Distance_To_Hydrology - test$Horizontal_Distance_To_Roadways)

train$Fire_Road_1 <- abs(train$Horizontal_Distance_To_Fire_Points + train$Horizontal_Distance_To_Roadways)
test$Fire_Road_1 <- abs(test$Horizontal_Distance_To_Fire_Points + test$Horizontal_Distance_To_Roadways)

train$Fire_Road_2 <- abs(train$Horizontal_Distance_To_Fire_Points - train$Horizontal_Distance_To_Roadways)
test$Fire_Road_2 <- abs(test$Horizontal_Distance_To_Fire_Points - test$Horizontal_Distance_To_Roadways)

#Factor Variables
#WRITE SOME CODE TO ITERATE THIS
train$Wilderness_Area1=as.factor(train$Wilderness_Area1)
train$Wilderness_Area2=as.factor(train$Wilderness_Area2)
train$Wilderness_Area3=as.factor(train$Wilderness_Area3)
train$Wilderness_Area4=as.factor(train$Wilderness_Area4)
train$Soil_Type1=as.factor(train$Soil_Type1)
train$Soil_Type2=as.factor(train$Soil_Type2)
train$Soil_Type3=as.factor(train$Soil_Type3)
train$Soil_Type4=as.factor(train$Soil_Type4)
train$Soil_Type5=as.factor(train$Soil_Type5)
train$Soil_Type6=as.factor(train$Soil_Type6)
train$Soil_Type8=as.factor(train$Soil_Type8)
train$Soil_Type9=as.factor(train$Soil_Type9)
train$Soil_Type10=as.factor(train$Soil_Type10)
train$Soil_Type11=as.factor(train$Soil_Type11)
train$Soil_Type12=as.factor(train$Soil_Type12)
train$Soil_Type13=as.factor(train$Soil_Type13)
train$Soil_Type14=as.factor(train$Soil_Type14)
train$Soil_Type16=as.factor(train$Soil_Type16)
train$Soil_Type17=as.factor(train$Soil_Type17)
train$Soil_Type18=as.factor(train$Soil_Type18)
train$Soil_Type19=as.factor(train$Soil_Type19)
train$Soil_Type20=as.factor(train$Soil_Type20)
train$Soil_Type21=as.factor(train$Soil_Type21)
train$Soil_Type22=as.factor(train$Soil_Type22)
train$Soil_Type23=as.factor(train$Soil_Type23)
train$Soil_Type24=as.factor(train$Soil_Type24)
train$Soil_Type25=as.factor(train$Soil_Type25)
train$Soil_Type26=as.factor(train$Soil_Type26)
train$Soil_Type27=as.factor(train$Soil_Type27)
train$Soil_Type28=as.factor(train$Soil_Type28)
train$Soil_Type29=as.factor(train$Soil_Type29)
train$Soil_Type30=as.factor(train$Soil_Type30)
train$Soil_Type31=as.factor(train$Soil_Type31)
train$Soil_Type32=as.factor(train$Soil_Type32)
train$Soil_Type33=as.factor(train$Soil_Type33)
train$Soil_Type34=as.factor(train$Soil_Type34)
train$Soil_Type35=as.factor(train$Soil_Type35)
train$Soil_Type36=as.factor(train$Soil_Type36)
train$Soil_Type37=as.factor(train$Soil_Type37)
train$Soil_Type38=as.factor(train$Soil_Type38)
train$Soil_Type39=as.factor(train$Soil_Type39)
train$Soil_Type40=as.factor(train$Soil_Type40)
train$Cover_Type=as.factor(train$Cover_Type)

#WRITE CODE TO ITERATE THIS
test$Wilderness_Area1=as.factor(test$Wilderness_Area1)
test$Wilderness_Area2=as.factor(test$Wilderness_Area2)
test$Wilderness_Area3=as.factor(test$Wilderness_Area3)
test$Wilderness_Area4=as.factor(test$Wilderness_Area4)
test$Soil_Type1=as.factor(test$Soil_Type1)
test$Soil_Type2=as.factor(test$Soil_Type2)
test$Soil_Type3=as.factor(test$Soil_Type3)
test$Soil_Type4=as.factor(test$Soil_Type4)
test$Soil_Type5=as.factor(test$Soil_Type5)
test$Soil_Type6=as.factor(test$Soil_Type6)
test$Soil_Type8=as.factor(test$Soil_Type8)
test$Soil_Type9=as.factor(test$Soil_Type9)
test$Soil_Type10=as.factor(test$Soil_Type10)
test$Soil_Type11=as.factor(test$Soil_Type11)
test$Soil_Type12=as.factor(test$Soil_Type12)
test$Soil_Type13=as.factor(test$Soil_Type13)
test$Soil_Type14=as.factor(test$Soil_Type14)
test$Soil_Type16=as.factor(test$Soil_Type16)
test$Soil_Type17=as.factor(test$Soil_Type17)
test$Soil_Type18=as.factor(test$Soil_Type18)
test$Soil_Type19=as.factor(test$Soil_Type19)
test$Soil_Type20=as.factor(test$Soil_Type20)
test$Soil_Type21=as.factor(test$Soil_Type21)
test$Soil_Type22=as.factor(test$Soil_Type22)
test$Soil_Type23=as.factor(test$Soil_Type23)
test$Soil_Type24=as.factor(test$Soil_Type24)
test$Soil_Type25=as.factor(test$Soil_Type25)
test$Soil_Type26=as.factor(test$Soil_Type26)
test$Soil_Type27=as.factor(test$Soil_Type27)
test$Soil_Type28=as.factor(test$Soil_Type28)
test$Soil_Type29=as.factor(test$Soil_Type29)
test$Soil_Type30=as.factor(test$Soil_Type30)
test$Soil_Type31=as.factor(test$Soil_Type31)
test$Soil_Type32=as.factor(test$Soil_Type32)
test$Soil_Type33=as.factor(test$Soil_Type33)
test$Soil_Type34=as.factor(test$Soil_Type34)
test$Soil_Type35=as.factor(test$Soil_Type35)
test$Soil_Type36=as.factor(test$Soil_Type36)
test$Soil_Type37=as.factor(test$Soil_Type37)
test$Soil_Type38=as.factor(test$Soil_Type38)
test$Soil_Type39=as.factor(test$Soil_Type39)
test$Soil_Type40=as.factor(test$Soil_Type40)

#WRITE CODE TO ITERATE THIS
train$Cover_Type = as.character(train$Cover_Type)
train$Cover_Type[train$Cover_Type == "1"] <- "Seg1"
train$Cover_Type[train$Cover_Type == "2"] <- "Seg2"
train$Cover_Type[train$Cover_Type == "3"] <- "Seg3"
train$Cover_Type[train$Cover_Type == "4"] <- "Seg4"
train$Cover_Type[train$Cover_Type == "5"] <- "Seg5"
train$Cover_Type[train$Cover_Type == "6"] <- "Seg6"
train$Cover_Type[train$Cover_Type == "7"] <- "Seg7"
train$Cover_Type = as.factor(train$Cover_Type)

#Grid var
Grid <- expand.grid(n.trees=100, interaction.depth=30, shrinkage=.1)

fitControl <- trainControl(method = "None", classProbs=TRUE)

x_vars <- setdiff(names(train), c("Cover_Type"))

train$Cover_Type = as.factor(train$Cover_Type)

#Boosting Model
library(gbm)
library(caret)
set.seed(123)
Boost.Forest <- gbm(Cover_Type ~ ., data=train, n.trees=10, 
                    interaction.depth=25, shrinkage=0.1, verbose=TRUE)

GBMmodel <- train(Cover_Type ~ .,
                  data = train,
                  method = "gbm",
                  trControl = fitControl,
                  verbose = TRUE,
                  tuneGrid = Grid,
                  ## Specify which metric to optimize
                  ## We will optimize AUC of ROC curve as it best encapsulates the predictive power of a model                 
                  metric = "ROC")

#Predict
Boost.Pred <- predict(Boost.Forest, newdata=test[,x_vars], n.trees=100)
Boost.Submit <- data.frame(Id = test$Id, Cover_Type = as.character(gsub("Seg", "", Boost.Pred)))

#Write csv files
write.csv(Boost.Submit, "Boost.Forest.csv", row.names=FALSE)
