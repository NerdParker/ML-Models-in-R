#install needed packages
install.packages("randomForest")

#import needed libraries
library(rpart)
library(rpart.plot)
library(caret)
library(lattice)
library(ggplot2)
library(randomForest)

#import data
wine_raw <- read.csv('C:/Users/607791/Desktop/DS/winequality-red.csv')
wine_raw$quality<-as.factor(wine_raw$quality)

#view data is in
str(wine_raw)

#set seed for reproducable results
set.seed(20)

#train model
wine_index <- createDataPartition(wine_raw$quality, p=0.8, list=FALSE) 
wine_train <- wine_raw[wine_index,]
wine_test <- wine_raw[-wine_index,]
wine_model <- rpart(quality~., data=wine_train)

#graph of tree model
prp(wine_model, type=1, extra=1)

#model summary
summary(wine_model)
#complexity plot to see where tree could be pruned
printcp(wine_model)
plotcp(wine_model)

#create a prediction
Wine_predict <- predict(wine_model, wine_test, type="class")
#create a confusion matrix
confusionMatrix(Wine_predict, wine_test$quality)

#prune for optimal cp value
model_pruned <- prune(wine_model, cp = 0.015 )
#prediction for pruned tree
wine_test$pred <- predict(model_pruned, wine_test, type = "class")
#create a confusion matrix
confusionMatrix(wine_test$pred, wine_test$quality)

#train random forest
wine_forest<- randomForest(quality~., wine_train, ntree=50)
varImpPlot(wine_forest)

#predict random forest
wine_predict_forest <- predict(wine_forest, wine_test)                   
confusionMatrix(wine_predict_forest, wine_test$quality)
