library(corrplot)
library(cluster) 
library(factoextra)
require(fastcluster)
require(graphics)
library(dendextend)
library(dplyr)
library(ggplot2)
library(caret)
library(rpart)
library(rpart.plot)
library(lattice)
library(randomForest)
library(mlbench)

#import the data
abalone_raw <- read.table("C:/Users/607791/Desktop/DS/abalone.csv", header = TRUE, sep = ",")
head(abalone_raw)
#pair plot of dataset
ggpairs(abalone_raw, aes(colour = ï..Sex, alpha = 0.8), title="Abalone Pairs") + 
  theme_grey(base_size = 8)

df <- abalone_raw[-c(1,1)]
head(df,3)

#silhouette method
silhouette_score <- function(k){
  km <- kmeans(df, centers = k, nstart=25)
  ss <- silhouette(km$cluster, dist(df))
  mean(ss[, 3])
}
k <- 2:10
avg_sil <- sapply(k, silhouette_score)
#plot of silhouette scores and number of clusters
plot(k, type='b', avg_sil, xlab='Number of clusters', ylab='Average Silhouette Scores', frame=FALSE)
#factoextra package to better visualize the optimal k clusters
#silhouette average width clusters
fviz_nbclust(df, kmeans, method='silhouette')

#factoextra visual of cluster 2
km_2 <- kmeans(df, 2)
fviz_cluster(km_2, data=df)
points(km_2$centers[,c("Shell.weight", "Rings")], col=7:8, pch=8, cex=2)
#cluster including explaination percent for cluster 2
data$cluster2 <- km_2$cluster
clusplot(data, data$cluster2, color=TRUE, shade = TRUE, label=2)

km_2
#train model
train_rings <- createDataPartition(y=abalone_raw$Rings, p=0.8, list=FALSE)
training_rings <- abalone_raw[train_rings,]
testing_rings <- abalone_raw[-train_rings,]
model_rings <- train(Rings~., data=training_rings, method="lm")
predict(model_rings, data = testing_rings)
sqrt(sum((model_rings$fitted - training_rings$Rings)^2))
#accuracy percent
sqrt(sum((predict(model_rings, newdata = testing_rings) - testing_rings$Rings)^2))




#Start of Decision tree method
abalone_raw$Rings<-as.factor(abalone_raw$Rings)
set.seed(20)

#train model
abalone_index <- createDataPartition(abalone_raw$Rings, p=0.8, list=FALSE) 
abalone_train <- abalone_raw[abalone_index,]
abalone_test <- abalone_raw[-abalone_index,]
abalone_model <- rpart(Rings~., data=abalone_train)

#graph of tree model
prp(abalone_model, type=1, extra=1)

#model summary
summary(abalone_model)
#complexity plot to see where tree could be pruned
printcp(abalone_model)
plotcp(abalone_model)

#create a prediction
abalone_predict <- predict(abalone_model, abalone_test, type="class")
#create a confusion matrix
confusionMatrix(abalone_predict, abalone_test$Rings)

table(abalone_predict,abalone_test$Rings)
#miscalculation percent
mean(abalone_predict != abalone_test$Rings)

#prune for optimal cp value
model_pruned <- prune(abalone_model, cp = 0.015 )
#prediction for pruned tree
abalone_test$pred <- predict(model_pruned, abalone_test, type = "class")
#create a confusion matrix
confusionMatrix(abalone_test$pred, abalone_test$Rings)

#train random forest
abalone_forest<- randomForest(Rings~., abalone_train, ntree=50)
varImpPlot(abalone_forest)

#predict random forest
abalone_predict_forest <- predict(abalone_forest, abalone_test)                   
confusionMatrix(abalone_predict_forest, abalone_test$Rings)


#other performance metrics
#RMSE & R^2
control <- trainControl(method="cv", number=5)
set.seed(7)
fit <- train(Rings~., data=training_rings, method="lm", metric="RMSE", trControl=control)
fit

#10-fold cross validation for Decision tree model
folds <- createFolds(factor(training_rings$Rings), k = 10, list = FALSE)
folds
abalone_train <- abalone_raw[folds,]
abalone_test <- abalone_raw[-folds,]
abalone_model <- rpart(Rings~., data=abalone_train)
summary(abalone_model)
#complexity plot to see where tree could be pruned
printcp(abalone_model)
plotcp(abalone_model)

#create a prediction
abalone_predict <- predict(abalone_model, abalone_test, type="class")
#create a confusion matrix
confusionMatrix(abalone_predict, abalone_test$Rings)