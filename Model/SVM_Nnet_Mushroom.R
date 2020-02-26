library(nnet)
library(ggplot2)
library(e1071)
library(rattle)
library(stringr)
library(caret)


#load the cleaned data into R
mushroom <- read.table("C:/Users/607791/Desktop/DS/agaricus_lepiota.csv", header = TRUE, sep = ",")
head(mushroom)

#seed for recreating same results
set.seed(10)
mushroom[,'train'] <- ifelse(runif(nrow(mushroom))<0.8,1,0)
#create training and test sets
trainset <- mushroom[mushroom$train==1,]
testset <- mushroom[mushroom$train==0,]
#train column index
trainColNum <- grep('train',names(trainset))
#remove train flag column from train and test sets
trainset <- trainset[,-trainColNum]
testset <- testset[,-trainColNum]

#column index of predicted variable
typeColNum <- grep('Classes',names(mushroom))

#plot of mushroom edibility 
ggplot(mushroom, aes(x = cap.surface, y = cap.color, col = classes)) + 
  geom_jitter(alpha = 0.5) + 
  scale_color_manual(breaks = c("e", "p"), 
                     values = c("blue", "red"))

# create svm model, not linear data so radial is used
svm_model <- svm(classes~., data=trainset, type='C-classification', kernel='radial')

#svm predict trainset
pred_train <-predict(svm_model,trainset)
#svm % correctly predicted by trainset
mean(pred_train==trainset$classes)
#svm predict testset
pred_test <-predict(svm_model,testset)
#svm % correctly predicted by testset
mean(pred_test==testset$classes)
#confusion matrix of predictions
table(pred_test, testset$classes)  

# create svm model, not linear data so radial is used
svm_model2 <- svm(classes~., data=trainset, type='C-classification', kernel='radial',cost = 100, scale = FALSE)

#svm predict trainset
pred_train2 <-predict(svm_model2,trainset)
#svm % correctly predicted by trainset
mean(pred_train2==trainset$classes)
#svm predict testset
pred_test2 <-predict(svm_model2,testset)
#svm % correctly predicted by testset
mean(pred_test2==testset$classes)
#confusion matrix of predictions
table(pred_test2, testset$classes)  

# create svm model, not linear data so radial is used
svm_model3 <- svm(classes~., data=trainset, type='C-classification', kernel='linear',cost = 100, scale = FALSE)

#svm predict trainset
pred_train3 <-predict(svm_model3,trainset)
#svm % correctly predicted by trainset
mean(pred_train3==trainset$classes)
#svm predict testset
pred_test3 <-predict(svm_model3,testset)
#svm % correctly predicted by testset
mean(pred_test3==testset$classes)
#confusion matrix of predictions
table(pred_test3, testset$classes) 

# create svm model, testing linear on on non-linear data
svm_model3 <- svm(classes~., data=trainset, type='C-classification', kernel='linear',cost = 100, scale = FALSE)

#svm predict trainset
pred_train3 <-predict(svm_model3,trainset)
#svm % correctly predicted by trainset
mean(pred_train3==trainset$classes)
#svm predict testset
pred_test3 <-predict(svm_model3,testset)
#svm % correctly predicted by testset
mean(pred_test3==testset$classes)
#confusion matrix of predictions
table(pred_test3, testset$classes) 

# create svm model, testing polynomial kernel
svm_model4 <- svm(classes~., data=trainset, type='C-classification', kernel='polynomial',cost = 100, scale = FALSE)

#svm predict trainset
pred_train4 <-predict(svm_model4,trainset)
#svm % correctly predicted by trainset
mean(pred_train4==trainset$classes)
#svm predict testset
pred_test4 <-predict(svm_model4,testset)
#svm % correctly predicted by testset
mean(pred_test4==testset$classes)
#confusion matrix of predictions
table(pred_test4, testset$classes) 

#dummy variables
mushroom_part <- createDataPartition(mushroom$classes, p = .7, list = FALSE)
dummy <- subset(mushroom, select = -classes)
mushroom_dum <- dummyVars(~., data = dummy, sep = ".")
mushroom_dum <- data.frame(predict(mushroom_dum, dummy))
ncol(mushroom_dum)

#train and test sets
mushroom_dum$classes <- mushroom$classes
ncol(mushroom_dum)
train <- mushroom_dum[mushroom_part,]
test <- mushroom_dum[-mushroom_part,]
testLabels <- subset(test, select = classes)
testset <- subset(test, select = -classes)

#neural net model
neural_net <- nnet(classes ~ ., data = train, size = 2, rang = 0.1, maxit = 200)
summary(neural_net)

#run prediction
mushroom_pred <- predict(neural_net, testset, type = "class")

#results table
net_table <- table(test$classes, mushroom_pred)
net_table

#results confusion matrix showing accuracy
confusionMatrix(net_table)

#neaural net with only 1 hidden layer
neural_net2 <- nnet(classes ~ ., data = train, size = 1, rang = 0.1, maxit = 200)
summary(neural_net2)

#run prediction
mushroom_pred2 <- predict(neural_net2, testset, type = "class")

#results table
net_table2 <- table(test$classes, mushroom_pred2)
net_table2

#results confusion matrix showing accuracy
confusionMatrix(net_table2)

#neaural net with 4 hidden layers
neural_net3 <- nnet(classes ~ ., data = train, size = 4, rang = 0.1, maxit = 200)
summary(neural_net3)

#run prediction
mushroom_pred3 <- predict(neural_net3, testset, type = "class")

#results table
net_table3 <- table(test$classes, mushroom_pred3)
net_table3

#results confusion matrix showing accuracy
confusionMatrix(net_table3)