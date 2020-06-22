library(e1071)
library(caret)

# Model SVM 1
SVM1<-svm(Volume~ ., trainSet[,c(-13,-15)]) # Without product ID (no meaning) and 5 stars (overfitting)
summary(SVM1)

# Testing 
SVM1_predictions <- predict(SVM1,testSet)
SVM1_predictions

postResample(pred = SVM1_predictions,obs = testSet$Volume) # Bad metrics

# Model SVM 2 
SVM2<-svm(Volume~ ., trainSet[,c(-13,-15,-17)]) # Same feature selection removing also 3 stars
summary(SVM2)

# Testing
SVM2_predictions <- predict(SVM2,testSet)
SVM2_predictions

postResample(pred = SVM2_predictions,obs = testSet$Volume) # Same value for each prediction and bad metrics


#Model SVM 3  changing library to caret and also excluding 2 stars
SVM3 <- train(Volume~., data=trainSet[,c(-13,-15,-17,-19)], method = "svmLinear", trControl = fitControl1)  
summary(SVM3)

# Testing
SVM3_predictions <- predict(SVM3,testSet)
SVM3_predictions

postResample(pred = SVM3_predictions,obs = testSet$Volume) # Better metircs but negative values and worse than lm (0.86 Rsquared)

#Model SVM 4 only with selected variables
SVM4 <- train(Volume~ x4star+x2star+PositiveServRev+NegServRev, trainSet, 
              method = "svmLinear", trControl = fitControl1)  
summary(SVM4)

# Testing
SVM4_predictions <- predict(SVM4,testSet)
SVM4_predictions

postResample(pred = SVM4_predictions,obs = testSet$Volume) # Best SVM model but still worse than lm

#Predictions SVM4 (best library and metrics)
Finalpred <- predict(SVM4,ready_new_prod)
Finalpred


