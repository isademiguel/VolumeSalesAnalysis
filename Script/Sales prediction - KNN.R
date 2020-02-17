library(caret)

# Model KNN1 after preprocessing 
system.time(ModelKNN1 <- train(Volume~., trainSet[,c(-4,-15)],
                               method = "kknn", trControl=fitControl1))
ModelKNN1
# Testing 
KNN1_predictions <- predict(ModelKNN1,testSet)
KNN1_predictions
#Metrics and errors
postResample(pred = KNN1_predictions,obs = testSet$Volume)


# Model KNN2
ModelKNN2 <- train(Volume~., trainSet[,c(-1,-2,-3,-4.-5,-6,-7,-8,-9,-10,-11,-12,-13,-15)],
                   method = "kknn", trControl=fitControl1)
summary(ModelKNN2)
# Testing KNN2
KNN2_predictions <- predict(ModelKNN2,testSet)
KNN2_predictions
#Metrics and errors
postResample(pred = KNN2_predictions,obs = testSet$Volume)

#Predictions KNN2 (best metrics and feature selection)
Finalpred <- predict(ModelKNN2,ready_new_prod)
Finalpred


