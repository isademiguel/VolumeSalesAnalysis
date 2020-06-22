library(caret)

# Model KNN1 with mimimum feature removal
system.time(ModelKNN1 <- train(Volume~., trainSet[,c(-4,-15)],
                               method = "kknn", trControl=fitControl1))
ModelKNN1

varImp(ModelKNN1)

# Testing 
KNN1_predictions <- predict(ModelKNN1,testSet)
KNN1_predictions

postResample(pred = KNN1_predictions,obs = testSet$Volume) 


# Model KNN2 only most important features
ModelKNN2 <- train(Volume~., trainSet[,c(-1,-2,-3,-5,-6,-7,-8,-9,-10,-11,-12,-13,-15,-17,-19,-20,-21)],
                   method = "kknn", trControl=fitControl1)
summary(ModelKNN2)

# Testing KNN2
KNN2_predictions <- predict(ModelKNN2,testSet)
KNN2_predictions

postResample(pred = KNN2_predictions,obs = testSet$Volume) # Bad performance

#Predictions KNN2 (best metrics and feature selection)
Finalpred <- predict(ModelKNN2,ready_new_prod)
Finalpred


