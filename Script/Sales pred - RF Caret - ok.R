
library(caret)
library(plotly)
set.seed(123)

# Cross validation
fitControl2 <- trainControl(method = "LOOCV")

# Model RF3
ModelRF3<- train(Volume~ ., trainSet[,c(-15)], method= "rf", trControl=fitControl2)
summary(ModelRF3)
varImp(ModelRF3)
# Testing 
RF3_predictions <- predict(ModelRF3,testSet)
RF3_predictions
#Metrics and errors
postResample(pred = RF3_predictions,obs = testSet$Volume)
testSet$RF3_predictions <- RF3_predictions
errors2 <- (testSet$RF3_predictions-testSet$Volume)
errors2
testSet$errors2 <- errors2 
ggdensity(errors2)
ggplot(testSet, aes(x=testSet$PositiveServRev, 
                    y=testSet$x4star)) +geom_point(aes(col=testSet$errors2))
ggplot(testSet, aes(x=testSet$PositiveServRev, 
                    y=testSet$x4star)) +geom_point(aes(col=testSet$RF3_predictions))


# Model RF4
ModelRF4<- train(Volume~ ., trainSet[,c(-1,-2,-3,-4,-5,-6,-7,-8,-9,-10,-11,-12,-13,-15,-17)],
                 method= "rf", trControl=fitControl2)
names(trainSet)
summary(ModelRF4)
# Testing RF
RF4_predictions <- predict(ModelRF4,testSet)
RF4_predictions
#Metrics and errors
postResample(pred = RF4_predictions,obs = testSet$Volume)
testSet$RF4_predictions <- RF4_predictions
errors3 <- (testSet$RF4_predictions-testSet$Volume)
errors3
ggdensity(errors3)

# Model RF5
ModelRF5<- train(Volume~ PositiveServRev+x4star+x2star, data = trainSet,
                 method= "rf", trControl=fitControl2)
summary(ModelRF5)
varImp(ModelRF5)
# Testing
RF5_predictions <- predict(ModelRF5,testSet)
RF5_predictions
#Metrics and errors
postResample(pred = RF5_predictions,obs = testSet$Volume)
testSet$RF5_predictions <- RF5_predictions
errors4 <- (testSet$RF5_predictions-testSet$Volume)
errors4
ggdensity(errors4)

# Model RF6
ModelRF6<- train(Volume~ PositiveServRev+x4star, data = trainSet,
                 method= "rf", trControl=fitControl2)
varImp(ModelRF6)
# Testing
RF6_predictions <- predict(ModelRF6,testSet)
RF6_predictions
#Metrics and errors
postResample(pred = RF6_predictions,obs = testSet$Volume)
testSet$RF6_predictions <- RF6_predictions

#Predictions
Finalpred <- predict(ModelRF4,ready_new_prod)
Finalpred
ready_new_prod$Volumepred <- Finalpred
ready_new_prod
write.csv(ready_new_prod, file="../Data/newproductattributes2017_afterpred.csv", row.names = TRUE)

names(newprod)
newprod$Volumen_PT <- Finalpred
ggplot(newprod, aes(ProductType, 
                    Volumen_PT)) + geom_point(aes(colour=factor(ProductType))) + theme(text = element_text(size=10), 
                                                                                      axis.text.x = element_text(angle=90, hjust=1))

