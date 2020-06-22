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

postResample(pred = RF3_predictions,obs = testSet$Volume) # Best performance until now
testSet$RF3_predictions <- RF3_predictions
errors2 <- (testSet$RF3_predictions-testSet$Volume)
errors2
testSet$errors2 <- errors2 
ggdensity(errors2)
ggplot(testSet, aes(x=testSet$PositiveServRev, 
                    y=testSet$x4star)) +geom_point(aes(col=testSet$errors2))
ggplot(testSet, aes(x=testSet$NegServRev, 
                    y=testSet$x1star)) +geom_point(aes(col=testSet$RF3_predictions))
# Correlation between these features


# Model RF4
ModelRF4<- train(Volume~ ., trainSet[,c(-1,-2,-3,-4,-5,-6,-7,-8,-9,-10,-11,-12,-13,-15,-17,-19,-20,-21,-29,-30,-31)],
                 method= "rf", trControl=fitControl2)
summary(ModelRF4)

# Testing RF
RF4_predictions <- predict(ModelRF4,testSet)
RF4_predictions

postResample(pred = RF4_predictions,obs = testSet$Volume)
testSet$RF4_predictions <- RF4_predictions
errors3 <- (testSet$RF4_predictions-testSet$Volume)
errors3
ggdensity(errors3) # Lower Rsquare and bigger errors

# Model RF5
ModelRF5<- train(Volume~ PositiveServRev+x4star+x2star, data = trainSet,
                 method= "rf", trControl=fitControl2)
summary(ModelRF5)

# Testing
RF5_predictions <- predict(ModelRF5,testSet)
RF5_predictions

postResample(pred = RF5_predictions,obs = testSet$Volume) # Best performance (0.92 Rsquare)


testSet$RF5_predictions <- RF5_predictions
errors4 <- (testSet$RF5_predictions-testSet$Volume)
errors4
ggdensity(errors4)

plot(RF5_predictions)

# Predictions
Newpred <-predict(ModelRF5,ready_new_prod)
Newpred
newprod$Volumen_PT2 <- Newpred
ggplot(newprod, aes(ProductType, 
                    Volumen_PT2)) + geom_area(aes(colour=factor(ProductType))) + theme(text = element_text(size=10), 
                                                                                       axis.text.x = element_text(angle=90, hjust=1))


#Predictions
Finalpred <- predict(ModelRF5,ready_new_prod)
Finalpred
ready_new_prod$Volumepred <- Finalpred
ready_new_prod
write.csv(ready_new_prod, file="../Data/newproductattributes2017_afterpred.csv", row.names = TRUE)

names(newprod)
newprod$Volumen_PT <- Finalpred
ggplot(newprod, aes(ProductType, 
                    Volumen_PT)) + geom_point(aes(colour=factor(ProductType))) + theme(text = element_text(size=10), 
                                                                                      axis.text.x = element_text(angle=90, hjust=1))

