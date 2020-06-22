# Model lm with every feature
Model_all<-lm(Volume~ ., trainSet)
summary(Model_all) # Overfitting


# Model lm avoiding overfitting (correlation matrix show extremly hight correlation of 5 stars)
Model2<-lm(Volume~ ., trainSet[,-15], trControl=fitControl1)
summary(Model2)

# Testing
lm2_predictions <- predict(Model2,testSet)
lm2_predictions

postResample(pred = lm2_predictions,obs = testSet$Volume) # Bad performance

# Model lm, feature selection with hight correlated variables seen on correlation matrix, taking into account collinearity
Model2feat<-lm(Volume~ x4star+x2star+PositiveServRev+NegServRev, trainSet, trControl=fitControl1)
summary(Model2feat)

# Testing 
lm2feat_predictions <- predict(Model2feat,testSet)
lm2feat_predictions

postResample(pred = lm2feat_predictions,obs = testSet$Volume) # Better performance, still it could improve
testSet$lm2feat_predictions <- lm2feat_predictions
errors_a <- (testSet$lm2feat_predictions-testSet$Volume)
errors_a
ggdensity(errors_a) # Bigger errors understimating values


# Model linear regression including some products
Model3<-lm(Volume~ ., trainSet[,c(-13,-15,-17,-19)])
summary(Model3)

# Testing 
lm3_predictions <- predict(Model3,testSet)
lm3_predictions

postResample(pred = lm3_predictions,obs = testSet$Volume) # Worse metrics and negative values on predictions that would be unreal


# Predictions with model selected: the simplest & less errors (lm2feat)
Pred_lm2feat <- predict(Model2feat,ready_new_prod)
Pred_lm2feat


