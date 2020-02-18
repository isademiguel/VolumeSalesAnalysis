# Model lm after preprocessing 2 (dummify & outlier vol)
Model_all<-lm(Volume~ ., trainSet)
summary(Model_all) # Overfitting


# Model lm after data exploration 3 (correlation matrix)
Model2<-lm(Volume~ ., trainSet[,-15], trControl=fitControl1) # Without 5 stars
summary(Model2)
# Testing
lm2_predictions <- predict(Model2,testSet)
lm2_predictions
#Metrics and errors
postResample(pred = lm2_predictions,obs = testSet$Volume) # Negative values

# Model lm, feature selection:
Model2feat<-lm(Volume~ x4star+x2star+PositiveServRev+NegServRev, trainSet, trControl=fitControl1)
# Model1 does not exist
summary(Model1)
# Testing 
lm2feat_predictions <- predict(Model2feat,testSet)
lm2feat_predictions
#Metrics and errors
postResample(pred = lm2feat_predictions,obs = testSet$Volume)
testSet$lm2feat_predictions <- lm2feat_predictions
errors_a <- (testSet$lm2feat_predictions-testSet$Volume)
errors_a
ggdensity(errors_a) # <- here we see clear  outliers, you can try to see which row, anf maybe understand whats happen


# Model linear regression including products
Model3<-lm(Volume~ ., trainSet[,c(-13,-15,-17,-19)])
summary(Model3)
# Testing 
lm3_predictions <- predict(Model3,testSet)
lm3_predictions
#Metrics and errors
postResample(pred = lm3_predictions,obs = testSet$Volume) # worse metrics and negative values


# Predictions with model selected: the simplest & less errors (lm2feat)
Pred_lm2feat <- predict(Model2feat,ready_new_prod)
Pred_lm2feat


