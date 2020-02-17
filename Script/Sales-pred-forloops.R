install.packages("pacman")
pacman:: p_load(caret, reshape, ggplot2, dplyr)

#Creating data train and test sets

set.seed(100)
in_training <- createDataPartition(trainSet$Volume, p = 0.75, list = F)

train <- trainSet[in_training,]
test <- testSet[-in_training,]

a <- c("lm", "rf","knn", "svmLinear", "svmRadial")

compare_model <- c()


#Loop for methods

for(i in a) {
  
  model <- train(Volume ~., data = train[,c(-1,-2,-3,-4,-5,-6,-7,-8,-9,-10,-11,-12,-13,-15,-17)], method = i)
  
  pred <- predict(model, newdata = test)
  
  pred_metric <- postResample(test$Volume, pred)
  
  compare_model <- cbind(compare_model , pred_metric)
  
}

colnames(compare_model) <- a

compare_model

compare_model_melt <- melt(compare_model, varnames = c("metric", "model"))
compare_model_melt <- as_data_frame(compare_model_melt)
compare_model_melt

ggplot(compare_model_melt, aes(x=model, y=value))+
  geom_col()+
  facet_grid(metric~., scales="free")


# Loop for features

a <- c("Volume ~ PositiveServRev + x4star", "Volume ~ x4star + x2star", "Volume ~ PositiveServRev")

compare_var <- c()


for ( i in a) {
  
  model <- train(formula(i), data = train, method = "rf")
  
  pred <- predict(model, newdata = test)
  
  pred_metric <- postResample(test$Volume, pred)
  
  compare_var <- cbind(compare_var , pred_metric)
  
}

colnames(compare_var) <- a

compare_var

compare_var_melt <- melt(compare_var, varnames = c("metric", "model"))
compare_var_melt <- as.data.frame(compare_var_melt)
compare_var_melt

ggplot(compare_var_melt, aes(x=model, y=value))+
  geom_col()+
  facet_grid(metric~., scales="free")


# Loop for variables and methods

a <- c("Volume ~ x4star + x2star", "Volume ~ x2star + PositiveServRev", "Volume ~ x4star")
b <- c("rf", "svmLinear")
compare_var_mod <- c()

for ( i in a) {
  for (j in b) {
    
    model <- train(formula(i), data = train, method = j)
    
    pred <- predict(model, newdata = test)
    
    pred_metric <- postResample(test$Volume, pred)
    
    compare_var_mod <- cbind(compare_var_mod , pred_metric)
    
  }
  
}



names_var <- c()
for (i in a) {
  for(j in b) {
    names_var <- append(names_var,paste(i,j))
  }
}

colnames(compare_var_mod) <- names_var

compare_var_mod

compare_var_mod_melt <- melt(compare_var_mod, varnames = c("metric", "model"))
compare_var_mod_melt <- as.data.frame(compare_var_mod_melt)
compare_var_mod_melt

ggplot(compare_var_mod_melt, aes(x=model, y=value))+
  geom_col()+
  facet_grid(metric~., scales="free")


# Final model

RF_ok<- train(Volume~ x4star, data = train,
                 method= "rf", trControl=fitControl2)

RF_ok_predictions <- predict(RF_ok,test)
RF_ok_predictions
postResample(pred = RF_ok_predictions,obs = test$Volume)
test$RF_ok_predictions <- RF_ok_predictions

# Predictions

Pred_def <- predict(RF_ok,ready_new_prod)
Pred_def
ready_new_prod$Volume_ok <- Pred_def
ready_new_prod
write.csv(ready_new_prod, file="../Data/newproductattributes2017_afterpredloop", row.names = TRUE)

newprod$Volume_ok <- Pred_def

ggplot(newprod, aes(ProductType, 
                    Volume_ok)) + geom_area(aes(colour=factor(ProductType))) + theme(text = element_text(size=10), 
                                                                                       axis.text.x = element_text(angle=90, hjust=1))



