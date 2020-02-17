library(readr)
library(caret)
library(ggplot2)
library(rstudioapi)
library(corrplot)
library(mlbench)
library(ggpubr)
library(Metrics)

setwd(dirname(getActiveDocumentContext()$path))
existingprod <- read_csv("../Data/existingproductattributes2017.csv")
newprod <- read_csv("../Data/newproductattributes2017_original.csv")

# Data exploration 1
summary(existingprod)
plot(existingprod$Volume)
qqnorm(existingprod$Volume)
ggplot(existingprod, 
       aes(x=Volume, 
           fill=ProductType)) + geom_histogram(color="blue",
                                               bins = 30) +  xlim(0, 3000)
ggplot(existingprod, 
       aes(x=Volume)) + geom_histogram(color="darkblue", 
                                       fill="lightblue", 
                                       bins=20 ) + facet_wrap(~ProductType) + xlim(0, 5000)
ggplot(existingprod, aes(x=ProductType, y=Volume, fill=ProductType)) + 
  geom_boxplot()+
  stat_summary(fun.y=median, colour="black", geom="text", 
               vjust=-0.7, aes(label=round(..y.., digits=0.7)))
ggplot(existingprod, aes(x="",fill=ProductType)) + geom_bar() + coord_polar(theta="y")

# Preprocessing 1 # Dummify the data
newDataFrame <- dummyVars(" ~ .", data = existingprod)
ready_exist_prod <- data.frame(predict(newDataFrame, newdata = existingprod))

DataFrame_NP <- dummyVars(" ~ .", data = newprod)
ready_new_prod <- data.frame(predict(DataFrame_NP, newdata = newprod))

# Data exploration 2
str(ready_exist_prod)
sum(is.na(ready_exist_prod))
ggdensity(exist_prod_ok$Volume)

# Preprocessing 2 # NA´s
ready_exist_prod$BestSellersRank <- NULL
names(ready_exist_prod)<-c("Accessories","Display","ExtWarranty",
                        "GameConsole","Laptop","Netbook","PC","Printer","PrintSupplies",
                        "Smartphone","Software","Tablet","ProdID","Price","x5star","x4star",
                        "x3star","x2star","x1star","PositiveServRev","NegServRev","RecomProd",
                        "ShipWeight","ProdDepth","ProdWidth","ProdHeight",
                        "ProfMargin","Volume")
names(ready_exist_prod)

ready_new_prod$BestSellersRank <- NULL
names(ready_new_prod)
names(ready_new_prod)<-c("Accessories","Display","ExtWarranty",
                           "GameConsole","Laptop","Netbook","PC","Printer","PrintSupplies",
                           "Smartphone","Software","Tablet","ProdID","Price","x5star","x4star",
                           "x3star","x2star","x1star","PositiveServRev","NegServRev","RecomProd",
                           "ShipWeight","ProdDepth","ProdWidth","ProdHeight",
                           "ProfMargin","Volume")

# Preprocessing 2 # Outliers
boxplot(ready_exist_prod$Volume)$out
outliers <- boxplot(ready_exist_prod$Volume)$out
ready_exist_prod[which(ready_exist_prod$Volume %in% outliers),]
exist_prod_ok <- ready_exist_prod[-which(ready_exist_prod$Volume %in% outliers),]
boxplot(exist_prod_ok$Volume)
names(exist_prod_ok)
sum (is.na(exist_prod_ok))
names(new_prod_ok)
boxplot(exist_prod_ok$PositiveServRev)
outliers2 <- boxplot(exist_prod_ok$PositiveServRev)$out
exist_prod_ok[which(exist_prod_ok$PositiveServRev %in% outliers2),]
exist_prod_ok2 <- exist_prod_ok[-which(exist_prod_ok$PositiveServRev %in% outliers2),]
qqplot(exist_prod_ok2$PositiveServRev, exist_prod_ok2$Volume)
densityplot(exist_prod_ok2$PositiveServRev)
boxplot(exist_prod_ok2$PositiveServRev)
histogram(exist_prod_ok2$PositiveServRev)

# Data exploration 3 # Correlation
corr2 <- cor(exist_prod_ok2)
corr2
corrplot(corr2, tl.cex = 0.70)
cor(exist_prod_ok2, exist_prod_ok2$Volume)

# Testing and Training Sets
set.seed(123)
trainSize<-round(nrow(exist_prod_ok2)*0.75)
testSize<-nrow(exist_prod_ok2)-trainSize
training_indices<-sample(seq_len(nrow(exist_prod_ok2)),size =trainSize)
trainSet<-exist_prod_ok2[training_indices,]
testSet<-exist_prod_ok2[-training_indices,]

# Cross validation
fitControl1 <- trainControl(method = "repeatedcv", number = 10, repeats = 1)
fitControl2 <- trainControl(method = "loocv")








