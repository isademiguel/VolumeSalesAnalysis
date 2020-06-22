library(readr)
library(caret)
library(ggplot2)
library(rstudioapi)
library(corrplot)
library(mlbench)
library(ggpubr)
library(Metrics)
library(dplyr)
library(DescTools)

setwd(dirname(getActiveDocumentContext()$path))
existingprod <- read_csv("../Data/existingproductattributes2017.csv")
newprod <- read_csv("../Data/newproductattributes2017_original.csv")

#### DATA EXPLORATION ####

summary(existingprod)
plot(existingprod$Volume) # It is necessary to deal with outliers
qqnorm(existingprod$Volume)

ggplot(existingprod,
       aes(x=Volume, fill=ProductType)) + geom_histogram(color="blue",
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

#### PREPROCESSING ####

# Dummify the data to analyze product type with every category by separated as numerical variable
newDataFrame <- dummyVars(" ~ .", data = existingprod)
ready_exist_prod <- data.frame(predict(newDataFrame, newdata = existingprod))
str(ready_exist_prod)

DataFrame_NP <- dummyVars(" ~ .", data = newprod)
ready_new_prod <- data.frame(predict(DataFrame_NP, newdata = newprod))

# Check duplicates
duplicates <- ready_exist_prod %>% 
  duplicated() %>% 
  table()

duplicates

# Remove NA
is.na(ready_exist_prod)
sum(is.na(ready_exist_prod)) # All NA are in Best Seller Rank, as we have a small data set, we'll remove that column instead of every NA row
ready_exist_prod$BestSellersRank <- NULL

# Change names
names(ready_exist_prod)
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

# Outliers
boxplot(ready_exist_prod$Volume)$out
outliers <- boxplot(ready_exist_prod$Volume)$out
ready_exist_prod[which(ready_exist_prod$Volume %in% outliers),]
exist_prod_ok <- ready_exist_prod[-which(ready_exist_prod$Volume %in% outliers),] # Remove outliers in response
boxplot(exist_prod_ok$Volume)

#boxplot(exist_prod_ok$PositiveServRev) # Checking outliers of other features
#outliers2 <- boxplot(exist_prod_ok$PositiveServRev)$out
#exist_prod_ok[which(exist_prod_ok$PositiveServRev %in% outliers2),]
#exist_prod_ok2 <- exist_prod_ok[-which(exist_prod_ok$PositiveServRev %in% outliers2),]
#qqplot(exist_prod_ok2$PositiveServRev, exist_prod_ok2$Volume)
#densityplot(exist_prod_ok2$PositiveServRev)
#boxplot(exist_prod_ok2$PositiveServRev)
#histogram(exist_prod_ok2$PositiveServRev)


#### CORRELATION ####

corr <- cor(exist_prod_ok)
corr
corrplot(corr, tl.cex = 0.70)
cor(exist_prod_ok, exist_prod_ok$Volume)

#corr2 <- cor(exist_prod_ok2)
#corr2
#corrplot(corr2, tl.cex = 0.70)
#cor(exist_prod_ok2, exist_prod_ok2$Volume)

set.seed(123)

# Testing and Training Sets

trainSize<-round(nrow(exist_prod_ok)*0.80)
testSize<-nrow(exist_prod_ok)-trainSize
training_indices<-sample(seq_len(nrow(exist_prod_ok)),size =trainSize)
trainSet<-exist_prod_ok[training_indices,]
testSet<-exist_prod_ok[-training_indices,]

# Checking distribution training and testing sets
histogram(trainSet$Volume)
histogram(testSet$Volume) # Sets don't have exactly the same distribution
hist(trainSet$Volume, col=rgb(0,0,1,1/4), breaks=200)
hist(testSet$Volume,  col=rgb(1,0,0,1/4), add=T, breaks=200)
legend("topright", c("Train", "Test"), fill=c("blue", "red"))
box()

table <- prop.table(table(trainSet$Volume))
table
table2 <- prop.table(table(testSet$Volume))
table2

# Cross validation
fitControl1 <- trainControl(method = "repeatedcv", number = 10, repeats = 1)
fitControl2 <- trainControl(method = "loocv")








