PREDICTING PRODUCT SALES

Goal:
In order to predict sales volume of certain potential products, it is analyzed the historical sales of an existing product mix, 
training several models, taking into account the necessary variables, and make predictions.

- Data sets:
  - Train: small dataset with 80 rows and 18 variables
  - Test: 25 rows for predicting new products sales volume
 
 Aproach:
 Regression problem developed in R.
 
 - Preprocessing highlights:
  - Features dummify
  - Outliers removal on response and some specific products
  - Feature selection: avoiding overfitting and collinearity
 
 Algorithms used: lm, SVM, KNN, Random Forest.
 
 The "Sales-pred-forloops" script has a compilation of every model performance.
 
 

