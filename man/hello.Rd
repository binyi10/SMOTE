\name{smote}
\alias{smote}
\title{smote}
\usage{
smote(X,Y,minority,m = 50,r = 2,neighbors = 5)
}
\description{
preprocess the imbalance data by using SMOTE.
}
\examples{
X = data.frame(x1 = rep(1,5),x2 = rep(0,5),x3 = runif(5))#train data
Y = as.factor(c(1,0,0,0,1))
minority = Y[1]
data = smote(X,Y,minority,m = 0.5,r = 2,neighbors = 1)
data$data_train
data$train_label
}
