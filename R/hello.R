# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'
########################################
#parameter:
#data(dataframe) is dataframe of raw data
#num(numeical) is smote para m determain the number of positive data we need
#k(numeical) is number of nearest neighbors
#r(numeical) is Minkowski distance's para
#
#detail:
#function smote is the method to process imbalance data to create new balance data
#
#return:
#corpus(dataframe) is processed data
#########################################
find_nearnest_neighbor <- function(data_train,data_test,k = 1,distance = 2)
{
  print("find neighbor")
  if(k > dim(data_train)[1])
  {
    stop("k is bigger than the number of data_train")
  }
  id = 1: dim(data_train)[1]
  C = matrix(NA,dim(data_test)[1],k)
  for(i in 1:dim(data_test)[1])
  {
    total = rbind(data_test[i,],data_train)
    dis = dist(total, method = "minkowski", p = distance)[1:dim(data_train)[1]]
    #distan = data.frame(distance = dis)
    #da = cbind(id,distan)
    k_id = id[order(dis)]
    k_id = k_id[1:k]
    C[i,] = k_id
  }
  print("find neighbor over")
  return( C)
}


Sprocess <- function(data,num,k = 5,r = 1)
{
  print("smote is start")
  corpus <- vector("numeric",0)
  if(dim(data)[1] < k)
  {
    k = dim(data)[1] - 1
  }
  data1 = cbind(data,data.frame(label = 1))
  Ma = find_nearnest_neighbor(data,data,k = k,distance = r)
  corpus = data
  num = num - dim(data)[1]
  for(i in 1:num)
  {
    mid = sample(1:dim(data)[1],1)
    nn = Ma[mid,sample(2:k,1)]
    Mat = data[mid,]
    for(j in 1:dim(data)[2])
    {
      gap = runif(1,min=0,max=1)
      Mat[,j] = data[mid,j] + (data[nn,j] - data[mid,j])*gap
    }
    corpus = rbind(Mat,corpus)
  }
  print("smote is over")
  return(corpus)
}

Sbalance <- function(data_train,train_label,m=0, r=0,neighbors = 0)
{
  pos_train = data_train[train_label == 1,]
  neg_train = data_train[train_label == 0,]
  if(dim(pos_train)[1] < dim(neg_train)[1])
  {
    pos_train = Sprocess(pos_train,m,k=neighbors,r=r)
    if(dim(neg_train)[1] < m)
    {
      m = dim(neg_train)[1]
    }
    neg_train = neg_train[sample(1:dim(neg_train)[1],m,replace = FALSE),]
  }
  data_train1 = rbind(pos_train,neg_train)
  label_train = c(rep(1,dim(pos_train)[1]),rep(0,dim(neg_train)[1]))
  return(list(data_train=data_train1,train_label=label_train))
}

smote <- function(X,Y,minority,m = 0.5,r = 2,neighbors = 5)
{
  num = m * dim(X)[1]
  lev = levels(Y)
  mi = as.character(minority)
  ma = lev[!(lev == mi)]
  Y = sapply(Y, function(x){if( x == minority)x = 1 else{x = 0}})
  result = Sbalance(X,Y,num,r,neighbors)
  X = result$train_label
  Y = as.factor(sapply(X, function(x){if( x == 1)x = mi else{x = ma}}))
  result$train_label = Y
  result
}

