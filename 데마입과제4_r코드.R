install.packages("dummy")
library(dummy)

bike = read.csv("C:/Users/LG/Downloads/bike_final.csv")
head(bike)
bike$X <- NULL
head(bike,10)

bike$Seasons <- factor(bike$Seasons)
bike$Holiday <- factor(bike$Holiday)
bike$weekday <- factor(bike$weekday)
bike$month <- factor(bike$month)
bike$Hour <- factor(bike$Hour)


# Computing the test error by paritioning
## Data partitioning 
set.seed(1234)
train.index = sample(1:nrow(bike), round(0.7*nrow(bike)))
bike.train = bike[ train.index,] #train data                              
bike.test  = bike[-train.index,] #test data

#1. SVM
## Calling packages
install.packages("e1071")
library(e1071) 


## Model fitting
fit = svm(Rented.Bike.Count ~ ., data = bike.train) 
summary(fit) # print the fitted model


## Predicting and Evaluating
yhat = predict(fit, newdata=bike.train, type="response") # predictions
mean((bike.train$Rented.Bike.Count - yhat)^2)  # train MSE
mean(abs(bike.train$Rented.Bike.Count - yhat)) # train MAE

plot(bike.train$Rented.Bike.Count, yhat, xlab="Observed Values", ylab="Fitted Values", main = 'Train')
abline(a=0, b=1, col='red')


yhat = predict(fit, newdata=bike.test, type="response") # predictions
mean((bike.test$Rented.Bike.Count - yhat)^2)  # test MSE
mean(abs(bike.test$Rented.Bike.Count - yhat)) # test MAE

plot(bike.test$Rented.Bike.Count, yhat, xlab="Observed Values", ylab="Fitted Values", main = 'Test')
abline(a=0, b=1, col='red')



##########################
# Computing the CV error

V = 10 #V-fold CV
mse.test = 0
mae.test = 0

set.seed(1234)
id = sample(1:V, nrow(bike), replace = T)

for(i in 1:V) {
  
  print(i)
  
  ## Data partitioning
  
  test.index = which(id==i)
  bike.train = bike[-test.index,] #train data                              
  bike.test  = bike[ test.index,] #test data
  
  ## Fitting
  
  fit = svm(Rented.Bike.Count ~ ., data = bike.train)
  
  ## Predicting and Evaluating
  
  yhat = predict(fit, newdata=bike.test, type="response")
  mse.test = mse.test + mean((bike.test$Rented.Bike.Count - yhat)^2)  # MSE
  mae.test = mae.test + mean(abs(bike.test$Rented.Bike.Count - yhat)) # MAE
  
}

cv.mse.test  = mse.test/V;  cv.mse.test  # test CV MSE
cv.mae.test  = mae.test/V;  cv.mae.test  # test CV MAE 




#2. KNN

## Calling packages for knnreg
install.packages("caret")
library(caret) 

#data
bike = read.csv("C:/Users/LG/Downloads/bike_final.csv")
bike$X <- NULL

# Computing the test error by paritioning
## Data partitioning 
set.seed(1234)
train.index = sample(1:nrow(bike), round(0.7*nrow(bike)))
bike.train = bike[ train.index,] #train data                              
bike.test  = bike[-train.index,] #test data

dvar = c(1,10,11,12) #find nominal variables(seasons, holiday, weekday) & response variable

## Selecting and standardizing numerical variables 
## Model Fitting

y.train = bike.train[,1] 
x.train = scale(bike.train[,-dvar]) 
x.test = scale(bike.test[,-dvar]) 
fit = knnreg(x=x.train, y=y.train, k=4)
str(fit)


## Predicting and Evaluating

yhat = predict(fit, newdata=x.train)
mean((bike.train$Rented.Bike.Count - yhat)^2)  # train MSE
mean(abs(bike.train$Rented.Bike.Count - yhat)) # train MAE

plot(bike.train$Rented.Bike.Count, yhat, xlab="Observed Values", ylab="Fitted Values", main = 'Train')
abline(a=0, b=1, col='red')


yhat = predict(fit, newdata=x.test)
mean((bike.test$Rented.Bike.Count - yhat)^2)  # test MSE
mean(abs(bike.test$Rented.Bike.Count - yhat)) # test MAE

plot(bike.test$Rented.Bike.Count, yhat, xlab="Observed Values", ylab="Fitted Values", main = 'Test')
abline(a=0, b=1, col='red')

##########################
# Computing the CV error


V = 10 #V-fold CV
mse.test = 0
mae.test = 0

set.seed(1234)
id = sample(1:V, nrow(bike), replace = T)

for(i in 1:V) {
  
  print(i)
  
  ## Data partitioning
  
  test.index = which(id==i)
  bike.train = bike[-test.index,] #train data                              
  bike.test  = bike[ test.index,] #test data
  
  ## Fitting
  dvar = c(1,10,11,12) #find nominal variables(seasons, holiday, weekday) & response variable
  
  y.train = bike.train[,1] 
  x.train = scale(bike.train[,-dvar])
  x.test = scale(bike.test[,-dvar])
  fit = knnreg(x=x.train, y=y.train, k=4)
  #str(fit)
  
  ## Predicting and Evaluating
  
  yhat = predict(fit, newdata=x.test)
  mse.test = mse.test + mean((bike.test$Rented.Bike.Count - yhat)^2)  # MSE
  mae.test = mae.test + mean(abs(bike.test$Rented.Bike.Count - yhat)) # MAE
}

#k=4
cv.mse.test  = mse.test/V;  cv.mse.test  # test CV MSE
cv.mae.test  = mae.test/V;  cv.mae.test  # test CV MAE 


#END

