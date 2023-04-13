install.packages("neuralnet")
install.packages("dummy")
library(neuralnet) 
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

## Model Fitting
fit.glm = glm(Rented.Bike.Count ~ ., data = bike.train, family = 'poisson') #poisson distribution
summary(fit.glm)
fit.step.glm = step(fit.glm, direction="both", trace=FALSE) #Stepwise variable selection
summary(fit.step.glm )


## Predicting and Evaluating
yhat.glm = predict(fit.step.glm, newdata=bike.train, type="response")
mean((bike.train$Rented.Bike.Count - yhat.glm)^2)  # train MSE
mean(abs(bike.train$Rented.Bike.Count - yhat.glm)) # train MAE

plot(bike$Rented.Bike.Count[1:length(yhat.glm)], yhat.glm, xlab="Observed Values", ylab="Fitted Values", main = 'Train')
abline(a=0, b=1, col = 'red')


yhat.glm = predict(fit.step.glm, newdata=bike.test, type="response")
mean((bike.test$Rented.Bike.Count - yhat.glm)^2)  # test MSE
mean(abs(bike.test$Rented.Bike.Count - yhat.glm)) # test MAE

plot(bike$Rented.Bike.Count[1:length(yhat.glm)], yhat.glm, xlab="Observed Values", ylab="Fitted Values", main = 'Test')
abline(a=0, b=1, col = 'red')


# Computing the CV error


V = 10 #V-fold CV
mse.train = 0; mse.test = 0
mae.train = 0; mae.test = 0

set.seed(1234)
id = sample(1:V, nrow(bike), replace = T)

for(i in 1:V) {
  
  print(i)
  
  ## Data partitioning
  
  test.index = which(id==i)
  bike.train = bike[ -test.index,]#train data                              
  bike.test = bike[ test.index,] #test data
  
  ## Model Fitting
  fit.glm = glm(Rented.Bike.Count ~ ., data = bike.train, family = 'poisson') #poisson distribution
  fit.step.glm = step(fit.glm, direction="both", trace=FALSE) #Stepwise variable selection
  summary(fit.step.glm )

  
  ## Predicting and Evaluating
  yhat.glm = predict(fit.step.glm, newdata=bike.test, type="response")
  mse.test = mse.test + mean((bike.test$Rented.Bike.Count - yhat.glm)^2)  # MSE
  mae.test = mae.test + mean(abs(bike.test$Rented.Bike.Count - yhat.glm)) # MAE
}


cv.mse.test  = mse.test/V;  cv.mse.test  # test CV MSE
cv.mae.test  = mae.test/V;  cv.mae.test  # test CV MAE 


#END







########################################


###neural network###
dvar = c(2,10,11,12,13) #find nominal variables(Hour, seasons, holiday, weekday, month)
bike2 = dummy(x=bike[,dvar]) # transform nominal variables into dummy variables
bike2
bike2 = bike2[,-c(24,28,30,32,44)] # delete redundant dummy variables 
bike2 = cbind(bike[,-dvar], bike2) # combine them
for(i in 1: ncol(bike2)) if(!is.numeric(bike2[,i])) bike2[,i] = as.numeric(bike2[,i])

## Data partitioning 
set.seed(1234)
train.index = sample(1:nrow(bike2), round(0.7*nrow(bike2)))
bike2.train = bike2[ train.index,] #train data                              
bike2.test  = bike2[-train.index,] #test data

## Scaling
max1 = apply(bike2.train, 2, max) 
min1 = apply(bike2.train, 2, min)
sdat.train = scale(bike2.train, center = min1, scale = max1 - min1)
sdat.train = as.data.frame(sdat.train)
sdat.test = scale(bike2.test, center = min1, scale = max1 - min1)
sdat.test = as.data.frame(sdat.test)

## Model fitting
vname = names(sdat.train)
f = as.formula(paste("Rented.Bike.Count ~", paste(vname[!vname %in% "Rented.Bike.Count"], collapse = " + ")))
fit.nn = neuralnet(f, data=sdat.train, hidden=c(3,1), linear.output=T, stepmax = 1e+7 ) #hidden (3,1) 
plot(fit.nn)

## train Predicting & Evaluating
pred.nn = predict(fit.nn, sdat.train) 
pred.nn = pred.nn*(max1[1]-min1[1])+min1[1]

mean((bike2.train$Rented.Bike.Count - pred.nn)^2)  # train MSE 
mean(abs(bike2.train$Rented.Bike.Count - pred.nn)) # train MAE 

plot(bike2.train$Rented.Bike.Count, yhat.tree, xlab="Observed Values", ylab="Fitted Values");abline(a=0, b=1)

## test Predicting & Evaluating
pred.nn = predict(fit.nn, sdat.test) 
pred.nn = pred.nn*(max1[1]-min1[1])+min1[1]

mean((bike2.test$Rented.Bike.Count - pred.nn)^2)  # test MSE 
mean(abs(bike2.test$Rented.Bike.Count - pred.nn)) # test MAE 

plot(bike2.test$Rented.Bike.Count, yhat.tree, xlab="Observed Values", ylab="Fitted Values");abline(a=0, b=1)




############################################


###Decision Tree###
install.packages("rpart")
library(rpart) 


bike = read.csv("bike_final.csv")
head(bike)
bike$X <- NULL
head(bike,10)

bike$Seasons <- factor(bike$Seasons)
bike$Holiday <- factor(bike$Holiday)
bike$weekday <- factor(bike$weekday)
bike$month <- factor(bike$month)
bike$Hour <- factor(bike$Hour)



# Computing the test error by partitioning
set.seed(1234)
train.index = sample(1:nrow(bike), round(0.7*nrow(bike)))
bike.train = bike[ train.index,] #train data                              
bike.test  = bike[-train.index,] #test data


## Growing a tree
fit2 = rpart(Rented.Bike.Count ~., data=bike.train, method="anova", 
             control = rpart.control(xval=10, cp=0.0001)) #cp=0 (maximal tree)


## Pruning
tmp = printcp(fit2)
k = which.min(tmp[,"xerror"])
cp.tmp = tmp[k,"CP"]
fit.pruned = prune(fit2, cp=cp.tmp)
plot(fit.pruned, margin = 0.1);text(fit.pruned, use.n=TRUE)


## Predicting and Evaluating
yhat.tree = predict(fit.pruned, newdata=bike.train, type="vector")
mean((bike.train$Rented.Bike.Count - yhat.tree)^2)  # train MSE
mean(abs(bike.train$Rented.Bike.Count - yhat.tree)) # train MAE

plot(bike.train$Rented.Bike.Count, yhat.tree, xlab="Observed Values", ylab="Fitted Values");abline(a=0, b=1)

yhat.tree = predict(fit.pruned, newdata=bike.test, type="vector")
mean((bike.test$Rented.Bike.Count - yhat.tree)^2)  # test MSE
mean(abs(bike.test$Rented.Bike.Count - yhat.tree)) # test MAE


plot(bike.test$Rented.Bike.Count, yhat.tree, xlab="Observed Values", ylab="Fitted Values");abline(a=0, b=1)


##########################
# Computing the CV error


V = 10 #V-fold CV
mse.train = 0; mse.test = 0
mae.train = 0; mae.test = 0

set.seed(1234)
id = sample(1:V, nrow(bike2), replace = T)

for(i in 1:V) {
  
  print(i)
  
  ## Data partitioning
  
  test.index = which(id==i)
  bike.train = bike2[ -test.index,]#train data                              
  bike.test = bike2[ test.index,] #test data
  
  ## Growing
  
  fit2 = rpart(Rented.Bike.Count ~., data=bike.train, method="anova", 
               control = rpart.control(xval=10, cp=0.001)) #cp=0 (maximal tree)
  
  ## Pruning
  
  tmp = printcp(fit2)
  k = which.min(tmp[,"xerror"])
  cp.tmp = tmp[k,"CP"]
  fit.pruned = prune(fit2, cp=cp.tmp)
  #plot(fit.pruned, margin = 0.1);text(fit.pruned, use.n=TRUE)
  
  ## Predicting and Evaluating
  
  yhat.tree = predict(fit.pruned, newdata=bike.test, type="vector")
  mse.test = mse.test + mean((bike.test$Rented.Bike.Count - yhat.tree)^2)  # MSE
  mae.test = mae.test + mean(abs(bike.test$Rented.Bike.Count - yhat.tree)) # MAE
}


cv.mse.test  = mse.test/V;  cv.mse.test  # test CV MSE
cv.mae.test  = mae.test/V;  cv.mae.test  # test CV MAE 


#END




