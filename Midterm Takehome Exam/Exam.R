# BOOK PROBLEMS

## CHAPTER 2: PROBLEM 10
#______________________________________________________________________________________________________________________________________
rm(list=ls()) 


library(MASS)

attach(Boston)

Boston = data.frame(Boston)

str(Boston)

############
### a
############
n = dim(Boston)

############
### b
############
pairs(Boston)

############
### c
############
cor.test(crim, zn)
cor.test(crim, indus)
cor.test(crim, chas)
cor.test(crim, nox)
cor.test(crim, zn)
cor.test(crim, rm)
cor.test(crim, age)
cor.test(crim, dis)
cor.test(crim, rad)
cor.test(crim, tax)
cor.test(crim, ptratio)
cor.test(crim, black)
cor.test(crim, lstat)
cor.test(crim, medv)

############
### d
############

#### crime
summary(crim)
highcrime = subset(Boston, crim > 10)
dim(highcrime)[1] / dim(Boston)[1]
#### tax
summary(tax)
hist(tax, ylab = "Number of Suburbs")
hightax = subset(Boston, tax >= 666)
dim(hightax)[1] / dim(Boston)[1]
#### pupil-teacher ratio by town
summary(ptratio)
hist(ptratio, ylab = "Number of Suburbs")


############
### e
############
bound = subset(Boston, chas == 1)
dim(bound)[1]

############
### f
############
summary(ptratio)

############
### g
############
summary(medv)
Boston[order(medv),][1,]
summary(Boston)

############
### h
############
dim(subset(Boston, rm > 7)[1])
highrm = (subset(Boston, rm > 8))
dim(highrm)[1]
summary(highrm)


#_____________________________________________________________________________________________________________________________________
## CHAPTER 3: PROBLEM 15
#_____________________________________________________________________________________________________________________________________
############
### a
############

summary(lm(crim ~ zn, data = Boston))
summary(lm(crim ~ indus, data = Boston))
summary(lm(crim ~ chas, data = Boston))
summary(lm(crim ~ nox, data = Boston))
summary(lm(crim ~ rm, data = Boston))
summary(lm(crim ~ age, data = Boston))
summary(lm(crim ~ dis, data = Boston))
summary(lm(crim ~ rad, data = Boston))
summary(lm(crim ~ tax, data = Boston))
summary(lm(crim ~ ptratio, data = Boston))
summary(lm(crim ~ black, data = Boston))
summary(lm(crim ~ lstat, data = Boston))
summary(lm(crim ~ medv, data = Boston))

lm.fit=lm(crim~medv)
plot(medv,crim)
abline(lm.fit,lwd = 3,col = "red")


lm.fit2=lm(crim~lstat)
plot(lstat,crim)
abline(lm.fit2,lwd = 3,col = "red")


lm.fit3=lm(crim~tax)
plot(tax,crim)
abline(lm.fit3,lwd = 3,col = "red")
     
############
### b
############
lm.fit4 = lm(crim ~., data = Boston)
summary(lm.fit4)

############
### c
############
univcof <- lm(crim ~ zn, data = Boston)$coefficients[2]
univcof <- append(univcof, lm(crim ~ indus, data = Boston)$coefficients[2])
univcof <- append(univcof, lm(crim ~ chas, data = Boston)$coefficients[2])
univcof <- append(univcof, lm(crim ~ nox, data = Boston)$coefficients[2])
univcof <- append(univcof, lm(crim ~ rm, data = Boston)$coefficients[2])
univcof <- append(univcof, lm(crim ~ age, data = Boston)$coefficients[2])
univcof <- append(univcof, lm(crim ~ dis, data = Boston)$coefficients[2])
univcof <- append(univcof, lm(crim ~ rad, data = Boston)$coefficients[2])
univcof <- append(univcof, lm(crim ~ tax, data = Boston)$coefficients[2])
univcof <- append(univcof, lm(crim ~ ptratio, data = Boston)$coefficients[2])
univcof <- append(univcof, lm(crim ~ black, data = Boston)$coefficients[2])
univcof <- append(univcof, lm(crim ~ lstat, data = Boston)$coefficients[2])
univcof <- append(univcof, lm(crim ~ medv, data = Boston)$coefficients[2])
fooBoston <- (lm(crim ~ . - crim, data = Boston))
fooBoston$coefficients[2:14]
plot(univcof, fooBoston$coefficients[2:14], main = "Univariate vs. Multiple Regression Coefficients", 
     xlab = "Univariate Coefficient", ylab = "Multiple Coefficient")

############
### c
############
summary(lm(crim ~ zn + I(zn^2) + I(zn^3), data = Boston))
summary(lm(crim ~ indus + I(indus^2) + I(indus^3), data = Boston))
summary(lm(crim ~ chas + I(chas^2) + I(chas^3), data = Boston))
summary(lm(crim ~ nox + I(nox^2) + I(nox^3), data = Boston))
summary(lm(crim ~ rm + I(rm^2) + I(rm^3), data = Boston))
summary(lm(crim ~ age + I(age^2) + I(age^3), data = Boston))
summary(lm(crim ~ dis + I(dis^2) + I(dis^3), data = Boston))
summary(lm(crim ~ rad + I(rad^2) + I(rad^3), data = Boston))
summary(lm(crim ~ tax + I(tax^2) + I(tax^3), data = Boston))
summary(lm(crim ~ ptratio + I(ptratio^2) + I(ptratio^3), data = Boston))
summary(lm(crim ~ black + I(black^2) + I(black^3), data = Boston))
summary(lm(crim ~ lstat + I(lstat^2) + I(lstat^3), data = Boston))
summary(lm(crim ~ medv + I(medv^2) + I(medv^3), data = Boston))

#_____________________________________________________________________________________________________________________________________
## CHAPTER 6: PROBLEM 9
#_____________________________________________________________________________________________________________________________________
rm(list=ls()) #Removes every object from your environment

set.seed(1)

############
### a
############

library(ISLR)
library(caret)
attach(College)

train = data.frame(College)
test = data.frame(College)

n = dim(train)[1]

#Sample (in this case with uniform distribution)
tr = sample(1:777, #The values that will be sampled
            size = 600, #The size of the sample
            replace = FALSE) #without replacement

train = train[tr,] #the rows of train will be the ones sampled
test = test[-tr,] #and test will be everything else (thus, out-of-sample)

preObj <- preProcess(train, method = c('center', 'scale'))

train <- predict(preObj, train)
test <- predict(preObj, test)


############
### b
############

model = lm(Apps ~ ., data = train)
summary(model)
pred = predict(model, test)
RMSE_linear = sqrt(mean((test$Apps-pred)^2))
print(RMSE_linear)

############
### c 
############

library(glmnet)

xtrain = model.matrix (Apps ~ ., train)[,-1]
ytrain = train$Apps
xtest = model.matrix(Apps ~ ., test)[,-1]
ytest = test$Apps

      #ridge regression !
set.seed (1)
CV.R = cv.glmnet(xtrain, ytrain, alpha = 0)
plot(CV.R)

# finding the best lambda from the cross validation
R.minlam = CV.R$lambda.min
print(R.minlam)
# Creating training model using ridge regression!
model.R = glmnet(xtrain, ytrain, alpha=0,lambda = R.minlam)


pred = predict(model.R, s = R.minlam, newx = xtest)
# Calculating Accuracy
RMSE_ridge = sqrt(mean((test$Apps-pred)^2))
print(RMSE_ridge)


############
### d
############

xtrain = model.matrix (Apps ~ ., train)[,-1]
ytrain = train$Apps
xtest = model.matrix(Apps ~ ., test)[,-1]
ytest = test$Apps

#lasso regression !
set.seed (1)
CV.L = cv.glmnet(xtrain, ytrain, alpha = 1)
plot(CV.L)

# finding the best lambda from the cross validation
L.minlam = CV.L$lambda.min
print(L.minlam)
# Creating training model using ridge regression!
model.L = glmnet(xtrain, ytrain, alpha = 1,lambda = L.minlam)


pred = predict(model.L, s = L.minlam, newx = xtest)
# Calculating Accuracy
RMSE_lasso = sqrt(mean((test$Apps-pred)^2))
print(RMSE_lasso)

# see the number of non-zero coefficients
coef.L = predict(CV.L,type="coefficients",s=L.minlam)[1:length(model.L$beta),]
coef.L[coef.L != 0]


############
### e
############

library(pls)

#pcr method 
pcr_fit <- train(x = xtrain, y = ytrain,
                   method = 'pcr',
                   trControl = trainControl(method = 'cv', number = 10),
                   tuneGrid = expand.grid(ncomp = 1:10))
#this will show the error of the prediction
(pcr_info = postResample(predict(pcr_fit, xtest), ytest))

# this will show a summary of the prediction with the number of components
summary(pcr_fit)

############
### f
############

#pls method
pls_fit <- train(x = xtrain, y = ytrain,
                 method = 'pls',
                 trControl = trainControl(method = 'cv', number = 10),
                 tuneGrid = expand.grid(ncomp = 1:10))
#this will show the error of the prediction
(pls_info = postResample(predict(pls_fit, xtest), ytest))

# this will show a summary of the prediction with the number of components
summary(pls_fit)

############
### g
############
# compared using data I already calculated on questions above!



#_____________________________________________________________________________________________________________________________________
## CHAPTER 6: PROBLEM 11
#_____________________________________________________________________________________________________________________________________

rm(list=ls()) #Removes every object from your environment

set.seed(1)
library(ISLR)
library(caret)
library(MASS)
attach(Boston)

############
### a
############

train = data.frame(Boston)
test = data.frame(Boston)

n = dim(train)[1]

#Sample (in this case with uniform distribution)
tr = sample(1:506, #The values that will be sampled
            size = 400, #The size of the sample
            replace = FALSE) #without replacement

train = train[tr,] #the rows of train will be the ones sampled
test = test[-tr,] #and test will be everything else (thus, out-of-sample)

preObj <- preProcess(train, method = c('center', 'scale'))

train <- predict(preObj, train)
test <- predict(preObj, test)


###### Multiple Linear Regression

model = lm(crim ~ ., data = train)
summary(model)
pred = predict(model, test)
RMSE_linear = sqrt(mean((test$crim-pred)^2))
print(RMSE_linear)


plot(model, main = "Multiple Linear Regression")
###### Ridge

library(glmnet)

xtrain = model.matrix (crim ~ ., train)[,-1]
ytrain = train$crim
xtest = model.matrix(crim ~ ., test)[,-1]
ytest = test$crim

#ridge regression !
set.seed (1)
CV.R = cv.glmnet(xtrain, ytrain, alpha = 0)
plot(CV.R, main = "Ridge")

# finding the best lambda from the cross validation
R.minlam = CV.R$lambda.min
print(R.minlam)
# Creating training model using ridge regression!
model.R = glmnet(xtrain, ytrain, alpha=0,lambda = R.minlam)


pred = predict(model.R, s = R.minlam, newx = xtest)
# Calculating Accuracy
RMSE_ridge = sqrt(mean((test$crim-pred)^2))
print(RMSE_ridge)


######## Lasso

xtrain = model.matrix (crim ~ ., train)[,-1]
ytrain = train$crim
xtest = model.matrix(crim ~ ., test)[,-1]
ytest = test$crim

#lasso regression !
set.seed (1)
CV.L = cv.glmnet(xtrain, ytrain, alpha = 1)
plot(CV.L, main = 'Lasso')

# finding the best lambda from the cross validation
L.minlam = CV.L$lambda.min
print(L.minlam)
# Creating training model using ridge regression!
model.L = glmnet(xtrain, ytrain, alpha = 1,lambda = L.minlam)


pred = predict(model.L, s = L.minlam, newx = xtest)
# Calculating Accuracy
RMSE_lasso = sqrt(mean((test$crim-pred)^2))
print(RMSE_lasso)

# see the number of non-zero coefficients
coef.L = predict(CV.L,type="coefficients",s=L.minlam)[1:length(model.L$beta),]
coef.L[coef.L != 0]


######## PCR

library(pls)

#pcr method 
pcr_fit <- train(x = xtrain, y = ytrain,
                 method = 'pcr',
                 trControl = trainControl(method = 'cv', number = 10),
                 tuneGrid = expand.grid(ncomp = 1:10))
#this will show the error of the prediction
(pcr_info = postResample(predict(pcr_fit, xtest), ytest))

# this will show a summary of the prediction with the number of components
summary(pcr_fit)
plot(pcr_fit, main = "PCR")

####### PLS

#pls method
pls_fit <- train(x = xtrain, y = ytrain,
                 method = 'pls',
                 trControl = trainControl(method = 'cv', number = 10),
                 tuneGrid = expand.grid(ncomp = 1:10))
#this will show the error of the prediction
(pls_info = postResample(predict(pls_fit, xtest), ytest))

# this will show a summary of the prediction with the number of components
summary(pls_fit)
plot(pls_fit, main = "PLS")

############
### b
############
print(RMSE_linear)
print(RMSE_ridge)
print(RMSE_lasso)
print(pcr_info)
print(pls_info)

############
### c
############
summary(model)



#_____________________________________________________________________________________________________________________________________
## CHAPTER 4: PROBLEM 10    ------> omit e and f
#_____________________________________________________________________________________________________________________________________

rm(list=ls()) #Removes every object from your environment

set.seed(1)
library(ISLR)
library(caret)
library(kknn)
library(class)
attach(Weekly)

############
### a
############

df = data.frame(Weekly)
summary(Weekly)

plot(Volume~Year, col="darkred", data=Weekly)
simplelm = lm(Volume~Year, data=Weekly)
abline(simplelm, lwd= 3, col= "darkgreen")

plot(Today~Year, col="darkred", data=Weekly)
simplelm = lm(Today~Year, data=Weekly)
abline(simplelm, lwd= 3, col= "darkgreen")

############
### b
############

log_reg = glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, family = "binomial", data = Weekly)
summary(log_reg)

############
### c
############
p = predict(log_reg, type = "response")
prediction = rep("Down", 1089)
prediction[p > 0.5] = "Up"
table(prediction, Direction)

############
### d
############


train = Weekly[Year<2009,]
test = Weekly[Year>2008,]
log_reg2 = glm(Direction ~ Lag2, data= train, family = "binomial")
summary(log_reg2)

p2 = predict(log_reg2, type = "response")
prediction2 = rep("Down", 1089)
prediction2[p2 > 0.5] = "Up"
table(prediction2, Direction)

############
### g
############

x.train = cbind(train$Lag2)
y.train = cbind(train$Direction)
x.test = cbind(test$Lag2)
prediction_knn = knn(x.train, x.test, y.train, k = 1)
table(prediction_knn, test$Direction)


############
### i
############

# logistic regression with lag1, lag2, and lag4
log_reg3 = glm(Direction ~ Lag1 + Lag2 + Lag4, family = "binomial", data = train)
# summary(log_reg3)
p3 = predict(log_reg3, type = "response")
prediction3 = rep("Down", 1089)
prediction3[p3 > 0.5] = "Up"
table(prediction3, Direction)

# logistic regression with lag1 and lag2
log_reg4 = glm(Direction ~ Lag1 + Lag2, family = "binomial", data = train)
# summary(log_reg4)
p4 = predict(log_reg4, type = "response")
prediction4 = rep("Down", 1089)
prediction4[p4 > 0.5] = "Up"
table(prediction4, Direction)



# different knn values

# k = 3
prediction_knn3 = knn(x.train, x.test, y.train, k = 3)
table(prediction_knn3, test$Direction)

# k = 5
prediction_knn5 = knn(x.train, x.test, y.train, k = 5)
table(prediction_knn5, test$Direction)

# k = 7
prediction_knn7 = knn(x.train, x.test, y.train, k = 7)
table(prediction_knn7, test$Direction)


#_____________________________________________________________________________________________________________________________________
## CHAPTER 8: PROBLEM 8   
#_____________________________________________________________________________________________________________________________________

rm(list=ls()) #Removes every object from your environment
set.seed(1)
library(tree)
library(ISLR)
library(randomForest)
attach(Carseats)
library(kknn)

############
### a
############

n = dim(Carseats)[1]
#Sample (in this case with uniform distribution)
tr = sample(1:400, #The values that will be sampled
            size = 300, #The size of the sample
            replace = FALSE) #without replacement

# train and test set of the Carseats data
train = Carseats[tr,]
test = Carseats[-tr,]

############
### b
############

# creates the tree with sales as the y variable
tree = tree(Sales ~ ., data = train)

#plots the tree
plot(tree)
text(tree, pretty = 0)

# predicts the validation set
tree.pred = predict(tree, newdata = test)

#finds the MSE
mean((tree.pred - test$Sales)^2)


############
### c
############


cv = cv.tree(tree)
plot(cv$size, cv$dev, type = "b")
#tree.min = which.min(cv$dev)

pruned = prune.tree(tree, best = 11)
plot(pruned)
text(pruned, pretty = 0)

# predicts the validation set
pruned.pred = predict(pruned, newdata = test)

#finds the MSE
mean((pruned.pred - test$Sales)^2)

############
### d
############

# bagging with 10 variables tried at each split
bag = randomForest(Sales ~ ., data = train, mtry = 10, importance = TRUE)

prediction_bag = predict(bag, newdata = test)

#Find the MSE of the bagging prediction 
mean((prediction_bag - test$Sales)^2)

# Use importance function to find out important variables 
importance = importance(bag)
# plot the most important variables
varImpPlot(bag, type = 2)

############
### e
############

# random forest with different numbers of mtry
m = c(1:10)
for (i in m){
  rf = randomForest(Sales ~ ., data = train, mtry = i, importance = TRUE)
  prediction_rf = predict(rf, newdata = test)
  print(i)
  MSE = (mean((prediction_rf - test$Sales)^2))
  print(MSE)
}

# randomForest with the lowest MSE
rf_best = randomForest(Sales ~ ., data = train, mtry = 7, importance = TRUE)
importance = importance(rf_best)
varImpPlot(rf_best, type = 2)

#_____________________________________________________________________________________________________________________________________
## CHAPTER 8: PROBLEM 11   
#_____________________________________________________________________________________________________________________________________

rm(list=ls()) #Removes every object from your environment
set.seed(1)
library(tree)
library(ISLR)
library(randomForest)
library(gbm)
attach(Caravan)


############
### a
############

n = dim(Caravan)[1]
#Sample (in this case with uniform distribution)
tr = sample(1:5822, #The values that will be sampled
            size = 1000, #The size of the sample
            replace = FALSE) #without replacement

# train and test set of the Carseats data
train = Caravan[tr,]
test = Caravan[-tr,]

############
### b
############

boost <- gbm(Purchase ~ ., data = train, distribution = "gaussian", n.trees = 1000, shrinkage = 0.01)
summary(boost)

############
### c
############

# using boost model 
probs <- predict(boost, test, n.trees = 1000, type = "response")

pred_boost <- ifelse(probs > 0.2, 1, 0)
table(test$Purchase, pred_boost)

#logistic reg
log_caravan = glm(Purchase ~ ., data = train, family = "binomial")
probs2 <- predict(log_caravan, test, type = "response")
pred2 =  ifelse(probs2 > .2, 1, 0)
table(test$Purchase, pred2)


#--------------------------------------------------------------------------------------------------------------------------------------

## NON-BOOK PROBLEMS ------------------------------------------------------------------------------------------------------------------
#_____________________________________________________________________________________________________________________________________
## PROBLEM 1: BEAUTY PAYS  
#_____________________________________________________________________________________________________________________________________

rm(list=ls()) #Removes every object from your environment
#Read data
beautyData = read.csv("BeautyData.csv",header=T)
attach(beautyData)

# linear regression
lm.1 = lm(CourseEvals ~ BeautyScore, data = beautyData)
plot(BeautyScore, CourseEvals)
abline(lsfit(BeautyScore,CourseEvals), #lsfit can be used instead of lm()
       lwd=2, #Line width
       col=2) #Line color
summary(lm.1)

lm.all = lm(CourseEvals ~ ., data = beautyData)
summary(lm.all)

#variable importance using bagging
bag_beauty = randomForest(CourseEvals ~ ., data = beautyData, mtry = 10, importance = TRUE)
importance(bag_beauty)
varImpPlot(bag_beauty, type = 2, main = "Variable Importance in Predicting Eval")

#_____________________________________________________________________________________________________________________________________
## PROBLEM 2: Housing Price structure 
#_____________________________________________________________________________________________________________________________________

rm(list=ls()) #Removes every object from your environment
#Read data
set.seed(1)
mc.data = read.csv("MidCity.csv",header=T)
attach(mc.data)

############
### 1
############
brick = ifelse(mc.data$Brick == "Yes", 1, 0)
mc.data = mc.data[-5]
mc.data = cbind(mc.data, brick)

lm.mc = lm(Price ~ ., data = mc.data)
summary(lm.mc)


############
### 2
############

# makes new column with dummy variable for houses in neighborhood 3
nbhd_3 = ifelse(mc.data$Nbhd == 3, 1, 0)
mc.data = mc.data[-2]
mc.data = cbind(mc.data, nbhd_3)

lm.mc3 = lm(Price ~ ., data = mc.data)
summary(lm.mc3)

############
### 3
############

# makes a new data frame with only houses in neighborhood 3
mc.data_3 = mc.data[ which(mc.data$nbhd_3 == 1),]
lm.brick3 = lm(Price ~ ., data = mc.data_3)
summary(lm.brick3)

############
### 4
############

##### refer to 2

#_____________________________________________________________________________________________________________________________________
## PROBLEM 4: Neural Nets
#_____________________________________________________________________________________________________________________________________
rm(list=ls()) #Removes every object from your environment

set.seed(1)
library(ISLR)
library(caret)
library(MASS)

#Read in the data
attach(Boston)
df = data.frame(Boston)
#Summary of the data
summary(Boston)

#Standardize the x's (the first 3 columns)
minv = rep(0,13) #Create vector which will hold the minimum
maxv = rep(0,13) #Create vector which will hold the maximum
Boston.xc = Boston #Create auxiliary copy of the matrix
for(i in 1:13) {
  minv[i] = min(Boston[[i]]) #Save the minimum
  maxv[i] = max(Boston[[i]]) #Save the maximum
  Boston.xc[[i]] = (Boston[[i]]-minv[i])/(maxv[i]-minv[i]) #Standardize the values
}

# nn library
library(nnet)

#Fit nn with just one x=lstat
set.seed(1) #Seed to guarantee the same results

#Create the model
znn = nnet(medv ~ lstat, #Formula
           data = Boston.xc, #Data frame with the traning set
           size=3, #Units in the hidden layer
           decay=0.1, #Parameter for weight decay
           linout=T) #Linear output


#Get fits, print summary, and plot fit
fznn = predict(znn,Boston.xc) #Gets the models fits for the data
plot(Boston.xc$lstat,Boston.xc$medv) #Dispersion plot of lstat and medv
oo = order(Boston.xc$lstat) #Get the indices that will order the column lstat
lines(Boston.xc$lstat[oo],fznn[oo],col="red",lwd=2) #Line of the fits
abline(lm(medv~lstat,Boston.xc)$coef) #Compare with the OLS fit

#What does this mean? Try to interpret looking at the Neural network
summary(znn)
NeuralNetTools::plotnet(znn)


#Now let us try a model with 5 units in the hidden layer
set.seed(99)
znn = nnet(medv ~ lstat, #Formula
           data = Boston.xc, #Data frame with the traning set
           size=5, #Units in the hidden layer
           decay=0.1, #Parameter for weight decay
           linout=T) #Linear output
print(summary(znn))

#Now, let us estimate a model with all covariates
znn = nnet(medv ~ ., #Formula
           data = Boston.xc, #Data frame with the traning set
           size=5, #Units in the hidden layer
           decay=0.1, #Parameter for weight decay
           linout=T) #Linear output
fznn = predict(znn,Boston.xc) #Gets the models fits for the data
zlm = lm(medv~.,Boston.xc) #Estimating medv using OLS
fzlm = predict(zlm,Boston.xc) #Gets the OLS fits for the data
temp = data.frame(y=Boston.xc$medv,fnn=fznn,flm=fzlm) #Data frame of results
pairs(temp) #Matrix of scatterplots
print(cor(temp)) #Correlation matrix


#Let us modify the number of nodes in the hidden layer and decay values

#Four different fits
set.seed(1)
znn1 = nnet(medv~lstat,Boston.xc,size=3,decay=.5,linout=T)
znn2 = nnet(medv~lstat,Boston.xc,size=3,decay=.00001,linout=T)
znn3 = nnet(medv~lstat,Boston.xc,size=50,decay=.5,linout=T)
znn4 = nnet(medv~lstat,Boston.xc,size=50,decay=.00001,linout=T)

temp = data.frame(medv = Boston.xc$medv, lstat = Boston.xc$lstat) #The data

#The predictions of each model for the data
znnf1 = predict(znn1,temp)
znnf2 = predict(znn2,temp)
znnf3 = predict(znn3,temp)
znnf4 = predict(znn4,temp)

#Plotting the fits
par(mfrow=c(2,2)) #Plot window: 2 row, 2 columns

plot(Boston.xc$lstat,Boston.xc$medv, xlab = "lstat", ylab = "medv") #Scatterplot
lines(Boston.xc$lstat[oo],znnf1[oo],lwd=2) #Adding the lines of predicted values
title("size=3, decay=.5")

plot(Boston.xc$lstat,Boston.xc$medv, xlab = "lstat", ylab = "medv")
lines(Boston.xc$lstat[oo],znnf2[oo],lwd=2)
title("size=3, decay=.00001")

plot(Boston.xc$lstat,Boston.xc$medv, xlab = "lstat", ylab = "medv")
lines(Boston.xc$lstat[oo],znnf3[oo],lwd=2)
title("size = 50, decay = .5")

plot(Boston.xc$lstat,Boston.xc$medv, xlab = "lstat", ylab = "medv")
lines(Boston.xc$lstat[oo],znnf4[oo],lwd=2)
title("size = 50, decay = .00001")
