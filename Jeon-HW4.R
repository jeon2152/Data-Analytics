#Assignment #4
#Name: Jiwon Jeon
#DSA/ISE 5103 Intelligent Data Analytics
#Date: 10/31/2016

#required packages for this assignment
library(ggplot2)    #provides the command 'ggplot' for Problem 1
library(jpeg)       #provides the command 'writeJPEG' for Problem 1
library(MASS)       #provides the command 'lda','stepAIC' for Problem 2
library(caret)      #provides the command 'train' & 'trainControl' for Problem 2 (data tuning; determines hyperparameters)
library(corrplot)   #provides the command 'corrplot' for Problem 2
library(car)        #provides the command 'vif' for Problem 2
library(pls)        #provides the command 'plsr' for Problem 2
library(lars)       #provides the command 'lars' for Problem 2
library(glmnet)     #provides the command 'glmnet' for Problem 2

#Problem 1(a)
train = read.csv("classDigits.csv", header = TRUE)     #load the digit data
label = train[,1]                                      #keep the label column
digit = train[,-1]                                     #remove the label column
pcaDigit = prcomp(digit)                               #PCA analysis on the digit data without scaling
evDigit = pcaDigit$rotation                            #principal components (eigenvectors) of the digit data
head(pcaDigit$rotation)                                #a part of the principal components (eigenvectors)


#Problem 1(b)
meanDigit = pcaDigit$center                            #mean of each column of the digit data (a vector of mean digit), x_bar
meanDigit                                              #a numeric vector of 28^2 values (784 values)

meanDigit = as.matrix(meanDigit)
digitMatrix = matrix(meanDigit, 28, 28, byrow = TRUE)  #converts the numeric vector into 28x28 matrix

image(digitMatrix, col = grey(seq(0,1,length=256)))     #prints an image of the mean digit
writeJPEG(digitMatrix, target = "meanDigit.jpg")       #creates a JPG image file of the mean digit

jpeg("meanDigitImp.jpg", width = 2800, height = 2800, res = 500)
image(digitMatrix, col = grey(seq(0,1,length=256)))
dev.off()


#Problem 1(c)

#image15-5.jpg

#calculation of weights ---------------------------------
meanDigit = t(meanDigit)                                    #transposes the meanDigit matrix from 784x1 to 1x784

x15 = digit[15,]                                            #extracts the pixels of the training image #15
x15 = as.matrix(x15)                                        #converts the vector into matrix of 1x784 

difx15 = x15 - meanDigit                                    #calculates the difference between x15 and meanDigit, x - x_bar

ev5 = evDigit[,1:5]                                         #extracts eigenvectors upto certain dimensions, k (PC1 to PCk)
ev5 = as.matrix(ev5)                                        #converts the eigenvectors into matrix of 784xk

a15.5 = difx15%*%ev5                                        #calculates the weights of each eigenvector, which becomes 1xk matrix
a15.5

#as the weights correspond to the projections on to pc dimentions, 
#they can also be directly obtained from pcaDigit$x
w15.5 = pcaDigit$x[15,1:5]
w15.5                                                       #it is confirmed that a15.5 == w15.5

x15.5New = meanDigit + a15.5%*%t(ev5)                       #reconstructs the training image #15, which becomes a 1x784 numeric vector 
x15.5New2 = meanDigit + w15.5%*%t(ev5)                      #the reconstruction also can be done using the projections from pca result

x15.5NewMatrix = matrix(x15.5New2, 28, 28, byrow = TRUE)    #converts the numeric vector into 28x28 matrix

image(x15.5NewMatrix, col = grey(seq(0,1,length=256)))     #prints an image of the reconstructed training image #15
writeJPEG(x15.5NewMatrix, target = "image15-5.jpg")         #creates a JPG image file of the reconstructed training image #15

jpeg("imageImp15-5.jpg", width = 2800, height = 2800, res = 500)
image(x15.5NewMatrix, col = grey(seq(0,1,length=256)))
dev.off()

#image15-20.jpg

ev20 = evDigit[,1:20]
ev20 = as.matrix(ev20)

w15.20 = pcaDigit$x[15, 1:20]
w15.20

x15.20New = meanDigit + w15.20%*%t(ev20)

x15.20NewMatrix = matrix(x15.20New, 28, 28, byrow = TRUE)

image(x15.20NewMatrix, col = grey(seq(0,1,length=256)))
writeJPEG(x15.20NewMatrix, target = "image15-20.jpg")

jpeg("imageImp15-20.jpg", width = 2800, height = 2800, res = 500)
image(x15.20NewMatrix, col = grey(seq(0,1,length=256)))
dev.off()

#image15-100.jpg

ev100 = evDigit[,1:100]
ev100 = as.matrix(ev100)

w15.100 = pcaDigit$x[15,1:100]
w15.100

x15.100New = meanDigit + w15.100%*%t(ev100)

x15.100NewMatrix = matrix(x15.100New, 28, 28, byrow = TRUE)

image(x15.100NewMatrix, col = grey(seq(0,1,length=256)))
writeJPEG(x15.100NewMatrix, target = "image15-100.jpg")

jpeg("imageImp15-100.jpg", width = 2800, height = 2800, res = 500)
image(x15.100NewMatrix, col = grey(seq(0,1,length=256)))
dev.off()

#image100-5.jpg

w100.5 = pcaDigit$x[100,1:5]
x100.5New = meanDigit + w100.5%*%t(ev5)
x100.5NewMatrix = matrix(x100.5New, 28, 28, byrow = TRUE)

image(x100.5NewMatrix, col = grey(seq(0,1,length=256)))
writeJPEG(x100.5NewMatrix, target = "image100-5.jpg")

jpeg("imageImp100-5.jpg", width = 2800, height = 2800, res = 500)
image(x100.5NewMatrix, col = grey(seq(0,1,length=256)))
dev.off()

#image100-20.jpg

w100.20 = pcaDigit$x[100,1:20]
x100.20New = meanDigit + w100.20%*%t(ev20)
x100.20NewMatrix = matrix(x100.20New, 28, 28, byrow = TRUE)

image(x100.20NewMatrix, col = grey(seq(0,1,length=256)))
writeJPEG(x100.20NewMatrix, target = "image100-20.jpg")

jpeg("imageImp100-20.jpg", width = 2800, height = 2800, res = 500)
image(x100.20NewMatrix, col = grey(seq(0,1,length=256)))
dev.off()

#image100-100.jpg

w100.100 = pcaDigit$x[100,1:100]
x100.100New = meanDigit + w100.100%*%t(ev100)
x100.100NewMatrix = matrix(x100.100New, 28, 28, byrow = TRUE)

image(x100.100NewMatrix, col = grey(seq(0,1,length=256)))
writeJPEG(x100.100NewMatrix, target = "image100-100.jpg")

jpeg("imageImp100-100.jpg", width = 2800, height = 2800, res = 500)
image(x100.100NewMatrix, col = grey(seq(0,1,length=256)))
dev.off()


#Problem 1(d)

#determines k

summary(pcaDigit)                                      #checks the proportion of variance and cumulative proportion of PC's

#screeplots ---------------------------------------------------------------------------------------------------------------

screeplot(pcaDigit, npcs = 200, type = "lines",        #screeplot for pcaDigit
          main = "Screeplot for digit data")

#a function to use ggplot2 to create prettier scree plots
#source: https://github.com/vqv/ggbiplot/blob/master/R/ggscreeplot.r
ggscreeplot = function(pcaDigit, k=200, type = c('pev', 'cev'))    #type: the type of scree plot
{                                                                  #      'pev' is proportion of explained variance, i.e. the eigenvalues divided by the trace
  type = match.arg(type)                                           #      'cev' is the cumulative proportion of explained variance, i.e. the partial sum of the first k eigenvalues divided by the trace
  d = pcaDigit$sdev^2
  yvar = switch(type, 
                pev = d / sum(d), 
                cev = cumsum(d) / sum(d))
  
  yvar.lab = switch(type,
                    pev = 'proportion of explained variance',
                    cev = 'cumulative proportion of explained variance')
  
  df = data.frame(PC = 1:length(d), yvar = yvar)
  
  ggplot(data = df[1:k,], aes(x = PC, y = yvar)) + 
    xlab('principal component number') + ylab(yvar.lab) +
    geom_bar(stat="identity",alpha=0.4,fill="blue",color="grey") + geom_line()
}

print(ggscreeplot(pcaDigit))                           #displays the screeplot using ggplot2

##more than 80% of variance is covered by PC1 ~ PC50, therefore k is determined to be 50 in this problem.

#the weights of the training data
k = 50
wTrain = pcaDigit$x[,1:k]              #weights of the training data (digit) with k=50; 
wTrain                                 #this is the first arg in mahalanobis function

#the weights of the test data
test = read.csv("class7test.csv", header = TRUE)
lableTest = test[,2]
testDigit = as.matrix(test[,c(-1,-2,-787)])

meanDigitMat = matrix(rep(meanDigit[1,],7), nrow = 7, ncol = 784, byrow = TRUE)     #makes 7x784 matrix of meanDigit

difDigit = testDigit - meanDigitMat                         #calculates the difference between test digits and meanDigit, x - x_bar

ev_k = as.matrix(evDigit[,1:k])                             #extracts eigenvectors upto certain dimensions, k (PC1 to PCk)
wTest = difDigit%*%ev_k                                     #calculates the weights of each eigenvector - the weigts of the test data;
wTest                                                       #each of the rows will be the second arg in mahalanobis function

#the covariance of the digit space
evTest = cov(wTrain)
evTest

#calculates the average mahalanobis distances
D = matrix(rep(0,30000*7),nrow = 30000, ncol = 7)           #calculate the mahalanobis distances
for (i in 1:7){                                             #from 30,000 training data to each of the 7 test data
  D[,i] = mahalanobis(wTrain, wTest[i,], evTest)
}
head(D)

M = colMeans(D)                                             #the 7 average mahalanobis distances for each test data
M

#or
DM = rep(0,7)
for (i in 1:7){
  DM[i] = mean(mahalanobis(wTrain, wTest[i,], evTest))
}
DM
plot(wTest)
points(DM)
plot(DM)

#Problem 1(e)
head(train)
head(test)

for (i in test[4:6,1]){
  k = 1
  repeat{
    wTrain = pcaDigit$x[,1:k]
    difImg = testDigit[i,] - meanDigit
    ev_k = as.matrix(evDigit[,1:k])
    wTest = difImg%*%ev_k
    
    wTrain = as.matrix(wTrain)
    evTest = cov(wTrain)
    
    whichImg = which.min(mahalanobis(wTrain, wTest, evTest))
    predLabel = train[whichImg,1]
    testLabel = test[i,2]
    
    if (predLabel == testLabel || k > 784) break
    k = k+1
  }
  print(k)
}



#Problem 2(a)
house = read.csv("housingData.csv", header = TRUE)    #loads the housing data
house = house[,c(-1,-2)]                              #removes the ID columns
house$SalePrice = log(house$SalePrice)
colnames(house)[colnames(house)=="SalePrice"] = "logSalePrice"
str(house)

#deletes features with missing data more than 20%
which(colSums(is.na(house))/nrow(house) >= 0.2)                   #finds out the attributes with >20% of NA
dataHousing = house[,colSums(is.na(house))/nrow(house) < 0.2]     #removes the attributes with >20% of NA
str(dataHousing)

#separates numeric variables and factor variables
attach(dataHousing)
numerics = sapply(dataHousing, is.numeric)
numericsData = dataHousing[,numerics]                             #numeric values and logSalePrice
str(numericsData)

factors = sapply(dataHousing, is.factor)
factorsData = dataHousing[,factors]
factorsData = cbind(factorsData, logSalePrice)                       #factor values and logSalePrice
str(factorsData)

#Visualizes and examines the important numeric variables to logSalePrice
numericsCor = cor(numericsData)                                   #calculates the correlation
corrplot(numericsCor, method = "circle")                          #finds out the most relavant variables to SalePrice
head(numericsCor)

##11 selected numeric variables: 
##OverallQual, YearBuilt, YearRemodAdd, TotalBsmtSF, X1stFlrSF, 
##GrLivArea, FullBath, TotRmsAbvGrd, Fireplaces, GarageCars, GarageArea

#Visualizes and examines the important factor variables to SalePrice
par(mfrow = c(3,3))
plot(logSalePrice ~., factorsData)                                   #plots the factor variables to logSalePrice
dev.off()

##11 selected factor variables:
##LandSlope, Neighborhood, Condition1, ExterQual, BsmtQual, 
##Heating, HeatingQC, CentralAir, KitchenQual, GarageFinish, SaleType


#Problem 2(b).i
validHousing = dataHousing[1:100,]                   
trainHousing = na.omit(dataHousing[101:1000,])       #deletes missing values for stepwise regression

ols.fit = lm(logSalePrice ~ OverallQual+YearBuilt+YearRemodAdd+
               TotalBsmtSF+X1stFlrSF+GrLivArea+FullBath+TotRmsAbvGrd+
               Fireplaces+GarageCars+GarageArea+LandSlope+Neighborhood+
               Condition1+ExterQual+BsmtQual+Heating+HeatingQC+CentralAir+
               KitchenQual+GarageFinish+SaleType, trainHousing)

summary(ols.fit)                                     #high adjusted R-squared: 0.8835, p-value: <2.2e-16, and RMSE: 0.1171
AIC(ols.fit)                                         #AIC: -1148.745
BIC(ols.fit)                                         #BIC: -912.976
mean(vif(ols.fit))                                   #average vif: 2.835265

ols.fit$coefficients

#stepwise regression
ols.fit.stepwise = stepAIC(ols.fit, direction = "both")
ols.fit.stepwise$anova
summary(ols.fit.stepwise)                            #high adjusted R-squared: 0.8834, p-value: <2.2e-16, and RMSE: 0.1171
mean(vif(ols.fit.stepwise))                          #average vif: 2.310478, it decreased a bit

ols.fit.stepwise$coefficients

#validates the OLS regression

ols.predict = predict(ols.fit, validHousing)
defaultSummary(data.frame(obs=validHousing$logSalePrice, pred=ols.predict))

ols.predict.stepwise = predict(ols.fit.stepwise, validHousing)
defaultSummary(data.frame(obs=validHousing$logSalePrice, pred=ols.predict.stepwise))

qplot(ols.predict,validHousing$logSalePrice)+geom_abline(slope=1,intercept=0,color="red")
qplot(ols.predict.stepwise,validHousing$logSalePrice)+geom_abline(slope=1,intercept=0,color="red")


#modeling and validation with caret package ----------------------------------------------------

ols.ctrl = trainControl(method="cv", number=5)
ols.model = train(logSalePrice ~ OverallQual+YearBuilt+YearRemodAdd+
                     TotalBsmtSF+X1stFlrSF+GrLivArea+FullBath+TotRmsAbvGrd+
                     Fireplaces+GarageCars+GarageArea+LandSlope+Neighborhood+
                     Condition1+ExterQual+BsmtQual+Heating+HeatingQC+CentralAir+
                     KitchenQual+GarageFinish+SaleType, 
                   data = trainHousing,
                   trControl=ols.ctrl, method="lm")

ols.model$resample$RMSE
ols.model$resample$Rsquared
ols.model$results

ols.se = ols.model$results$RMSESD/sqrt(5)
c(ols.model$results$RMSE - ols.se, ols.model$results$RMSE + ols.se)


#Problem 2(b).ii
plot(ols.fit.stepwise)

#test for non-constant variance (test for heteroscedasticity)
ncvTest(ols.fit.stepwise)                                           #p = 0.02792218: less than 0.05, 
                                                                    #therefore the variance of residuals is slightly not constant.
                                                                    #OLS may not be a proper model for this data set.


#Problem 2(c)
trainHousing2 = dataHousing                                         #training data with 1,000 observations and 67 variables

trainHousing2 = dataHousing[,c("OverallQual","YearBuilt","YearRemodAdd",
                                 "TotalBsmtSF","X1stFlrSF","GrLivArea","FullBath","TotRmsAbvGrd",
                                 "Fireplaces","GarageCars","GarageArea","LandSlope","Neighborhood",
                                 "Condition1","ExterQual","BsmtQual","Heating","HeatingQC","CentralAir",
                                 "KitchenQual","GarageFinish","SaleType","logSalePrice")]

pls.fit = plsr(logSalePrice ~., 
               data = trainHousing2,
               validataion = "CV")

summary(pls.fit)

#determine the number of PC's (ncomp) corresponding to the minimum CVRMSE
pls.CVRMSE<-RMSEP(pls.fit,validation="CV")
?RMSEP
pls.CVRMSE                                                          
str(pls.CVRMSE)
plot(pls.CVRMSE)
(min<-which.min(pls.CVRMSE$val[1,1,]))                              #ncomp is determined to be 46 & CVRMSE is 0.1124
points(min-1,pls.CVRMSE$val[1,1,min],col="red",cex=1.5, lwd=2)

#re-run pls with the determined ncomp
pls.fit = plsr(logSalePrice ~., 
               data = trainHousing2,
               ncomp = 46)
pls.coef = drop(coef(pls.fit))                                      #pls.fit$coefficients for ncomp = 46

#modeling and validation with caret package ----------------------------------------------------
pls.ctrl = trainControl(method="cv", number=5)                      #prediction with the number of k-fold = 5

pls.model = train(logSalePrice ~., 
                 data = trainHousing2, na.action = na.exclude,      #missing values will be removed from the fitting process, but
                 trControl=pls.ctrl, method="pls")                  #included as NAs in the residuals and fitted values
plot(pls.model)

pls.model$bestTune
pls.model$resample$RMSE
pls.model$resample$Rsquared
pls.model$results
str(pls.model)


#Problem 2(d)

#modeling and validation with caret package ----------------------------------------------------
lasso.ctrl = trainControl(method="cv", number=5)                      #prediction with the number of k-fold = 5

lasso.model = train(logSalePrice ~., 
                  data = trainHousing2, na.action = na.exclude,      #missing values will be removed from the fitting process, but
                  trControl=lasso.ctrl, method="glmnet")               #included as NAs in the residuals and fitted values
plot(lasso.model)

lasso.model$bestTune
lasso.model$resample$RMSE
lasso.model$resample$Rsquared
lasso.model$results
str(lasso.model)

lasso.coef = coef(lasso.model$finalModel, lasso.model$bestTune$lambda)


#Problem 2(e)
test = read.csv("housingTest.csv", header = TRUE)     #loads the test data
testHousing = test[,c(-1,-2)]                              #removes the ID columns

#Based on the RMSE, OLS is selected for prediction of SalePrice of test data:
#test the OLS regression
ols.test = predict(ols.fit.stepwise, testHousing)
test["SalePrice"] = exp(ols.test)
head(test)

write.csv(test[,c("Id","SalePrice")],"testHousing_result_Jeon-HW4.csv")
