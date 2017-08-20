#Assignment #5
#Name: Jiwon Jeon
#DSA/ISE 5103 Intelligent Data Analytics
#Date: 11/20/2016

#required packages for this assignment
library(caret)          #provides several model commands in Problem 3
library(randomForest)   #provides command 'randomForest' in Problem 3
library(pROC)           #for plotting ROC curves in Problem 3
library(glmnet)         #provides command glm for logistic regression in Problem 3
library(ROCR)           #provides command 'predict' in Problem 3 & 4
library(rpart.plot)     #provides decision tree plotting in Problem 3
library(MASS)           #provides command 'stepAIC' in Problem 3
library(AUC)            #for calculating AUC value in Problem 3 & 4
library(rminer)         #provides command 'fit' for svm and nn in Problem 4
library(LogicReg)       #for logistic regression
library(car)            #for outlierTest function
library(dplyr)
library(rpart)


# Problem 1

myModel = function(predVals) {

  # fit : fitted model
  # predicted_val : predicted target value using the model

    # i. Confusion matrix and statistics ----------------------------------
    print("Confusion matrix")
    
    conf.matrix = confusionMatrix(predVals$trueVal, predVals$predClass)
    print(conf.matrix)
    
    # ii. KS chart  and statistics ----------------------------------------
    print("KS CHART")
    
    predVals$group = cut(predVals$predProb, seq(1,0,-.1), include.lowest=T)
    xtab = table(predVals$group, predVals$trueVal)

    # create empty dataframe
    x = rnorm(50)
    y = runif(30)
    
    # check if x and y come from the same distribution
    ks.test(x, y)
    
    # check if x comes from a shifted gamma distribution with shape 3 and rate 2
    ks.test(x+2, "pgamma", 3, 2) # two-sided, exact
    ks.test(x+2, "pgamma", 3, 2, exact = FALSE)
    ks.test(x+2, "pgamma", 3, 2, alternative = "gr")

    KS = data.frame(Group=numeric(10),
                    CumPct0=numeric(10),
                    CumPct1=numeric(10),
                    Dif=numeric(10))
    for (i in 1:10)
    {
      KS$Group[i] = i
      KS$CumPct0[i] = sum(xtab[1:i,1]) / sum(xtab[,1])
      KS$CumPct1[i] = sum(xtab[1:i,2]) / sum(xtab[,2])
      KS$Dif[i] = abs(KS$CumPct0[i]-KS$CumPct1[i])
    }

    KS[KS$Dif==max(KS$Dif),]
    maxGroup = KS[KS$Dif==max(KS$Dif),][1,1]
    print(ggplot(data=KS)+
            geom_line(aes(Group,CumPct0), color="blue")+
            geom_line(aes(Group,CumPct1), color="red")+
            geom_segment(x=maxGroup, xend=maxGroup,
                         y=KS$CumPct0[maxGroup], yend=KS$CumPct1[maxGroup])+
            labs(title = "KS Chart", x= "Deciles", y = "Cumulative Percentage"))

    # iii. ROC curve and AUC ----------------------------------------------
    print("ROC curve with AUC value")
    
    rocobj = roc(predVals$trueVal, predVals$predClass)
    plot(rocobj, print.auc=TRUE, auc.polygon=TRUE, grid=c(0.1, 0.2),
     grid.col=c("green", "red"), max.auc.polygon=TRUE,
     auc.polygon.col="blue", print.thres=TRUE)

    # iv. Distribution of predicted probabilities values ------------------
    print("Distribution of predicted probablities")
    
    pred = ROCR::prediction(data.frame(predVals$predClass), data.frame(predVals$trueVal) )    #ROC curve for training data
    perf = ROCR::performance(pred,"tpr","tnr") 

    plot(perf,colorize=TRUE, print.cutoffs.at = c(0.25,0.5,0.75)); 
    abline(0, 1, col="red")  

    # plot accuracy by average cutoff level 
    perf = performance(pred, "acc")
    
    plot(0,0,type="n", xlim=c(0,1), ylim=c(0,7),     
       xlab="Prediction", ylab="Density",  
       main="Prediction Accuracy Comparison")
    # or
    # plot(perf, avg= "vertical",  
    #    spread.estimate="boxplot", 
    #    show.spread.at= seq(0.1, 0.9, by=0.1))
    
    for (runi in 1:length(pred@predictions)) {
        lines(density(pred@predictions[[runi]][pred@labels[[runi]]==1]), col= "blue")
        lines(density(pred@predictions[[runi]][pred@labels[[runi]]==0]), col="green")
    }
    
  # v. Concordant pairs ---------------------------------------------------
    Concordance = function(predicted_val, predVals) {
    predicted_val = predVals$predClass
    
    outcome_and_fitted_col = cbind(predicted_val, predVals$predProb)
    
    # get a subset of outcomes where the event happened
    ones = outcome_and_fitted_col[outcome_and_fitted_col[,1] == 1,]
    
    # get a subset of outcomes where the event did not actually happen
    zeros = outcome_and_fitted_col[outcome_and_fitted_col[,1] == 0,]
    
    # equate the length of the event and non-event tables
    if (length(ones[,1])>length(zeros[,1])) {ones = ones[1:length(zeros[,1]),]}
    else {zeros = zeros[1:length(ones[,1]),]}
    
    # make the result into dataframe: c(ones_outcome, ones_fitted, zeros_outcome, zeros_fitted)
    ones_and_zeros = data.frame(ones, zeros)
    
    # create columns to store concordant, discordant, and tie pair evaluations
    conc = rep(NA, length(ones_and_zeros[,1]))
    disc = rep(NA, length(ones_and_zeros[,1]))
    ties = rep(NA, length(ones_and_zeros[,1]))
    
    for (i in 1:length(ones_and_zeros[,1])) {
      
      # concordance test
      if (ones_and_zeros[i,2] > ones_and_zeros[i,4])
      {conc[i] = 1
      disc[i] = 0
      ties[i] = 0}
      
      # tie test
      else if (ones_and_zeros[i,2] == ones_and_zeros[i,4])
      {
        conc[i] = 0
        disc[i] = 0
        ties[i] = 1
      }
      
      # discordant pairs test
      else if (ones_and_zeros[i,2] < ones_and_zeros[i,4])
      {
        conc[i] = 0
        disc[i] = 1
        ties[i] = 0
      }
    }
    
    # save the rates
    conc_rate = mean(conc, na.rm=TRUE)
    disc_rate = mean(disc, na.rm=TRUE)
    tie_rate = mean(ties, na.rm=TRUE)
    
    return(list(conc_rate=conc_rate, num_concordant=sum(conc), disc_rate=disc_rate, num_discordant=sum(disc), tie_rate=tie_rate,num_tied=sum(ties)))
    }
    
  print(Concordance(predicted_val, predVals))
  
  # vi. D statistic -------------------------------------------------------
  data.1 = predVals[predVals$trueVal==1,]
  data.0 = predVals[predVals$trueVal==0,]
  print("D-statistic: ")
  
  dstat = mean(data.1$predProb) - mean(data.0$predProb)
  print(dstat)

  # vii. Lift chart -------------------------------------------------------
  perf.obj = ROCR::prediction(predictions=predVals$predClass,
                              labels=predVals$trueVal)
  lift.obj = ROCR::performance(perf.obj, measure="lift", x.measure="rpp")
  plot(lift.obj,
       main="Lift Chart",
       xlab="% Populations",
       ylab="Lift",
       col="orange")
  abline(1,0,col="red")
  
  # return values ---------------------------------------------------------
  auc_val = pROC::auc(predVals$trueVal, predVals$predClass)[1]
  result =  data.frame(accuracy = conf.matrix$overall[[1]], auc = auc_val)
  
  return(result)
}


# Problem 3(a)

# load data ---------------------------------------------------------------
bankData = read.csv("bank-additional-full.csv", sep = ";")
colnames(bankData)[21] = 'target'

headers = colnames(bankData)
featrues = headers[1:20]
target = headers[21]

head(bankData)

# split data --------------------------------------------------------------
set.seed(0)

##stratified random sampling: 80% of data for training and 20% for testing (caret PKG)
trainset = createDataPartition(y = bankData$target, 
                               p = .8,
                               list = FALSE,
                               times = 1)
training = bankData[trainset,]        #provides training dataset
testing = bankData[-trainset,]        #provides test dataset

dim(training)
dim(testing)

##summary(bankData)
summary(bankData$target)

#bankData$target = ifelse(bankData$target == "yes", 1,0)   #myModel function requires numeric type input.
                                                           #however, the dataset is converted into numeric type
                                                           #before it passes into the function.
training$target = as.factor(training$target)   #decision tree method requires factor type input
testing$target = as.factor(testing$target)     #decision tree method requires factor type input

# cross validation strategy -----------------------------------------------
cv.repeated.kfold = trainControl(method="repeatedcv", number=5, repeats=5)   #repeated k-fold cv, selected for use
#cv.boot = trainControl(method="boot", number=20)                             #boot strap
cv.kfold = trainControl(method = "cv",number = 5)                            #k-fold cv
#cv.leaveOneOut = trainControl(method="LOOCV")                                #leave-one-out cv 


# Problem 3(b)

formula = target ~.     #use all given features (21 features)

# logistic regression usin GLM command wihtout CV -------------------------

##actual classification of test data
table(testing$target)

##logistic regression model using caret PKG (GLM wih repeated k-fold CV)
glm.model.kfold = train(formula,
                        data = training, 
                        family=binomial,
                        method = "glm", 
                        trControl = cv.repeated.kfold
)

##classification(prediction) by logistic regression
glm_cv_prob1 = predict(glm.model.kfold, newdata=testing, type="prob") 
glm_cv_prob2 = predict(glm.model.kfold, newdata=testing, type="raw") 
table(glm_cv_prob2)

predVals_logis = data.frame(trueVal=as.numeric(testing$target)-1, predClass=as.numeric(glm_cv_prob2)-1, predProb = glm_cv_prob1[2])
tail(predVals_logis)
colnames(predVals_logis)[3]  = "predProb"
str(predVals_logis)
tail(predVals_logis)


##influence
plot(dffits(glm.model), pch=23, bg='grey', cex=2, ylab="DFFITS",
     main="Influence measure for bankData")

influence.measures(glm.model)
influencePlot(glm.model)

##variable inflation
vif(glm.model)      #shows there exist linearly dependent variables (multicollinearity)
alias(glm.model)
stepAIC(glm.model,k=log(length(training[,1])))
##result obtained (to be used if necessary)
formulaNew = target ~ default + contact + month + duration + pdays + poutcome + emp.var.rate + cons.price.idx + euribor3m

##residual analysis
par(mfrow = c(2,2))
plot(glm.model)

dev.off()

##outlier test, if necessary
#outlierTest(glm.model)                                         #detect outliers
#training = training[-c(24867,24092,40538,36044,24005,24044),]  #removes the outliers (cleaning dataset)


##evaluate the logistic regression model by 'myModel'
eval_logistic = myModel(predVals_logis)
eval_logistic


#Problem 3(c)

# elastic net -------------------------------------------------------------

##elastic net model
train.mat = model.matrix(formula, family = binomial, training)[,-1]
lambdas = 10 ^ seq(8, -4, length = 250)

eNet.model = glmnet(train.mat,
                    training$target, 
                    alpha = 1, 
                    lambda = lambdas, 
                    family = "binomial")

plot(eNet.model)

##classification(prediction) by elastic net
test.mat = model.matrix(formula, family = binomial, testing)[,-1]
pred_enet = predict(eNet.model,s = lambda_elastic, 
                    newx = test.mat, 
                    type = "response")
str(pred_enet)
pred_enet_class = as.numeric(pred_enet > 0.5)
table(pred_enet_class)

predVals_enet = data.frame(trueVal=as.numeric(testing$target)-1, predClass=pred_enet_class, predProb = pred_enet)
colnames(predVals_enet)[3]  = "predProb"
str(predVals_enet)
tail(predVals_enet)

##evaluate the elastic net model by 'myModel'
eval_enet = myModel(predVals_enet)
eval_enet


# decision tree -----------------------------------------------------------

##decision tree model
tree.model = train(target~.,
                   data = training,
                   method = "rpart",
                   trControl = cv.repeated.kfold)

rpart.plot(tree.model$finalModel)

##classification(prediction) by decision tree
tree_prob1 = predict(tree.model, newdata=testing, type="prob") 
tree_prob2 = predict(tree.model, newdata=testing, type="raw") 
summary(tree_prob2)
table(tree_prob2)

predVals_tree = data.frame(trueVal=as.numeric(testing$target)-1, predClass=as.numeric(tree_prob2)-1, predProb = tree_prob1[2])
colnames(predVals_tree)[3]  = "predProb"
str(predVals_tree)
tail(predVals_tree)

##evaluate the decision tree model by 'myModel'
eval_tree = myModel(predVals_tree)
eval_tree


# random forest -----------------------------------------------------------

##random forest model
rf.model = randomForest(target~., data=training)

#or
# rf_model = train(formula,
#                 data = training,
#                 method = "rf",
#                 trControl = cv.repeated.kfold)

temp = data.frame(varImp(rf.model))
result = data.frame(Feature = rownames(temp), InformationGain = temp$Overall)
result = arrange(result,-InformationGain)

##classification(prediction) by random forest
rf_pred = predict(rf.model,testing)
table(rf_pred)

predVals_rf = data.frame(trueVal=as.numeric(testing$target)-1, predClass=as.numeric(rf_pred)-1, predProb= as.numeric(rf_pred))
str(predVals_rf)
tail(predVals_rf)

##evaluate the decision tree model by 'myModel'
eval_rf = myModel(predVals_rf)
eval_rf


# Problem 3(d)
comparison = rbind(eval_logistic,eval_enet,eval_tree,eval_rf)
rownames(comparison) = c("Logistic Regression","Elastic Net","Decision Tree","Random Forest")
comparison


# Problem 4(a) - EXTRA CREDIT

# SVM model ---------------------------------------------------------------
svm.model = fit(formula,training,model="ksvm",task="class")

##classification(prediction) by SVM
svm_pred = predict(svm.model,testing)
table(svm_pred)
predVals_svm = data.frame(trueVal=as.numeric(testing$target)-1, predClass=as.numeric(svm_pred)-1, predProb= as.numeric(svm_pred))
str(predVals_svm)
tail(predVals_svm)

##evaluate the SVM model by 'myModel'
eval_svm = myModel(predVals_svm)
eval_svm


# Problem 4(b) - EXTRA CREDIT

# neural network (NN) model ----------------------------------------------------
predictprob = function(object,newdata)
{ predict(object,newdata,type="class") }
temp_model = list(fit=nnet::nnet,predict=predictprob,name="nnet")

nn_model = fit(formula,training,
               model=temp_model,
               size=5,
               task="class") 

##classification(prediction) by NN
nn_pred = predict(nn_model,testing)
table(nn_pred)
nn_pred = ifelse(nn_pred == "yes",1,0)
nn_pred

predVals_nn = data.frame(trueVal=as.numeric(testing$target)-1, predClass=nn_pred, predProb= nn_pred)
str(predVals_nn)
tail(predVals_nn)

##evaluate the NN model by 'myModel'
eval_nn = myModel(predVals_nn)
eval_nn


# Problem 3(d) - UPDATE
comparison = rbind(eval.logis,eval_enet,eval_tree,eval_rf,eval_svm,eval_nn)
rownames(comparison) = c("Logistic Regression","Elastic Net","Decision Tree","Random Forest","SVM","Neural Net")
comparison
