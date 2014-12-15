#Split Data into training and test sets
TrainTestSets = function(df, trainPerc, trainName, testName, seed=1){
  set.seed=(seed)
  split = round(trainPerc*nrow(df))
  df$rand = sample(1:nrow(df), nrow(df), replace=F)
  assign(toString(trainName), df[df$rand<=split,],envir = .GlobalEnv)
  assign(toString(testName),  df[df$rand>split,], envir = .GlobalEnv)
}

## prediction: predicted default probabilities for cases in test set
m = glm(Outcome~.-rand-predProb, family=binomial, data=train);
train$predProb <- predict(m, newdata=train,type="response")
mtest <- predict(m, newdata=test,type="response")
bb=data.frame(cbind(mtest, test$Outcome))
names(bb) = c("mtest", "Outcome")
bb1= data.frame(bb[order(mtest, decreasing=TRUE),])
names(bb) = c("mtest", "Outcome")
summary(bb1[bb1$Outcome ==1,])

p.naive=mean(test$Outcome) ## overall success (death penalty) prob in the test data set
accu = cumsum(bb1[, 2]);   ## accumulative outcome 
## Lift curve 
#check if cases with largest predicted probabilities of success are actually true successes. identifying the most important class 4) very steep incline at the beginning → good lift (model)
#If it's close to reference line → not much better than naive model
plot(1:nrow(test), accu, type="l", xlab="number of cases",ylab="number of successes",main="Lift: Cum successes sorted by pred prob")
abline(0, p.naive)

## Misclassification rates on that testing set 
## Case 1: using probability cutoff 1/6 (Outcome= 1 if probability 1/6 or larger)
cutoff=1/6

## Confusion Matrix
y.pred = ifelse(mtest >= cutoff, 1, 0);
cm = table(mtest, y.pred)
cm

## Sensitivity (predict default when it does happen)
##
sum(ytest==1 & ptest >= cutoff)/sum(ytest==1) 

## Specificity (predict no default when it does not happen)
##
sum(ytest==0 & ptest < cutoff)/sum(ytest==0) 

cutoff=1/2
y.pred = ifelse(ptest >= cutoff, 1, 0);

## Sensitivity (predict default when it does happen)
sum(ytest==1 & ptest >= cutoff)/sum(ytest==1) 

## Specificity (predict no default when it does not happen)
sum(ytest==0 & ptest < cutoff)/sum(ytest==0) 

## using the ROCR package to graph the ROC curves 
## input is a data frame of two columns: 1=prob predictions, 2=actual outcome 
##
library(ROCR)  

predictions=ptest
labels=ytest
data=data.frame(predictions,labels)
data[1:20, ]

## pred: function to create prediction objects
pred <- prediction(data$predictions,data$labels)
pred

## perf: creates the input to be plotted
## sensitivity and one minus specificity (the false positive rate)
perf <- performance(pred, "sens", "fpr")
perf
plot(perf)
abline(0, 1);