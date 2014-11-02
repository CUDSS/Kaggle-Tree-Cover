##################################################################
#
#	Kaggle forest type cover challenge
#	http://www.kaggle.com/c/forest-cover-type-prediction
#	
#	Health warning: this is just to get going, not a good model
#
###################################################################

### Getting the data

#training data
D<-read.csv("train.csv")
X<-D[,2:55] #design matrix. drop first column - ids - and last column - the outcome variable.
Y<-factor(D[,56])  #response. Y is a factor because the numbers 1...7 mean different species in no particular order

#test data
D.test<-read.csv("test.csv")
I.test<-D.test[,1] #ids of the test entries, we will need these for output
X.test<-D.test[,2:55] #design matrix. drop first column keep the rest

### Modelling

library(nnet) #for multinomial logistic regression functions
model.1 <- multinom(Y~.,X) #This says regress Y on all given predictors, given predictors are in matrix X.
preds.1 <- predict(model.1) #Return predictions for the training data

### Assessment

correct.1 <- (as.numeric(Y) - as.numeric(preds))==0 #which ones did we get right?
sum(correct.1) / nrow(X) #what fraction did we get right (WARNING: in the training data!!)?

### Prepare a submission

preds.1.test <- predict(model.1,newdata=X.test)
output <- cbind(I.test,preds.1.test)
colnames(output) <- c("Id","Cover_Type")
write.csv(output,row.names=FALSE,file="output1.csv")





