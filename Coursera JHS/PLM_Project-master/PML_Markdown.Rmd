Practical Machine Learning Project
========================================================

Here is the code I ran to get my results and what not.

```
library(caret)
library(randomForest)
library(gbm)
library(plyr)

##########################################
# readin datasets

train <- read.csv("train.csv",stringsAsFactors = TRUE)

test.set <- read.csv("test.csv")

#checking what was readin
names(train); dim(train); str(train)
test

#wow that's a lot vars in train!
# I am going to see if I can reduce them

#find the number of missing values in each column
colSums(is.na(train))

# remove my columns that contain all nas.
train2 <- train[ , colSums(is.na(train)) == 0]

colSums(is.na(train2))

#str(train2); view(train2)
# wait a second ! There looks to be a lot of simply blank cells too
colSums(train2=="")

train3 <- train2[ , !colSums(train2=="")]

colSums(train3=="")

## looks like these are the varaibles I will be using
#but dropping X, user_name,  numwindow because the are all identifiable
# dropping time stamps because they are not sensor data

training <- train3[8:60]
rm(train , train2, train3)

#doing same for the test
keep.names <- names(training)
testing <- test.set[names(test.set) %in% keep.names]

test2 <- test.set[ , colSums(is.na(test)) == 0]
test3 <- test2[ , !colSums(test2=="")]
testing <- test3[8:60]
rm(test2, test3)

## look at intercorrelation among vars - none
descrCor <- cor(training[1:52])
highCorr <- sum(abs(descrCor[upper.tri(descrCor)]) > 0.999)

#double check for uniary columns - my work here is done
nearZeroVar(training[1:52])


################################################
#  Seperate Training and Test by 70/30 
set.seed(12345)

inTrain <- createDataPartition(y = training$classe, p = 0.7, list = FALSE)
train  <- training[inTrain,]
test <- training[-inTrain,]

#################################################
# Trying various types of techinques form the course
#and then will compare the reuslts

# Bootstrapped Decision Tree

modFit <- train(classe ~., method="rpart", data=train)
#library(rattle); library(rpart.plot)
#fancyRpartPlot(modFit$finalModel)
a.tree <- predict(modFit, test)

# Boosted Decision Tree

btree.modFit <- train(classe~., method = "gbm", data =train)

b.tree <- predict(btree.modFit, test)

# Random Forest

ran.modFit <- train(classe~., method = "rf", data =train)

ran.forest <- predict(ran.modFit, test)

# "Traditional" Stat Models  using Naive Bayes 

modnb = train(classe ~ ., data=training,method="nb")

nb <- predict(mobnb, test)

#################################################################
# Compare the results side by side and see how they stack up

confusionMatrix(a.tree, test$classe)
# Decsion Tree is 
## 46.5% accuray 0.31 kappa - random chance is better

confusionMatrix(b.tree, test$classe)
# boosted Tree is
## 95.8% accurate and Kappa is 0.94

confusionMatrix(ran.forest, test$classe)
# Random Forest is
##98.9% accuracy  0.98 kappa

confusionMatrix(nb, test$classe)
# Naive Bayes is
## 75%  accuracy 0.68 kappa

################################################################
# based on the above it seems like  random forest  is the most accurate model
# Compared the to the others it has 98.9% accuracy 

pred <- predict(ran.modFit, testing)

################################################################
# write my answers out - sorry deleted so not to show anyone

answers= paste(pred,sep=",")

pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}

pml_write_files(answers)

```
