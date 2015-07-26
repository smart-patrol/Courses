Practical Machine Learning -Prediction Assignment
========================================================

Overview
-------------------------

The task of the project was to predict how well subjects performed weight lifting exercises based on data collected form accelerometers attached to the person performing the exercises. The data set consists of data form six different people and the outcome is classified into five different categories. The goal is to correctly predict which category a person falls under based on the provided test data.

Data Cleaning
-------------------------

The initial data set read in was 160 variables but in actuality, after missing NAs and blank columns are taken into consideration, along with variables related to the user or time, there ended up being only 52 variables to use minus the classe variable (dependent variable).

In addition, I double checked these results by seeing if there was little variance in the remaining 52 and if those 52 were correlated amongst each other or the classe variable.
```
 #find the number of missing values in each column

# remove my columns that contain all nas.

train2 <- train[ , colSums(is.na(train)) == 0]

# next blank cells

train3 <- train2[ , !colSums(train2=="")]

# drop identifier and Time Variables

training <- train3[8:60]

## look at intercorrelation among vars - none

descrCor <- cor(training[1:52])

highCorr <- sum(abs(descrCor[upper.tri(descrCor)]) > 0.999)

#double check for uniary columns - my work here is done

nearZeroVar(training[1:52])
```

Splitting the Data
------------------------------
The data is the split into two files a test and train.

Actually, there were two sets provided at start. A training set and testing set with the problems for the final project.

In order to actually train a model here, the training set needs to be split for cross validation. Once an accurate model is the built, the provided test data set will be scored.

To get a model with the best lift, I choose the 70/30 split and set a seed as Jeff has taught us, so that the results are reproducible.

```
set.seed(12345)

inTrain <- createDataPartition(y = training$classe, p = 0.7, list = FALSE)

train  <- training[inTrain,]

test <- training[-inTrain,]
```


Running Models to Find the Most Accurate
---------------------------------------------------
After the data was split, models were run on the training set to find the model with the highest predictive accuracy.

The order of the runs in the code is: Bootstrapped Tree, Boosted Tree, Random Forest, and Na�ve Bayes.

Primarily, the Caret Package was used to build these but the gbm and randomforest package were required directly.

Before going on, I would like to take this opportunity to talk about error here.

The ultimate goal is to get the lowest amount of error in any of these models, so how accurate they are predicting will be a large factor.

 

Cross Validation and Comparison of Results
--------------------------------------------
All results were cross-validated versus the testing data (the other 30%) and then compared to judge each model's accuracy.

As taught, in class the false positive and false negatives - along with the true positives and true negatives were compared in a table of test set results.

I was also looking at the kappa. This measures the agreement of prediction with the true class.

The confusionMatrix in Caret provides all of this functionally I need to judge this and more, like where it was misclassified (error rate) for example.

The below code compares the models : Decision Tree, Boosted Decision Tree, Random Forest, and Na�ve Bayes - in that order.

```
confusionMatrix(a.tree, test$classe)

confusionMatrix(b.tree, test$classe)

confusionMatrix(ran.forest, test$classe)

confusionMatrix(nb, test$classe)
```

Results
-------------------------------------
The end result was that Random Forest won with a predictive accuracy of 98.9% and a kappa 0.98.

The error rate is 1-accuracy. That means that 1% of the time it is misclassifying something!

If I had included num_window and user_name into this, I would have gotten 100% accuracy.

Likewise, if this was a model that I would implement going forward, I would be a bit concerned that there it is over-fitted if it is this accurate.

However, I hope that near 99% is enough to get all 20 answers right on this assignment.

At any rate, as per the code provided, I output my answers as individual text files.


