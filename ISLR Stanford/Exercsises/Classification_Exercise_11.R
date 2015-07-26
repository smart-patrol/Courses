# 11
# develop a model to predict wether a car gets high
# or low gas milage 
library(ISLR)
library(dplyr)
library(corrplot)
library(caret)
library(pROC)

df <- Auto
names(df) <- tolower(names(df))
df$name <- NULL

# create a binary variable flagging the median of mpg

med_mpg <- na.omit(median(df$mpg))
df$mpg01 <- ifelse(df$mpg > med_mpg, 'Y', 'N')
table(df$mpg01)

# explore the assocation with med mpg
rho <- cor(df)
corrplot(rho)
rho[,9]

# keep year weight ,origin
df = df[ ,names(df) %in% c("year","origin","mpg01","weight")]

# split into test and traning
df$mpg01 <- factor(df$mpg01)
df$mpg01 <- relevel(df$mpg01, 2)


ind <- createDataPartition(y = df$mpg01,   p = .5,list = FALSE)
tr = df[ind,]
tst = df[-ind,]

#preprocess
proc <- preProcess(tr[, names(tr) != 'mpg01']
                   , method = c("BoxCox"))

t_tr <- predict(proc, tr[, names(tr) != 'mpg01'])
t_tst <- predict(proc, tst[, names(tst) != 'mpg01'])

# Perform LDA, QDA, Logisitc, and KNN

ctrl <- trainControl(method = "cv", repeats =3, 
                     classProbs = TRUE
                     , summaryFunction = twoClassSummary )

lda_fit <- train(x = t_tr, y = tr$mpg01,
      method="lda",
      trControl = ctrl,
      metric = "ROC")

qda_fit <- train(x = t_tr, y = tr$mpg01,
                 method="qda",
                 trControl = ctrl,
                 metric = "ROC")

log_fit <- train(x = t_tr, y = tr$mpg01,
                 method="glm",
                 trControl = ctrl,
                 metric = "ROC")

# examine the accuracy of each
pred <- predict(lda_fit, t_tst)
confusionMatrix(pred, tst$mpg01)
# 89%
pred <- predict(qda_fit, t_tst)
confusionMatrix(pred, tst$mpg01)
# 89%  lil better sens and spec
pred <- predict(log_fit, t_tst)
confusionMatrix(pred, tst$mpg01)
# 89%



proc <- preProcess(tr[, names(tr) != 'mpg01']
                   , method = c("center","scale"))

t_tr <- predict(proc, tr[, names(t_tr) != 'mpg01'])
t_tst <- predict(proc, tst[, names(t_tr) != 'mpg01'])

t_tst$mpg01 <-NULL
t_tr$mpg01 <- NULL

knn_fit <- train(x = t_tr, y = tr$mpg01,
                 method="knn",
                 trControl = ctrl,
                 metric = "ROC")





pred <- predict(knn_fit, t_tst)
confusionMatrix(pred, tst$mpg01)
# 89% 

# the data is clearly non-normal so discrim will not work - knn works best
knn_fit # k = 5

# area under the curve
plot(knn_fit)

probs = predict(knn_fit, t_tst, type="prob")
head(probs)

k_roc = roc(predictor=probs$Y, response=tst$mpg01)
plot(k_roc)


