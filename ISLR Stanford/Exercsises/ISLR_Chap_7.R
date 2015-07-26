
#---------------------------------------------------------------
#Random Forests
library(randomForest)
library(MASS)
library(gbm)
library(ISLR)

set.seed(101)
dim(Boston)
train = sample(1:nrow(Boston),300)
?Boston

rf <- randomForest(medv ~., data = Boston, subset=train)
rf
plot(rf)

#out of bag error
# mtry number of vars selected at random
oob_err <- double(13)
test_err <- double(13)

for(mtry in 1:13){
  fit <- randomForest(medv~. , data = Boston, subset=train, mtry=mtry, ntree=400)
  oob_err[mtry] = fit$mse[400]
  pred <- predict(fit, Boston[-train,])
  test_err[mtry] = with(Boston[-train,], mean((medv-pred)^2))
  cat(mtry,"")
}

matplot(1:mtry, cbind(test_err, oob_err), pch=19, col = c("red","blue"), type="b",
        ylab = "Mean Squared Error")
legend("topright", legend = c("00B","Test"), pch=19, col-c("red","blue"))

# reduced the error by half

#---------------------------------------------------------------
#Boosting

bst <- gbm(medv~., data=Boston[train,] , distribution = "gaussian", n.trees=10000,
           shrinkage=0.01,  interaction.depth = 4)

bst
summary(bst)

#plot partial dependcen plot
# the higher the lower income the lower value of the house
plot(bst, i="lstat")

# the highet number of rooms the more the house value increase
plot(bst, i="rm")

#---------------------------------------------------------------
# LOOKING AT TEST PERFROMANCE BY NUMBER OF TREES

n.trees <- seq(from=100, to=1000, by=100)
predmat <- predict(bst, newdata=Boston[-train, ], n.trees=n.trees)
dim(predmat)
# compute test error for each of the trees
berr <- with(Boston[-train, ], apply((predmat - medv)^2,2,mean))
plot(n.trees, berr, pch=19, ylab="MSE", xlab="# of tress", main="Bossting TEst 
     Error")
abline(h=min(test_err), col="red")
# boosting is reclutant to overfit
# beats rf in prediction

library(tree)


#---------------------------------------------------------------
# Chapter exercsises
# 8

oob_err <- double(13)
test_err <- double(13)

for(mtry in 1:13){
  fit <- randomForest(medv~. , data = Boston, subset=train, mtry=mtry, ntree=500)
  oob_err[mtry] = fit$mse[500]
  pred <- predict(fit, Boston[-train,])
  test_err[mtry] = with(Boston[-train,], mean((medv-pred)^2))
  cat(mtry,"")
}

matplot(1:mtry, cbind(test_err, oob_err), pch=19, col = c("red","blue"), type="b",
        ylab = "Mean Squared Error")
legend("topright", legend = c("OOB","Test"), pch=19, col-c("red","blue"))


#------------------------------------------------------------------
#9

library(ISLR)
library(rpart)
library(tree)
data(Carseats)
df = Carseats

mysamp <- floor(0.5 * nrow(df))
set.seed(123)
ind <- sample(seq_len(nrow(df)), size = mysamp)

tr <- df[ind, ]
tst <- df[-ind, ]

mod <- tree(Sales ~. , data = tr)

summary(mod)

cv.tr <- cv.tree(mod)

plot(cv.tr$size, cv.tr$dev, type="b")

pr_tr <- prune.tree(mod, best = 8)

plot(pr_tr)
text(pr_tr, pretty=0)

yhat = predict(pr_tr, newdata = tst)
plot(yhat, tst$Sales)
abline(0,1)

# the mse show that it is around 2 from teh actual mean
mean((yhat - tst$Sales)^2)

sqrt(5.31)
mean(tst$Sales)

# using randomforeset

rf <- randomForest(Sales~. , data = tr,  mtry = 5, ntree=200)
rf

importance(rf, scale=TRUE)
varImpPlot(rf)


oob_err <- double(10)
test_err <- double(10)

for(mtry in 1:20){
  fit <- randomForest(Sales ~. , data = tr,  mtry=mtry, ntree=800)
  oob_err[mtry] = fit$mse[800]
  pred <- predict(fit, tst)
  test_err[mtry] = with(tst, mean((Sales-pred)^2))
  cat(mtry,"")
}

matplot(1:20, cbind(test_err, oob_err), pch=19, col = c("red","blue"), type="b",
        ylab = "Mean Squared Error")
legend("topright", legend = c("OOB","Test"), pch=19, col=c("red","blue"))


rf <- randomForest(Sales~. , data = tr,  mtry = 6, ntree=200)
rf

yhat = predict(rf, newdata = tst)
plot(yhat, tst$Sales) ; abline(0,1)

mean((yhat - tst$Sales)^2)
# 2.655


# using a Boosted model for the hell of it

bst <- gbm(Sales ~., data=tr , distribution = "gaussian", n.trees=1000,
           shrinkage=0.01,  interaction.depth = 20)

bst
summary(bst)
plot(bst)

yhat = predict(bst, n.trees = 1000 , newdata = tst)
plot(yhat, tst$Sales) ; abline(0,1)

mean((yhat - tst$Sales)^2)
# 1.8

#-------------------------------------------------------------------------------------
#10

set.seed(123)

data(Hitters)
df = Hitters
names(df) <- tolower(names(df))

df = na.omit(df)

df$s_log <- log(df$salary)
with(df, plot(salary, s_log))

df$salary <- NULL

dim(df)
200/263

mysamp <- floor(0.7604563 * nrow(df))
set.seed(123)
ind <- sample(seq_len(nrow(df)), size = mysamp)

tr <- df[ind, ]
tst <- df[-ind, ]

pows = seq(-10, -0.2, by = 0.1)
lambdas <- 10^pows
l_lambdas <- length(lambdas)
tr_er <- rep(NA, l_lambdas)
tst_er <- rep(NA, l_lambdas)

for (i in 1:l_lambdas){
  mod = gbm( s_log ~., data=tr, distribution ="gaussian",
    n.trees = 1000, shrinkage = lambdas[i])
  tr_pred <- predict(mod, tr, n.trees=1000)
  tst_pred <- predict(mod, tst, n.trees=1000)
  tr_er[i] <- mean((tr$s_log - tr_pred)^2)
  tst_er[i] <- mean((tst$s_log - tst_pred)^2)
}

par(mfrow=c(1,2))

plot(lambdas, tr_er, type = "b", xlab="Shrinkage",
     ylab = "Train MSE", col = "blue", pch=20)

plot(lambdas, tst_er, type = "b", xlab="Shrinkage",
     ylab = "Test MSE", col = "red", pch=20)
par(mfrow=c(1,1))

min(tst_er)
lambdas[which.min(tst_er)]

gbm_fit <- gbm(s_log ~., data= tr, n.trees=1000,
               shrinkage = 0.007943282)
gbm_fit

# trying ols and lasso
lm_fit <- lm(s_log ~., data=tr)
lm_pred <- predict(lm_fit, tst)
mean((tst$s_log - lm_pred)^2)

library(glmnet)
x = model.matrix(s_log  ~., data=tr)
y = tr$s_log
x_tst <- model.matrix(s_log ~., data=tst)

l_fit <- glmnet(x,y, alpha=1)
l_pred <- predict(l_fit, s=0.01, newx=x_tst)
mean((tst$s_log - l_pred)^2)

# boosting has the smallest test error
min(tst_er)

summary(gbm_fit)

# trying ranodom forest
rf <- randomForest(s_log~. , data = tr,  mtry = 19, ntree=1000)
rf

rf_pred <- predict(rf, tst)
mean((tst$s_log - rf_pred)^2)

# random forest won ! barely
min(tst_er)

#-------------------------------------------------------------------------------------
#11
set.seed(123)

data(Caravan)
df = Caravan
summary(df)
names(df) <- tolower(names(df))
dim(df)

df$purchase <- ifelse(df$purchase == "No",0,1)


tr <- df[1:1000,]
tst <- df[1001:5822,]

bst <- gbm( purchase~.
           ,  n.trees=1000 
           ,  shrinkage = 0.01
           , data=tr[ , !names(tr) %in% c("pvraaut","avraaut")])

summary(bst, plotit=FALSE, n.trees=1000)

tst_pred <- predict(bst, tst, n.trees=1000, type="response")
probs <- ifelse(tst_pred > 0.2, 1, 0)
table(tst$purchase, probs)
prop.table(table(tst$purchase, probs))*100
33/146 #positive rate of 22%

#RMSE
mean((tst$purchase - tst_pred)^2)
#11.00387

# trying with logistic 
log <- glm(purchase ~.
           ,family = "binomial" 
           ,data=tr[ , !names(tr) %in% c("pvraaut","avraaut")])
summary(log)

tst_pred <- predict(log, tst, type="response")
probs <- ifelse(tst_pred > 0.2, 1, 0)
table(tst$purchase, probs)
prop.table(table(tst$purchase, probs))*100
58/408 # positive rate of 14%

#RMSE
mean((tst$purchase - tst_pred)^2)
#0.06732105

# trying with knn
library(class)
tr_x <- as.matrix(tr[ , -86])
tst_x <- as.matrix(tr[ ,-86])
tr_y <- tr$purchase
tst_y <- tr$purchase

k_mod <- knn(tr_x, tst_x, tst_x, k=3, prob=TRUE)
summary(k_mod)

# error rate
mean(k_mod != tst_y)

#RMSE
mean((as.numeric(tst_y) - as.numeric(k_mod))^2)
#0.963

k_prob <- attr(k_mod, "prob")
probs <- ifelse(k_prob > 0.2, 1, 0)
table(tst$purchase, probs)
13/59
#22% rate similar to boost

# trying with random forest

rf <- randomForest(purchase~. , data = tr,  mtry = 19, ntree=1000)
rf

#RMSE
rf_pred <- predict(rf, tst)
probs <- ifelse(rf_pred > 0.2, 1, 0)
mean((tst$purchase - rf_pred)^2)
#0.057

table(tst$purchase, probs)
50 / (50+283) # 15% positive rate

# actual test positive rate
prop.table(table(tst$purchase))









