library(ISLR)
library(leaps)

summary(Hitters)

#-------------------------------------------------------
# subset regression

# remove na's
Hitters = na.omit(Hitters)
with(Hitters, sum(is.na(Salary)))

#best subset regression
library(leaps)

fit = regsubsets(Salary ~., data=Hitters)
summary(fit)
# goes up tp only 8 at default

fit = regsubsets(Salary ~., data=Hitters, nvmax = 19)
names(summary(fit))
plot( summary(fit)$cp, xlab="Number of Variables", ylab=
        "Cp")
# identify smallest cp
which.min(summary(fit)$cp)
# cover plot in red tha is the best cp
points(10, summary(fit)$cp[10], pch=20, col="red")

#full plot
plot(fit, scale="Cp")
coef(fit, 10) # for model 10

#--------------------------------------------------
# forward stepwise selection

fit = regsubsets(Salary ~., data=Hitters, nvmax = 19,
                 method = "forward")
summary(fit)
plot(fit, scale="Cp")

# model selectio nusing validcation set
dim(Hitters)
set.seed(1)
tr <- sample(seq(263), 180, replace=FALSE)
tr
fit <- regsubsets(Salary ~., data=Hitters[tr,], nvmax=19, 
                  method="forward")

# fitting the model with errors and what not
val_error <- rep(NA,19)
x_tst <- model.matrix(Salary~., data=Hitters[-tr, ])
# predict for each model
for(i in 1:19){
  coefi <- coef(fit, id=i)
  pred <- x_tst[, names(coefi)]%*%coefi  # index columns by names to select
  val_error[i] <- mean((Hitters$Salary[-tr]-pred)^2)
}

plot(sqrt(val_error), ylab="Root MSE", ylim=c(300,400),
     pch=19, type="b")
points(sqrt(fit$rss[-1]/180), col="blue", pch=19, type="b")     
legend("topright", legend=c("Training","Validation"),col=c(
  "blue","black"), pch=19)     

#-------------------------------------------------------------
# write function to predict regsubset

pred_reg_subsets <- function(x, data, id, ...){
  form <- as.formula(x$call[[2]])
  mat <- model.matrix(form, data)
  coefi <- coef(x, id = id)
  mat[.names(coefi)]%*%coefi
}

#-------------------------------------------------------------
# Model Selection by Cross Validation

set.seed(1)
Hitters = na.omit(Hitters)
with(Hitters, sum(is.na(Salary)))

folds = sample(rep(1:10, length = nrow(Hitters)))
folds
table(folds)

cv.errors = matrix(NA, 10, 19)
for (k in 1:10) {
  best.fit = regsubsets(Salary ~ ., data = Hitters[folds != k, ], nvmax = 19, 
                        method = "forward")
  for (i in 1:19) {
    pred = predict(best.fit, Hitters[folds == k, ], id = i)
    cv.errors[k, i] = mean((Hitters$Salary[folds == k] - pred)^2)
  }
}
rmse.cv = sqrt(apply(cv.errors, 2, mean))
plot(rmse.cv, pch = 19, type = "b")


#-------------------------------------------------------------
# Ridge Regression and Lasso
library(glmnet)

x = model.matrix(Salary ~ . - 1, data = Hitters)
y = Hitters$Salary

fit.ridge = glmnet(x, y, alpha = 0)
plot(fit.ridge, xvar = "lambda", label = TRUE)

cv.ridge = cv.glmnet(x, y, alpha = 0)
plot(cv.ridge)

# fitting a lasso

fit.lasso = glmnet(x, y)
plot(fit.lasso, xvar = "lambda", label = TRUE)

cv.lasso = cv.glmnet(x, y)
plot(cv.lasso)

coef(cv.lasso)

lasso.tr = glmnet(x[tr, ], y[tr])
lasso.tr

pred = predict(lasso.tr, x[-tr, ])
dim(pred)

rmse = sqrt(apply((y[-tr] - pred)^2, 2, mean))
plot(log(lasso.tr$lambda), rmse, type = "b", xlab = "Log(lambda)")


# find the lowest rmse
lam.best = lasso.tr$lambda[order(rmse)[1]]
lam.best
coef(lasso.tr, s = lam.best)



vignette(arulesViz)

