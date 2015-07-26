
#applied
# 8

str(rnorm)

set.seed(1)
x = rnorm(100)
e = rnorm(100)

b0 =3
b1 =2
b2 = -3
b3 = 0.3
y = b0 + b1 * x + b2 * x^2 + b3 * x^3 + e
dat <- data.frame(y,x)

library(leaps)

#-------------------------------------------------------------
# regsubsets

fit <- regsubsets(y ~ poly(x,10, raw=T), data=dat, nvmax =10)

which.min(summary(fit)$cp)
which.min(summary(fit)$bic)
which.min(summary(fit)$adjr2)

par(mfrow=c(3,1))

plot(summary(fit)$cp, xlab="Number of Variables", 
     ylab="Cp", col="black", pch=20, type = "b")
points(3, summary(fit)$cp[3], pch=20, col="red")

plot(summary(fit)$bic, xlab="Number of Variables", 
     ylab="BIC", col="black", pch=20, type = "b")
points(3, summary(fit)$bic[3], pch=20, col="red")

plot(summary(fit)$adjr2, xlab="Number of Variables", 
     ylab="Adj R", col="black", pch=20, type = "b")
points(1, summary(fit)$adjr2[1], pch=20, col="red")

par(mfrow=c(1,1))

coef(fit, id=3)

#-------------------------------------------------------------
# forward stepwise and backward setpwise

fit <- regsubsets(y ~ poly(x,10, raw=T), data=dat, nvmax =10, 
#                 method="forward")
                   method="backward")
            
which.min(summary(fit)$cp)
which.min(summary(fit)$bic)
which.min(summary(fit)$adjr2)

coef(fit, id=3)

# similar but differ on the variables picked and coeffcients

#-------------------------------------------------------------
# fitting a lasso model
library(glmnet)
set.seed(1)
# split into test and train
tr <- sample(seq(100), 50, replace=FALSE) 


mat = model.matrix( ~ poly(x,10, raw=T))

fit <- cv.glmnet(mat[tr, ], y[tr], alpha=1)
best_lam <- fit$lambda.min

plot(fit)

best_mod <- glmnet(mat, y, alpha=1)
predict(best_mod, s = best_lam, type = "coefficients")

#-------------------------------------------------------------
# using x^7 as the predictor variable

b7 = 7
y = b0 + b7 + x^7 + e
dat <- data.frame(y,x)
mod <- regsubsets(y ~ poly(x,10, raw=T), data=dat, nvmax=10)
which.min(summary(mod)$cp)
which.min(summary(mod)$bic)
which.min(summary(mod)$adjr2)

coef(mod, id=2)
coef(mod, id=1)
coef(mod, id=10)

mat <- model.matrix(y ~ poly(x, 10, raw=T), data=dat)[, -1]
mod <- cv.glmnet(mat, y, alpha = 1)
best_lam <- mod$lambda.min

final_mod <- glmnet(mat, y, alpha=1)
predict(final_mod, s = best_lam, type = "coefficients")













