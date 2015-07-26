library(ISLR)
library(boot)

?cv.glm

plot(mpg~horsepower, data=Auto)

#------------------------------------------
#LOOCV
glm.fit <- glm(mpg~horsepower, data=Auto)
cv.glm(Auto, glm.fit)$delta
#first # raw leave one out
#secon bias correction

plot(mpg~horsepower, data=Auto)
abline(glm.fit, col="red")

## write function on formula 5.2
loocv = function(fit){
  h = lm.influence(fit)$h
  mean((residuals(fit)/(1-h)^2))
}

loocv(glm.fit)

cv.error=rep(0,5)
degree = 1:5
for(d in degree){
  glm.fit = glm(mpg ~ poly(horsepower,d), data=Auto)
  cv.error[d] = loocv(glm.fit)  
}

plot(degree,cv.error, type="b")

#------------------------------------------
### 10-fold CV

cv.error10=rep(0,5)
for(d in degree){
  glm.fit=glm(mpg~poly(horsepower,d), data=Auto)
  cv.error10[d]=cv.glm(Auto,glm.fit,K=10)$delta[1]
}

lines(degree,cv.error10,type="b",col="red")

#------------------------------------------
### Bootstap

alpha = function(x,y){
  vx = var(x)
  vy = var(y)
  cxy = cov(x,y)
  (vy-cxy)/(vx+vy-2*cxy)
}

alpha(Portfolio$X,Portfolio$Y )

#find teh standard error

alpha.fn = function(data, index){
  with(data[index,], alpha(X,Y))
}

alpha.fn(Portfolio, 1:100)

# run boostrap
set.seed(1)
alpha.fn(Portfolio, sample(1:100, 100, replace=T))

boot.out = boot(Portfolio, alpha.fn, R=1000)
plot(boot.out)

#---------------------------------
## End quiz
setwd("C:\\Users\\Home\\Documents\\R_code")
load("5.R.RData")

names(Xy)

fit = lm(y ~ X1 + X2, data=Xy)
  
summary(fit)

matplot(Xy, type="l")
?matplot

bs <- function(formula, data, indx){
  d <- data[indx,]
  fit <- lm(formula, data=d)
  return(coef(fit))
}




b_out <- boot(data=Xy, statistic=bs, 
               R=10000, formula = y ~ X1 + X2)

b_out
plot(b_out, index=2)
boot.ci(b_out, type="bca",index=2)



fn = function(data){
  fit <- with(data=data, lm(y ~ X1 + X2))
  return(coef(fit)[1:3])  
}0

fn(Xy)





b_out <- boot(data=Xy, alpha.fn, sim = "parametric", ran.gen= fn ,
              R=1000)





boot.out=boot(Xy,blocklm.fn,R=1000,sim = "parametric",ran.gen = data.lm)

boot.out = tsboot(Xy, fn, R=100, l =100)



alpha.fn = function(data, index, X, Y){
  with(data[index,], alpha(X,Y))
}


alpha.fn(Xy, 1:100, Xy$X1, Xy$y)

#Chapter 5 ex 9
library(MASS)
summary(Boston)
set.seed(1)
attach(Boston)

mean(medv)
sd(medv) / sqrt(length(medv))

b_fn <- function(x,ind) return(mean(x[ind]))
library(boot)
boot(medv, b_fn, 1000)

t.test(medv)

b_fn <- function(x,ind) return(median(x[ind]))
boot(medv, b_fn, 1000)

quantile(medv,0.1)

b_fn <- function(x,ind) return(quantile(x[ind],0.1))
boot(medv, b_fn, 1000)








